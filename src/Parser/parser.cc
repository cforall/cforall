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

ForCtrl * enumRangeCtrl( ExpressionNode * index_expr, OperKinds compop, ExpressionNode * range_over_expr, DeclarationNode * type ) {
	assert( compop == OperKinds::LEThan || compop == OperKinds::GEThan );
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index_expr->expr.get()) ) {
		DeclarationNode * indexDecl =
			DeclarationNode::newName( new std::string(identifier->name) )->addType( type );
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

#line 349 "Parser/parser.cc"

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
#line 321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 744 "Parser/parser.cc"

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
#define YYFINAL  150
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   26021

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  183
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1122
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2209

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
       0,   644,   644,   648,   655,   656,   657,   658,   659,   663,
     664,   665,   666,   667,   668,   669,   670,   674,   675,   679,
     680,   685,   686,   687,   691,   695,   696,   707,   709,   711,
     713,   714,   716,   718,   720,   722,   732,   734,   736,   738,
     740,   742,   747,   748,   759,   764,   769,   770,   775,   781,
     783,   785,   791,   793,   797,   799,   801,   821,   824,   826,
     828,   830,   832,   834,   836,   838,   840,   842,   844,   846,
     855,   856,   860,   861,   863,   865,   867,   869,   871,   876,
     878,   880,   888,   889,   897,   900,   901,   903,   908,   924,
     926,   928,   930,   932,   934,   936,   941,   943,   946,   948,
     953,   955,   960,   961,   963,   967,   968,   969,   970,   974,
     975,   977,   979,   981,   983,   985,   987,   989,   996,   997,
     998,   999,  1003,  1004,  1008,  1009,  1014,  1015,  1017,  1019,
    1024,  1025,  1027,  1032,  1033,  1035,  1040,  1041,  1043,  1045,
    1047,  1052,  1053,  1055,  1060,  1061,  1066,  1067,  1072,  1073,
    1078,  1079,  1084,  1085,  1090,  1091,  1093,  1098,  1103,  1104,
    1108,  1110,  1115,  1118,  1121,  1126,  1127,  1135,  1141,  1142,
    1146,  1147,  1151,  1152,  1156,  1157,  1158,  1159,  1160,  1161,
    1162,  1163,  1164,  1165,  1166,  1176,  1178,  1183,  1184,  1186,
    1188,  1193,  1194,  1200,  1201,  1207,  1208,  1209,  1210,  1211,
    1212,  1213,  1214,  1215,  1216,  1217,  1218,  1219,  1220,  1222,
    1223,  1229,  1231,  1241,  1243,  1251,  1252,  1257,  1259,  1261,
    1263,  1265,  1269,  1270,  1272,  1278,  1307,  1310,  1312,  1314,
    1324,  1326,  1328,  1333,  1338,  1340,  1342,  1344,  1352,  1353,
    1355,  1359,  1361,  1365,  1367,  1368,  1370,  1372,  1377,  1378,
    1382,  1387,  1388,  1392,  1394,  1399,  1401,  1406,  1408,  1410,
    1412,  1417,  1419,  1421,  1423,  1428,  1430,  1435,  1436,  1458,
    1460,  1464,  1467,  1469,  1472,  1474,  1477,  1479,  1484,  1489,
    1491,  1496,  1501,  1503,  1505,  1507,  1509,  1514,  1516,  1519,
    1521,  1526,  1532,  1535,  1537,  1542,  1548,  1550,  1555,  1561,
    1564,  1566,  1569,  1571,  1576,  1583,  1585,  1590,  1596,  1598,
    1603,  1609,  1612,  1616,  1627,  1630,  1633,  1642,  1644,  1646,
    1648,  1653,  1655,  1657,  1662,  1663,  1665,  1670,  1672,  1677,
    1679,  1681,  1683,  1686,  1690,  1693,  1697,  1699,  1701,  1703,
    1705,  1707,  1709,  1711,  1713,  1715,  1717,  1722,  1723,  1727,
    1733,  1741,  1746,  1747,  1751,  1752,  1757,  1761,  1762,  1765,
    1767,  1772,  1775,  1777,  1779,  1782,  1784,  1789,  1794,  1795,
    1799,  1804,  1806,  1811,  1813,  1818,  1820,  1822,  1827,  1832,
    1837,  1842,  1844,  1846,  1851,  1853,  1859,  1860,  1864,  1865,
    1866,  1867,  1871,  1876,  1877,  1879,  1881,  1883,  1887,  1891,
    1892,  1896,  1898,  1900,  1902,  1904,  1910,  1911,  1917,  1918,
    1922,  1923,  1928,  1930,  1939,  1940,  1942,  1947,  1952,  1963,
    1964,  1968,  1969,  1975,  1976,  1980,  1982,  1986,  1988,  1992,
    1993,  1997,  1998,  2002,  2003,  2004,  2008,  2010,  2025,  2026,
    2027,  2028,  2030,  2034,  2036,  2040,  2047,  2049,  2051,  2059,
    2061,  2066,  2067,  2069,  2071,  2073,  2083,  2085,  2097,  2100,
    2105,  2107,  2113,  2118,  2123,  2134,  2141,  2146,  2148,  2150,
    2156,  2160,  2167,  2169,  2170,  2171,  2187,  2189,  2192,  2194,
    2197,  2202,  2203,  2207,  2208,  2209,  2210,  2219,  2220,  2221,
    2230,  2231,  2232,  2236,  2237,  2238,  2247,  2248,  2249,  2254,
    2255,  2264,  2265,  2270,  2272,  2276,  2278,  2280,  2282,  2289,
    2294,  2299,  2300,  2302,  2312,  2313,  2318,  2320,  2322,  2324,
    2326,  2328,  2331,  2333,  2335,  2340,  2346,  2348,  2350,  2352,
    2354,  2356,  2358,  2360,  2362,  2364,  2366,  2368,  2370,  2372,
    2374,  2376,  2378,  2380,  2382,  2384,  2386,  2388,  2390,  2392,
    2394,  2396,  2398,  2400,  2405,  2406,  2410,  2416,  2417,  2423,
    2424,  2426,  2428,  2430,  2435,  2437,  2442,  2443,  2445,  2447,
    2452,  2454,  2456,  2458,  2460,  2462,  2467,  2468,  2470,  2472,
    2477,  2479,  2478,  2482,  2490,  2491,  2493,  2495,  2500,  2501,
    2503,  2508,  2509,  2511,  2513,  2518,  2520,  2522,  2527,  2529,
    2531,  2533,  2534,  2536,  2541,  2543,  2545,  2550,  2551,  2555,
    2556,  2563,  2562,  2567,  2566,  2576,  2575,  2586,  2585,  2595,
    2600,  2601,  2606,  2612,  2630,  2631,  2635,  2637,  2639,  2644,
    2646,  2648,  2650,  2655,  2657,  2662,  2664,  2673,  2674,  2679,
    2688,  2693,  2695,  2697,  2706,  2708,  2709,  2710,  2712,  2714,
    2715,  2720,  2721,  2722,  2727,  2729,  2732,  2735,  2742,  2743,
    2744,  2750,  2755,  2757,  2763,  2764,  2770,  2771,  2775,  2783,
    2790,  2803,  2802,  2806,  2809,  2808,  2817,  2821,  2825,  2827,
    2833,  2834,  2839,  2844,  2853,  2854,  2856,  2862,  2864,  2869,
    2870,  2876,  2877,  2878,  2887,  2888,  2890,  2891,  2896,  2897,
    2898,  2900,  2906,  2907,  2909,  2910,  2911,  2913,  2915,  2922,
    2923,  2925,  2927,  2932,  2933,  2942,  2944,  2949,  2951,  2956,
    2957,  2959,  2962,  2964,  2968,  2969,  2970,  2972,  2974,  2982,
    2984,  2989,  2990,  2991,  2996,  2997,  3002,  3003,  3004,  3005,
    3009,  3010,  3015,  3016,  3017,  3018,  3019,  3033,  3034,  3039,
    3040,  3046,  3048,  3051,  3053,  3055,  3078,  3079,  3085,  3086,
    3092,  3091,  3101,  3100,  3104,  3110,  3112,  3122,  3123,  3125,
    3129,  3134,  3136,  3138,  3140,  3146,  3147,  3151,  3152,  3157,
    3159,  3166,  3168,  3169,  3171,  3176,  3178,  3180,  3185,  3187,
    3192,  3197,  3205,  3210,  3212,  3217,  3222,  3223,  3228,  3229,
    3233,  3234,  3235,  3240,  3242,  3248,  3250,  3255,  3257,  3263,
    3264,  3268,  3272,  3276,  3278,  3290,  3292,  3294,  3296,  3298,
    3300,  3302,  3303,  3308,  3311,  3310,  3322,  3321,  3334,  3333,
    3347,  3346,  3360,  3359,  3372,  3377,  3383,  3385,  3391,  3392,
    3403,  3410,  3415,  3421,  3424,  3427,  3431,  3437,  3440,  3443,
    3448,  3449,  3450,  3451,  3455,  3463,  3464,  3476,  3477,  3481,
    3482,  3487,  3489,  3491,  3496,  3497,  3503,  3504,  3506,  3511,
    3512,  3514,  3549,  3551,  3554,  3559,  3561,  3562,  3564,  3569,
    3571,  3573,  3575,  3580,  3582,  3584,  3586,  3588,  3590,  3592,
    3597,  3599,  3601,  3603,  3612,  3614,  3615,  3620,  3622,  3624,
    3626,  3628,  3633,  3635,  3637,  3639,  3644,  3646,  3648,  3650,
    3652,  3654,  3666,  3667,  3668,  3672,  3674,  3676,  3678,  3680,
    3685,  3687,  3689,  3691,  3696,  3698,  3700,  3702,  3704,  3706,
    3718,  3723,  3728,  3730,  3731,  3733,  3738,  3740,  3742,  3744,
    3749,  3751,  3753,  3755,  3757,  3759,  3761,  3766,  3768,  3770,
    3772,  3781,  3783,  3784,  3789,  3791,  3793,  3795,  3797,  3802,
    3804,  3806,  3808,  3813,  3815,  3817,  3819,  3821,  3823,  3833,
    3835,  3838,  3839,  3841,  3846,  3848,  3850,  3855,  3857,  3859,
    3861,  3866,  3868,  3870,  3884,  3886,  3889,  3890,  3892,  3897,
    3899,  3904,  3906,  3908,  3913,  3915,  3920,  3922,  3939,  3940,
    3942,  3947,  3949,  3951,  3953,  3955,  3960,  3961,  3963,  3965,
    3970,  3972,  3974,  3980,  3982,  3985,  3992,  3994,  4003,  4005,
    4007,  4008,  4010,  4012,  4016,  4018,  4023,  4025,  4027,  4029,
    4064,  4065,  4069,  4070,  4073,  4075,  4080,  4082,  4084,  4086,
    4088,  4093,  4094,  4096,  4098,  4103,  4105,  4107,  4113,  4114,
    4116,  4125,  4128,  4130,  4133,  4135,  4137,  4151,  4152,  4154,
    4159,  4161,  4163,  4165,  4167,  4172,  4173,  4175,  4177,  4182,
    4184,  4192,  4193,  4194,  4199,  4200,  4205,  4207,  4209,  4211,
    4213,  4215,  4222,  4224,  4226,  4228,  4230,  4233,  4235,  4237,
    4239,  4241,  4246,  4248,  4250,  4255,  4281,  4282,  4284,  4288,
    4289,  4293,  4295,  4297,  4299,  4301,  4303,  4310,  4312,  4314,
    4316,  4318,  4320,  4325,  4327,  4329,  4334,  4336,  4338,  4356,
    4358,  4363,  4364
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
  "postfix_expression", "field_name_list", "field", "field_name",
  "fraction_constants_opt", "unary_expression", "ptrref_operator",
  "unary_operator", "cast_expression", "qualifier_cast_list",
  "cast_modifier", "exponential_expression", "multiplicative_expression",
  "additive_expression", "shift_expression", "relational_expression",
  "equality_expression", "AND_expression", "exclusive_OR_expression",
  "inclusive_OR_expression", "logical_AND_expression",
  "logical_OR_expression", "conditional_expression", "constant_expression",
  "argument_expression_list_opt", "argument_expression_list",
  "argument_expression", "assignment_expression",
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

#define YYPACT_NINF (-1881)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1121)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     139, 11717,   211,   276, 19550,  -100, -1881, -1881, -1881, -1881,
   -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881,   -56,   864,
      23, -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881,   322,   284,
   -1881, -1881, -1881, -1881, -1881, -1881,  6055,  6055,   213, 11717,
     222,   237, 23080, -1881,   253, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881, -1881, -1881, -1881,   259,  2695, -1881,   460,   274,
   -1881, -1881,  2938, -1881, -1881, -1881, -1881, 14463, -1881,   323,
     361,   386,   543,    51, -1881,  6095,   398,   420,   429,   454,
    4987,   669,   789, 11897, -1881, -1881,   605, 14304,  1685, -1881,
   -1881, -1881, -1881,  2628,   689, 17210,  7341,   996,  2628,  1140,
     547, -1881, -1881, -1881, -1881,    78, -1881, -1881, -1881, -1881,
     574, -1881, -1881, -1881, -1881, -1881,   588,   600,    78, -1881,
      78, 17886, -1881, -1881, -1881, 20748,  6055, -1881, -1881,  6055,
   -1881, 11717, -1881,   599, 20802, -1881, -1881,  5783, 22024, -1881,
   -1881,  1185,  1185,   661,  2480, -1881, -1881, -1881, -1881,   180,
   16326,    78,  3615,    78, -1881, -1881, -1881, -1881, -1881, -1881,
     645, -1881,   656,   755,   917, -1881,   666, 25292, -1881, -1881,
   -1881, -1881, -1881, -1881, -1881, 18270,  2405,  2926,  2695,   500,
     685,   781,   797,   809,   823,   825, -1881, -1881, 19707, 13125,
   -1881,   773,   834, -1881,  9162, -1881, -1881, -1881, -1881,   841,
   -1881, -1881, -1881,   782, -1881, 23285,   978, 23439, -1881,   850,
    6055,   600,   868,   882, -1881,  2938,  5783,  2938, -1881, -1881,
   -1881,  3454,  4414,   852,   939,   590,   939, -1881,    78,    78,
     -19, 17560,   671,   939, -1881,    78,    78,   -19,    78, -1881,
      78, -1881,  4489, -1881, -1881,   887,   902,  1185, 22833,   890,
   14463, -1881,  6095, -1881,  2628, -1881,  2828,   547,   918,   995,
   17560,  6055,  6055,   543, -1881, 12429, -1881,  1185,  1185,   941,
     995, 17560,  6055, -1881, 10814, -1881, -1881, -1881,  1185, -1881,
   -1881, -1881, -1881,  1185, -1881,   703,  5562,  6055, -1881, 19245,
     955, -1881, -1881, -1881, 22694,   600, 17723,   952,  5783, 10046,
   22833,  2628, -1881, -1881, 22229, -1881,   939,    62, -1881, 25292,
   22174,  4164,  4489, -1881,   747, -1881, -1881, -1881, -1881, -1881,
   20802,   939,  6055, -1881,   956,   937, -1881, -1881, -1881, -1881,
    6055,  3859,   417,   464, -1881,  6055,   656, -1881,   711,    78,
   -1881,   964, 20959,   944, 16824, 22887,  2628, -1881,  2628,  1185,
    2628,  1185, -1881, -1881,    78, -1881, -1881,   980, 21013, -1881,
   -1881, -1881, 21170,   841, -1881,  3429,   250,   612, -1881,   299,
     547,   984,   965, -1881,  2480,   961,   656,  2480, -1881, -1881,
    2405, -1881,   630, -1881,   994, -1881,  1002,  1057, 25369,  1023,
   25446,  1037,  1060, 25292, 25523,  1063, 23196, -1881, -1881, -1881,
   -1881, -1881, -1881, 25600, 25600, 18109,  1040,  4712, -1881, -1881,
   -1881, -1881,   309, -1881,   549, -1881,  1232, -1881, 25292, 25292,
   -1881,  1049,   649,   953,   790,   687,   932,  1045,  1066,  1064,
    1113,   -25, -1881,   637, -1881,  1097, -1881,  1106,  5978, 18753,
   -1881, -1881,   385,  1097, -1881, -1881,   735, -1881, -1881,   804,
    2926,  1116,  1125,  1130,  1133,  1141,  1153, -1881, -1881,   754,
    1156, -1881,   374,  1156,  1163, -1881,  1178, -1881, 20748, -1881,
    1162,  1187, 18914, -1881, -1881,  5596,  5316,  1214, 16824,  1216,
    1047,  1159,  1198,  1209, -1881, -1881, -1881,  6055,  5948, 20223,
   -1881, -1881, -1881, -1881, -1881, -1881, -1881,  7816,  4874,  1040,
   23285,  1212,  1217, -1881, -1881,  1228, 23439,   786, -1881, -1881,
   -1881, 18753,  1255, -1881,   666, -1881, -1881, -1881,  1242,  3454,
     805,  1280,  1287,  1298,   820,  1302,  1304,  1307,  1321,  1325,
    1333,  4414, -1881, -1881, -1881,    78,  1260,  1340, -1881, -1881,
    1337,   543, -1881, -1881,   600,   995, 19873, -1881, -1881,   543,
   -1881, -1881,   600, -1881, -1881,  4489, -1881, 18753, 18753, -1881,
    1185,  5783, 23026,  4828, 16990, -1881, -1881, -1881, -1881, -1881,
   -1881,   600,   995,    62,  1348, -1881, -1881,  2628,  1352,   995,
   17560, -1881,   600,   995, -1881, 25761, -1881,  1185,  1185, -1881,
   -1881,  1362,   372,  1366,   547,  1376, -1881, -1881, -1881, 20169,
    1392,  1391, -1881, -1881,   815, -1881,  1488, -1881,  1387, -1881,
   -1881, -1881, 21336, 25764, -1881, -1881, -1881, -1881, -1881,  4164,
     831,  4489, 19873,   939, 11717, -1881,  6055,  1410, -1881,  1418,
   -1881, -1881, -1881, -1881, -1881,  2480, -1881, -1881,  1498,  5684,
   20380, 13125, -1881, 21390, -1881,  1185,  1185, -1881, -1881,   841,
   -1881, 15828,  1416,  1565, 25292,  2297,  1337,  1408, -1881,    78,
      78, -1881,  1156, -1881, 20959, -1881, -1881, 20169,  1185,  1185,
   -1881,  5684, -1881, -1881, 21969, -1881, -1881, 21013, -1881,    78,
    1425,    78,   965,   433,  1426,   858, 20802,   883,   895, -1881,
    2405,  9478,  1413, -1881, 18431, -1881,  4712, 18592, -1881, 21547,
   20802, -1881, 18431, -1881, 25292, -1881, -1881, -1881, -1881, -1881,
   -1881, 18592, -1881, -1881, 20434, 21547, 21547,  1165,  1765,  1914,
     639,  1954, -1881,   909,  1437,  1019,  1439, -1881, 23516, 25292,
   23593,  1435, 25292,  2938, 25292,  2938, -1881,  2672, -1881, -1881,
    9478,  3281, 25292,  9478,  2938, -1881, -1881, 25292, 25292, 25292,
   25292, 25292, 25292, 25292, 25292, 25292, 25292, 25292, 25292, 25292,
   25292, 25292, 25292, 25292, 25292, 25292, 23670, -1881,   666,  5414,
   13125, -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881,  1436, 25292, -1881, -1881, 15994,  2342, -1881, -1881,
      78,    78, -1881, -1881, 18753, -1881, -1881,   776,  1156, -1881,
     749,  1156, 19873, -1881, -1881,  1337, 19873, -1881,  1337, -1881,
   25848, -1881, -1881, -1881, 19393, 13125,  1440,  1175,  1441, 12252,
    1586,  3627,   777,  1408, -1881,    78,    78,  1408,   828, -1881,
      78,    78, 25292,  6055, 16990,  1444, 16990,  1446,  1408,   165,
   16160, 16160, 17156, 16160,  6055, -1881, -1881, 25292,  1228, -1881,
   23285,  1455, -1881,  2794, -1881, -1881, -1881,   912, -1881,  1454,
   16160, 25292,   916,  1458,  1460,  1462,   921,  1465,  1466,  1468,
    1470,  1471,  1473,   886,  1156, -1881, -1881,   943,  1156, -1881,
   -1881,  1025,  1156, -1881, -1881, -1881,  5783,  1592,  1156, 22379,
   -1881, -1881,   600,  1478, -1881, -1881, -1881,   929,  1480,   934,
    1481, -1881,  1163,  1479,  1487, -1881,   600, -1881,  1489, -1881,
     600,   995,  1487, -1881,   600,  1482,  1483,  1484, -1881, -1881,
   20030, -1881,  2938,  6055, 11177,  1575, -1881,  1187, -1881, 16160,
     938, -1881,  1487,  1492, -1881, 21601, 18753,  1472, -1881,  1472,
   -1881, -1881, -1881, -1881, 21013, -1881, 13295, 19075, -1881,  1493,
    1495,  1497,  1500, -1881,  4340,    78, -1881,  2297, -1881, -1881,
   -1881, -1881,  1337, -1881, -1881, -1881,  1185, -1881, -1881, -1881,
   -1881,   433,   965,  1499,   180, -1881, -1881,  1502,  6055,   433,
   -1881, -1881,  1501,  1508, -1881,  2938,  1509,  1511, -1881, -1881,
   -1881,  1516,  1518,  1515,  1521,  1520,  1523,  1541,  1543,  1544,
    1546,  1545, 25292,  1547,  1551,  1556, 21758, 13465, 25292, -1881,
   -1881,  2185, -1881, -1881, -1881, 25292, -1881,  1559,  1560, 23362,
   -1881, -1881,  1182, -1881,  9478,  1558, -1881,  1563, -1881, -1881,
    4690, -1881,  1562, -1881,  4690, -1881, -1881,  1206,  1504, -1881,
   -1881,  1049,  1049,  1049,   649,   649,   953,   953,   790,   790,
     790,   790,   687,   687,   932,  1045,  1066,  1064,  1113, 25292,
    1237,  1564,  4690, -1881, -1881, 23285, -1881,  1567,  1568,  1569,
    1572,  2342, -1881, -1881, -1881, -1881, -1881, 19873, -1881, -1881,
    1337, 19873, -1881,  1337,  1574,  1578, 16160, 16160, -1881, -1881,
   12252,   861,  1579,  1580,  1585,  1587,  3001,  3627, -1881, -1881,
   19873, -1881, -1881, -1881, -1881, -1881, -1881, 19873, -1881, -1881,
   -1881, -1881,  1570, -1881,  1408,  1582, -1881, -1881, -1881, -1881,
   -1881, -1881, -1881, -1881,  1589,  1590,  1593, -1881,  1594, -1881,
     543,  4690,  1253,    55, -1881, -1881,  1596, -1881, 23439, -1881,
   25292,    78, 16160,    78, -1881, -1881,  1034,  1156, -1881,  1055,
    1156, -1881, -1881,  1117,  1156, 19873, -1881, -1881,  1337, 19873,
   -1881, -1881,  1337, 19873, -1881, -1881,  1337,   939, -1881,  1337,
     178, -1881,  1097,  1595, -1881, -1881, -1881, -1881, -1881, -1881,
    1604, -1881, -1881, -1881, 21601,  1487, -1881,   600, -1881, -1881,
   -1881, -1881, -1881, 15127, -1881, -1881, -1881, -1881, -1881,    -4,
     658,   426, 12955,  1605,  1606, 17380,  1608,  1609,  3018,  3100,
    3810, 23747,  1611, -1881, -1881,  1612,  1615, 17380,  1618, -1881,
   -1881,   600, 25292, 25292,  1735,  1597,   446, -1881, 17948,  1257,
    1620,  1613,  1607, -1881, -1881, -1881, 10997, -1881, -1881, -1881,
   -1881, -1881,  1872, -1881, -1881, -1881,  1335,   143, -1881,   152,
   -1881,   143, -1881, -1881, -1881, -1881, -1881,  2938, -1881, -1881,
   12075, 14622, -1881,  6055,  1588,  1624, -1881, -1881, -1881,  6055,
   -1881, -1881,  5783, -1881, -1881,  1614,  1616,   972, 20802,   656,
     656, -1881, -1881,  1040,  1187, 18914, -1881,  1097, -1881, 13635,
   -1881,  1146,  1156, -1881,  1185,  8235, -1881, -1881,   965,  1502,
    1622,   433,   547,   529,  1634,  1610,  1502,  1639, -1881,  1619,
   -1881,  9478,   592, -1881, 20169,   592, 13465,  2938, -1881,   592,
   -1881, 20591,   592, -1881, 25292, 25292, 25292, -1881, -1881, -1881,
   -1881, 25292, 25292,  1636, 23285, -1881, -1881, 23824,  1640,  1646,
   -1881, -1881, -1881,  3915, -1881, -1881,  1282, -1881,    31, -1881,
    1320, -1881, 23516, -1881, -1881, 25292, -1881,  1332,  1338,  1228,
   -1881,  1186,  1156, -1881, -1881,  1647,  1649, -1881, -1881, -1881,
   -1881,  1654,  1194,  1156, -1881,  1226,  3159,    78,    78, -1881,
   -1881,  1655,  1657, -1881,  1643, -1881, 16990,  1656, -1881, 16492,
   16658,  1658, 17156,  1661, -1881,  1660, 25292, 25292,  1343,  1664,
   -1881, -1881, -1881, -1881, -1881, -1881,  1668, 19873, -1881, -1881,
    1337, 19873, -1881, -1881,  1337, 19873, -1881, -1881,  1337,  1669,
    1670,  1672,   543, -1881, -1881,  1360, 25292, 22533,  1673,  1676,
   -1881, -1881, -1881,  1678, 15287, 15447, 15607, 21601, 22833, 21547,
   21547,  1681,  1659,   244,   298,  2660,  9809, -1881,   303,  6055,
    6055, -1881,  9478,   376,   432, -1881, -1881, -1881, -1881, 12955,
   25292,  1682,  1759, 12784, 11357, -1881,  1662, -1881,  1667, 25292,
    1674, 23285,  1675, 25292, 18753, 25292, -1881, 11537,  1357, -1881,
    1680,     8, -1881,    37,  1757,   318,    78, -1881,  1695, -1881,
    1686, -1881,  1688,  1703,  1710, 17380, 17380, -1881, -1881,  1780,
   -1881, -1881,    81,    81,   583, 12606,   321, -1881, -1881,  1719,
    1724,   417, -1881,  1725, -1881,  1720, -1881,  1721, -1881, -1881,
   -1881, -1881, 13805,  1723,  1729,  1741, -1881, 19873, -1881, -1881,
    1337, 25292, 25292,  1187,  1742, -1881,  1740,  1750,   433,  1502,
     180,  6055, -1881, 23901, -1881,  1751, -1881, 21601, 25292, -1881,
    1261,  1756,  1745,   977, -1881,  1746, -1881, -1881, -1881, -1881,
   -1881, 23285,  1228, -1881, -1881, 23516, -1881,  1795,  4690, -1881,
    1795,  1795, -1881,  4690,  4054,  4177, -1881,  1363, -1881, -1881,
    1769, 19873, -1881, -1881,  1337, -1881, -1881,  1763,  1774,    78,
   19873, -1881, -1881,  1337, 19873, -1881, -1881,  1775, -1881, -1881,
   -1881, -1881, -1881, -1881, -1881, -1881,  1582, -1881, -1881, -1881,
    1777, -1881, -1881, -1881, -1881,  1779,  1786,    78,  1789,  1793,
    1794, -1881, -1881, -1881, -1881, 25292, -1881,   178, -1881,  1097,
   -1881, -1881,  1802,  1806, -1881,  1681,  1681,  1681,  3826,  1264,
    1796,   351, -1881,  3826,   457, 18753, -1881, -1881, -1881, -1881,
    4841, 25292,  5647,   262, -1881, -1881,   335,  1812,  1812,  1812,
    6055, -1881, -1881, -1881,  1818, -1881, -1881, -1881, -1881,  1613,
    1833, 25292,   361,  1831,   454, 15774, 21601,  1001,  1838, 17380,
    1837, -1881, -1881, -1881, -1881,  1297, 17380, 25292,  1336,   683,
   -1881, 25292, 23203, -1881, -1881,   469, -1881,  1228, -1881,  1018,
    1046,  1052,   692, -1881, -1881, -1881, -1881,   600,  1357,  1842,
   -1881, -1881, 25292, -1881,  1843,   666, -1881, 10817, -1881, -1881,
   -1881, 25292, 25292, -1881, -1881,   514,    81, -1881,   522, -1881,
   -1881, -1881,    78, -1881,  1472, -1881, 21601, -1881, -1881, -1881,
   -1881, -1881,  1839,  1845, -1881, -1881,  1844, -1881,  1848,   433,
   -1881,  1502,  1849,   547,  1610, 23285, -1881, -1881, -1881, -1881,
    1853, -1881, -1881, 25292, -1881, 20591, 25292,  1228,  1860,  1367,
   -1881,  1372, -1881,  4690, -1881,  4690, -1881, -1881, -1881,  1858,
      78,    78,  1859,  1862, -1881,  1857, -1881, -1881, -1881, -1881,
   -1881,  1374, 25292, -1881, -1881, -1881, -1881, -1881,   474,  1264,
    2545,   487, -1881, -1881, -1881, -1881,    78,    78, -1881, -1881,
   -1881,   505, -1881,  1059,  4841,   792, -1881,  5647, -1881,    78,
   -1881, -1881, -1881, -1881, -1881, -1881, 17380, 17380,  1613,  8039,
     120, 23978,  1946, 17380, -1881, -1881, -1881, -1881, -1881, 25292,
   -1881, 24055,  1947,  1850, 10295, 24132, 17380, 11537,  1613,  1303,
    1561,  1851, 25292, -1881,  1868,   430, 17380, -1881, 17380, -1881,
    1869, -1881, -1881,  1855,   666,   718, -1881, -1881,  1890,  1356,
    1067, 17380,  1870, 17380, 17380, 17380, -1881,   656, -1881,  6055,
    5783, -1881, -1881,  1894,  1896, -1881, -1881,  1502,  1875, -1881,
   -1881,  1228,  1905, -1881, -1881, -1881, -1881,  1917, -1881, -1881,
   -1881,  1377,  1382, -1881, -1881, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881,  1893,  1911,  1915,  2545, -1881,    78, -1881, -1881,
   -1881, -1881, -1881,  1919,  3826, -1881,  1999,  6763,    66, 13978,
   -1881, 17254, -1881,   -22,  1072, 17380,  2000,   536,  1908,   110,
   17380, 25292,  1303,  1561,  1901, 24214,   758,  1185,  1920,   516,
    2002, -1881, 24291, 24368, 25292,  1613,  1906, 14147, -1881, -1881,
   -1881, -1881, 21812, -1881,  1930,  1913,    48, -1881, 25292,  9478,
   -1881, -1881, 25292,   143, -1881, -1881, -1881, -1881, -1881, -1881,
   -1881,  1943, -1881,  1944, -1881, -1881, -1881, -1881,  1233,  1156,
   -1881, -1881,  1264, -1881, 17380, -1881,   113, -1881,   292, -1881,
   -1881, -1881,  1945, 14800, -1881, -1881, 17380, -1881,    20, -1881,
   17380, 25292,  1949, 24445, -1881, -1881, 24522, 24599, 25292,  5684,
    1613, -1881,  1097, 24676, 24753, 17380,  1931,   567,  1935,   568,
   -1881, -1881,  1953, 14800, 21812, -1881,  5209, 21390,  2938,  1939,
   -1881,  2003,  1960,   721,  1948, -1881, -1881,  1959,  1087,   437,
   -1881, -1881, 19873, -1881, -1881,  1337, -1881, -1881, 25292, -1881,
   25292, -1881, -1881,  1469, 14967, -1881, -1881, 17380, -1881, -1881,
    1613, -1881, -1881,  1613,  1950,   622,  1961,   648, -1881, -1881,
     547, -1881,  1613, -1881,  1613, -1881,  1963, 24830, 24907, 24984,
   -1881,  1469, -1881,  1937,  5097,  3932, -1881, -1881, -1881,    48,
    1970, 25292,  1955,    48,    48, -1881, -1881, 17380,  2059,  1977,
   -1881, -1881, 17254, -1881,  1469, -1881, -1881,  1982, 25061, 25138,
   25215, -1881, -1881,  1613, -1881,  1613, -1881,  1613, -1881,  1937,
   25292,  1983,  3932,  1986,   666,  1987, -1881,   722, -1881, -1881,
   17380, -1881, -1881, 10424,  1985, 17254, -1881, -1881,  1613, -1881,
    1613, -1881,  1613,  1991,  1994, -1881,   600,   666,  1997, -1881,
    1973,   666, -1881, -1881, -1881, -1881, 10643, -1881,   600, -1881,
   -1881,  1386, 25292, -1881,  1127, -1881,   666,  2938,  2004,  1974,
   -1881, -1881,  1150, -1881, -1881,  1976,  2938, -1881, -1881
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   499,     0,     2,   499,   516,   517,   518,   519,   520,
     521,   522,   523,   524,   505,   507,   506,   508,     0,     0,
       0,   526,   528,   549,   529,   550,   532,   533,   547,   548,
     527,   545,   546,   530,   531,   534,   535,   536,   537,   538,
     539,   540,   541,   542,   543,   544,   551,   552,   857,   554,
     627,   628,   631,   633,   629,   635,     0,     0,     0,   499,
       0,     0,    17,   598,   604,     9,    10,    11,    12,    13,
      14,    15,    16,   813,   104,     0,     0,    20,     0,     2,
     102,   103,     0,   834,    18,    19,   872,   499,   814,     0,
       0,   438,   736,   440,   451,   855,   439,   473,   474,     0,
       0,     0,     0,   581,   501,   503,   509,   499,   511,   514,
     566,   525,   553,   483,   559,   564,   485,   576,   484,   591,
     595,   601,   580,   607,   619,   857,   624,   625,   608,   677,
     441,   442,     3,   821,   835,   504,     0,     0,   857,   895,
     857,   499,   912,   913,   914,   499,     0,  1099,  1100,     0,
       1,   499,    17,     0,   499,   462,   463,     0,   581,   509,
     493,   494,   495,   824,     0,   630,   632,   634,   636,     0,
     499,   857,   680,   858,   859,   626,   555,    22,    23,    21,
     790,   785,   775,     0,   866,   822,     0,     0,   516,   815,
     819,   820,   816,   817,   818,   499,   866,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   599,   602,   499,   499,
     873,     2,     0,  1101,   581,   902,   920,  1105,  1098,  1096,
    1103,   435,   434,     0,   173,   742,   172,     0,   443,     0,
       0,     0,     0,     0,   449,     0,     0,     0,   433,   989,
     990,     0,     0,   472,   855,   857,   855,   876,   857,   857,
     482,   499,   857,   855,   933,   857,   857,   481,   857,   952,
     857,   930,     0,   574,   575,     0,     0,   499,   499,     2,
     499,   452,   855,   502,   512,   567,     0,   596,     0,   838,
     499,     0,     0,   736,   453,   581,   560,   577,   592,     0,
     838,   499,     0,   515,   561,   568,   569,   486,   578,   488,
     489,   487,   583,   593,   597,     0,   611,     0,   807,   499,
       2,   836,   894,   896,   499,     0,   499,     0,     0,   581,
     499,   511,     2,  1109,   581,  1112,   855,   855,     3,     0,
     581,     0,     0,   465,   857,   850,   852,   851,   853,     2,
     499,   855,     0,   811,     0,     0,   771,   773,   772,   774,
       0,     0,   767,     0,   756,     0,   765,   777,     0,   857,
     678,     2,   499,  1121,   500,   499,   490,   559,   491,   584,
     492,   591,   588,   609,   857,   610,   724,     0,   499,   725,
    1074,  1075,   499,   726,   728,   680,   598,   604,   681,   682,
     683,     0,   680,   860,     0,   788,   776,     0,   871,   870,
     866,   869,     0,   864,   867,    25,     0,    24,     0,     0,
       0,     0,     0,     0,     0,     0,    27,    29,     4,     8,
       5,     6,     7,     0,     0,   499,     2,     0,   105,   106,
     107,   108,    85,    28,    86,    46,    84,   109,     0,     0,
     124,   126,   130,   133,   136,   141,   144,   146,   148,   150,
     152,   154,   157,     0,    30,     0,   605,     2,   109,   499,
     165,   782,   732,   595,   734,   781,     0,   731,   735,     0,
       0,     0,     0,     0,     0,     0,     0,   874,   900,   857,
     910,   918,   922,   928,   598,     2,     0,  1107,   499,  1110,
       2,   102,   499,     3,   723,     0,  1121,     0,   500,   559,
     584,   591,     3,     3,   719,   709,   713,   725,   726,   499,
       2,     2,   903,   921,  1097,     2,     2,    27,     0,     2,
     742,    28,     0,   740,   743,  1119,     0,     0,   749,   738,
     737,   499,     0,   840,     0,     2,   464,   466,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   879,   936,   959,   857,   478,     2,   875,   883,
    1017,   736,   877,   878,     0,   838,   499,   932,   940,   736,
     934,   935,     0,   951,   953,     0,   468,   499,   499,   565,
     500,     0,   581,     0,   499,  1102,  1106,  1104,   450,   582,
     811,     0,   838,   855,     0,   444,   454,   513,     0,   838,
     499,   811,     0,   838,   786,   562,   563,   579,   594,   600,
     603,   598,   604,   622,   623,     0,   787,   695,   729,   500,
       0,   696,   698,   699,     0,   213,   427,   837,     0,   425,
     482,   481,   581,     0,   446,     2,   447,   808,   470,     0,
       0,     0,   499,   855,   499,   811,     0,     0,     2,     0,
     770,   769,   768,   762,   510,     0,   760,   778,   557,     0,
     499,   499,  1076,   500,   496,   497,   498,  1080,  1071,  1072,
    1078,   499,     2,   103,     0,  1036,  1050,  1121,  1032,   857,
     857,  1041,  1048,   717,   499,   589,   727,   500,   585,   586,
     590,     0,   679,  1086,   500,  1091,  1083,   499,  1088,   857,
       0,   857,   680,   680,     0,     0,   499,     0,     0,   862,
     866,   158,     0,    26,   499,    92,     0,   499,   100,   499,
     499,    87,   499,    94,     0,    36,    40,    41,    37,    38,
      39,   499,    90,    91,   499,   499,   499,     2,   105,   106,
       0,     0,   191,     0,     0,   625,     0,  1096,     0,     0,
       0,     0,     0,     0,     0,     0,    59,     0,    65,    66,
     158,     0,     0,   158,     0,    88,    89,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   437,     0,     0,
     499,   174,   175,   176,   177,   178,   179,   180,   181,   182,
     183,   184,   172,     0,   170,   171,   499,  1001,   733,   998,
     857,   857,  1006,   606,   499,   863,   901,   857,   911,   919,
     923,   929,   499,   904,   906,   908,   499,   924,   926,     2,
       0,     2,  1108,  1111,   499,   499,     0,     2,     0,   499,
     103,  1036,   857,  1121,   971,   857,   857,  1121,   857,   986,
     857,   857,     3,   727,   499,     0,   499,     0,  1121,  1121,
     499,   499,   499,   499,     0,     2,   751,     0,  1119,   748,
    1120,     0,   744,     0,     2,   747,   750,     0,     2,     0,
     499,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   857,   888,   892,   931,   857,   945,   949,
     957,   857,   967,   880,   937,   960,     0,     0,  1013,     0,
     476,   841,     0,     0,   477,   842,   469,     0,     0,     0,
       0,   467,     0,     2,     2,   843,     0,   448,     2,   811,
       0,   838,     2,   844,     0,     0,     0,     0,   637,   897,
     499,   915,     0,     0,   499,   428,   426,   102,     3,   499,
       0,   812,     2,     0,   764,   499,   499,   758,   757,   758,
     558,   556,   682,  1082,   499,  1087,   500,   499,  1073,     0,
       0,     0,     0,  1051,     0,   857,  1122,  1037,  1038,   718,
    1034,  1035,  1049,  1077,  1081,  1079,   587,   622,  1085,  1090,
     674,   680,   680,     0,     0,   690,   689,  1119,     0,   680,
     791,   789,     0,     0,   865,   162,     0,   159,   160,   164,
     823,     0,     0,     0,     0,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   499,   499,     0,   123,
     122,     0,   119,   118,    31,     0,    32,     0,     0,     0,
     188,   187,     0,     3,   158,     0,    55,     0,    56,    63,
       0,    62,     0,    58,     0,    57,    61,     0,     0,    54,
     125,   127,   128,   129,   131,   132,   134,   135,   139,   140,
     137,   138,   142,   143,   145,   147,   149,   151,   153,     0,
       0,     0,     0,    33,     3,   742,   166,     0,     0,     0,
       0,  1002,  1003,   999,  1000,   784,   783,   499,   905,   907,
     909,   499,   925,   927,     0,     0,   499,   499,  1027,  1026,
     499,     0,     0,     0,     0,     0,   857,  1037,   974,   991,
     499,   969,   977,   715,   972,   973,   716,   499,   984,   994,
     987,   988,     0,     3,  1121,     3,   711,   460,   710,   714,
    1113,   720,   721,   703,     0,   704,   705,     3,     3,     3,
     736,     0,   157,     0,     3,     3,     0,   745,     0,   739,
       0,   857,   499,   857,     3,   471,   857,   889,   893,   857,
     946,   950,   958,   857,   968,   499,   881,   884,   886,   499,
     938,   941,   943,   499,   961,   963,   965,   855,   479,  1014,
       3,  1018,  1019,     3,   846,   954,   571,   570,   573,   572,
       2,   812,   847,   793,   499,     2,   845,     0,   812,   848,
     637,   637,   637,   499,   697,   700,   701,   730,   431,     0,
       0,     0,   499,     0,     0,   352,     0,     0,     0,     0,
       0,   193,     0,   347,   348,     0,     0,   352,     0,   400,
     399,     0,   168,   168,   406,   598,   604,   210,   499,     2,
       0,   194,     0,   221,   195,   196,   499,   215,   197,   198,
     199,   200,     0,   201,   202,   353,     0,   367,   203,   373,
     375,   378,   204,   205,   206,   207,   208,     0,   209,   217,
     581,   499,   219,     0,     0,     0,     3,   825,   812,     0,
     800,   801,     0,     3,   796,     3,     3,     0,   499,   775,
     775,  1084,  1089,     2,   102,   499,     3,   596,     3,   500,
    1045,   857,  1044,  1047,   499,     3,  1033,  1039,   680,  1119,
       0,   680,   686,   680,     0,   691,  1119,     2,   861,     0,
     868,     0,    93,    96,   499,   101,   499,     0,    99,    95,
      97,   499,     0,   113,     0,     0,     0,   117,   121,   120,
     192,     0,     0,     0,   742,   110,   185,     0,     0,     0,
      49,    50,    82,     0,    82,    82,     0,    70,    72,    52,
       0,    48,     0,    51,   156,     0,   436,     0,     0,  1119,
    1010,   857,  1009,  1012,  1004,     0,     0,   898,   916,     3,
       3,     0,   857,   980,   983,   857,     0,   857,   857,   975,
     992,     0,     0,  1114,     0,   722,   499,     0,  1116,   499,
     499,     0,   499,     0,   445,     3,     0,     0,     0,     0,
     741,   746,     3,   839,     3,   856,     0,   499,   882,   885,
     887,   499,   939,   942,   944,   499,   962,   964,   966,     0,
       0,     0,   736,  1025,  1024,     0,     0,     0,     0,     0,
       3,   812,   849,     0,   499,   499,   499,   499,   499,   499,
     499,   620,     0,     0,     0,   651,   581,   638,     0,     0,
       0,   429,   158,     0,     0,   338,   339,   218,   220,   499,
       0,     0,     0,   499,   499,   334,     0,   332,     0,     0,
       0,   742,     0,     0,   499,     0,   379,   499,     0,   169,
       0,     0,   407,     0,     0,     0,   857,   225,     0,   216,
       0,   329,     0,     0,     0,   352,   352,   358,   357,   352,
     369,   368,   352,   352,     0,   581,     0,  1029,  1028,     0,
       0,   767,   803,     2,   798,     0,   799,     0,   779,   759,
     763,   761,   499,     0,     0,     0,     3,   499,  1040,  1042,
    1043,     0,     0,   102,     0,     3,     0,     0,   680,  1119,
       0,     0,   669,     0,   685,     0,   792,   499,     0,   161,
    1030,     0,     0,     0,    42,     0,   114,   116,   115,   112,
     111,   742,  1119,   190,   189,     0,    69,    79,     0,    73,
      80,    81,    64,     0,     0,     0,    60,     0,   155,    34,
       0,   499,  1005,  1007,  1008,   899,   917,     0,     0,   857,
     499,   976,   978,   979,   499,   993,   995,     0,   970,   985,
     981,   996,  1115,   712,   461,   707,   706,   708,  1118,  1117,
       0,     3,   854,   752,   753,     0,     0,   857,     0,     0,
       0,   890,   947,   955,   480,     0,  1020,     0,  1021,  1022,
    1016,   829,     2,     0,   831,   620,   620,   620,   651,   658,
     625,     0,   664,   651,     0,   499,   612,   650,   649,   645,
       0,     0,     0,     0,   652,   654,   857,   666,   666,   666,
       0,   646,   662,   432,     0,   342,   343,   340,   341,   234,
       0,     0,   236,   440,   235,   581,   499,     0,     0,   352,
       0,   317,   319,   318,   320,     0,   352,   193,   274,     0,
     267,     0,   193,   335,   333,     0,   327,  1119,   336,     0,
       0,     0,     0,   388,   389,   390,   391,     0,   381,     0,
     382,   344,     0,   345,     0,     0,   372,     0,   214,   331,
     330,     0,     0,   361,   371,     0,   352,   374,     0,   376,
     398,   430,   857,   827,   758,   780,   499,     2,     2,  1092,
    1093,  1094,     0,     0,     3,     3,     0,  1053,     0,   680,
     670,  1119,     0,   688,   691,   742,   692,   673,     3,   163,
       0,  1031,    98,     0,    35,   499,     0,  1119,     0,     0,
      83,     0,    71,     0,    77,     0,    75,    47,   167,     0,
     857,   857,     0,     0,   755,     0,   455,   459,   891,   948,
     956,     0,     0,   795,   833,   616,   618,   614,     0,     0,
    1060,     0,   659,  1065,   661,  1057,   857,   857,   644,   665,
     648,     0,   647,     0,     0,     0,   668,     0,   640,   857,
     639,   655,   667,   656,   657,   663,   352,   352,   237,   581,
       0,     0,   255,   352,   322,   325,   323,   326,   321,     0,
     324,     0,   263,     0,   193,     0,   352,   499,   275,     0,
     300,     0,     0,   328,     0,     0,   352,   351,   352,   392,
       0,   383,     2,     0,     0,     0,   212,   211,   354,     0,
       0,   352,     0,   352,   352,   352,   458,   775,   797,     0,
       0,  1095,  1046,     0,     0,  1052,  1054,  1119,     0,   672,
     687,  1119,     2,    53,    45,    43,    44,     0,    67,   186,
      74,     0,     0,  1011,   457,   456,   982,   997,   754,  1015,
    1023,   642,     0,     0,     0,  1061,  1062,   857,   643,  1058,
    1059,   641,   621,     0,     0,   350,   226,     0,     0,     0,
     248,   352,   228,     0,     0,   352,   257,   272,   283,   277,
     352,   193,     0,   287,     0,     0,     0,   312,   278,   276,
     265,   268,     0,     0,   193,   301,     0,     0,   231,   349,
     380,     2,   499,   346,     0,     0,   408,   359,     0,   158,
     370,   363,     0,   364,   362,   377,   766,   802,   804,  1055,
    1056,     0,   676,     0,   794,    68,    78,    76,   857,  1068,
    1070,  1063,     0,   653,   352,   243,   238,   241,     0,   240,
     247,   246,     0,   499,   250,   249,   352,   259,     0,   256,
     352,     0,     0,     0,   264,   269,     0,     0,   193,     0,
     288,   313,   314,     0,     0,   352,     0,   303,   304,   302,
     271,   337,     0,   499,   499,     3,   393,   500,   397,     0,
     401,     0,     0,     0,   409,   410,   355,     0,     0,     0,
     675,   693,   499,  1064,  1066,  1067,   660,   227,     0,   245,
       0,   244,   230,   251,   499,   421,   260,   352,   261,   258,
     273,   286,   284,   280,   292,   290,   291,   289,   270,   315,
     316,   285,   281,   282,   279,   266,     0,     0,     0,     0,
     233,   251,     3,   386,     0,  1060,   394,   395,   396,   408,
       0,     0,     0,   408,     0,   360,   356,   352,     0,     0,
     239,   242,   352,     3,   252,   422,   262,     0,     0,     0,
       0,   311,   309,   306,   310,   307,   308,   305,     3,   386,
       0,     0,  1061,     0,     0,     0,   402,     0,   411,   365,
     352,  1069,   222,     0,     0,   352,   299,   297,   294,   298,
     295,   296,   293,     0,     0,   387,     0,   414,     0,   412,
       0,   414,   366,   224,   223,   229,     0,   232,     0,   384,
     415,     0,     0,   403,     0,   385,     0,     0,     0,     0,
     416,   417,     0,   413,   404,     0,     0,   405,   418
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1881,  4771,  2706, -1881,    -1,   216,  -157,  6254,    53, -1881,
    -329, -1881,   381, -1881, -1017, -1145, -1881,   193,  4961,  1766,
   -1881,   975, -1881,  1421,   399,   793,   788,   628,   794,  1388,
    1389,  1397,  1390,  1385, -1881,    87,  -123,  -747, -1881,   853,
    8907,   946, -1881,  1728, -1881, -1881, -1336,  8256, -1180,  4816,
   -1881,  2163, -1881,   931,    25, -1881, -1881,   712,   114, -1881,
   -1703, -1880,   329,    95, -1881, -1881,   714,   342, -1881, -1602,
   -1881, -1465, -1881, -1881, -1881, -1881,   145, -1122, -1881, -1881,
   -1250,   467, -1881, -1881, -1881, -1881, -1881,    45, -1210, -1881,
   -1881, -1881, -1881, -1881,    69,   489,   491,   166, -1881, -1881,
   -1881, -1881,  -874, -1881,    97,    41, -1881,   170, -1881,  -211,
   -1881, -1881, -1881,   949,  -819,  -963,  -165, -1881,    21,    17,
      59,  1086,  -879,  -774, -1881,  -137, -1881, -1881,    94, -1881,
     -83,   608,   206,  -258,  2653,  2865,  -689,    49,   375,   364,
    2065,  3089, -1881, -1881,  2175, -1881,   563,  4751, -1881,  2110,
   -1881,   126, -1881, -1881,  3217,   641,  5189,  4056,    -6,  1957,
    -349, -1881, -1881, -1881, -1881, -1881,  -291,  8205,  7815, -1881,
    -414,   174, -1881,  -727,   285, -1881,   223,   778, -1881,   -97,
    -268, -1881, -1881, -1881, -1881,  -139,  8597,  -965,   920,   465,
    3063, -1881,  -301,  -102,   -32,  1584,  1573,  -733,  -150,   963,
    -101,  -532,  -178,  -200,  -454,  1371, -1881,  1732,   -61,  -947,
    1591, -1881, -1881,   716, -1881, -1251,  -181,  -170,  -524, -1881,
     355, -1881, -1881, -1185,   494, -1881, -1881, -1881,  2251,  -794,
    -461, -1123,   -16, -1881, -1881, -1881, -1881, -1881, -1881,   511,
    -847,  -201, -1859,   230,  7055,   -71,  5943,  -122,  1542, -1881,
    3023,   -30,  -227,  -219,  -218,    30,   -74,   -66,   -37,   632,
      27,    60,    68,  -212,     0,  -179,  -158,  -149,   101,   -99,
     -94,   -35,  -782,  -815,  -746,  -724,  -759,  -130,  -708, -1881,
   -1881,  -754,  1450,  1457,  1459,  3835, -1881,   614,  6605, -1881,
    -618,  -663,  -592,  -587,  -801, -1881, -1628, -1775, -1742, -1729,
    -652,  -140,  -262, -1881, -1881,   -81,   204,  -117, -1881,  7306,
     424,  -430,  -490
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   857,   432,   433,   179,    86,  1250,   434,   407,
     435,  1573,  1574,   436,  1366,  1367,  1368,  1587,   458,   438,
     439,   440,   740,   741,   441,   442,   443,   444,   445,   446,
     447,   448,   449,   450,   451,   460,  1153,  1006,  1007,  1008,
     742,  1500,   803,   227,   805,   454,  1042,  1251,  1252,  1253,
    1254,  1255,  1256,  1257,  2163,  1258,  1259,  1690,  2017,  2018,
    1950,  1951,  1952,  2133,  2134,  1260,  1709,  1710,  2041,  1711,
    1860,  1861,  1261,  1262,  1263,  1264,  1265,  1266,  1889,  1893,
    1523,  1515,  1267,  1268,  1522,  1516,  1269,  1270,  1271,  1272,
    1273,  1274,  1275,  1728,  2151,  1729,  1730,  2055,  1276,  1277,
    1278,  1503,  2063,  2064,  2065,  2191,  2202,  2083,  2084,   315,
     316,   944,   945,  1219,    88,    89,    90,    91,    92,  1693,
     494,   212,    96,    97,    98,    99,   243,   244,   318,   297,
     496,   462,   497,   102,   330,   104,   105,   159,   365,   321,
     109,   110,   111,   175,   112,   961,   366,   160,   115,   267,
     116,   161,   276,   368,   369,   370,   162,   455,   121,   122,
     372,   123,   615,   937,   935,   936,  1666,   124,   125,   126,
     127,  1213,  1467,  1673,  1674,  1821,  1822,  1468,  1661,  1841,
    1675,   128,   702,  1318,   171,   996,   129,   997,   998,  1564,
     969,   621,  1144,  1145,  1146,   622,   376,   505,   506,   624,
     464,   465,   228,   524,   525,   526,   527,   528,   353,  1299,
     354,   959,   957,   653,   355,   395,   356,   357,   466,   130,
     181,   182,   131,  1293,  1294,  1295,  1296,     2,  1200,  1201,
     644,  1287,   132,   343,   344,   278,   289,   598,   133,   231,
     134,   333,  1155,   588,   558,   173,   135,   402,   403,   404,
     136,   335,   247,   248,   249,   336,   138,   139,   140,   141,
     142,   143,   144,   252,   337,   254,   255,   256,   338,   258,
     259,   260,   843,   844,   845,   846,   847,   261,   849,   850,
     851,   808,   809,   810,   811,   559,  1193,  1446,   145,  1780,
     677,   678,   679,   680,   681,   682,  1824,  1825,  1826,  1827,
     667,   507,   380,   381,   382,   467,   218,   147,   148,   149,
     384,   871,   683
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      84,   396,   200,    84,   317,   198,   217,   877,   970,   579,
     201,   745,  1300,  1052,   541,   685,  1058,  1519,    93,  1450,
     377,   163,   542,   543,  1112,   155,  1319,   530,   325,   544,
     379,   137,   984,   391,  1326,   537,  1597,  1370,  1505,   202,
     565,  1122,   392,   185,  1932,   918,   920,  1129,  1540,  1541,
     106,  1492,  1033,  1092,  2026,    84,    84,   978,    84,  1118,
      94,   576,   545,   156,   453,  1377,   868,  2020,   522,   592,
     246,   229,   207,   217,   469,    84,    93,  1933,  1449,   971,
     603,    84,  1119,   546,   972,  1453,    84,   363,  2019,   137,
    1934,   489,   547,  1113,    84,   100,  2087,  1282,   751,    84,
     253,   164,    84,   203,   541,   595,    84,   502,   106,  1732,
    1872,   785,   542,   543,   317,  1114,   695,   215,    94,   544,
     698,  1136,  1734,  1138,   471,  1279,  1504,   117,   283,   924,
     250,  1115,   472,   279,  1415,   328,   204,   290,  1208,  -805,
     932,   229,   548,   317,   205,    84,  -838,   549,    84,   685,
      84,    58,   545,   100,   317,   786,    84,  1594,  1288,  1470,
    2027,   473,   271,    84,   232,  1530,   284,   200,    93,   405,
     539,    84,  1236,   546,  1001,   201,   970,   503,  1471,   628,
     169,   137,   547,    84,   952,   117,   657,   979,  1017,   586,
    1733,  1595,  1936,  1416,  1735,    84,    84,    84,  1947,  1948,
     106,   257,  2088,  2132,   202,   146,   550,    84,   146,  2061,
      94,   150,   552,    84,   230,    75,  -838,    85,  1589,   635,
     153,  2019,   668,   978,   521,   474,   657,   215,  1417,    84,
    2021,  2132,   548,   287,    84,    84,    84,   549,  1746,   406,
      84,    84,   553,  1865,   512,   100,   700,   971,  2025,  1789,
    1462,  2078,   972,   704,  2165,   701,  1517,   200,   475,   586,
     639,    84,  1965,   146,  1520,   201,   476,   117,   203,    84,
     685,    84,   215,  1035,   452,    85,  -806,   117,   708,  1518,
      84,    84,  1096,  2033,    84,  1949,  1417,  1521,   246,    63,
      64,    84,    85,  2079,   202,  1112,   550,  1359,   210,   610,
     215,   204,   552,    85,    20,    84,    84,  2011,    84,   205,
    1122,   146,   883,    84,   685,   599,    85,    84,   253,    85,
     884,   885,   635,    85,   746,   234,   872,   886,  1653,  1443,
      84,    84,   553,   705,  1463,  1399,   707,  1384,   685,  1932,
      78,    84,  1349,   554,   596,   685,   674,  1056,   630,    84,
      84,  1444,   668,  1123,    84,   146,  1559,  1126,  1400,  1317,
     887,   866,   215,   522,  1113,   108,   848,    85,  1141,  1142,
     184,   833,  1933,    85,   916,  1478,   107,   117,   217,   186,
     921,   888,  1778,   910,    84,  1934,  1114,  1013,   389,   931,
     889,   914,   489,    84,   187,    58,    84,  1745,   965,    84,
    1504,  1748,  1391,  1477,  1972,  1973,   117,   235,    58,  1282,
     195,  1451,   883,    85,    85,  -989,   196,   117,  1838,   631,
     884,   885,  -989,   108,   486,  1839,  1668,   886,  2117,  2025,
      85,  1517,  1297,   554,   107,   989,   208,  1279,  1156,  1464,
     890,    85,   117,   994,  1840,   891,   287,   970,  1792,  1794,
    1796,   536,    85,   538,  1518,  2080,  2081,    85,    85,    75,
     887,   236,  2025,   317,  -671,   471,  1680,   274,  1205,    84,
     752,  -671,    75,   472,   556,   753,   561,  1736,    85,   170,
    1669,   888,   502,   569,  1470,  1681,    85,  1936,  1473,  1474,
     889,  1462,  1462,  1462,    84,    84,   557,  2036,  2037,  1317,
     512,    85,   473,  1751,   892,   221,    84,    84,  1947,  1948,
    1549,   220,   207,  2127,  1829,   108,   320,    84,   971,   521,
      74,   903,   613,   972,  2011,   618,   107,  1863,  1236,   195,
     153,   826,  1871,  1830,    85,   557,  1505,  -617,    84,  1560,
     890,   310,   806,   222,   668,   891,   557,    85,    85,    -3,
      84,   904,   503,    80,    81,   650,   634,   636,  1685,   502,
    1147,   235,  1149,   993,   113,   471,   474,  1324,   662,   206,
      64,  1791,   965,   472,    84,  1463,  1463,  1463,   220,  1164,
      84,   512,    84,   236,   693,   651,   652,   879,   696,  -684,
    1891,   310,   237,  1771,  1616,  1977,  -684,   310,  1895,   475,
     995,   699,   473,   195,   892,   388,  1513,   476,  1475,   101,
     -23,   903,   157,  1011,  1687,  1514,  1014,   685,  1016,   468,
    1680,  1018,   113,   654,  1504,  1892,   320,   655,   951,   503,
    1021,  1379,  1035,  1023,  1024,  1025,   238,  1839,    84,  1832,
      84,   904,   118,    84,  1405,    84,  1996,   604,  1286,   597,
    1937,  1873,   905,   984,    84,   320,  1931,   309,    84,   477,
    1691,    93,   616,    58,  1691,  1712,   320,   101,  1839,  1938,
    1157,   512,   262,  1623,   137,   282,  1136,  1138,  1712,  1035,
    1464,  1464,  1464,   274,  1964, -1120,    85,  1941,   502,  2044,
      84,   320,  -493,   106,   587,   213,   995,   224,   662,  1035,
     118,   388,  1302,    94,   113,  1187,  1139,   305,   225,    84,
     754,   848,    85,   522,   113,   755,   866,   165,  2031,   216,
     166,   167,   379,   168,   226,  1684,   117,    75,   274,  1133,
    1035,  1035,   251,   502,    85,   280,    85,   307,   100,   291,
    2107,  2109,   905,  2069,    58,   309,   942,   309,   310,   101,
    1353,   557,    84,   323,    84,    85,    84,  1354,   503,   101,
      84,   564,   213,    84,   587,   310,  1539,    85,   572,   195,
     117,    14,    15,    16,    17,    18,  1921,  -990,  1922,   363,
     329,  2035,   118,   610,  -990,  1035,   670,   405,    84,   709,
     591,    85,   118,   710,  2050,  2138,   787,    85,  1028,   486,
     788,   602,   394,   503,  1907,  1389,  1390,  1897,    75,  1029,
    1030,  1035,   609,    64,   113,  1472,   487,   768,   775,   776,
      58,  2140,  1135,   927,   769,   770,  -826,    58,   566,   216,
    1148,    58,   557,    84,   745,   351,   263,   264,    84,   265,
      84,  1081,  1866,   113,   478,   266,   513,  1867,   146,    58,
      58,  1878,    84,  1320,   113,    85,  1867,    85,  2098,   101,
      85,  1424,   274,    84,   777,   778,   963,    63,    64,   521,
     658,   305,    84,   927,   216,   962,   581,  1985,   585,   113,
    2122,  2180,  1986,   220,    75,  2123,  2181,   379,   101,  1557,
     983,    75,   118,  1365,   813,    75,  1565,  1365,   814,   101,
    1582,    58,   216,   988,   642,    84,  1101,   987,   557,   489,
     557,   822,   397,    75,    75,   557,   670,   600,    78,   773,
     774,   118,   157,   165,   101,  1365,   166,   167,   522,   168,
     398,  1818,   118,  1097,  1120,   509,  1831,   557,   672,   452,
     479,    84,    84,   521,   363,   516,   873,   874,   585,  1600,
     875,   477,  1781,   557,  1152,   468,   480,   118,  1421,    58,
     685,    93,   309,   815,   477,    75,   557,   710,   481,  1046,
    1720,  1048,  1414,  1051,   941,   320,  1290,   566,   942,   896,
    1059,   557,   482,   274,   483,  1127,  1398,   848,   642,   672,
     477,   510,   557,   106,  1365,  1148,   597,    84,   515,  -494,
     529,   468,   468,    94,    84,  1083,   399,   531,   108,    14,
      15,    16,    17,    18,  1291,   555,    58,  1000,  1120,   107,
     477,   655,   672,    75,   152,   534,   177,   178,    65,    66,
      67,    68,    69,    70,    71,    72,   967,  1717,   100,   535,
     963,   232,  1002,  1175,   577,  1660,   655,   557,  1712,    84,
     486,   597,   583,    84,  1003,    85,  1551,    85,   710,   578,
      14,    15,    16,    17,    18,   779,   780,   213,  1034,    58,
     117,  1161,  1035,   309,   400,   814,   912,   557,   566,    74,
      75,    84,   557,   590,   521,   229,    85,    95,  1196,    85,
      95,  1532,  1035,  1198,   646,   642,   832,  1035,    58,   557,
    1179,   671,   513,   926,   557,   672,   601,    58,   985,    84,
     930,   625,    80,   673,   934,    84,    84,   487,   396,   396,
      58,   645,    85,   771,   772,   674,   660,  1787,    58,  1772,
     468,  1538,  1469,    75,   629,   814,  1784,   388,  1012,   692,
    1785,  1833,   706,  -495,   468,    95,  1019,   746,   146,   703,
      84,   711,  1788,    14,    15,    16,    17,    18,  1217,   146,
    1850,   712,    75,   113,  1035,   668,  1374,  1061,  1062,  1063,
    1301,    75,    14,    15,    16,    17,    18,  1875,   713,  1556,
     716,  1035,  1183,   513,    75,  -497,   557,  1037,  1038,   272,
      58,  1427,    75,    95,   719,   557,   379,   522,    14,    15,
      16,    17,    18,   748,  2085,  1876,  1365,   113,   101,   814,
     967,  1877,  1431,    58,   781,  1035,   557,   720,  1942,    58,
     724,  1329,   814,   767,   521,  1290,  1990,    84,    84,    84,
    1035,  2028,    58,   782,  2085,  1035,   521,    95,   468,    93,
    1575,   118,  2067,   783,   341,  2153,  2126,  1152,   784,  2157,
    1035,  1570,   101,   363,    75,   521,  1139,   789,    58,    58,
    1139,    84,  1139,  1291,  1644,  2135,    85,    58,   509,   748,
      85,   106,   379,    93,  1435,   816,    84,    75,   557,    84,
      84,    94,    84,    75,   817,   118,  2199,  1874,    84,   818,
    2196,    84,   819,  1631,  1632,   495,    75,  -498,    85,    58,
     820,    85,   283,  1547,  1148,   106,    58,   672,   108,  2205,
     279,   290,   821,  2206,  1692,    94,   100,   485,  1692,  1281,
     829,  1911,    75,    75,   834,   748,    85,  1026,   748,   363,
     274,    75,    85,    85,   522,   831,    84,   660,   748,   271,
     284,  1908,  1694,  1601,  1356,  1357,  1694,   557,   117,    -3,
     100,  1610,   852,   521,  -496,   557,   756,  1917,   757,   758,
     759,   854,    84,    75,  1815,  1816,  1817,    85,  1371,  1372,
      75,   272,   856,  1469,  1469,  1469,   -18,  1626,  1662,  1469,
     468,   869,   117,  1614,  1454,  1455,  1456,   672,   721,   760,
    2072,   870,   761,   762,   557,    84,    74,   763,   764,    74,
    1035,  1375,   379,  1068,  1069,  1070,  1071,   287,   146,  1842,
    1842,  1842,   878,   765,   766,  -165,  -165,  1442,   671,   208,
     748,  1819,   672,   881,   522,   557,   146,  1513,  1514,    80,
     673,  1365,    80,    81,   765,  1677,  1365,  1365,  1365,   893,
    1776,   907,  1207,   541,  1592,  1593,   894,  1854,  1855,  1856,
    1857,   542,   543,  1854,  1855,  1856,  1857,   895,   544,   363,
     146,   897,  1598,   898,    84,  1678,   899,   765,    84,    84,
    1858,  1723,  1724,  1725,  1726,  1727,  1858,  2001,   155,  1859,
     900,  2003,  1596,  1593,   901,   146,  1854,  1855,  1856,  1857,
     521,   545,   902,   618,  1599,  1593,    85,    85,   322,  1035,
    1109,  1585,   908,   452,   452,  1633,  1585,   113,    85,  1858,
    1148,   983,   546,   928,   521,   521,   156,   929,  1864,  1988,
    1989,   547,  1109,  1645,    84,  1797,  1357,  -615,   106,  1919,
    1357,  -613,   106,   106,  1920,  1593,  1929,  1035,    94,  2006,
    1593,   938,    94,    94,  2007,  1593,   106,  1947,  1948,  2196,
    2197,   939,   101,  1571,   940,   599,    94,  1590,  1591,   943,
      84,  1066,  1067,  1292,  1064,  1065,  1679,  1747,  1749,   946,
      85,   548,   954,  1072,  1073,   956,   549,   960,   973,    85,
     521,  1843,  1844,   975,   596,   118,   108,    84,  1290,   674,
     991,   999,    84,    84,    84,  1010,  1036,  1281,  1039,  1834,
    1044,  1085,  1108,  1109,  1116,   117,  1137,   883,  1140,   117,
     117,  1159,    85,  1163,  1188,   884,   885,  1166,   522,  1167,
     108,  1168,   886,   117,  1169,  1170,  1291,  1171,  1677,  1172,
    1173,  1281,  1174,  1677,   487,   550,  1365,  1195,  1365,  1197,
    1199,   834,   552,  -809,   274,  1203,  1283,  1210,  1211,  1212,
     452,  1289,  1310,  1298,  1311,   887,  1312,    84,  1678,  1313,
    1373,   146,    84,  1678,  1321,  1323,  1327,  1328,  1330,    84,
     495,    84,   553,   597,  1331,  1332,   888,  1333,  1334,    84,
    1335,    85,  1026,   146,  1575,   889,  1337,   146,   146,   188,
       6,     7,     8,     9,    10,    11,    12,    13,   521,  1998,
    1338,   146,  1339,  1340,  1342,   521,  1344,    85,   685,  1341,
    1345,  1854,  1855,  1856,  1857,  1346,   396,   283,  1351,  1352,
    1360,  1369,   468,  1376,  1035,  1361,  1380,  1381,  1382,   985,
      95,  1383,  1403,  1387,  1858,   890,   521,  1388,  1392,  1393,
     891,    85,  1060,  -194,  1394,  1406,  1395,   495,  1408,  1502,
    1527,   292,  1420,  1409,   271,   284,  1410,  1412,  1447,  1679,
    -810,   -22,  1479,  1480,  1679,  1483,  1484,    87,  1493,  1494,
     154,   146,  1495,   554,   521,  1497,  1035,  1290,  -123,  -123,
    -123,  -123,  -123,  -123,  1506,   113,  1528,  1558,  1885,  1507,
    1562,  1563,    84,   504,    84,  1566,  1534,    85,  1536,   892,
    1568,  1581,  1586,  1585,    85,  1622,  1605,   903,  1606,    85,
      85,    85,  1292,  1609,  1620,  1291,  1621,  1628,  1624,   113,
    1629,  1465,   287,  1593,  2016,    87,  1634,  1637,  1641,  1642,
     101,  1643,  1651,    84,  1654,  1650,    84,   904,  1665,  1472,
    1698,  1667,   197,   108,  1713,   521,   521,   108,   108,  1714,
    1514,  1738,   521,    87,  1696,  2116,  1716,  1718,  1696,  1696,
    1741,   108,  1731,   118,   101,   521,   242,  1742,  1739,   270,
    1740,  1236,  1696,    87,    85,   521,   495,   521,  1752,    85,
    1753,  1755,   623,  1757,  1758,  1759,    85,   541,    85,   597,
     521,  1760,   521,   521,   521,   542,   543,   118,    84,    84,
     146,  2058,   544,  1761,  1767,  1769,  1770,  1777,   596,  1783,
    1786,   154,   280,   291,  1677,  1782,   106,    87,   468,  1790,
     154,   495,  1800,   332,   340,  1798,    94,  -122,  -122,  -122,
    -122,  -122,  -122,  1801,   477,   545,   362,  1984,   905,  1804,
     495,  1633,   495,    84,  1678,  1806,   495,   495,  1808,   495,
     521,  1510,  1809,  1810,   521,  2130,   546,  2016,  1813,   521,
     146,   459,  1814,   197,   197,   547,   495,    14,    15,    16,
      17,    18,  1032,  2058,   154,   492,  1671,  1846,  1828,   152,
     270,   177,   178,    65,    66,    67,    68,    69,    70,    71,
      72,    85,  1847,   117,   230,  1851,  1853,  1343,  2155,  1882,
    1884,  1901,   332,  1347,  1902,  1909,  1905,   242,   242,    85,
    1906,    85,  1913,   521,  1355,   548,  1918,  1923,  1926,  1928,
     549,  1927,  1955,  1960,  1976,   521,  1981,  1992,   332,   521,
      95,  2002,  1961,  1974,   452,   495,    87,  1983,    84,  2062,
     200,    95,   113,   639,   521,  1679,   113,   113,   201,  1987,
      85,   270,  2008,    85,  1511,    84,  1999,    84,  2000,   274,
     113,  2004,  1465,  1465,  1465,   157,  1658,  1659,  1663,  2198,
    2009,   146,   106,  2005,  2010,  2014,  2030,   202,  2045,   550,
     557,  2032,    94,  2038,   332,   552,   521,   101,  2051,   468,
     340,   101,   101,  2043,  2059,  2060,   340,   332,   332,  2070,
    2071,  2082,   106,  2119,  2106,   101,   154,  2091,  2108,  2110,
    2120,  2124,    94,    84,    84,   553,    85,  2121,  2125,  2150,
     118,  2141,   903,  2137,   118,   118,   521,   193,   362,   675,
     684,   521,  2154,   106,  2139,  2160,  2161,  2156,   118,   623,
    2166,  2185,  2176,    94,   362,   215,  2179,  2187,   362,   117,
    2177,    84,   904,  2188,  2192,  2193,  2204,   600,  2207,   521,
      85,  1031,   521,  2203,   521,   452,  1915,   452,   504,  1074,
    1078,  1075,  2062,   293,  1077,  1292,  2062,  2062,   294,   117,
    1076,   298,   512,   303,  1569,   521,   804,  1509,  2056,  1501,
    2186,   459,   495,   495,  2131,  1700,    84,   721,    14,    15,
      16,    17,    18,  1348,  1978,    84,  2148,  2178,   452,  1971,
     117,  1722,  1894,   597,  2128,   623,   554,  1880,  2174,  1881,
    2112,  2158,  2194,  2111,   176,   459,   300,   146,   807,  2013,
    2190,   108,  1526,   589,  2190,  2076,   197,  2175,  1664,  1910,
    1524,  1158,  1696,  1561,   623,   504,   958,  1754,   495,  2200,
    1898,     3,  1004,   905,   154,  2099,  1088,   146,   492,   876,
    2056,  1812,   841,  1089,   684,  1090,     0,     0,     0,     0,
       0,     0,    85,     0,  2118,   154,     0,     0,     0,   452,
     765,     0,     0,     0,     0,     0,     0,     0,   146,     0,
      95,     0,     0,     0,     0,     0,     0,   459,     0,     0,
     311,     0,     0,     0,     0,   242,     0,     0,    95,     0,
      14,    15,    16,    17,    18,     0,     0,   242,     0,  1576,
    1577,  1578,     0,     0,     0,     0,  1579,  1580,     0,     0,
      85,    85,     0,     0,     0,     0,     0,     0,     0,   293,
       0,   332,    95,   459,   459,     0,     0,   332,     0,     0,
     362,     0,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,     0,     0,  1292,     0,   272,    95,    85,     0,
      58,     0,     0,     0,   504,     0,     0,     0,     0,   623,
       0,     0,     0,     0,     0,     0,   293,   108,     0,     0,
       0,     0,     0,     0,   533,   623,     0,     0,  1696,   623,
       0,     0,     0,     0,     0,   332,     0,   332,     0,     0,
      87,     0,   623,  2201,     0,    58,     0,   108,   398,   504,
       0,     0,  2208,     0,     0,     0,   362,   492,  1696,   684,
     113,   294,    74,   689,    75,   303,     0,   675,   504,     0,
     504,   675,     0,     0,   504,   504,     0,   504,   108,     0,
     362,     0,     0,     0,   671,     0,     0,     0,   672,  1696,
     684,     0,     0,   362,   504,    80,   673,     0,     0,     0,
       0,     0,   154,     0,     0,   101,     0,    74,   627,    75,
     459,   729,     0,   459,     0,   154,   154,     0,   459,     0,
       0,     0,   495,     0,   399,   495,   495,   459,     0,   806,
     154,   154,   154,   557,     0,     0,     0,     0,   118,     0,
      80,    81,   152,  1216,   177,   178,    65,    66,    67,    68,
      69,    70,    71,    72,  1215,     0,     0,     0,     0,     0,
       0,     0,     0,   504,     0,     0,     0,     0,   345,     0,
       0,     0,     0,    95,     0,     0,   346,   347,   348,   349,
       0,     0,     0,     0,     0,     0,   492,     0,    14,    15,
      16,    17,    18,     0,     0,    95,     0,     0,     0,    95,
      95,     0,   807,   807,     0,     0,     0,     0,     0,     0,
     459,     0,   729,    95,     0,     0,   113,   152,   744,   177,
     178,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     362,   492,     0,     0,     0,   841,     0,   841,     0,     0,
       0,   272,     0,     0,     0,     0,   113,     0,    58,     0,
     362,     0,   362,     0,     0,     0,   362,   362,   362,   362,
       0,   101,   188,     6,     7,     8,     9,    10,    11,    12,
      13,   350,     0,     0,     0,     0,   362,   113,     0,     0,
       0,     0,     0,    95,   103,     0,     0,   158,     0,   351,
       0,   101,   293,   262,   118,     0,     0,     0,     0,     0,
     623,     0,   332,     0,   623,     0,     0,     0,     0,     0,
      74,     0,    75,   623,     0,     0,     0,     0,     0,     0,
     504,   504,   101,   623,   118,     0,     0,     0,     0,     0,
     623,     0,  1819,     0,     0,     0,   557,     0,     0,     0,
     459,     0,   103,    80,    81,   362,  1888,     0,     0,     0,
       0,   154,   459,     0,     0,   118,     0,   911,     0,     0,
     362,     0,  1305,     0,     0,   915,     0,     0,     0,     0,
     214,     0,     0,   675,     0,     0,   504,     0,   623,     0,
       0,     0,   623,     0,   925,     0,   623,     0,     0,     0,
     285,     0,     0,     0,     0,   933,     0,   152,    58,   239,
     240,    65,    66,    67,    68,    69,    70,    71,    72,   152,
       0,   272,    95,    65,    66,    67,    68,    69,    70,    71,
      72,  1049,   154,   492,   319,    74,     0,   223,   324,     0,
       0,     0,   152,     0,   103,     0,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,  1670,    77,     0,
       0,     0,     0,   364,  1671,     0,     0,     0,    80,    81,
      74,     0,    75,  1050,     0,     0,     0,     0,   308,     0,
      82,     0,    95,     0,     0,     0,    19,     0,     0,     0,
     470,     0,    76,    77,     0,     0,     0,   807,     0,     0,
       0,   324,   498,    80,    81,     0,     0,     0,     0,     0,
       0,     0,   362,   362,     0,    82,   841,   744,     0,     0,
     744,     0,     0,   841,     0,   744,    48,    49,    50,    51,
      52,    53,    54,    55,   744,   551,     0,     0,     0,     0,
       0,   152,     0,     0,   319,    65,    66,    67,    68,    69,
      70,    71,    72,   744,     0,   575,     0,     0,     0,     0,
     580,   582,     0,   214,     0,     0,     0,   192,   362,     0,
       0,     0,     0,   319,     0,   272,     0,     0,     0,    14,
      15,    16,    17,    18,   319,     0,     0,   605,     0,     0,
       0,   607,    77,    95,     0,   865,   608,     0,     0,     0,
       0,     0,   619,  2066,     0,     0,     0,   582,   273,   319,
     154,     0,     0,   632,     0,     0,     0,     0,     0,   154,
     295,     0,   302,     0,   304,   641,     0,     0,   459,     0,
     504,     0,     0,   504,   504,     0,     0,     0,     0,    58,
     623,     0,     0,     0,   623,     0,     0,     0,   623,     0,
       0,     0,     0,     0,   459,   663,     0,     0,   687,     0,
       0,     0,   459,   273,     0,     0,   302,   304,     0,     0,
       0,   694,     0,   152,   637,   694,     0,    65,    66,    67,
      68,    69,    70,    71,    72,   152,   270,    87,     0,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   332,     0,
       0,    74,     0,    75,   154,     0,     0,     0,     0,     0,
       0,   492,     0,     0,    58,  1194,     0,     0,     0,   273,
       0,     0,     0,    76,    77,     0,     0,     0,     0,  1202,
       0,     0,     0,  1206,    80,    81,     0,  1209,     0,   199,
       0,     0,   492,     0,     0,     0,    82,   154,   152,    95,
     239,   240,    65,    66,    67,    68,    69,    70,    71,    72,
     623,     0,     0,   245,     0,   152,     0,   177,   178,    65,
      66,    67,    68,    69,    70,    71,    72,     0,    75,    95,
       0,   324,     0,     0,     0,   663,     0,     0,     0,     0,
     273,   194,   302,   304,     0,     0,     0,     0,  1396,    77,
       0,     0,   324,     0,     0,     0,     0,     0,     0,     0,
      95,     0,   362,     0,   623,   362,   362,     0,   362,     0,
     334,    82,     0,   623,   273,     0,     0,   623,     0,   273,
       0,     0,   275,     0,     0,   273,     0,   836,     0,   838,
    1485,     0,     0,     0,   296,   299,     0,   152,   855,   177,
     178,    65,    66,    67,    68,    69,    70,    71,    72,   619,
     154,   154,   154,   154,     0,   154,   154,     0,     0,   273,
       0,  1672,   340,     0,   690,     0,   304,   498,     0,     0,
       0,     0,     0,     0,     0,   459,     0,   275,     0,   459,
     459,     0,     0,   319,     0,     0,     0,     0,     0,   334,
     459,     0,     0,   459,   540,   245,   152,     0,     0,     0,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   728,  1487,     0,     0,   334,     0,     0,     0,     0,
       0,   270,     0,     0,     0,   619,     0,   103,     0,    19,
       0,     0,     0,   275,     0,     0,     0,     0,   492,     0,
       0,     0,     0,   694,   966,     0,  1396,    77,     0,     0,
       0,     0,     0,     0,   619,     0,     0,     0,   977,     0,
       0,     0,     0,   154,     0,   273,   675,   663,     0,    82,
       0,   334,   986,    52,    53,    54,    55,     0,     0,     0,
     694,     0,     0,     0,   640,   334,     0,     0,     0,     0,
       0,     0,     0,   273,     0,   690,   304,     0,     0,     0,
    1452,     0,   620,     0,   275,     0,     0,     0,     0,     0,
       0,     0,   728,     0,  1476,     0,     0,     0,   152,     0,
     177,   178,    65,    66,    67,    68,    69,    70,    71,    72,
    1053,     0,     0,     0,  1498,     0,     0,     0,   275,     0,
       0,   744,     0,   275,     0,     0,   273,     0,     0,   275,
       0,     0,     0,     0,  1672,  1820,   500,     0,     0,  1672,
       0,   459,     0,     0,     0,     0,  1672,     0,  1672,     0,
     273,     0,  1054,   498,     0,   273,     0,   273,     0,     0,
       0,     0,     0,   275,     0,     0,     0,     0,     0,   619,
    1091,   340,   154,     0,     0,     0,     0,     0,     0,     0,
     273,     0,   273,   273,     0,   619,     0,     0,     0,   619,
       0,     0,     0,     0,   273,     0,     0,   694,   966,     0,
       0,     0,   619,     0,  1117,     0,     0,   273,     0,     0,
       0,     0,     0,     0,     0,   730,   273,   498,     0,   498,
       0,     0,     0,   498,   498,   364,   498,     0,     0,   842,
       0,     0,   154,     0,     0,     0,     0,    58,   273,     0,
     690,   304,     0,   498,     0,     0,   152,     0,   239,   240,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   154,   273,   690,     0,     0,     0,     0,  1132,   273,
       0,   152,   882,   239,   240,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   245,     0,     0,     0,     0,   665,
       0,     0,   688,     0,     0,  1820,  1820,   275,     0,    74,
       0,    75,     0,   619,     0,   665,     0,  1280,   334,   665,
    1672,   388,   498,  1672,   334,     0,   730,     0,   158,     0,
       0,   241,    77,     0,     0,   340,     0,   694,     0,     0,
    1309,     0,    80,    81,     0,     0,     0,  1315,     0,   913,
       0,     0,     0,   459,    82,     0,  1686,  1688,     0,     0,
      14,    15,    16,    17,    18,   623,     0,     0,     0,     0,
       0,     0,     0,  1284,  1285,     0,     0,     0,     0,     0,
       0,     0,   950,     0,   334,     0,   332,     0,     0,     0,
       0,   275,     0,     0,     0,     0,     0,     0,     0,   324,
     364,     0,     0,     0,     0,     0,     0,  1750,     0,     0,
       0,     0,     0,     0,   275,     0,     0,     0,     0,     0,
      58,  1820,     0,     0,     0,   620,     0,     0,   275,   665,
    1672,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   275,   152,     0,   386,   387,    65,    66,    67,    68,
      69,    70,    71,    72,   152,     0,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   154,  1358,
     619,     0,   275,     0,   619,     0,     0,     0,     0,   498,
     498,     0,    74,   619,    75,     0,     0,     0,     0,     0,
       0,     0,     0,   619,     0,    78,   275,     0,  1820,     0,
     619,     0,     0,   275,   839,    77,     0,   388,   672,   154,
    1378,     0,     0,     0,     0,    80,   840,     0,     0,     0,
       0,   500,     0,     0,     0,     0,     0,    82,     0,     0,
       0,     0,     0,     0,     0,   498,     0,     0,     0,   154,
     154,     0,  2115,   340,     0,     0,     0,     0,   619,     0,
       0,   273,   619,     0,     0,     0,   619,     0,     0,  1404,
       0,  1407,   273,     0,     0,     0,     0,     0,     0,     0,
     154,   273,     0,  1411,     0,  1413,     0,   158,     0,     0,
    1418,  1419,  1111,     0,   842,     0,  1466,     0,     0,  1087,
    1426,     0,     0,     0,     0,  1280,     0,   665,   500,     0,
    2115,  2115,     0,     0,     0,  1104,     0,     0,     0,  1105,
    1879,     0,     0,     0,     0,     0,  1445,     0,     0,  1448,
       0,   665,     0,     0,     0,     0,     0,     0,     0,  1280,
       0,     0,     0,     0,   665,     0,     0,   152,  2115,   177,
     178,    65,    66,    67,    68,    69,    70,    71,    72,   334,
       0,     0,     0,   152,  1525,   239,   240,    65,    66,    67,
      68,    69,    70,    71,    72,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,   273,     0,   663,     0,
       0,    74,  1508,     0,     0,     0,   152,   580,   177,   178,
      65,    66,    67,    68,    69,    70,    71,    72,  1489,     0,
       0,     0,   273,  1670,    77,     0,     0,   619,     0,   364,
    1671,     0,  1529,     0,    80,    81,     0,     0,  1191,  1533,
       0,  1535,  1537,     0,     0,    58,    82,   500,     0,     0,
    1543,     0,  1544,     0,  1545,     0,     0,     0,     0,     0,
       0,  1554,   152,     0,   648,     0,    65,    66,    67,    68,
      69,    70,    71,    72,  1362,     0,     0,     0,  1363,   152,
    1364,   239,   240,    65,    66,    67,    68,    69,    70,    71,
      72,   665,   500,     0,     0,   275,     0,   120,     0,   498,
     120,     0,   498,   498,     0,   364,     0,    74,     0,    75,
       0,     0,     0,    77,     0,     0,  1588,     0,     0,   500,
     619,     0,     0,     0,   619,     0,     0,   568,   619,  2114,
      77,     0,     0,   557,     0,  1607,  1608,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,  1466,  1466,  1466,
     158,   582,    82,     0,     0,   120,     0,     0,     0,     0,
       0,  1630,     0,     0,     0,     0,     0,     0,  1635,     0,
    1636,     0,  1695,  1111,     0,     0,  1695,  1695,     0,  1397,
     842,     0,     0,   120,     0,   273,     0,     0,     0,     0,
    1695,     0,     0,     0,     0,     0,  1652,     0,     0,   277,
    1385,   152,     0,   120,  1386,    65,    66,    67,    68,    69,
      70,    71,    72,  1362,   273,     0,     0,  1363,     0,  1364,
     273,   665,     0,  1401,   688,     0,     0,     0,     0,     0,
    1402,     0,     0,     0,     0,   364,     0,   120,     0,     0,
     619,   120,     0,     0,     0,     0,     0,   120,     0,     0,
     120,     0,    77,     0,   277,  1793,     0,     0,     0,     0,
     158,     0,     0,     0,     0,   358,   120,     0,   390,     0,
       0,     0,     0,     0,     0,     0,     0,    58,  1439,     0,
       0,     0,  1440,     0,   500,     0,  1441,     0,     0,     0,
       0,   463,  1762,     0,   619,     0,     0,     0,     0,  1766,
       0,  1768,     0,   619,   120,   463,     0,   619,     0,     0,
     277,   152,     0,   239,   240,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   152,     0,     0,     0,    65,    66,
      67,    68,    69,    70,    71,    72,  1362,   812,     0,    74,
    1363,    75,  1364,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,   824,   334,     0,   827,     0,     0,
       0,   331,    77,     0,   120,  1837,   120,     0,     0,     0,
       0,   273,    80,    81,     0,    77,   120,  1805,  1795,  2189,
       0,   277,     0,     0,    82,  1314,     0,   120,     0,  1849,
       0,  2195,     0,    14,    15,    16,    17,    18,     0,     0,
       0,     0,   614,     0,     0,   120,     0,     0,     0,   275,
     120,     0,   120,     0,     0,   277,   120,     0,     0,   568,
     277,     0,     0,     0,     0,     0,   277,     0,     0,   408,
     273,     0,   409,     0,   410,   411,   120,   412,   275,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   158,
       0,     0,     0,    58,   413,     0,     0,     0,   120,  1617,
     277,   120,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,   120,     0,     0,     0,   120,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,     0,   423,   424,     0,
    1903,  1904,     0,  1935,     0,    74,     0,    75,     0,     0,
       0,   463,     0,     0,  1912,     0,     0,    58,  1676,     0,
    1638,     0,     0,     0,  1639,     0,     0,   425,  1640,     0,
      78,   426,    14,    15,    16,    17,    18,   427,   491,    81,
     428,   429,   430,   431,     0,   463,     0,  1966,     0,     0,
    1695,   152,   665,   239,   240,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,  1648,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,   463,    74,
       0,    75,     0,   500,   277,   275,     0,     0,     0,     0,
     273,     0,    58,     0,     0,   120,     0,     0,     0,     0,
       0,   241,    77,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,    81,     0,     0,     0,   463,     0,     0,
       0,     0,     0,     0,    82,     0,   152,     0,   239,   240,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
    1763,     0,     0,     0,   275,     0,     0,     0,     0,     0,
       0,     0,   120,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,   463,   463,  2057,     0,     0,   277,     0,
     120,   812,   812,     0,     0,     0,   331,    77,     0,     0,
       0,     0,  1099,     0,     0,  1102,   120,    80,    81,     0,
       0,     0,     0,     0,  1799,     0,     0,     0,     0,    82,
       0,     0,     0,  1802,     0,   277,  1695,  1803,     0,     0,
       0,  1676,     0,     0,     0,     0,  1676,     0,   277,     0,
       0,     0,     0,  1835,     0,  1676,     0,     0,   120,     0,
     120,     0,   273,     0,     0,     0,  1695,  2057,     0,     0,
       0,     0,     0,     0,   273,   390,   120,   463,     0,   277,
       0,   568,     0,     0,     0,   619,     0,   120,  1177,     0,
       0,     0,  1181,     0,     0,     0,  1185,  1695,     0,     0,
     120,     0,     0,   277,     0,     0,     0,   614,     0,     0,
     277,     0,   114,   120,     0,     0,     0,     0,     0,   500,
       0,  2113,   120,     0,     0,     0,     0,     0,  2152,     0,
     463,     0,     0,   463,   151,   120,   120,     0,   463,     0,
       0,     0,     0,     0,   275,     0,     0,   463,     0,     0,
     120,   120,   120,     0,     0,     0,     0,   152,     0,     0,
     273,    65,    66,    67,    68,    69,    70,    71,    72,  1362,
     114,     0,     0,  1363,     0,  1364,     0,   415,  2149,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   273,   304,     0,     0,     0,     0,     0,     0,  2164,
       0,     0,     0,     0,     0,     0,   463,     0,    77,     0,
     209,     0,     0,     0,  2173,     0,     0,  1943,   286,     0,
    1676,     0,   120,     0,     0,     0,     0,     0,     0,   750,
     463,     0,    78,   426,     0,     0,     0,     0,   120,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
     120,   463,   114,     0,     0,   120,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,     0,     0,     0,     0,
     120,     0,   120,     0,    58,     0,   120,   120,   120,   120,
       0,   367,   273,   334,     0,     0,   812,     0,     0,     0,
       0,     0,     0,     0,     0,   152,   120,   922,   275,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   152,     0,
     239,   240,    65,    66,    67,    68,    69,    70,    71,    72,
     499,     0,     0,     0,     0,  1192,     0,  1676,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,   152,   209,   206,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,   120,     0,  1670,    77,
     463,  1429,   114,     0,  1433,   120,     0,     0,  1437,    80,
      81,   120,   463,     0,     0,     0,     0,   273,     0,     0,
     120,    82,  1307,   463,     0,     0,     0,     0,     0,     0,
       0,   114,    77,     0,     0,   865,     0,     0,     0,     0,
     584,     0,   114,     0,     0,   606,     0,     0,     0,     0,
    1322,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     367,     0,     0,     0,     0,     0,     0,   114,     0,     0,
       0,   286,     0,     0,     0,     0,     0,     0,     0,   334,
       0,   626,   120,   463,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   633,   152,     0,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
     584,     0,     0,   664,     0,     0,   286,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,   664,
       0,     0,   661,   664,     0,  2129,     0,   640,   334,     0,
       0,     0,     0,     0,   241,    77,   275,     0,   437,     0,
       0,     0,     0,   120,     0,    80,    81,   120,     0,     0,
       0,     0,   120,   120,     0,     0,   120,    82,     0,     0,
      58,     0,     0,     0,     0,   334,   120,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,   749,     0,   665,
       0,     0,     0,     0,   152,     0,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,  1603,     0,   120,     0,
       0,     0,     0,     0,     0,     0,     0,  1612,   790,     0,
       0,   120,    74,     0,    75,   120,     0,     0,     0,   120,
       0,     0,     0,   664,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,  2114,    77,   830,     0,   557,     0,
     120,   835,     0,     0,     0,    80,    81,     0,     0,   120,
       0,   665,     0,     0,     0,     0,     0,    82,   463,     0,
       0,   861,   862,     0,     0,     0,   863,   864,     0,     0,
     867,     0,     0,     0,     0,     0,   288,     0,     0,     0,
       0,     0,     0,     0,   463,     0,   880,     0,     0,     0,
       0,     0,   463,     0,     0,     0,   152,   367,   239,   240,
      65,    66,    67,    68,    69,    70,    71,    72,   909,     0,
     119,     0,     0,     0,     0,   499,   277,   120,     0,     0,
     119,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,   114,     0,     0,   120,     0,     0,     0,     0,   371,
       0,   463,     0,     0,     0,  1307,  2114,    77,     0,   715,
     557,   718,     0,     0,   437,   723,     0,    80,    81,     0,
       0,     0,     0,     0,   732,   733,     0,     0,     0,    82,
     120,     0,   463,   367,     0,   114,     0,   120,   501,   437,
     437,     0,     0,     0,     0,     0,   949,     0,     0,     0,
       0,   664,   499,     0,     0,     0,     0,     0,     0,   955,
     437,     0,   367,   152,     0,   239,   240,    65,    66,    67,
      68,    69,    70,    71,    72,   664,     0,     0,     0,     0,
     119,     0,     0,   974,     0,     0,     0,     0,   664,     0,
       0,    74,     0,   437,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,   120,   120,     0,   120,   119,
       0,     0,     0,   839,    77,     0,     0,   672,     0,     0,
     119,     0,     0,   120,    80,   840,     0,   120,     0,     0,
       0,   120,     0,     0,  1823,     0,    82,   674,   371,     0,
       0,     0,     0,  1649,     0,   119,     0,     0,  1027,   288,
     120,   120,   120,   120,   120,   120,   120,     0,     0,     0,
       0,   152,   277,   609,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,   463,     0,     0,     0,   463,
     463,   499,     0,     0,     0,     0,     0,     0,     0,     0,
     463,   666,     0,   463,   288,     0,     0,   367,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   666,     0,     0,
       0,   666,     0,   367,     0,  1082,     0,   367,     0,     0,
       0,   277,     0,     0,     0,   664,   499,     0,     0,     0,
     367,     0,     0,     0,     0,     0,     0,     0,   463,     0,
    1106,     0,  1107,   120,     0,   367,     0,   367,   835,     0,
       0,   367,   367,   499,   367,     0,  1773,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
       0,   367,     0,     0,     0,     0,  1151,     0,     0,     0,
       0,     0,     0,     0,     0,  1160,     0,     0,     0,  1162,
       0,     0,     0,     0,  1823,  1823,     0,   120,     0,     0,
      14,    15,    16,    17,    18,     0,   120,     0,     0,   152,
     120,   611,   612,    65,    66,    67,    68,    69,    70,    71,
      72,   666,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   367,     0,     0,   661,   114,     0,     0,     0,  1204,
     367,     0,     0,   152,     0,   177,   178,    65,    66,    67,
      68,    69,    70,    71,    72,   664,     0,     0,   286,     0,
      58,   463,    78,     0,     0,     0,     0,     0,   437,   437,
     437,   437,   437,   437,   437,   437,   437,   437,   437,   437,
     437,   437,   437,   437,   437,   437,   437,     0,     0,     0,
       0,   277,   120,   510,   152,   371,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
    1823,     0,     0,   501,     0,     0,     0,     0,   499,     0,
       0,     0,    74,     0,    75,     0,  1336,     0,     0,   119,
       0,   152,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,  1670,    77,     0,     0,     0,     0,
       0,     0,   120,   437,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    82,     0,     0,
       0,   371,     0,   119,     0,     0,     0,     0,     0,     0,
       0,   120,     0,  2074,    78,     0,     0,  1823,   367,   666,
     501,     0,   367,     0,     0,     0,     0,   367,   367,     0,
     371,   367,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   367,     0,   666,     0,     0,     0,     0,   367,     0,
       0,     0,     0,     0,     0,     0,   666,     0,     0,     0,
     152,  1823,   239,   240,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,   277,     0,     0,     0,     0,
       0,     0,     0,   367,     0,     0,     0,     0,    74,     0,
     463,     0,     0,   463,     0,     0,   367,     0,     0,     0,
     367,     0,     0,     0,   367,     0,     0,     0,     0,     0,
     331,    77,     0,     0,     0,     0,     0,     0,     0,  1823,
    1823,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    82,     0,     0,     0,     0,     0,     0,
       0,   151,     0,   114,     0,     0,     0,     0,     0,   501,
       0,     0,     0,   437,     0,     0,     0,  1823,     0,   437,
       0,   174,     0,     0,     0,   371,     0,     0,     0,     0,
     437,     0,     0,     0,     0,     0,     0,   114,     0,     0,
       0,   371,     0,     0,     0,   371,     0,     0,     0,   174,
     790,  2042,   277,   666,   501,     0,     0,     0,   371,     0,
       0,     0,   286,     0,     0,     0,     0,     0,   120,     0,
     437,  1482,     0,   371,     0,   371,     0,     0,     0,   371,
     371,   501,   371,  1496,     0,   152,   664,   177,   178,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   174,   371,
       0,     0,     0,     0,  1542,     0,     0,     0,     0,   120,
       0,   174,     0,   174,     0,   367,     0,   499,     0,     0,
       0,     0,     0,     0,     0,  2100,     0,     0,  1567,     0,
       0,     0,     0,     0,     0,   515,     0,     0,     0,   120,
     120,     0,     0,   277,   174,     0,   393,   791,   792,   793,
     794,   795,   796,   797,   798,   799,   800,   801,   120,   371,
       0,     0,   224,   119,     0,     0,     0,     0,   371,     0,
     120,   393,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   666,     0,     0,   288,   367,     0,   802,
     367,   367,   152,   367,   177,   178,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,   367,     0,
       0,     0,   367,   437,   174,     0,   367,     0,   174,     0,
       0,   174,   174,     0,     0,   174,     0,   232,   174,   174,
       0,   174,   152,   174,   177,   178,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,   501,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     0,     0,   114,   114,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   437,     0,     0,     0,
       0,     0,     0,     0,   174,     0,     0,   174,     0,     0,
       0,     0,     0,     0,     0,     0,   371,     0,     0,     0,
     371,     0,     0,   499,     0,   371,   371,     0,   367,   371,
       0,     0,   174,     0,  1756,   437,   437,   437,     0,   371,
     180,   183,   437,   437,     0,     0,   371,   174,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1743,  1744,     0,     0,     0,   437,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   233,
       0,   371,   367,     0,     0,     0,     0,     0,     0,     0,
       0,   367,     0,     0,   371,   367,     0,     0,   371,     0,
       0,     0,   371,     0,     0,     0,     0,   437,   437,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     326,     0,     0,   327,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,   352,     0,
       0,     0,   174,  1756,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   401,     0,
       0,     0,     0,     0,     0,   119,     0,   286,     0,     0,
     401,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     288,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   393,     0,   532,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   666,     0,     0,     0,   174,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1852,     0,     0,     0,     0,
       0,     0,  1862,   371,   437,   501,   233,     0,  1899,  1900,
       0,     0,     0,     0,     0,   593,   594,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,     0,     0,     0,
       0,     0,     0,  1887,     0,     0,     0,     0,     0,     0,
       0,   180,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   393,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   371,   643,     0,   371,   371,
       0,   371,     0,     0,   647,   649,     0,     0,     0,   656,
       0,     0,     0,     0,     0,     0,   371,     0,   114,     0,
     371,     0,   174,   174,   371,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,     0,   174,     0,     0,     0,   352,     0,
       0,   352,     0,  1982,   401,     0,     0,     0,     0,     0,
       0,     0,  1945,  1946,     0,     0,     0,     0,   119,  1956,
       0,     0,   119,   119,     0,     0,     0,     0,     0,     0,
       0,     0,  1970,  1756,     0,     0,   119,     0,     0,     0,
       0,     0,  1979,     0,  1980,     0,     0,     0,     0,     0,
       0,     0,   437,     0,     0,     0,     0,  1991,     0,  1993,
    1994,  1995,     0,     0,     0,     0,     0,     0,     0,     0,
    2023,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   501,     0,   664,     0,     0,   371,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2053,   233,
       0,     0,  2054,   174,   174,     0,     0,     0,     0,     0,
     174,   858,   859,     0,  2015,     0,     0,  2024,     0,     0,
       0,  2029,     0,     0,   114,   378,  2034,     0,     0,     0,
       0,     0,     0,     0,     0,   174,     0,     0,   174,   174,
     371,   174,     0,   174,   174,     0,     0,     0,     0,   371,
       0,     0,     0,   371,   114,   664,     0,     0,     0,     0,
       0,     0,   408,   488,   378,   409,     0,   410,   411,     0,
     412,     0,     0,   367,     0,     0,     0,     0,     0,     0,
    2077,     0,     0,     0,     0,   114,   174,   413,     0,     0,
     174,     0,  2086,     0,   174,     0,  2089,     0,     0,     0,
     560,     0,     0,     0,     0,     0,     0,   560,     0,     0,
       0,  2105,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,   288,   420,   421,   422,     0,
     423,   424,     0,     0,     0,     0,     0,     0,    74,     0,
     953,     0,     0,  2136,     0,     0,     0,     0,   437,   352,
       0,     0,     0,     0,     0,     0,     0,     0,   174,     0,
     425,     0,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,   560,
       0,     0,     0,  2159,     0,     0,     0,     0,  2162,   437,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   401,     0,     0,   378,   676,     0,
       0,     0,     0,     0,     0,     0,  2182,     0,     0,  2184,
       0,  2162,     0,     0,     0,     0,     0,   697,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2184,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1055,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   437,
       0,   437,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1967,     0,     0,   119,     0,     0,   174,
       0,     0,     0,     0,     0,     0,     0,   560,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   437,     0,   560,   825,     0,   560,   828,     0,
       0,     0,     0,     0,     0,     0,     0,   378,     0,     0,
       0,   676,     0,   172,   174,     0,   174,  1134,     0,   174,
       0,   437,   174,     0,   488,     0,   174,     0,  1150,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   560,     0,     0,     0,   560,
       0,     0,     0,   437,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   666,     0,     0,     0,     0,     0,     0,     0,     0,
     306,     0,     0,     0,     0,     0,     0,     0,     0,   378,
       0,     0,     0,   312,     0,   313,     0,  1218,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,   666,     0,   560,     0,     0,     0,     0,
       0,     0,  1325,     0,   174,     0,     0,     0,     0,     0,
       0,   371,     0,     0,     0,   964,   378,     0,     0,     0,
       0,     0,     0,   119,     0,     0,   676,     0,     0,     0,
     676,     0,     0,     0,     0,     0,     0,   982,     0,   378,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   562,   563,     0,     0,   567,     0,     0,
     570,   571,     0,   573,     0,   574,     0,     0,     0,     0,
       0,     0,     0,     0,   174,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,     0,     0,   174,     0,
     174,   174,     0,     0,     0,   188,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,     0,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,   263,   264,
       0,   265,    46,   219,    47,   378,     0,   266,     0,     0,
      49,    50,    51,    52,    53,    54,    55,     0,     0,   281,
       0,   560,   560,     0,   659,     0,     0,     0,     0,     0,
       0,     0,   560,  1100,     0,   560,  1103,     0,     0,   691,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   964,
     378,     0,     0,     0,   676,     0,   676,   676,     0,   174,
       0,     0,     0,   676,     0,     0,     0,     0,     0,   378,
     219,   378,     0,     0,   342,   378,   378,   378,   378,     0,
       0,     0,     0,     0,     0,     0,   383,     0,     0,     0,
       0,     0,  1486,  1488,  1490,   378,     0,   560,     0,     0,
       0,   560,     0,     0,     0,     0,     0,     0,   560,  1178,
    -475,   219,   560,  1182,     0,     0,   560,  1186,     0,     0,
       0,     0,     0,  1189,     0,   508,  1512,     0,     0,     0,
     514,     0,     0,  -475,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   823,     0,     0,  1218,     0,     0,
       0,     0,     0,  1531,     0,     0,     0,     0,     0,     0,
       0,     0,   174,     0,   378,   560,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   219,     0,     0,     0,
     174,     0,   676,     0,     0,     0,     0,     0,     0,     0,
       0,   281,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     906,     0,     0,   174,     0,     0,     0,     0,     0,   174,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   488,   378,     0,     0,     0,   514,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   219,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   669,     0,
     686,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   560,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   378,   378,     0,     0,   676,   676,     0,     0,     0,
       0,     0,   676,  1682,  1683,     0,     0,     0,     0,     0,
       0,   747,     0,     0,   980,   981,     0,     0,     0,     0,
       0,     0,     0,   174,   174,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   990,     0,   992,     0,     0,     0,
       0,     0,     0,     0,     0,   219,     0,   378,     0,   174,
     174,   560,  1430,     0,   560,  1434,     0,   393,   560,  1438,
       0,     0,   174,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   669,     0,
       0,     0,     0,     0,   853,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1774,     0,     0,     0,     0,
     188,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,   219,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,  1093,  1094,    46,     0,    47,
       0,     0,  1098,     0,     0,     0,     0,     0,     0,     0,
     174,     0,     0,   219,   219,     0,     0,     0,     0,    58,
     508,     0,     0,     0,     0,     0,     0,  1121,     0,     0,
    1124,  1125,     0,  1128,     0,  1130,  1131,     0,     0,     0,
     378,     0,     0,     0,     0,     0,   676,  1550,     0,     0,
       0,     0,     0,   725,     0,   726,   727,     0,     0,     0,
       0,   301,     0,     0,  1845,     0,     0,     0,     0,     0,
       0,   378,     0,     0,     0,     0,     0,     0,  1176,     0,
       0,   174,  1180,    75,     0,     0,  1184,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   508,     0,   968,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     -17,     0,     0,     0,     0,   374,   560,  1604,     0,     0,
     669,     0,     0,     0,     0,     0,     0,   560,  1613,     0,
     676,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   378,   219,     0,   378,   378,     0,   378,     0,     0,
     219,     0,     0,   747,   374,   747,   219,     0,   219,     0,
    1316,     0,     0,     0,     0,     0,     0,   747,     0,     0,
     747,   747,   747,   188,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,   174,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -500,  -500,     0,  -500,
      46,     0,    47,     0,     0,  -500,   508,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
     219,     0,     0,     0,   374,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   508,     0,     0,     0,     0,     0,   378,    63,    64,
       0,     0,     0,  1997,     0,     0,     0,     0,     0,     0,
     508,     0,   508,     0,     0,     0,   508,   508,   383,   508,
       0,  1316,     0,     0,    74,   676,    75,   374,     0,   374,
     374,     0,     0,     0,     0,     0,   508,     0,     0,     0,
       0,     0,     0,   374,     0,     0,     0,   374,     0,    78,
     269,     0,     0,     0,     0,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,  1423,     0,  1425,     0,
       0,  1428,     0,     0,  1432,     0,     0,     0,  1436,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1552,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,   508,     0,     0,     0,     0,
       0,     0,   219,     0,   560,     0,     0,     0,     0,     0,
       0,     0,   853,     0,     0,     0,     0,     0,     0,     0,
       0,   560,     0,     0,   408,     0,     0,   409,     0,   410,
     411,     0,   412,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,    58,   413,
       0,     0,     0,   374,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   383,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,     0,  1548,     0,     0,     0,
      74,     0,    75,     0,     0,   373,     0,     0,     0,     0,
       0,   374,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   425,     0,     0,    78,   426,     0,     0,   374,
       0,     0,   427,  1553,    81,   428,   429,   430,   431,     0,
       0,     0,   508,   508,   373,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   560,   560,     0,     0,     0,     0,
       0,     0,     0,     0,   374,     0,  1602,     0,     0,     0,
     560,     0,     0,     0,     0,     0,     0,  1611,     0,     0,
    1615,     0,  1618,  1619,     0,     0,     0,   374,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   508,     0,
       0,     0,     0,     0,     0,   374,   374,     0,   374,     0,
       0,     0,     0,     0,     0,     0,   374,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
       0,     0,   374,     0,     0,     0,     0,     0,     0,   374,
       0,     0,   374,     0,   373,     0,     0,     0,     0,   747,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     560,     0,     0,     0,     0,     0,     0,     0,   560,     0,
       0,     0,     0,     0,   747,     0,     0,     0,     0,     0,
       0,  1737,     0,     0,     0,     0,     0,   373,     0,   373,
     373,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,   281,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   219,   374,     0,     0,     0,     0,
       0,   669,     0,   560,  2075,     0,     0,   560,     0,     0,
       0,   374,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,     0,     0,
       0,   374,   383,     0,     0,     0,     0,   747,     0,   374,
     374,     0,     0,     0,   374,     0,     0,     0,     0,     0,
       0,   560,     0,     0,  1615,     0,     0,     0,     0,   374,
       0,   374,     0,     0,     0,   374,   374,   374,   374,     0,
       0,   743,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1807,     0,     0,   374,     0,   373,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,     0,     0,
       0,     0,   508,     0,     0,   508,   508,     0,   383,   560,
     560,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,     0,   560,     0,     0,
     747,   747,   747,     0,   374,   747,   747,   375,     0,     0,
       0,   373,   514,     0,     0,     0,     0,     0,     0,   374,
       0,   374,   374,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     219,     0,     0,     0,     0,     0,   375,  1896,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,     0,
       0,   281,     0,   917,   919,     0,     0,     0,     0,     0,
       0,     0,   374,     0,     0,     0,     0,   373,   383,     0,
       0,     0,     0,     0,     0,  1924,  1925,     0,     0,     0,
       0,     0,     0,     0,     0,   373,   373,     0,   373,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,  1939,  1940,     0,     0,     0,     0,     0,     0,   373,
       0,     0,   373,     0,  1944,     0,     0,     0,     0,   373,
       0,     0,   373,     0,     0,     0,   375,     0,     0,     0,
       0,     0,   374,     0,     0,     0,   374,     0,     0,     0,
       0,   374,   374,     0,     0,   374,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,     0,     0,     0,     0,
       0,     0,   374,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   375,
       0,   375,   375,     0,     0,     0,     0,     0,     0,     0,
     743,   219,     0,   743,     0,   375,     0,   374,   743,   375,
       0,     0,     0,     0,     0,     0,     0,   743,     0,     0,
     374,     0,  2012,     0,   374,   373,     0,     0,   374,     0,
       0,   281,     0,     0,     0,     0,   743,     0,     0,     0,
       0,   373,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,   373,     0,     0,     0,     0,     0,     0,     0,   373,
     373,     0,  1080,     0,   373,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,   373,     0,  2073,     0,   373,   373,   373,   373,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,   375,
       0,   747,     0,     0,     0,   375,     0,     0,     0,     0,
       0,     0,   461,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   493,     0,     0,     0,
     374,     0,     0,     0,   374,     0,     0,     0,     0,     0,
       0,     0,   523,     0,   523,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,   374,
       0,   374,     0,     0,   373,   281,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,     0,     0,     0,   373,
       0,   373,   373,     0,     0,    14,    15,    16,    17,    18,
       0,   375,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -500,
    -500,     0,  -500,    46,     0,    47,   375,     0,  -500,     0,
       0,   374,     0,     0,   374,   374,     0,   374,     0,     0,
       0,     0,   373,     0,     0,    58,   638,     0,     0,   375,
       0,     0,   374,     0,     0,     0,   374,     0,     0,     0,
     374,     0,     0,     0,     0,     0,     0,   375,   375,     0,
     375,     0,     0,     0,     0,     0,     0,     0,   375,   152,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   375,     0,     0,   375,     0,     0,     0,   747,     0,
       0,   375,     0,     0,   375,     0,     0,    74,     0,    75,
       0,     0,   373,     0,     0,     0,   373,     0,     0,     0,
       0,   373,   373,     0,     0,   373,     0,     0,     0,    76,
      77,     0,    78,   511,     0,   373,     0,     0,     0,     0,
      80,    81,   373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,     0,     0,
     747,     0,   374,   514,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,   373,     0,     0,   375,   373,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,     0,     0,   375,
       0,     0,     0,   375,     0,   374,     0,   523,     0,   374,
       0,   375,   375,   523,     0,     0,   375,     0,   461,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,   375,     0,     0,     0,   375,   375,   375,
     375,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   743,     0,     0,     0,     0,     0,
     373,     0,     0,     0,   373,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,     0,     0,
     409,     0,   410,   411,     0,   412,     0,   375,     0,   373,
     948,   373,     0,     0,     0,     0,   375,     0,     0,     0,
       0,     0,   413,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,   375,   375,     0,     0,     0,   493,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   976,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,     0,   423,   424,     0,     0,     0,
       0,   373,     0,    74,   373,   373,     0,   373,  1009,     0,
       0,     0,     0,     0,   375,     0,     0,     0,     0,     0,
       0,  1020,   373,     0,     0,   425,   373,     0,    78,   426,
     373,     0,     0,     0,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,  1041,  1043,     0,  1005,  1045,
       0,  1047,     0,     0,     0,     0,     0,  1009,     0,  1057,
    1009,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,  1084,   375,     0,
       0,     0,     0,   375,   375,     0,     0,   375,     0,     0,
    1086,     0,     0,     0,     0,     0,     0,   375,     0,     0,
       0,  1095,     0,     0,   375,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1689,  1697,   493,     0,  1689,
    1708,     0,  1084,     0,     0,  1715,     0,   373,     0,  1719,
       0,  1721,   373,  1708,     0,     0,     0,     0,     0,   375,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,     0,  1154,     0,   375,   523,     0,     0,
     375,     0,     0,     0,     0,     0,     0,     0,  1165,     0,
       0,     0,     0,     0,     0,     0,     0,   374,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,     0,   268,     0,     0,   373,  1190,     0,     0,   373,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -500,  -500,     0,  -500,
      46,     0,    47,   461,     0,  -500,     0,     0,     0,   374,
       0,     0,   374,  1306,  1308,     0,     0,     0,     0,     0,
       0,   493,    58,     0,     0,     0,     0,   374,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1811,   375,     0,     0,     0,   375,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,   375,  1084,     0,     0,     0,     0,     0,
       0,     0,  1350,     0,    74,     0,    75,  1848,     0,     0,
       0,  1009,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1868,  1870,    78,
     511,     0,     0,     0,     0,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   523,     0,     0,     0,     0,     0,  1890,     0,
       0,     0,     0,   375,     0,     0,   375,   375,     0,   375,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,     0,   375,     0,
       0,     0,   375,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   268,
     188,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,   523,    20,  1422,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -500,  -500,     0,  -500,    46,     0,    47,
       0,     0,  -500,     0,     0,     0,     0,  1954,     0,     0,
       0,     0,     0,     0,     0,  1957,     0,  1959,     0,    58,
    1963,  1969,     0,  1708,     0,     0,     0,     0,  1975,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   375,
       0,     0,     0,     0,   375,     0,     0,     0,     0,  1499,
    1499,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,     0,     0,     0,     0,     0,    78,   375,     0,     0,
       0,   375,     0,     0,     0,     0,  1546,     0,     0,     0,
       0,  2040,  1555,     0,     0,     0,     0,     0,  2047,  2049,
       0,     0,     0,     0,     0,     0,     0,     0,  1009,     0,
       0,     0,     0,   493,     0,     0,     0,     0,  2068,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,   523,   373,     0,  1584,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,  1041,
       0,     0,     0,     0,     0,     0,     0,  2090,     0,  2093,
       0,     0,  2095,  2097,     0,     0,     0,     0,     0,  2102,
    2104,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   408,     0,     0,   409,     0,   410,
     411,     0,   412,  1646,  1647,     0,     0,     0,     0,     0,
       0,     0,     0,  2143,  2145,  2147,     0,     0,    58,   413,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1009,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2168,  2170,  2172,     0,   523,   414,
     415,   461,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,  2183,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1481,     0,     0,  1701,  1702,  1703,  1704,  1043,
       0,     0,   425,  1962,     0,    78,   426,     0,  1764,  1765,
       0,     0,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,   408,     0,  1779,   409,     0,   410,   411,
       0,   412,     0,     0,     0,     0,     0,     0,   523,     0,
       0,     0,  1041,     0,     0,     0,  1221,     0,   413,  1223,
       0,  1224,  -253,  -253,  1225,  1226,  1227,  1228,  1229,  1230,
    1231,  1232,  1233,  1234,  1235,  1236,  -352,  -352,  1237,  1238,
    1239,  1240,  1241,  1242,  1243,     0,  1244,     0,   414,   415,
       0,   517,   417,  1245,  1246,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,  1247,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   461,     0,     0,     0,     0,     0,  1836,   375,
    -253,  1248,     0,     0,    78,   426,     0,     0,     0,   310,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,     0,  -193,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1883,
       0,     0,     0,     0,  2183,     0,     0,     0,     0,     0,
       0,   375,     0,     0,   375,     0,     0,     0,     0,     0,
       0,  1481,     0,     0,     0,     0,     0,     0,     0,   375,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   523,     0,     0,     0,     0,     0,     0,     0,
    1914,     0,   408,  1916,     0,   409,     0,   410,   411,     0,
     412,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1221,     0,   413,  1223,  1930,
    1224,  -254,  -254,  1225,  1226,  1227,  1228,  1229,  1230,  1231,
    1232,  1233,  1234,  1235,  1236,  -352,  -352,  1237,  1238,  1239,
    1240,  1241,  1242,  1243,     0,  1244,     0,   414,   415,     0,
     517,   417,  1245,  1246,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,  1247,   420,   421,   422,     0,
     423,   424,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -254,
    1248,     0,     0,    78,   426,     0,     0,     0,   310,     0,
     427,    80,    81,   428,   429,   430,   431,     0,  1886,     0,
       0,     0,     0,     0,     0,  -193,     0,    14,    15,    16,
      17,    18,     0,     0,    20,  1481,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -499,  -499,     0,  -499,    46,   408,    47,     0,   409,
    -499,   410,   411,     0,   412,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,  1221,
       0,   413,  1223,     0,  1224,     0,  1009,  1225,  1226,  1227,
    1228,  1229,  1230,  1231,  1232,  1233,  1234,  1235,  1236,  -352,
    -352,  1237,  1238,  1239,  1240,  1241,  1242,  1243,     0,  1244,
       0,   414,   415,     0,   517,   417,  1245,  1246,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,  1247,
     420,   421,   422,     0,   423,   424,     0,     0,     0,     0,
       0,    75,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1248,     0,     0,    78,   426,     0,
       0,     0,   310,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,     0,  -193,
       4,   188,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1220,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   408,     0,    46,   409,
      47,   410,   411,     0,   412,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1221,
      58,  1222,  1223,     0,  1224,     0,     0,  1225,  1226,  1227,
    1228,  1229,  1230,  1231,  1232,  1233,  1234,  1235,  1236,  -352,
    -352,  1237,  1238,  1239,  1240,  1241,  1242,  1243,     0,  1244,
       0,   414,   415,    61,   517,   417,  1245,  1246,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,  1247,
     420,   421,   422,     0,   423,   424,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -3,  1248,     0,     0,    78,  1249,     0,
       0,     0,   310,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,     0,  -193,
       4,   188,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1220,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   408,     0,    46,   409,
      47,   410,   411,     0,   412,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1221,
      58,  1222,  1223,     0,  1224,     0,     0,  1225,  1226,  1227,
    1228,  1229,  1230,  1231,  1232,  1233,  1234,  1235,  1236,  -352,
    -352,  1237,  1238,  1239,  1240,  1241,  1242,  1243,     0,  1244,
       0,   414,   415,    61,   517,   417,  1245,  1246,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,  1247,
     420,   421,   422,     0,   423,   424,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1248,     0,     0,    78,  1249,     0,
       0,     0,   310,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,     0,  -193,
       4,   188,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   408,     0,    46,   409,
      47,   410,   411,     0,   412,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   413,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   414,   415,    61,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,     0,   423,   424,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1701,  1702,  1703,
    1704,     0,     0,     0,   425,  1705,  1706,    78,  1249,     0,
       0,     0,     0,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,     0,  1707,
       4,   188,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   408,     0,    46,   409,
      47,   410,   411,     0,   412,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   413,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   414,   415,    61,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,     0,   423,   424,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1701,  1702,  1703,
    1704,     0,     0,     0,   425,  1705,     0,    78,  1249,     0,
       0,     0,     0,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,     0,  1707,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,    59,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    61,    62,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,     0,    78,    79,     0,
       0,     0,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    82,     0,    83,
     268,   188,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -500,  -500,     0,  -500,    46,     0,
      47,     0,     0,  -500,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   152,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,     0,    78,   269,     0,
       0,     0,  -828,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    82,   268,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -500,  -500,     0,  -500,    46,     0,    47,     0,
       0,  -500,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   152,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,   269,     0,     0,     0,
       0,     0,     0,    80,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     359,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   152,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
     617,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1110,
      77,  -694,    78,   672,     0,     0,     0,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    82,   188,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -500,  -500,     0,  -500,
      46,     0,    47,     0,     0,  -500,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   152,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,    78,
     269,     0,     0,     0,  -832,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    82,
     188,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -500,  -500,     0,  -500,    46,     0,    47,
       0,     0,  -500,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   152,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,    77,     0,    78,   269,     0,     0,
       0,     0,     0,     0,    80,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    82,     4,   188,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   408,     0,    46,   409,    47,   410,   411,
       0,   412,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,   413,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   414,   415,
      61,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   425,     0,  1699,    78,  1249,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,     4,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   408,     0,    46,   409,    47,   410,
     411,     0,   412,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,   413,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
     415,    61,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   425,     0,     0,    78,  1249,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   408,     0,    46,   409,    47,   410,
     411,     0,   412,   359,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   413,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   425,     0,     0,    78,   490,     0,     0,     0,
       0,     0,   427,   491,    81,   428,   429,   430,   431,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   408,     0,    46,   409,    47,   410,
     411,     0,   412,   359,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   413,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   425,     0,     0,    78,  1303,     0,     0,     0,
       0,     0,   427,  1304,    81,   428,   429,   430,   431,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   408,     0,    46,   409,    47,   410,
     411,     0,   412,   359,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   413,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   425,     0,     0,    78,   837,     0,     0,     0,
       0,     0,   427,   491,    81,   428,   429,   430,   431,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   408,     0,    46,   409,    47,   410,
     411,     0,   412,   359,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   413,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   425,     0,     0,    78,   426,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   408,     0,    46,   409,    47,   410,
     411,     0,   412,   359,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   413,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   425,     0,     0,    78,   837,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,  2022,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,
       0,    -2,     0,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,
       0,    -2,     0,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,    -2,    -2,  2052,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,
      -2,     0,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,
      -2,     0,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,    -2,    -2,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   152,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,     0,    78,    79,     0,     0,     0,  -830,
       0,     0,    80,    81,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,    82,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     152,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,   211,     0,     0,     0,     0,     0,
       0,    80,    81,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,    82,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   152,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
      77,     0,    78,    79,     0,     0,     0,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    82,     4,   188,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,     0,     0,     0,     0,  -419,  -419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -419,     0,     0,     0,
      78,    79,     0,     0,     0,     0,     0,     0,    80,    81,
       4,   188,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,     0,     0,     0,     0,  -420,  -420,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -420,     0,     0,     0,    78,    79,     0,
    1457,     0,  1458,     0,     0,    80,    81,  1459,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1460,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1461,     0,     0,     0,    78,  1015,     0,
    1457,     0,  1458,     0,     0,    80,    81,  1459,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1460,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1655,     0,     0,     0,    78,  1015,     0,
    1457,     0,  1458,     0,     0,    80,    81,  1459,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1460,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1656,     0,     0,     0,    78,  1015,     0,
    1457,     0,  1458,     0,     0,    80,    81,  1459,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1460,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1657,     0,     0,     0,    78,  1015,     0,
       0,     0,     0,     0,     0,    80,    81,   268,   188,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -500,  -500,     0,  -500,    46,     0,    47,     0,     0,
    -500,     0,   188,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,     0,   359,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   269,     0,    63,    64,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,   617,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   671,     0,  -694,    78,   672,
       0,     0,     0,     0,     0,     0,    80,    81,   188,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   359,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,   617,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   806,     0,  -694,    78,   557,     0,     0,     0,     0,
       0,     0,    80,    81,   188,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,   359,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,  1143,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -702,
      78,   923,     0,     0,     0,     0,     0,     0,    80,    81,
     188,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,   359,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   360,    78,   361,     0,     0,
       0,     0,     0,     0,    80,    81,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     359,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
    1625,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   923,     0,     0,     0,     0,     0,     0,
      80,    81,   188,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,   359,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,  1627,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   923,
       0,     0,     0,     0,     0,     0,    80,    81,   188,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   359,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   511,     0,     0,     0,     0,
       0,     0,    80,    81,   188,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,   359,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   923,     0,     0,     0,     0,     0,     0,    80,    81,
     188,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,   359,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,     0,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,     0,     0,     0,
       0,     0,  1481,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,     0,     0,   409,     0,   410,   411,
       0,   412,     0,     0,     0,     0,    78,   361,     0,     0,
       0,     0,     0,     0,    80,    81,  1221,     0,   413,  1223,
       0,  1224,  1947,  1948,  1225,  1226,  1227,  1228,  1229,  1230,
    1231,  1232,  1233,  1234,  1235,  1236,     0,    75,  1237,  1238,
    1239,  1240,  1241,  1242,  1243,     0,  1244,     0,   414,   415,
       0,   517,   417,  1245,  1246,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,  1247,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,  1481,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1248,     0,     0,    78,   426,     0,     0,     0,   310,
       0,   427,    80,    81,   428,   429,   430,   431,     0,   408,
       0,     0,   409,     0,   410,   411,  -193,   412,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1221,     0,   413,  1223,     0,  1224,     0,     0,
    1225,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,  1234,
    1235,  1236,     0,     0,  1237,  1238,  1239,  1240,  1241,  1242,
    1243,     0,  1244,     0,   414,   415,     0,   517,   417,  1245,
    1246,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,  1247,   420,   421,   422,     0,   423,   424,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1248,     0,     0,
      78,   426,     0,     0,     0,   310,     0,   427,    80,    81,
     428,   429,   430,   431,     0,     0,     0,     0,     0,     0,
       0,     0,  -193,   314,   188,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -423,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,     0,     0,     0,     0,  -423,   314,   188,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -424,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,     0,     0,     0,     0,  -424,   314,
     188,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,    14,    15,    16,    17,    18,    19,   734,    20,   735,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    63,    64,   408,     0,    46,
     409,    47,   410,   411,     0,   412,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   413,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   736,     0,     0,     0,     0,  1236,
       0,  -352,     0,     0,     0,     0,    78,     0,     0,     0,
       0,  -423,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,     0,   423,   424,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1248,     0,     0,    78,   737,
       0,     0,     0,   310,     0,   427,    80,    81,   738,   739,
     430,   431,    14,    15,    16,    17,    18,    19,   734,    20,
     735,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   408,     0,
      46,   409,    47,   410,   411,     0,   412,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   413,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   736,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,     0,   423,   424,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   425,     0,     0,    78,
     737,     0,     0,     0,   310,     0,   427,    80,    81,   738,
     739,   430,   431,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   408,
       0,    46,   409,    47,   410,   411,     0,   412,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   413,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,     0,   423,   424,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   425,     0,   456,
      78,   457,     0,     0,     0,     0,     0,   427,    80,    81,
     428,   429,   430,   431,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     408,     0,    46,   409,    47,   410,   411,     0,   412,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   413,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,     0,   423,   424,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   425,     0,
       0,    78,   457,     0,     0,     0,   310,     0,   427,    80,
      81,   428,   429,   430,   431,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   408,     0,    46,   409,    47,   410,   411,     0,   412,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   413,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,     0,   423,
     424,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   425,
       0,     0,    78,   737,     0,     0,     0,   310,     0,   427,
      80,    81,   428,   429,   430,   431,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   408,     0,    46,   409,    47,   410,   411,     0,
     412,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   413,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,     0,
     423,   424,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     425,     0,     0,    78,   457,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   408,     0,    46,   409,    47,   410,   411,
       0,   412,   359,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   413,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   425,     0,     0,    78,   837,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   408,     0,    46,   409,    47,   410,
     411,     0,   412,   359,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   413,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   425,     0,     0,    78,   426,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,   359,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   152,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   617,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -694,    78,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,   359,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     152,     0,   484,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     860,     0,     0,    78,   485,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   152,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,    79,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   152,     0,   484,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   485,     0,
       0,     0,     0,     0,     0,    80,    81,   188,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,   359,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   617,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -694,    78,   188,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,   359,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,  1214,     0,
       0,     0,     0,   188,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
      78,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   359,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     860,     0,     0,    78,   485,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,   359,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   860,     0,     0,
      78,   485,     0,    63,    64,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1022,    78,  1015,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
    1572,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,  1015,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   322,
       0,    63,    64,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   211,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   359,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
       0,   359,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     361,     0,    63,    64,     0,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   322,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,   359,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   485,     0,     0,     0,     0,     0,     0,    80,    81,
     188,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -500,  -500,     0,  -500,    46,     0,    47,
       0,     0,  -500,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,     0,   359,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   511,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,  1015,     0,
      63,    64,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,    79,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,     0,
     359,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   485,
       0,    63,    64,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,  1015,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   359,    49,    50,
      51,    52,    53,    54,    55,     0,     0,    14,    15,    16,
      17,    18,    58,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -500,  -500,     0,  -500,    46,     0,    47,    63,    64,
    -500,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
       0,     0,     0,    63,    64,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   339,     0,    14,    15,    16,
      17,    18,    80,    81,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -500,  -500,     0,  -500,    46,     0,    47,     0,     0,
    -500,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,    58,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -500,  -500,     0,  -500,
      46,     0,    47,    63,    64,  -500,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   511,     0,     0,    63,    64,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
       0,     0,     0,     0,     0,     0,     0,    80,    81,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   408,     0,
      46,   409,    47,   410,   411,     0,   412,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   413,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,     0,   423,   424,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   425,     0,     0,    78,
     426,     0,     0,     0,     0,     0,   427,   491,    81,   428,
     429,   430,   431,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   408,     0,    46,   409,    47,   410,   411,     0,
     412,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,     0,
     423,   424,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     425,     0,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   152,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,    78,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,     0,   359,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,    20,    78,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -500,  -500,     0,  -500,    46,     0,    47,
       0,     0,  -500,     0,   188,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   189,     0,   190,
     191,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,    75,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,   408,     0,     0,   409,     0,   410,   411,     0,
     412,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   725,     0,   726,   727,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,     0,
     423,   424,     0,    75,   408,     0,     0,   409,    74,   410,
     411,     0,   412,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1701,  1702,  1703,  1704,     0,     0,   413,
     425,  1869,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   517,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,   408,   423,   424,   409,     0,   410,   411,     0,   412,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   413,     0,     0,     0,
       0,     0,   425,    77,     0,   518,   519,     0,     0,     0,
     520,     0,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,   408,   423,
     424,   409,     0,   410,   411,     0,   412,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   413,     0,     0,     0,     0,     0,   425,
    1353,     0,    78,   426,     0,     0,     0,  1354,     0,   427,
      80,    81,   428,   429,   430,   431,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,   408,   423,   424,   409,     0,
     410,   411,     0,   412,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,     0,     0,   425,     0,     0,    78,
     426,     0,     0,     0,   520,     0,   427,    80,    81,   428,
     429,   430,   431,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,   408,   423,   424,   409,     0,   410,   411,     0,
     412,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,   425,  1040,     0,    78,   426,     0,     0,
       0,     0,     0,   427,    80,    81,   428,   429,   430,   431,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,   408,
     423,   424,   409,     0,   410,   411,     0,   412,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
     425,     0,     0,    78,   426,     0,     0,     0,   310,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,   408,   423,   424,   409,
       0,   410,   411,     0,   412,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   413,     0,     0,     0,     0,     0,   425,     0,     0,
      78,   426,     0,     0,  1079,     0,     0,   427,    80,    81,
     428,   429,   430,   431,     0,     0,     0,     0,     0,     0,
       0,   414,   415,     0,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,   408,   423,   424,   409,     0,   410,   411,
       0,   412,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   413,     0,
       0,     0,     0,     0,   425,     0,     0,    78,   426,     0,
       0,     0,  1491,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
     408,   423,   424,   409,     0,   410,   411,     0,   412,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   413,     0,     0,     0,     0,
       0,   425,  1583,     0,    78,   426,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,   408,   423,   424,
     409,     0,   410,   411,     0,   412,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   413,     0,     0,     0,     0,     0,   425,     0,
       0,    78,   426,     0,     0,     0,  1775,     0,   427,    80,
      81,   428,   429,   430,   431,     0,     0,     0,     0,     0,
       0,     0,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,   408,   423,   424,   409,     0,   410,
     411,     0,   412,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,     0,     0,     0,     0,   425,     0,  1953,    78,   426,
       0,     0,     0,     0,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,   408,   423,   424,   409,     0,   410,   411,     0,   412,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   413,     0,     0,     0,
       0,     0,   425,  1958,     0,    78,   426,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,     0,   423,
     424,     0,     0,   408,     0,     0,   409,    74,   410,   411,
       0,   412,  2039,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   413,   425,
    1968,     0,    78,   426,     0,     0,     0,     0,     0,   427,
      80,    81,   428,   429,   430,   431,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
     408,   423,   424,   409,     0,   410,   411,     0,   412,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   413,     0,     0,     0,     0,
       0,   425,     0,     0,    78,   426,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,   408,   423,   424,
     409,     0,   410,   411,     0,   412,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   413,     0,     0,     0,     0,     0,   425,  2046,
       0,    78,   426,     0,     0,     0,     0,     0,   427,    80,
      81,   428,   429,   430,   431,     0,     0,     0,     0,     0,
       0,     0,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,   408,   423,   424,   409,     0,   410,
     411,     0,   412,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,     0,     0,     0,     0,   425,  2048,     0,    78,   426,
       0,     0,     0,     0,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,   408,   423,   424,   409,     0,   410,   411,     0,   412,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   413,     0,     0,     0,
       0,     0,   425,  2092,     0,    78,   426,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,   408,   423,
     424,   409,     0,   410,   411,     0,   412,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   413,     0,     0,     0,     0,     0,   425,
    2094,     0,    78,   426,     0,     0,     0,     0,     0,   427,
      80,    81,   428,   429,   430,   431,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,   408,   423,   424,   409,     0,
     410,   411,     0,   412,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,     0,     0,   425,  2096,     0,    78,
     426,     0,     0,     0,     0,     0,   427,    80,    81,   428,
     429,   430,   431,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,   408,   423,   424,   409,     0,   410,   411,     0,
     412,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,   425,  2101,     0,    78,   426,     0,     0,
       0,     0,     0,   427,    80,    81,   428,   429,   430,   431,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,   408,
     423,   424,   409,     0,   410,   411,     0,   412,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
     425,  2103,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,   408,   423,   424,   409,
       0,   410,   411,     0,   412,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   413,     0,     0,     0,     0,     0,   425,  2142,     0,
      78,   426,     0,     0,     0,     0,     0,   427,    80,    81,
     428,   429,   430,   431,     0,     0,     0,     0,     0,     0,
       0,   414,   415,     0,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,   408,   423,   424,   409,     0,   410,   411,
       0,   412,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   413,     0,
       0,     0,     0,     0,   425,  2144,     0,    78,   426,     0,
       0,     0,     0,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
     408,   423,   424,   409,     0,   410,   411,     0,   412,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   413,     0,     0,     0,     0,
       0,   425,  2146,     0,    78,   426,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,   408,   423,   424,
     409,     0,   410,   411,     0,   412,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   413,     0,     0,     0,     0,     0,   425,  2167,
       0,    78,   426,     0,     0,     0,     0,     0,   427,    80,
      81,   428,   429,   430,   431,     0,     0,     0,     0,     0,
       0,     0,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,   408,   423,   424,   409,     0,   410,
     411,     0,   412,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,     0,     0,     0,     0,   425,  2169,     0,    78,   426,
       0,     0,     0,     0,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,   408,   423,   424,   409,     0,   410,   411,     0,   412,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   413,     0,     0,     0,
       0,     0,   425,  2171,     0,    78,   426,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,   408,   423,
     424,   409,     0,   410,   411,     0,   412,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   413,     0,     0,     0,     0,     0,   425,
       0,     0,    78,   426,     0,     0,     0,     0,     0,   427,
      80,    81,   428,   429,   430,   431,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,   408,   423,   424,   409,     0,
     410,   411,     0,   412,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,     0,     0,   714,     0,     0,    78,
     426,     0,     0,     0,     0,     0,   427,    80,    81,   428,
     429,   430,   431,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,   408,   423,   424,   409,     0,   410,   411,     0,
     412,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,   717,     0,     0,    78,   426,     0,     0,
       0,     0,     0,   427,    80,    81,   428,   429,   430,   431,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,   408,
     423,   424,   409,     0,   410,   411,     0,   412,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
     722,     0,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,     0,   423,   424,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   731,     0,     0,
      78,   426,     0,     0,     0,     0,     0,   427,    80,    81,
     428,   429,   430,   431,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -500,  -500,
       0,  -500,    46,   408,    47,     0,   409,  -500,   410,   411,
       0,   412,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,   413,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,   408,    75,    74,
     409,     0,   410,   411,     0,   412,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   425,   413,     0,    78,   426,     0,     0,     0,     0,
       0,   427,   947,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,     0,   423,   424,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   425,     0,     0,    78,   426,
       0,     0,     0,     0,     0,   427,   491,    81,   428,   429,
     430,   431
};

static const yytype_int16 yycheck[] =
{
       1,   182,    76,     4,   141,    76,    87,   531,   671,   267,
      76,   425,   959,   760,   241,   364,   763,  1267,     1,  1204,
     170,   121,   241,   241,   839,     4,   991,   227,   145,   241,
     170,     1,   684,   172,   999,   236,  1372,  1054,  1248,    76,
     251,   842,   172,    59,  1819,   577,   578,   848,  1299,  1300,
       1,  1231,   741,   807,    76,    56,    57,   675,    59,   841,
       1,   262,   241,     4,   187,  1082,   520,     1,   225,   280,
     100,    90,    78,   154,   196,    76,    59,  1819,  1201,   671,
     291,    82,   841,   241,   671,  1208,    87,   170,  1947,    59,
    1819,   208,   241,   839,    95,     1,    76,   944,   427,   100,
     100,   157,   103,    76,   331,   283,   107,   209,    59,   101,
    1712,   136,   331,   331,   251,   839,   378,    87,    59,   331,
     382,   854,    85,   856,   198,   944,  1248,     1,   107,   590,
     100,   839,   198,   103,  1151,   151,    76,   107,   932,     0,
     601,    90,   241,   280,    76,   146,   165,   241,   149,   498,
     151,    73,   331,    59,   291,   180,   157,   126,   952,   163,
     182,   198,   103,   164,   102,  1288,   107,   241,   151,   121,
     241,   172,    91,   331,   706,   241,   839,   209,   182,   316,
     157,   151,   331,   184,   645,    59,   356,   677,   720,   270,
     182,   160,  1820,   138,   157,   196,   197,   198,    78,    79,
     151,   100,   182,  2083,   241,     1,   241,   208,     4,   161,
     151,     0,   242,   214,   163,   137,   165,     1,  1363,   157,
       4,  2080,   362,   841,   225,   198,   396,   197,   173,   230,
     164,  2111,   331,   107,   235,   236,   237,   331,   157,   186,
     241,   242,   242,  1708,   214,   151,   385,   839,  1951,  1585,
    1213,   138,   839,   392,  2134,   385,   113,   331,   198,   340,
     331,   262,  1864,    59,   112,   331,   198,   141,   241,   270,
     619,   272,   242,   163,   187,    59,     0,   151,   400,   136,
     281,   282,   814,   173,   285,   165,   173,   135,   318,   109,
     110,   292,    76,     1,   331,  1110,   331,  1044,    82,   305,
     270,   241,   332,    87,    20,   306,   307,  1935,   309,   241,
    1111,   107,   539,   314,   663,   285,   100,   318,   318,   103,
     539,   539,   157,   107,   425,    95,   526,   539,  1451,   151,
     331,   332,   332,   394,  1213,  1117,   397,  1091,   687,  2114,
     160,   342,  1031,   242,   285,   694,   181,   761,   318,   350,
     351,   173,   492,   843,   355,   151,  1321,   847,  1117,   977,
     539,   518,   332,   520,  1110,     1,   496,   151,   858,   859,
     157,   488,  2114,   157,   575,  1222,     1,   251,   459,   157,
     581,   539,  1567,   561,   385,  2114,  1110,   716,   172,   600,
     539,   569,   509,   394,   157,    73,   397,  1519,   660,   400,
    1522,  1523,  1110,  1222,  1869,  1870,   280,   163,    73,  1256,
     157,  1205,   639,   197,   198,   165,   157,   291,   156,   318,
     639,   639,   172,    59,   208,   163,   182,   639,  2056,  2132,
     214,   113,   956,   332,    59,   697,   162,  1256,   868,  1213,
     539,   225,   316,    10,   182,   539,   320,  1110,  1593,  1594,
    1595,   235,   236,   237,   136,   163,   164,   241,   242,   137,
     639,   163,  2165,   600,   165,   539,   163,   103,   929,   470,
     161,   172,   137,   539,   244,   166,   246,   159,   262,   157,
     182,   639,   584,   253,   163,   182,   270,  2115,    62,    63,
     639,  1454,  1455,  1456,   495,   496,   161,  1962,  1963,  1117,
     470,   285,   539,   182,   539,   182,   507,   508,    78,    79,
    1311,    87,   518,    76,   163,   151,   141,   518,  1110,   520,
     135,   551,   306,  1110,  2152,   309,   151,  1707,    91,   157,
     314,   157,  1712,   182,   318,   161,  1746,   165,   539,    10,
     639,   165,   157,   182,   684,   639,   161,   331,   332,   163,
     551,   551,   584,   168,   169,   138,   326,   327,   182,   661,
     861,   163,   863,   702,     1,   639,   539,   997,   362,   109,
     110,  1588,   834,   639,   575,  1454,  1455,  1456,   154,   880,
     581,   551,   583,   163,   378,   168,   169,   534,   382,   156,
      76,   165,   163,  1558,  1395,   165,   163,   165,    76,   539,
     167,   385,   639,   157,   639,   172,    92,   539,   182,     1,
     164,   641,     4,   714,   182,    93,   717,   966,   719,   195,
     163,   722,    59,   159,  1746,   111,   251,   163,   644,   661,
     731,  1085,   163,   734,   735,   736,   182,   163,   639,   182,
     641,   641,     1,   644,  1134,   646,  1897,   292,   949,   285,
     163,   182,   551,  1305,   655,   280,   182,   157,   659,   159,
    1479,   644,   307,    73,  1483,  1484,   291,    59,   163,   182,
     870,   641,     3,  1406,   644,    70,  1409,  1410,  1497,   163,
    1454,  1455,  1456,   319,  1864,   156,   470,   182,   790,   173,
     691,   316,     3,   644,   270,    87,   167,   154,   492,   163,
      59,   172,   964,   644,   141,   906,   856,   160,   165,   710,
     161,   841,   496,   870,   151,   166,   873,    59,   182,    87,
      62,    63,   862,    65,   181,  1472,   600,   137,   364,   852,
     163,   163,   100,   835,   518,   103,   520,   163,   644,   107,
     173,   173,   641,  1993,    73,   157,   163,   157,   165,   141,
     158,   161,   753,   145,   755,   539,   757,   165,   790,   151,
     761,   250,   154,   764,   340,   165,  1298,   551,   257,   157,
     644,    13,    14,    15,    16,    17,  1793,   165,  1795,   862,
     181,  1961,   141,   789,   172,   163,   362,   121,   789,   159,
     279,   575,   151,   163,  1974,   173,   159,   581,   159,   583,
     163,   290,   157,   835,  1769,  1106,  1107,  1754,   137,   170,
     171,   163,   109,   110,   251,   157,   208,   168,   131,   132,
      73,   173,   854,   593,   175,   176,   165,    73,   157,   197,
     862,    73,   161,   834,  1248,   179,    47,    48,   839,    50,
     841,   788,   159,   280,   159,    56,   214,   164,   644,    73,
      73,   159,   853,   992,   291,   639,   164,   641,  2038,   251,
     644,  1162,   498,   864,   177,   178,   660,   109,   110,   870,
     159,   160,   873,   643,   242,   659,   268,   159,   270,   316,
     159,   159,   164,   459,   137,   164,   164,  1027,   280,  1319,
     684,   137,   251,  1050,   159,   137,  1326,  1054,   163,   291,
    1354,    73,   270,   697,   157,   906,   157,   691,   161,  1026,
     161,   157,   157,   137,   137,   161,   492,   285,   160,   129,
     130,   280,   314,    59,   316,  1082,    62,    63,  1085,    65,
      13,  1658,   291,   157,   157,   162,  1663,   161,   161,   852,
     159,   942,   943,   944,  1027,   163,   160,   161,   340,  1379,
     164,   159,  1570,   161,   867,   531,   159,   316,  1158,    73,
    1309,   944,   157,   159,   159,   137,   161,   163,   159,   753,
    1494,   755,  1150,   757,   159,   600,   955,   157,   163,   159,
     764,   161,   159,   619,   159,   157,  1116,  1117,   157,   161,
     159,   157,   161,   944,  1151,  1027,   632,   998,   157,     3,
      22,   577,   578,   944,  1005,   789,    89,   157,   644,    13,
      14,    15,    16,    17,   955,   163,    73,   159,   157,   644,
     159,   163,   161,   137,   107,   157,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   661,  1491,   944,   157,
     834,   102,   159,   157,   157,  1459,   163,   161,  1867,  1050,
     834,   687,   162,  1054,   159,   839,  1314,   841,   163,   157,
      13,    14,    15,    16,    17,   133,   134,   459,   159,    73,
     944,   159,   163,   157,   157,   163,   565,   161,   157,   135,
     137,  1082,   161,   165,  1085,    90,   870,     1,   159,   873,
       4,  1292,   163,   159,   157,   157,   488,   163,    73,   161,
     157,   157,   470,   592,   161,   161,   165,    73,   684,  1110,
     599,   156,   168,   169,   603,  1116,  1117,   509,  1299,  1300,
      73,   165,   906,   170,   171,   181,   162,  1581,    73,  1559,
     706,   159,  1213,   137,   182,   163,   159,   172,   714,   159,
     163,  1665,   181,     3,   720,    59,   722,  1248,   944,   165,
    1151,   157,  1582,    13,    14,    15,    16,    17,   942,   955,
     159,   159,   137,   600,   163,  1305,  1079,   768,   769,   770,
     964,   137,    13,    14,    15,    16,    17,   159,   121,  1318,
     157,   163,   157,   551,   137,   138,   161,   168,   169,   103,
      73,   157,   137,   107,   157,   161,  1336,  1354,    13,    14,
      15,    16,    17,   163,  2023,   159,  1363,   644,   600,   163,
     835,   159,   157,    73,   169,   163,   161,   157,   159,    73,
     157,  1005,   163,   174,  1225,  1204,   159,  1228,  1229,  1230,
     163,   159,    73,   167,  2053,   163,  1237,   151,   814,  1222,
    1341,   600,  1989,   179,   158,  2119,   159,  1160,   135,  2123,
     163,  1334,   644,  1336,   137,  1256,  1406,   160,    73,    73,
    1410,  1262,  1412,  1204,  1442,  2084,  1050,    73,   162,   163,
    1054,  1222,  1412,  1256,   157,   159,  1277,   137,   161,  1280,
    1281,  1222,  1283,   137,   159,   644,   159,  1717,  1289,   159,
     163,  1292,   159,  1416,  1417,   209,   137,   138,  1082,    73,
     159,  1085,  1281,   157,  1336,  1256,    73,   161,   944,   159,
    1280,  1281,   159,   163,  1479,  1256,  1222,   161,  1483,   944,
     157,  1775,   137,   137,   162,   163,  1110,   162,   163,  1412,
     966,   137,  1116,  1117,  1491,   157,  1337,   162,   163,  1280,
    1281,  1771,  1479,   157,   162,   163,  1483,   161,  1222,   162,
    1256,   157,   138,  1354,   138,   161,   124,  1787,   126,   127,
     128,   163,  1363,   137,  1655,  1656,  1657,  1151,   162,   163,
     137,   285,   163,  1454,  1455,  1456,   164,  1409,  1459,  1460,
     956,   164,  1256,   157,  1210,  1211,  1212,   161,   413,   157,
     157,   163,   160,   161,   161,  1396,   135,   165,   166,   135,
     163,   164,  1542,   775,   776,   777,   778,  1281,  1204,  1677,
    1678,  1679,   157,   438,   439,   162,   163,  1187,   157,   162,
     163,   157,   161,   181,  1581,   161,  1222,    92,    93,   168,
     169,  1588,   168,   169,   459,  1465,  1593,  1594,  1595,   159,
    1563,   181,   931,  1670,   162,   163,   159,   150,   151,   152,
     153,  1670,  1670,   150,   151,   152,   153,   159,  1670,  1542,
    1256,   159,  1375,   159,  1465,  1465,   159,   492,  1469,  1470,
     173,   114,   115,   116,   117,   118,   173,  1907,  1457,   182,
     159,  1911,   162,   163,   159,  1281,   150,   151,   152,   153,
    1491,  1670,   159,  1277,   162,   163,  1280,  1281,   161,   163,
     162,   163,   162,  1416,  1417,   162,   163,   944,  1292,   173,
    1542,  1305,  1670,   165,  1515,  1516,  1457,   165,   182,   163,
     164,  1670,   162,   163,  1525,   162,   163,   165,  1479,   162,
     163,   165,  1483,  1484,   162,   163,   162,   163,  1479,   162,
     163,   165,  1483,  1484,   162,   163,  1497,    78,    79,   163,
     164,   159,   944,  1337,   163,  1525,  1497,  1364,  1365,    71,
    1561,   773,   774,   955,   771,   772,  1465,  1522,  1523,   182,
    1354,  1670,   162,   779,   780,   157,  1670,    79,   162,  1363,
    1581,  1678,  1679,    18,  1525,   944,  1222,  1588,  1567,   181,
     165,   165,  1593,  1594,  1595,   182,   159,  1222,   159,  1670,
     165,   165,   162,   162,    18,  1479,   162,  1834,   162,  1483,
    1484,   156,  1396,   159,    22,  1834,  1834,   159,  1775,   159,
    1256,   159,  1834,  1497,   159,   159,  1567,   159,  1658,   159,
     159,  1256,   159,  1663,  1026,  1670,  1793,   159,  1795,   159,
     159,   162,  1672,   156,  1280,   156,    71,   165,   165,   165,
    1563,   159,   159,   181,   159,  1834,   159,  1658,  1658,   159,
     156,  1457,  1663,  1663,   165,   163,   165,   159,   159,  1670,
     584,  1672,  1672,  1309,   163,   159,  1834,   159,   163,  1680,
     159,  1465,   162,  1479,  1785,  1834,   163,  1483,  1484,     4,
       5,     6,     7,     8,     9,    10,    11,    12,  1699,  1900,
     159,  1497,   159,   159,   159,  1706,   159,  1491,  2057,   163,
     159,   150,   151,   152,   153,   159,  1897,  1696,   159,   159,
     162,   159,  1298,   159,   163,   162,   159,   159,   159,  1305,
     644,   159,   162,   159,   173,  1834,  1737,   159,   159,   159,
    1834,  1525,   767,   182,   159,   163,   159,   661,   159,    14,
     162,    66,   156,   163,  1695,  1696,   163,   163,   163,  1658,
     156,   164,   157,   157,  1663,   157,   157,     1,   157,   157,
       4,  1567,   157,  1672,  1775,   157,   163,  1756,    13,    14,
      15,    16,    17,    18,   164,  1222,   162,   165,  1735,   182,
     156,   181,  1793,   209,  1795,   156,   182,  1581,   182,  1834,
     181,   165,   156,   163,  1588,   162,   159,  1837,   159,  1593,
    1594,  1595,  1204,   159,   159,  1756,   159,   159,   162,  1256,
     159,  1213,  1696,   163,  1947,    59,   162,   159,   159,   159,
    1222,   159,   156,  1834,   156,   162,  1837,  1837,   157,   157,
      81,   182,    76,  1479,   182,  1846,  1847,  1483,  1484,   182,
      93,   156,  1853,    87,  1479,  2056,   182,   182,  1483,  1484,
     157,  1497,   182,  1222,  1256,  1866,   100,   157,   182,   103,
     182,    91,  1497,   107,  1658,  1876,   790,  1878,   159,  1663,
     156,   156,   309,   163,   163,   162,  1670,  2114,  1672,  1525,
    1891,   162,  1893,  1894,  1895,  2114,  2114,  1256,  1899,  1900,
    1696,  1982,  2114,   162,   162,   165,   156,   156,  1849,   164,
     164,   145,  1280,  1281,  1944,   159,  1867,   151,  1494,   124,
     154,   835,   159,   157,   158,   156,  1867,    13,    14,    15,
      16,    17,    18,   159,   159,  2114,   170,  1884,  1837,   162,
     854,   162,   856,  1944,  1944,   159,   860,   861,   159,   863,
    1951,    79,   159,   159,  1955,  2078,  2114,  2080,   156,  1960,
    1756,   195,   156,   197,   198,  2114,   880,    13,    14,    15,
      16,    17,    18,  2054,   208,   209,   164,   159,   182,   107,
     214,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,  1775,   159,  1867,   163,   157,   159,  1022,  2121,   157,
     157,   162,   236,  1028,   159,   156,   162,   241,   242,  1793,
     162,  1795,   159,  2014,  1039,  2114,   156,   159,   159,   162,
    2114,   159,    76,    76,   156,  2026,   157,   157,   262,  2030,
     944,   156,   182,   182,  1947,   949,   270,   182,  2039,  1986,
    2114,   955,  1479,  2114,  2045,  1944,  1483,  1484,  2114,   159,
    1834,   285,   159,  1837,   182,  2056,   162,  2058,   162,  1695,
    1497,   156,  1454,  1455,  1456,  1457,  1458,  1459,  1460,  2192,
     159,  1867,  2023,   156,   159,    76,    76,  2114,    76,  2114,
     161,   173,  2023,   182,   318,  2115,  2087,  1479,   182,  1665,
     324,  1483,  1484,   173,   164,   182,   330,   331,   332,   156,
     156,   156,  2053,   164,   173,  1497,   340,   158,   173,   156,
     107,   163,  2053,  2114,  2115,  2115,  1900,   157,   159,   182,
    1479,   158,  2152,   173,  1483,  1484,  2127,    62,   362,   363,
     364,  2132,   162,  2084,   173,    76,   159,   182,  1497,   566,
     158,   156,   159,  2084,   378,  2115,   159,   156,   382,  2023,
     164,  2152,  2152,   159,   157,   182,   182,  1525,   182,  2160,
    1944,   740,  2163,   159,  2165,  2078,  1785,  2080,   584,   781,
     785,   782,  2119,   108,   784,  1567,  2123,  2124,   113,  2053,
     783,   116,  2152,   118,  1331,  2186,   458,  1256,  1982,  1243,
    2165,   425,  1106,  1107,  2080,  1483,  2197,  1222,    13,    14,
      15,    16,    17,    18,  1875,  2206,  2111,  2154,  2121,  1867,
    2084,  1497,  1745,  1849,  2069,   642,  2115,  1728,  2149,  1728,
    2054,  2124,  2181,  2053,    49,   459,   116,  2023,   462,  1944,
    2177,  1867,  1283,   276,  2181,  2012,   470,  2150,  1460,  1774,
    1277,   870,  1867,  1323,   671,   661,   655,  1531,  1162,  2196,
    1756,     0,   710,  2152,   488,  2039,   806,  2053,   492,   527,
    2054,  1647,   496,   806,   498,   806,    -1,    -1,    -1,    -1,
      -1,    -1,  2056,    -1,  2058,   509,    -1,    -1,    -1,  2192,
    1305,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2084,    -1,
    1204,    -1,    -1,    -1,    -1,    -1,    -1,   531,    -1,    -1,
     137,    -1,    -1,    -1,    -1,   539,    -1,    -1,  1222,    -1,
      13,    14,    15,    16,    17,    -1,    -1,   551,    -1,  1344,
    1345,  1346,    -1,    -1,    -1,    -1,  1351,  1352,    -1,    -1,
    2114,  2115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   274,
      -1,   575,  1256,   577,   578,    -1,    -1,   581,    -1,    -1,
     584,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,  1756,    -1,  1280,  1281,  2152,    -1,
      73,    -1,    -1,    -1,   790,    -1,    -1,    -1,    -1,   806,
      -1,    -1,    -1,    -1,    -1,    -1,   321,  2023,    -1,    -1,
      -1,    -1,    -1,    -1,   231,   822,    -1,    -1,  2023,   826,
      -1,    -1,    -1,    -1,    -1,   639,    -1,   641,    -1,    -1,
     644,    -1,   839,  2197,    -1,    73,    -1,  2053,    13,   835,
      -1,    -1,  2206,    -1,    -1,    -1,   660,   661,  2053,   663,
    1867,   366,   135,   368,   137,   370,    -1,   671,   854,    -1,
     856,   675,    -1,    -1,   860,   861,    -1,   863,  2084,    -1,
     684,    -1,    -1,    -1,   157,    -1,    -1,    -1,   161,  2084,
     694,    -1,    -1,   697,   880,   168,   169,    -1,    -1,    -1,
      -1,    -1,   706,    -1,    -1,  1867,    -1,   135,   315,   137,
     714,   416,    -1,   717,    -1,   719,   720,    -1,   722,    -1,
      -1,    -1,  1406,    -1,    89,  1409,  1410,   731,    -1,   157,
     734,   735,   736,   161,    -1,    -1,    -1,    -1,  1867,    -1,
     168,   169,   107,   940,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   940,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   949,    -1,    -1,    -1,    -1,    58,    -1,
      -1,    -1,    -1,  1457,    -1,    -1,    66,    67,    68,    69,
      -1,    -1,    -1,    -1,    -1,    -1,   790,    -1,    13,    14,
      15,    16,    17,    -1,    -1,  1479,    -1,    -1,    -1,  1483,
    1484,    -1,   806,   807,    -1,    -1,    -1,    -1,    -1,    -1,
     814,    -1,   517,  1497,    -1,    -1,  2023,   107,   425,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     834,   835,    -1,    -1,    -1,   839,    -1,   841,    -1,    -1,
      -1,  1525,    -1,    -1,    -1,    -1,  2053,    -1,    73,    -1,
     854,    -1,   856,    -1,    -1,    -1,   860,   861,   862,   863,
      -1,  2023,     4,     5,     6,     7,     8,     9,    10,    11,
      12,   161,    -1,    -1,    -1,    -1,   880,  2084,    -1,    -1,
      -1,    -1,    -1,  1567,     1,    -1,    -1,     4,    -1,   179,
      -1,  2053,   597,     3,  2023,    -1,    -1,    -1,    -1,    -1,
    1097,    -1,   906,    -1,  1101,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,  1110,    -1,    -1,    -1,    -1,    -1,    -1,
    1106,  1107,  2084,  1120,  2053,    -1,    -1,    -1,    -1,    -1,
    1127,    -1,   157,    -1,    -1,    -1,   161,    -1,    -1,    -1,
     944,    -1,    59,   168,   169,   949,  1741,    -1,    -1,    -1,
      -1,   955,   956,    -1,    -1,  2084,    -1,   564,    -1,    -1,
     964,    -1,   966,    -1,    -1,   572,    -1,    -1,    -1,    -1,
      87,    -1,    -1,   977,    -1,    -1,  1162,    -1,  1175,    -1,
      -1,    -1,  1179,    -1,   591,    -1,  1183,    -1,    -1,    -1,
     107,    -1,    -1,    -1,    -1,   602,    -1,   107,    73,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   107,
      -1,  1695,  1696,   111,   112,   113,   114,   115,   116,   117,
     118,   119,  1026,  1027,   141,   135,    -1,    91,   145,    -1,
      -1,    -1,   107,    -1,   151,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,   170,   164,    -1,    -1,    -1,   168,   169,
     135,    -1,   137,   161,    -1,    -1,    -1,    -1,   132,    -1,
     180,    -1,  1756,    -1,    -1,    -1,    18,    -1,    -1,    -1,
     197,    -1,   157,   158,    -1,    -1,    -1,  1091,    -1,    -1,
      -1,   208,   209,   168,   169,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1106,  1107,    -1,   180,  1110,   714,    -1,    -1,
     717,    -1,    -1,  1117,    -1,   722,    58,    59,    60,    61,
      62,    63,    64,    65,   731,   242,    -1,    -1,    -1,    -1,
      -1,   107,    -1,    -1,   251,   111,   112,   113,   114,   115,
     116,   117,   118,   750,    -1,   262,    -1,    -1,    -1,    -1,
     267,   268,    -1,   270,    -1,    -1,    -1,    62,  1162,    -1,
      -1,    -1,    -1,   280,    -1,  1849,    -1,    -1,    -1,    13,
      14,    15,    16,    17,   291,    -1,    -1,   294,    -1,    -1,
      -1,   298,   158,  1867,    -1,   161,   303,    -1,    -1,    -1,
      -1,    -1,   309,  1988,    -1,    -1,    -1,   314,   103,   316,
    1204,    -1,    -1,   320,    -1,    -1,    -1,    -1,    -1,  1213,
     115,    -1,   117,    -1,   119,   332,    -1,    -1,  1222,    -1,
    1406,    -1,    -1,  1409,  1410,    -1,    -1,    -1,    -1,    73,
    1427,    -1,    -1,    -1,  1431,    -1,    -1,    -1,  1435,    -1,
      -1,    -1,    -1,    -1,  1248,   362,    -1,    -1,   365,    -1,
      -1,    -1,  1256,   158,    -1,    -1,   161,   162,    -1,    -1,
      -1,   378,    -1,   107,   328,   382,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   107,  1280,  1281,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,  1292,    -1,
      -1,   135,    -1,   137,  1298,    -1,    -1,    -1,    -1,    -1,
      -1,  1305,    -1,    -1,    73,   912,    -1,    -1,    -1,   214,
      -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,   926,
      -1,    -1,    -1,   930,   168,   169,    -1,   934,    -1,    76,
      -1,    -1,  1336,    -1,    -1,    -1,   180,  1341,   107,  2023,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
    1547,    -1,    -1,   100,    -1,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   137,  2053,
      -1,   488,    -1,    -1,    -1,   492,    -1,    -1,    -1,    -1,
     285,    62,   287,   288,    -1,    -1,    -1,    -1,   157,   158,
      -1,    -1,   509,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2084,    -1,  1406,    -1,  1601,  1409,  1410,    -1,  1412,    -1,
     157,   180,    -1,  1610,   319,    -1,    -1,  1614,    -1,   324,
      -1,    -1,   103,    -1,    -1,   330,    -1,   491,    -1,   493,
     182,    -1,    -1,    -1,   115,   116,    -1,   107,   502,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   566,
    1454,  1455,  1456,  1457,    -1,  1459,  1460,    -1,    -1,   364,
      -1,  1465,  1466,    -1,   369,    -1,   371,   584,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1479,    -1,   158,    -1,  1483,
    1484,    -1,    -1,   600,    -1,    -1,    -1,    -1,    -1,   236,
    1494,    -1,    -1,  1497,   241,   242,   107,    -1,    -1,    -1,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,   416,   182,    -1,    -1,   262,    -1,    -1,    -1,    -1,
      -1,  1525,    -1,    -1,    -1,   642,    -1,   644,    -1,    18,
      -1,    -1,    -1,   214,    -1,    -1,    -1,    -1,  1542,    -1,
      -1,    -1,    -1,   660,   661,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,    -1,   671,    -1,    -1,    -1,   675,    -1,
      -1,    -1,    -1,  1567,    -1,   470,  1570,   684,    -1,   180,
      -1,   318,   689,    62,    63,    64,    65,    -1,    -1,    -1,
     697,    -1,    -1,    -1,   331,   332,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   498,    -1,   500,   501,    -1,    -1,    -1,
    1207,    -1,   309,    -1,   285,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   517,    -1,  1221,    -1,    -1,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,    -1,    -1,  1241,    -1,    -1,    -1,   319,    -1,
      -1,  1248,    -1,   324,    -1,    -1,   551,    -1,    -1,   330,
      -1,    -1,    -1,    -1,  1658,  1659,   209,    -1,    -1,  1663,
      -1,  1665,    -1,    -1,    -1,    -1,  1670,    -1,  1672,    -1,
     575,    -1,   161,   790,    -1,   580,    -1,   582,    -1,    -1,
      -1,    -1,    -1,   364,    -1,    -1,    -1,    -1,    -1,   806,
     807,  1695,  1696,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     605,    -1,   607,   608,    -1,   822,    -1,    -1,    -1,   826,
      -1,    -1,    -1,    -1,   619,    -1,    -1,   834,   835,    -1,
      -1,    -1,   839,    -1,   841,    -1,    -1,   632,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   416,   641,   854,    -1,   856,
      -1,    -1,    -1,   860,   861,   862,   863,    -1,    -1,   496,
      -1,    -1,  1756,    -1,    -1,    -1,    -1,    73,   663,    -1,
     665,   666,    -1,   880,    -1,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,  1785,   687,   688,    -1,    -1,    -1,    -1,   852,   694,
      -1,   107,   539,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   551,    -1,    -1,    -1,    -1,   362,
      -1,    -1,   365,    -1,    -1,  1819,  1820,   498,    -1,   135,
      -1,   137,    -1,   940,    -1,   378,    -1,   944,   575,   382,
    1834,   172,   949,  1837,   581,    -1,   517,    -1,   955,    -1,
      -1,   157,   158,    -1,    -1,  1849,    -1,   964,    -1,    -1,
     967,    -1,   168,   169,    -1,    -1,    -1,   974,    -1,   566,
      -1,    -1,    -1,  1867,   180,    -1,  1473,  1474,    -1,    -1,
      13,    14,    15,    16,    17,  2072,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   947,   948,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   639,    -1,   641,    -1,  1900,    -1,    -1,    -1,
      -1,   582,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1026,
    1027,    -1,    -1,    -1,    -1,    -1,    -1,  1524,    -1,    -1,
      -1,    -1,    -1,    -1,   605,    -1,    -1,    -1,    -1,    -1,
      73,  1935,    -1,    -1,    -1,   642,    -1,    -1,   619,   492,
    1944,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   632,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,  1982,  1043,
    1097,    -1,   663,    -1,  1101,    -1,    -1,    -1,    -1,  1106,
    1107,    -1,   135,  1110,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1120,    -1,   160,   687,    -1,  2012,    -1,
    1127,    -1,    -1,   694,   157,   158,    -1,   172,   161,  2023,
    1084,    -1,    -1,    -1,    -1,   168,   169,    -1,    -1,    -1,
      -1,   584,    -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1162,    -1,    -1,    -1,  2053,
    2054,    -1,  2056,  2057,    -1,    -1,    -1,    -1,  1175,    -1,
      -1,   966,  1179,    -1,    -1,    -1,  1183,    -1,    -1,  1133,
      -1,  1135,   977,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2084,   986,    -1,  1147,    -1,  1149,    -1,  1204,    -1,    -1,
    1154,  1155,   839,    -1,   841,    -1,  1213,    -1,    -1,   806,
    1164,    -1,    -1,    -1,    -1,  1222,    -1,   660,   661,    -1,
    2114,  2115,    -1,    -1,    -1,   822,    -1,    -1,    -1,   826,
    1727,    -1,    -1,    -1,    -1,    -1,  1190,    -1,    -1,  1193,
      -1,   684,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1256,
      -1,    -1,    -1,    -1,   697,    -1,    -1,   107,  2152,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   906,
      -1,    -1,    -1,   107,  1281,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,  1091,    -1,  1305,    -1,
      -1,   135,  1256,    -1,    -1,    -1,   107,  1314,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   168,    -1,
      -1,    -1,  1117,   157,   158,    -1,    -1,  1334,    -1,  1336,
     164,    -1,  1286,    -1,   168,   169,    -1,    -1,   909,  1293,
      -1,  1295,  1296,    -1,    -1,    73,   180,   790,    -1,    -1,
    1304,    -1,  1306,    -1,  1308,    -1,    -1,    -1,    -1,    -1,
      -1,  1315,   107,    -1,   165,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,    -1,   123,   107,
     125,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   834,   835,    -1,    -1,   966,    -1,     1,    -1,  1406,
       4,    -1,  1409,  1410,    -1,  1412,    -1,   135,    -1,   137,
      -1,    -1,    -1,   158,    -1,    -1,   161,    -1,    -1,   862,
    1427,    -1,    -1,    -1,  1431,    -1,    -1,   252,  1435,   157,
     158,    -1,    -1,   161,    -1,  1389,  1390,    -1,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,    -1,  1454,  1455,  1456,
    1457,  1458,   180,    -1,    -1,    59,    -1,    -1,    -1,    -1,
      -1,  1415,    -1,    -1,    -1,    -1,    -1,    -1,  1422,    -1,
    1424,    -1,  1479,  1110,    -1,    -1,  1483,  1484,    -1,  1116,
    1117,    -1,    -1,    87,    -1,  1280,    -1,    -1,    -1,    -1,
    1497,    -1,    -1,    -1,    -1,    -1,  1450,    -1,    -1,   103,
    1097,   107,    -1,   107,  1101,   111,   112,   113,   114,   115,
     116,   117,   118,   119,  1309,    -1,    -1,   123,    -1,   125,
    1315,   964,    -1,  1120,   967,    -1,    -1,    -1,    -1,    -1,
    1127,    -1,    -1,    -1,    -1,  1542,    -1,   141,    -1,    -1,
    1547,   145,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,
     154,    -1,   158,    -1,   158,   161,    -1,    -1,    -1,    -1,
    1567,    -1,    -1,    -1,    -1,   169,   170,    -1,   172,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,  1175,    -1,
      -1,    -1,  1179,    -1,  1027,    -1,  1183,    -1,    -1,    -1,
      -1,   195,  1546,    -1,  1601,    -1,    -1,    -1,    -1,  1553,
      -1,  1555,    -1,  1610,   208,   209,    -1,  1614,    -1,    -1,
     214,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   107,    -1,    -1,    -1,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   462,    -1,   135,
     123,   137,   125,    -1,    -1,    -1,    -1,   251,    -1,    -1,
      -1,    -1,    -1,    -1,   479,  1292,    -1,   482,    -1,    -1,
      -1,   157,   158,    -1,   268,  1672,   270,    -1,    -1,    -1,
      -1,  1466,   168,   169,    -1,   158,   280,  1631,   161,  2176,
      -1,   285,    -1,    -1,   180,     5,    -1,   291,    -1,  1696,
      -1,  2188,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,   306,    -1,    -1,   309,    -1,    -1,    -1,  1280,
     314,    -1,   316,    -1,    -1,   319,   320,    -1,    -1,   544,
     324,    -1,    -1,    -1,    -1,    -1,   330,    -1,    -1,    49,
    1525,    -1,    52,    -1,    54,    55,   340,    57,  1309,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1756,
      -1,    -1,    -1,    73,    74,    -1,    -1,    -1,   362,  1396,
     364,   365,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,   378,    -1,    -1,    -1,   382,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    -1,   127,   128,    -1,
    1764,  1765,    -1,  1820,    -1,   135,    -1,   137,    -1,    -1,
      -1,   425,    -1,    -1,  1778,    -1,    -1,    73,  1465,    -1,
    1427,    -1,    -1,    -1,  1431,    -1,    -1,   157,  1435,    -1,
     160,   161,    13,    14,    15,    16,    17,   167,   168,   169,
     170,   171,   172,   173,    -1,   459,    -1,  1864,    -1,    -1,
    1867,   107,  1305,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,  1447,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,   492,   135,
      -1,   137,    -1,  1336,   498,  1466,    -1,    -1,    -1,    -1,
    1695,    -1,    73,    -1,    -1,   509,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    -1,    -1,    -1,   531,    -1,    -1,
      -1,    -1,    -1,    -1,   180,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
    1547,    -1,    -1,    -1,  1525,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   566,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,   577,   578,  1982,    -1,    -1,   582,    -1,
     584,   806,   807,    -1,    -1,    -1,   157,   158,    -1,    -1,
      -1,    -1,   817,    -1,    -1,   820,   600,   168,   169,    -1,
      -1,    -1,    -1,    -1,  1601,    -1,    -1,    -1,    -1,   180,
      -1,    -1,    -1,  1610,    -1,   619,  2023,  1614,    -1,    -1,
      -1,  1658,    -1,    -1,    -1,    -1,  1663,    -1,   632,    -1,
      -1,    -1,    -1,  1670,    -1,  1672,    -1,    -1,   642,    -1,
     644,    -1,  1837,    -1,    -1,    -1,  2053,  2054,    -1,    -1,
      -1,    -1,    -1,    -1,  1849,   659,   660,   661,    -1,   663,
      -1,   886,    -1,    -1,    -1,  2072,    -1,   671,   893,    -1,
      -1,    -1,   897,    -1,    -1,    -1,   901,  2084,    -1,    -1,
     684,    -1,    -1,   687,    -1,    -1,    -1,   691,    -1,    -1,
     694,    -1,     1,   697,    -1,    -1,    -1,    -1,    -1,  1542,
      -1,  2055,   706,    -1,    -1,    -1,    -1,    -1,  2115,    -1,
     714,    -1,    -1,   717,     3,   719,   720,    -1,   722,    -1,
      -1,    -1,    -1,    -1,  1695,    -1,    -1,   731,    -1,    -1,
     734,   735,   736,    -1,    -1,    -1,    -1,   107,    -1,    -1,
    1935,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      59,    -1,    -1,   123,    -1,   125,    -1,   105,  2112,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,  1966,  1967,    -1,    -1,    -1,    -1,    -1,    -1,  2133,
      -1,    -1,    -1,    -1,    -1,    -1,   790,    -1,   158,    -1,
      79,    -1,    -1,    -1,  2148,    -1,    -1,  1834,   107,    -1,
    1837,    -1,   806,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     814,    -1,   160,   161,    -1,    -1,    -1,    -1,   822,    -1,
      -1,    -1,   826,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     834,   835,   141,    -1,    -1,   839,    -1,    -1,    -1,    -1,
      -1,    -1,   151,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     854,    -1,   856,    -1,    73,    -1,   860,   861,   862,   863,
      -1,   170,  2057,  1900,    -1,    -1,  1091,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   880,   109,  1849,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     209,    -1,    -1,    -1,    -1,   909,    -1,  1944,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,   107,   211,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,   940,    -1,   157,   158,
     944,  1166,   251,    -1,  1169,   949,    -1,    -1,  1173,   168,
     169,   955,   956,    -1,    -1,    -1,    -1,  2152,    -1,    -1,
     964,   180,   966,   967,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   280,   158,    -1,    -1,   161,    -1,    -1,    -1,    -1,
     269,    -1,   291,    -1,    -1,   294,    -1,    -1,    -1,    -1,
     994,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     309,    -1,    -1,    -1,    -1,    -1,    -1,   316,    -1,    -1,
      -1,   320,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2056,
      -1,   310,  1026,  1027,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   322,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
     339,    -1,    -1,   362,    -1,    -1,   365,    -1,    -1,    -1,
      -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,   378,
      -1,    -1,   361,   382,    -1,  2072,    -1,  2114,  2115,    -1,
      -1,    -1,    -1,    -1,   157,   158,  2057,    -1,   187,    -1,
      -1,    -1,    -1,  1097,    -1,   168,   169,  1101,    -1,    -1,
      -1,    -1,  1106,  1107,    -1,    -1,  1110,   180,    -1,    -1,
      73,    -1,    -1,    -1,    -1,  2152,  1120,    -1,    -1,    -1,
      -1,    -1,    -1,  1127,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,    -1,   426,    -1,  1982,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,  1381,    -1,  1162,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1392,   457,    -1,
      -1,  1175,   135,    -1,   137,  1179,    -1,    -1,    -1,  1183,
      -1,    -1,    -1,   492,    -1,    -1,    -1,    -1,    59,    -1,
      -1,    -1,    -1,    -1,   157,   158,   485,    -1,   161,    -1,
    1204,   490,    -1,    -1,    -1,   168,   169,    -1,    -1,  1213,
      -1,  2054,    -1,    -1,    -1,    -1,    -1,   180,  1222,    -1,
      -1,   510,   511,    -1,    -1,    -1,   515,   516,    -1,    -1,
     519,    -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1248,    -1,   535,    -1,    -1,    -1,
      -1,    -1,  1256,    -1,    -1,    -1,   107,   566,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   557,    -1,
     141,    -1,    -1,    -1,    -1,   584,  1280,  1281,    -1,    -1,
     151,    -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   600,    -1,    -1,  1298,    -1,    -1,    -1,    -1,   170,
      -1,  1305,    -1,    -1,    -1,  1309,   157,   158,    -1,   408,
     161,   410,    -1,    -1,   413,   414,    -1,   168,   169,    -1,
      -1,    -1,    -1,    -1,   423,   424,    -1,    -1,    -1,   180,
    1334,    -1,  1336,   642,    -1,   644,    -1,  1341,   209,   438,
     439,    -1,    -1,    -1,    -1,    -1,   635,    -1,    -1,    -1,
      -1,   660,   661,    -1,    -1,    -1,    -1,    -1,    -1,   648,
     459,    -1,   671,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   684,    -1,    -1,    -1,    -1,
     251,    -1,    -1,   672,    -1,    -1,    -1,    -1,   697,    -1,
      -1,   135,    -1,   492,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1406,    -1,    -1,  1409,  1410,    -1,  1412,   280,
      -1,    -1,    -1,   157,   158,    -1,    -1,   161,    -1,    -1,
     291,    -1,    -1,  1427,   168,   169,    -1,  1431,    -1,    -1,
      -1,  1435,    -1,    -1,  1659,    -1,   180,   181,   309,    -1,
      -1,    -1,    -1,  1447,    -1,   316,    -1,    -1,   737,   320,
    1454,  1455,  1456,  1457,  1458,  1459,  1460,    -1,    -1,    -1,
      -1,   107,  1466,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,  1479,    -1,    -1,    -1,  1483,
    1484,   790,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1494,   362,    -1,  1497,   365,    -1,    -1,   806,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   378,    -1,    -1,
      -1,   382,    -1,   822,    -1,   161,    -1,   826,    -1,    -1,
      -1,  1525,    -1,    -1,    -1,   834,   835,    -1,    -1,    -1,
     839,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1542,    -1,
     829,    -1,   831,  1547,    -1,   854,    -1,   856,   837,    -1,
      -1,   860,   861,   862,   863,    -1,  1560,    -1,    -1,    -1,
      -1,    -1,    -1,  1567,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   880,    -1,    -1,    -1,    -1,   865,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   874,    -1,    -1,    -1,   878,
      -1,    -1,    -1,    -1,  1819,  1820,    -1,  1601,    -1,    -1,
      13,    14,    15,    16,    17,    -1,  1610,    -1,    -1,   107,
    1614,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   492,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   940,    -1,    -1,   923,   944,    -1,    -1,    -1,   928,
     949,    -1,    -1,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   964,    -1,    -1,   967,    -1,
      73,  1665,   160,    -1,    -1,    -1,    -1,    -1,   767,   768,
     769,   770,   771,   772,   773,   774,   775,   776,   777,   778,
     779,   780,   781,   782,   783,   784,   785,    -1,    -1,    -1,
      -1,  1695,  1696,   157,   107,   566,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
    1935,    -1,    -1,   584,    -1,    -1,    -1,    -1,  1027,    -1,
      -1,    -1,   135,    -1,   137,    -1,  1015,    -1,    -1,   600,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,    -1,  1756,   852,    -1,   168,   169,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,
      -1,   642,    -1,   644,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1785,    -1,  2008,   160,    -1,    -1,  2012,  1097,   660,
     661,    -1,  1101,    -1,    -1,    -1,    -1,  1106,  1107,    -1,
     671,  1110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1120,    -1,   684,    -1,    -1,    -1,    -1,  1127,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   697,    -1,    -1,    -1,
     107,  2056,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,  1849,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1162,    -1,    -1,    -1,    -1,   135,    -1,
    1864,    -1,    -1,  1867,    -1,    -1,  1175,    -1,    -1,    -1,
    1179,    -1,    -1,    -1,  1183,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2114,
    2115,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1200,    -1,  1222,    -1,    -1,    -1,    -1,    -1,   790,
      -1,    -1,    -1,  1022,    -1,    -1,    -1,  2152,    -1,  1028,
      -1,    48,    -1,    -1,    -1,   806,    -1,    -1,    -1,    -1,
    1039,    -1,    -1,    -1,    -1,    -1,    -1,  1256,    -1,    -1,
      -1,   822,    -1,    -1,    -1,   826,    -1,    -1,    -1,    76,
    1249,  1965,  1966,   834,   835,    -1,    -1,    -1,   839,    -1,
      -1,    -1,  1281,    -1,    -1,    -1,    -1,    -1,  1982,    -1,
    1079,  1225,    -1,   854,    -1,   856,    -1,    -1,    -1,   860,
     861,   862,   863,  1237,    -1,   107,  1305,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   125,   880,
      -1,    -1,    -1,    -1,  1303,    -1,    -1,    -1,    -1,  2023,
      -1,   138,    -1,   140,    -1,  1334,    -1,  1336,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2039,    -1,    -1,  1327,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,    -1,  2053,
    2054,    -1,    -1,  2057,   171,    -1,   173,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,  2072,   940,
      -1,    -1,   154,   944,    -1,    -1,    -1,    -1,   949,    -1,
    2084,   198,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   964,    -1,    -1,   967,  1406,    -1,   181,
    1409,  1410,   107,  1412,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,  1427,    -1,
      -1,    -1,  1431,  1222,   241,    -1,  1435,    -1,   245,    -1,
      -1,   248,   249,    -1,    -1,   252,    -1,   102,   255,   256,
      -1,   258,   107,   260,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,  1027,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1479,    -1,    -1,    -1,  1483,  1484,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1497,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1305,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   331,    -1,    -1,   334,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1097,    -1,    -1,    -1,
    1101,    -1,    -1,  1542,    -1,  1106,  1107,    -1,  1547,  1110,
      -1,    -1,   359,    -1,  1533,  1344,  1345,  1346,    -1,  1120,
      56,    57,  1351,  1352,    -1,    -1,  1127,   374,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1515,  1516,    -1,    -1,    -1,  1375,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    95,
      -1,  1162,  1601,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1610,    -1,    -1,  1175,  1614,    -1,    -1,  1179,    -1,
      -1,    -1,  1183,    -1,    -1,    -1,    -1,  1416,  1417,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     146,    -1,    -1,   149,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1222,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,    -1,   479,  1652,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,    -1,
      -1,    -1,    -1,    -1,    -1,  1256,    -1,  1696,    -1,    -1,
     196,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1281,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   539,    -1,   230,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1305,    -1,    -1,    -1,   555,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1699,    -1,    -1,    -1,    -1,
      -1,    -1,  1706,  1334,  1563,  1336,   272,    -1,  1757,  1758,
      -1,    -1,    -1,    -1,    -1,   281,   282,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   292,    -1,    -1,    -1,
      -1,    -1,    -1,  1737,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   307,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   639,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1406,   342,    -1,  1409,  1410,
      -1,  1412,    -1,    -1,   350,   351,    -1,    -1,    -1,   355,
      -1,    -1,    -1,    -1,    -1,    -1,  1427,    -1,  1867,    -1,
    1431,    -1,   679,   680,  1435,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   699,    -1,   701,    -1,    -1,    -1,   394,    -1,
      -1,   397,    -1,  1882,   400,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1846,  1847,    -1,    -1,    -1,    -1,  1479,  1853,
      -1,    -1,  1483,  1484,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1866,  1912,    -1,    -1,  1497,    -1,    -1,    -1,
      -1,    -1,  1876,    -1,  1878,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1741,    -1,    -1,    -1,    -1,  1891,    -1,  1893,
    1894,  1895,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1949,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1542,    -1,  1982,    -1,    -1,  1547,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1977,   495,
      -1,    -1,  1981,   810,   811,    -1,    -1,    -1,    -1,    -1,
     817,   507,   508,    -1,     1,    -1,    -1,  1951,    -1,    -1,
      -1,  1955,    -1,    -1,  2023,   170,  1960,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   842,    -1,    -1,   845,   846,
    1601,   848,    -1,   850,   851,    -1,    -1,    -1,    -1,  1610,
      -1,    -1,    -1,  1614,  2053,  2054,    -1,    -1,    -1,    -1,
      -1,    -1,    49,   208,   209,    52,    -1,    54,    55,    -1,
      57,    -1,    -1,  2072,    -1,    -1,    -1,    -1,    -1,    -1,
    2014,    -1,    -1,    -1,    -1,  2084,   893,    74,    -1,    -1,
     897,    -1,  2026,    -1,   901,    -1,  2030,    -1,    -1,    -1,
     245,    -1,    -1,    -1,    -1,    -1,    -1,   252,    -1,    -1,
      -1,  2045,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,  1696,   123,   124,   125,    -1,
     127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     646,    -1,    -1,  2087,    -1,    -1,    -1,    -1,  1947,   655,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   975,    -1,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,   334,
      -1,    -1,    -1,  2127,    -1,    -1,    -1,    -1,  2132,  1988,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   710,    -1,    -1,   362,   363,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2160,    -1,    -1,  2163,
      -1,  2165,    -1,    -1,    -1,    -1,    -1,   382,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2186,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   761,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2078,
      -1,  2080,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1864,    -1,    -1,  1867,    -1,    -1,  1116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   462,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2121,    -1,   479,   480,    -1,   482,   483,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   492,    -1,    -1,
      -1,   496,    -1,    48,  1161,    -1,  1163,   853,    -1,  1166,
      -1,  2150,  1169,    -1,   509,    -1,  1173,    -1,   864,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   540,    -1,    -1,    -1,   544,
      -1,    -1,    -1,  2192,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1982,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   584,
      -1,    -1,    -1,   138,    -1,   140,    -1,   943,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2053,  2054,    -1,   640,    -1,    -1,    -1,    -1,
      -1,    -1,   998,    -1,  1311,    -1,    -1,    -1,    -1,    -1,
      -1,  2072,    -1,    -1,    -1,   660,   661,    -1,    -1,    -1,
      -1,    -1,    -1,  2084,    -1,    -1,   671,    -1,    -1,    -1,
     675,    -1,    -1,    -1,    -1,    -1,    -1,   682,    -1,   684,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   248,   249,    -1,    -1,   252,    -1,    -1,
     255,   256,    -1,   258,    -1,   260,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1381,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1392,    -1,    -1,  1395,    -1,
    1397,  1398,    -1,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    87,    53,   790,    -1,    56,    -1,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,   103,
      -1,   806,   807,    -1,   359,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   817,   818,    -1,   820,   821,    -1,    -1,   374,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   834,
     835,    -1,    -1,    -1,   839,    -1,   841,   842,    -1,  1506,
      -1,    -1,    -1,   848,    -1,    -1,    -1,    -1,    -1,   854,
     154,   856,    -1,    -1,   158,   860,   861,   862,   863,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,
      -1,    -1,  1228,  1229,  1230,   880,    -1,   882,    -1,    -1,
      -1,   886,    -1,    -1,    -1,    -1,    -1,    -1,   893,   894,
     159,   195,   897,   898,    -1,    -1,   901,   902,    -1,    -1,
      -1,    -1,    -1,   908,    -1,   209,  1262,    -1,    -1,    -1,
     214,    -1,    -1,   182,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   479,    -1,    -1,  1283,    -1,    -1,
      -1,    -1,    -1,  1289,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1609,    -1,   949,   950,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   270,    -1,    -1,    -1,
    1637,    -1,   977,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   285,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     555,    -1,    -1,  1670,    -1,    -1,    -1,    -1,    -1,  1676,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1026,  1027,    -1,    -1,    -1,   330,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   340,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   362,    -1,
     364,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1752,  1091,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1106,  1107,    -1,    -1,  1110,  1111,    -1,    -1,    -1,
      -1,    -1,  1117,  1469,  1470,    -1,    -1,    -1,    -1,    -1,
      -1,   425,    -1,    -1,   679,   680,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1800,  1801,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   699,    -1,   701,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   459,    -1,  1162,    -1,  1826,
    1827,  1166,  1167,    -1,  1169,  1170,    -1,  1834,  1173,  1174,
      -1,    -1,  1839,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   492,    -1,
      -1,    -1,    -1,    -1,   498,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1561,    -1,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,   531,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,   810,   811,    51,    -1,    53,
      -1,    -1,   817,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1937,    -1,    -1,   577,   578,    -1,    -1,    -1,    -1,    73,
     584,    -1,    -1,    -1,    -1,    -1,    -1,   842,    -1,    -1,
     845,   846,    -1,   848,    -1,   850,   851,    -1,    -1,    -1,
    1305,    -1,    -1,    -1,    -1,    -1,  1311,  1312,    -1,    -1,
      -1,    -1,    -1,   107,    -1,   109,   110,    -1,    -1,    -1,
      -1,   116,    -1,    -1,  1680,    -1,    -1,    -1,    -1,    -1,
      -1,  1336,    -1,    -1,    -1,    -1,    -1,    -1,   893,    -1,
      -1,  2008,   897,   137,    -1,    -1,   901,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   661,    -1,   663,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,    -1,    -1,   170,  1381,  1382,    -1,    -1,
     684,    -1,    -1,    -1,    -1,    -1,    -1,  1392,  1393,    -1,
    1395,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1406,   706,    -1,  1409,  1410,    -1,  1412,    -1,    -1,
     714,    -1,    -1,   717,   209,   719,   720,    -1,   722,    -1,
     975,    -1,    -1,    -1,    -1,    -1,    -1,   731,    -1,    -1,
     734,   735,   736,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,  2114,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,   790,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     814,    -1,    -1,    -1,   309,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   835,    -1,    -1,    -1,    -1,    -1,  1542,   109,   110,
      -1,    -1,    -1,  1899,    -1,    -1,    -1,    -1,    -1,    -1,
     854,    -1,   856,    -1,    -1,    -1,   860,   861,   862,   863,
      -1,  1116,    -1,    -1,   135,  1570,   137,   362,    -1,   364,
     365,    -1,    -1,    -1,    -1,    -1,   880,    -1,    -1,    -1,
      -1,    -1,    -1,   378,    -1,    -1,    -1,   382,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1161,    -1,  1163,    -1,
      -1,  1166,    -1,    -1,  1169,    -1,    -1,    -1,  1173,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       5,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,   949,    -1,    -1,    -1,    -1,
      -1,    -1,   956,    -1,  1659,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   966,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1676,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
      55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   492,    73,    74,
      -1,    -1,    -1,   498,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1027,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    -1,   127,   128,    -1,    -1,  1311,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,   566,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,   584,
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,  1106,  1107,   209,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1819,  1820,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   619,    -1,  1381,    -1,    -1,    -1,
    1835,    -1,    -1,    -1,    -1,    -1,    -1,  1392,    -1,    -1,
    1395,    -1,  1397,  1398,    -1,    -1,    -1,   642,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1162,    -1,
      -1,    -1,    -1,    -1,    -1,   660,   661,    -1,   663,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   671,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   684,
      -1,    -1,   687,    -1,    -1,    -1,    -1,    -1,    -1,   694,
      -1,    -1,   697,    -1,   309,    -1,    -1,    -1,    -1,  1213,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1935,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1943,    -1,
      -1,    -1,    -1,    -1,  1248,    -1,    -1,    -1,    -1,    -1,
      -1,  1506,    -1,    -1,    -1,    -1,    -1,   362,    -1,   364,
     365,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   378,    -1,    -1,  1280,   382,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1298,   790,    -1,    -1,    -1,    -1,
      -1,  1305,    -1,  2008,  2009,    -1,    -1,  2012,    -1,    -1,
      -1,   806,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   822,    -1,    -1,
      -1,   826,  1336,    -1,    -1,    -1,    -1,  1341,    -1,   834,
     835,    -1,    -1,    -1,   839,    -1,    -1,    -1,    -1,    -1,
      -1,  2056,    -1,    -1,  1609,    -1,    -1,    -1,    -1,   854,
      -1,   856,    -1,    -1,    -1,   860,   861,   862,   863,    -1,
      -1,   425,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1637,    -1,    -1,   880,    -1,   492,    -1,    -1,
      -1,    -1,    -1,   498,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1406,    -1,    -1,  1409,  1410,    -1,  1412,  2114,
    2115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   940,    -1,  2152,    -1,    -1,
    1454,  1455,  1456,    -1,   949,  1459,  1460,   170,    -1,    -1,
      -1,   566,  1466,    -1,    -1,    -1,    -1,    -1,    -1,   964,
      -1,   966,   967,    -1,    -1,    -1,    -1,    -1,    -1,   584,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1494,    -1,    -1,    -1,    -1,    -1,   209,  1752,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   619,    -1,    -1,    -1,    -1,    -1,
      -1,  1525,    -1,   577,   578,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1027,    -1,    -1,    -1,    -1,   642,  1542,    -1,
      -1,    -1,    -1,    -1,    -1,  1800,  1801,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   660,   661,    -1,   663,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   671,    -1,    -1,    -1,
      -1,  1826,  1827,    -1,    -1,    -1,    -1,    -1,    -1,   684,
      -1,    -1,   687,    -1,  1839,    -1,    -1,    -1,    -1,   694,
      -1,    -1,   697,    -1,    -1,    -1,   309,    -1,    -1,    -1,
      -1,    -1,  1097,    -1,    -1,    -1,  1101,    -1,    -1,    -1,
      -1,  1106,  1107,    -1,    -1,  1110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1120,    -1,    -1,    -1,    -1,
      -1,    -1,  1127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   362,
      -1,   364,   365,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     714,  1665,    -1,   717,    -1,   378,    -1,  1162,   722,   382,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   731,    -1,    -1,
    1175,    -1,  1937,    -1,  1179,   790,    -1,    -1,  1183,    -1,
      -1,  1695,    -1,    -1,    -1,    -1,   750,    -1,    -1,    -1,
      -1,   806,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   822,    -1,    -1,
      -1,   826,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   834,
     835,    -1,   786,    -1,   839,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   854,
      -1,   856,    -1,  2008,    -1,   860,   861,   862,   863,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   880,    -1,    -1,    -1,   492,
      -1,  1785,    -1,    -1,    -1,   498,    -1,    -1,    -1,    -1,
      -1,    -1,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   209,    -1,    -1,    -1,
    1305,    -1,    -1,    -1,  1309,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   225,    -1,   227,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   940,    -1,    -1,    -1,  1334,
      -1,  1336,    -1,    -1,   949,  1849,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   566,    -1,    -1,    -1,    -1,    -1,   964,
      -1,   966,   967,    -1,    -1,    13,    14,    15,    16,    17,
      -1,   584,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,   619,    -1,    56,    -1,
      -1,  1406,    -1,    -1,  1409,  1410,    -1,  1412,    -1,    -1,
      -1,    -1,  1027,    -1,    -1,    73,   329,    -1,    -1,   642,
      -1,    -1,  1427,    -1,    -1,    -1,  1431,    -1,    -1,    -1,
    1435,    -1,    -1,    -1,    -1,    -1,    -1,   660,   661,    -1,
     663,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   671,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   684,    -1,    -1,   687,    -1,    -1,    -1,  1982,    -1,
      -1,   694,    -1,    -1,   697,    -1,    -1,   135,    -1,   137,
      -1,    -1,  1097,    -1,    -1,    -1,  1101,    -1,    -1,    -1,
      -1,  1106,  1107,    -1,    -1,  1110,    -1,    -1,    -1,   157,
     158,    -1,   160,   161,    -1,  1120,    -1,    -1,    -1,    -1,
     168,   169,  1127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1542,    -1,    -1,
    2054,    -1,  1547,  2057,    -1,    -1,    -1,  1162,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1175,    -1,    -1,    -1,  1179,    -1,    -1,   790,  1183,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   806,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1601,    -1,    -1,   822,
      -1,    -1,    -1,   826,    -1,  1610,    -1,   520,    -1,  1614,
      -1,   834,   835,   526,    -1,    -1,   839,    -1,   531,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   854,    -1,   856,    -1,    -1,    -1,   860,   861,   862,
     863,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   880,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1248,    -1,    -1,    -1,    -1,    -1,
    1305,    -1,    -1,    -1,  1309,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    55,    -1,    57,    -1,   940,    -1,  1334,
     633,  1336,    -1,    -1,    -1,    -1,   949,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   964,    -1,   966,   967,    -1,    -1,    -1,   661,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   674,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,  1406,    -1,   135,  1409,  1410,    -1,  1412,   711,    -1,
      -1,    -1,    -1,    -1,  1027,    -1,    -1,    -1,    -1,    -1,
      -1,   724,  1427,    -1,    -1,   157,  1431,    -1,   160,   161,
    1435,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,   748,   749,    -1,   180,   752,
      -1,   754,    -1,    -1,    -1,    -1,    -1,   760,    -1,   762,
     763,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1097,    -1,    -1,   790,  1101,    -1,
      -1,    -1,    -1,  1106,  1107,    -1,    -1,  1110,    -1,    -1,
     803,    -1,    -1,    -1,    -1,    -1,    -1,  1120,    -1,    -1,
      -1,   814,    -1,    -1,  1127,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1479,  1480,   830,    -1,  1483,
    1484,    -1,   835,    -1,    -1,  1489,    -1,  1542,    -1,  1493,
      -1,  1495,  1547,  1497,    -1,    -1,    -1,    -1,    -1,  1162,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1175,    -1,   867,    -1,  1179,   870,    -1,    -1,
    1183,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   881,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1982,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1601,    -1,    -1,    -1,
      -1,    -1,     3,    -1,    -1,  1610,   909,    -1,    -1,  1614,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,   956,    -1,    56,    -1,    -1,    -1,  2054,
      -1,    -1,  2057,   966,   967,    -1,    -1,    -1,    -1,    -1,
      -1,   974,    73,    -1,    -1,    -1,    -1,  2072,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1645,  1305,    -1,    -1,    -1,  1309,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1334,    -1,  1336,  1027,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1035,    -1,   135,    -1,   137,  1691,    -1,    -1,
      -1,  1044,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1711,  1712,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1085,    -1,    -1,    -1,    -1,    -1,  1742,    -1,
      -1,    -1,    -1,  1406,    -1,    -1,  1409,  1410,    -1,  1412,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1427,    -1,    -1,    -1,  1431,    -1,
      -1,    -1,  1435,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,  1158,    20,  1160,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,  1851,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1859,    -1,  1861,    -1,    73,
    1864,  1865,    -1,  1867,    -1,    -1,    -1,    -1,  1872,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1542,
      -1,    -1,    -1,    -1,  1547,    -1,    -1,    -1,    -1,  1242,
    1243,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   137,    -1,    -1,    -1,  1982,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1601,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,  1610,    -1,    -1,
      -1,  1614,    -1,    -1,    -1,    -1,  1309,    -1,    -1,    -1,
      -1,  1965,  1315,    -1,    -1,    -1,    -1,    -1,  1972,  1973,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1331,    -1,
      -1,    -1,    -1,  1336,    -1,    -1,    -1,    -1,  1992,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2054,
      -1,  1354,  2057,    -1,  1357,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2072,    -1,  1372,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2031,    -1,  2033,
      -1,    -1,  2036,  2037,    -1,    -1,    -1,    -1,    -1,  2043,
    2044,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
      55,    -1,    57,  1446,  1447,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2107,  2108,  2109,    -1,    -1,    73,    74,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1472,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2138,  2139,  2140,    -1,  1491,   104,
     105,  1494,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    -1,   127,   128,    -1,     1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    -1,   150,   151,   152,   153,  1542,
      -1,    -1,   157,   158,    -1,   160,   161,    -1,  1551,  1552,
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,    -1,    49,    -1,  1568,    52,    -1,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,  1581,    -1,
      -1,    -1,  1585,    -1,    -1,    -1,    72,    -1,    74,    75,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,    -1,   102,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1665,    -1,    -1,    -1,    -1,    -1,  1671,  1982,
     156,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   182,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1732,
      -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,
      -1,  2054,    -1,    -1,  2057,    -1,    -1,    -1,    -1,    -1,
      -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2072,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1775,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1783,    -1,    49,  1786,    -1,    52,    -1,    54,    55,    -1,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    74,    75,  1812,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,    -1,   102,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,    -1,
     127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,     1,    -1,
      -1,    -1,    -1,    -1,    -1,   182,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    20,    18,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    49,    53,    -1,    52,
      56,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    72,
      -1,    74,    75,    -1,    77,    -1,  1989,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,    -1,   102,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,   137,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    72,
      73,    74,    75,    -1,    77,    -1,    -1,    80,    81,    82,
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
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
      -1,    -1,    -1,   106,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,   122,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,   182,
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
      -1,    -1,   165,    -1,    -1,   168,   169,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   180,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
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
     138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     158,   159,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,     4,     5,     6,     7,     8,     9,    10,
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
     161,    -1,    -1,    -1,   165,    -1,    -1,   168,   169,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   180,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    55,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    70,    -1,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      55,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    70,    -1,    -1,    73,    74,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,     1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    -1,    56,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    78,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,    -1,    -1,   109,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    -1,    56,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    78,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,   165,
      -1,    -1,   168,   169,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,   180,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,   180,    22,    23,    24,    25,    26,    27,
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
     158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    -1,    -1,    -1,
      70,    -1,    -1,    73,    -1,    -1,    -1,    -1,    78,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    78,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,
       3,    -1,     5,    -1,    -1,   168,   169,    10,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,
       3,    -1,     5,    -1,    -1,   168,   169,    10,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,
       3,    -1,     5,    -1,    -1,   168,   169,    10,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,
       3,    -1,     5,    -1,    -1,   168,   169,    10,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,
      56,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    73,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,   109,   110,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,   109,   110,    -1,
      -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,   138,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,   138,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
     138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,   138,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    13,    14,    15,    16,    17,    -1,    73,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    72,    -1,    74,    75,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    -1,   137,    94,    95,
      96,    97,    98,    99,   100,    -1,   102,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    49,
      -1,    -1,    52,    -1,    54,    55,   182,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    74,    75,    -1,    77,    -1,    -1,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    -1,    -1,    94,    95,    96,    97,    98,    99,
     100,    -1,   102,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,
     160,   161,    -1,    -1,    -1,   165,    -1,   167,   168,   169,
     170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   182,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     160,    -1,    -1,    -1,    -1,   165,     3,     4,     5,     6,
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
      -1,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,   109,   110,    49,    -1,    51,
      52,    53,    54,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    74,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,    91,
      -1,    93,    -1,    -1,    -1,    -1,   160,    -1,    -1,    -1,
      -1,   165,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,   165,    -1,   167,   168,   169,   170,   171,
     172,   173,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    55,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,    -1,    -1,    -1,   165,    -1,   167,   168,   169,   170,
     171,   172,   173,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    55,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,   159,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,
      -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,   168,
     169,   170,   171,   172,   173,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    55,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
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
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
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
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,     4,
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
      -1,    -1,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   159,   160,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     137,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   159,   160,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,   138,    -1,
      -1,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
     160,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    13,    14,    15,    16,
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
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,
     160,   161,    -1,   109,   110,    -1,    -1,    -1,   168,   169,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   159,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    13,    14,    15,    16,    17,
      18,    73,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,   109,   110,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,   109,   110,    -1,    -1,    -1,   168,   169,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    13,    14,    15,    16,
      17,    18,    73,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   109,   110,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,    -1,   109,   110,    -1,    -1,    -1,   168,   169,    -1,
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
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    73,
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
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    13,    14,    15,    16,    17,    18,
      73,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,   109,   110,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,
     109,   110,    -1,    -1,    -1,   168,   169,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    13,    14,    15,    16,    17,
      18,    73,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,   109,   110,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,   109,   110,    -1,    -1,    -1,   168,   169,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    13,    14,    15,
      16,    17,    73,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,   109,   110,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
      -1,    -1,    -1,   109,   110,    -1,    -1,   168,   169,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    13,    14,    15,
      16,    17,   168,   169,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    73,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,   109,   110,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,   109,   110,
      -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    55,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,
     171,   172,   173,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    55,    -1,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    -1,
     127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,   160,    22,    23,    24,    25,    26,
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
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,   160,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    -1,    56,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    73,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   107,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,   137,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    49,    -1,    -1,    52,    -1,    54,    55,    -1,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,    -1,   109,   110,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    -1,
     127,   128,    -1,   137,    49,    -1,    -1,    52,   135,    54,
      55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,   151,   152,   153,    -1,    -1,    74,
     157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    49,   127,   128,    52,    -1,    54,    55,    -1,    57,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,
      -1,    -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,
     165,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    49,   127,
     128,    52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,
     158,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,
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
      -1,    74,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,
     160,   161,    -1,    -1,   164,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    49,   127,   128,    52,    -1,    54,    55,
      -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,
      -1,    -1,   165,    -1,   167,   168,   169,   170,   171,   172,
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
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,    -1,
      -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,   168,
     169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    49,   127,   128,    52,    -1,    54,
      55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
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
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    49,    -1,    -1,    52,   135,    54,    55,
      -1,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,   157,
     158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      49,   127,   128,    52,    -1,    54,    55,    -1,    57,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
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
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    49,    53,    -1,    52,    56,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    49,   137,   135,
      52,    -1,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    74,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173
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
     168,   169,   180,   182,   187,   188,   189,   202,   297,   298,
     299,   300,   301,   302,   303,   304,   305,   306,   307,   308,
     311,   314,   316,   317,   318,   319,   320,   321,   322,   323,
     324,   325,   327,   329,   330,   331,   333,   334,   338,   339,
     340,   341,   342,   344,   350,   351,   352,   353,   364,   369,
     402,   405,   415,   421,   423,   429,   433,   438,   439,   440,
     441,   442,   443,   444,   445,   471,   489,   490,   491,   492,
       0,   184,   107,   188,   202,   301,   303,   314,   317,   320,
     330,   334,   339,   121,   157,    59,    62,    63,    65,   157,
     157,   367,   427,   428,   429,   326,   327,   109,   110,   188,
     190,   403,   404,   190,   157,   415,   157,   157,     4,   107,
     109,   110,   318,   323,   324,   157,   157,   202,   428,   433,
     439,   440,   441,   443,   444,   445,   109,   341,   162,   184,
     188,   161,   304,   314,   317,   438,   442,   488,   489,   492,
     493,   182,   182,   185,   154,   165,   181,   226,   385,    90,
     163,   422,   102,   190,   426,   163,   163,   163,   182,   109,
     110,   157,   202,   309,   310,   433,   434,   435,   436,   437,
     438,   442,   446,   447,   448,   449,   450,   451,   452,   453,
     454,   460,     3,    47,    48,    50,    56,   332,     3,   161,
     202,   303,   304,   318,   322,   324,   335,   340,   418,   438,
     442,   492,    70,   301,   303,   317,   330,   334,   339,   419,
     438,   442,    66,   323,   323,   318,   324,   312,   323,   324,
     332,   351,   318,   323,   318,   160,   427,   163,   185,   157,
     165,   234,   427,   427,     3,   292,   293,   308,   311,   317,
     321,   322,   161,   314,   317,   490,   190,   190,   415,   181,
     317,   157,   202,   424,   433,   434,   438,   447,   451,   161,
     202,   304,   492,   416,   417,    58,    66,    67,    68,    69,
     161,   179,   190,   391,   393,   397,   399,   400,   340,    58,
     159,   161,   202,   313,   317,   321,   329,   330,   336,   337,
     338,   339,   343,   350,   351,   369,   379,   381,   471,   484,
     485,   486,   487,   492,   493,   427,   109,   110,   172,   188,
     340,   368,   460,   429,   157,   398,   399,   157,    13,    89,
     157,   190,   430,   431,   432,   121,   191,   192,    49,    52,
      54,    55,    57,    74,   104,   105,   107,   108,   119,   120,
     123,   124,   125,   127,   128,   157,   161,   167,   170,   171,
     172,   173,   186,   187,   191,   193,   196,   201,   202,   203,
     204,   207,   208,   209,   210,   211,   212,   213,   214,   215,
     216,   217,   218,   219,   228,   340,   159,   161,   201,   202,
     218,   223,   314,   340,   383,   384,   401,   488,   493,   430,
     317,   439,   440,   441,   443,   444,   445,   159,   159,   159,
     159,   159,   159,   159,   109,   161,   188,   314,   471,   490,
     161,   168,   202,   223,   303,   304,   313,   315,   317,   330,
     337,   339,   376,   377,   378,   380,   381,   484,   492,   162,
     157,   161,   438,   442,   492,   157,   163,   107,   160,   161,
     165,   187,   189,   223,   386,   387,   388,   389,   390,    22,
     386,   157,   190,   234,   157,   157,   188,   424,   188,   428,
     433,   435,   436,   437,   446,   448,   449,   450,   452,   453,
     454,   317,   434,   447,   451,   163,   426,   161,   427,   468,
     471,   426,   427,   427,   422,   292,   157,   427,   468,   426,
     427,   427,   422,   427,   427,   317,   424,   157,   157,   316,
     317,   314,   317,   162,   184,   314,   488,   493,   426,   342,
     165,   422,   292,   190,   190,   385,   303,   322,   420,   438,
     442,   165,   422,   292,   403,   317,   330,   317,   317,   109,
     341,   109,   110,   188,   340,   345,   403,   138,   188,   317,
     373,   374,   378,   379,   382,   156,   184,   234,   308,   182,
     438,   451,   317,   184,   426,   157,   426,   185,   223,   428,
     433,   317,   157,   190,   413,   165,   157,   190,   165,   190,
     138,   168,   169,   396,   159,   163,   190,   400,   159,   427,
     162,   184,   315,   317,   330,   337,   339,   483,   484,   492,
     493,   157,   161,   169,   181,   202,   471,   473,   474,   475,
     476,   477,   478,   495,   202,   343,   492,   317,   337,   323,
     318,   427,   159,   315,   317,   485,   315,   471,   485,   188,
     368,   460,   365,   165,   368,   391,   181,   391,   430,   159,
     163,   157,   159,   121,   157,   201,   157,   157,   201,   157,
     157,   204,   157,   201,   157,   107,   109,   110,   318,   323,
     324,   157,   201,   201,    19,    21,    86,   161,   170,   171,
     205,   206,   223,   230,   234,   353,   383,   492,   163,   184,
     157,   193,   161,   166,   161,   166,   124,   126,   127,   128,
     157,   160,   161,   165,   166,   204,   204,   174,   168,   175,
     176,   170,   171,   129,   130,   131,   132,   177,   178,   133,
     134,   169,   167,   179,   135,   136,   180,   159,   163,   160,
     184,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   181,   225,   226,   227,   157,   202,   464,   465,
     466,   467,   468,   159,   163,   159,   159,   159,   159,   159,
     159,   159,   157,   427,   468,   471,   157,   468,   471,   157,
     184,   157,   314,   490,   162,   184,   185,   161,   185,   157,
     169,   202,   433,   455,   456,   457,   458,   459,   460,   461,
     462,   463,   138,   492,   163,   185,   163,   185,   190,   190,
     157,   184,   184,   184,   184,   161,   189,   184,   387,   164,
     163,   494,   386,   160,   161,   164,   390,   401,   157,   191,
     184,   181,   433,   435,   436,   437,   446,   448,   449,   450,
     452,   453,   454,   159,   159,   159,   159,   159,   159,   159,
     159,   159,   159,   434,   447,   451,   427,   181,   162,   184,
     385,   234,   422,   373,   385,   234,   424,   230,   384,   230,
     384,   424,   109,   161,   413,   234,   422,   426,   165,   165,
     422,   292,   413,   234,   422,   347,   348,   346,   165,   159,
     163,   159,   163,    71,   294,   295,   182,   168,   223,   184,
     433,   415,   413,   190,   162,   184,   157,   395,   393,   394,
      79,   328,   188,   315,   471,   485,   317,   321,   492,   373,
     474,   475,   476,   162,   184,    18,   223,   317,   473,   495,
     427,   427,   471,   315,   483,   493,   317,   188,   315,   485,
     427,   165,   427,   368,    10,   167,   368,   370,   371,   165,
     159,   384,   159,   159,   431,   180,   220,   221,   222,   223,
     182,   383,   493,   193,   383,   161,   383,   384,   383,   493,
     223,   383,   159,   383,   383,   383,   162,   184,   159,   170,
     171,   206,    18,   319,   159,   163,   159,   168,   169,   159,
     158,   223,   229,   223,   165,   223,   188,   223,   188,   119,
     161,   188,   220,   119,   161,   190,   353,   223,   220,   188,
     204,   207,   207,   207,   208,   208,   209,   209,   210,   210,
     210,   210,   211,   211,   212,   213,   214,   215,   216,   164,
     230,   191,   161,   188,   223,   165,   223,   373,   465,   466,
     467,   317,   464,   427,   427,   223,   384,   157,   427,   468,
     471,   157,   468,   471,   373,   373,   184,   184,   162,   162,
     157,   433,   456,   457,   458,   461,    18,   317,   455,   459,
     157,   427,   477,   495,   427,   427,   495,   157,   427,   477,
     427,   427,   185,   219,   190,   377,   380,   162,   380,   381,
     162,   495,   495,   138,   375,   376,   377,   375,   377,   375,
     190,   184,   218,   219,   223,   425,   494,   386,   388,   156,
     184,   159,   184,   159,   375,   223,   159,   159,   159,   159,
     159,   159,   159,   159,   159,   157,   427,   468,   471,   157,
     427,   468,   471,   157,   427,   468,   471,   424,    22,   471,
     223,   324,   340,   469,   234,   159,   159,   159,   159,   159,
     411,   412,   234,   156,   184,   413,   234,   422,   412,   234,
     165,   165,   165,   354,   138,   378,   379,   188,   190,   296,
      18,    72,    74,    75,    77,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    94,    95,    96,
      97,    98,    99,   100,   102,   109,   110,   122,   157,   161,
     190,   230,   231,   232,   233,   234,   235,   236,   238,   239,
     248,   255,   256,   257,   258,   259,   260,   265,   266,   269,
     270,   271,   272,   273,   274,   275,   281,   282,   283,   297,
     317,   321,   423,    71,   185,   185,   375,   414,   412,   159,
     301,   303,   314,   406,   407,   408,   409,   401,   181,   392,
     392,   315,   485,   161,   168,   202,   223,   340,   223,   317,
     159,   159,   159,   159,     5,   317,   427,   473,   366,   370,
     368,   165,   340,   163,   494,   190,   370,   165,   159,   188,
     159,   163,   159,   159,   163,   159,   184,   163,   159,   159,
     159,   163,   159,   204,   159,   159,   159,   204,    18,   319,
     223,   159,   159,   158,   165,   204,   162,   163,   185,   220,
     162,   162,   119,   123,   125,   189,   197,   198,   199,   159,
     197,   162,   163,   156,   218,   164,   159,   197,   185,   387,
     159,   159,   159,   159,   464,   373,   373,   159,   159,   375,
     375,   461,   159,   159,   159,   159,   157,   433,   460,   455,
     459,   373,   373,   162,   185,   495,   163,   185,   159,   163,
     163,   185,   163,   185,   385,   197,   138,   173,   185,   185,
     156,   386,   223,   427,   375,   427,   185,   157,   427,   468,
     471,   157,   427,   468,   471,   157,   427,   468,   471,   373,
     373,   373,   426,   151,   173,   185,   470,   163,   185,   414,
     406,   412,   234,   414,   354,   354,   354,     3,     5,    10,
      74,   156,   298,   305,   306,   314,   317,   355,   360,   488,
     163,   182,   157,    62,    63,   182,   234,   297,   423,   157,
     157,    18,   232,   157,   157,   182,   190,   182,   190,   168,
     190,   165,   231,   157,   157,   157,   232,   157,   234,   223,
     224,   224,    14,   284,   260,   271,   164,   182,   185,   236,
      79,   182,   190,    92,    93,   264,   268,   113,   136,   263,
     112,   135,   267,   263,   382,   317,   296,   162,   162,   185,
     414,   190,   424,   185,   182,   185,   182,   185,   159,   384,
     398,   398,   184,   185,   185,   185,   223,   157,   427,   477,
     471,   316,     5,   168,   185,   223,   368,   494,   165,   370,
      10,   371,   156,   181,   372,   494,   156,   184,   181,   222,
     313,   188,    79,   194,   195,   383,   204,   204,   204,   204,
     204,   165,   387,   158,   223,   163,   156,   200,   161,   198,
     200,   200,   162,   163,   126,   160,   162,   229,   218,   162,
     494,   157,   427,   468,   471,   159,   159,   185,   185,   159,
     157,   427,   468,   471,   157,   427,   477,   433,   427,   427,
     159,   159,   162,   380,   162,   138,   377,   138,   159,   159,
     185,   219,   219,   162,   162,   185,   185,   159,   373,   373,
     373,   159,   159,   159,   385,   163,   223,   223,   324,   340,
     162,   156,   185,   414,   156,   156,   156,   156,   314,   314,
     353,   361,   488,   314,   360,   157,   349,   182,   182,   182,
     157,   164,   202,   356,   357,   363,   433,   434,   447,   451,
     163,   182,   190,   190,   220,   182,   234,   182,   234,   230,
     240,   297,   299,   302,   308,   317,   321,   230,    81,   159,
     240,   150,   151,   152,   153,   158,   159,   182,   230,   249,
     250,   252,   297,   182,   182,   230,   182,   387,   182,   230,
     401,   230,   249,   114,   115,   116,   117,   118,   276,   278,
     279,   182,   101,   182,    85,   157,   159,   427,   156,   182,
     182,   157,   157,   232,   232,   260,   157,   270,   260,   270,
     234,   182,   159,   156,   396,   156,   184,   163,   163,   162,
     162,   162,   185,   373,   223,   223,   185,   162,   185,   165,
     156,   370,   494,   340,   190,   165,   219,   156,   406,   223,
     472,   473,   159,   164,   159,   163,   164,   387,   494,   229,
     124,   197,   198,   161,   198,   161,   198,   162,   156,   373,
     159,   159,   373,   373,   162,   185,   159,   427,   159,   159,
     159,   230,   470,   156,   156,   349,   349,   349,   356,   157,
     202,   358,   359,   468,   479,   480,   481,   482,   182,   163,
     182,   356,   182,   401,   428,   433,   223,   317,   156,   163,
     182,   362,   363,   362,   362,   190,   159,   159,   230,   317,
     159,   157,   232,   159,   150,   151,   152,   153,   173,   182,
     253,   254,   232,   231,   182,   254,   159,   164,   230,   158,
     230,   231,   252,   182,   494,   159,   159,   159,   159,   234,
     278,   279,   157,   223,   157,   191,     1,   232,   204,   261,
     230,    76,   111,   262,   264,    76,   427,   392,   407,   184,
     184,   162,   159,   185,   185,   162,   162,   370,   494,   156,
     372,   387,   185,   159,   223,   195,   223,   494,   156,   162,
     162,   197,   197,   159,   427,   427,   159,   159,   162,   162,
     223,   182,   480,   481,   482,   317,   479,   163,   182,   427,
     427,   182,   159,   433,   427,   232,   232,    78,    79,   165,
     243,   244,   245,   159,   230,    76,   232,   230,   158,   230,
      76,   182,   158,   230,   231,   252,   317,   339,   158,   230,
     232,   250,   254,   254,   182,   230,   156,   165,   245,   232,
     232,   157,   184,   182,   191,   159,   164,   159,   163,   164,
     159,   232,   157,   232,   232,   232,   398,   190,   424,   162,
     162,   494,   156,   494,   156,   156,   162,   162,   159,   159,
     159,   479,   427,   357,    76,     1,   219,   241,   242,   425,
       1,   164,     1,   184,   232,   243,    76,   182,   159,   232,
      76,   182,   173,   173,   232,   231,   254,   254,   182,    58,
     230,   251,   340,   173,   173,    76,   158,   230,   158,   230,
     231,   182,     1,   184,   184,   280,   315,   317,   488,   164,
     182,   161,   191,   285,   286,   287,   204,   220,   230,   263,
     156,   156,   157,   427,   468,   471,   359,   232,   138,     1,
     163,   164,   156,   290,   291,   297,   232,    76,   182,   232,
     230,   158,   158,   230,   158,   230,   158,   230,   231,   188,
     340,   158,   230,   158,   230,   232,   173,   173,   173,   173,
     156,   290,   280,   185,   157,   202,   424,   479,   188,   164,
     107,   157,   159,   164,   163,   159,   159,    76,   259,   373,
     219,   241,   244,   246,   247,   297,   232,   173,   173,   173,
     173,   158,   158,   230,   158,   230,   158,   230,   246,   185,
     182,   277,   317,   285,   162,   219,   182,   285,   287,   232,
      76,   159,   232,   237,   185,   244,   158,   158,   230,   158,
     230,   158,   230,   185,   277,   218,   159,   164,   191,   159,
     159,   164,   232,     1,   232,   156,   237,   156,   159,   234,
     191,   288,   157,   182,   288,   234,   163,   164,   219,   159,
     191,   188,   289,   159,   182,   159,   163,   182,   188
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
     197,   197,   198,   198,   198,   198,   198,   198,   198,   199,
     199,   199,   200,   200,   201,   201,   201,   201,   201,   201,
     201,   201,   201,   201,   201,   201,   201,   201,   201,   201,
     201,   201,   202,   202,   202,   203,   203,   203,   203,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   205,   205,
     205,   205,   206,   206,   207,   207,   208,   208,   208,   208,
     209,   209,   209,   210,   210,   210,   211,   211,   211,   211,
     211,   212,   212,   212,   213,   213,   214,   214,   215,   215,
     216,   216,   217,   217,   218,   218,   218,   219,   220,   220,
     221,   221,   222,   222,   222,   223,   223,   223,   224,   224,
     225,   225,   226,   226,   227,   227,   227,   227,   227,   227,
     227,   227,   227,   227,   227,   228,   228,   229,   229,   229,
     229,   230,   230,   231,   231,   232,   232,   232,   232,   232,
     232,   232,   232,   232,   232,   232,   232,   232,   232,   232,
     232,   233,   233,   234,   234,   235,   235,   236,   236,   236,
     236,   236,   237,   237,   237,   238,   239,   239,   239,   239,
     239,   239,   239,   239,   240,   240,   240,   240,   241,   241,
     241,   242,   242,   243,   243,   243,   243,   243,   244,   244,
     245,   246,   246,   247,   247,   248,   248,   248,   248,   248,
     248,   248,   248,   248,   248,   248,   248,   249,   249,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   251,   251,   251,   252,   252,   252,
     252,   253,   253,   253,   254,   254,   254,   255,   255,   255,
     255,   255,   255,   255,   255,   255,   255,   255,   255,   255,
     255,   255,   255,   255,   255,   255,   255,   256,   256,   257,
     258,   259,   260,   260,   261,   261,   262,   263,   263,   264,
     264,   265,   265,   265,   265,   265,   265,   266,   267,   267,
     268,   269,   269,   270,   270,   271,   271,   271,   272,   273,
     274,   275,   275,   275,   276,   276,   277,   277,   278,   278,
     278,   278,   279,   280,   280,   280,   280,   280,   281,   282,
     282,   283,   283,   283,   283,   283,   284,   284,   285,   285,
     286,   286,   287,   287,   288,   288,   288,   289,   289,   290,
     290,   291,   291,   292,   292,   293,   293,   294,   294,   295,
     295,   296,   296,   297,   297,   297,   298,   298,   299,   299,
     299,   299,   299,   300,   300,   300,   301,   301,   301,   301,
     301,   302,   302,   302,   302,   302,   303,   303,   303,   303,
     304,   304,   305,   305,   305,   306,   306,   306,   306,   306,
     307,   307,   308,   308,   308,   308,   309,   309,   309,   309,
     309,   310,   310,   311,   311,   311,   311,   312,   312,   312,
     313,   313,   313,   314,   314,   314,   315,   315,   315,   316,
     316,   317,   317,   318,   318,   319,   319,   319,   319,   319,
     320,   321,   321,   321,   322,   322,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   324,   325,   325,   325,   325,
     325,   325,   325,   325,   325,   325,   325,   325,   325,   325,
     325,   325,   325,   325,   325,   325,   325,   325,   325,   325,
     325,   325,   325,   325,   326,   326,   327,   328,   328,   329,
     329,   329,   329,   329,   330,   330,   331,   331,   331,   331,
     332,   332,   332,   332,   332,   332,   333,   333,   333,   333,
     334,   335,   334,   334,   336,   336,   336,   336,   337,   337,
     337,   338,   338,   338,   338,   339,   339,   339,   340,   340,
     340,   340,   340,   340,   341,   341,   341,   342,   342,   343,
     343,   345,   344,   346,   344,   347,   344,   348,   344,   344,
     349,   349,   350,   350,   351,   351,   352,   352,   352,   353,
     353,   353,   353,   353,   353,   353,   353,   354,   354,   355,
     355,   355,   355,   355,   355,   355,   355,   355,   355,   355,
     355,   356,   356,   356,   357,   357,   357,   357,   358,   358,
     358,   359,   360,   360,   361,   361,   362,   362,   363,   364,
     364,   365,   364,   364,   366,   364,   364,   364,   367,   367,
     368,   368,   369,   369,   370,   370,   370,   370,   370,   371,
     371,   372,   372,   372,   373,   373,   373,   373,   374,   374,
     374,   374,   375,   375,   375,   375,   375,   375,   375,   376,
     376,   376,   376,   377,   377,   378,   378,   379,   379,   380,
     380,   380,   380,   380,   381,   381,   381,   381,   381,   382,
     382,   383,   383,   383,   384,   384,   385,   385,   385,   385,
     386,   386,   387,   387,   387,   387,   387,   388,   388,   389,
     389,   390,   390,   390,   390,   390,   391,   391,   392,   392,
     394,   393,   395,   393,   393,   393,   393,   396,   396,   396,
     396,   397,   397,   397,   397,   398,   398,   399,   399,   400,
     400,   401,   401,   401,   401,   402,   402,   402,   403,   403,
     404,   404,   405,   405,   405,   405,   406,   406,   407,   407,
     408,   408,   408,   409,   409,   410,   410,   411,   411,   412,
     412,   413,   414,   415,   415,   415,   415,   415,   415,   415,
     415,   415,   415,   415,   416,   415,   417,   415,   418,   415,
     419,   415,   420,   415,   415,   421,   421,   421,   422,   422,
     423,   423,   423,   423,   423,   423,   423,   423,   423,   423,
     424,   424,   424,   424,   425,   426,   426,   427,   427,   428,
     428,   429,   429,   429,   430,   430,   431,   431,   431,   432,
     432,   432,   433,   433,   433,   434,   434,   434,   434,   435,
     435,   435,   435,   436,   436,   436,   436,   436,   436,   436,
     437,   437,   437,   437,   438,   438,   438,   439,   439,   439,
     439,   439,   440,   440,   440,   440,   441,   441,   441,   441,
     441,   441,   442,   442,   442,   443,   443,   443,   443,   443,
     444,   444,   444,   444,   445,   445,   445,   445,   445,   445,
     446,   446,   447,   447,   447,   447,   448,   448,   448,   448,
     449,   449,   449,   449,   449,   449,   449,   450,   450,   450,
     450,   451,   451,   451,   452,   452,   452,   452,   452,   453,
     453,   453,   453,   454,   454,   454,   454,   454,   454,   455,
     455,   455,   455,   455,   456,   456,   456,   457,   457,   457,
     457,   458,   458,   458,   459,   459,   459,   459,   459,   460,
     460,   461,   461,   461,   462,   462,   463,   463,   464,   464,
     464,   465,   465,   465,   465,   465,   466,   466,   466,   466,
     467,   467,   467,   468,   468,   468,   468,   468,   469,   469,
     469,   469,   469,   469,   470,   470,   471,   471,   471,   471,
     472,   472,   473,   473,   473,   473,   474,   474,   474,   474,
     474,   475,   475,   475,   475,   476,   476,   476,   477,   477,
     477,   478,   478,   478,   478,   478,   478,   479,   479,   479,
     480,   480,   480,   480,   480,   481,   481,   481,   481,   482,
     482,   483,   483,   483,   484,   484,   485,   485,   485,   485,
     485,   485,   486,   486,   486,   486,   486,   486,   486,   486,
     486,   486,   487,   487,   487,   487,   488,   488,   488,   489,
     489,   490,   490,   490,   490,   490,   490,   491,   491,   491,
     491,   491,   491,   492,   492,   492,   493,   493,   493,   494,
     494,   495,   495
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
       1,     3,     1,     2,     4,     3,     5,     3,     5,     2,
       2,     2,     0,     2,     1,     1,     1,     2,     2,     2,
       2,     2,     2,     4,     2,     4,     4,     4,     6,     4,
       2,     4,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     5,     5,     4,     5,     5,     5,     4,     2,     2,
       3,     3,     1,     1,     1,     3,     1,     3,     3,     3,
       1,     3,     3,     1,     3,     3,     1,     3,     3,     3,
       3,     1,     3,     3,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     3,     1,     5,     4,     1,     0,     1,
       1,     3,     1,     4,     1,     1,     3,     6,     0,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     4,     7,     1,     1,     3,
       3,     1,     3,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     4,     4,     2,     6,     1,     2,     1,     2,     1,
       2,     1,     1,     2,     2,     2,     5,     7,     5,    10,
       7,     5,    10,     7,     1,     1,     1,     2,     1,     3,
       1,     1,     3,     2,     3,     3,     2,     2,     1,     2,
       2,     0,     1,     2,     3,     4,     6,     5,     7,     6,
       7,     7,     8,     4,     6,     5,     7,     1,     3,     4,
       5,     4,     3,     5,     1,     2,     3,     3,     3,     5,
       5,     5,     5,     3,     5,     5,     5,     3,     4,     5,
       5,     5,     5,     7,     7,     7,     7,     7,     7,     7,
       2,     3,     4,     4,     4,     6,     6,     6,     6,     6,
       6,     6,     3,     4,     1,     2,     2,     1,     1,     1,
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
       4,     1,     3,     2,     2,     2,     6,     4,     1,     1,
       1,     1,     1,     2,     3,     6,     3,     3,     4,     2,
       3,     1,     2,     2,     3,     8,     9,     9,     8,     8,
       5,     7,     2,     2,     3,     3,     3,     4,     3,     4,
       4,     5,     2,     1,     1,     1,     3,     3,     2,     4,
       6,     1,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       4,     1,     2,     3,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     5,     0,     1,     1,
       2,     2,     3,     3,     1,     3,     1,     2,     2,     2,
       4,     4,     4,     4,     1,     1,     1,     2,     2,     3,
       1,     0,     3,     2,     1,     2,     2,     3,     1,     2,
       2,     1,     2,     2,     3,     1,     2,     2,     1,     2,
       3,     1,     2,     3,     1,     3,     4,     1,     1,     1,
       1,     0,     7,     0,     8,     0,     8,     0,     8,     1,
       0,     3,     3,     3,     1,     1,     2,     1,     1,     1,
       2,     1,     2,     1,     2,     1,     2,     0,     2,     3,
       3,     4,     4,     4,     3,     2,     2,     3,     3,     2,
       2,     0,     1,     4,     1,     2,     2,     2,     0,     1,
       4,     1,     2,     3,     1,     2,     0,     1,     2,     7,
       8,     0,     9,     8,     0,    11,    10,     1,     2,     3,
       0,     1,     3,     3,     0,     3,     2,     5,     4,     1,
       1,     0,     2,     5,     0,     1,     1,     3,     1,     1,
       3,     3,     0,     1,     1,     1,     3,     3,     3,     1,
       3,     3,     5,     1,     3,     3,     3,     2,     3,     1,
       3,     3,     4,     1,     1,     1,     1,     2,     1,     1,
       3,     1,     1,     2,     1,     1,     0,     2,     2,     4,
       1,     4,     0,     1,     2,     3,     4,     2,     2,     1,
       2,     2,     5,     5,     7,     6,     1,     3,     0,     2,
       0,     5,     0,     5,     3,     1,     8,     0,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     1,     2,     5,
       6,     1,     1,     3,     3,     2,     3,     3,     2,     4,
       1,     4,     7,     5,    10,     8,     1,     4,     2,     2,
       1,     1,     5,     2,     5,     0,     1,     3,     4,     0,
       1,     0,     0,     1,     1,     2,     2,     2,     2,     2,
       2,     1,     2,     5,     0,     6,     0,     8,     0,     7,
       0,     7,     0,     8,     1,     1,     2,     3,     0,     5,
       3,     4,     4,     4,     4,     5,     5,     5,     5,     6,
       1,     1,     1,     1,     3,     0,     5,     0,     1,     1,
       2,     6,     4,     4,     1,     3,     0,     1,     4,     1,
       1,     1,     1,     2,     3,     2,     1,     2,     2,     2,
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
       3,     4,     2,     5,     6,     7,     5,     6,     6,     0,
       1,     0,     2
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
#line 644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 8446 "Parser/parser.cc"
    break;

  case 3:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8452 "Parser/parser.cc"
    break;

  case 4:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8458 "Parser/parser.cc"
    break;

  case 5:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8464 "Parser/parser.cc"
    break;

  case 6:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8470 "Parser/parser.cc"
    break;

  case 7:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8476 "Parser/parser.cc"
    break;

  case 8:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8482 "Parser/parser.cc"
    break;

  case 20:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8488 "Parser/parser.cc"
    break;

  case 24:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8494 "Parser/parser.cc"
    break;

  case 25:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8500 "Parser/parser.cc"
    break;

  case 26:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8510 "Parser/parser.cc"
    break;

  case 27:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8516 "Parser/parser.cc"
    break;

  case 28:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8522 "Parser/parser.cc"
    break;

  case 29:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8528 "Parser/parser.cc"
    break;

  case 31:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8534 "Parser/parser.cc"
    break;

  case 32:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8540 "Parser/parser.cc"
    break;

  case 33:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8546 "Parser/parser.cc"
    break;

  case 34:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8552 "Parser/parser.cc"
    break;

  case 35:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8562 "Parser/parser.cc"
    break;

  case 36:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8568 "Parser/parser.cc"
    break;

  case 37:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8574 "Parser/parser.cc"
    break;

  case 38:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8580 "Parser/parser.cc"
    break;

  case 39:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8586 "Parser/parser.cc"
    break;

  case 40:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8592 "Parser/parser.cc"
    break;

  case 41:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8598 "Parser/parser.cc"
    break;

  case 43:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8610 "Parser/parser.cc"
    break;

  case 44:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8619 "Parser/parser.cc"
    break;

  case 45:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8625 "Parser/parser.cc"
    break;

  case 47:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8631 "Parser/parser.cc"
    break;

  case 48:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8637 "Parser/parser.cc"
    break;

  case 49:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8643 "Parser/parser.cc"
    break;

  case 50:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8649 "Parser/parser.cc"
    break;

  case 51:
#line 786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8659 "Parser/parser.cc"
    break;

  case 52:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8665 "Parser/parser.cc"
    break;

  case 53:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8672 "Parser/parser.cc"
    break;

  case 54:
#line 798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8678 "Parser/parser.cc"
    break;

  case 55:
#line 800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8684 "Parser/parser.cc"
    break;

  case 56:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8690 "Parser/parser.cc"
    break;

  case 57:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8696 "Parser/parser.cc"
    break;

  case 58:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8702 "Parser/parser.cc"
    break;

  case 59:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8708 "Parser/parser.cc"
    break;

  case 60:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8714 "Parser/parser.cc"
    break;

  case 61:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8720 "Parser/parser.cc"
    break;

  case 62:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8726 "Parser/parser.cc"
    break;

  case 63:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8732 "Parser/parser.cc"
    break;

  case 64:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8738 "Parser/parser.cc"
    break;

  case 65:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8744 "Parser/parser.cc"
    break;

  case 66:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8750 "Parser/parser.cc"
    break;

  case 67:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8756 "Parser/parser.cc"
    break;

  case 68:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8762 "Parser/parser.cc"
    break;

  case 69:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8772 "Parser/parser.cc"
    break;

  case 71:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8778 "Parser/parser.cc"
    break;

  case 73:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8784 "Parser/parser.cc"
    break;

  case 74:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8790 "Parser/parser.cc"
    break;

  case 75:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8796 "Parser/parser.cc"
    break;

  case 76:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8802 "Parser/parser.cc"
    break;

  case 77:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8808 "Parser/parser.cc"
    break;

  case 78:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8814 "Parser/parser.cc"
    break;

  case 79:
#line 877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8820 "Parser/parser.cc"
    break;

  case 80:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8826 "Parser/parser.cc"
    break;

  case 81:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8834 "Parser/parser.cc"
    break;

  case 82:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8840 "Parser/parser.cc"
    break;

  case 83:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8849 "Parser/parser.cc"
    break;

  case 86:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8855 "Parser/parser.cc"
    break;

  case 87:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8861 "Parser/parser.cc"
    break;

  case 88:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8881 "Parser/parser.cc"
    break;

  case 89:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8887 "Parser/parser.cc"
    break;

  case 90:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8893 "Parser/parser.cc"
    break;

  case 91:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8899 "Parser/parser.cc"
    break;

  case 92:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8905 "Parser/parser.cc"
    break;

  case 93:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8911 "Parser/parser.cc"
    break;

  case 94:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8917 "Parser/parser.cc"
    break;

  case 95:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8923 "Parser/parser.cc"
    break;

  case 96:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8929 "Parser/parser.cc"
    break;

  case 97:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8935 "Parser/parser.cc"
    break;

  case 98:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8941 "Parser/parser.cc"
    break;

  case 99:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8950 "Parser/parser.cc"
    break;

  case 100:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {  (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8956 "Parser/parser.cc"
    break;

  case 101:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8962 "Parser/parser.cc"
    break;

  case 102:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 8968 "Parser/parser.cc"
    break;

  case 103:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 8974 "Parser/parser.cc"
    break;

  case 104:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 8980 "Parser/parser.cc"
    break;

  case 105:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 8986 "Parser/parser.cc"
    break;

  case 106:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 8992 "Parser/parser.cc"
    break;

  case 107:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 8998 "Parser/parser.cc"
    break;

  case 108:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9004 "Parser/parser.cc"
    break;

  case 110:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9010 "Parser/parser.cc"
    break;

  case 111:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9016 "Parser/parser.cc"
    break;

  case 112:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9022 "Parser/parser.cc"
    break;

  case 113:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9028 "Parser/parser.cc"
    break;

  case 114:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9034 "Parser/parser.cc"
    break;

  case 115:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9040 "Parser/parser.cc"
    break;

  case 116:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9046 "Parser/parser.cc"
    break;

  case 117:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9052 "Parser/parser.cc"
    break;

  case 125:
#line 1010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9058 "Parser/parser.cc"
    break;

  case 127:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9064 "Parser/parser.cc"
    break;

  case 128:
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9070 "Parser/parser.cc"
    break;

  case 129:
#line 1020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9076 "Parser/parser.cc"
    break;

  case 131:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9082 "Parser/parser.cc"
    break;

  case 132:
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9088 "Parser/parser.cc"
    break;

  case 134:
#line 1034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9094 "Parser/parser.cc"
    break;

  case 135:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9100 "Parser/parser.cc"
    break;

  case 137:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9106 "Parser/parser.cc"
    break;

  case 138:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9112 "Parser/parser.cc"
    break;

  case 139:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9118 "Parser/parser.cc"
    break;

  case 140:
#line 1048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9124 "Parser/parser.cc"
    break;

  case 142:
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9130 "Parser/parser.cc"
    break;

  case 143:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9136 "Parser/parser.cc"
    break;

  case 145:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9142 "Parser/parser.cc"
    break;

  case 147:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9148 "Parser/parser.cc"
    break;

  case 149:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9154 "Parser/parser.cc"
    break;

  case 151:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9160 "Parser/parser.cc"
    break;

  case 153:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9166 "Parser/parser.cc"
    break;

  case 155:
#line 1092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9172 "Parser/parser.cc"
    break;

  case 156:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9178 "Parser/parser.cc"
    break;

  case 158:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9184 "Parser/parser.cc"
    break;

  case 161:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9190 "Parser/parser.cc"
    break;

  case 162:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 9196 "Parser/parser.cc"
    break;

  case 163:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 9202 "Parser/parser.cc"
    break;

  case 166:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 9214 "Parser/parser.cc"
    break;

  case 167:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9220 "Parser/parser.cc"
    break;

  case 168:
#line 1141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9226 "Parser/parser.cc"
    break;

  case 172:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9232 "Parser/parser.cc"
    break;

  case 173:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9238 "Parser/parser.cc"
    break;

  case 174:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9244 "Parser/parser.cc"
    break;

  case 175:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9250 "Parser/parser.cc"
    break;

  case 176:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9256 "Parser/parser.cc"
    break;

  case 177:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9262 "Parser/parser.cc"
    break;

  case 178:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9268 "Parser/parser.cc"
    break;

  case 179:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9274 "Parser/parser.cc"
    break;

  case 180:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9280 "Parser/parser.cc"
    break;

  case 181:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9286 "Parser/parser.cc"
    break;

  case 182:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9292 "Parser/parser.cc"
    break;

  case 183:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9298 "Parser/parser.cc"
    break;

  case 184:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9304 "Parser/parser.cc"
    break;

  case 185:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9310 "Parser/parser.cc"
    break;

  case 186:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9316 "Parser/parser.cc"
    break;

  case 188:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9322 "Parser/parser.cc"
    break;

  case 189:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9328 "Parser/parser.cc"
    break;

  case 190:
#line 1189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9334 "Parser/parser.cc"
    break;

  case 192:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9340 "Parser/parser.cc"
    break;

  case 193:
#line 1200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9346 "Parser/parser.cc"
    break;

  case 208:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9352 "Parser/parser.cc"
    break;

  case 210:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9358 "Parser/parser.cc"
    break;

  case 211:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9364 "Parser/parser.cc"
    break;

  case 212:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9375 "Parser/parser.cc"
    break;

  case 213:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9381 "Parser/parser.cc"
    break;

  case 214:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9387 "Parser/parser.cc"
    break;

  case 216:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9393 "Parser/parser.cc"
    break;

  case 217:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9399 "Parser/parser.cc"
    break;

  case 218:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9405 "Parser/parser.cc"
    break;

  case 219:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9411 "Parser/parser.cc"
    break;

  case 220:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9417 "Parser/parser.cc"
    break;

  case 223:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9423 "Parser/parser.cc"
    break;

  case 224:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9430 "Parser/parser.cc"
    break;

  case 225:
#line 1279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9436 "Parser/parser.cc"
    break;

  case 226:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9442 "Parser/parser.cc"
    break;

  case 227:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9448 "Parser/parser.cc"
    break;

  case 228:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9454 "Parser/parser.cc"
    break;

  case 229:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9468 "Parser/parser.cc"
    break;

  case 230:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9474 "Parser/parser.cc"
    break;

  case 231:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9480 "Parser/parser.cc"
    break;

  case 232:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9489 "Parser/parser.cc"
    break;

  case 233:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9495 "Parser/parser.cc"
    break;

  case 234:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9501 "Parser/parser.cc"
    break;

  case 235:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9507 "Parser/parser.cc"
    break;

  case 236:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9513 "Parser/parser.cc"
    break;

  case 237:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9519 "Parser/parser.cc"
    break;

  case 238:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9525 "Parser/parser.cc"
    break;

  case 239:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9531 "Parser/parser.cc"
    break;

  case 241:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9537 "Parser/parser.cc"
    break;

  case 242:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9543 "Parser/parser.cc"
    break;

  case 243:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9549 "Parser/parser.cc"
    break;

  case 244:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9555 "Parser/parser.cc"
    break;

  case 245:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9561 "Parser/parser.cc"
    break;

  case 246:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9567 "Parser/parser.cc"
    break;

  case 247:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9573 "Parser/parser.cc"
    break;

  case 249:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9579 "Parser/parser.cc"
    break;

  case 250:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9585 "Parser/parser.cc"
    break;

  case 251:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9591 "Parser/parser.cc"
    break;

  case 253:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9597 "Parser/parser.cc"
    break;

  case 254:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9603 "Parser/parser.cc"
    break;

  case 255:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9609 "Parser/parser.cc"
    break;

  case 256:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9618 "Parser/parser.cc"
    break;

  case 257:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9624 "Parser/parser.cc"
    break;

  case 258:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9630 "Parser/parser.cc"
    break;

  case 259:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9636 "Parser/parser.cc"
    break;

  case 260:
#line 1413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9645 "Parser/parser.cc"
    break;

  case 261:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9651 "Parser/parser.cc"
    break;

  case 262:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9657 "Parser/parser.cc"
    break;

  case 263:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9663 "Parser/parser.cc"
    break;

  case 264:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9672 "Parser/parser.cc"
    break;

  case 265:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9678 "Parser/parser.cc"
    break;

  case 266:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9684 "Parser/parser.cc"
    break;

  case 268:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9703 "Parser/parser.cc"
    break;

  case 269:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9709 "Parser/parser.cc"
    break;

  case 270:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9717 "Parser/parser.cc"
    break;

  case 271:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9723 "Parser/parser.cc"
    break;

  case 272:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9729 "Parser/parser.cc"
    break;

  case 273:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9735 "Parser/parser.cc"
    break;

  case 274:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9741 "Parser/parser.cc"
    break;

  case 275:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9747 "Parser/parser.cc"
    break;

  case 276:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9753 "Parser/parser.cc"
    break;

  case 277:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9762 "Parser/parser.cc"
    break;

  case 278:
#line 1485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9771 "Parser/parser.cc"
    break;

  case 279:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9777 "Parser/parser.cc"
    break;

  case 280:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9786 "Parser/parser.cc"
    break;

  case 281:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9795 "Parser/parser.cc"
    break;

  case 282:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9801 "Parser/parser.cc"
    break;

  case 283:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9807 "Parser/parser.cc"
    break;

  case 284:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9813 "Parser/parser.cc"
    break;

  case 285:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9819 "Parser/parser.cc"
    break;

  case 286:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9825 "Parser/parser.cc"
    break;

  case 287:
#line 1515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9831 "Parser/parser.cc"
    break;

  case 288:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9837 "Parser/parser.cc"
    break;

  case 289:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9843 "Parser/parser.cc"
    break;

  case 290:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9852 "Parser/parser.cc"
    break;

  case 291:
#line 1527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9862 "Parser/parser.cc"
    break;

  case 292:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9868 "Parser/parser.cc"
    break;

  case 293:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9874 "Parser/parser.cc"
    break;

  case 294:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9883 "Parser/parser.cc"
    break;

  case 295:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9893 "Parser/parser.cc"
    break;

  case 296:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9899 "Parser/parser.cc"
    break;

  case 297:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9908 "Parser/parser.cc"
    break;

  case 298:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9918 "Parser/parser.cc"
    break;

  case 299:
#line 1562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9924 "Parser/parser.cc"
    break;

  case 300:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9930 "Parser/parser.cc"
    break;

  case 301:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9936 "Parser/parser.cc"
    break;

  case 302:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9942 "Parser/parser.cc"
    break;

  case 303:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9951 "Parser/parser.cc"
    break;

  case 304:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9961 "Parser/parser.cc"
    break;

  case 305:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9967 "Parser/parser.cc"
    break;

  case 306:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9976 "Parser/parser.cc"
    break;

  case 307:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9986 "Parser/parser.cc"
    break;

  case 308:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9992 "Parser/parser.cc"
    break;

  case 309:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10001 "Parser/parser.cc"
    break;

  case 310:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10011 "Parser/parser.cc"
    break;

  case 311:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10017 "Parser/parser.cc"
    break;

  case 312:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 10025 "Parser/parser.cc"
    break;

  case 313:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); (yyval.forctl) = nullptr;
				(yyvsp[-1].oper) = OperKinds::GEThan;
			} // if
			(yyval.forctl) = enumRangeCtrl( (yyvsp[-3].expr), (yyvsp[-1].oper), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 10037 "Parser/parser.cc"
    break;

  case 314:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10044 "Parser/parser.cc"
    break;

  case 315:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false ); }
#line 10051 "Parser/parser.cc"
    break;

  case 316:
#line 1634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10058 "Parser/parser.cc"
    break;

  case 317:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10064 "Parser/parser.cc"
    break;

  case 318:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10070 "Parser/parser.cc"
    break;

  case 319:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10076 "Parser/parser.cc"
    break;

  case 320:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10082 "Parser/parser.cc"
    break;

  case 321:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10088 "Parser/parser.cc"
    break;

  case 322:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10094 "Parser/parser.cc"
    break;

  case 323:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10100 "Parser/parser.cc"
    break;

  case 325:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10106 "Parser/parser.cc"
    break;

  case 326:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10112 "Parser/parser.cc"
    break;

  case 327:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10118 "Parser/parser.cc"
    break;

  case 328:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10124 "Parser/parser.cc"
    break;

  case 329:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10130 "Parser/parser.cc"
    break;

  case 330:
#line 1680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10136 "Parser/parser.cc"
    break;

  case 331:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10142 "Parser/parser.cc"
    break;

  case 332:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10148 "Parser/parser.cc"
    break;

  case 333:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10154 "Parser/parser.cc"
    break;

  case 334:
#line 1692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10160 "Parser/parser.cc"
    break;

  case 335:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10166 "Parser/parser.cc"
    break;

  case 336:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10172 "Parser/parser.cc"
    break;

  case 337:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10178 "Parser/parser.cc"
    break;

  case 338:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10184 "Parser/parser.cc"
    break;

  case 339:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10190 "Parser/parser.cc"
    break;

  case 340:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10196 "Parser/parser.cc"
    break;

  case 341:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10202 "Parser/parser.cc"
    break;

  case 342:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10208 "Parser/parser.cc"
    break;

  case 343:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10214 "Parser/parser.cc"
    break;

  case 344:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10220 "Parser/parser.cc"
    break;

  case 345:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10226 "Parser/parser.cc"
    break;

  case 346:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10232 "Parser/parser.cc"
    break;

  case 349:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10238 "Parser/parser.cc"
    break;

  case 350:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10247 "Parser/parser.cc"
    break;

  case 351:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10253 "Parser/parser.cc"
    break;

  case 352:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10259 "Parser/parser.cc"
    break;

  case 355:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10265 "Parser/parser.cc"
    break;

  case 356:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10271 "Parser/parser.cc"
    break;

  case 359:
#line 1766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10277 "Parser/parser.cc"
    break;

  case 360:
#line 1768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10283 "Parser/parser.cc"
    break;

  case 361:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10289 "Parser/parser.cc"
    break;

  case 362:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10295 "Parser/parser.cc"
    break;

  case 363:
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10301 "Parser/parser.cc"
    break;

  case 364:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10307 "Parser/parser.cc"
    break;

  case 365:
#line 1783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10313 "Parser/parser.cc"
    break;

  case 366:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10319 "Parser/parser.cc"
    break;

  case 367:
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10325 "Parser/parser.cc"
    break;

  case 370:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10331 "Parser/parser.cc"
    break;

  case 371:
#line 1805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10337 "Parser/parser.cc"
    break;

  case 372:
#line 1807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10343 "Parser/parser.cc"
    break;

  case 373:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10349 "Parser/parser.cc"
    break;

  case 374:
#line 1814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10355 "Parser/parser.cc"
    break;

  case 375:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10361 "Parser/parser.cc"
    break;

  case 376:
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10367 "Parser/parser.cc"
    break;

  case 377:
#line 1823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10373 "Parser/parser.cc"
    break;

  case 378:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10379 "Parser/parser.cc"
    break;

  case 379:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10385 "Parser/parser.cc"
    break;

  case 380:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10391 "Parser/parser.cc"
    break;

  case 381:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10397 "Parser/parser.cc"
    break;

  case 382:
#line 1845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10403 "Parser/parser.cc"
    break;

  case 383:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10409 "Parser/parser.cc"
    break;

  case 384:
#line 1852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10415 "Parser/parser.cc"
    break;

  case 385:
#line 1854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10421 "Parser/parser.cc"
    break;

  case 386:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10427 "Parser/parser.cc"
    break;

  case 387:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10433 "Parser/parser.cc"
    break;

  case 388:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10439 "Parser/parser.cc"
    break;

  case 389:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10445 "Parser/parser.cc"
    break;

  case 390:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10451 "Parser/parser.cc"
    break;

  case 391:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10457 "Parser/parser.cc"
    break;

  case 392:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10463 "Parser/parser.cc"
    break;

  case 394:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10469 "Parser/parser.cc"
    break;

  case 395:
#line 1880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10475 "Parser/parser.cc"
    break;

  case 396:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10481 "Parser/parser.cc"
    break;

  case 401:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10487 "Parser/parser.cc"
    break;

  case 402:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10493 "Parser/parser.cc"
    break;

  case 403:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10499 "Parser/parser.cc"
    break;

  case 404:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10505 "Parser/parser.cc"
    break;

  case 405:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10511 "Parser/parser.cc"
    break;

  case 406:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10517 "Parser/parser.cc"
    break;

  case 407:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10523 "Parser/parser.cc"
    break;

  case 408:
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10529 "Parser/parser.cc"
    break;

  case 411:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10535 "Parser/parser.cc"
    break;

  case 412:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10541 "Parser/parser.cc"
    break;

  case 413:
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10550 "Parser/parser.cc"
    break;

  case 414:
#line 1939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10556 "Parser/parser.cc"
    break;

  case 415:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10562 "Parser/parser.cc"
    break;

  case 416:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10568 "Parser/parser.cc"
    break;

  case 417:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10577 "Parser/parser.cc"
    break;

  case 418:
#line 1953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10586 "Parser/parser.cc"
    break;

  case 419:
#line 1963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10592 "Parser/parser.cc"
    break;

  case 422:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10598 "Parser/parser.cc"
    break;

  case 423:
#line 1975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10604 "Parser/parser.cc"
    break;

  case 425:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10610 "Parser/parser.cc"
    break;

  case 426:
#line 1983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10616 "Parser/parser.cc"
    break;

  case 436:
#line 2009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 10622 "Parser/parser.cc"
    break;

  case 437:
#line 2011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10628 "Parser/parser.cc"
    break;

  case 441:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10634 "Parser/parser.cc"
    break;

  case 443:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10640 "Parser/parser.cc"
    break;

  case 444:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10646 "Parser/parser.cc"
    break;

  case 445:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10652 "Parser/parser.cc"
    break;

  case 446:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10658 "Parser/parser.cc"
    break;

  case 447:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10664 "Parser/parser.cc"
    break;

  case 448:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10670 "Parser/parser.cc"
    break;

  case 449:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10676 "Parser/parser.cc"
    break;

  case 450:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10682 "Parser/parser.cc"
    break;

  case 452:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10688 "Parser/parser.cc"
    break;

  case 453:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10694 "Parser/parser.cc"
    break;

  case 454:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10700 "Parser/parser.cc"
    break;

  case 455:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10711 "Parser/parser.cc"
    break;

  case 456:
#line 2084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10717 "Parser/parser.cc"
    break;

  case 457:
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10723 "Parser/parser.cc"
    break;

  case 458:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10729 "Parser/parser.cc"
    break;

  case 459:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10735 "Parser/parser.cc"
    break;

  case 460:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10741 "Parser/parser.cc"
    break;

  case 461:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10747 "Parser/parser.cc"
    break;

  case 462:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10756 "Parser/parser.cc"
    break;

  case 463:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10765 "Parser/parser.cc"
    break;

  case 464:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10774 "Parser/parser.cc"
    break;

  case 465:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10785 "Parser/parser.cc"
    break;

  case 466:
#line 2142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10794 "Parser/parser.cc"
    break;

  case 467:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10800 "Parser/parser.cc"
    break;

  case 468:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10806 "Parser/parser.cc"
    break;

  case 469:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10812 "Parser/parser.cc"
    break;

  case 470:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10820 "Parser/parser.cc"
    break;

  case 471:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10828 "Parser/parser.cc"
    break;

  case 472:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10834 "Parser/parser.cc"
    break;

  case 475:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10849 "Parser/parser.cc"
    break;

  case 476:
#line 2188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10855 "Parser/parser.cc"
    break;

  case 477:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10861 "Parser/parser.cc"
    break;

  case 478:
#line 2193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10867 "Parser/parser.cc"
    break;

  case 479:
#line 2195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10873 "Parser/parser.cc"
    break;

  case 480:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10879 "Parser/parser.cc"
    break;

  case 486:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10889 "Parser/parser.cc"
    break;

  case 499:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10895 "Parser/parser.cc"
    break;

  case 502:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10901 "Parser/parser.cc"
    break;

  case 503:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10907 "Parser/parser.cc"
    break;

  case 505:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10913 "Parser/parser.cc"
    break;

  case 506:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 10919 "Parser/parser.cc"
    break;

  case 507:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 10925 "Parser/parser.cc"
    break;

  case 508:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 10931 "Parser/parser.cc"
    break;

  case 509:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 10937 "Parser/parser.cc"
    break;

  case 510:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10943 "Parser/parser.cc"
    break;

  case 512:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10949 "Parser/parser.cc"
    break;

  case 513:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10955 "Parser/parser.cc"
    break;

  case 515:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10961 "Parser/parser.cc"
    break;

  case 516:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10967 "Parser/parser.cc"
    break;

  case 517:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 10973 "Parser/parser.cc"
    break;

  case 518:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 10979 "Parser/parser.cc"
    break;

  case 519:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 10985 "Parser/parser.cc"
    break;

  case 520:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 10991 "Parser/parser.cc"
    break;

  case 521:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 10997 "Parser/parser.cc"
    break;

  case 522:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 11003 "Parser/parser.cc"
    break;

  case 523:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 11009 "Parser/parser.cc"
    break;

  case 524:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11015 "Parser/parser.cc"
    break;

  case 525:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11021 "Parser/parser.cc"
    break;

  case 526:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 11027 "Parser/parser.cc"
    break;

  case 527:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11033 "Parser/parser.cc"
    break;

  case 528:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11039 "Parser/parser.cc"
    break;

  case 529:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11045 "Parser/parser.cc"
    break;

  case 530:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11051 "Parser/parser.cc"
    break;

  case 531:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11057 "Parser/parser.cc"
    break;

  case 532:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11063 "Parser/parser.cc"
    break;

  case 533:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11069 "Parser/parser.cc"
    break;

  case 534:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11075 "Parser/parser.cc"
    break;

  case 535:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11081 "Parser/parser.cc"
    break;

  case 536:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11087 "Parser/parser.cc"
    break;

  case 537:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11093 "Parser/parser.cc"
    break;

  case 538:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11099 "Parser/parser.cc"
    break;

  case 539:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11105 "Parser/parser.cc"
    break;

  case 540:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11111 "Parser/parser.cc"
    break;

  case 541:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11117 "Parser/parser.cc"
    break;

  case 542:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11123 "Parser/parser.cc"
    break;

  case 543:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11129 "Parser/parser.cc"
    break;

  case 544:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11135 "Parser/parser.cc"
    break;

  case 545:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11141 "Parser/parser.cc"
    break;

  case 546:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11147 "Parser/parser.cc"
    break;

  case 547:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11153 "Parser/parser.cc"
    break;

  case 548:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11159 "Parser/parser.cc"
    break;

  case 549:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11165 "Parser/parser.cc"
    break;

  case 550:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11171 "Parser/parser.cc"
    break;

  case 551:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11177 "Parser/parser.cc"
    break;

  case 552:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11183 "Parser/parser.cc"
    break;

  case 554:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11189 "Parser/parser.cc"
    break;

  case 556:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11195 "Parser/parser.cc"
    break;

  case 557:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11201 "Parser/parser.cc"
    break;

  case 558:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11207 "Parser/parser.cc"
    break;

  case 560:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11213 "Parser/parser.cc"
    break;

  case 561:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11219 "Parser/parser.cc"
    break;

  case 562:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11225 "Parser/parser.cc"
    break;

  case 563:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11231 "Parser/parser.cc"
    break;

  case 565:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11237 "Parser/parser.cc"
    break;

  case 567:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11243 "Parser/parser.cc"
    break;

  case 568:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11249 "Parser/parser.cc"
    break;

  case 569:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11255 "Parser/parser.cc"
    break;

  case 570:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11261 "Parser/parser.cc"
    break;

  case 571:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11267 "Parser/parser.cc"
    break;

  case 572:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11273 "Parser/parser.cc"
    break;

  case 573:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11279 "Parser/parser.cc"
    break;

  case 574:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11285 "Parser/parser.cc"
    break;

  case 575:
#line 2463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11291 "Parser/parser.cc"
    break;

  case 577:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11297 "Parser/parser.cc"
    break;

  case 578:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11303 "Parser/parser.cc"
    break;

  case 579:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11309 "Parser/parser.cc"
    break;

  case 581:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11315 "Parser/parser.cc"
    break;

  case 582:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11321 "Parser/parser.cc"
    break;

  case 583:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11330 "Parser/parser.cc"
    break;

  case 585:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11336 "Parser/parser.cc"
    break;

  case 586:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11342 "Parser/parser.cc"
    break;

  case 587:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11348 "Parser/parser.cc"
    break;

  case 589:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11354 "Parser/parser.cc"
    break;

  case 590:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11360 "Parser/parser.cc"
    break;

  case 592:
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11366 "Parser/parser.cc"
    break;

  case 593:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11372 "Parser/parser.cc"
    break;

  case 594:
#line 2514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11378 "Parser/parser.cc"
    break;

  case 595:
#line 2519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11384 "Parser/parser.cc"
    break;

  case 596:
#line 2521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11390 "Parser/parser.cc"
    break;

  case 597:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11396 "Parser/parser.cc"
    break;

  case 598:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11402 "Parser/parser.cc"
    break;

  case 599:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11408 "Parser/parser.cc"
    break;

  case 600:
#line 2532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11414 "Parser/parser.cc"
    break;

  case 602:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11420 "Parser/parser.cc"
    break;

  case 603:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11426 "Parser/parser.cc"
    break;

  case 604:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11432 "Parser/parser.cc"
    break;

  case 605:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11438 "Parser/parser.cc"
    break;

  case 606:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11444 "Parser/parser.cc"
    break;

  case 611:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11450 "Parser/parser.cc"
    break;

  case 612:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11456 "Parser/parser.cc"
    break;

  case 613:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11465 "Parser/parser.cc"
    break;

  case 614:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11473 "Parser/parser.cc"
    break;

  case 615:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11482 "Parser/parser.cc"
    break;

  case 616:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11491 "Parser/parser.cc"
    break;

  case 617:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11500 "Parser/parser.cc"
    break;

  case 618:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11509 "Parser/parser.cc"
    break;

  case 620:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11515 "Parser/parser.cc"
    break;

  case 621:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11521 "Parser/parser.cc"
    break;

  case 622:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11531 "Parser/parser.cc"
    break;

  case 623:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11550 "Parser/parser.cc"
    break;

  case 626:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11556 "Parser/parser.cc"
    break;

  case 627:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11562 "Parser/parser.cc"
    break;

  case 628:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11568 "Parser/parser.cc"
    break;

  case 629:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11574 "Parser/parser.cc"
    break;

  case 630:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11580 "Parser/parser.cc"
    break;

  case 631:
#line 2649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11586 "Parser/parser.cc"
    break;

  case 632:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11595 "Parser/parser.cc"
    break;

  case 633:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11601 "Parser/parser.cc"
    break;

  case 634:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11610 "Parser/parser.cc"
    break;

  case 635:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11616 "Parser/parser.cc"
    break;

  case 636:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11625 "Parser/parser.cc"
    break;

  case 637:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11631 "Parser/parser.cc"
    break;

  case 638:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11637 "Parser/parser.cc"
    break;

  case 639:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11650 "Parser/parser.cc"
    break;

  case 640:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11659 "Parser/parser.cc"
    break;

  case 641:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11665 "Parser/parser.cc"
    break;

  case 642:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11671 "Parser/parser.cc"
    break;

  case 643:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11684 "Parser/parser.cc"
    break;

  case 644:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11690 "Parser/parser.cc"
    break;

  case 647:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11696 "Parser/parser.cc"
    break;

  case 648:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11702 "Parser/parser.cc"
    break;

  case 651:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11708 "Parser/parser.cc"
    break;

  case 653:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11714 "Parser/parser.cc"
    break;

  case 654:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11720 "Parser/parser.cc"
    break;

  case 655:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11726 "Parser/parser.cc"
    break;

  case 656:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11732 "Parser/parser.cc"
    break;

  case 657:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11738 "Parser/parser.cc"
    break;

  case 658:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11744 "Parser/parser.cc"
    break;

  case 660:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11750 "Parser/parser.cc"
    break;

  case 662:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11756 "Parser/parser.cc"
    break;

  case 663:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11762 "Parser/parser.cc"
    break;

  case 665:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11768 "Parser/parser.cc"
    break;

  case 666:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11774 "Parser/parser.cc"
    break;

  case 668:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11780 "Parser/parser.cc"
    break;

  case 669:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11791 "Parser/parser.cc"
    break;

  case 670:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11805 "Parser/parser.cc"
    break;

  case 671:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11811 "Parser/parser.cc"
    break;

  case 672:
#line 2805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11817 "Parser/parser.cc"
    break;

  case 673:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11823 "Parser/parser.cc"
    break;

  case 674:
#line 2809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11834 "Parser/parser.cc"
    break;

  case 675:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11840 "Parser/parser.cc"
    break;

  case 676:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11846 "Parser/parser.cc"
    break;

  case 678:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11852 "Parser/parser.cc"
    break;

  case 679:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11858 "Parser/parser.cc"
    break;

  case 680:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11864 "Parser/parser.cc"
    break;

  case 681:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11870 "Parser/parser.cc"
    break;

  case 682:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11879 "Parser/parser.cc"
    break;

  case 683:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11888 "Parser/parser.cc"
    break;

  case 684:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 11894 "Parser/parser.cc"
    break;

  case 685:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11900 "Parser/parser.cc"
    break;

  case 686:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 11910 "Parser/parser.cc"
    break;

  case 687:
#line 2863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11916 "Parser/parser.cc"
    break;

  case 688:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 11922 "Parser/parser.cc"
    break;

  case 690:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11928 "Parser/parser.cc"
    break;

  case 691:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11934 "Parser/parser.cc"
    break;

  case 692:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11940 "Parser/parser.cc"
    break;

  case 693:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11946 "Parser/parser.cc"
    break;

  case 694:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11952 "Parser/parser.cc"
    break;

  case 695:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11958 "Parser/parser.cc"
    break;

  case 697:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11964 "Parser/parser.cc"
    break;

  case 700:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11970 "Parser/parser.cc"
    break;

  case 701:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11976 "Parser/parser.cc"
    break;

  case 702:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 11982 "Parser/parser.cc"
    break;

  case 703:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11988 "Parser/parser.cc"
    break;

  case 706:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11994 "Parser/parser.cc"
    break;

  case 707:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12000 "Parser/parser.cc"
    break;

  case 708:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12006 "Parser/parser.cc"
    break;

  case 710:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12012 "Parser/parser.cc"
    break;

  case 711:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12018 "Parser/parser.cc"
    break;

  case 712:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12024 "Parser/parser.cc"
    break;

  case 714:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12030 "Parser/parser.cc"
    break;

  case 715:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12036 "Parser/parser.cc"
    break;

  case 716:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12042 "Parser/parser.cc"
    break;

  case 717:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12048 "Parser/parser.cc"
    break;

  case 718:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12054 "Parser/parser.cc"
    break;

  case 720:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12060 "Parser/parser.cc"
    break;

  case 721:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12066 "Parser/parser.cc"
    break;

  case 722:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12072 "Parser/parser.cc"
    break;

  case 727:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12078 "Parser/parser.cc"
    break;

  case 729:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12084 "Parser/parser.cc"
    break;

  case 730:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12090 "Parser/parser.cc"
    break;

  case 733:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12096 "Parser/parser.cc"
    break;

  case 736:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12102 "Parser/parser.cc"
    break;

  case 737:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12108 "Parser/parser.cc"
    break;

  case 738:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12114 "Parser/parser.cc"
    break;

  case 739:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12120 "Parser/parser.cc"
    break;

  case 740:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12126 "Parser/parser.cc"
    break;

  case 741:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12132 "Parser/parser.cc"
    break;

  case 742:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12138 "Parser/parser.cc"
    break;

  case 744:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12144 "Parser/parser.cc"
    break;

  case 745:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12150 "Parser/parser.cc"
    break;

  case 746:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12156 "Parser/parser.cc"
    break;

  case 748:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12162 "Parser/parser.cc"
    break;

  case 750:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12168 "Parser/parser.cc"
    break;

  case 751:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12174 "Parser/parser.cc"
    break;

  case 752:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12180 "Parser/parser.cc"
    break;

  case 753:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12186 "Parser/parser.cc"
    break;

  case 754:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12192 "Parser/parser.cc"
    break;

  case 755:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12198 "Parser/parser.cc"
    break;

  case 757:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12204 "Parser/parser.cc"
    break;

  case 758:
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12210 "Parser/parser.cc"
    break;

  case 759:
#line 3087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12216 "Parser/parser.cc"
    break;

  case 760:
#line 3092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12227 "Parser/parser.cc"
    break;

  case 761:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12233 "Parser/parser.cc"
    break;

  case 762:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12239 "Parser/parser.cc"
    break;

  case 763:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12245 "Parser/parser.cc"
    break;

  case 764:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12254 "Parser/parser.cc"
    break;

  case 765:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12260 "Parser/parser.cc"
    break;

  case 766:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12270 "Parser/parser.cc"
    break;

  case 767:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12276 "Parser/parser.cc"
    break;

  case 768:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12282 "Parser/parser.cc"
    break;

  case 769:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12288 "Parser/parser.cc"
    break;

  case 770:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12294 "Parser/parser.cc"
    break;

  case 771:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12300 "Parser/parser.cc"
    break;

  case 772:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12306 "Parser/parser.cc"
    break;

  case 773:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12312 "Parser/parser.cc"
    break;

  case 774:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12318 "Parser/parser.cc"
    break;

  case 775:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12324 "Parser/parser.cc"
    break;

  case 778:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12330 "Parser/parser.cc"
    break;

  case 779:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12336 "Parser/parser.cc"
    break;

  case 780:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12342 "Parser/parser.cc"
    break;

  case 781:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12348 "Parser/parser.cc"
    break;

  case 783:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12354 "Parser/parser.cc"
    break;

  case 784:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12360 "Parser/parser.cc"
    break;

  case 785:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12366 "Parser/parser.cc"
    break;

  case 786:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12372 "Parser/parser.cc"
    break;

  case 787:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12378 "Parser/parser.cc"
    break;

  case 788:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12384 "Parser/parser.cc"
    break;

  case 789:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12390 "Parser/parser.cc"
    break;

  case 790:
#line 3193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12399 "Parser/parser.cc"
    break;

  case 791:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12408 "Parser/parser.cc"
    break;

  case 792:
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12417 "Parser/parser.cc"
    break;

  case 793:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12423 "Parser/parser.cc"
    break;

  case 794:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12432 "Parser/parser.cc"
    break;

  case 795:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12438 "Parser/parser.cc"
    break;

  case 797:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12444 "Parser/parser.cc"
    break;

  case 802:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12450 "Parser/parser.cc"
    break;

  case 803:
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12456 "Parser/parser.cc"
    break;

  case 804:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12462 "Parser/parser.cc"
    break;

  case 806:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 12468 "Parser/parser.cc"
    break;

  case 807:
#line 3256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12474 "Parser/parser.cc"
    break;

  case 808:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12480 "Parser/parser.cc"
    break;

  case 809:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12486 "Parser/parser.cc"
    break;

  case 811:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12492 "Parser/parser.cc"
    break;

  case 812:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12498 "Parser/parser.cc"
    break;

  case 813:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12504 "Parser/parser.cc"
    break;

  case 814:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12520 "Parser/parser.cc"
    break;

  case 815:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12526 "Parser/parser.cc"
    break;

  case 816:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12532 "Parser/parser.cc"
    break;

  case 817:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12538 "Parser/parser.cc"
    break;

  case 818:
#line 3297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12544 "Parser/parser.cc"
    break;

  case 819:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12550 "Parser/parser.cc"
    break;

  case 820:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12556 "Parser/parser.cc"
    break;

  case 822:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12565 "Parser/parser.cc"
    break;

  case 823:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12571 "Parser/parser.cc"
    break;

  case 824:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12580 "Parser/parser.cc"
    break;

  case 825:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12590 "Parser/parser.cc"
    break;

  case 826:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12599 "Parser/parser.cc"
    break;

  case 827:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12609 "Parser/parser.cc"
    break;

  case 828:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12620 "Parser/parser.cc"
    break;

  case 829:
#line 3341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12630 "Parser/parser.cc"
    break;

  case 830:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12641 "Parser/parser.cc"
    break;

  case 831:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12651 "Parser/parser.cc"
    break;

  case 832:
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12662 "Parser/parser.cc"
    break;

  case 833:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12672 "Parser/parser.cc"
    break;

  case 834:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12678 "Parser/parser.cc"
    break;

  case 836:
#line 3384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12684 "Parser/parser.cc"
    break;

  case 837:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12690 "Parser/parser.cc"
    break;

  case 838:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12696 "Parser/parser.cc"
    break;

  case 839:
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12708 "Parser/parser.cc"
    break;

  case 840:
#line 3404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12719 "Parser/parser.cc"
    break;

  case 841:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12728 "Parser/parser.cc"
    break;

  case 842:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12737 "Parser/parser.cc"
    break;

  case 843:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12743 "Parser/parser.cc"
    break;

  case 844:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12749 "Parser/parser.cc"
    break;

  case 845:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12755 "Parser/parser.cc"
    break;

  case 846:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12764 "Parser/parser.cc"
    break;

  case 847:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12770 "Parser/parser.cc"
    break;

  case 848:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12776 "Parser/parser.cc"
    break;

  case 849:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12782 "Parser/parser.cc"
    break;

  case 854:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12788 "Parser/parser.cc"
    break;

  case 855:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12794 "Parser/parser.cc"
    break;

  case 856:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12804 "Parser/parser.cc"
    break;

  case 857:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12810 "Parser/parser.cc"
    break;

  case 860:
#line 3483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12816 "Parser/parser.cc"
    break;

  case 861:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12822 "Parser/parser.cc"
    break;

  case 862:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12828 "Parser/parser.cc"
    break;

  case 863:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12834 "Parser/parser.cc"
    break;

  case 865:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12840 "Parser/parser.cc"
    break;

  case 866:
#line 3503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12846 "Parser/parser.cc"
    break;

  case 867:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12852 "Parser/parser.cc"
    break;

  case 868:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12858 "Parser/parser.cc"
    break;

  case 870:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12864 "Parser/parser.cc"
    break;

  case 871:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12870 "Parser/parser.cc"
    break;

  case 872:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12876 "Parser/parser.cc"
    break;

  case 873:
#line 3553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12882 "Parser/parser.cc"
    break;

  case 874:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12888 "Parser/parser.cc"
    break;

  case 875:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12894 "Parser/parser.cc"
    break;

  case 877:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12900 "Parser/parser.cc"
    break;

  case 878:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12906 "Parser/parser.cc"
    break;

  case 879:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12912 "Parser/parser.cc"
    break;

  case 880:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12918 "Parser/parser.cc"
    break;

  case 881:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12924 "Parser/parser.cc"
    break;

  case 882:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12930 "Parser/parser.cc"
    break;

  case 883:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12936 "Parser/parser.cc"
    break;

  case 884:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12942 "Parser/parser.cc"
    break;

  case 885:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12948 "Parser/parser.cc"
    break;

  case 886:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12954 "Parser/parser.cc"
    break;

  case 887:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12960 "Parser/parser.cc"
    break;

  case 888:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12966 "Parser/parser.cc"
    break;

  case 889:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12972 "Parser/parser.cc"
    break;

  case 890:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12978 "Parser/parser.cc"
    break;

  case 891:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12984 "Parser/parser.cc"
    break;

  case 892:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12990 "Parser/parser.cc"
    break;

  case 893:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12996 "Parser/parser.cc"
    break;

  case 894:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13002 "Parser/parser.cc"
    break;

  case 896:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13008 "Parser/parser.cc"
    break;

  case 897:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13014 "Parser/parser.cc"
    break;

  case 898:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13020 "Parser/parser.cc"
    break;

  case 899:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13026 "Parser/parser.cc"
    break;

  case 900:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13032 "Parser/parser.cc"
    break;

  case 901:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13038 "Parser/parser.cc"
    break;

  case 902:
#line 3634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13044 "Parser/parser.cc"
    break;

  case 903:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13050 "Parser/parser.cc"
    break;

  case 904:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13056 "Parser/parser.cc"
    break;

  case 905:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13062 "Parser/parser.cc"
    break;

  case 906:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13068 "Parser/parser.cc"
    break;

  case 907:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13074 "Parser/parser.cc"
    break;

  case 908:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13080 "Parser/parser.cc"
    break;

  case 909:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13086 "Parser/parser.cc"
    break;

  case 910:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13092 "Parser/parser.cc"
    break;

  case 911:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13098 "Parser/parser.cc"
    break;

  case 915:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13104 "Parser/parser.cc"
    break;

  case 916:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13110 "Parser/parser.cc"
    break;

  case 917:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13116 "Parser/parser.cc"
    break;

  case 918:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13122 "Parser/parser.cc"
    break;

  case 919:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13128 "Parser/parser.cc"
    break;

  case 920:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13134 "Parser/parser.cc"
    break;

  case 921:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13140 "Parser/parser.cc"
    break;

  case 922:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13146 "Parser/parser.cc"
    break;

  case 923:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13152 "Parser/parser.cc"
    break;

  case 924:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13158 "Parser/parser.cc"
    break;

  case 925:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13164 "Parser/parser.cc"
    break;

  case 926:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13170 "Parser/parser.cc"
    break;

  case 927:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13176 "Parser/parser.cc"
    break;

  case 928:
#line 3705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13182 "Parser/parser.cc"
    break;

  case 929:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13188 "Parser/parser.cc"
    break;

  case 930:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13197 "Parser/parser.cc"
    break;

  case 931:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13203 "Parser/parser.cc"
    break;

  case 932:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13209 "Parser/parser.cc"
    break;

  case 934:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13215 "Parser/parser.cc"
    break;

  case 935:
#line 3734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13221 "Parser/parser.cc"
    break;

  case 936:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13227 "Parser/parser.cc"
    break;

  case 937:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13233 "Parser/parser.cc"
    break;

  case 938:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13239 "Parser/parser.cc"
    break;

  case 939:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13245 "Parser/parser.cc"
    break;

  case 940:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13251 "Parser/parser.cc"
    break;

  case 941:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13257 "Parser/parser.cc"
    break;

  case 942:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13263 "Parser/parser.cc"
    break;

  case 943:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13269 "Parser/parser.cc"
    break;

  case 944:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13275 "Parser/parser.cc"
    break;

  case 945:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13281 "Parser/parser.cc"
    break;

  case 946:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13287 "Parser/parser.cc"
    break;

  case 947:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13293 "Parser/parser.cc"
    break;

  case 948:
#line 3769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13299 "Parser/parser.cc"
    break;

  case 949:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13305 "Parser/parser.cc"
    break;

  case 950:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13311 "Parser/parser.cc"
    break;

  case 951:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13317 "Parser/parser.cc"
    break;

  case 953:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13323 "Parser/parser.cc"
    break;

  case 954:
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13329 "Parser/parser.cc"
    break;

  case 955:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13335 "Parser/parser.cc"
    break;

  case 956:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13341 "Parser/parser.cc"
    break;

  case 957:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13347 "Parser/parser.cc"
    break;

  case 958:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13353 "Parser/parser.cc"
    break;

  case 959:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13359 "Parser/parser.cc"
    break;

  case 960:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13365 "Parser/parser.cc"
    break;

  case 961:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13371 "Parser/parser.cc"
    break;

  case 962:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13377 "Parser/parser.cc"
    break;

  case 963:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13383 "Parser/parser.cc"
    break;

  case 964:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13389 "Parser/parser.cc"
    break;

  case 965:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13395 "Parser/parser.cc"
    break;

  case 966:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13401 "Parser/parser.cc"
    break;

  case 967:
#line 3822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13407 "Parser/parser.cc"
    break;

  case 968:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13413 "Parser/parser.cc"
    break;

  case 969:
#line 3834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13419 "Parser/parser.cc"
    break;

  case 970:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13426 "Parser/parser.cc"
    break;

  case 972:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13432 "Parser/parser.cc"
    break;

  case 973:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13438 "Parser/parser.cc"
    break;

  case 974:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13444 "Parser/parser.cc"
    break;

  case 975:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13450 "Parser/parser.cc"
    break;

  case 976:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13456 "Parser/parser.cc"
    break;

  case 977:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13462 "Parser/parser.cc"
    break;

  case 978:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13468 "Parser/parser.cc"
    break;

  case 979:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13474 "Parser/parser.cc"
    break;

  case 980:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13480 "Parser/parser.cc"
    break;

  case 981:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13486 "Parser/parser.cc"
    break;

  case 982:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13492 "Parser/parser.cc"
    break;

  case 983:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13498 "Parser/parser.cc"
    break;

  case 984:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13504 "Parser/parser.cc"
    break;

  case 985:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13511 "Parser/parser.cc"
    break;

  case 987:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13517 "Parser/parser.cc"
    break;

  case 988:
#line 3893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13523 "Parser/parser.cc"
    break;

  case 989:
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13529 "Parser/parser.cc"
    break;

  case 990:
#line 3900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13535 "Parser/parser.cc"
    break;

  case 991:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13541 "Parser/parser.cc"
    break;

  case 992:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13547 "Parser/parser.cc"
    break;

  case 993:
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13553 "Parser/parser.cc"
    break;

  case 994:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13559 "Parser/parser.cc"
    break;

  case 995:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13565 "Parser/parser.cc"
    break;

  case 996:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13571 "Parser/parser.cc"
    break;

  case 997:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13577 "Parser/parser.cc"
    break;

  case 999:
#line 3941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13583 "Parser/parser.cc"
    break;

  case 1000:
#line 3943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13589 "Parser/parser.cc"
    break;

  case 1001:
#line 3948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13595 "Parser/parser.cc"
    break;

  case 1002:
#line 3950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13601 "Parser/parser.cc"
    break;

  case 1003:
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13607 "Parser/parser.cc"
    break;

  case 1004:
#line 3954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13613 "Parser/parser.cc"
    break;

  case 1005:
#line 3956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13619 "Parser/parser.cc"
    break;

  case 1007:
#line 3962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13625 "Parser/parser.cc"
    break;

  case 1008:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13631 "Parser/parser.cc"
    break;

  case 1009:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13637 "Parser/parser.cc"
    break;

  case 1010:
#line 3971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13643 "Parser/parser.cc"
    break;

  case 1011:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13649 "Parser/parser.cc"
    break;

  case 1012:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13655 "Parser/parser.cc"
    break;

  case 1013:
#line 3981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13661 "Parser/parser.cc"
    break;

  case 1014:
#line 3983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13667 "Parser/parser.cc"
    break;

  case 1015:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13673 "Parser/parser.cc"
    break;

  case 1016:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13679 "Parser/parser.cc"
    break;

  case 1018:
#line 4004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13685 "Parser/parser.cc"
    break;

  case 1019:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13691 "Parser/parser.cc"
    break;

  case 1021:
#line 4009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13697 "Parser/parser.cc"
    break;

  case 1022:
#line 4011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13703 "Parser/parser.cc"
    break;

  case 1024:
#line 4017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13709 "Parser/parser.cc"
    break;

  case 1025:
#line 4019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13715 "Parser/parser.cc"
    break;

  case 1026:
#line 4024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13721 "Parser/parser.cc"
    break;

  case 1027:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13727 "Parser/parser.cc"
    break;

  case 1028:
#line 4028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13733 "Parser/parser.cc"
    break;

  case 1029:
#line 4030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13739 "Parser/parser.cc"
    break;

  case 1030:
#line 4064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13745 "Parser/parser.cc"
    break;

  case 1033:
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13752 "Parser/parser.cc"
    break;

  case 1034:
#line 4074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13758 "Parser/parser.cc"
    break;

  case 1035:
#line 4076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13764 "Parser/parser.cc"
    break;

  case 1036:
#line 4081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13770 "Parser/parser.cc"
    break;

  case 1037:
#line 4083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13776 "Parser/parser.cc"
    break;

  case 1038:
#line 4085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13782 "Parser/parser.cc"
    break;

  case 1039:
#line 4087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13788 "Parser/parser.cc"
    break;

  case 1040:
#line 4089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13794 "Parser/parser.cc"
    break;

  case 1042:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13800 "Parser/parser.cc"
    break;

  case 1043:
#line 4097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13806 "Parser/parser.cc"
    break;

  case 1044:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13812 "Parser/parser.cc"
    break;

  case 1045:
#line 4104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13818 "Parser/parser.cc"
    break;

  case 1046:
#line 4106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13824 "Parser/parser.cc"
    break;

  case 1047:
#line 4108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13830 "Parser/parser.cc"
    break;

  case 1049:
#line 4115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13836 "Parser/parser.cc"
    break;

  case 1051:
#line 4126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13842 "Parser/parser.cc"
    break;

  case 1052:
#line 4129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13848 "Parser/parser.cc"
    break;

  case 1053:
#line 4131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13854 "Parser/parser.cc"
    break;

  case 1054:
#line 4134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13860 "Parser/parser.cc"
    break;

  case 1055:
#line 4136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13866 "Parser/parser.cc"
    break;

  case 1056:
#line 4138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13872 "Parser/parser.cc"
    break;

  case 1058:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13878 "Parser/parser.cc"
    break;

  case 1059:
#line 4155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13884 "Parser/parser.cc"
    break;

  case 1060:
#line 4160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13890 "Parser/parser.cc"
    break;

  case 1061:
#line 4162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13896 "Parser/parser.cc"
    break;

  case 1062:
#line 4164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13902 "Parser/parser.cc"
    break;

  case 1063:
#line 4166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13908 "Parser/parser.cc"
    break;

  case 1064:
#line 4168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13914 "Parser/parser.cc"
    break;

  case 1066:
#line 4174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13920 "Parser/parser.cc"
    break;

  case 1067:
#line 4176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13926 "Parser/parser.cc"
    break;

  case 1068:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13932 "Parser/parser.cc"
    break;

  case 1069:
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13938 "Parser/parser.cc"
    break;

  case 1070:
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13944 "Parser/parser.cc"
    break;

  case 1073:
#line 4195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13950 "Parser/parser.cc"
    break;

  case 1076:
#line 4206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13956 "Parser/parser.cc"
    break;

  case 1077:
#line 4208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13962 "Parser/parser.cc"
    break;

  case 1078:
#line 4210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13968 "Parser/parser.cc"
    break;

  case 1079:
#line 4212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13974 "Parser/parser.cc"
    break;

  case 1080:
#line 4214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13980 "Parser/parser.cc"
    break;

  case 1081:
#line 4216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13986 "Parser/parser.cc"
    break;

  case 1082:
#line 4223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13992 "Parser/parser.cc"
    break;

  case 1083:
#line 4225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13998 "Parser/parser.cc"
    break;

  case 1084:
#line 4227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14004 "Parser/parser.cc"
    break;

  case 1085:
#line 4229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14010 "Parser/parser.cc"
    break;

  case 1086:
#line 4231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14016 "Parser/parser.cc"
    break;

  case 1087:
#line 4234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14022 "Parser/parser.cc"
    break;

  case 1088:
#line 4236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14028 "Parser/parser.cc"
    break;

  case 1089:
#line 4238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14034 "Parser/parser.cc"
    break;

  case 1090:
#line 4240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14040 "Parser/parser.cc"
    break;

  case 1091:
#line 4242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14046 "Parser/parser.cc"
    break;

  case 1092:
#line 4247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14052 "Parser/parser.cc"
    break;

  case 1093:
#line 4249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14058 "Parser/parser.cc"
    break;

  case 1094:
#line 4254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14064 "Parser/parser.cc"
    break;

  case 1095:
#line 4256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14070 "Parser/parser.cc"
    break;

  case 1097:
#line 4283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14076 "Parser/parser.cc"
    break;

  case 1101:
#line 4294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14082 "Parser/parser.cc"
    break;

  case 1102:
#line 4296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14088 "Parser/parser.cc"
    break;

  case 1103:
#line 4298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14094 "Parser/parser.cc"
    break;

  case 1104:
#line 4300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14100 "Parser/parser.cc"
    break;

  case 1105:
#line 4302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14106 "Parser/parser.cc"
    break;

  case 1106:
#line 4304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14112 "Parser/parser.cc"
    break;

  case 1107:
#line 4311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14118 "Parser/parser.cc"
    break;

  case 1108:
#line 4313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14124 "Parser/parser.cc"
    break;

  case 1109:
#line 4315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14130 "Parser/parser.cc"
    break;

  case 1110:
#line 4317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14136 "Parser/parser.cc"
    break;

  case 1111:
#line 4319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14142 "Parser/parser.cc"
    break;

  case 1112:
#line 4321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14148 "Parser/parser.cc"
    break;

  case 1113:
#line 4326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14154 "Parser/parser.cc"
    break;

  case 1114:
#line 4328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14160 "Parser/parser.cc"
    break;

  case 1115:
#line 4330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14166 "Parser/parser.cc"
    break;

  case 1116:
#line 4335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14172 "Parser/parser.cc"
    break;

  case 1117:
#line 4337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14178 "Parser/parser.cc"
    break;

  case 1118:
#line 4339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14184 "Parser/parser.cc"
    break;

  case 1121:
#line 4363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14190 "Parser/parser.cc"
    break;

  case 1122:
#line 4365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14196 "Parser/parser.cc"
    break;


#line 14200 "Parser/parser.cc"

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
#line 4368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
