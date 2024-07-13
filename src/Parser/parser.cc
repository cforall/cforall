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
#define YYFINAL  149
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   26484

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  183
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1119
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2204

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
     856,   857,   861,   862,   867,   870,   874,   875,   879,   880,
     882,   884,   886,   888,   890,   895,   897,   899,   907,   908,
     916,   919,   920,   922,   927,   943,   945,   947,   949,   951,
     953,   955,   960,   962,   965,   967,   972,   974,   979,   980,
     982,   986,   987,   988,   989,   993,   994,   996,   998,  1000,
    1002,  1004,  1006,  1008,  1015,  1016,  1017,  1018,  1022,  1023,
    1027,  1028,  1033,  1034,  1036,  1038,  1043,  1044,  1046,  1051,
    1052,  1054,  1059,  1060,  1062,  1064,  1066,  1071,  1072,  1074,
    1079,  1080,  1085,  1086,  1091,  1092,  1097,  1098,  1103,  1104,
    1109,  1110,  1112,  1117,  1122,  1123,  1131,  1137,  1138,  1142,
    1143,  1147,  1148,  1152,  1153,  1154,  1155,  1156,  1157,  1158,
    1159,  1160,  1161,  1162,  1172,  1174,  1179,  1180,  1182,  1184,
    1189,  1190,  1196,  1197,  1203,  1204,  1205,  1206,  1207,  1208,
    1209,  1210,  1211,  1212,  1213,  1214,  1215,  1216,  1218,  1219,
    1225,  1227,  1237,  1239,  1247,  1248,  1253,  1255,  1257,  1259,
    1261,  1265,  1266,  1268,  1274,  1303,  1306,  1308,  1310,  1320,
    1322,  1324,  1329,  1334,  1336,  1338,  1340,  1348,  1349,  1351,
    1355,  1357,  1361,  1363,  1364,  1366,  1368,  1373,  1374,  1378,
    1383,  1384,  1388,  1390,  1395,  1397,  1402,  1404,  1406,  1408,
    1413,  1415,  1417,  1419,  1424,  1426,  1431,  1432,  1454,  1456,
    1460,  1463,  1465,  1468,  1470,  1473,  1475,  1480,  1485,  1487,
    1492,  1497,  1499,  1501,  1503,  1505,  1510,  1512,  1515,  1517,
    1522,  1528,  1531,  1533,  1538,  1544,  1546,  1551,  1557,  1560,
    1562,  1565,  1567,  1572,  1579,  1581,  1586,  1592,  1594,  1599,
    1605,  1608,  1612,  1623,  1626,  1629,  1638,  1640,  1642,  1644,
    1649,  1651,  1653,  1658,  1659,  1661,  1666,  1668,  1673,  1675,
    1677,  1679,  1682,  1686,  1689,  1693,  1695,  1697,  1699,  1701,
    1703,  1705,  1707,  1709,  1711,  1713,  1718,  1719,  1723,  1729,
    1737,  1742,  1743,  1747,  1748,  1753,  1757,  1758,  1761,  1763,
    1768,  1771,  1773,  1775,  1778,  1780,  1785,  1790,  1791,  1795,
    1800,  1802,  1807,  1809,  1814,  1816,  1818,  1823,  1828,  1833,
    1838,  1840,  1842,  1847,  1849,  1855,  1856,  1860,  1861,  1862,
    1863,  1867,  1872,  1873,  1875,  1877,  1879,  1883,  1887,  1888,
    1892,  1894,  1896,  1898,  1900,  1906,  1907,  1913,  1914,  1918,
    1919,  1924,  1926,  1935,  1936,  1938,  1943,  1948,  1959,  1960,
    1964,  1965,  1971,  1972,  1976,  1978,  1982,  1984,  1988,  1989,
    1993,  1994,  1998,  1999,  2000,  2004,  2006,  2021,  2022,  2023,
    2024,  2026,  2030,  2032,  2036,  2043,  2045,  2047,  2055,  2057,
    2062,  2063,  2065,  2067,  2069,  2079,  2081,  2093,  2096,  2101,
    2103,  2109,  2114,  2119,  2130,  2137,  2142,  2144,  2146,  2152,
    2156,  2163,  2165,  2166,  2167,  2183,  2185,  2188,  2190,  2193,
    2198,  2199,  2203,  2204,  2205,  2206,  2215,  2216,  2217,  2226,
    2227,  2228,  2232,  2233,  2234,  2243,  2244,  2245,  2250,  2251,
    2260,  2261,  2266,  2268,  2272,  2274,  2276,  2278,  2285,  2290,
    2295,  2296,  2298,  2308,  2309,  2314,  2316,  2318,  2320,  2322,
    2324,  2327,  2329,  2331,  2336,  2342,  2344,  2346,  2348,  2350,
    2352,  2354,  2356,  2358,  2360,  2362,  2364,  2366,  2368,  2370,
    2372,  2374,  2376,  2378,  2380,  2382,  2384,  2386,  2388,  2390,
    2392,  2394,  2396,  2401,  2402,  2406,  2412,  2413,  2419,  2420,
    2422,  2424,  2426,  2431,  2433,  2438,  2439,  2441,  2443,  2448,
    2450,  2452,  2454,  2456,  2458,  2463,  2464,  2466,  2468,  2473,
    2475,  2474,  2478,  2486,  2487,  2489,  2491,  2496,  2497,  2499,
    2504,  2505,  2507,  2509,  2514,  2516,  2518,  2523,  2525,  2527,
    2529,  2530,  2532,  2537,  2539,  2541,  2546,  2547,  2551,  2552,
    2559,  2558,  2563,  2562,  2572,  2571,  2582,  2581,  2591,  2596,
    2597,  2602,  2608,  2626,  2627,  2631,  2633,  2635,  2640,  2642,
    2644,  2646,  2651,  2653,  2658,  2660,  2669,  2670,  2675,  2684,
    2689,  2691,  2693,  2702,  2704,  2705,  2706,  2708,  2710,  2711,
    2716,  2717,  2718,  2723,  2725,  2728,  2731,  2738,  2739,  2740,
    2746,  2751,  2753,  2759,  2760,  2766,  2767,  2771,  2779,  2786,
    2799,  2798,  2802,  2805,  2804,  2813,  2817,  2821,  2823,  2829,
    2830,  2835,  2840,  2848,  2850,  2856,  2858,  2863,  2864,  2870,
    2871,  2872,  2881,  2882,  2884,  2885,  2890,  2891,  2892,  2894,
    2900,  2901,  2903,  2904,  2905,  2907,  2909,  2916,  2917,  2919,
    2921,  2926,  2927,  2936,  2938,  2943,  2945,  2950,  2951,  2953,
    2956,  2958,  2962,  2963,  2964,  2966,  2968,  2976,  2978,  2983,
    2984,  2985,  2990,  2991,  2996,  2997,  2998,  2999,  3003,  3004,
    3009,  3010,  3011,  3012,  3013,  3027,  3028,  3033,  3034,  3040,
    3042,  3045,  3047,  3049,  3072,  3073,  3079,  3080,  3086,  3085,
    3095,  3094,  3098,  3104,  3106,  3116,  3117,  3119,  3123,  3128,
    3130,  3132,  3134,  3140,  3141,  3145,  3146,  3151,  3153,  3160,
    3162,  3163,  3165,  3170,  3172,  3174,  3179,  3181,  3186,  3191,
    3199,  3204,  3206,  3211,  3216,  3217,  3222,  3223,  3227,  3228,
    3229,  3234,  3236,  3242,  3244,  3249,  3251,  3257,  3258,  3262,
    3266,  3270,  3272,  3284,  3286,  3288,  3290,  3292,  3294,  3296,
    3297,  3302,  3305,  3304,  3316,  3315,  3328,  3327,  3341,  3340,
    3354,  3353,  3366,  3371,  3377,  3379,  3385,  3386,  3397,  3404,
    3409,  3415,  3418,  3421,  3425,  3431,  3434,  3437,  3442,  3443,
    3444,  3445,  3449,  3457,  3458,  3470,  3471,  3475,  3476,  3481,
    3483,  3485,  3490,  3491,  3497,  3498,  3500,  3505,  3506,  3508,
    3543,  3545,  3550,  3552,  3553,  3555,  3560,  3562,  3564,  3566,
    3571,  3573,  3575,  3577,  3579,  3581,  3583,  3588,  3590,  3592,
    3594,  3603,  3605,  3606,  3611,  3613,  3615,  3617,  3619,  3624,
    3626,  3628,  3630,  3635,  3637,  3639,  3641,  3643,  3645,  3657,
    3658,  3659,  3663,  3665,  3667,  3669,  3671,  3676,  3678,  3680,
    3682,  3687,  3689,  3691,  3693,  3695,  3697,  3709,  3714,  3719,
    3721,  3722,  3724,  3729,  3731,  3733,  3735,  3740,  3742,  3744,
    3746,  3748,  3750,  3752,  3757,  3759,  3761,  3763,  3772,  3774,
    3775,  3780,  3782,  3784,  3786,  3788,  3793,  3795,  3797,  3799,
    3804,  3806,  3808,  3810,  3812,  3814,  3824,  3826,  3829,  3830,
    3832,  3837,  3839,  3841,  3846,  3848,  3850,  3852,  3857,  3859,
    3861,  3875,  3877,  3880,  3881,  3883,  3888,  3890,  3895,  3897,
    3899,  3904,  3906,  3911,  3913,  3930,  3931,  3933,  3938,  3940,
    3942,  3944,  3946,  3951,  3952,  3954,  3956,  3961,  3963,  3965,
    3971,  3973,  3976,  3983,  3985,  3994,  3996,  3998,  3999,  4001,
    4003,  4007,  4009,  4014,  4016,  4018,  4020,  4055,  4056,  4060,
    4061,  4064,  4066,  4071,  4073,  4075,  4077,  4079,  4084,  4085,
    4087,  4089,  4094,  4096,  4098,  4104,  4105,  4107,  4116,  4119,
    4121,  4124,  4126,  4128,  4142,  4143,  4145,  4150,  4152,  4154,
    4156,  4158,  4163,  4164,  4166,  4168,  4173,  4175,  4183,  4184,
    4185,  4190,  4191,  4196,  4198,  4200,  4202,  4204,  4206,  4213,
    4215,  4217,  4219,  4221,  4224,  4226,  4228,  4230,  4232,  4237,
    4239,  4241,  4246,  4272,  4273,  4275,  4279,  4280,  4284,  4286,
    4288,  4290,  4292,  4294,  4301,  4303,  4305,  4307,  4309,  4311,
    4316,  4318,  4320,  4325,  4327,  4329,  4347,  4349,  4354,  4355
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
     405,   406,   407,   408,   409,   410,   125,    40,    64,    41,
      46,    91,    93,    44,    58,   123,    96,    94,    42,    38,
      43,    45,    33,   126,    92,    47,    37,    60,    62,   124,
      63,    61,    59
};
# endif

#define YYPACT_NINF (-1853)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1118)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     146, 11772,   174,   375, 20071,    16, -1853, -1853, -1853, -1853,
   -1853, -1853, -1853, -1853, -1853, -1853, -1853, -1853,   179,   924,
     278, -1853, -1853, -1853, -1853, -1853, -1853, -1853, -1853, -1853,
   -1853, -1853, -1853, -1853, -1853, -1853, -1853, -1853, -1853, -1853,
   -1853, -1853, -1853, -1853, -1853, -1853, -1853, -1853,    83,   361,
   -1853, -1853, -1853, -1853, -1853, -1853,  5170,  5170,   298, 11772,
     304,   342, 23612, -1853,   420, -1853, -1853, -1853, -1853, -1853,
   -1853, -1853, -1853, -1853, -1853,   435,  3340, -1853,   142,   446,
   -1853, -1853, -1853, -1853, -1853, -1853, 19600, -1853,   553,   556,
     581,   440,    85, -1853,  5014,   590,   611,   633,   574,  5679,
     802,   979, 13553, -1853, -1853,   547, 19443,  2257, -1853, -1853,
   -1853, -1853,  2757,   813, 17128,  9779,   796,  2757,  1215,   668,
   -1853, -1853, -1853, -1853,   135, -1853, -1853, -1853, -1853,   672,
   -1853, -1853, -1853, -1853, -1853,   695,   728,   135, -1853,   135,
   17804, -1853, -1853, -1853, 21333,  5170, -1853, -1853,  5170, -1853,
   11772, -1853,   704, 21387, -1853, -1853,  5863, 22609, -1853, -1853,
     875,   875,   735,  2581, -1853, -1853, -1853, -1853,   337, 16078,
     135,  3255,   135, -1853, -1853, -1853, -1853, -1853, -1853,   745,
   -1853,   734,   777,  2218, -1853,   832, 25755, -1853, -1853, -1853,
   -1853, -1853, -1853, -1853, 18188,  2548,  4906,  3340,   417,   778,
     804,   822,   825,   835,   845, -1853, -1853, 20228, 12293,   829,
     854, -1853, 20449, -1853, -1853, -1853, -1853,   893, -1853, -1853,
   -1853,   851, -1853, 23671,  1015, 23825, -1853,   914,  5170,   728,
     923,   939, -1853,  4001,  5863,  4001, -1853, -1853, -1853,  3522,
    4423,   898,  1001,    52,  1001, -1853,   135,   135,    97, 17478,
     204,  1001, -1853,   135,   135,    97,   135, -1853,   135, -1853,
    4494, -1853, -1853,   948,   954,   875,  9308,   963, 19600, -1853,
    5014, -1853,  2757, -1853,  2188,   668,   974,  1055, 17478,  5170,
    5170,   440, -1853, 15248, -1853,   875,   875,   982,  1055, 17478,
    5170, -1853,  8418, -1853, -1853, -1853,   875, -1853, -1853, -1853,
   -1853,   875, -1853,   866,  4654,  5170, -1853, 19295,   998, -1853,
   -1853, -1853, 23279,   728, 17641,   980,  5863, 19241,  9308,  2757,
   -1853, -1853, 22814, -1853,  1001,    33, -1853, 25755, 22759,  3974,
    4494, -1853,   339, -1853, -1853, -1853, -1853, -1853, 21387,  1001,
    5170, -1853,  1005,  1019, -1853, -1853, -1853, -1853,  5170,  3284,
     388,   248, -1853,  5170,   734, -1853,   880,   135, -1853,  1025,
   21544,   748, 16576, 23418,  2757, -1853,  2757,   875,  2757,   875,
   -1853, -1853,   135, -1853, -1853,  1030, 21598, -1853, -1853, -1853,
   21755,   893, -1853,  2925,   454,   676, -1853,   501,   668,  1041,
    1031, -1853,  2581,  1033,   734,  2581, -1853, -1853,  2548, -1853,
     561, -1853,  1064, -1853,  1053,  1120, 25832,  1089, 25909,  1092,
    1095, 25755, 25986,  1101, 23664, -1853, -1853, -1853, -1853, -1853,
   -1853, 26063, 26063, 18027,  1097,  4873, -1853, -1853, -1853, -1853,
     525, -1853,   693, -1853,  1157, -1853, 25755, 25755, -1853,  1102,
     739,   946,   959,   549,  1003,  1110,  1115,  1108,  1161,   100,
   -1853,   631, -1853,  1138, -1853,   960,  5320, 18671, -1853, -1853,
     941,  1138, -1853, -1853,   840, -1853, -1853,   846,  4906,  1162,
    1171,  1202,  1205,  1208,  1214, -1853, -1853,   351,  1155, -1853,
     766,  1155,  1223, -1853,  1249, -1853, 21333, -1853,  1106,  1252,
   18832, -1853, -1853,  4894,  5068,  1278, 16576,  1286,  1117,  1369,
    1269,  1297, -1853, -1853, -1853,  5170,  5145, 20808, -1853, -1853,
   -1853, -1853, -1853, -1853, -1853,  7371,  4532,  1097, 23671,  1298,
    1300, -1853, -1853,  1306, 23825,   861, -1853, -1853, -1853, 18671,
    1323, -1853,   832, -1853, -1853, -1853,  1301,  3522,   873,  1327,
    1349,  1351,   940,  1361,  1377,  1379,  1383,  1397,  1418,  4423,
   -1853, -1853, -1853,   135,  1348,  1389, -1853, -1853,  1406,   440,
   -1853, -1853,   728,  1055, 20394, -1853, -1853,   440, -1853, -1853,
     728, -1853, -1853,  4494, -1853, 18671, 18671, -1853,   875,  5863,
   23473,  2776, 16742, -1853, -1853, -1853, -1853, -1853, -1853,   728,
    1055,    33,  1416, -1853, -1853,  2757,  1432,  1055, 17478, -1853,
     728,  1055, -1853, 26224, -1853,   875,   875, -1853, -1853,  1436,
     266,  1439,   668,  1444, -1853, -1853, -1853, 20754,  1454,  1465,
   -1853, -1853,   857, -1853,  1558, -1853,  1458, -1853, -1853, -1853,
   21921, 26227, -1853, -1853, -1853, -1853, -1853,  3974,  1004,  4494,
   20394,  1001, 11772, -1853,  5170,  1479, -1853,  1487, -1853, -1853,
   -1853, -1853, -1853,  2581, -1853, -1853,  1567,  4722, 20965, 12293,
   -1853, 21975, -1853,   875,   875, -1853, -1853,   893, -1853, 15580,
    1485,  1631, 25755,  1194,  1406,  1469, -1853,   135,   135, -1853,
    1155, -1853, 21544, -1853, -1853, 20754,   875,   875, -1853,  4722,
   -1853, -1853, 22554, -1853, -1853, 21598, -1853,   135,  1489,   135,
    1031,   109,  1494,   897, 21387,   904,   918, -1853,  2548, 23902,
    1470, -1853, 18349, -1853,  4873, 18510, -1853, 22132, 21387, -1853,
   18349, -1853, 25755, -1853, -1853, -1853, -1853, -1853, -1853, 18510,
   -1853, -1853, 21019, 22132, 22132,  1186,  1531,  1548,   687,  1607,
   -1853,   931,  1497,  1219,  1502, -1853, 23979, 25755, 24056,  1499,
   25755,  4001, 25755,  4001, -1853,  4613, -1853, -1853, 23902,  3225,
   25755, 23902,  4001, -1853, -1853, 25755, 25755, 25755, 25755, 25755,
   25755, 25755, 25755, 25755, 25755, 25755, 25755, 25755, 25755, 25755,
   25755, 25755, 25755, 25755, 24133, -1853,   832,  3416, 12293, -1853,
   -1853, -1853, -1853, -1853, -1853, -1853, -1853, -1853, -1853, -1853,
    1501, 25755, -1853, -1853, 15746,  2136, -1853, -1853,   135,   135,
   -1853, -1853, 18671, -1853, -1853,   445,  1155, -1853,   989,  1155,
   20394, -1853, -1853,  1406, 20394, -1853,  1406, -1853, 26311, -1853,
   -1853, -1853, 19914, 12293,  1500,  1256,  1506, 15082,  1652,  3704,
     485,  1469, -1853,   135,   135,  1469,   522, -1853,   135,   135,
   25755,  5170, 16742,  1510, 16742,  1512,  1469,   192, 15912, 15912,
   16908, 15912,  5170, -1853, -1853, 25755,  1306, -1853, 23671,  1521,
   -1853,  3044, -1853, -1853, -1853,  1016, -1853,  1520, 15912, 25755,
    1038,  1523,  1526,  1527,  1043,  1528,  1529,  1533,  1534,  1536,
    1537,   614,  1155, -1853, -1853,   643,  1155, -1853, -1853,   658,
    1155, -1853, -1853, -1853,  5863,  1658,  1155, 22964, -1853, -1853,
     728,  1538, -1853, -1853, -1853,  1054,  1539,  1056,  1540, -1853,
    1223,  1541,  1551, -1853,   728, -1853,  1552, -1853,   728,  1055,
    1551, -1853,   728,  1525,  1544,  1545, -1853, -1853, 20615, -1853,
    4001,  5170, 11232,  1640, -1853,  1252, -1853, 15912,  1063, -1853,
    1551,  1554, -1853, 22186, 18671,  1568, -1853,  1568, -1853, -1853,
   -1853, -1853, 21598, -1853, 12463, 18993, -1853,  1555,  1556,  1562,
    1576, -1853, 10024,   135, -1853,  1194, -1853, -1853, -1853, -1853,
    1406, -1853, -1853, -1853,   875, -1853, -1853, -1853, -1853,   109,
    1031,  1585,   337, -1853, -1853,  1588,  5170,   109, -1853, -1853,
    1587,  1594, -1853, -1853,  1074, -1853, -1853, -1853, -1853,  1595,
    1598,  1596,  1599,  1600,  1601,  1602,  1604,  1606,  1605,  1610,
   25755,  1619,  1621,  1622, 22343, 12633, 25755, -1853, -1853,  1711,
   -1853, -1853, -1853, 25755, -1853,  1623,  1626, 23748, -1853, -1853,
    1259, -1853, 23902,  1624, -1853,  1628, -1853, -1853,  4975, -1853,
    1077, -1853,  4975, -1853, -1853,  1265,   521, -1853, -1853,  1102,
    1102,  1102,   739,   739,   946,   946,   959,   959,   959,   959,
     549,   549,  1003,  1110,  1115,  1108,  1161, 25755,  1282,  1634,
    4975, -1853, -1853, 23671, -1853,  1635,  1636,  1638,  1639,  2136,
   -1853, -1853, -1853, -1853, -1853, 20394, -1853, -1853,  1406, 20394,
   -1853,  1406,  1643,  1644, 15912, 15912, -1853, -1853, 15082,  1037,
    1645,  1646,  1648,  1649,  3622,  3704, -1853, -1853, 20394, -1853,
   -1853, -1853, -1853, -1853, -1853, 20394, -1853, -1853, -1853, -1853,
    1637, -1853,  1469,  1650, -1853, -1853, -1853, -1853, -1853, -1853,
   -1853, -1853,  1653,  1654,  1655, -1853,  1656, -1853,   440,  4975,
    1296,   306, -1853, -1853,  1659, -1853, 23825, -1853, 25755,   135,
   15912,   135, -1853, -1853,   664,  1155, -1853,   686,  1155, -1853,
   -1853,   725,  1155, 20394, -1853, -1853,  1406, 20394, -1853, -1853,
    1406, 20394, -1853, -1853,  1406,  1001, -1853,  1406,   386, -1853,
    1138,  1660, -1853, -1853, -1853, -1853, -1853, -1853,  1666, -1853,
   -1853, -1853, 22186,  1551, -1853,   728, -1853, -1853, -1853, -1853,
   -1853, 14214, -1853, -1853, -1853, -1853, -1853,   -42,   363,   293,
   12123,  1668,  1669, 17298,  1670,  1673,  1627,  2911,  3783, 24210,
    1675, -1853, -1853,  1676,  1677, 17298,  1678, -1853, -1853,   728,
   25755, 25755,  1802,  1674,   538, -1853, 17866,  1309,  1680,  1679,
    1657, -1853, -1853, -1853, 11033, -1853, -1853, -1853, -1853, -1853,
    2827, -1853, -1853, -1853,  1382,   410, -1853,   321, -1853,   410,
   -1853, -1853, -1853, -1853, -1853,  4001, -1853, -1853, 13720, 19757,
   -1853,  5170,  1683,  1684, -1853, -1853, -1853,  5170, -1853, -1853,
    5863, -1853, -1853,  1667,  1671,  1079, 21387,   734,   734, -1853,
   -1853,  1097,  1252, 18832, -1853,  1138, -1853, 12803, -1853,   764,
    1155, -1853,   875, 13382, -1853, -1853,  1031,  1588,  1672,   109,
     668,   443,  1692,  1685,  1588,  1694, -1853, -1853, 23902,   582,
   -1853, 20754,   582, 12633,  4001, -1853,   582, -1853, 21176,   582,
   -1853, 25755, 25755, 25755, -1853, -1853, -1853, -1853, 25755, 25755,
    1689, 23671, -1853, -1853, 24287,  1693,   613, -1853, -1853, -1853,
    2339, -1853, -1853,  1314, -1853,    31, -1853,  1322, -1853, 23979,
   -1853, -1853, 25755, -1853,  1330,  1333,  1306, -1853,   769,  1155,
   -1853, -1853,  1698,  1700, -1853, -1853, -1853, -1853,  1702,   901,
    1155, -1853,   909,  2471,   135,   135, -1853, -1853,  1705,  1706,
   -1853,  1709, -1853, 16742,  1712, -1853, 16244, 16410,  1714, 16908,
    1716, -1853,  1715, 25755, 25755,  1336,  1717, -1853, -1853, -1853,
   -1853, -1853, -1853,  1718, 20394, -1853, -1853,  1406, 20394, -1853,
   -1853,  1406, 20394, -1853, -1853,  1406,  1721,  1722,  1723,   440,
   -1853, -1853,  1338, 25755, 23118,  1730,  1727, -1853, -1853, -1853,
    1741, 14374, 14534, 14694, 22186,  9308, 22132, 22132,  1744,  1686,
      37,   451,  2697, 14916, -1853,   494,  5170,  5170, -1853, 23902,
     103,   379, -1853, -1853, -1853, -1853, 12123, 25755,  1746,  1817,
   11952, 11412, -1853,  1725, -1853,  1726, 25755,  1729, 23671,  1731,
   25755, 18671, 25755, -1853, 11592,  1400, -1853,  1732,    32, -1853,
      60,  1819,   336,   135, -1853,  1761, -1853,  1736, -1853,  1737,
    1763,  1764, 17298, 17298, -1853, -1853,  1832, -1853, -1853,    88,
      88,   474, 15414,   531, -1853, -1853,  1765,  1770,   388, -1853,
    1771, -1853,  1766, -1853,  1773, -1853, -1853, -1853, -1853, 12973,
    1772,  1775,  1779, -1853, 20394, -1853, -1853,  1406, 25755, 25755,
    1252,  1780, -1853,  1778,  1788,   109,  1588,   337,  5170, -1853,
   24364, -1853,  1791, -1853, 22186, -1853,   983,  1789,  1786,  1085,
   -1853,  1798, -1853, -1853, -1853, -1853, -1853, 23671,  1306, -1853,
   -1853, 23979, -1853,  1831,  4975, -1853,  1831,  1831, -1853,  4975,
    3490,  4367, -1853,  1342, -1853, -1853,  1807, 20394, -1853, -1853,
    1406, -1853, -1853,  1805,  1806,   135, 20394, -1853, -1853,  1406,
   20394, -1853, -1853,  1809, -1853, -1853, -1853, -1853, -1853, -1853,
   -1853, -1853,  1650, -1853, -1853, -1853,  1808, -1853, -1853, -1853,
   -1853,  1810,  1812,   135,  1814,  1816,  1818, -1853, -1853, -1853,
   -1853, 25755, -1853,   386, -1853,  1138, -1853, -1853,  1813,  1822,
   -1853,  1744,  1744,  1744,  5244,  1023,  1797,   543, -1853,  5244,
     583, 18671, -1853, -1853, -1853, -1853,  4223, 25755,  4743,   365,
   -1853, -1853,    86,  1821,  1821,  1821,  5170, -1853, -1853, -1853,
    1107, -1853, -1853, -1853, -1853,  1679,  1824, 25755,   556,  1826,
     574, 14861, 22186,  1112,  1823, 17298,  1828, -1853, -1853, -1853,
   -1853,   892, 17298, 25755,  1152,   720, -1853, 25755, 10837, -1853,
   -1853,   585, -1853,  1306, -1853,  1136,  1147,  1148,   760, -1853,
   -1853, -1853, -1853,   728,  1400,  1833, -1853, -1853, 25755, -1853,
    1834,   832, -1853, 10755, -1853, -1853, -1853, 25755, 25755, -1853,
   -1853,    74,    88, -1853,   217, -1853, -1853, -1853,   135, -1853,
    1568, -1853, 22186, -1853, -1853, -1853, -1853, -1853,  1830,  1836,
   -1853, -1853,  1837, -1853,  1841,   109, -1853,  1588,  1825,   668,
    1685, 23671, -1853, -1853, -1853,  1839, -1853, -1853, 25755, -1853,
   21176, 25755,  1306,  1840,  1363, -1853,  1365, -1853,  4975, -1853,
    4975, -1853, -1853, -1853,  1850,   135,   135,  1851,  1856, -1853,
    1855, -1853, -1853, -1853, -1853, -1853,  1370, 25755, -1853, -1853,
   -1853, -1853, -1853,   597,  1023,  2208,   599, -1853, -1853, -1853,
   -1853,   135,   135, -1853, -1853, -1853,   615, -1853,  1169,  4223,
     550, -1853,  4743, -1853,   135, -1853, -1853, -1853, -1853, -1853,
   -1853, 17298, 17298,  1679, 17074,   137, 24441,  1917, 17298, -1853,
   -1853, -1853, -1853, -1853, 25755, -1853, 24518,  1942,  1843, 19070,
   24595, 17298, 11592,  1679,  1284,  1247,  1844, 25755, -1853,  1863,
     326, 17298, -1853, 17298, -1853,  1865, -1853, -1853,  1846,   832,
     772, -1853, -1853,  1871,  1371,  1176, 17298,  1874, 17298, 17298,
   17298, -1853,   734, -1853,  5170,  5863, -1853, -1853,  1870,  1878,
   -1853, -1853,  1588,  1880, -1853, -1853,  1306,  1888, -1853, -1853,
   -1853, -1853,  1889, -1853, -1853, -1853,  1392,  1396, -1853, -1853,
   -1853, -1853, -1853, -1853, -1853, -1853, -1853,  1887,  1890,  1894,
    2208, -1853,   135, -1853, -1853, -1853, -1853, -1853,  1886,  5244,
   -1853,  1980,  6467,   110, 13146, -1853, 17172, -1853,    15,  1177,
   17298,  1981,   621,  1885,   462, 17298, 25755,  1284,  1247,  1877,
   24677,   859,   875,  1893,   560,  1986, -1853, 24754, 24831, 25755,
    1679,  1881, 13315, -1853, -1853, -1853, -1853, 22397, -1853,  1905,
    1891,   322, -1853, 25755, 23902, -1853, -1853, 25755,   410, -1853,
   -1853, -1853, -1853, -1853, -1853, -1853,  1914, -1853,  1915, -1853,
   -1853, -1853, -1853,   958,  1155, -1853, -1853,  1023, -1853, 17298,
   -1853,   343, -1853,   133, -1853, -1853, -1853,  1918, 13887, -1853,
   -1853, 17298, -1853,    48, -1853, 17298, 25755,  1919, 24908, -1853,
   -1853, 24985, 25062, 25755,  4722,  1679, -1853,  1138, 25139, 25216,
   17298,  1903,   619,  1907,   645, -1853, -1853,  1922, 13887, 22397,
   -1853,  5648, 21975,  4001,  1920, -1853,  1975,  1926,   785,  1924,
   -1853, -1853,  1182,  1184,   473, -1853, -1853, 20394, -1853, -1853,
    1406, -1853, -1853, 25755, -1853, 25755, -1853, -1853,  1491, 14054,
   -1853, -1853, 17298, -1853, -1853,  1679, -1853, -1853,  1679,  1912,
     651,  1916,   667, -1853, -1853,   668, -1853,  1679, -1853,  1679,
   -1853,  1932, 25293, 25370, 25447, -1853,  1491, -1853,  1911,  3100,
    4048, -1853, -1853, -1853,   322,  1934, 25755,  1925,   322,   322,
   -1853, -1853, 17298,  2021,  1943, -1853, -1853, 17172, -1853,  1491,
   -1853, -1853,  1946, 25524, 25601, 25678, -1853, -1853,  1679, -1853,
    1679, -1853,  1679, -1853,  1911, 25755,  1952,  4048,  1937,   832,
    1953, -1853,   806, -1853, -1853, 17298, -1853, -1853, 10229,  1957,
   17172, -1853, -1853,  1679, -1853,  1679, -1853,  1679,  1958,  1956,
   -1853,   728,   832,  1959, -1853,  1936,   832, -1853, -1853, -1853,
   -1853, 10358, -1853,   728, -1853, -1853,  1408, 25755, -1853,  1195,
   -1853,   832,  4001,  1960,  1938, -1853, -1853,  1197, -1853, -1853,
    1941,  4001, -1853, -1853
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   498,     0,     2,   498,   515,   516,   517,   518,   519,
     520,   521,   522,   523,   504,   506,   505,   507,     0,     0,
       0,   525,   527,   548,   528,   549,   531,   532,   546,   547,
     526,   544,   545,   529,   530,   533,   534,   535,   536,   537,
     538,   539,   540,   541,   542,   543,   550,   551,   855,   553,
     626,   627,   630,   632,   628,   634,     0,     0,     0,   498,
       0,     0,    17,   597,   603,     9,    10,    11,    12,    13,
      14,    15,    16,   811,   110,     0,     0,    20,     0,     2,
     108,   109,   832,    18,    19,   870,   498,   812,     0,     0,
     437,   734,   439,   450,   853,   438,   472,   473,     0,     0,
       0,     0,   580,   500,   502,   508,   498,   510,   513,   565,
     524,   552,   482,   558,   563,   484,   575,   483,   590,   594,
     600,   579,   606,   618,   855,   623,   624,   607,   676,   440,
     441,     3,   819,   833,   503,     0,     0,   855,   892,   855,
     498,   909,   910,   911,   498,     0,  1096,  1097,     0,     1,
     498,    17,     0,   498,   461,   462,     0,   580,   508,   492,
     493,   494,   822,     0,   629,   631,   633,   635,     0,   498,
     855,   679,   856,   857,   625,   554,    22,    23,    21,   788,
     783,   773,     0,   864,   820,     0,     0,   515,   813,   817,
     818,   814,   815,   816,   498,   864,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   598,   601,   498,   498,     2,
       0,  1098,   580,   899,   917,  1102,  1095,  1093,  1100,   434,
     433,     0,   172,   740,   171,     0,   442,     0,     0,     0,
       0,     0,   448,     0,     0,     0,   432,   986,   987,     0,
       0,   471,   853,   855,   853,   873,   855,   855,   481,   498,
     855,   853,   930,   855,   855,   480,   855,   949,   855,   927,
       0,   573,   574,     0,     0,   498,   498,     2,   498,   451,
     853,   501,   511,   566,     0,   595,     0,   836,   498,     0,
       0,   734,   452,   580,   559,   576,   591,     0,   836,   498,
       0,   514,   560,   567,   568,   485,   577,   487,   488,   486,
     582,   592,   596,     0,   610,     0,   805,   498,     2,   834,
     891,   893,   498,     0,   498,     0,     0,   580,   498,   510,
       2,  1106,   580,  1109,   853,   853,     3,     0,   580,     0,
       0,   464,   855,   848,   850,   849,   851,     2,   498,   853,
       0,   809,     0,     0,   769,   771,   770,   772,     0,     0,
     765,     0,   754,     0,   763,   775,     0,   855,   677,     2,
     498,  1118,   499,   498,   489,   558,   490,   583,   491,   590,
     587,   608,   855,   609,   722,     0,   498,   723,  1071,  1072,
     498,   724,   726,   679,   597,   603,   680,   681,   682,     0,
     679,   858,     0,   786,   774,     0,   869,   868,   864,   867,
       0,   862,   865,    25,     0,    24,     0,     0,     0,     0,
       0,     0,     0,     0,    27,    29,     4,     8,     5,     6,
       7,     0,     0,   498,     2,     0,   111,   112,   113,   114,
      91,    28,    92,    46,    90,   115,     0,     0,   130,   132,
     136,   139,   142,   147,   150,   152,   154,   156,   158,   160,
     163,     0,    30,     0,   604,     2,   115,   498,   164,   780,
     730,   594,   732,   779,     0,   729,   733,     0,     0,     0,
       0,     0,     0,     0,     0,   871,   897,   855,   907,   915,
     919,   925,   597,     2,     0,  1104,   498,  1107,     2,   108,
     498,     3,   721,     0,  1118,     0,   499,   558,   583,   590,
       3,     3,   717,   707,   711,   723,   724,   498,     2,     2,
     900,   918,  1094,     2,     2,    27,     0,     2,   740,    28,
       0,   738,   741,  1116,     0,     0,   747,   736,   735,   498,
       0,   838,     0,     2,   463,   465,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     876,   933,   956,   855,   477,     2,   872,   880,  1014,   734,
     874,   875,     0,   836,   498,   929,   937,   734,   931,   932,
       0,   948,   950,     0,   467,   498,   498,   564,   499,     0,
     580,     0,   498,  1099,  1103,  1101,   449,   581,   809,     0,
     836,   853,     0,   443,   453,   512,     0,   836,   498,   809,
       0,   836,   784,   561,   562,   578,   593,   599,   602,   597,
     603,   621,   622,     0,   785,   693,   727,   499,     0,   694,
     696,   697,     0,   212,   426,   835,     0,   424,   481,   480,
     580,     0,   445,     2,   446,   806,   469,     0,     0,     0,
     498,   853,   498,   809,     0,     0,     2,     0,   768,   767,
     766,   760,   509,     0,   758,   776,   556,     0,   498,   498,
    1073,   499,   495,   496,   497,  1077,  1068,  1069,  1075,   498,
       2,   109,     0,  1033,  1047,  1118,  1029,   855,   855,  1038,
    1045,   715,   498,   588,   725,   499,   584,   585,   589,     0,
     678,  1083,   499,  1088,  1080,   498,  1085,   855,     0,   855,
     679,   679,     0,     0,   498,     0,     0,   860,   864,    70,
       0,    26,   498,    98,     0,   498,   106,   498,   498,    93,
     498,   100,     0,    36,    40,    41,    37,    38,    39,   498,
      96,    97,   498,   498,   498,     2,   111,   112,     0,     0,
     190,     0,     0,   624,     0,  1093,     0,     0,     0,     0,
       0,     0,     0,     0,    59,     0,    65,    66,    70,     0,
       0,    70,     0,    94,    95,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   436,     0,     0,   498,   173,
     174,   175,   176,   177,   178,   179,   180,   181,   182,   183,
     171,     0,   169,   170,   498,   998,   731,   995,   855,   855,
    1003,   605,   498,   861,   898,   855,   908,   916,   920,   926,
     498,   901,   903,   905,   498,   921,   923,     2,     0,     2,
    1105,  1108,   498,   498,     0,     2,     0,   498,   109,  1033,
     855,  1118,   968,   855,   855,  1118,   855,   983,   855,   855,
       3,   725,   498,     0,   498,     0,  1118,  1118,   498,   498,
     498,   498,     0,     2,   749,     0,  1116,   746,  1117,     0,
     742,     0,     2,   745,   748,     0,     2,     0,   498,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   855,   885,   889,   928,   855,   942,   946,   954,   855,
     964,   877,   934,   957,     0,     0,  1010,     0,   475,   839,
       0,     0,   476,   840,   468,     0,     0,     0,     0,   466,
       0,     2,     2,   841,     0,   447,     2,   809,     0,   836,
       2,   842,     0,     0,     0,     0,   636,   894,   498,   912,
       0,     0,   498,   427,   425,   108,     3,   498,     0,   810,
       2,     0,   762,   498,   498,   756,   755,   756,   557,   555,
     681,  1079,   498,  1084,   499,   498,  1070,     0,     0,     0,
       0,  1048,     0,   855,  1119,  1034,  1035,   716,  1031,  1032,
    1046,  1074,  1078,  1076,   586,   621,  1082,  1087,   673,   679,
     679,     0,     0,   688,   687,  1116,     0,   679,   789,   787,
       0,     0,   863,    74,     0,    71,    72,    75,   821,     0,
       0,     0,     0,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   498,   498,     0,   129,   128,     0,
     125,   124,    31,     0,    32,     0,     0,     0,   187,   186,
       0,     3,    70,     0,    55,     0,    56,    63,     0,    62,
       0,    58,     0,    57,    61,     0,     0,    54,   131,   133,
     134,   135,   137,   138,   140,   141,   145,   146,   143,   144,
     148,   149,   151,   153,   155,   157,   159,     0,     0,     0,
       0,    33,     3,   740,   165,     0,     0,     0,     0,   999,
    1000,   996,   997,   782,   781,   498,   902,   904,   906,   498,
     922,   924,     0,     0,   498,   498,  1024,  1023,   498,     0,
       0,     0,     0,     0,   855,  1034,   971,   988,   498,   966,
     974,   713,   969,   970,   714,   498,   981,   991,   984,   985,
       0,     3,  1118,     3,   709,   459,   708,   712,  1110,   718,
     719,   701,     0,   702,   703,     3,     3,     3,   734,     0,
     163,     0,     3,     3,     0,   743,     0,   737,     0,   855,
     498,   855,     3,   470,   855,   886,   890,   855,   943,   947,
     955,   855,   965,   498,   878,   881,   883,   498,   935,   938,
     940,   498,   958,   960,   962,   853,   478,  1011,     3,  1015,
    1016,     3,   844,   951,   570,   569,   572,   571,     2,   810,
     845,   791,   498,     2,   843,     0,   810,   846,   636,   636,
     636,   498,   695,   698,   699,   728,   430,     0,     0,     0,
     498,     0,     0,   351,     0,     0,     0,     0,     0,   192,
       0,   346,   347,     0,     0,   351,     0,   399,   398,     0,
     167,   167,   405,   597,   603,   209,   498,     2,     0,   193,
       0,   220,   194,   195,   498,   214,   196,   197,   198,   199,
       0,   200,   201,   352,     0,   366,   202,   372,   374,   377,
     203,   204,   205,   206,   207,     0,   208,   216,   580,   498,
     218,     0,     0,     0,     3,   823,   810,     0,   798,   799,
       0,     3,   794,     3,     3,     0,   498,   773,   773,  1081,
    1086,     2,   108,   498,     3,   595,     3,   499,  1042,   855,
    1041,  1044,   498,     3,  1030,  1036,   679,  1116,     0,   679,
     684,   679,     0,   689,  1116,     2,   859,   866,     0,    99,
     102,   498,   107,   498,     0,   105,   101,   103,   498,     0,
     119,     0,     0,     0,   123,   127,   126,   191,     0,     0,
       0,   740,   116,   184,     0,     0,     0,    49,    50,    88,
       0,    88,    88,     0,    76,    78,    52,     0,    48,     0,
      51,   162,     0,   435,     0,     0,  1116,  1007,   855,  1006,
    1009,  1001,     0,     0,   895,   913,     3,     3,     0,   855,
     977,   980,   855,     0,   855,   855,   972,   989,     0,     0,
    1111,     0,   720,   498,     0,  1113,   498,   498,     0,   498,
       0,   444,     3,     0,     0,     0,     0,   739,   744,     3,
     837,     3,   854,     0,   498,   879,   882,   884,   498,   936,
     939,   941,   498,   959,   961,   963,     0,     0,     0,   734,
    1022,  1021,     0,     0,     0,     0,     0,     3,   810,   847,
       0,   498,   498,   498,   498,   498,   498,   498,   619,     0,
       0,     0,   650,   580,   637,     0,     0,     0,   428,    70,
       0,     0,   337,   338,   217,   219,   498,     0,     0,     0,
     498,   498,   333,     0,   331,     0,     0,     0,   740,     0,
       0,   498,     0,   378,   498,     0,   168,     0,     0,   406,
       0,     0,     0,   855,   224,     0,   215,     0,   328,     0,
       0,     0,   351,   351,   357,   356,   351,   368,   367,   351,
     351,     0,   580,     0,  1026,  1025,     0,     0,   765,   801,
       2,   796,     0,   797,     0,   777,   757,   761,   759,   498,
       0,     0,     0,     3,   498,  1037,  1039,  1040,     0,     0,
     108,     0,     3,     0,     0,   679,  1116,     0,     0,   668,
       0,   683,     0,   790,   498,    73,  1027,     0,     0,     0,
      42,     0,   120,   122,   121,   118,   117,   740,  1116,   189,
     188,     0,    69,    85,     0,    79,    86,    87,    64,     0,
       0,     0,    60,     0,   161,    34,     0,   498,  1002,  1004,
    1005,   896,   914,     0,     0,   855,   498,   973,   975,   976,
     498,   990,   992,     0,   967,   982,   978,   993,  1112,   710,
     460,   705,   704,   706,  1115,  1114,     0,     3,   852,   750,
     751,     0,     0,   855,     0,     0,     0,   887,   944,   952,
     479,     0,  1017,     0,  1018,  1019,  1013,   827,     2,     0,
     829,   619,   619,   619,   650,   657,   624,     0,   663,   650,
       0,   498,   611,   649,   648,   644,     0,     0,     0,     0,
     651,   653,   855,   665,   665,   665,     0,   645,   661,   431,
       0,   341,   342,   339,   340,   233,     0,     0,   235,   439,
     234,   580,   498,     0,     0,   351,     0,   316,   318,   317,
     319,     0,   351,   192,   273,     0,   266,     0,   192,   334,
     332,     0,   326,  1116,   335,     0,     0,     0,     0,   387,
     388,   389,   390,     0,   380,     0,   381,   343,     0,   344,
       0,     0,   371,     0,   213,   330,   329,     0,     0,   360,
     370,     0,   351,   373,     0,   375,   397,   429,   855,   825,
     756,   778,   498,     2,     2,  1089,  1090,  1091,     0,     0,
       3,     3,     0,  1050,     0,   679,   669,  1116,     0,   686,
     689,   740,   690,   672,     3,     0,  1028,   104,     0,    35,
     498,     0,  1116,     0,     0,    89,     0,    77,     0,    83,
       0,    81,    47,   166,     0,   855,   855,     0,     0,   753,
       0,   454,   458,   888,   945,   953,     0,     0,   793,   831,
     615,   617,   613,     0,     0,  1057,     0,   658,  1062,   660,
    1054,   855,   855,   643,   664,   647,     0,   646,     0,     0,
       0,   667,     0,   639,   855,   638,   654,   666,   655,   656,
     662,   351,   351,   236,   580,     0,     0,   254,   351,   321,
     324,   322,   325,   320,     0,   323,     0,   262,     0,   192,
       0,   351,   498,   274,     0,   299,     0,     0,   327,     0,
       0,   351,   350,   351,   391,     0,   382,     2,     0,     0,
       0,   211,   210,   353,     0,     0,   351,     0,   351,   351,
     351,   457,   773,   795,     0,     0,  1092,  1043,     0,     0,
    1049,  1051,  1116,     0,   671,   685,  1116,     2,    53,    45,
      43,    44,     0,    67,   185,    80,     0,     0,  1008,   456,
     455,   979,   994,   752,  1012,  1020,   641,     0,     0,     0,
    1058,  1059,   855,   642,  1055,  1056,   640,   620,     0,     0,
     349,   225,     0,     0,     0,   247,   351,   227,     0,     0,
     351,   256,   271,   282,   276,   351,   192,     0,   286,     0,
       0,     0,   311,   277,   275,   264,   267,     0,     0,   192,
     300,     0,     0,   230,   348,   379,     2,   498,   345,     0,
       0,   407,   358,     0,    70,   369,   362,     0,   363,   361,
     376,   764,   800,   802,  1052,  1053,     0,   675,     0,   792,
      68,    84,    82,   855,  1065,  1067,  1060,     0,   652,   351,
     242,   237,   240,     0,   239,   246,   245,     0,   498,   249,
     248,   351,   258,     0,   255,   351,     0,     0,     0,   263,
     268,     0,     0,   192,     0,   287,   312,   313,     0,     0,
     351,     0,   302,   303,   301,   270,   336,     0,   498,   498,
       3,   392,   499,   396,     0,   400,     0,     0,     0,   408,
     409,   354,     0,     0,     0,   674,   691,   498,  1061,  1063,
    1064,   659,   226,     0,   244,     0,   243,   229,   250,   498,
     420,   259,   351,   260,   257,   272,   285,   283,   279,   291,
     289,   290,   288,   269,   314,   315,   284,   280,   281,   278,
     265,     0,     0,     0,     0,   232,   250,     3,   385,     0,
    1057,   393,   394,   395,   407,     0,     0,     0,   407,     0,
     359,   355,   351,     0,     0,   238,   241,   351,     3,   251,
     421,   261,     0,     0,     0,     0,   310,   308,   305,   309,
     306,   307,   304,     3,   385,     0,     0,  1058,     0,     0,
       0,   401,     0,   410,   364,   351,  1066,   221,     0,     0,
     351,   298,   296,   293,   297,   294,   295,   292,     0,     0,
     386,     0,   413,     0,   411,     0,   413,   365,   223,   222,
     228,     0,   231,     0,   383,   414,     0,     0,   402,     0,
     384,     0,     0,     0,     0,   415,   416,     0,   412,   403,
       0,     0,   404,   417
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1853,  4657,  2281, -1853,    -1,   852,  1496,  9359,  -112, -1853,
    -392, -1853,   344, -1853,  -733, -1853,   797, -1001, -1303, -1853,
     233,  5932,  1898, -1853,  1181, -1853,  1390,   628,   816,   828,
     679,   834,  1350,  1353,  1354,  1359,  1360, -1853,  -122,   -87,
    9247,   903, -1853,  1699, -1853, -1853, -1254,  6449, -1184,  4914,
   -1853,  2003, -1853,   891,   -12, -1853, -1853,   680,    81, -1853,
   -1852, -1822,   291,    56, -1853, -1853,   669,   302, -1853, -1595,
   -1853, -1521, -1853, -1853, -1853, -1853,   101, -1118, -1853, -1853,
   -1246,   426, -1853, -1853, -1853, -1853, -1853,    96, -1197, -1853,
   -1853, -1853, -1853, -1853,    24,   447,   448,   121, -1853, -1853,
   -1853, -1853,  -742, -1853,    54,     0, -1853,   131, -1853,  -171,
   -1853, -1853, -1853,   894,  -488,  -899,   -65, -1853,    14,     3,
     224,  4463,  -803,  -790, -1853,  -111, -1853, -1853,    36, -1853,
    -162,   320,    39,  -259,  2823,  3388,  -713,    43,   236,   330,
     593,  3692, -1853, -1853,  2134, -1853,   382,  4788, -1853,  2069,
   -1853,   151, -1853, -1853,   240,   415,  5571,  4093,   -30,  1913,
    -297, -1853, -1853, -1853, -1853, -1853,  -249,  8572,  7982, -1853,
    -401,   231, -1853,  -688,   246, -1853,   181,   733, -1853,   -41,
    -225, -1853, -1853, -1853, -1853,  -130,  8619,  -926,   872,   424,
    -231, -1853,  -208,  -165,   -69,  2216,  2152,  -822,  -164,   920,
     409,  -541,  -267,  -179,  -471,  1328, -1853,  1688,   148,  -948,
    1549, -1853, -1853,   670, -1853, -1238,  -170,   136,  -512, -1853,
     261, -1853, -1853, -1134,   452, -1853, -1853, -1853,  2197,  -758,
    -445, -1125,   -46, -1853, -1853, -1853, -1853, -1853, -1853,   254,
    -858,  -222, -1834,   127,  7219,   -68,  5920,   -99,  1503, -1853,
     844,   -72,  -223,  -219,  -203,    30,   -75,   -74,   -66,   505,
       4,    93,   128,  -199,   -11,  -187,  -178,  -169,   271,  -153,
    -127,   -98,  -725,  -768,  -755,  -754,  -712,  -117,  -745, -1853,
   -1853,  -752,  1401,  1404,  1411,  3551, -1853,   573,  6682, -1853,
    -658,  -630,  -619,  -602,  -774, -1853, -1550, -1737, -1724, -1717,
    -659,  -148,  -192, -1853, -1853,   -44,    99,  -120, -1853,  7465,
    2296,  -749,  -142
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   855,   430,   431,   178,    85,  1248,   432,   405,
     433,  1569,  1570,   434,  1004,  1005,  1006,  1363,  1364,  1365,
    1583,   456,   436,   437,   438,   738,   739,   439,   440,   441,
     442,   443,   444,   445,   446,   447,   448,   449,   458,  1151,
     740,  1497,   801,   225,   803,   452,  1040,  1249,  1250,  1251,
    1252,  1253,  1254,  1255,  2158,  1256,  1257,  1686,  2012,  2013,
    1945,  1946,  1947,  2128,  2129,  1258,  1705,  1706,  2036,  1707,
    1855,  1856,  1259,  1260,  1261,  1262,  1263,  1264,  1884,  1888,
    1520,  1512,  1265,  1266,  1519,  1513,  1267,  1268,  1269,  1270,
    1271,  1272,  1273,  1724,  2146,  1725,  1726,  2050,  1274,  1275,
    1276,  1500,  2058,  2059,  2060,  2186,  2197,  2078,  2079,   313,
     314,   942,   943,  1217,    87,    88,    89,    90,    91,  1689,
     492,   210,    95,    96,    97,    98,   241,   242,   316,   295,
     494,   460,   495,   101,   328,   103,   104,   158,   363,   319,
     108,   109,   110,   174,   111,   959,   364,   159,   114,   265,
     115,   160,   274,   366,   367,   368,   161,   453,   120,   121,
     370,   122,   613,   935,   933,   934,  1662,   123,   124,   125,
     126,  1211,  1464,  1669,  1670,  1816,  1817,  1465,  1657,  1836,
    1671,   127,   700,  1316,   170,   994,   128,   995,   996,  1561,
     967,   619,  1142,  1143,  1144,   620,   374,   503,   504,   622,
     462,   463,   226,   522,   523,   524,   525,   526,   351,  1297,
     352,   957,   955,   651,   353,   393,   354,   355,   464,   129,
     180,   181,   130,  1291,  1292,  1293,  1294,     2,  1198,  1199,
     642,  1285,   131,   341,   342,   276,   287,   596,   132,   229,
     133,   331,  1153,   586,   556,   172,   134,   400,   401,   402,
     135,   333,   245,   246,   247,   334,   137,   138,   139,   140,
     141,   142,   143,   250,   335,   252,   253,   254,   336,   256,
     257,   258,   841,   842,   843,   844,   845,   259,   847,   848,
     849,   806,   807,   808,   809,   557,  1191,  1443,   144,  1775,
     675,   676,   677,   678,   679,   680,  1819,  1820,  1821,  1822,
     665,   505,   378,   379,   380,   465,   216,   146,   147,   148,
     382,   869,   681
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      83,   199,   200,    83,    92,   375,   577,   361,   197,  1298,
     201,   394,   535,   184,   593,   976,   539,   875,   154,  1516,
     540,   377,   743,   982,   323,  1050,  1031,   244,  1056,   315,
    1134,   136,  1136,   749,   916,   918,   541,    99,   574,   968,
     542,   389,   215,   500,   105,  1489,   528,   866,   206,  1502,
     969,  1367,   543,  1090,   390,    83,    83,  1585,    83,  1537,
    1538,   544,    92,  1317,   450,   683,  1120,   970,  1447,  1110,
     545,  1324,  1127,   404,  1446,    83,   618,  1927,   563,  1374,
     202,  1450,  1111,  1112,  1280,    83,   546,   487,   251,   136,
    1928,  2021,  1113,    83,  2020,    99,   467,  1929,    83,   451,
     145,    83,   105,   145,   326,    83,   539,   590,  2014,   215,
     540,  2015,   547,  1867,  1116,  1593,   213,  1154,   601,   992,
     281,  1467,   469,   470,  2082,    58,   541,  1117,  1501,   248,
     542,   471,   277,  1728,  2074,   230,   288,   162,   315,   501,
    1468,   548,   543,   922,    83,  1730,  -803,    83,  1412,    83,
    1886,   544,   116,    92,   930,    83,    58,  1590,   145,    58,
     545,  1527,    83,   999,   199,   200,  1510,   315,   550,   203,
      83,   537,  1206,   201,   149,   227,   546,  1015,   315,  1234,
     136,   976,    83,  1860,   693,  1887,    99,   227,   696,    75,
     633,  1591,  1286,   105,    83,    83,    83,  2022,   950,   683,
     233,   472,   547,   626,   204,   145,    83,   968,    58,   307,
     116,    83,   666,   555,  1729,  1942,  1943,  1731,   969,  1664,
      75,   232,   519,    75,   584,    93,   213,    83,   155,   551,
    2083,   548,    83,    83,    83,   970,   783,   106,    83,    83,
     169,  2014,   510,   202,   244,  1742,  1322,   555,   228,   145,
    -836,   205,    64,   698,   199,   200,  2127,   285,   550,    83,
     702,   637,  -836,   201,  1960,  1931,   699,    83,   308,    83,
     213,  1094,    75,   608,  2016,  2020,   993,    58,    83,    83,
     784,   386,    83,    93,  2127,  1681,  1787,  1789,  1791,    83,
     473,   116,   908,  1890,   584,   106,  2075,  2076,   213,   706,
     912,   116,  1944,    83,    83,   251,    83,  2160,  2020,  1356,
    1511,    83,  1459,   597,   881,    83,  1346,  1315,   882,   551,
     683,   100,  1011,  1649,   156,   474,   269,  1784,    83,    83,
     282,   107,   203,   911,   883,  1120,   163,  1381,   884,    83,
    1110,    75,   666,  1967,  1968,   870,   628,    83,    83,   633,
     885,   914,    83,  1111,  1112,  1470,  1471,   919,  1054,   886,
     213,   564,  1475,  1388,   683,   555,   831,   204,   887,   554,
     255,   559,  1927,   672,    93,  -804,   318,   846,   567,   100,
    2006,    20,    83,   112,   888,  1928,   106,   487,   683,   107,
    1396,    83,  1929,  1556,    83,   683,  1280,    83,  1741,   660,
     116,  1501,  1744,  1397,  1942,  1943,   211,   652,  1460,   618,
     889,   653,    58,   215,   881,   691,   117,   500,   882,   694,
     877,  1461,   164,   194,    58,   165,   166,   929,   167,   116,
    1774,  -616,   272,  1517,   883,   168,  2031,  2032,   884,   890,
     116,   112,  1295,   403,  1413,  1448,    63,    64,   498,  1514,
     885,   632,   634,  1557,  1277,   183,  1518,  1315,   308,   886,
     100,   185,   469,   470,   321,   116,   963,    83,   887,   285,
     100,   471,  1515,   211,   117,  1472,    75,   901,   968,  1414,
     107,  2073,  1203,  2056,   888,   318,   206,   315,    75,   969,
     655,  1972,    83,    83,   500,  1732,   640,    78,   510,   186,
     555,  2112,   562,   987,    83,    83,   970,   594,   820,   570,
     889,   552,   555,   501,   318,    83,  1414,   519,    58,  1858,
    1469,  1833,   112,  1514,  1866,   318,   648,   485,  1834,   660,
     655,   589,   112,   977,   666,  1546,    83,  1440,   902,   890,
     703,   472,   600,   705,   308,  1502,  1515,  1835,    83,  2122,
     318,   602,  1459,  1459,  1459,   117,   649,   650,    58,  1441,
    1931,  1683,   469,   470,  1234,   117,   614,   901,  1554,   100,
     991,   471,    83,  1085,   307,  1562,   475,   194,    83,   510,
      83,  1619,    75,  1786,  1134,  1136,   579,   629,   583,  1102,
     501,   214,   195,  1103,   222,    58,   949,  2006,   100, -1117,
     663,   552,  1095,   686,   249,   223,   555,   278,   207,   100,
     993,   289,  1376,   595,   234,   386,   663,   280,  1612,  -986,
     663,   224,    75,   500,  1501,  1033,  -986,  1596,   902,  1767,
     473,   112,   156,  1665,   100,  2028,    83,   940,    83,   308,
     963,    83,  1118,    83,   982,    92,   670,   272,  1460,  1460,
    1460,  1145,    83,  1147,  1991,   192,    83,  1676,   583,    75,
     112,  1461,  1461,  1461,   117,   474,  -670,   683,   500,   510,
    1162,   112,   136,  -670,  1079,  1959,  1677,  1370,    99,  1125,
     773,   774,  1185,   670,  1328,   105,   750,    58,    83,  1155,
    1137,   751,   272,   117,  1467,   194,   112,   961,   361,  1121,
     291,   214,   -23,  1124,   117,   292,  1824,    83,   296,   475,
     301,   555,   377,  1747,  1139,  1140,    58,   511,   925,   501,
     707,   981,   846,  1033,   708,  1825,   775,   776,   450,   117,
     663,    58,  1474,  2039,   986,   219,  1680,    58,   220,  1284,
    1350,   145,  2064,  1150,    -3,   214,  1676,  1351,  1033,   116,
      83,    75,    83,   233,    83,  1536,   236,   608,    83,    58,
    1834,    83,  1932,  1131,   501,  1827,  1277,  1868,   925,  1582,
    1300,  1173,  2030,   214,   234,   555,  1328,   211,  1834,  1926,
      75,  1933,  1033,  1133,  1033,  2045,    83,  1916,   598,  1917,
     785,  1146,  2102,   116,   786,    75,   235,  1936,    58,  -493,
    1177,    75,  1892,  2026,   555,   260,   830,  1768,  1033,    14,
      15,    16,    17,    18,  1033,  1181,  -492,   910,  2104,   555,
     903,  1424,   498,    75,  2133,   555,   272,   485,   303,  1783,
    1033,    83,   744,   194,   318,   305,    83,    58,    83,  1902,
    2135,  -987,    58,  1428,   924,   743,  1026,   555,  -987,  2093,
      83,   928,   307,    84,   752,   932,   152,  1027,  1028,   753,
    1318,    83,    75,   361,  1382,   291,    93,   519,  1383,    58,
      83,   961,    14,    15,    16,    17,    18,   377,   106,  1861,
    1578,  1411,  1432,    74,  1862,   327,   555,  1398,    14,    15,
      16,    17,    18,   308,  1399,   965,  1386,  1387,   663,   498,
    -824,    75,   392,    83,   487,   669,    75,   766,  1776,   670,
     903,    84,   291,   349,   767,   768,    80,   671,   100,  1873,
     198,  1544,   663,   824,  1862,   670,  1597,   555,    84,   672,
     555,  1980,    58,    75,   395,   663,  1981,   476,    84,    83,
      83,   519,  1436,   243,  2117,    92,  1437,   272,    58,  2118,
    1438,    84,  1421,   403,    84,  1371,  1146,   292,    84,   687,
     595,   301,   100,   477,  1869,  2175,  1813,  1288,    63,    64,
    2176,  1826,   107,   511,    58,   607,    64,  1418,    99,  1716,
     112,   478,    58,   164,   479,   105,   165,   166,  1687,   167,
    1402,   507,  1687,  1708,   480,    83,    75,  1395,   846,   811,
     332,  1299,    84,   812,   481,   813,  1708,   727,    84,   708,
     683,   508,    75,   117,   514,   595,   939,  1713,  1903,    78,
     940,   871,   872,   387,   112,   873,   261,   262,   498,   263,
     307,    58,   475,  1912,   555,   264,  1150,   527,    75,   656,
     303,   145,  1849,  1850,  1851,  1852,    75,    83,    84,    84,
     513,    83,   145,  1548,   511,  1656,   998,   117,  1606,   484,
     653,   553,   555,  1000,    84,  1853,  1610,   653,  1529,   965,
     670,   529,   663,   498,  1854,    84,    74,  1001,   332,    83,
     532,   708,   519,   538,   243,   534,    84,   536,   771,   772,
    1032,    84,    84,   116,  1033,    75,   533,   564,   804,   894,
     498,   555,   555,   230,   332,   575,  1782,    83,   727,    80,
      81,   576,    84,    83,    83,  2067,   769,   770,    74,   555,
      84,  1009,   507,   746,  1012,   581,  1014,   394,   394,  1016,
      14,    15,    16,    17,    18,    84,   777,   778,  1019,   588,
     669,  1021,  1022,  1023,   670,   227,  1099,   599,    83,  1828,
     555,    80,   671,  1996,   623,   666,   611,  1998,    74,   616,
     332,   640,   627,   475,   152,   555,    93,  1466,    84,  1566,
     643,   361,  1640,   638,   332,  1159,   644,  1289,  1279,   812,
    1814,    84,    84,  1205,   555,   377,  1553,   658,   291,   690,
      58,    80,    81,  1634,  1118,   307,   475,  1635,   670,   555,
     564,  1636,   663,   386,   555,   686,   701,    14,    15,    16,
      17,    18,   710,  1194,   704,  1196,  1288,  1033,  -494,  1033,
     640,   709,   519,    92,   555,    83,    83,    83,    14,    15,
      16,    17,    18,  1327,   519,   697,  1366,  1328,  1535,  1137,
    1328,   711,   812,  1137,  1779,  1137,   714,   361,  1780,   717,
    1594,  2062,   718,   519,    75,  -496,    99,    92,   722,    83,
     746,   377,   100,   105,  1146,   498,  1841,    58,   832,   746,
    1328,  1845,   107,  1290,    83,  1033,   765,    83,    83,   779,
      83,   754,   780,   755,   756,   757,    83,   781,    58,    83,
      99,   450,   450,   281,   272,  1870,   782,   105,   787,  1033,
    1906,   145,  1849,  1850,  1851,  1852,  1871,  1872,   277,   288,
     812,  1033,  1439,  1759,   758,  1033,   483,   759,   760,   145,
      84,   814,   761,   762,   112,  1853,  1627,  1628,  1937,    74,
     815,    75,   812,    83,  1859,  1985,  2023,  1622,   840,  1033,
    1033,  2120,   981,  2121,   485,  1328,    84,  1033,  1024,   746,
     519,   669,    75,   145,  2194,   670,  2200,   117,  2191,    83,
    2201,   816,    80,   671,   817,  1690,  1794,   818,    84,  1690,
      84,   116,  2148,   819,  1708,  1797,  2152,   361,   145,  1798,
     827,   880,    14,    15,    16,    17,    18,  1035,  1036,    84,
    1673,   377,    83,   243,  1059,  1060,  1061,  1849,  1850,  1851,
    1852,    84,  1810,  1811,  1812,   116,   829,  1466,  1466,  1466,
    1033,  1688,  1658,  1466,    -3,  1688,   850,   332,   658,   746,
    1853,  1353,  1354,   332,  -495,    84,  1289,  1368,  1369,  -193,
     285,    84,   852,   484,  1849,  1850,  1851,  1852,   450,  1451,
    1452,  1453,    58,   539,    93,  1033,  1372,   540,  1837,  1837,
    1837,  1674,  1066,  1067,  1068,  1069,  1279,  1853,  -164,  -164,
     854,    83,   -18,   541,   867,    83,    83,   542,   154,   868,
    1146,   207,   746,  1772,  1510,  1511,  1588,  1589,    93,   543,
     876,   948,   879,   332,  1592,  1589,   891,   519,   544,    84,
    1279,    84,  1595,  1589,    84,  1107,  1581,   545,  1629,  1581,
    1107,  1641,   269,   282,  1792,  1354,    75,  -497,   892,   960,
     893,   519,   519,   546,  1719,  1720,  1721,  1722,  1723,   105,
     895,    83,  1290,   105,   105,  1914,  1354,  1915,  1589,   905,
    2080,  1462,  1924,  1033,  1983,  1984,   896,   105,   897,   547,
     100,   985,   898,   663,  -129,  -129,  -129,  -129,  -129,  -129,
     107,   906,   597,   145,  2001,  1589,   899,    83,  2002,  1589,
    2080,  -128,  -128,  -128,  -128,  -128,  -128,   320,   548,  1942,
    1943,  2191,  2192,   498,   100,   145,   519,   900,  1288,   145,
     145,   926,  1673,    83,   107,  1062,  1063,  1673,    83,    83,
      83,  2130,   719,   145,  1586,  1587,   550,   927,  1829,  1064,
    1065,  -614,   112,  1044,  -612,  1046,   881,  1049,   272,   936,
     882,  1070,  1071,   937,  1057,  1743,  1745,   763,   764,  1880,
      14,    15,    16,    17,    18,  1030,   883,   116,   938,   941,
     884,   116,   116,  1838,  1839,   117,   112,   595,   763,  1081,
     944,   952,   885,  1674,   954,   116,   958,   971,  1674,   973,
     672,   886,  1008,    83,   989,   744,  1034,   551,    83,   997,
     887,  1037,  1106,   145,  1042,    83,  1083,    83,  1107,   117,
    1114,   763,  1135,  1993,  1138,    83,   888,  1157,   155,  1161,
    1186,  1109,  1164,   840,   484,  1165,  1166,  1167,  1168,    84,
    1208,    84,  1169,  1170,   519,  1171,  1172,  1193,  1195,  1197,
      93,   519,   889,   832,    93,    93,   281,  -807,  1201,  1209,
    1210,  1281,  1692,  1287,  1308,  1309,  1692,  1692,    93,   520,
      84,  1310,   394,    84,    14,    15,    16,    17,    18,  1345,
    1692,   890,   519,  1675,   151,  1311,   176,   177,    65,    66,
      67,    68,    69,    70,    71,    72,   594,  1571,   332,  1296,
    1319,  1321,  1325,  1326,  1329,   683,    84,  1330,  1332,  1331,
     901,  1335,  1024,  1336,  1334,  1337,  1288,  1979,  1338,  1339,
     519,  1462,  1462,  1462,   156,  1654,  1655,  1659,  1341,   498,
    1342,  1343,  1348,   278,   289,  1349,  1357,    83,  1289,    83,
    1358,   145,  1215,  1373,  1377,  1378,   100,  1379,  1380,  1400,
     100,   100,  1384,  1385,  1389,  1390,   107,  1391,  1392,  1482,
     107,   107,  1405,  1403,   100,  1417,  1499,  1406,  1407,  1409,
     450,   902,  -808,  1444,   107,  1476,  1477,  1480,    83,  2111,
    1481,    83,  1490,  1491,  1492,  1494,  2124,  1555,   -22,  1504,
     519,   519,  1033,   285,  1503,  1524,  1525,   519,  1559,  1531,
    1563,   145,   595,  1533,  1577,  2011,  1581,  1601,   112,  1602,
     519,  1605,   112,   112,  1616,  1617,  1560,  1673,  1663,  2057,
     519,  1618,   519,  1624,  1620,  1625,   112,  1633,  1589,  1630,
    1637,  1638,  1639,  1647,  1290,   519,   539,   519,   519,   519,
     540,   117,  1646,    83,    83,   117,   117,  1650,  1694,    86,
      84,  1661,   153,  1469,    84,   105,   541,  1709,  1710,   117,
     542,  1712,  1511,  1714,  1727,   269,   282,  1734,  1735,  1736,
    1737,  1738,   543,  1234,  1748,  1675,  1749,  1751,  1674,  1753,
    1675,   544,    84,  2053,  1755,    84,  1754,  1756,    83,   552,
     545,  1757,  1763,  1765,  1766,   519,  1058,  1773,  1777,   519,
    1778,   450,  1109,   450,   519,  1785,   546,    86,  1394,   840,
      84,   145,  1781,  1793,  1795,  1796,    84,    84,   475,  1808,
    1799,  1801,  1629,  1803,   196,  1804,  1289,  1805,  1809,  1823,
    1846,  1904,   547,  1842,    86,  1667,  2125,  1848,  2011,   228,
    1877,  1879,  1896,  1950,   450,  1897,  1913,   240,  1908,  1900,
     268,    84,  2057,  1901,    86,  2053,  2057,  2057,   519,  1918,
    1921,   548,   864,   116,   520,  1922,  2051,  1923,  1955,  1971,
     519,   272,  1976,  2170,   519,  1956,  1969,   598,  1978,  2150,
    1982,  1987,  1994,    83,   199,   200,  1997,  2173,   550,   519,
    1995,   637,   153,   201,  1999,  2000,  2003,   555,    86,  2004,
      83,   153,    83,  2005,   330,   338,  2009,  2025,  2027,  2033,
    2185,   105,  2040,  2046,  2185,   450,  2038,   360,   594,  2054,
    2065,  2066,  1290,  2055,  2077,   901,  2101,  2086,  2105,  2195,
    2103,   519,  2115,  2116,  2114,  2132,    93,  2119,  2051,  2134,
    2136,   105,   457,  2145,   196,   196,  2149,  2155,  1692,   551,
    2193,  2172,  2156,   903,  2161,   153,   490,  2151,    83,    83,
     268,  2171,  2174,  2180,  2182,  2183,  2187,   145,  2188,  2198,
    2199,   519,   105,  2202,  1910,  1565,   519,   616,  1029,  1072,
      84,    84,   330,  1073,   332,  1074,   902,   240,   240,   309,
     213,  1075,    84,  1076,  1498,  1506,    83,   145,  2181,    14,
      15,    16,    17,    18,   519,   802,  2126,   519,   330,   519,
    1696,  1973,  2143,  1718,  1966,  2123,    86,  1889,  2169,   116,
    2107,  1875,  1876,  2153,   595,  1523,  2189,   510,   145,  2106,
     519,   268,   100,   175,   298,  2008,  1567,   587,  2071,  1571,
    1660,    83,   107,  1558,  1905,  1521,  1156,     3,  1750,   116,
      83,  1340,   956,    84,  1893,  1086,    19,  1344,  1087,    58,
    1675,  1002,    84,   874,   330,  1088,  1807,   663,  1352,     0,
     338,    14,    15,    16,    17,    18,   338,   330,   330,     0,
     116,   396,   531,     0,     0,     0,   153,  1613,     0,     0,
       0,     0,    93,     0,   112,    84,    48,    49,    50,    51,
      52,    53,    54,    55,  1692,     0,     0,     0,   360,   673,
     682,   187,     6,     7,     8,     9,    10,    11,    12,    13,
       0,    74,    93,    75,   360,     0,     0,   117,   360,     0,
       0,    58,     0,     0,  1692,     0,     0,     0,     0,   663,
       0,     0,     0,   804,     0,     0,     0,   555,     0,     0,
       0,     0,     0,    93,    80,    81,  1672,   397,     0,     0,
       0,     0,     0,     0,    84,  1692,   625,     0,     0,     0,
       0,   457,     0,   290,     0,   151,     0,   176,   177,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   100,     0,
      84,     0,     0,    74,     0,    75,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   457,     0,     0,   805,     0,
       0,     0,     0,     0,   520,  1814,   196,   864,   100,   555,
       0,   221,     0,     0,    84,   398,    80,    81,   107,     0,
       0,   552,   218,     0,   153,     0,     0,     0,   490,     0,
       0,     0,   839,     0,   682,     0,     0,     0,     0,   100,
     112,   719,     0,     0,     0,   153,     0,     0,     0,   107,
       0,     0,   306,     0,     0,     0,     0,     0,   903,     0,
       0,     0,     0,     0,   502,     0,   742,   457,     0,    84,
     112,     0,     0,   117,     0,   240,    84,     0,     0,     0,
       0,    84,    84,    84,     0,     0,   151,   240,     0,   218,
      65,    66,    67,    68,    69,    70,    71,    72,  1359,   621,
       0,   112,  1360,   117,  1361,     0,     0,     0,     0,     0,
       0,   330,     0,   457,   457,     0,     0,   330,     0,     0,
     360,     0,     0,     0,   763,     0,     0,     0,     0,     0,
     466,     0,     0,     0,   117,     0,     0,    77,  1672,     0,
    1584,     0,     0,  1672,     0,     0,    84,     0,     0,     0,
    1830,    84,  1672,     0,     0,     0,     0,     0,    84,     0,
      84,     0,  1572,  1573,  1574,     0,     0,     0,     0,  1575,
    1576,     0,     0,     0,     0,   330,     0,   330,     0,     0,
      86,     0,     0,     0,  1362,     0,     0,     0,  1362,     0,
       0,     0,     0,     0,     0,     0,   360,   490,     0,   682,
       0,   396,     0,     0,   585,   909,     0,   673,     0,     0,
       0,   673,     0,   913,     0,     0,  1362,     0,   151,   520,
     360,     0,    65,    66,    67,    68,    69,    70,    71,    72,
     682,     0,   923,   360,     0,     0,     0,     0,     0,     0,
       0,     0,   153,   931,     0,     0,     0,   635,     0,     0,
     457,     0,     0,   457,     0,   153,   153,     0,   457,     0,
       0,     0,     0,    84,     0,     0,     0,   457,  1393,    77,
     153,   153,   153,     0,   585,     0,     0,   397,     0,   343,
      84,     0,    84,     0,     0,  1362,     0,   344,   345,   346,
     347,     0,     0,     0,     0,   151,   668,   176,   177,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,  1938,     0,     0,  1672,     0,     0,     0,
       0,    84,     0,     0,    84,     0,   490,     0,   151,     0,
     176,   177,    65,    66,    67,    68,    69,    70,    71,    72,
     260,     0,   805,   805,     0,     0,     0,     0,     0,     0,
     457,     0,     0,     0,     0,   742,   621,     0,   742,     0,
       0,     0,     0,   742,     0,     0,     0,     0,     0,     0,
     360,   490,   742,     0,     0,   839,     0,   839,     0,   332,
       0,     0,   348,     0,     0,     0,     0,    84,     0,     0,
     360,   742,   360,   218,     0,     0,   360,   360,   360,   360,
     349,   187,     6,     7,     8,     9,    10,    11,    12,    13,
     834,     0,   836,     0,     0,     0,   360,     0,     0,     0,
       0,   853,     0,  1672,     0,     0,   668,     0,     0,     0,
       0,    84,   621,     0,     0,     0,     0,     0,   502,     0,
       0,     0,   330,     0,   151,     0,   237,   238,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,   621,     0,     0,   102,   466,     0,   157,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
     457,     0,     0,     0,     0,   360,     0,   520,     0,     0,
       0,   153,   457,     0,  1666,    77,  1362,     0,     0,     0,
     360,  1667,  1303,     0,     0,    80,    81,     0,     0,     0,
       0,   466,   466,   673,     0,   502,     0,     0,     0,     0,
       0,     0,   102,   151,     0,   920,  2094,    65,    66,    67,
      68,    69,    70,    71,    72,   332,     0,     0,     0,     0,
       0,     0,     0,    84,     0,  2113,  1507,     0,     0,   212,
       0,     0,     0,  1192,     0,     0,     0,     0,  1883,     0,
       0,     0,   153,   490,     0,     0,     0,  1200,     0,   283,
       0,  1204,     0,     0,   151,  1207,   176,   177,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,   638,   332,     0,   621,     0,     0,     0,
       0,    84,    84,   317,     0,     0,     0,   322,     0,     0,
       0,     0,   621,   102,     0,     0,   621,     0,   983,     0,
       0,     0,     0,     0,   520,     0,     0,   805,     0,   621,
       0,   332,   362,     0,     0,     0,     0,     0,     0,    84,
     466,     0,   360,   360,   502,     0,   839,     0,  1010,  1508,
       0,     0,     0,   839,   466,     0,  1017,     0,   151,   468,
     176,   177,    65,    66,    67,    68,    69,    70,    71,    72,
     322,   496,   151,     0,   237,   238,    65,    66,    67,    68,
      69,    70,    71,    72,  2196,     0,     0,     0,     0,   502,
       0,     0,     0,  2203,     0,     0,     0,     0,   360,     0,
       0,     0,     0,   549,     0,     0,     0,     0,   502,     0,
     502,     0,   317,   520,   502,   502,     0,   502,     0,     0,
    1362,     0,     0,   573,     0,  1362,  1362,  1362,   578,   580,
    1214,   212,     0,  1484,   502,     0,     0,   386,     0,     0,
     153,   317,     0,     0,     0,     0,     0,     0,   466,   153,
       0,     0,   317,     0,     0,   603,     0,     0,   457,   605,
       0,     0,     0,     0,   606,     0,     0,     0,     0,     0,
     617,  1130,     0,     0,     0,   580,     0,   317,     0,     0,
       0,   630,     0,     0,   457,     0,     0,     0,     0,     0,
       0,   151,   457,   639,  1213,    65,    66,    67,    68,    69,
      70,    71,    72,   502,  2061,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,   268,    86,     0,     0,
       0,     0,     0,   661,     0,     0,   685,     0,   330,     0,
       0,     0,     0,     0,   153,     0,     0,     0,     0,   692,
       0,   490,    77,   692,     0,   863,     0,   151,  1449,   237,
     238,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,  1473,     0,     0,     0,  1282,  1283,     0,     0,
       0,   490,     0,     0,     0,    74,   153,    75,     0,     0,
       0,     0,  1495,    19,     0,     0,     0,   621,     0,   742,
     466,   621,     0,     0,     0,     0,     0,  2109,    77,     0,
     621,   555,     0,     0,     0,     0,     0,   520,    80,    81,
     621,     0,     0,     0,     0,     0,     0,   621,     0,     0,
       0,     0,     0,     0,  1362,     0,  1362,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   360,     0,     0,   360,   360,     0,   360,     0,   322,
       0,     0,     0,   661,     0,     0,     0,     0,     0,     0,
     502,   502,  1355,     0,     0,   621,     0,     0,     0,   621,
     322,     0,   151,   621,   176,   177,    65,    66,    67,    68,
      69,    70,    71,    72,  1051,     0,     0,     0,     0,   153,
     153,   153,   153,     0,   153,   153,     0,     0,     0,     0,
    1668,   338,   151,  1375,   384,   385,    65,    66,    67,    68,
      69,    70,    71,    72,   457,     0,   502,     0,   457,   457,
       0,     0,     0,     0,     0,     0,  1052,   617,     0,   457,
       0,   151,   457,   176,   177,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,   496,     0,     0,     0,     0,
       0,     0,  1401,    58,  1404,    78,     0,     0,     0,     0,
     268,   317,     0,     0,     0,     0,  1408,   386,  1410,     0,
       0,     0,     0,  1415,  1416,     0,     0,   490,     0,     0,
       0,     0,     0,  1423,     0,     0,     0,   151,     0,   646,
     191,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,   153,   617,   673,   102,     0,     0,     0,  1442,
       0,     0,  1445,  1682,  1684,    74,     0,    75,     0,     0,
       0,   692,   964,     0,     0,     0,     0,     0,     0,     0,
     271,     0,   617,     0,     0,     0,   975,    76,    77,     0,
       0,     0,   293,     0,   300,   661,   302,     0,    80,    81,
     984,     0,     0,     0,     0,     0,     0,     0,   692,     0,
       0,     0,     0,   151,  1746,   607,    64,    65,    66,    67,
      68,    69,    70,    71,    72,  1505,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   271,     0,     0,   300,   302,
       0,     0,  1668,  1815,     0,     0,     0,  1668,     0,   457,
       0,     0,     0,     0,  1668,  1526,  1668,     0,     0,     0,
       0,     0,  1530,     0,  1532,  1534,   621,  1080,     0,     0,
     621,     0,     0,  1540,   621,  1541,     0,  1542,     0,   338,
     153,     0,   466,     0,  1551,    58,     0,   151,     0,   983,
     271,    65,    66,    67,    68,    69,    70,    71,    72,  1359,
       0,   496,     0,  1360,     0,  1361,     0,     0,     0,   502,
       0,     0,   502,   502,     0,     0,     0,   617,  1089,   151,
       0,   237,   238,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,   617,     0,     0,     0,   617,    77,     0,
     153,  1788,     0,     0,     0,   692,   964,    74,     0,    75,
     617,     0,  1115,     0,     0,     0,     0,  1603,  1604,     0,
       0,   271,     0,   300,   302,   496,     0,   496,   153,   239,
      77,   496,   496,   362,   496,     0,     0,     0,     0,     0,
      80,    81,     0,  1626,     0,    58,   621,     0,     0,     0,
    1631,   496,  1632,     0,     0,   271,     0,     0,     0,     0,
     271,     0,  1815,  1815,     0,     0,   271,    14,    15,    16,
      17,    18,     0,     0,     0,     0,  1874,  1668,  1648,   151,
    1668,   237,   238,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   338,     0,     0,     0,     0,     0,     0,   621,
     271,     0,     0,     0,   193,   688,     0,   302,   621,    75,
     457,   617,   621,     0,     0,  1278,     0,     0,     0,     0,
     496,     0,     0,     0,     0,     0,   157,    58,     0,  1393,
      77,     0,     0,     0,     0,   692,     0,   466,  1307,     0,
       0,     0,     0,   330,   273,  1313,     0,     0,     0,     0,
       0,   566,   726,     0,     0,     0,   294,   297,     0,     0,
       0,   151,     0,   237,   238,    65,    66,    67,    68,    69,
      70,    71,    72,     0,  1758,     0,     0,     0,  1815,     0,
       0,  1762,     0,  1764,     0,     0,     0,  1668,     0,    74,
       0,    75,     0,     0,     0,     0,     0,   322,   362,   273,
       0,     0,     0,     0,     0,     0,   271,     0,     0,     0,
       0,   837,    77,     0,     0,   670,     0,     0,     0,     0,
       0,     0,    80,   838,     0,   153,     0,     0,     0,     0,
       0,     0,     0,     0,   271,     0,   688,   302,     0,     0,
     151,     0,   176,   177,    65,    66,    67,    68,    69,    70,
      71,    72,     0,   726,   273,  1815,     0,     0,  1800,     0,
       0,     0,     0,     0,     0,     0,   153,     0,   617,     0,
       0,     0,   617,     0,     0,     0,     0,   496,   496,     0,
       0,   617,     0,     0,     0,     0,     0,   271,     0,     0,
       0,   617,     0,     0,     0,     0,   153,   153,   617,  2110,
     338,  1486,     0,     0,     0,     0,     0,   466,     0,     0,
       0,   271,     0,     0,     0,     0,   271,     0,   271,     0,
       0,     0,     0,     0,     0,   273,     0,   153,     0,     0,
       0,     0,     0,   496,     0,     0,     0,     0,     0,     0,
       0,   271,     0,   271,   271,     0,   617,     0,     0,     0,
     617,     0,     0,     0,   617,   271,     0,  2110,  2110,   273,
       0,   810,     0,     0,   273,     0,     0,     0,   271,     0,
     273,     0,     0,     0,     0,   157,     0,   271,   822,     0,
       0,   825,     0,     0,  1463,     0,     0,     0,     0,     0,
       0,  1898,  1899,  1278,     0,  2110,     0,    58,     0,   271,
       0,   688,   302,     0,   273,  1907,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,     0,     0,
       0,     0,     0,   271,   688,     0,     0,  1278,     0,     0,
     271,   151,     0,   237,   238,    65,    66,    67,    68,    69,
      70,    71,    72,   566,   119,     0,     0,   119,     0,     0,
       0,     0,  1522,     0,     0,     0,   728,     0,   151,    74,
       0,    75,    65,    66,    67,    68,    69,    70,    71,    72,
       0,    58,     0,     0,     0,     0,   661,     0,     0,     0,
       0,   329,    77,     0,     0,   578,     0,     0,     0,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,   617,   151,   362,   237,   238,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,  2184,     0,     0,     0,     0,   119,
       0,     0,     0,    74,     0,    75,  2190,     0,   273,     0,
       0,     0,     0,     0,     0,   275,     0,     0,     0,   119,
       0,     0,     0,     0,     0,  2109,    77,   728,     0,   555,
       0,     0,     0,     0,     0,     0,    80,    81,     0,   621,
       0,     0,     0,     0,     0,     0,   496,     0,     0,   496,
     496,     0,   362,   119,     0,     0,     0,   119,     0,     0,
       0,     0,     0,   119,     0,     0,   119,   617,     0,     0,
     275,   617,     0,     0,     0,   617,     0,     0,     0,     0,
       0,   356,   119,     0,   388,     0,     0,     0,     0,     0,
       0,     0,   273,     0,  1463,  1463,  1463,   157,   580,     0,
       0,     0,     0,     0,     0,     0,     0,   461,     0,     0,
       0,     0,     0,     0,     0,   273,    58,     0,     0,  1691,
     119,   461,     0,  1691,  1691,   275,     0,     0,     0,   273,
       0,     0,     0,     0,     0,     0,     0,  1691,     0,     0,
       0,     0,   273,     0,     0,     0,     0,     0,     0,     0,
     151,  2108,   237,   238,    65,    66,    67,    68,    69,    70,
      71,    72,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   271,   273,     0,   810,   810,     0,    74,   119,
      75,   119,   362,   271,     0,     0,  1097,   617,     0,  1100,
       0,   119,   271,     0,     0,     0,   275,   273,     0,     0,
    1666,    77,   119,     0,   273,     0,     0,   157,  2144,     0,
       0,    80,    81,     0,     0,     0,     0,   612,     0,     0,
     119,     0,     0,     0,     0,   119,     0,   119,     0,  2159,
     275,   119,     0,     0,     0,   275,     0,     0,     0,     0,
     617,   275,     0,     0,  2168,     0,     0,     0,     0,   617,
       0,   119,     0,   617,     0,   566,    14,    15,    16,    17,
      18,     0,  1175,     0,     0,     0,  1179,     0,     0,     0,
    1183,     0,     0,   119,     0,   275,   119,     0,     0,     0,
       0,     0,     0,     0,    94,     0,     0,    94,     0,   119,
       0,     0,     0,   119,   151,     0,     0,   271,    65,    66,
      67,    68,    69,    70,    71,    72,  1359,     0,     0,     0,
    1360,  1832,  1361,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,   271,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,     0,  1844,   461,     0,     0,     0,
       0,     0,    94,     0,     0,    77,     0,     0,  1790,     0,
     151,     0,   237,   238,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
     461,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,   270,     0,    58,     0,    94,
       0,     0,     0,     0,     0,   157,     0,     0,     0,   119,
     239,    77,     0,   461,     0,     0,     0,     0,     0,   275,
       0,    80,    81,     0,     0,     0,     0,     0,     0,  1189,
     119,   151,     0,   237,   238,    65,    66,    67,    68,    69,
      70,    71,    72,    94,     0,     0,     0,     0,     0,     0,
     339,     0,   461,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,  1930,   151,
     810,   205,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   329,    77,     0,     0,     0,   273,   119,     0,     0,
     150,     0,    80,    81,     0,     0,   271,     0,   461,   461,
       0,   493,     0,   275,     0,   119,     0,     0,     0,     0,
       0,     0,  1961,     0,     0,  1691,     0,     0,     0,     0,
      77,   119,     0,   863,     0,   271,     0,     0,     0,     0,
       0,   271,     0,     0,     0,     0,     0,     0,     0,     0,
     275,     0,     0,     0,     0,  1426,     0,     0,  1430,     0,
     151,     0,  1434,   275,    65,    66,    67,    68,    69,    70,
      71,    72,  1047,   119,     0,   119,   208,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   270,     0,     0,     0,
     388,   119,   461,     0,   275,     0,    14,    15,    16,    17,
      18,   151,   119,   609,   610,    65,    66,    67,    68,    69,
      70,    71,    72,     0,  1048,   119,     0,     0,   275,     0,
       0,     0,   612,     0,     0,   275,     0,     0,   119,   113,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
    2052,     0,     0,     0,     0,   461,     0,     0,   461,     0,
     119,   119,     0,   461,    78,     0,    58,     0,     0,     0,
       0,     0,   461,     0,     0,   119,   119,   119,     0,   151,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,  1691,     0,     0,     0,     0,     0,   113,     0,     0,
     151,   271,   237,   238,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,   208,     0,     0,     0,
       0,  1691,  2052,     0,     0,     0,     0,     0,    74,     0,
      75,   461,    78,     0,     0,     0,     0,     0,     0,     0,
     617,     0,     0,     0,   284,     0,     0,   119,     0,     0,
    1666,    77,  1691,     0,     0,   461,     0,     0,     0,     0,
     271,    80,    81,   119,     0,     0,     0,   119,     0,    14,
      15,    16,    17,    18,   582,   119,   461,     0,   113,  1599,
     119,     0,     0,  2147,     0,     0,     0,     0,   113,     0,
    1608,     0,     0,     0,     0,   119,     0,   119,     0,     0,
       0,   119,   119,   119,   119,     0,     0,   365,     0,     0,
       0,     0,     0,     0,     0,   624,     0,     0,     0,     0,
     273,   119,     0,     0,     0,     0,     0,   631,   413,    58,
     414,   415,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,   582,     0,   497,     0,     0,   273,
    1190,   151,     0,   176,   177,    65,    66,    67,    68,    69,
      70,    71,    72,   151,     0,     0,   659,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
     748,   119,     0,    78,   424,   461,     0,   113,     0,     0,
     119,    74,     0,    75,     0,   493,   119,   461,     0,     0,
       0,   508,     0,     0,     0,   119,     0,  1305,   461,     0,
       0,     0,     0,    76,    77,     0,   113,     0,     0,     0,
       0,     0,     0,     0,    80,    81,     0,   113,     0,   271,
     604,   747,   151,     0,     0,  1320,    65,    66,    67,    68,
      69,    70,    71,    72,  1359,   365,     0,     0,  1360,     0,
    1361,     0,   113,     0,     0,    94,   284,     0,     0,     0,
       0,     0,   788,     0,     0,     0,   230,   119,   461,     0,
       0,   151,   493,   176,   177,    65,    66,    67,    68,    69,
      70,    71,    72,    77,     0,     0,  1644,     0,     0,     0,
     828,     0,     0,     0,     0,   833,     0,     0,   662,     0,
       0,   284,     0,     0,     0,   273,     0,     0,     0,     0,
       0,     0,     0,     0,   662,   859,   860,     0,   662,     0,
     861,   862,     0,     0,   865,   151,     0,   237,   238,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   119,     0,
     878,     0,   119,     0,     0,     0,     0,   119,   119,     0,
       0,   119,     0,    74,     0,     0,  1818,     0,     0,     0,
       0,   119,   907,     0,   273,     0,     0,     0,   119,     0,
     271,     0,     0,     0,     0,   837,    77,     0,     0,   670,
       0,     0,   271,     0,     0,     0,    80,   838,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   672,
       0,   493,   151,   119,   176,   177,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,   119,     0,     0,     0,
     119,     0,     0,     0,   119,     0,     0,   151,   662,   176,
     177,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     947,     0,     0,     0,     0,   119,   493,     0,     0,     0,
       0,     0,   513,   953,   119,     0,     0,     0,     0,     0,
       0,     0,     0,   461,     0,   493,     0,   493,   271,     0,
       0,   493,   493,     0,   493,     0,     0,   972,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   461,
       0,   493,     0,     0,     0,     0,     0,   461,     0,   271,
     302,   151,   365,   237,   238,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,  1818,  1818,     0,     0,     0,
     497,   275,   119,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,   273,     0,     0,   113,     0,     0,   119,
       0,     0,  1025,     0,     0,     0,   461,     0,     0,     0,
    1305,  1666,    77,     0,     0,    94,     0,     0,  1667,     0,
     493,     0,    80,    81,     0,     0,    94,     0,     0,     0,
       0,     0,     0,     0,   119,     0,   461,     0,   365,     0,
     113,   119,     0,     0,     0,     0,     0,     0,     0,     0,
     271,     0,     0,     0,     0,     0,   662,   497,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,   789,
     790,   791,   792,   793,   794,   795,   796,   797,   798,   799,
     662,     0,     0,     0,   222,     0,     0,     0,     0,     0,
       0,  1818,     0,   662,  1104,     0,  1105,     0,     0,     0,
       0,     0,   833,     0,     0,     0,   119,     0,     0,   119,
     119,   800,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
    1149,   119,     0,     0,     0,   119,     0,     0,     0,  1158,
       0,     0,     0,  1160,     0,   271,   273,  1645,     0,     0,
       0,     0,     0,     0,   119,   119,   119,   119,   119,   119,
     119,     0,     0,     0,  2069,     0,   275,     0,  1818,     0,
       0,     0,     0,     0,     0,     0,     0,   493,   493,   461,
       0,     0,   118,   461,   461,     0,   497,     0,   659,     0,
       0,     0,     0,  1202,   461,     0,     0,   461,     0,     0,
       0,     0,   365,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1818,     0,     0,     0,     0,     0,   365,     0,
       0,     0,   365,     0,     0,   275,     0,     0,     0,     0,
     662,   497,     0,   493,     0,   365,     0,     0,     0,     0,
     118,     0,   461,     0,     0,     0,     0,   119,     0,     0,
     365,     0,   365,     0,     0,     0,   365,   365,   497,   365,
    1769,     0,     0,     0,     0,     0,     0,   119,     0,     0,
    1818,  1818,     0,     0,     0,    94,   365,     0,     0,     0,
    1333,     0,     0,     0,     0,     0,     0,   286,     0,     0,
       0,     0,     0,    94,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,  1818,   119,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,    94,     0,     0,
       0,   118,     0,     0,     0,     0,   365,     0,     0,     0,
     113,     0,     0,     0,     0,   365,     0,     0,     0,     0,
     369,   270,    94,     0,   273,     0,     0,     0,     0,     0,
     662,     0,     0,   284,   461,   151,     0,   237,   238,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   499,
       0,     0,     0,    74,   275,   119,   151,     0,   237,   238,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,  2109,    77,     0,     0,   555,
       0,     0,     0,   497,    74,     0,    80,    81,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   239,    77,     0,     0,
       0,     0,     0,     0,     0,   119,     0,    80,    81,   118,
       0,     0,     0,     0,     0,   150,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,   493,     0,     0,   493,
     493,     0,     0,   119,     0,     0,     0,     0,   369,     0,
       0,     0,     0,   365,     0,   118,     0,   365,     0,   286,
       0,     0,   365,   365,     0,     0,   365,     0,     0,     0,
       0,     0,     0,     0,   788,     0,   365,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,    94,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   664,     0,     0,   286,     0,     0,   275,     0,    94,
       0,     0,     0,    94,    94,     0,     0,   664,   365,     0,
       0,   664,   461,     0,     0,   461,     0,    94,  1539,     0,
       0,   365,     0,     0,     0,   365,     0,     0,   173,   365,
     151,     0,   237,   238,    65,    66,    67,    68,    69,    70,
      71,    72,  1564,     0,     0,   270,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   173,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     329,    77,     0,     0,     0,     0,     0,    94,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,   173,     0,     0,     0,     0,     0,
       0,     0,     0,  2037,   275,     0,     0,   173,     0,   173,
       0,   664,     0,     0,     0,     0,     0,   284,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     173,   662,   391,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,   391,   435,   365,
       0,   497,     0,     0,     0,     0,     0,  2095,     0,     0,
       0,     0,     0,     0,     0,   369,     0,  1479,     0,     0,
       0,   119,   119,     0,     0,   275,     0,     0,     0,  1493,
       0,     0,     0,   499,   270,    94,     0,     0,     0,   173,
     119,     0,     0,   173,     0,     0,   173,   173,     0,   118,
     173,     0,   119,   173,   173,     0,   173,     0,   173,     0,
       0,     0,     0,     0,     0,     0,     0,  1752,     0,     0,
       0,   365,     0,     0,   365,   365,     0,   365,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   369,   365,   118,     0,    94,   365,     0,     0,     0,
     365,     0,     0,     0,     0,     0,     0,     0,     0,   664,
     499,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     369,     0,     0,     0,     0,     0,     0,     0,     0,   173,
       0,     0,   173,   664,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   113,     0,   664,     0,   113,   113,
       0,     0,     0,     0,     0,     0,     0,   173,     0,     0,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   173,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1752,     0,   270,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    94,     0,   497,     0,     0,
       0,     0,   365,     0,     0,     0,     0,     0,   713,     0,
     716,     0,     0,   435,   721,     0,     0,     0,     0,     0,
       0,     0,     0,   730,   731,     0,     0,     0,     0,   499,
       0,     0,     0,     0,     0,     0,     0,     0,   435,   435,
       0,     0,     0,     0,     0,   369,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,     0,     0,     0,   435,
       0,   369,     0,     0,   365,   369,     0,   173,   365,     0,
       0,     0,     0,   664,   499,     0,     0,     0,   369,     0,
    1894,  1895,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   435,   369,     0,   369,  1739,  1740,     0,   369,
     369,   499,   369,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   369,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2010,     0,
       0,     0,     0,   173,     0,     0,     0,     0,     0,     0,
     284,    94,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   369,
       0,    94,     0,   118,     0,     0,   406,     0,   369,   407,
       0,   408,   409,     0,   410,     0,     0,     0,     0,     0,
       0,     0,     0,   664,  1977,     0,   286,     0,     0,     0,
       0,   411,    94,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,     0,     0,  1752,     0,     0,     0,     0,     0,
       0,   412,   413,     0,   414,   415,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   416,   417,   403,     0,
     418,   419,   420,     0,   421,   422,   499,   173,   173,     0,
       0,  2018,    74,     0,     0,     0,     0,     0,     0,  1847,
       0,     0,     0,     0,     0,     0,  1857,   173,     0,   173,
       0,     0,     0,     0,   423,     0,     0,    78,   424,  2048,
       0,     0,     0,  2049,   425,    80,    81,   426,   427,   428,
     429,     0,     0,     0,     0,     0,     0,  1882,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,     0,     0,     0,
     369,     0,     0,     0,     0,   369,   369,     0,     0,   369,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   369,
       0,     0,     0,     0,     0,     0,   369,   435,   435,   435,
     435,   435,   435,   435,   435,   435,   435,   435,   435,   435,
     435,   435,   435,   435,   435,   435,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   173,   173,
       0,   369,     0,     0,     0,   173,     0,     0,     0,     0,
       0,     0,     0,     0,   369,     0,     0,     0,   369,     0,
       0,     0,   369,     0,     0,  1940,  1941,     0,     0,     0,
     173,     0,  1951,   173,   173,   662,   173,     0,   173,   173,
       0,     0,     0,     0,     0,  1965,     0,     0,     0,     0,
       0,     0,   435,     0,     0,  1974,     0,  1975,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
    1986,     0,  1988,  1989,  1990,     0,   113,     0,     0,     0,
       0,   173,     0,     0,     0,   173,     0,     0,     0,   173,
       0,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   113,   662,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     286,   376,     0,     0,     0,   365,     0,     0,     0,     0,
    2019,     0,     0,     0,  2024,     0,     0,   113,     0,  2029,
       0,     0,   741,     0,   664,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   486,
     376,     0,     0,   173,     0,     0,     0,     0,     0,     0,
       0,     0,   369,     0,   499,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2072,     0,   558,     0,     0,     0,     0,
       0,     0,   558,     0,     0,  2081,     0,     0,     0,  2084,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   435,     0,  2100,     0,     0,     0,   435,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   435,
       0,     0,     0,     0,   369,     0,     0,   369,   369,     0,
     369,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   369,  2131,     0,     0,   369,
       0,     0,     0,   369,     0,     0,     0,     0,     0,   435,
       0,     0,     0,     0,   558,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   915,   917,     0,     0,     0,     0,
       0,     0,     0,     0,   173,     0,  2154,     0,     0,     0,
       0,  2157,   376,   674,     0,     0,     0,   118,     0,     0,
       0,   118,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   695,     0,     0,   118,     0,     0,     0,  2177,
       0,     0,  2179,     0,  2157,     0,     0,     0,     0,   173,
       0,   173,     0,     0,   173,     0,     0,   173,     0,     0,
       0,   173,     0,     0,     0,  2179,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     499,     0,     0,     0,     0,   369,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   558,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   435,     0,     0,     0,     0,     0,     0,   558,
     823,   741,   558,   826,   741,     0,     0,     0,   369,   741,
       0,     0,   376,     0,     0,     0,   674,   369,   741,     0,
       0,   369,     0,     0,     0,     0,     0,     0,     0,   486,
       0,     0,     0,     0,     0,     0,     0,   741,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     558,     0,     0,     0,   558,     0,     0,     0,     0,   173,
       0,     0,     0,  1078,     0,   435,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   286,   376,     0,     0,   171,     0,     0,
       0,     0,     0,   435,   435,   435,     0,     0,     0,     0,
     435,   435,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   173,     0,
       0,     0,     0,     0,   435,     0,     0,     0,     0,   173,
       0,     0,   173,     0,   173,   173,     0,     0,     0,     0,
     558,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     962,   376,     0,   304,     0,   435,   435,     0,     0,     0,
       0,   674,     0,     0,     0,   674,   310,     0,   311,     0,
       0,     0,   980,     0,   376,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,   383,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,   173,    47,     0,     0,     0,     0,     0,
    1962,     0,     0,   118,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   560,   561,     0,     0,   565,
     376,     0,   568,   569,     0,   571,     0,   572,   723,     0,
     724,   725,     0,     0,     0,     0,   558,   558,     0,     0,
       0,     0,   435,     0,     0,     0,     0,   558,  1098,     0,
     558,  1101,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,   962,   376,     0,     0,     0,   674,
       0,   674,   674,     0,     0,   173,     0,     0,   674,     0,
       0,     0,     0,     0,   376,   -17,   376,     0,     0,     0,
     376,   376,   376,   376,     0,     0,     0,     0,   664,     0,
       0,   217,     0,   173,     0,     0,     0,     0,     0,     0,
     376,     0,   558,     0,     0,     0,   558,   279,     0,     0,
       0,     0,     0,   558,  1176,     0,   657,   558,  1180,     0,
       0,   558,  1184,     0,     0,     0,   173,     0,  1187,   118,
       0,   689,   173,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   217,   118,
     664,     0,   340,     0,     0,     0,     0,     0,     0,   376,
     558,     0,     0,     0,   381,     0,     0,     0,   369,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,   674,     0,   217,
       0,     0,     0,     0,     0,     0,     0,     0,   173,   435,
       0,     0,     0,   506,     0,     0,     0,   512,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   741,   821,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   486,   376,     0,     0,
       0,     0,     0,     0,     0,   173,   173,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   217,     0,     0,     0,     0,     0,     0,
       0,   173,   173,     0,     0,     0,     0,     0,   279,   391,
       0,     0,     0,     0,   173,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   558,   904,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   376,   376,     0,     0,
     674,   674,     0,   512,     0,     0,     0,   674,     0,     0,
       0,     0,     0,   217,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   667,     0,   684,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   376,     0,     0,     0,   558,  1427,     0,   558,
    1431,     0,   173,   558,  1435,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   435,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   745,     0,
       0,     0,     0,     0,     0,     0,   978,   979,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   435,   988,     0,   990,     0,
       0,     0,   217,   173,     0,  1685,  1693,     0,     0,  1685,
    1704,     0,     0,     0,     0,  1711,     0,     0,     0,  1715,
       0,  1717,     0,  1704,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   667,     0,     0,     0,     0,
       0,   851,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   376,     0,     0,     0,     0,
       0,   674,  1547,     0,   217,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   435,     0,   435,     0,     0,
       0,     0,     0,     0,     0,   376,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1091,  1092,   173,
       0,     0,     0,     0,  1096,     0,     0,     0,     0,     0,
     217,   217,     0,     0,     0,     0,     0,   506,   435,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1119,
     558,  1600,  1122,  1123,     0,  1126,     0,  1128,  1129,     0,
       0,   558,  1609,     0,   674,     0,     0,   435,     0,     0,
       0,     0,     0,     0,     0,   376,     0,     0,   376,   376,
    1806,   376,     0,     0,     0,     0,     0,   299,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1174,     0,     0,     0,  1178,     0,     0,     0,  1182,   435,
       0,     0,     0,     0,   506,     0,   966,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1843,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   667,     0,     0,
       0,   372,     0,     0,     0,     0,  1863,  1865,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   217,
       0,     0,     0,     0,     0,     0,     0,   217,     0,     0,
     745,     0,   745,   217,     0,   217,     0,  1885,     0,     0,
     372,     0,  1314,     0,   745,     0,     0,   745,   745,   745,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   376,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   674,     0,
       0,     0,     0,   506,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   217,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   372,
       0,     0,     0,     0,     0,  1949,     0,     0,   506,     0,
       0,     0,     0,  1952,     0,  1954,     0,     0,  1958,  1964,
       0,  1704,     0,     0,     0,     0,  1970,   506,     0,   506,
       0,     0,     0,   506,   506,   381,   506,     0,     0,     0,
       0,     0,     0,  1314,     0,     0,     0,   558,     0,     0,
       0,     0,   372,   506,   372,   372,     0,     0,     0,     0,
       0,     0,     0,     0,   558,     0,     0,     0,   372,     0,
       0,     0,   372,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1420,     0,
    1422,     0,     0,  1425,     0,     0,  1429,     0,     0,     0,
    1433,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2035,
       0,     0,   506,     0,     0,     0,  2042,  2044,     0,   217,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   851,
       0,    14,    15,    16,    17,    18,  2063,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -498,  -498,     0,  -498,    46,
       0,    47,   372,     0,  -498,  2085,     0,  2088,   372,     0,
    2090,  2092,     0,     0,     0,     0,     0,  2097,  2099,     0,
     381,    58,     0,     0,     0,     0,   558,   558,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   558,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1545,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   372,     0,     0,     0,
       0,  2138,  2140,  2142,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,   372,     0,     0,     0,     0,   506,
     506,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2163,  2165,  2167,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1598,     0,   372,
       0,     0,     0,     0,     0,     0,     0,     0,  1607,     0,
       0,  1611,   558,  1614,  1615,     0,     0,     0,     0,     0,
     558,     0,   372,     0,     0,   506,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,   372,     0,   372,     0,     0,     0,     0,     0,     0,
       0,   372,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   372,     0,     0,   372,     0,     0,
       0,     0,     0,     0,   372,     0,   745,   372,     0,     0,
       0,     0,     0,     0,     0,   558,  2070,     0,     0,   558,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   745,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1733,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   558,     0,     0,     0,     0,     0,     0,
       0,   371,     0,   279,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   217,     0,     0,     0,     0,     0,     0,   667,     0,
     372,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     371,     0,     0,     0,     0,     0,   372,     0,   373,     0,
       0,   558,   558,     0,     0,     0,     0,     0,   381,     0,
       0,     0,   372,   745,     0,     0,   372,     0,     0,     0,
       0,     0,     0,     0,   372,   372,     0,     0,     0,   372,
       0,     0,     0,     0,  1611,     0,     0,   373,     0,   558,
       0,     0,     0,     0,   372,     0,   372,     0,     0,     0,
     372,   372,   372,   372,     0,     0,     0,     0,     0,     0,
       0,     0,  1802,     0,     0,     0,     0,     0,     0,     0,
     372,     0,     0,     0,     0,     0,     0,     0,   506,     0,
       0,   506,   506,     0,   381,     0,     0,     0,     0,   371,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   745,   745,   745,     0,
     372,   745,   745,     0,     0,     0,   373,     0,   512,   372,
       0,     0,   371,     0,   371,   371,     0,     0,     0,     0,
       0,     0,     0,     0,   372,     0,   372,   372,   371,     0,
       0,     0,   371,     0,     0,     0,   217,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1891,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,   373,   373,     0,     0,     0,     0,   279,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,   373,
       0,     0,     0,     0,   381,     0,     0,   372,     0,     0,
       0,     0,     0,     0,  1919,  1920,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1934,  1935,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1939,     0,     0,     0,     0,     0,     0,
       0,     0,   371,     0,     0,     0,     0,     0,   371,     0,
       0,     0,     0,     0,     0,     0,     0,   372,     0,     0,
       0,   372,     0,     0,     0,     0,   372,   372,     0,     0,
     372,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,     0,     0,     0,     0,     0,     0,   372,     0,   373,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   217,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   371,     0,     0,     0,
       0,     0,   372,     0,     0,     0,     0,     0,     0,     0,
       0,  2007,     0,     0,   371,   372,   279,     0,     0,   372,
       0,     0,     0,   372,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,     0,   371,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   371,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2068,     0,     0,     0,     0,     0,     0,     0,
     371,   371,     0,   371,     0,     0,   373,     0,     0,     0,
       0,   371,     0,     0,     0,   745,     0,     0,     0,     0,
       0,     0,     0,     0,   371,     0,     0,   371,     0,   373,
       0,     0,     0,     0,   371,     0,     0,   371,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,   373,     0,
     373,     0,     0,     0,     0,   372,     0,     0,   373,   372,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,   373,     0,     0,     0,     0,   279,
       0,   373,     0,   372,   373,   372,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
     371,    47,     0,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,   371,     0,     0,     0,
       0,    58,     0,     0,     0,   372,     0,     0,   372,   372,
       0,   372,   371,     0,     0,     0,   371,     0,     0,     0,
       0,     0,     0,     0,   371,   371,   372,   373,     0,   371,
     372,     0,     0,     0,   372,   179,   182,    63,    64,     0,
       0,     0,     0,   373,   371,     0,   371,     0,     0,     0,
     371,   371,   371,   371,     0,     0,     0,     0,     0,   373,
       0,   459,   745,   373,     0,    75,     0,     0,     0,     0,
     371,   373,   373,   231,     0,   491,   373,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
     521,   373,   521,   373,     0,     0,     0,   373,   373,   373,
     373,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,   324,     0,     0,   325,     0,     0,
     371,     0,     0,     0,   745,     0,     0,   512,     0,   371,
       0,   372,   350,     0,     0,     0,   372,     0,     0,     0,
       0,     0,     0,     0,   371,     0,   371,   371,     0,     0,
       0,     0,   399,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   399,     0,     0,   373,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,     0,     0,     0,   636,     0,     0,     0,     0,   372,
       0,   373,     0,   373,   373,     0,     0,   530,   372,     0,
       0,     0,   372,     0,     0,     0,     0,   371,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   231,
       0,     0,     0,     0,     0,     0,     0,     0,   591,   592,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   179,     0,     0,   371,     0,     0,
       0,   371,     0,     0,     0,     0,   371,   371,     0,     0,
     371,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     371,     0,     0,     0,     0,     0,     0,   371,     0,   641,
       0,     0,     0,     0,     0,     0,     0,   645,   647,     0,
       0,     0,   654,     0,   373,     0,     0,     0,   373,     0,
       0,     0,     0,   373,   373,     0,     0,   373,     0,     0,
       0,     0,   371,     0,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,   373,   371,     0,     0,     0,   371,
       0,   350,     0,   371,   350,     0,     0,   399,     0,     0,
       0,     0,     0,     0,     0,   521,     0,     0,     0,     0,
       0,   521,     0,     0,     0,     0,   459,     0,     0,   373,
       0,     0,     0,   187,     6,     7,     8,     9,    10,    11,
      12,    13,   373,     0,     0,     0,   373,    19,     0,    20,
     373,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,   261,   262,     0,   263,
      46,     0,    47,     0,     0,   264,     0,     0,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,   231,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   856,   857,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   371,     0,     0,   946,   371,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   371,     0,   371,   491,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   974,
       0,     0,   373,     0,     0,     0,   373,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -474,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,   373,     0,     0,     0,  1007,     0,     0,   372,
       0,  -474,     0,     0,     0,     0,     0,     0,     0,  1018,
       0,     0,     0,     0,     0,   371,     0,     0,   371,   371,
       0,   371,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1039,  1041,     0,   371,  1043,     0,  1045,
     371,     0,     0,   951,   371,  1007,     0,  1055,  1007,     0,
       0,     0,   350,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,   373,   373,     0,   373,  1312,
       0,   372,     0,     0,   372,  1082,     0,    14,    15,    16,
      17,    18,     0,   373,     0,     0,     0,   373,  1084,   372,
       0,   373,     0,     0,     0,     0,     0,     0,     0,  1093,
       0,     0,     0,     0,     0,     0,     0,   399,     0,     0,
       0,     0,     0,   406,     0,   491,   407,     0,   408,   409,
    1082,   410,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,   411,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   371,  1152,     0,     0,   521,   371,     0,  1053,     0,
       0,     0,     0,     0,     0,     0,  1163,     0,   412,   413,
       0,   414,   415,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   416,   417,   403,     0,   418,   419,   420,
       0,   421,   422,     0,  1188,     0,     0,     0,   373,    74,
       0,    75,     0,   373,     0,     0,     0,     0,     0,   371,
       0,     0,     0,     0,     0,     0,     0,     0,   371,     0,
       0,   423,   371,     0,    78,   424,     0,     0,     0,     0,
       0,   425,   489,    81,   426,   427,   428,   429,     0,     0,
       0,   459,     0,     0,     0,     0,     0,     0,     0,     0,
    1132,  1304,  1306,     0,     0,     0,   373,     0,     0,   491,
       0,  1148,     0,     0,     0,   373,     0,     0,     0,   373,
    2178,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1478,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1082,     0,     0,     0,     0,     0,   406,     0,
    1347,   407,     0,   408,   409,     0,   410,     0,     0,  1007,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1216,  1219,     0,   411,  1221,     0,  1222,  -252,  -252,  1223,
    1224,  1225,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,
    1234,  -351,  -351,  1235,  1236,  1237,  1238,  1239,  1240,  1241,
     521,  1242,     0,   412,   413,     0,   515,   415,  1243,  1244,
      65,    66,    67,    68,    69,    70,    71,    72,   416,   417,
     403,  1245,   418,   419,   420,  1323,   421,   422,     0,  2178,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1478,     0,     0,     0,
       0,     0,     0,     0,     0,  -252,  1246,     0,     0,    78,
     424,     0,     0,     0,   308,     0,   425,    80,    81,   426,
     427,   428,   429,   521,     0,  1419,     0,   406,     0,     0,
     407,  -192,   408,   409,     0,   410,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1219,     0,   411,  1221,     0,  1222,  -253,  -253,  1223,  1224,
    1225,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,  1234,
    -351,  -351,  1235,  1236,  1237,  1238,  1239,  1240,  1241,     0,
    1242,     0,   412,   413,     0,   515,   415,  1243,  1244,    65,
      66,    67,    68,    69,    70,    71,    72,   416,   417,   403,
    1245,   418,   419,   420,     0,   421,   422,  1496,  1496,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -253,  1246,     0,     0,    78,   424,
       0,     0,     0,   308,     0,   425,    80,    81,   426,   427,
     428,   429,     0,     0,     0,     0,     0,     0,     0,     0,
    -192,     0,     0,     0,     0,     0,     0,     0,     0,   371,
       0,     0,     0,     0,  1543,     0,     0,     0,     0,     0,
    1552,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1007,     0,     0,     0,     0,
     491,     0,     0,     0,     0,  1483,  1485,  1487,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,   521,     0,
       0,  1580,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1039,     0,     0,  1509,
       0,   371,     0,     0,   371,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   371,
    1216,     0,     0,     0,     0,     0,  1528,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,   373,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
    1642,  1643,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1007,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   521,     0,     0,   459,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1881,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1478,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1041,     0,     0,     0,
       0,     0,     0,     0,     0,  1760,  1761,     0,     0,     0,
       0,     0,     0,     0,   406,     0,     0,   407,     0,   408,
     409,     0,   410,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   521,  1678,  1679,  1219,  1039,   411,
    1221,     0,  1222,     0,     0,  1223,  1224,  1225,  1226,  1227,
    1228,  1229,  1230,  1231,  1232,  1233,  1234,  -351,  -351,  1235,
    1236,  1237,  1238,  1239,  1240,  1241,     0,  1242,     0,   412,
     413,     0,   515,   415,  1243,  1244,    65,    66,    67,    68,
      69,    70,    71,    72,   416,   417,   403,  1245,   418,   419,
     420,     0,   421,   422,     0,     0,   406,     0,     0,   407,
      74,   408,   409,     0,   410,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   459,     0,
       0,   411,  1246,     0,  1831,    78,   424,  1770,     0,     0,
     308,     0,   425,    80,    81,   426,   427,   428,   429,     0,
       0,     0,     0,     0,     0,     0,     0,  -192,     0,     0,
       0,   412,   413,     0,   414,   415,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   416,   417,   403,     0,
     418,   419,   420,     0,   421,   422,     0,     0,     0,     0,
       0,     0,    74,     0,     0,  1878,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1697,  1698,  1699,
    1700,     0,     0,     0,   423,  1864,     0,    78,   424,     0,
       0,     0,     0,     0,   425,    80,    81,   426,   427,   428,
     429,     0,     0,     0,     0,     0,     0,     0,   521,     0,
       0,     0,     0,     0,     0,  1909,     0,     0,  1911,     0,
       0,     0,     0,     0,     0,  1840,     4,   187,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,  1218,     0,    20,  1925,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   406,     0,    46,   407,    47,   408,   409,     0,
     410,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,  1219,    58,  1220,  1221,     0,
    1222,     0,     0,  1223,  1224,  1225,  1226,  1227,  1228,  1229,
    1230,  1231,  1232,  1233,  1234,  -351,  -351,  1235,  1236,  1237,
    1238,  1239,  1240,  1241,     0,  1242,     0,   412,   413,    61,
     515,   415,  1243,  1244,    65,    66,    67,    68,    69,    70,
      71,    72,   416,   417,   403,  1245,   418,   419,   420,     0,
     421,   422,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -3,
    1246,     0,     0,    78,  1247,     0,     0,     0,   308,     0,
     425,    80,    81,   426,   427,   428,   429,     0,     0,     0,
       0,     0,     0,     0,     0,  -192,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1007,     0,     0,     0,     4,   187,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    1218,     0,    20,  1992,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   406,     0,    46,   407,    47,   408,   409,     0,   410,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,  1219,    58,  1220,  1221,     0,  1222,
       0,     0,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,
    1231,  1232,  1233,  1234,  -351,  -351,  1235,  1236,  1237,  1238,
    1239,  1240,  1241,     0,  1242,     0,   412,   413,    61,   515,
     415,  1243,  1244,    65,    66,    67,    68,    69,    70,    71,
      72,   416,   417,   403,  1245,   418,   419,   420,     0,   421,
     422,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1246,
       0,     0,    78,  1247,     0,     0,     0,   308,     0,   425,
      80,    81,   426,   427,   428,   429,     0,     0,     0,     0,
       0,     0,     0,     0,  -192,     4,   187,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   406,     0,    46,   407,    47,   408,   409,     0,   410,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   411,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   412,   413,    61,   414,
     415,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   416,   417,   403,     0,   418,   419,   420,     0,   421,
     422,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1697,  1698,  1699,  1700,     0,     0,     0,   423,
    1701,  1702,    78,  1247,     0,     0,     0,     0,     0,   425,
      80,    81,   426,   427,   428,   429,     0,     0,     0,     0,
       0,     0,     0,     0,  1703,     4,   187,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   406,     0,    46,   407,    47,   408,   409,     0,   410,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   411,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   412,   413,    61,   414,
     415,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   416,   417,   403,     0,   418,   419,   420,     0,   421,
     422,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1697,  1698,  1699,  1700,     0,     0,     0,   423,
    1701,     0,    78,  1247,     0,     0,     0,     0,     0,   425,
      80,    81,   426,   427,   428,   429,     0,     0,     0,     0,
       0,     0,     0,     0,  1703,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,    59,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    60,     0,     0,     0,    61,    62,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
      77,     0,    78,    79,     0,     0,     0,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    82,     4,   187,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   406,     0,    46,   407,    47,   408,   409,     0,   410,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   411,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   412,   413,    61,   414,
     415,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   416,   417,   403,     0,   418,   419,   420,     0,   421,
     422,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   423,
       0,  1695,    78,  1247,     0,     0,     0,     0,     0,   425,
      80,    81,   426,   427,   428,   429,     4,   187,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   406,     0,    46,   407,    47,   408,   409,     0,
     410,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   411,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   412,   413,    61,
     414,   415,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   416,   417,   403,     0,   418,   419,   420,     0,
     421,   422,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     423,     0,     0,    78,  1247,     0,     0,     0,     0,     0,
     425,    80,    81,   426,   427,   428,   429,   187,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   406,     0,    46,   407,    47,   408,   409,     0,
     410,   357,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   411,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   412,   413,     0,
     414,   415,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   416,   417,   403,     0,   418,   419,   420,     0,
     421,   422,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     423,     0,     0,    78,   488,     0,     0,     0,     0,     0,
     425,   489,    81,   426,   427,   428,   429,   187,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   406,     0,    46,   407,    47,   408,   409,     0,
     410,   357,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   411,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   412,   413,     0,
     414,   415,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   416,   417,   403,     0,   418,   419,   420,     0,
     421,   422,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     423,     0,     0,    78,  1301,     0,     0,     0,     0,     0,
     425,  1302,    81,   426,   427,   428,   429,   187,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   406,     0,    46,   407,    47,   408,   409,     0,
     410,   357,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   411,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   412,   413,     0,
     414,   415,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   416,   417,   403,     0,   418,   419,   420,     0,
     421,   422,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     423,     0,     0,    78,   835,     0,     0,     0,     0,     0,
     425,   489,    81,   426,   427,   428,   429,   187,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   406,     0,    46,   407,    47,   408,   409,     0,
     410,   357,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   411,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   412,   413,     0,
     414,   415,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   416,   417,   403,     0,   418,   419,   420,     0,
     421,   422,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     423,     0,     0,    78,   424,     0,     0,     0,     0,     0,
     425,    80,    81,   426,   427,   428,   429,   187,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   406,     0,    46,   407,    47,   408,   409,     0,
     410,   357,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   411,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   412,   413,     0,
     414,   415,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   416,   417,   403,     0,   418,   419,   420,     0,
     421,   422,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     423,     0,     0,    78,   835,     0,     0,     0,     0,     0,
     425,    80,    81,   426,   427,   428,   429,  2017,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,    -2,
       0,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,    -2,
       0,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,  2047,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,     0,     0,    -2,     0,  1549,    -2,     0,
       0,     0,     0,    -2,    -2,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,   406,     0,     0,   407,     0,   408,   409,     0,   410,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,    -2,     0,     0,    58,   411,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,    -2,    -2,     0,   412,   413,     0,   414,
     415,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   416,   417,   403,     0,   418,   419,   420,     0,   421,
     422,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   423,
       0,     0,    78,   424,     0,     0,     0,     0,     0,   425,
    1550,    81,   426,   427,   428,   429,   266,   187,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -499,  -499,     0,  -499,    46,     0,    47,     0,     0,  -499,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     151,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,   267,     0,     0,     0,  -826,     0,
       0,    80,    81,   266,   187,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -499,  -499,     0,
    -499,    46,     0,    47,     0,     0,  -499,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   151,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
      78,   267,     0,     0,     0,     0,     0,     0,    80,    81,
       4,   187,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,     0,     0,     0,     0,  -418,  -418,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -418,     0,     0,     0,    78,    79,     0,
       0,     0,     0,     0,     0,    80,    81,     4,   187,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,     0,     0,
       0,     0,  -419,  -419,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -419,     0,     0,     0,    78,    79,     0,  1454,     0,  1455,
       0,     0,    80,    81,  1456,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,  1457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1458,     0,     0,     0,    78,  1013,     0,  1454,     0,  1455,
       0,     0,    80,    81,  1456,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,  1457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1651,     0,     0,     0,    78,  1013,     0,  1454,     0,  1455,
       0,     0,    80,    81,  1456,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,  1457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1652,     0,     0,     0,    78,  1013,     0,  1454,     0,  1455,
       0,     0,    80,    81,  1456,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,  1457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1653,     0,     0,     0,    78,  1013,     0,     0,     0,     0,
       0,     0,    80,    81,   266,   187,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -499,  -499,
       0,  -499,    46,     0,    47,     0,     0,  -499,     0,   266,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,    58,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -499,  -499,     0,  -499,    46,     0,    47,
      63,    64,  -499,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   267,     0,     0,    63,    64,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   509,     0,     0,
       0,     0,     0,     0,    80,    81,   187,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     357,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   151,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
     615,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1108,
      77,  -692,    78,   670,     0,     0,     0,     0,     0,     0,
      80,    81,   187,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -499,  -499,     0,  -499,    46,
       0,    47,     0,     0,  -499,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   151,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,    77,     0,    78,   267,
       0,     0,     0,  -830,     0,     0,    80,    81,   187,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -499,  -499,     0,  -499,    46,     0,    47,     0,     0,
    -499,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   151,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,     0,    78,   267,     0,     0,     0,     0,
       0,     0,    80,    81,   187,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,   357,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,   615,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   669,     0,  -692,
      78,   670,     0,     0,     0,     0,     0,     0,    80,    81,
     187,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,   357,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,   615,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   804,     0,  -692,    78,   555,     0,     0,
       0,     0,     0,     0,    80,    81,   187,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     357,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
    1141,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -700,    78,   921,     0,     0,     0,     0,     0,     0,
      80,    81,   187,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,   357,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   358,    78,   359,
       0,     0,     0,     0,     0,     0,    80,    81,   187,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   357,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,  1621,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   921,     0,     0,     0,     0,
       0,     0,    80,    81,   187,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,   357,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,  1623,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   921,     0,     0,     0,     0,     0,     0,    80,    81,
     187,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,   357,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   509,     0,     0,
       0,     0,     0,     0,    80,    81,   187,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     357,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   921,     0,     0,     0,     0,     0,     0,
      80,    81,   187,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,   357,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   359,
       0,     0,     0,     0,     0,     0,    80,    81,   187,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -499,  -499,     0,  -499,    46,     0,    47,     0,     0,
    -499,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,     0,     0,     0,     0,     0,
    1478,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   406,     0,     0,   407,     0,   408,   409,     0,   410,
       0,     0,     0,     0,    78,   267,     0,     0,     0,     0,
       0,     0,    80,    81,  1219,     0,   411,  1221,     0,  1222,
    1942,  1943,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,
    1231,  1232,  1233,  1234,     0,    75,  1235,  1236,  1237,  1238,
    1239,  1240,  1241,     0,  1242,     0,   412,   413,     0,   515,
     415,  1243,  1244,    65,    66,    67,    68,    69,    70,    71,
      72,   416,   417,   403,  1245,   418,   419,   420,     0,   421,
     422,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,  1478,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1246,
       0,     0,    78,   424,     0,     0,     0,   308,     0,   425,
      80,    81,   426,   427,   428,   429,     0,   406,     0,     0,
     407,     0,   408,   409,  -192,   410,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1219,     0,   411,  1221,     0,  1222,     0,     0,  1223,  1224,
    1225,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,  1234,
       0,     0,  1235,  1236,  1237,  1238,  1239,  1240,  1241,     0,
    1242,     0,   412,   413,     0,   515,   415,  1243,  1244,    65,
      66,    67,    68,    69,    70,    71,    72,   416,   417,   403,
    1245,   418,   419,   420,     0,   421,   422,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1246,     0,     0,    78,   424,
       0,     0,     0,   308,     0,   425,    80,    81,   426,   427,
     428,   429,     0,     0,     0,     0,     0,     0,     0,     0,
    -192,   312,   187,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -422,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
       0,     0,     0,  -422,   312,   187,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -423,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,     0,     0,  -423,   312,   187,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,    14,
      15,    16,    17,    18,    19,   732,    20,   733,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    63,    64,   406,     0,    46,   407,    47,
     408,   409,     0,   410,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     411,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   734,     0,     0,     0,     0,  1234,     0,  -351,
       0,     0,     0,     0,    78,     0,     0,     0,     0,  -422,
     412,   413,     0,   414,   415,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   416,   417,   403,     0,   418,
     419,   420,     0,   421,   422,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1246,     0,     0,    78,   735,     0,     0,
       0,   308,     0,   425,    80,    81,   736,   737,   428,   429,
      14,    15,    16,    17,    18,    19,   732,    20,   733,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   406,     0,    46,   407,
      47,   408,   409,     0,   410,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   411,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   734,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   412,   413,     0,   414,   415,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   416,   417,   403,     0,
     418,   419,   420,     0,   421,   422,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   423,     0,     0,    78,   735,     0,
       0,     0,   308,     0,   425,    80,    81,   736,   737,   428,
     429,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   406,     0,    46,
     407,    47,   408,   409,     0,   410,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   411,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   412,   413,     0,   414,   415,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   416,   417,   403,
       0,   418,   419,   420,     0,   421,   422,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   423,     0,   454,    78,   455,
       0,     0,     0,     0,     0,   425,    80,    81,   426,   427,
     428,   429,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   406,     0,
      46,   407,    47,   408,   409,     0,   410,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   411,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   412,   413,     0,   414,   415,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   416,   417,
     403,     0,   418,   419,   420,     0,   421,   422,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   423,     0,     0,    78,
     455,     0,     0,     0,   308,     0,   425,    80,    81,   426,
     427,   428,   429,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   406,
       0,    46,   407,    47,   408,   409,     0,   410,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   411,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   412,   413,     0,   414,   415,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   416,
     417,   403,     0,   418,   419,   420,     0,   421,   422,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   423,     0,     0,
      78,   735,     0,     0,     0,   308,     0,   425,    80,    81,
     426,   427,   428,   429,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     406,     0,    46,   407,    47,   408,   409,     0,   410,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   411,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   412,   413,     0,   414,   415,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     416,   417,   403,     0,   418,   419,   420,     0,   421,   422,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   423,     0,
       0,    78,   455,     0,     0,     0,     0,     0,   425,    80,
      81,   426,   427,   428,   429,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   406,     0,    46,   407,    47,   408,   409,     0,   410,
     357,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   411,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   412,   413,     0,   414,
     415,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   416,   417,   403,     0,   418,   419,   420,     0,   421,
     422,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   423,
       0,     0,    78,   835,     0,     0,     0,     0,     0,   425,
      80,    81,   426,   427,   428,   429,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   406,     0,    46,   407,    47,   408,   409,     0,
     410,   357,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   411,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   412,   413,     0,
     414,   415,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   416,   417,   403,     0,   418,   419,   420,   406,
     421,   422,   407,     0,   408,   409,     0,   410,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,   411,     0,     0,     0,     0,     0,
     423,     0,     0,    78,   424,     0,     0,     0,     0,     0,
     425,    80,    81,   426,   427,   428,   429,     0,     0,     0,
       0,     0,     0,     0,   412,   413,     0,   414,   415,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   416,
     417,   403,     0,   418,   419,   420,     0,   421,   422,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1697,  1698,  1699,  1700,     0,     0,     0,   423,  1957,     0,
      78,   424,     0,     0,     0,     0,     0,   425,    80,    81,
     426,   427,   428,   429,   266,   187,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -499,  -499,
       0,  -499,    46,     0,    47,     0,     0,  -499,     0,   187,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,     0,   357,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   151,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   615,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -692,    78,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     151,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,    79,     0,     0,     0,  -828,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   151,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
      78,   209,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   151,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,     0,    78,    79,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   357,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   151,     0,   482,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   858,     0,     0,    78,   483,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   151,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
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
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   151,     0,   482,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   483,
       0,     0,     0,     0,     0,     0,    80,    81,   187,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   357,    49,    50,    51,    52,    53,    54,    55,
       0,     0,    14,    15,    16,    17,    18,    58,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -499,  -499,     0,  -499,
      46,     0,    47,    63,    64,  -499,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   615,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -692,    78,     0,   151,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,    78,
     509,     0,     0,     0,     0,     0,     0,    80,    81,   187,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,   357,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,  1212,     0,     0,     0,     0,   187,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,    78,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   357,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   858,     0,     0,    78,   483,
       0,     0,     0,     0,     0,     0,    80,    81,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,   357,    49,    50,    51,    52,    53,    54,
      55,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   858,     0,     0,    78,   483,     0,    63,    64,
       0,     0,     0,    80,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1020,    78,
    1013,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,  1568,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,  1013,     0,     0,
       0,     0,     0,     0,    80,    81,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   320,     0,    63,    64,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   209,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   357,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,     0,   357,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   359,     0,    63,    64,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   320,
       0,     0,     0,     0,     0,     0,    80,    81,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,   357,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   483,     0,     0,     0,
       0,     0,     0,    80,    81,   187,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -499,  -499,
       0,  -499,    46,     0,    47,     0,     0,  -499,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,     0,   357,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   509,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,  1013,     0,    63,    64,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,    80,    81,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,     0,   357,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   483,     0,    63,    64,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,  1013,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   357,    49,    50,    51,    52,    53,    54,    55,
       0,     0,    14,    15,    16,    17,    18,    58,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -499,  -499,     0,  -499,
      46,     0,    47,    63,    64,  -499,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,     0,     0,     0,    63,    64,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     337,     0,    14,    15,    16,    17,    18,    80,    81,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -499,  -499,     0,  -499,
      46,     0,    47,     0,     0,  -499,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,    58,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -499,  -499,     0,  -499,    46,     0,    47,    63,    64,
    -499,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     509,     0,     0,    63,    64,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,     0,     0,     0,     0,     0,
       0,     0,    80,    81,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   406,     0,    46,   407,    47,   408,   409,
       0,   410,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   411,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   412,   413,
       0,   414,   415,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   416,   417,   403,     0,   418,   419,   420,
       0,   421,   422,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   423,     0,     0,    78,   424,     0,     0,     0,     0,
       0,   425,   489,    81,   426,   427,   428,   429,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   406,     0,    46,
     407,    47,   408,   409,     0,   410,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   411,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   412,   413,     0,   414,   415,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   416,   417,   403,
       0,   418,   419,   420,     0,   421,   422,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   423,     0,     0,    78,   424,
       0,     0,     0,     0,     0,   425,    80,    81,   426,   427,
     428,   429,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   151,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,    78,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,   357,    49,    50,    51,
      52,    53,    54,    55,     0,     0,    14,    15,    16,    17,
      18,    58,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -499,  -499,     0,  -499,    46,     0,    47,    63,    64,  -499,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,   187,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,    78,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,   187,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,    58,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,   188,
     406,   189,   190,   407,     0,   408,   409,     0,   410,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,   411,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   723,     0,   724,   725,   412,   413,     0,   515,   415,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     416,   417,   403,     0,   418,   419,   420,   406,   421,   422,
     407,    75,   408,   409,     0,   410,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   411,     0,     0,     0,     0,     0,   423,    77,
       0,   516,   517,     0,     0,     0,   518,     0,   425,    80,
      81,   426,   427,   428,   429,     0,     0,     0,     0,     0,
       0,     0,   412,   413,     0,   414,   415,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   416,   417,   403,
       0,   418,   419,   420,   406,   421,   422,   407,     0,   408,
     409,     0,   410,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   411,
       0,     0,     0,     0,     0,   423,  1350,     0,    78,   424,
       0,     0,     0,  1351,     0,   425,    80,    81,   426,   427,
     428,   429,     0,     0,     0,     0,     0,     0,     0,   412,
     413,     0,   414,   415,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   416,   417,   403,     0,   418,   419,
     420,   406,   421,   422,   407,     0,   408,   409,     0,   410,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   411,     0,     0,     0,
       0,     0,   423,     0,     0,    78,   424,     0,     0,     0,
     518,     0,   425,    80,    81,   426,   427,   428,   429,     0,
       0,     0,     0,     0,     0,     0,   412,   413,     0,   414,
     415,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   416,   417,   403,     0,   418,   419,   420,   406,   421,
     422,   407,     0,   408,   409,     0,   410,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   411,     0,     0,     0,     0,     0,   423,
    1003,     0,    78,   424,     0,     0,     0,     0,     0,   425,
      80,    81,   426,   427,   428,   429,     0,     0,     0,     0,
       0,     0,     0,   412,   413,     0,   414,   415,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   416,   417,
     403,     0,   418,   419,   420,   406,   421,   422,   407,     0,
     408,   409,     0,   410,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     411,     0,     0,     0,     0,     0,   423,  1038,     0,    78,
     424,     0,     0,     0,     0,     0,   425,    80,    81,   426,
     427,   428,   429,     0,     0,     0,     0,     0,     0,     0,
     412,   413,     0,   414,   415,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   416,   417,   403,     0,   418,
     419,   420,   406,   421,   422,   407,     0,   408,   409,     0,
     410,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   411,     0,     0,
       0,     0,     0,   423,     0,     0,    78,   424,     0,     0,
       0,   308,     0,   425,    80,    81,   426,   427,   428,   429,
       0,     0,     0,     0,     0,     0,     0,   412,   413,     0,
     414,   415,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   416,   417,   403,     0,   418,   419,   420,   406,
     421,   422,   407,     0,   408,   409,     0,   410,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   411,     0,     0,     0,     0,     0,
     423,     0,     0,    78,   424,     0,     0,  1077,     0,     0,
     425,    80,    81,   426,   427,   428,   429,     0,     0,     0,
       0,     0,     0,     0,   412,   413,     0,   414,   415,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   416,
     417,   403,     0,   418,   419,   420,   406,   421,   422,   407,
       0,   408,   409,     0,   410,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   411,     0,     0,     0,     0,     0,   423,     0,     0,
      78,   424,     0,     0,     0,  1488,     0,   425,    80,    81,
     426,   427,   428,   429,     0,     0,     0,     0,     0,     0,
       0,   412,   413,     0,   414,   415,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   416,   417,   403,     0,
     418,   419,   420,   406,   421,   422,   407,     0,   408,   409,
       0,   410,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   411,     0,
       0,     0,     0,     0,   423,  1579,     0,    78,   424,     0,
       0,     0,     0,     0,   425,    80,    81,   426,   427,   428,
     429,     0,     0,     0,     0,     0,     0,     0,   412,   413,
       0,   414,   415,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   416,   417,   403,     0,   418,   419,   420,
     406,   421,   422,   407,     0,   408,   409,     0,   410,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   411,     0,     0,     0,     0,
       0,   423,     0,     0,    78,   424,     0,     0,     0,  1771,
       0,   425,    80,    81,   426,   427,   428,   429,     0,     0,
       0,     0,     0,     0,     0,   412,   413,     0,   414,   415,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     416,   417,   403,     0,   418,   419,   420,   406,   421,   422,
     407,     0,   408,   409,     0,   410,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   411,     0,     0,     0,     0,     0,   423,     0,
    1948,    78,   424,     0,     0,     0,     0,     0,   425,    80,
      81,   426,   427,   428,   429,     0,     0,     0,     0,     0,
       0,     0,   412,   413,     0,   414,   415,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   416,   417,   403,
       0,   418,   419,   420,   406,   421,   422,   407,     0,   408,
     409,     0,   410,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   411,
       0,     0,     0,     0,     0,   423,  1953,     0,    78,   424,
       0,     0,     0,     0,     0,   425,    80,    81,   426,   427,
     428,   429,     0,     0,     0,     0,     0,     0,     0,   412,
     413,     0,   414,   415,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   416,   417,   403,     0,   418,   419,
     420,     0,   421,   422,     0,     0,   406,     0,     0,   407,
      74,   408,   409,     0,   410,  2034,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   411,   423,  1963,     0,    78,   424,     0,     0,     0,
       0,     0,   425,    80,    81,   426,   427,   428,   429,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   412,   413,     0,   414,   415,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   416,   417,   403,     0,
     418,   419,   420,   406,   421,   422,   407,     0,   408,   409,
       0,   410,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   411,     0,
       0,     0,     0,     0,   423,     0,     0,    78,   424,     0,
       0,     0,     0,     0,   425,    80,    81,   426,   427,   428,
     429,     0,     0,     0,     0,     0,     0,     0,   412,   413,
       0,   414,   415,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   416,   417,   403,     0,   418,   419,   420,
     406,   421,   422,   407,     0,   408,   409,     0,   410,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   411,     0,     0,     0,     0,
       0,   423,  2041,     0,    78,   424,     0,     0,     0,     0,
       0,   425,    80,    81,   426,   427,   428,   429,     0,     0,
       0,     0,     0,     0,     0,   412,   413,     0,   414,   415,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     416,   417,   403,     0,   418,   419,   420,   406,   421,   422,
     407,     0,   408,   409,     0,   410,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   411,     0,     0,     0,     0,     0,   423,  2043,
       0,    78,   424,     0,     0,     0,     0,     0,   425,    80,
      81,   426,   427,   428,   429,     0,     0,     0,     0,     0,
       0,     0,   412,   413,     0,   414,   415,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   416,   417,   403,
       0,   418,   419,   420,   406,   421,   422,   407,     0,   408,
     409,     0,   410,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   411,
       0,     0,     0,     0,     0,   423,  2087,     0,    78,   424,
       0,     0,     0,     0,     0,   425,    80,    81,   426,   427,
     428,   429,     0,     0,     0,     0,     0,     0,     0,   412,
     413,     0,   414,   415,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   416,   417,   403,     0,   418,   419,
     420,   406,   421,   422,   407,     0,   408,   409,     0,   410,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   411,     0,     0,     0,
       0,     0,   423,  2089,     0,    78,   424,     0,     0,     0,
       0,     0,   425,    80,    81,   426,   427,   428,   429,     0,
       0,     0,     0,     0,     0,     0,   412,   413,     0,   414,
     415,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   416,   417,   403,     0,   418,   419,   420,   406,   421,
     422,   407,     0,   408,   409,     0,   410,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   411,     0,     0,     0,     0,     0,   423,
    2091,     0,    78,   424,     0,     0,     0,     0,     0,   425,
      80,    81,   426,   427,   428,   429,     0,     0,     0,     0,
       0,     0,     0,   412,   413,     0,   414,   415,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   416,   417,
     403,     0,   418,   419,   420,   406,   421,   422,   407,     0,
     408,   409,     0,   410,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     411,     0,     0,     0,     0,     0,   423,  2096,     0,    78,
     424,     0,     0,     0,     0,     0,   425,    80,    81,   426,
     427,   428,   429,     0,     0,     0,     0,     0,     0,     0,
     412,   413,     0,   414,   415,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   416,   417,   403,     0,   418,
     419,   420,   406,   421,   422,   407,     0,   408,   409,     0,
     410,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   411,     0,     0,
       0,     0,     0,   423,  2098,     0,    78,   424,     0,     0,
       0,     0,     0,   425,    80,    81,   426,   427,   428,   429,
       0,     0,     0,     0,     0,     0,     0,   412,   413,     0,
     414,   415,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   416,   417,   403,     0,   418,   419,   420,   406,
     421,   422,   407,     0,   408,   409,     0,   410,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   411,     0,     0,     0,     0,     0,
     423,  2137,     0,    78,   424,     0,     0,     0,     0,     0,
     425,    80,    81,   426,   427,   428,   429,     0,     0,     0,
       0,     0,     0,     0,   412,   413,     0,   414,   415,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   416,
     417,   403,     0,   418,   419,   420,   406,   421,   422,   407,
       0,   408,   409,     0,   410,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   411,     0,     0,     0,     0,     0,   423,  2139,     0,
      78,   424,     0,     0,     0,     0,     0,   425,    80,    81,
     426,   427,   428,   429,     0,     0,     0,     0,     0,     0,
       0,   412,   413,     0,   414,   415,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   416,   417,   403,     0,
     418,   419,   420,   406,   421,   422,   407,     0,   408,   409,
       0,   410,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   411,     0,
       0,     0,     0,     0,   423,  2141,     0,    78,   424,     0,
       0,     0,     0,     0,   425,    80,    81,   426,   427,   428,
     429,     0,     0,     0,     0,     0,     0,     0,   412,   413,
       0,   414,   415,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   416,   417,   403,     0,   418,   419,   420,
     406,   421,   422,   407,     0,   408,   409,     0,   410,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   411,     0,     0,     0,     0,
       0,   423,  2162,     0,    78,   424,     0,     0,     0,     0,
       0,   425,    80,    81,   426,   427,   428,   429,     0,     0,
       0,     0,     0,     0,     0,   412,   413,     0,   414,   415,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     416,   417,   403,     0,   418,   419,   420,   406,   421,   422,
     407,     0,   408,   409,     0,   410,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   411,     0,     0,     0,     0,     0,   423,  2164,
       0,    78,   424,     0,     0,     0,     0,     0,   425,    80,
      81,   426,   427,   428,   429,     0,     0,     0,     0,     0,
       0,     0,   412,   413,     0,   414,   415,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   416,   417,   403,
       0,   418,   419,   420,   406,   421,   422,   407,     0,   408,
     409,     0,   410,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   411,
       0,     0,     0,     0,     0,   423,  2166,     0,    78,   424,
       0,     0,     0,     0,     0,   425,    80,    81,   426,   427,
     428,   429,     0,     0,     0,     0,     0,     0,     0,   412,
     413,     0,   414,   415,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   416,   417,   403,     0,   418,   419,
     420,   406,   421,   422,   407,     0,   408,   409,     0,   410,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   411,     0,     0,     0,
       0,     0,   423,     0,     0,    78,   424,     0,     0,     0,
       0,     0,   425,    80,    81,   426,   427,   428,   429,     0,
       0,     0,     0,     0,     0,     0,   412,   413,     0,   414,
     415,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   416,   417,   403,     0,   418,   419,   420,   406,   421,
     422,   407,     0,   408,   409,     0,   410,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   411,     0,     0,     0,     0,     0,   712,
       0,     0,    78,   424,     0,     0,     0,     0,     0,   425,
      80,    81,   426,   427,   428,   429,     0,     0,     0,     0,
       0,     0,     0,   412,   413,     0,   414,   415,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   416,   417,
     403,     0,   418,   419,   420,   406,   421,   422,   407,     0,
     408,   409,     0,   410,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     411,     0,     0,     0,     0,     0,   715,     0,     0,    78,
     424,     0,     0,     0,     0,     0,   425,    80,    81,   426,
     427,   428,   429,     0,     0,     0,     0,     0,     0,     0,
     412,   413,     0,   414,   415,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   416,   417,   403,     0,   418,
     419,   420,   406,   421,   422,   407,     0,   408,   409,     0,
     410,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   411,     0,     0,
       0,     0,     0,   720,     0,     0,    78,   424,     0,     0,
       0,     0,     0,   425,    80,    81,   426,   427,   428,   429,
       0,     0,     0,     0,     0,     0,     0,   412,   413,     0,
     414,   415,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   416,   417,   403,     0,   418,   419,   420,     0,
     421,   422,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     729,     0,     0,    78,   424,     0,     0,     0,     0,     0,
     425,    80,    81,   426,   427,   428,   429,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -499,  -499,     0,  -499,    46,   406,    47,     0,   407,
    -499,   408,   409,     0,   410,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,   411,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   412,   413,     0,   414,   415,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   416,   417,   403,     0,
     418,   419,   420,     0,   421,   422,     0,     0,     0,     0,
     406,    75,    74,   407,     0,   408,   409,     0,   410,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   423,   411,     0,    78,   424,     0,
       0,     0,     0,     0,   425,   945,    81,   426,   427,   428,
     429,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   412,   413,     0,   414,   415,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     416,   417,   403,     0,   418,   419,   420,     0,   421,   422,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   423,     0,
       0,    78,   424,     0,     0,     0,     0,     0,   425,   489,
      81,   426,   427,   428,   429
};

static const yytype_int16 yycheck[] =
{
       1,    76,    76,     4,     1,   169,   265,   169,    76,   957,
      76,   181,   234,    59,   281,   673,   239,   529,     4,  1265,
     239,   169,   423,   682,   144,   758,   739,    99,   761,   140,
     852,     1,   854,   425,   575,   576,   239,     1,   260,   669,
     239,   171,    86,   208,     1,  1229,   225,   518,    78,  1246,
     669,  1052,   239,   805,   171,    56,    57,  1360,    59,  1297,
    1298,   239,    59,   989,   186,   362,   840,   669,  1202,   837,
     239,   997,   846,   185,  1199,    76,   307,  1814,   249,  1080,
      76,  1206,   837,   837,   942,    86,   239,   207,    99,    59,
    1814,    76,   837,    94,  1946,    59,   195,  1814,    99,   186,
       1,   102,    59,     4,   150,   106,   329,   278,  1942,   153,
     329,     1,   239,  1708,   839,  1369,    86,   866,   289,    10,
     106,   163,   197,   197,    76,    73,   329,   839,  1246,    99,
     329,   197,   102,   101,     1,   102,   106,   121,   249,   208,
     182,   239,   329,   588,   145,    85,     0,   148,  1149,   150,
      76,   329,     1,   150,   599,   156,    73,   126,    59,    73,
     329,  1286,   163,   704,   239,   239,    92,   278,   240,    76,
     171,   239,   930,   239,     0,    90,   329,   718,   289,    91,
     150,   839,   183,  1704,   376,   111,   150,    90,   380,   137,
     157,   160,   950,   150,   195,   196,   197,   182,   643,   496,
     163,   197,   329,   314,    76,   106,   207,   837,    73,   157,
      59,   212,   360,   161,   182,    78,    79,   157,   837,   182,
     137,    94,   223,   137,   268,     1,   196,   228,     4,   240,
     182,   329,   233,   234,   235,   837,   136,     1,   239,   240,
     157,  2075,   212,   239,   316,   157,   995,   161,   163,   150,
     165,   109,   110,   383,   329,   329,  2078,   106,   330,   260,
     390,   329,   165,   329,  1859,  1815,   383,   268,   165,   270,
     240,   812,   137,   303,   164,  2127,   167,    73,   279,   280,
     180,   172,   283,    59,  2106,   182,  1589,  1590,  1591,   290,
     197,   140,   559,    76,   338,    59,   163,   164,   268,   398,
     567,   150,   165,   304,   305,   316,   307,  2129,  2160,  1042,
      93,   312,  1211,   283,   537,   316,  1029,   975,   537,   330,
     617,     1,   714,  1448,     4,   197,   102,  1581,   329,   330,
     106,     1,   239,   564,   537,  1109,   157,  1089,   537,   340,
    1108,   137,   490,  1864,  1865,   524,   316,   348,   349,   157,
     537,   573,   353,  1108,  1108,    62,    63,   579,   759,   537,
     330,   157,  1220,  1108,   661,   161,   486,   239,   537,   242,
      99,   244,  2109,   181,   150,     0,   140,   494,   251,    59,
    1930,    20,   383,     1,   537,  2109,   150,   507,   685,    59,
    1115,   392,  2109,  1319,   395,   692,  1254,   398,  1516,   360,
     249,  1519,  1520,  1115,    78,    79,    86,   159,  1211,   640,
     537,   163,    73,   457,   637,   376,     1,   582,   637,   380,
     532,  1211,    59,   157,    73,    62,    63,   598,    65,   278,
    1564,   165,   102,   112,   637,   157,  1957,  1958,   637,   537,
     289,    59,   954,   121,   138,  1203,   109,   110,   208,   113,
     637,   324,   325,    10,   942,   157,   135,  1115,   165,   637,
     140,   157,   537,   537,   144,   314,   658,   468,   637,   318,
     150,   537,   136,   153,    59,   182,   137,   549,  1108,   173,
     150,   138,   927,   161,   637,   249,   516,   598,   137,  1108,
     354,   165,   493,   494,   659,   159,   157,   160,   468,   157,
     161,  2051,   248,   695,   505,   506,  1108,   283,   157,   255,
     637,   240,   161,   582,   278,   516,   173,   518,    73,  1703,
     157,   156,   140,   113,  1708,   289,   138,   207,   163,   490,
     394,   277,   150,   675,   682,  1309,   537,   151,   549,   637,
     392,   537,   288,   395,   165,  1742,   136,   182,   549,    76,
     314,   290,  1451,  1452,  1453,   140,   168,   169,    73,   173,
    2110,   182,   637,   637,    91,   150,   305,   639,  1317,   249,
     700,   637,   573,   804,   157,  1324,   159,   157,   579,   549,
     581,  1403,   137,  1584,  1406,  1407,   266,   316,   268,   820,
     659,    86,   157,   824,   154,    73,   642,  2147,   278,   156,
     360,   330,   157,   363,    99,   165,   161,   102,   162,   289,
     167,   106,  1083,   283,   163,   172,   376,    70,  1392,   165,
     380,   181,   137,   788,  1742,   163,   172,  1376,   639,  1555,
     537,   249,   312,   182,   314,   173,   637,   163,   639,   165,
     832,   642,   157,   644,  1303,   642,   161,   317,  1451,  1452,
    1453,   859,   653,   861,  1892,    62,   657,   163,   338,   137,
     278,  1451,  1452,  1453,   249,   537,   165,   964,   833,   639,
     878,   289,   642,   172,   786,  1859,   182,   156,   642,   157,
     131,   132,   904,   161,   163,   642,   161,    73,   689,   868,
     854,   166,   362,   278,   163,   157,   314,   658,   860,   841,
     107,   196,   164,   845,   289,   112,   163,   708,   115,   159,
     117,   161,   860,   182,   856,   857,    73,   212,   591,   788,
     159,   682,   839,   163,   163,   182,   177,   178,   850,   314,
     490,    73,  1220,   173,   695,   182,  1469,    73,   182,   947,
     158,   642,  1988,   865,   163,   240,   163,   165,   163,   598,
     751,   137,   753,   163,   755,  1296,   182,   787,   759,    73,
     163,   762,   163,   850,   833,   182,  1254,   182,   641,   156,
     962,   157,  1956,   268,   163,   161,   163,   457,   163,   182,
     137,   182,   163,   852,   163,  1969,   787,  1788,   283,  1790,
     159,   860,   173,   642,   163,   137,   163,   182,    73,     3,
     157,   137,  1750,   182,   161,     3,   486,  1556,   163,    13,
      14,    15,    16,    17,   163,   157,     3,   563,   173,   161,
     549,   157,   582,   137,   173,   161,   496,   507,   160,  1578,
     163,   832,   423,   157,   598,   163,   837,    73,   839,  1765,
     173,   165,    73,   157,   590,  1246,   159,   161,   172,  2033,
     851,   597,   157,     1,   161,   601,     4,   170,   171,   166,
     990,   862,   137,  1025,  1095,   272,   642,   868,  1099,    73,
     871,   832,    13,    14,    15,    16,    17,  1025,   642,   159,
    1351,  1148,   157,   135,   164,   181,   161,  1118,    13,    14,
      15,    16,    17,   165,  1125,   659,  1104,  1105,   658,   659,
     165,   137,   157,   904,  1024,   157,   137,   168,  1566,   161,
     639,    59,   319,   179,   175,   176,   168,   169,   598,   159,
      76,   157,   682,   157,   164,   161,   157,   161,    76,   181,
     161,   159,    73,   137,   157,   695,   164,   159,    86,   940,
     941,   942,  1173,    99,   159,   942,  1177,   617,    73,   164,
    1181,    99,  1160,   121,   102,  1077,  1025,   364,   106,   366,
     630,   368,   642,   159,  1713,   159,  1654,   953,   109,   110,
     164,  1659,   642,   468,    73,   109,   110,  1156,   942,  1491,
     598,   159,    73,    59,   159,   942,    62,    63,  1476,    65,
    1132,   162,  1480,  1481,   159,   996,   137,  1114,  1115,   159,
     156,   962,   150,   163,   159,   159,  1494,   414,   156,   163,
    1307,   157,   137,   598,   163,   685,   159,  1488,  1767,   160,
     163,   160,   161,   171,   642,   164,    47,    48,   788,    50,
     157,    73,   159,  1782,   161,    56,  1158,    22,   137,   159,
     160,   942,   150,   151,   152,   153,   137,  1048,   196,   197,
     157,  1052,   953,  1312,   549,  1456,   159,   642,   157,   207,
     163,   163,   161,   159,   212,   173,   157,   163,  1290,   833,
     161,   157,   832,   833,   182,   223,   135,   159,   234,  1080,
     157,   163,  1083,   239,   240,   233,   234,   235,   129,   130,
     159,   239,   240,   942,   163,   137,   157,   157,   157,   159,
     860,   161,   161,   102,   260,   157,  1577,  1108,   515,   168,
     169,   157,   260,  1114,  1115,   157,   170,   171,   135,   161,
     268,   712,   162,   163,   715,   162,   717,  1297,  1298,   720,
      13,    14,    15,    16,    17,   283,   133,   134,   729,   165,
     157,   732,   733,   734,   161,    90,   157,   165,  1149,  1661,
     161,   168,   169,  1902,   156,  1303,   304,  1906,   135,   307,
     316,   157,   182,   159,   312,   161,   942,  1211,   316,  1331,
     165,  1333,  1439,   329,   330,   159,   157,   953,   942,   163,
     157,   329,   330,   929,   161,  1333,  1316,   162,   595,   159,
      73,   168,   169,  1424,   157,   157,   159,  1428,   161,   161,
     157,  1432,   962,   172,   161,   965,   165,    13,    14,    15,
      16,    17,   159,   159,   181,   159,  1202,   163,     3,   163,
     157,   157,  1223,  1220,   161,  1226,  1227,  1228,    13,    14,
      15,    16,    17,   159,  1235,   383,   159,   163,   159,  1403,
     163,   121,   163,  1407,   159,  1409,   157,  1409,   163,   157,
    1372,  1984,   157,  1254,   137,   138,  1220,  1254,   157,  1260,
     163,  1409,   942,  1220,  1333,  1025,   159,    73,   162,   163,
     163,   159,   942,   953,  1275,   163,   174,  1278,  1279,   169,
    1281,   124,   167,   126,   127,   128,  1287,   179,    73,  1290,
    1254,  1413,  1414,  1279,   964,   159,   135,  1254,   160,   163,
    1771,  1202,   150,   151,   152,   153,   159,   159,  1278,  1279,
     163,   163,  1185,  1544,   157,   163,   161,   160,   161,  1220,
     468,   159,   165,   166,   942,   173,  1413,  1414,   159,   135,
     159,   137,   163,  1334,   182,   159,   159,  1406,   494,   163,
     163,   159,  1303,   159,  1024,   163,   494,   163,   162,   163,
    1351,   157,   137,  1254,   159,   161,   159,   942,   163,  1360,
     163,   159,   168,   169,   159,  1476,  1597,   159,   516,  1480,
     518,  1220,  2114,   159,  1862,  1606,  2118,  1539,  1279,  1610,
     157,   537,    13,    14,    15,    16,    17,   168,   169,   537,
    1462,  1539,  1393,   549,   766,   767,   768,   150,   151,   152,
     153,   549,  1651,  1652,  1653,  1254,   157,  1451,  1452,  1453,
     163,  1476,  1456,  1457,   162,  1480,   138,   573,   162,   163,
     173,   162,   163,   579,   138,   573,  1202,   162,   163,   182,
    1279,   579,   163,   581,   150,   151,   152,   153,  1560,  1208,
    1209,  1210,    73,  1666,  1220,   163,   164,  1666,  1673,  1674,
    1675,  1462,   773,   774,   775,   776,  1220,   173,   162,   163,
     163,  1462,   164,  1666,   164,  1466,  1467,  1666,  1454,   163,
    1539,   162,   163,  1560,    92,    93,   162,   163,  1254,  1666,
     157,   637,   181,   639,   162,   163,   159,  1488,  1666,   637,
    1254,   639,   162,   163,   642,   162,   163,  1666,   162,   163,
     162,   163,  1278,  1279,   162,   163,   137,   138,   159,   657,
     159,  1512,  1513,  1666,   114,   115,   116,   117,   118,  1476,
     159,  1522,  1202,  1480,  1481,   162,   163,   162,   163,   181,
    2018,  1211,   162,   163,   163,   164,   159,  1494,   159,  1666,
    1220,   689,   159,  1303,    13,    14,    15,    16,    17,    18,
    1220,   162,  1522,  1454,   162,   163,   159,  1558,   162,   163,
    2048,    13,    14,    15,    16,    17,    18,   161,  1666,    78,
      79,   163,   164,  1333,  1254,  1476,  1577,   159,  1564,  1480,
    1481,   165,  1654,  1584,  1254,   769,   770,  1659,  1589,  1590,
    1591,  2079,   411,  1494,  1361,  1362,  1668,   165,  1666,   771,
     772,   165,  1220,   751,   165,   753,  1829,   755,  1278,   165,
    1829,   777,   778,   159,   762,  1519,  1520,   436,   437,  1731,
      13,    14,    15,    16,    17,    18,  1829,  1476,   163,    71,
    1829,  1480,  1481,  1674,  1675,  1220,  1254,  1307,   457,   787,
     182,   162,  1829,  1654,   157,  1494,    79,   162,  1659,    18,
     181,  1829,   182,  1654,   165,  1246,   159,  1668,  1659,   165,
    1829,   159,   162,  1564,   165,  1666,   165,  1668,   162,  1254,
      18,   490,   162,  1895,   162,  1676,  1829,   156,  1454,   159,
      22,   837,   159,   839,   832,   159,   159,   159,   159,   837,
     165,   839,   159,   159,  1695,   159,   159,   159,   159,   159,
    1476,  1702,  1829,   162,  1480,  1481,  1692,   156,   156,   165,
     165,    71,  1476,   159,   159,   159,  1480,  1481,  1494,   223,
     868,   159,  1892,   871,    13,    14,    15,    16,    17,    18,
    1494,  1829,  1733,  1462,   107,   159,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,  1522,  1338,   904,   181,
     165,   163,   165,   159,   159,  2052,   904,   159,   159,   163,
    1832,   159,   162,   159,   163,   159,  1752,  1879,   163,   159,
    1771,  1451,  1452,  1453,  1454,  1455,  1456,  1457,   159,  1539,
     159,   159,   159,  1278,  1279,   159,   162,  1788,  1564,  1790,
     162,  1692,   940,   159,   159,   159,  1476,   159,   159,   162,
    1480,  1481,   159,   159,   159,   159,  1476,   159,   159,   182,
    1480,  1481,   159,   163,  1494,   156,    14,   163,   163,   163,
    1942,  1832,   156,   163,  1494,   157,   157,   157,  1829,  2051,
     157,  1832,   157,   157,   157,   157,  2067,   165,   164,   182,
    1841,  1842,   163,  1692,   164,   162,   162,  1848,   156,   182,
     156,  1752,  1522,   182,   165,  1942,   163,   159,  1476,   159,
    1861,   159,  1480,  1481,   159,   159,   181,  1939,   182,  1981,
    1871,   162,  1873,   159,   162,   159,  1494,   159,   163,   162,
     159,   159,   159,   156,  1564,  1886,  2109,  1888,  1889,  1890,
    2109,  1476,   162,  1894,  1895,  1480,  1481,   156,    81,     1,
    1048,   157,     4,   157,  1052,  1862,  2109,   182,   182,  1494,
    2109,   182,    93,   182,   182,  1691,  1692,   156,   182,   182,
     157,   157,  2109,    91,   159,  1654,   156,   156,  1939,   163,
    1659,  2109,  1080,  1977,   162,  1083,   163,   162,  1939,  1668,
    2109,   162,   162,   165,   156,  1946,   765,   156,   159,  1950,
     164,  2073,  1108,  2075,  1955,   124,  2109,    59,  1114,  1115,
    1108,  1862,   164,   156,   159,   159,  1114,  1115,   159,   156,
     162,   159,   162,   159,    76,   159,  1752,   159,   156,   182,
     157,   156,  2109,   159,    86,   164,  2073,   159,  2075,   163,
     157,   157,   162,    76,  2116,   159,   156,    99,   159,   162,
     102,  1149,  2114,   162,   106,  2049,  2118,  2119,  2009,   159,
     159,  2109,   516,  1862,   518,   159,  1977,   162,    76,   156,
    2021,  1691,   157,  2145,  2025,   182,   182,  1522,   182,  2116,
     159,   157,   162,  2034,  2109,  2109,   156,  2149,  2110,  2040,
     162,  2109,   144,  2109,   156,   156,   159,   161,   150,   159,
    2051,   153,  2053,   159,   156,   157,    76,    76,   173,   182,
    2172,  2018,    76,   182,  2176,  2187,   173,   169,  1844,   164,
     156,   156,  1752,   182,   156,  2147,   173,   158,   156,  2191,
     173,  2082,   107,   157,   164,   173,  1862,   163,  2049,   173,
     158,  2048,   194,   182,   196,   197,   162,    76,  1862,  2110,
    2187,   164,   159,  1832,   158,   207,   208,   182,  2109,  2110,
     212,   159,   159,   156,   156,   159,   157,  2018,   182,   159,
     182,  2122,  2079,   182,  1780,  1328,  2127,  1275,   738,   779,
    1278,  1279,   234,   780,  1290,   781,  2147,   239,   240,   136,
    2110,   782,  1290,   783,  1241,  1254,  2147,  2048,  2160,    13,
      14,    15,    16,    17,  2155,   456,  2075,  2158,   260,  2160,
    1480,  1870,  2106,  1494,  1862,  2064,   268,  1741,  2144,  2018,
    2049,  1724,  1724,  2119,  1844,  1281,  2176,  2147,  2079,  2048,
    2181,   283,  1862,    49,   115,  1939,  1334,   274,  2007,  1780,
    1457,  2192,  1862,  1321,  1770,  1275,   868,     0,  1528,  2048,
    2201,  1020,   653,  1351,  1752,   804,    18,  1026,   804,    73,
    1939,   708,  1360,   525,   316,   804,  1643,  1977,  1037,    -1,
     322,    13,    14,    15,    16,    17,   328,   329,   330,    -1,
    2079,    13,   229,    -1,    -1,    -1,   338,  1393,    -1,    -1,
      -1,    -1,  2018,    -1,  1862,  1393,    58,    59,    60,    61,
      62,    63,    64,    65,  2018,    -1,    -1,    -1,   360,   361,
     362,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,   135,  2048,   137,   376,    -1,    -1,  1862,   380,    -1,
      -1,    73,    -1,    -1,  2048,    -1,    -1,    -1,    -1,  2049,
      -1,    -1,    -1,   157,    -1,    -1,    -1,   161,    -1,    -1,
      -1,    -1,    -1,  2079,   168,   169,  1462,    89,    -1,    -1,
      -1,    -1,    -1,    -1,  1462,  2079,   313,    -1,    -1,    -1,
      -1,   423,    -1,    66,    -1,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,  2018,    -1,
    1488,    -1,    -1,   135,    -1,   137,    -1,    -1,  2018,    -1,
      -1,    -1,    -1,    -1,    -1,   457,    -1,    -1,   460,    -1,
      -1,    -1,    -1,    -1,   868,   157,   468,   871,  2048,   161,
      -1,    90,    -1,    -1,  1522,   157,   168,   169,  2048,    -1,
      -1,  2110,    86,    -1,   486,    -1,    -1,    -1,   490,    -1,
      -1,    -1,   494,    -1,   496,    -1,    -1,    -1,    -1,  2079,
    2018,  1220,    -1,    -1,    -1,   507,    -1,    -1,    -1,  2079,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,  2147,    -1,
      -1,    -1,    -1,    -1,   208,    -1,   423,   529,    -1,  1577,
    2048,    -1,    -1,  2018,    -1,   537,  1584,    -1,    -1,    -1,
      -1,  1589,  1590,  1591,    -1,    -1,   107,   549,    -1,   153,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   307,
      -1,  2079,   123,  2048,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   573,    -1,   575,   576,    -1,    -1,   579,    -1,    -1,
     582,    -1,    -1,    -1,  1303,    -1,    -1,    -1,    -1,    -1,
     194,    -1,    -1,    -1,  2079,    -1,    -1,   158,  1654,    -1,
     161,    -1,    -1,  1659,    -1,    -1,  1654,    -1,    -1,    -1,
    1666,  1659,  1668,    -1,    -1,    -1,    -1,    -1,  1666,    -1,
    1668,    -1,  1341,  1342,  1343,    -1,    -1,    -1,    -1,  1348,
    1349,    -1,    -1,    -1,    -1,   637,    -1,   639,    -1,    -1,
     642,    -1,    -1,    -1,  1048,    -1,    -1,    -1,  1052,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   658,   659,    -1,   661,
      -1,    13,    -1,    -1,   268,   562,    -1,   669,    -1,    -1,
      -1,   673,    -1,   570,    -1,    -1,  1080,    -1,   107,  1083,
     682,    -1,   111,   112,   113,   114,   115,   116,   117,   118,
     692,    -1,   589,   695,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   704,   600,    -1,    -1,    -1,   326,    -1,    -1,
     712,    -1,    -1,   715,    -1,   717,   718,    -1,   720,    -1,
      -1,    -1,    -1,  1771,    -1,    -1,    -1,   729,   157,   158,
     732,   733,   734,    -1,   338,    -1,    -1,    89,    -1,    58,
    1788,    -1,  1790,    -1,    -1,  1149,    -1,    66,    67,    68,
      69,    -1,    -1,    -1,    -1,   107,   360,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,  1829,    -1,    -1,  1832,    -1,    -1,    -1,
      -1,  1829,    -1,    -1,  1832,    -1,   788,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
       3,    -1,   804,   805,    -1,    -1,    -1,    -1,    -1,    -1,
     812,    -1,    -1,    -1,    -1,   712,   564,    -1,   715,    -1,
      -1,    -1,    -1,   720,    -1,    -1,    -1,    -1,    -1,    -1,
     832,   833,   729,    -1,    -1,   837,    -1,   839,    -1,  1895,
      -1,    -1,   161,    -1,    -1,    -1,    -1,  1895,    -1,    -1,
     852,   748,   854,   457,    -1,    -1,   858,   859,   860,   861,
     179,     4,     5,     6,     7,     8,     9,    10,    11,    12,
     489,    -1,   491,    -1,    -1,    -1,   878,    -1,    -1,    -1,
      -1,   500,    -1,  1939,    -1,    -1,   490,    -1,    -1,    -1,
      -1,  1939,   640,    -1,    -1,    -1,    -1,    -1,   582,    -1,
      -1,    -1,   904,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,   669,    -1,    -1,     1,   529,    -1,     4,    -1,    -1,
      -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     942,    -1,    -1,    -1,    -1,   947,    -1,  1351,    -1,    -1,
      -1,   953,   954,    -1,   157,   158,  1360,    -1,    -1,    -1,
     962,   164,   964,    -1,    -1,   168,   169,    -1,    -1,    -1,
      -1,   575,   576,   975,    -1,   659,    -1,    -1,    -1,    -1,
      -1,    -1,    59,   107,    -1,   109,  2034,   111,   112,   113,
     114,   115,   116,   117,   118,  2051,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2051,    -1,  2053,    79,    -1,    -1,    86,
      -1,    -1,    -1,   910,    -1,    -1,    -1,    -1,  1737,    -1,
      -1,    -1,  1024,  1025,    -1,    -1,    -1,   924,    -1,   106,
      -1,   928,    -1,    -1,   107,   932,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2109,  2110,    -1,   804,    -1,    -1,    -1,
      -1,  2109,  2110,   140,    -1,    -1,    -1,   144,    -1,    -1,
      -1,    -1,   820,   150,    -1,    -1,   824,    -1,   682,    -1,
      -1,    -1,    -1,    -1,  1488,    -1,    -1,  1089,    -1,   837,
      -1,  2147,   169,    -1,    -1,    -1,    -1,    -1,    -1,  2147,
     704,    -1,  1104,  1105,   788,    -1,  1108,    -1,   712,   182,
      -1,    -1,    -1,  1115,   718,    -1,   720,    -1,   107,   196,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     207,   208,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,  2192,    -1,    -1,    -1,    -1,   833,
      -1,    -1,    -1,  2201,    -1,    -1,    -1,    -1,  1160,    -1,
      -1,    -1,    -1,   240,    -1,    -1,    -1,    -1,   852,    -1,
     854,    -1,   249,  1577,   858,   859,    -1,   861,    -1,    -1,
    1584,    -1,    -1,   260,    -1,  1589,  1590,  1591,   265,   266,
     938,   268,    -1,   182,   878,    -1,    -1,   172,    -1,    -1,
    1202,   278,    -1,    -1,    -1,    -1,    -1,    -1,   812,  1211,
      -1,    -1,   289,    -1,    -1,   292,    -1,    -1,  1220,   296,
      -1,    -1,    -1,    -1,   301,    -1,    -1,    -1,    -1,    -1,
     307,   850,    -1,    -1,    -1,   312,    -1,   314,    -1,    -1,
      -1,   318,    -1,    -1,  1246,    -1,    -1,    -1,    -1,    -1,
      -1,   107,  1254,   330,   938,   111,   112,   113,   114,   115,
     116,   117,   118,   947,  1983,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,  1278,  1279,    -1,    -1,
      -1,    -1,    -1,   360,    -1,    -1,   363,    -1,  1290,    -1,
      -1,    -1,    -1,    -1,  1296,    -1,    -1,    -1,    -1,   376,
      -1,  1303,   158,   380,    -1,   161,    -1,   107,  1205,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,  1219,    -1,    -1,    -1,   945,   946,    -1,    -1,
      -1,  1333,    -1,    -1,    -1,   135,  1338,   137,    -1,    -1,
      -1,    -1,  1239,    18,    -1,    -1,    -1,  1095,    -1,  1246,
     954,  1099,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
    1108,   161,    -1,    -1,    -1,    -1,    -1,  1771,   168,   169,
    1118,    -1,    -1,    -1,    -1,    -1,    -1,  1125,    -1,    -1,
      -1,    -1,    -1,    -1,  1788,    -1,  1790,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1403,    -1,    -1,  1406,  1407,    -1,  1409,    -1,   486,
      -1,    -1,    -1,   490,    -1,    -1,    -1,    -1,    -1,    -1,
    1104,  1105,  1041,    -1,    -1,  1173,    -1,    -1,    -1,  1177,
     507,    -1,   107,  1181,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,  1451,
    1452,  1453,  1454,    -1,  1456,  1457,    -1,    -1,    -1,    -1,
    1462,  1463,   107,  1082,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,  1476,    -1,  1160,    -1,  1480,  1481,
      -1,    -1,    -1,    -1,    -1,    -1,   161,   564,    -1,  1491,
      -1,   107,  1494,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,   582,    -1,    -1,    -1,    -1,
      -1,    -1,  1131,    73,  1133,   160,    -1,    -1,    -1,    -1,
    1522,   598,    -1,    -1,    -1,    -1,  1145,   172,  1147,    -1,
      -1,    -1,    -1,  1152,  1153,    -1,    -1,  1539,    -1,    -1,
      -1,    -1,    -1,  1162,    -1,    -1,    -1,   107,    -1,   165,
      62,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,  1564,   640,  1566,   642,    -1,    -1,    -1,  1188,
      -1,    -1,  1191,  1470,  1471,   135,    -1,   137,    -1,    -1,
      -1,   658,   659,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     102,    -1,   669,    -1,    -1,    -1,   673,   157,   158,    -1,
      -1,    -1,   114,    -1,   116,   682,   118,    -1,   168,   169,
     687,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   695,    -1,
      -1,    -1,    -1,   107,  1521,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,  1254,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,  1654,  1655,    -1,    -1,    -1,  1659,    -1,  1661,
      -1,    -1,    -1,    -1,  1666,  1284,  1668,    -1,    -1,    -1,
      -1,    -1,  1291,    -1,  1293,  1294,  1424,   161,    -1,    -1,
    1428,    -1,    -1,  1302,  1432,  1304,    -1,  1306,    -1,  1691,
    1692,    -1,  1296,    -1,  1313,    73,    -1,   107,    -1,  1303,
     212,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,   788,    -1,   123,    -1,   125,    -1,    -1,    -1,  1403,
      -1,    -1,  1406,  1407,    -1,    -1,    -1,   804,   805,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,   820,    -1,    -1,    -1,   824,   158,    -1,
    1752,   161,    -1,    -1,    -1,   832,   833,   135,    -1,   137,
     837,    -1,   839,    -1,    -1,    -1,    -1,  1386,  1387,    -1,
      -1,   283,    -1,   285,   286,   852,    -1,   854,  1780,   157,
     158,   858,   859,   860,   861,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,  1412,    -1,    73,  1544,    -1,    -1,    -1,
    1419,   878,  1421,    -1,    -1,   317,    -1,    -1,    -1,    -1,
     322,    -1,  1814,  1815,    -1,    -1,   328,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,  1723,  1829,  1447,   107,
    1832,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,  1844,    -1,    -1,    -1,    -1,    -1,    -1,  1597,
     362,    -1,    -1,    -1,    62,   367,    -1,   369,  1606,   137,
    1862,   938,  1610,    -1,    -1,   942,    -1,    -1,    -1,    -1,
     947,    -1,    -1,    -1,    -1,    -1,   953,    73,    -1,   157,
     158,    -1,    -1,    -1,    -1,   962,    -1,  1491,   965,    -1,
      -1,    -1,    -1,  1895,   102,   972,    -1,    -1,    -1,    -1,
      -1,   250,   414,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,  1543,    -1,    -1,    -1,  1930,    -1,
      -1,  1550,    -1,  1552,    -1,    -1,    -1,  1939,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,  1024,  1025,   157,
      -1,    -1,    -1,    -1,    -1,    -1,   468,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    -1,  1977,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   496,    -1,   498,   499,    -1,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   515,   212,  2007,    -1,    -1,  1627,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2018,    -1,  1095,    -1,
      -1,    -1,  1099,    -1,    -1,    -1,    -1,  1104,  1105,    -1,
      -1,  1108,    -1,    -1,    -1,    -1,    -1,   549,    -1,    -1,
      -1,  1118,    -1,    -1,    -1,    -1,  2048,  2049,  1125,  2051,
    2052,   168,    -1,    -1,    -1,    -1,    -1,  1661,    -1,    -1,
      -1,   573,    -1,    -1,    -1,    -1,   578,    -1,   580,    -1,
      -1,    -1,    -1,    -1,    -1,   283,    -1,  2079,    -1,    -1,
      -1,    -1,    -1,  1160,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   603,    -1,   605,   606,    -1,  1173,    -1,    -1,    -1,
    1177,    -1,    -1,    -1,  1181,   617,    -1,  2109,  2110,   317,
      -1,   460,    -1,    -1,   322,    -1,    -1,    -1,   630,    -1,
     328,    -1,    -1,    -1,    -1,  1202,    -1,   639,   477,    -1,
      -1,   480,    -1,    -1,  1211,    -1,    -1,    -1,    -1,    -1,
      -1,  1760,  1761,  1220,    -1,  2147,    -1,    73,    -1,   661,
      -1,   663,   664,    -1,   362,  1774,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   685,   686,    -1,    -1,  1254,    -1,    -1,
     692,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   542,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,  1279,    -1,    -1,    -1,   414,    -1,   107,   135,
      -1,   137,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    73,    -1,    -1,    -1,    -1,  1303,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,  1312,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,  1331,   107,  1333,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2171,    -1,    -1,    -1,    -1,    86,
      -1,    -1,    -1,   135,    -1,   137,  2183,    -1,   496,    -1,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,   106,
      -1,    -1,    -1,    -1,    -1,   157,   158,   515,    -1,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,  2067,
      -1,    -1,    -1,    -1,    -1,    -1,  1403,    -1,    -1,  1406,
    1407,    -1,  1409,   140,    -1,    -1,    -1,   144,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,   153,  1424,    -1,    -1,
     157,  1428,    -1,    -1,    -1,  1432,    -1,    -1,    -1,    -1,
      -1,   168,   169,    -1,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   580,    -1,  1451,  1452,  1453,  1454,  1455,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   194,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   603,    73,    -1,    -1,  1476,
     207,   208,    -1,  1480,  1481,   212,    -1,    -1,    -1,   617,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1494,    -1,    -1,
      -1,    -1,   630,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,  2050,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   964,   661,    -1,   804,   805,    -1,   135,   266,
     137,   268,  1539,   975,    -1,    -1,   815,  1544,    -1,   818,
      -1,   278,   984,    -1,    -1,    -1,   283,   685,    -1,    -1,
     157,   158,   289,    -1,   692,    -1,    -1,  1564,  2107,    -1,
      -1,   168,   169,    -1,    -1,    -1,    -1,   304,    -1,    -1,
     307,    -1,    -1,    -1,    -1,   312,    -1,   314,    -1,  2128,
     317,   318,    -1,    -1,    -1,   322,    -1,    -1,    -1,    -1,
    1597,   328,    -1,    -1,  2143,    -1,    -1,    -1,    -1,  1606,
      -1,   338,    -1,  1610,    -1,   884,    13,    14,    15,    16,
      17,    -1,   891,    -1,    -1,    -1,   895,    -1,    -1,    -1,
     899,    -1,    -1,   360,    -1,   362,   363,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,   376,
      -1,    -1,    -1,   380,   107,    -1,    -1,  1089,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,    -1,    -1,
     123,  1668,   125,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,  1115,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    -1,  1692,   423,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,   158,    -1,    -1,   161,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     457,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,   102,    -1,    73,    -1,   106,
      -1,    -1,    -1,    -1,    -1,  1752,    -1,    -1,    -1,   486,
     157,   158,    -1,   490,    -1,    -1,    -1,    -1,    -1,   496,
      -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,   907,
     507,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   150,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,   529,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,  1815,   107,
    1089,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   157,   158,    -1,    -1,    -1,   964,   564,    -1,    -1,
       3,    -1,   168,   169,    -1,    -1,  1278,    -1,   575,   576,
      -1,   208,    -1,   580,    -1,   582,    -1,    -1,    -1,    -1,
      -1,    -1,  1859,    -1,    -1,  1862,    -1,    -1,    -1,    -1,
     158,   598,    -1,   161,    -1,  1307,    -1,    -1,    -1,    -1,
      -1,  1313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     617,    -1,    -1,    -1,    -1,  1164,    -1,    -1,  1167,    -1,
     107,    -1,  1171,   630,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   640,    -1,   642,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   283,    -1,    -1,    -1,
     657,   658,   659,    -1,   661,    -1,    13,    14,    15,    16,
      17,   107,   669,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   161,   682,    -1,    -1,   685,    -1,
      -1,    -1,   689,    -1,    -1,   692,    -1,    -1,   695,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   704,    -1,    -1,
    1977,    -1,    -1,    -1,    -1,   712,    -1,    -1,   715,    -1,
     717,   718,    -1,   720,   160,    -1,    73,    -1,    -1,    -1,
      -1,    -1,   729,    -1,    -1,   732,   733,   734,    -1,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,  2018,    -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,
     107,  1463,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,   209,    -1,    -1,    -1,
      -1,  2048,  2049,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,   788,   160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2067,    -1,    -1,    -1,   106,    -1,    -1,   804,    -1,    -1,
     157,   158,  2079,    -1,    -1,   812,    -1,    -1,    -1,    -1,
    1522,   168,   169,   820,    -1,    -1,    -1,   824,    -1,    13,
      14,    15,    16,    17,   267,   832,   833,    -1,   140,  1378,
     837,    -1,    -1,  2110,    -1,    -1,    -1,    -1,   150,    -1,
    1389,    -1,    -1,    -1,    -1,   852,    -1,   854,    -1,    -1,
      -1,   858,   859,   860,   861,    -1,    -1,   169,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   308,    -1,    -1,    -1,    -1,
    1278,   878,    -1,    -1,    -1,    -1,    -1,   320,   105,    73,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,   337,    -1,   208,    -1,    -1,  1307,
     907,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   107,    -1,    -1,   359,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,
     157,   938,    -1,   160,   161,   942,    -1,   249,    -1,    -1,
     947,   135,    -1,   137,    -1,   582,   953,   954,    -1,    -1,
      -1,   157,    -1,    -1,    -1,   962,    -1,   964,   965,    -1,
      -1,    -1,    -1,   157,   158,    -1,   278,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    -1,   289,    -1,  1691,
     292,   424,   107,    -1,    -1,   992,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   307,    -1,    -1,   123,    -1,
     125,    -1,   314,    -1,    -1,   642,   318,    -1,    -1,    -1,
      -1,    -1,   455,    -1,    -1,    -1,   102,  1024,  1025,    -1,
      -1,   107,   659,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   158,    -1,    -1,  1444,    -1,    -1,    -1,
     483,    -1,    -1,    -1,    -1,   488,    -1,    -1,   360,    -1,
      -1,   363,    -1,    -1,    -1,  1463,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   376,   508,   509,    -1,   380,    -1,
     513,   514,    -1,    -1,   517,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,  1095,    -1,
     533,    -1,  1099,    -1,    -1,    -1,    -1,  1104,  1105,    -1,
      -1,  1108,    -1,   135,    -1,    -1,  1655,    -1,    -1,    -1,
      -1,  1118,   555,    -1,  1522,    -1,    -1,    -1,  1125,    -1,
    1832,    -1,    -1,    -1,    -1,   157,   158,    -1,    -1,   161,
      -1,    -1,  1844,    -1,    -1,    -1,   168,   169,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   181,
      -1,   788,   107,  1160,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,  1173,    -1,    -1,    -1,
    1177,    -1,    -1,    -1,  1181,    -1,    -1,   107,   490,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     633,    -1,    -1,    -1,    -1,  1202,   833,    -1,    -1,    -1,
      -1,    -1,   157,   646,  1211,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1220,    -1,   852,    -1,   854,  1930,    -1,
      -1,   858,   859,    -1,   861,    -1,    -1,   670,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1246,
      -1,   878,    -1,    -1,    -1,    -1,    -1,  1254,    -1,  1961,
    1962,   107,   564,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,  1814,  1815,    -1,    -1,    -1,
     582,  1278,  1279,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,  1691,    -1,    -1,   598,    -1,    -1,  1296,
      -1,    -1,   735,    -1,    -1,    -1,  1303,    -1,    -1,    -1,
    1307,   157,   158,    -1,    -1,   942,    -1,    -1,   164,    -1,
     947,    -1,   168,   169,    -1,    -1,   953,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1331,    -1,  1333,    -1,   640,    -1,
     642,  1338,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2052,    -1,    -1,    -1,    -1,    -1,   658,   659,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   669,    -1,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     682,    -1,    -1,    -1,   154,    -1,    -1,    -1,    -1,    -1,
      -1,  1930,    -1,   695,   827,    -1,   829,    -1,    -1,    -1,
      -1,    -1,   835,    -1,    -1,    -1,  1403,    -1,    -1,  1406,
    1407,   181,  1409,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1424,    -1,    -1,
     863,  1428,    -1,    -1,    -1,  1432,    -1,    -1,    -1,   872,
      -1,    -1,    -1,   876,    -1,  2147,  1844,  1444,    -1,    -1,
      -1,    -1,    -1,    -1,  1451,  1452,  1453,  1454,  1455,  1456,
    1457,    -1,    -1,    -1,  2003,    -1,  1463,    -1,  2007,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1104,  1105,  1476,
      -1,    -1,     1,  1480,  1481,    -1,   788,    -1,   921,    -1,
      -1,    -1,    -1,   926,  1491,    -1,    -1,  1494,    -1,    -1,
      -1,    -1,   804,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2051,    -1,    -1,    -1,    -1,    -1,   820,    -1,
      -1,    -1,   824,    -1,    -1,  1522,    -1,    -1,    -1,    -1,
     832,   833,    -1,  1160,    -1,   837,    -1,    -1,    -1,    -1,
      59,    -1,  1539,    -1,    -1,    -1,    -1,  1544,    -1,    -1,
     852,    -1,   854,    -1,    -1,    -1,   858,   859,   860,   861,
    1557,    -1,    -1,    -1,    -1,    -1,    -1,  1564,    -1,    -1,
    2109,  2110,    -1,    -1,    -1,  1202,   878,    -1,    -1,    -1,
    1013,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,
      -1,    -1,    -1,  1220,    -1,    -1,    -1,    -1,    -1,    -1,
    1597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2147,  1606,
      -1,    -1,    -1,  1610,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   140,    -1,    -1,    -1,    -1,    -1,  1254,    -1,    -1,
      -1,   150,    -1,    -1,    -1,    -1,   938,    -1,    -1,    -1,
     942,    -1,    -1,    -1,    -1,   947,    -1,    -1,    -1,    -1,
     169,  1278,  1279,    -1,  2052,    -1,    -1,    -1,    -1,    -1,
     962,    -1,    -1,   965,  1661,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   208,
      -1,    -1,    -1,   135,  1691,  1692,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,   158,    -1,    -1,   161,
      -1,    -1,    -1,  1025,   135,    -1,   168,   169,    -1,    -1,
     249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1752,    -1,   168,   169,   278,
      -1,    -1,    -1,    -1,    -1,  1198,    -1,    -1,    -1,    -1,
     289,    -1,    -1,    -1,    -1,    -1,  1403,    -1,    -1,  1406,
    1407,    -1,    -1,  1780,    -1,    -1,    -1,    -1,   307,    -1,
      -1,    -1,    -1,  1095,    -1,   314,    -1,  1099,    -1,   318,
      -1,    -1,  1104,  1105,    -1,    -1,  1108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1247,    -1,  1118,    -1,    -1,    -1,
      -1,    -1,    -1,  1125,    -1,    -1,    -1,  1454,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   360,    -1,    -1,   363,    -1,    -1,  1844,    -1,  1476,
      -1,    -1,    -1,  1480,  1481,    -1,    -1,   376,  1160,    -1,
      -1,   380,  1859,    -1,    -1,  1862,    -1,  1494,  1301,    -1,
      -1,  1173,    -1,    -1,    -1,  1177,    -1,    -1,    48,  1181,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,  1325,    -1,    -1,  1522,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1220,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,  1564,    -1,    -1,
      -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1254,    -1,   124,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1960,  1961,    -1,    -1,   137,    -1,   139,
      -1,   490,    -1,    -1,    -1,    -1,    -1,  1279,    -1,    -1,
    1977,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     170,  1303,   172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2018,    -1,    -1,    -1,    -1,    -1,   197,   186,  1331,
      -1,  1333,    -1,    -1,    -1,    -1,    -1,  2034,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   564,    -1,  1223,    -1,    -1,
      -1,  2048,  2049,    -1,    -1,  2052,    -1,    -1,    -1,  1235,
      -1,    -1,    -1,   582,  1691,  1692,    -1,    -1,    -1,   239,
    2067,    -1,    -1,   243,    -1,    -1,   246,   247,    -1,   598,
     250,    -1,  2079,   253,   254,    -1,   256,    -1,   258,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1530,    -1,    -1,
      -1,  1403,    -1,    -1,  1406,  1407,    -1,  1409,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   640,  1424,   642,    -1,  1752,  1428,    -1,    -1,    -1,
    1432,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   658,
     659,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     669,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   329,
      -1,    -1,   332,   682,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1476,    -1,   695,    -1,  1480,  1481,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   357,    -1,    -1,
      -1,    -1,  1494,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   372,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1648,    -1,  1844,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1862,    -1,  1539,    -1,    -1,
      -1,    -1,  1544,    -1,    -1,    -1,    -1,    -1,   406,    -1,
     408,    -1,    -1,   411,   412,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   421,   422,    -1,    -1,    -1,    -1,   788,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   436,   437,
      -1,    -1,    -1,    -1,    -1,   804,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1597,    -1,    -1,    -1,   457,
      -1,   820,    -1,    -1,  1606,   824,    -1,   477,  1610,    -1,
      -1,    -1,    -1,   832,   833,    -1,    -1,    -1,   837,    -1,
    1753,  1754,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   490,   852,    -1,   854,  1512,  1513,    -1,   858,
     859,   860,   861,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   878,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   537,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
      -1,    -1,    -1,   553,    -1,    -1,    -1,    -1,    -1,    -1,
    1692,  2018,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   938,
      -1,  2048,    -1,   942,    -1,    -1,    49,    -1,   947,    52,
      -1,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   962,  1877,    -1,   965,    -1,    -1,    -1,
      -1,    74,  2079,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   637,    -1,    -1,
      -1,    -1,    -1,    -1,  1907,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,  1025,   677,   678,    -1,
      -1,  1944,   135,    -1,    -1,    -1,    -1,    -1,    -1,  1695,
      -1,    -1,    -1,    -1,    -1,    -1,  1702,   697,    -1,   699,
      -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,  1972,
      -1,    -1,    -1,  1976,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,  1733,    -1,    -1,
    1862,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1095,    -1,    -1,    -1,
    1099,    -1,    -1,    -1,    -1,  1104,  1105,    -1,    -1,  1108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1118,
      -1,    -1,    -1,    -1,    -1,    -1,  1125,   765,   766,   767,
     768,   769,   770,   771,   772,   773,   774,   775,   776,   777,
     778,   779,   780,   781,   782,   783,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   808,   809,
      -1,  1160,    -1,    -1,    -1,   815,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1173,    -1,    -1,    -1,  1177,    -1,
      -1,    -1,  1181,    -1,    -1,  1841,  1842,    -1,    -1,    -1,
     840,    -1,  1848,   843,   844,  1977,   846,    -1,   848,   849,
      -1,    -1,    -1,    -1,    -1,  1861,    -1,    -1,    -1,    -1,
      -1,    -1,   850,    -1,    -1,  1871,    -1,  1873,    -1,    -1,
      -1,  1220,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1886,    -1,  1888,  1889,  1890,    -1,  2018,    -1,    -1,    -1,
      -1,   891,    -1,    -1,    -1,   895,    -1,    -1,    -1,   899,
      -1,    -1,    -1,    -1,    -1,  1254,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2048,  2049,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1279,   169,    -1,    -1,    -1,  2067,    -1,    -1,    -1,    -1,
    1946,    -1,    -1,    -1,  1950,    -1,    -1,  2079,    -1,  1955,
      -1,    -1,   423,    -1,  1303,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   207,
     208,    -1,    -1,   973,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1331,    -1,  1333,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2009,    -1,   243,    -1,    -1,    -1,    -1,
      -1,    -1,   250,    -1,    -1,  2021,    -1,    -1,    -1,  2025,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1020,    -1,  2040,    -1,    -1,    -1,  1026,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1037,
      -1,    -1,    -1,    -1,  1403,    -1,    -1,  1406,  1407,    -1,
    1409,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1424,  2082,    -1,    -1,  1428,
      -1,    -1,    -1,  1432,    -1,    -1,    -1,    -1,    -1,  1077,
      -1,    -1,    -1,    -1,   332,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   575,   576,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1114,    -1,  2122,    -1,    -1,    -1,
      -1,  2127,   360,   361,    -1,    -1,    -1,  1476,    -1,    -1,
      -1,  1480,  1481,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   380,    -1,    -1,  1494,    -1,    -1,    -1,  2155,
      -1,    -1,  2158,    -1,  2160,    -1,    -1,    -1,    -1,  1159,
      -1,  1161,    -1,    -1,  1164,    -1,    -1,  1167,    -1,    -1,
      -1,  1171,    -1,    -1,    -1,  2181,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1539,    -1,    -1,    -1,    -1,  1544,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   460,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1220,    -1,    -1,    -1,    -1,    -1,    -1,   477,
     478,   712,   480,   481,   715,    -1,    -1,    -1,  1597,   720,
      -1,    -1,   490,    -1,    -1,    -1,   494,  1606,   729,    -1,
      -1,  1610,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   507,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   748,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     538,    -1,    -1,    -1,   542,    -1,    -1,    -1,    -1,  1309,
      -1,    -1,    -1,   784,    -1,  1303,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1692,   582,    -1,    -1,    48,    -1,    -1,
      -1,    -1,    -1,  1341,  1342,  1343,    -1,    -1,    -1,    -1,
    1348,  1349,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1378,    -1,
      -1,    -1,    -1,    -1,  1372,    -1,    -1,    -1,    -1,  1389,
      -1,    -1,  1392,    -1,  1394,  1395,    -1,    -1,    -1,    -1,
     638,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     658,   659,    -1,   124,    -1,  1413,  1414,    -1,    -1,    -1,
      -1,   669,    -1,    -1,    -1,   673,   137,    -1,   139,    -1,
      -1,    -1,   680,    -1,   682,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,   170,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,  1503,    53,    -1,    -1,    -1,    -1,    -1,
    1859,    -1,    -1,  1862,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   246,   247,    -1,    -1,   250,
     788,    -1,   253,   254,    -1,   256,    -1,   258,   107,    -1,
     109,   110,    -1,    -1,    -1,    -1,   804,   805,    -1,    -1,
      -1,    -1,  1560,    -1,    -1,    -1,    -1,   815,   816,    -1,
     818,   819,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,
      -1,    -1,    -1,    -1,   832,   833,    -1,    -1,    -1,   837,
      -1,   839,   840,    -1,    -1,  1605,    -1,    -1,   846,    -1,
      -1,    -1,    -1,    -1,   852,   164,   854,    -1,    -1,    -1,
     858,   859,   860,   861,    -1,    -1,    -1,    -1,  1977,    -1,
      -1,    86,    -1,  1633,    -1,    -1,    -1,    -1,    -1,    -1,
     878,    -1,   880,    -1,    -1,    -1,   884,   102,    -1,    -1,
      -1,    -1,    -1,   891,   892,    -1,   357,   895,   896,    -1,
      -1,   899,   900,    -1,    -1,    -1,  1666,    -1,   906,  2018,
      -1,   372,  1672,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,  2048,
    2049,    -1,   157,    -1,    -1,    -1,    -1,    -1,    -1,   947,
     948,    -1,    -1,    -1,   169,    -1,    -1,    -1,  2067,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2079,    -1,    -1,    -1,    -1,    -1,    -1,   975,    -1,   194,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1748,  1737,
      -1,    -1,    -1,   208,    -1,    -1,    -1,   212,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1246,   477,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1024,  1025,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1795,  1796,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   268,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1821,  1822,    -1,    -1,    -1,    -1,    -1,   283,  1829,
      -1,    -1,    -1,    -1,  1834,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1089,   553,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1104,  1105,    -1,    -1,
    1108,  1109,    -1,   328,    -1,    -1,    -1,  1115,    -1,    -1,
      -1,    -1,    -1,   338,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   360,    -1,   362,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1160,    -1,    -1,    -1,  1164,  1165,    -1,  1167,
    1168,    -1,  1932,  1171,  1172,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1942,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   423,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   677,   678,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1983,   697,    -1,   699,    -1,
      -1,    -1,   457,  2003,    -1,  1476,  1477,    -1,    -1,  1480,
    1481,    -1,    -1,    -1,    -1,  1486,    -1,    -1,    -1,  1490,
      -1,  1492,    -1,  1494,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   490,    -1,    -1,    -1,    -1,
      -1,   496,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1303,    -1,    -1,    -1,    -1,
      -1,  1309,  1310,    -1,   529,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2073,    -1,  2075,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1333,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   808,   809,  2109,
      -1,    -1,    -1,    -1,   815,    -1,    -1,    -1,    -1,    -1,
     575,   576,    -1,    -1,    -1,    -1,    -1,   582,  2116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   840,
    1378,  1379,   843,   844,    -1,   846,    -1,   848,   849,    -1,
      -1,  1389,  1390,    -1,  1392,    -1,    -1,  2145,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1403,    -1,    -1,  1406,  1407,
    1641,  1409,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     891,    -1,    -1,    -1,   895,    -1,    -1,    -1,   899,  2187,
      -1,    -1,    -1,    -1,   659,    -1,   661,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1687,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   682,    -1,    -1,
      -1,   169,    -1,    -1,    -1,    -1,  1707,  1708,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   704,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   712,    -1,    -1,
     715,    -1,   717,   718,    -1,   720,    -1,  1738,    -1,    -1,
     208,    -1,   973,    -1,   729,    -1,    -1,   732,   733,   734,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1539,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1566,    -1,
      -1,    -1,    -1,   788,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   812,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   307,
      -1,    -1,    -1,    -1,    -1,  1846,    -1,    -1,   833,    -1,
      -1,    -1,    -1,  1854,    -1,  1856,    -1,    -1,  1859,  1860,
      -1,  1862,    -1,    -1,    -1,    -1,  1867,   852,    -1,   854,
      -1,    -1,    -1,   858,   859,   860,   861,    -1,    -1,    -1,
      -1,    -1,    -1,  1114,    -1,    -1,    -1,  1655,    -1,    -1,
      -1,    -1,   360,   878,   362,   363,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1672,    -1,    -1,    -1,   376,    -1,
      -1,    -1,   380,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1159,    -1,
    1161,    -1,    -1,  1164,    -1,    -1,  1167,    -1,    -1,    -1,
    1171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1960,
      -1,    -1,   947,    -1,    -1,    -1,  1967,  1968,    -1,   954,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   964,
      -1,    13,    14,    15,    16,    17,  1987,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,   490,    -1,    56,  2026,    -1,  2028,   496,    -1,
    2031,  2032,    -1,    -1,    -1,    -1,    -1,  2038,  2039,    -1,
    1025,    73,    -1,    -1,    -1,    -1,  1814,  1815,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1309,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   564,    -1,    -1,    -1,
      -1,  2102,  2103,  2104,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   582,    -1,    -1,    -1,    -1,  1104,
    1105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2133,  2134,  2135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1378,    -1,   617,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1389,    -1,
      -1,  1392,  1930,  1394,  1395,    -1,    -1,    -1,    -1,    -1,
    1938,    -1,   640,    -1,    -1,  1160,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     658,   659,    -1,   661,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   669,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   682,    -1,    -1,   685,    -1,    -1,
      -1,    -1,    -1,    -1,   692,    -1,  1211,   695,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2003,  2004,    -1,    -1,  2007,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1503,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2051,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   169,    -1,  1278,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1296,    -1,    -1,    -1,    -1,    -1,    -1,  1303,    -1,
     788,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     208,    -1,    -1,    -1,    -1,    -1,   804,    -1,   169,    -1,
      -1,  2109,  2110,    -1,    -1,    -1,    -1,    -1,  1333,    -1,
      -1,    -1,   820,  1338,    -1,    -1,   824,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   832,   833,    -1,    -1,    -1,   837,
      -1,    -1,    -1,    -1,  1605,    -1,    -1,   208,    -1,  2147,
      -1,    -1,    -1,    -1,   852,    -1,   854,    -1,    -1,    -1,
     858,   859,   860,   861,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1633,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     878,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1403,    -1,
      -1,  1406,  1407,    -1,  1409,    -1,    -1,    -1,    -1,   307,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1451,  1452,  1453,    -1,
     938,  1456,  1457,    -1,    -1,    -1,   307,    -1,  1463,   947,
      -1,    -1,   360,    -1,   362,   363,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   962,    -1,   964,   965,   376,    -1,
      -1,    -1,   380,    -1,    -1,    -1,  1491,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1748,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   360,
      -1,   362,   363,    -1,    -1,    -1,    -1,  1522,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   376,    -1,    -1,    -1,   380,
      -1,    -1,    -1,    -1,  1539,    -1,    -1,  1025,    -1,    -1,
      -1,    -1,    -1,    -1,  1795,  1796,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1821,  1822,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1834,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   490,    -1,    -1,    -1,    -1,    -1,   496,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1095,    -1,    -1,
      -1,  1099,    -1,    -1,    -1,    -1,  1104,  1105,    -1,    -1,
    1108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1118,    -1,    -1,    -1,    -1,    -1,    -1,  1125,    -1,   490,
      -1,    -1,    -1,    -1,    -1,   496,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1661,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   564,    -1,    -1,    -1,
      -1,    -1,  1160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1932,    -1,    -1,   582,  1173,  1691,    -1,    -1,  1177,
      -1,    -1,    -1,  1181,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   564,    -1,    -1,    -1,    -1,    -1,   617,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   582,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   640,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2003,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     658,   659,    -1,   661,    -1,    -1,   617,    -1,    -1,    -1,
      -1,   669,    -1,    -1,    -1,  1780,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   682,    -1,    -1,   685,    -1,   640,
      -1,    -1,    -1,    -1,   692,    -1,    -1,   695,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   658,   659,    -1,
     661,    -1,    -1,    -1,    -1,  1303,    -1,    -1,   669,  1307,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   682,    -1,    -1,   685,    -1,    -1,    -1,    -1,  1844,
      -1,   692,    -1,  1331,   695,  1333,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
     788,    53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,   804,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,  1403,    -1,    -1,  1406,  1407,
      -1,  1409,   820,    -1,    -1,    -1,   824,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   832,   833,  1424,   788,    -1,   837,
    1428,    -1,    -1,    -1,  1432,    56,    57,   109,   110,    -1,
      -1,    -1,    -1,   804,   852,    -1,   854,    -1,    -1,    -1,
     858,   859,   860,   861,    -1,    -1,    -1,    -1,    -1,   820,
      -1,   194,  1977,   824,    -1,   137,    -1,    -1,    -1,    -1,
     878,   832,   833,    94,    -1,   208,   837,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,    -1,
     223,   852,   225,   854,    -1,    -1,    -1,   858,   859,   860,
     861,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   878,    -1,    -1,
      -1,    -1,    -1,    -1,   145,    -1,    -1,   148,    -1,    -1,
     938,    -1,    -1,    -1,  2049,    -1,    -1,  2052,    -1,   947,
      -1,  1539,   163,    -1,    -1,    -1,  1544,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   962,    -1,   964,   965,    -1,    -1,
      -1,    -1,   183,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   195,    -1,    -1,   938,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   947,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   327,    -1,    -1,    -1,    -1,  1597,
      -1,   962,    -1,   964,   965,    -1,    -1,   228,  1606,    -1,
      -1,    -1,  1610,    -1,    -1,    -1,    -1,  1025,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   270,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   279,   280,
      -1,    -1,    -1,    -1,  1025,    -1,    -1,    -1,    -1,   290,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   305,    -1,    -1,  1095,    -1,    -1,
      -1,  1099,    -1,    -1,    -1,    -1,  1104,  1105,    -1,    -1,
    1108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1118,    -1,    -1,    -1,    -1,    -1,    -1,  1125,    -1,   340,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   348,   349,    -1,
      -1,    -1,   353,    -1,  1095,    -1,    -1,    -1,  1099,    -1,
      -1,    -1,    -1,  1104,  1105,    -1,    -1,  1108,    -1,    -1,
      -1,    -1,  1160,    -1,    -1,    -1,    -1,  1118,    -1,    -1,
      -1,    -1,    -1,    -1,  1125,  1173,    -1,    -1,    -1,  1177,
      -1,   392,    -1,  1181,   395,    -1,    -1,   398,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   518,    -1,    -1,    -1,    -1,
      -1,   524,    -1,    -1,    -1,    -1,   529,    -1,    -1,  1160,
      -1,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,  1173,    -1,    -1,    -1,  1177,    18,    -1,    20,
    1181,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,    -1,    -1,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   493,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   505,   506,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1303,    -1,    -1,   631,  1307,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1331,    -1,  1333,   659,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   672,
      -1,    -1,  1303,    -1,    -1,    -1,  1307,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1331,    -1,  1333,    -1,    -1,    -1,   709,    -1,    -1,  1977,
      -1,   182,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   722,
      -1,    -1,    -1,    -1,    -1,  1403,    -1,    -1,  1406,  1407,
      -1,  1409,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   746,   747,    -1,  1424,   750,    -1,   752,
    1428,    -1,    -1,   644,  1432,   758,    -1,   760,   761,    -1,
      -1,    -1,   653,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1403,    -1,    -1,  1406,  1407,    -1,  1409,     5,
      -1,  2049,    -1,    -1,  2052,   788,    -1,    13,    14,    15,
      16,    17,    -1,  1424,    -1,    -1,    -1,  1428,   801,  2067,
      -1,  1432,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   812,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   708,    -1,    -1,
      -1,    -1,    -1,    49,    -1,   828,    52,    -1,    54,    55,
     833,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1539,   865,    -1,    -1,   868,  1544,    -1,   759,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   879,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,   907,    -1,    -1,    -1,  1539,   135,
      -1,   137,    -1,  1544,    -1,    -1,    -1,    -1,    -1,  1597,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1606,    -1,
      -1,   157,  1610,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,   954,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     851,   964,   965,    -1,    -1,    -1,  1597,    -1,    -1,   972,
      -1,   862,    -1,    -1,    -1,  1606,    -1,    -1,    -1,  1610,
       1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1025,    -1,    -1,    -1,    -1,    -1,    49,    -1,
    1033,    52,    -1,    54,    55,    -1,    57,    -1,    -1,  1042,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     941,    72,    -1,    74,    75,    -1,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
    1083,   102,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   996,   127,   128,    -1,     1,
      -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,   157,    -1,    -1,   160,
     161,    -1,    -1,    -1,   165,    -1,   167,   168,   169,   170,
     171,   172,   173,  1156,    -1,  1158,    -1,    49,    -1,    -1,
      52,   182,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    74,    75,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,    -1,
     102,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,    -1,   127,   128,  1240,  1241,    -1,
      -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   156,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,   165,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     182,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1977,
      -1,    -1,    -1,    -1,  1307,    -1,    -1,    -1,    -1,    -1,
    1313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1328,    -1,    -1,    -1,    -1,
    1333,    -1,    -1,    -1,    -1,  1226,  1227,  1228,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1977,    -1,  1351,    -1,
      -1,  1354,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1369,    -1,    -1,  1260,
      -1,  2049,    -1,    -1,  2052,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2067,
    1281,    -1,    -1,    -1,    -1,    -1,  1287,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2049,    -1,
      -1,  2052,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2067,    -1,    -1,    -1,
    1443,  1444,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1469,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1488,    -1,    -1,  1491,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1539,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1548,  1549,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
      55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1577,  1466,  1467,    72,  1581,    74,
      75,    -1,    77,    -1,    -1,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,    -1,   102,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,    -1,   127,   128,    -1,    -1,    49,    -1,    -1,    52,
     135,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1661,    -1,
      -1,    74,   157,    -1,  1667,   160,   161,  1558,    -1,    -1,
     165,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   182,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,    -1,  1728,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,   151,   152,
     153,    -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1771,    -1,
      -1,    -1,    -1,    -1,    -1,  1778,    -1,    -1,  1781,    -1,
      -1,    -1,    -1,    -1,    -1,  1676,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,  1807,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    55,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    -1,    72,    73,    74,    75,    -1,
      77,    -1,    -1,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,    -1,
     127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   182,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1984,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,  1894,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    55,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    70,    -1,    72,    73,    74,    75,    -1,    77,
      -1,    -1,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   182,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    55,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    70,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,   152,   153,    -1,    -1,    -1,   157,
     158,   159,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   182,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    55,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    70,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,   152,   153,    -1,    -1,    -1,   157,
     158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   182,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    70,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,   106,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   182,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    55,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    70,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,   159,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    55,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    -1,    -1,    73,    74,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    -1,
     127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
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
     167,   168,   169,   170,   171,   172,   173,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
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
     167,   168,   169,   170,   171,   172,   173,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
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
     167,   168,   169,   170,   171,   172,   173,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
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
     167,   168,   169,   170,   171,   172,   173,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
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
     167,   168,   169,   170,   171,   172,   173,     1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    -1,    56,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    78,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      -1,    56,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    70,    -1,     5,    73,    -1,
      -1,    -1,    -1,    78,    79,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   156,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,
      -1,   168,   169,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
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
      -1,    -1,    -1,    -1,    -1,   168,   169,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    70,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    78,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,    -1,    -1,   160,   161,    -1,     3,    -1,     5,
      -1,    -1,   168,   169,    10,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,    -1,    -1,   160,   161,    -1,     3,    -1,     5,
      -1,    -1,   168,   169,    10,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,    -1,    -1,   160,   161,    -1,     3,    -1,     5,
      -1,    -1,   168,   169,    10,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,    -1,    -1,   160,   161,    -1,     3,    -1,     5,
      -1,    -1,   168,   169,    10,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,     3,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    73,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
     109,   110,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,    -1,   109,   110,    -1,    -1,   168,
     169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
     138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     158,   159,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,
      -1,    -1,    -1,   165,    -1,    -1,   168,   169,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,   159,
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
      -1,   135,    -1,   137,   138,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,    -1,    -1,
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
      -1,   159,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
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
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,   138,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    73,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    72,    -1,    74,    75,    -1,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    -1,   137,    94,    95,    96,    97,
      98,    99,   100,    -1,   102,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    49,    -1,    -1,
      52,    -1,    54,    55,   182,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    74,    75,    -1,    77,    -1,    -1,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      -1,    -1,    94,    95,    96,    97,    98,    99,   100,    -1,
     102,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,   165,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     182,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,    -1,
      -1,    -1,    -1,   165,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,    -1,    -1,    -1,    -1,   165,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,   109,   110,    49,    -1,    51,    52,    53,
      54,    55,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      74,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    86,    -1,    -1,    -1,    -1,    91,    -1,    93,
      -1,    -1,    -1,    -1,   160,    -1,    -1,    -1,    -1,   165,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,   165,    -1,   167,   168,   169,   170,   171,   172,   173,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,
      -1,    -1,   165,    -1,   167,   168,   169,   170,   171,   172,
     173,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    55,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,
     160,   161,    -1,    -1,    -1,   165,    -1,   167,   168,   169,
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
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
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
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    55,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    49,
     127,   128,    52,    -1,    54,    55,    -1,    57,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,   151,   152,   153,    -1,    -1,    -1,   157,   158,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    73,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
     109,   110,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   107,    -1,   109,   110,   111,   112,   113,   114,
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
     157,   158,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
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
      -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,
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
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
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
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    13,    14,    15,    16,    17,    73,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,   109,   110,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   159,   160,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     4,
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
      -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   137,   138,    -1,    -1,    -1,    -1,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,   160,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    13,    14,    15,    16,    17,    18,    73,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,   109,   110,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,    -1,    -1,   109,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    13,    14,    15,    16,    17,    18,    73,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,   109,   110,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,    -1,    -1,   160,   161,    -1,   109,   110,
      -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
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
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    -1,   109,   110,    -1,    -1,
      -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    13,    14,    15,    16,    17,    18,    73,    20,    -1,
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
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    18,    73,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
     109,   110,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    13,
      14,    15,    16,    17,    18,    73,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,   109,   110,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,   109,   110,    -1,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
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
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    -1,   109,   110,    -1,    -1,
      -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    13,    14,    15,    16,    17,    73,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,   109,   110,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,    -1,    -1,    -1,   109,   110,
      -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,    -1,    13,    14,    15,    16,    17,   168,   169,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    73,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,   109,   110,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,    -1,    -1,   109,   110,    -1,    -1,   168,   169,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,   160,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    13,    14,    15,    16,
      17,    73,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,   109,   110,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,    -1,
      -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     137,    -1,    -1,    -1,    -1,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,   160,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    73,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   107,
      49,   109,   110,    52,    -1,    54,    55,    -1,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,   109,   110,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    49,   127,   128,
      52,   137,    54,    55,    -1,    57,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,   168,
     169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    49,   127,   128,    52,    -1,    54,
      55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,
      -1,    -1,    -1,   165,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    49,   127,   128,    52,    -1,    54,    55,    -1,    57,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,
      -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,
     165,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
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
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,   165,    -1,   167,   168,   169,   170,   171,   172,   173,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    49,
     127,   128,    52,    -1,    54,    55,    -1,    57,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    -1,    -1,   164,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    49,   127,   128,    52,
      -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,
     160,   161,    -1,    -1,    -1,   165,    -1,   167,   168,   169,
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
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    49,   127,   128,
      52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
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
     125,    -1,   127,   128,    -1,    -1,    49,    -1,    -1,    52,
     135,    54,    55,    -1,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,   157,   158,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    49,   127,   128,    52,    -1,    54,    55,
      -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,
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
      -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,
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
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    -1,
     127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    49,    53,    -1,    52,
      56,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      49,   137,   135,    52,    -1,    54,    55,    -1,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    74,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
     169,   170,   171,   172,   173
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
     168,   169,   182,   187,   188,   189,   205,   297,   298,   299,
     300,   301,   302,   303,   304,   305,   306,   307,   308,   311,
     314,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   327,   329,   330,   331,   333,   334,   338,   339,   340,
     341,   342,   344,   350,   351,   352,   353,   364,   369,   402,
     405,   415,   421,   423,   429,   433,   438,   439,   440,   441,
     442,   443,   444,   445,   471,   489,   490,   491,   492,     0,
     184,   107,   188,   205,   301,   303,   314,   317,   320,   330,
     334,   339,   121,   157,    59,    62,    63,    65,   157,   157,
     367,   427,   428,   429,   326,   327,   109,   110,   188,   190,
     403,   404,   190,   157,   415,   157,   157,     4,   107,   109,
     110,   318,   323,   324,   157,   157,   205,   428,   433,   439,
     440,   441,   443,   444,   445,   109,   341,   162,   184,   161,
     304,   314,   317,   438,   442,   488,   489,   492,   493,   182,
     182,   185,   154,   165,   181,   226,   385,    90,   163,   422,
     102,   190,   426,   163,   163,   163,   182,   109,   110,   157,
     205,   309,   310,   433,   434,   435,   436,   437,   438,   442,
     446,   447,   448,   449,   450,   451,   452,   453,   454,   460,
       3,    47,    48,    50,    56,   332,     3,   161,   205,   303,
     304,   318,   322,   324,   335,   340,   418,   438,   442,   492,
      70,   301,   303,   317,   330,   334,   339,   419,   438,   442,
      66,   323,   323,   318,   324,   312,   323,   324,   332,   351,
     318,   323,   318,   160,   427,   163,   185,   157,   165,   234,
     427,   427,     3,   292,   293,   308,   311,   317,   321,   322,
     161,   314,   317,   490,   190,   190,   415,   181,   317,   157,
     205,   424,   433,   434,   438,   447,   451,   161,   205,   304,
     492,   416,   417,    58,    66,    67,    68,    69,   161,   179,
     190,   391,   393,   397,   399,   400,   340,    58,   159,   161,
     205,   313,   317,   321,   329,   330,   336,   337,   338,   339,
     343,   350,   351,   369,   379,   381,   471,   484,   485,   486,
     487,   492,   493,   427,   109,   110,   172,   188,   340,   368,
     460,   429,   157,   398,   399,   157,    13,    89,   157,   190,
     430,   431,   432,   121,   191,   192,    49,    52,    54,    55,
      57,    74,   104,   105,   107,   108,   119,   120,   123,   124,
     125,   127,   128,   157,   161,   167,   170,   171,   172,   173,
     186,   187,   191,   193,   196,   204,   205,   206,   207,   210,
     211,   212,   213,   214,   215,   216,   217,   218,   219,   220,
     221,   222,   228,   340,   159,   161,   204,   205,   221,   223,
     314,   340,   383,   384,   401,   488,   493,   430,   317,   439,
     440,   441,   443,   444,   445,   159,   159,   159,   159,   159,
     159,   159,   109,   161,   188,   314,   471,   490,   161,   168,
     205,   223,   303,   304,   313,   315,   317,   330,   337,   339,
     376,   377,   378,   380,   381,   484,   492,   162,   157,   161,
     438,   442,   492,   157,   163,   107,   160,   161,   165,   187,
     189,   223,   386,   387,   388,   389,   390,    22,   386,   157,
     190,   234,   157,   157,   188,   424,   188,   428,   433,   435,
     436,   437,   446,   448,   449,   450,   452,   453,   454,   317,
     434,   447,   451,   163,   426,   161,   427,   468,   471,   426,
     427,   427,   422,   292,   157,   427,   468,   426,   427,   427,
     422,   427,   427,   317,   424,   157,   157,   316,   317,   314,
     317,   162,   184,   314,   488,   493,   426,   342,   165,   422,
     292,   190,   190,   385,   303,   322,   420,   438,   442,   165,
     422,   292,   403,   317,   330,   317,   317,   109,   341,   109,
     110,   188,   340,   345,   403,   138,   188,   317,   373,   374,
     378,   379,   382,   156,   184,   234,   308,   182,   438,   451,
     317,   184,   426,   157,   426,   185,   223,   428,   433,   317,
     157,   190,   413,   165,   157,   190,   165,   190,   138,   168,
     169,   396,   159,   163,   190,   400,   159,   427,   162,   184,
     315,   317,   330,   337,   339,   483,   484,   492,   493,   157,
     161,   169,   181,   205,   471,   473,   474,   475,   476,   477,
     478,   495,   205,   343,   492,   317,   337,   323,   318,   427,
     159,   315,   317,   485,   315,   471,   485,   188,   368,   460,
     365,   165,   368,   391,   181,   391,   430,   159,   163,   157,
     159,   121,   157,   204,   157,   157,   204,   157,   157,   207,
     157,   204,   157,   107,   109,   110,   318,   323,   324,   157,
     204,   204,    19,    21,    86,   161,   170,   171,   208,   209,
     223,   230,   234,   353,   383,   492,   163,   184,   157,   193,
     161,   166,   161,   166,   124,   126,   127,   128,   157,   160,
     161,   165,   166,   207,   207,   174,   168,   175,   176,   170,
     171,   129,   130,   131,   132,   177,   178,   133,   134,   169,
     167,   179,   135,   136,   180,   159,   163,   160,   184,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     181,   225,   226,   227,   157,   205,   464,   465,   466,   467,
     468,   159,   163,   159,   159,   159,   159,   159,   159,   159,
     157,   427,   468,   471,   157,   468,   471,   157,   184,   157,
     314,   490,   162,   184,   185,   161,   185,   157,   169,   205,
     433,   455,   456,   457,   458,   459,   460,   461,   462,   463,
     138,   492,   163,   185,   163,   185,   190,   190,   157,   184,
     184,   184,   184,   161,   189,   184,   387,   164,   163,   494,
     386,   160,   161,   164,   390,   401,   157,   191,   184,   181,
     433,   435,   436,   437,   446,   448,   449,   450,   452,   453,
     454,   159,   159,   159,   159,   159,   159,   159,   159,   159,
     159,   434,   447,   451,   427,   181,   162,   184,   385,   234,
     422,   373,   385,   234,   424,   230,   384,   230,   384,   424,
     109,   161,   413,   234,   422,   426,   165,   165,   422,   292,
     413,   234,   422,   347,   348,   346,   165,   159,   163,   159,
     163,    71,   294,   295,   182,   168,   223,   184,   433,   415,
     413,   190,   162,   184,   157,   395,   393,   394,    79,   328,
     188,   315,   471,   485,   317,   321,   492,   373,   474,   475,
     476,   162,   184,    18,   223,   317,   473,   495,   427,   427,
     471,   315,   483,   493,   317,   188,   315,   485,   427,   165,
     427,   368,    10,   167,   368,   370,   371,   165,   159,   384,
     159,   159,   431,   158,   197,   198,   199,   223,   182,   383,
     493,   193,   383,   161,   383,   384,   383,   493,   223,   383,
     159,   383,   383,   383,   162,   184,   159,   170,   171,   209,
      18,   319,   159,   163,   159,   168,   169,   159,   158,   223,
     229,   223,   165,   223,   188,   223,   188,   119,   161,   188,
     197,   119,   161,   190,   353,   223,   197,   188,   207,   210,
     210,   210,   211,   211,   212,   212,   213,   213,   213,   213,
     214,   214,   215,   216,   217,   218,   219,   164,   230,   191,
     161,   188,   223,   165,   223,   373,   465,   466,   467,   317,
     464,   427,   427,   223,   384,   157,   427,   468,   471,   157,
     468,   471,   373,   373,   184,   184,   162,   162,   157,   433,
     456,   457,   458,   461,    18,   317,   455,   459,   157,   427,
     477,   495,   427,   427,   495,   157,   427,   477,   427,   427,
     185,   222,   190,   377,   380,   162,   380,   381,   162,   495,
     495,   138,   375,   376,   377,   375,   377,   375,   190,   184,
     221,   222,   223,   425,   494,   386,   388,   156,   184,   159,
     184,   159,   375,   223,   159,   159,   159,   159,   159,   159,
     159,   159,   159,   157,   427,   468,   471,   157,   427,   468,
     471,   157,   427,   468,   471,   424,    22,   471,   223,   324,
     340,   469,   234,   159,   159,   159,   159,   159,   411,   412,
     234,   156,   184,   413,   234,   422,   412,   234,   165,   165,
     165,   354,   138,   378,   379,   188,   190,   296,    18,    72,
      74,    75,    77,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    94,    95,    96,    97,    98,
      99,   100,   102,   109,   110,   122,   157,   161,   190,   230,
     231,   232,   233,   234,   235,   236,   238,   239,   248,   255,
     256,   257,   258,   259,   260,   265,   266,   269,   270,   271,
     272,   273,   274,   275,   281,   282,   283,   297,   317,   321,
     423,    71,   185,   185,   375,   414,   412,   159,   301,   303,
     314,   406,   407,   408,   409,   401,   181,   392,   392,   315,
     485,   161,   168,   205,   223,   340,   223,   317,   159,   159,
     159,   159,     5,   317,   427,   473,   366,   370,   368,   165,
     340,   163,   494,   190,   370,   165,   159,   159,   163,   159,
     159,   163,   159,   184,   163,   159,   159,   159,   163,   159,
     207,   159,   159,   159,   207,    18,   319,   223,   159,   159,
     158,   165,   207,   162,   163,   185,   197,   162,   162,   119,
     123,   125,   189,   200,   201,   202,   159,   200,   162,   163,
     156,   221,   164,   159,   200,   185,   387,   159,   159,   159,
     159,   464,   373,   373,   159,   159,   375,   375,   461,   159,
     159,   159,   159,   157,   433,   460,   455,   459,   373,   373,
     162,   185,   495,   163,   185,   159,   163,   163,   185,   163,
     185,   385,   200,   138,   173,   185,   185,   156,   386,   223,
     427,   375,   427,   185,   157,   427,   468,   471,   157,   427,
     468,   471,   157,   427,   468,   471,   373,   373,   373,   426,
     151,   173,   185,   470,   163,   185,   414,   406,   412,   234,
     414,   354,   354,   354,     3,     5,    10,    74,   156,   298,
     305,   306,   314,   317,   355,   360,   488,   163,   182,   157,
      62,    63,   182,   234,   297,   423,   157,   157,    18,   232,
     157,   157,   182,   190,   182,   190,   168,   190,   165,   231,
     157,   157,   157,   232,   157,   234,   223,   224,   224,    14,
     284,   260,   271,   164,   182,   185,   236,    79,   182,   190,
      92,    93,   264,   268,   113,   136,   263,   112,   135,   267,
     263,   382,   317,   296,   162,   162,   185,   414,   190,   424,
     185,   182,   185,   182,   185,   159,   384,   398,   398,   184,
     185,   185,   185,   223,   157,   427,   477,   471,   316,     5,
     168,   185,   223,   368,   494,   165,   370,    10,   371,   156,
     181,   372,   494,   156,   184,   199,   313,   188,    79,   194,
     195,   383,   207,   207,   207,   207,   207,   165,   387,   158,
     223,   163,   156,   203,   161,   201,   203,   203,   162,   163,
     126,   160,   162,   229,   221,   162,   494,   157,   427,   468,
     471,   159,   159,   185,   185,   159,   157,   427,   468,   471,
     157,   427,   477,   433,   427,   427,   159,   159,   162,   380,
     162,   138,   377,   138,   159,   159,   185,   222,   222,   162,
     162,   185,   185,   159,   373,   373,   373,   159,   159,   159,
     385,   163,   223,   223,   324,   340,   162,   156,   185,   414,
     156,   156,   156,   156,   314,   314,   353,   361,   488,   314,
     360,   157,   349,   182,   182,   182,   157,   164,   205,   356,
     357,   363,   433,   434,   447,   451,   163,   182,   190,   190,
     197,   182,   234,   182,   234,   230,   240,   297,   299,   302,
     308,   317,   321,   230,    81,   159,   240,   150,   151,   152,
     153,   158,   159,   182,   230,   249,   250,   252,   297,   182,
     182,   230,   182,   387,   182,   230,   401,   230,   249,   114,
     115,   116,   117,   118,   276,   278,   279,   182,   101,   182,
      85,   157,   159,   427,   156,   182,   182,   157,   157,   232,
     232,   260,   157,   270,   260,   270,   234,   182,   159,   156,
     396,   156,   184,   163,   163,   162,   162,   162,   185,   373,
     223,   223,   185,   162,   185,   165,   156,   370,   494,   340,
     190,   165,   222,   156,   406,   472,   473,   159,   164,   159,
     163,   164,   387,   494,   229,   124,   200,   201,   161,   201,
     161,   201,   162,   156,   373,   159,   159,   373,   373,   162,
     185,   159,   427,   159,   159,   159,   230,   470,   156,   156,
     349,   349,   349,   356,   157,   205,   358,   359,   468,   479,
     480,   481,   482,   182,   163,   182,   356,   182,   401,   428,
     433,   223,   317,   156,   163,   182,   362,   363,   362,   362,
     190,   159,   159,   230,   317,   159,   157,   232,   159,   150,
     151,   152,   153,   173,   182,   253,   254,   232,   231,   182,
     254,   159,   164,   230,   158,   230,   231,   252,   182,   494,
     159,   159,   159,   159,   234,   278,   279,   157,   223,   157,
     191,     1,   232,   207,   261,   230,    76,   111,   262,   264,
      76,   427,   392,   407,   184,   184,   162,   159,   185,   185,
     162,   162,   370,   494,   156,   372,   387,   185,   159,   223,
     195,   223,   494,   156,   162,   162,   200,   200,   159,   427,
     427,   159,   159,   162,   162,   223,   182,   480,   481,   482,
     317,   479,   163,   182,   427,   427,   182,   159,   433,   427,
     232,   232,    78,    79,   165,   243,   244,   245,   159,   230,
      76,   232,   230,   158,   230,    76,   182,   158,   230,   231,
     252,   317,   339,   158,   230,   232,   250,   254,   254,   182,
     230,   156,   165,   245,   232,   232,   157,   184,   182,   191,
     159,   164,   159,   163,   164,   159,   232,   157,   232,   232,
     232,   398,   190,   424,   162,   162,   494,   156,   494,   156,
     156,   162,   162,   159,   159,   159,   479,   427,   357,    76,
       1,   222,   241,   242,   425,     1,   164,     1,   184,   232,
     243,    76,   182,   159,   232,    76,   182,   173,   173,   232,
     231,   254,   254,   182,    58,   230,   251,   340,   173,   173,
      76,   158,   230,   158,   230,   231,   182,     1,   184,   184,
     280,   315,   317,   488,   164,   182,   161,   191,   285,   286,
     287,   207,   197,   230,   263,   156,   156,   157,   427,   468,
     471,   359,   232,   138,     1,   163,   164,   156,   290,   291,
     297,   232,    76,   182,   232,   230,   158,   158,   230,   158,
     230,   158,   230,   231,   188,   340,   158,   230,   158,   230,
     232,   173,   173,   173,   173,   156,   290,   280,   185,   157,
     205,   424,   479,   188,   164,   107,   157,   159,   164,   163,
     159,   159,    76,   259,   373,   222,   241,   244,   246,   247,
     297,   232,   173,   173,   173,   173,   158,   158,   230,   158,
     230,   158,   230,   246,   185,   182,   277,   317,   285,   162,
     222,   182,   285,   287,   232,    76,   159,   232,   237,   185,
     244,   158,   158,   230,   158,   230,   158,   230,   185,   277,
     221,   159,   164,   191,   159,   159,   164,   232,     1,   232,
     156,   237,   156,   159,   234,   191,   288,   157,   182,   288,
     234,   163,   164,   222,   159,   191,   188,   289,   159,   182,
     159,   163,   182,   188
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
     197,   197,   198,   198,   199,   199,   200,   200,   201,   201,
     201,   201,   201,   201,   201,   202,   202,   202,   203,   203,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   205,   205,
     205,   206,   206,   206,   206,   207,   207,   207,   207,   207,
     207,   207,   207,   207,   208,   208,   208,   208,   209,   209,
     210,   210,   211,   211,   211,   211,   212,   212,   212,   213,
     213,   213,   214,   214,   214,   214,   214,   215,   215,   215,
     216,   216,   217,   217,   218,   218,   219,   219,   220,   220,
     221,   221,   221,   222,   223,   223,   223,   224,   224,   225,
     225,   226,   226,   227,   227,   227,   227,   227,   227,   227,
     227,   227,   227,   227,   228,   228,   229,   229,   229,   229,
     230,   230,   231,   231,   232,   232,   232,   232,   232,   232,
     232,   232,   232,   232,   232,   232,   232,   232,   232,   232,
     233,   233,   234,   234,   235,   235,   236,   236,   236,   236,
     236,   237,   237,   237,   238,   239,   239,   239,   239,   239,
     239,   239,   239,   240,   240,   240,   240,   241,   241,   241,
     242,   242,   243,   243,   243,   243,   243,   244,   244,   245,
     246,   246,   247,   247,   248,   248,   248,   248,   248,   248,
     248,   248,   248,   248,   248,   248,   249,   249,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   251,   251,   251,   252,   252,   252,   252,
     253,   253,   253,   254,   254,   254,   255,   255,   255,   255,
     255,   255,   255,   255,   255,   255,   255,   255,   255,   255,
     255,   255,   255,   255,   255,   255,   256,   256,   257,   258,
     259,   260,   260,   261,   261,   262,   263,   263,   264,   264,
     265,   265,   265,   265,   265,   265,   266,   267,   267,   268,
     269,   269,   270,   270,   271,   271,   271,   272,   273,   274,
     275,   275,   275,   276,   276,   277,   277,   278,   278,   278,
     278,   279,   280,   280,   280,   280,   280,   281,   282,   282,
     283,   283,   283,   283,   283,   284,   284,   285,   285,   286,
     286,   287,   287,   288,   288,   288,   289,   289,   290,   290,
     291,   291,   292,   292,   293,   293,   294,   294,   295,   295,
     296,   296,   297,   297,   297,   298,   298,   299,   299,   299,
     299,   299,   300,   300,   300,   301,   301,   301,   301,   301,
     302,   302,   302,   302,   302,   303,   303,   303,   303,   304,
     304,   305,   305,   305,   306,   306,   306,   306,   306,   307,
     307,   308,   308,   308,   308,   309,   309,   309,   309,   309,
     310,   310,   311,   311,   311,   311,   312,   312,   312,   313,
     313,   313,   314,   314,   314,   315,   315,   315,   316,   316,
     317,   317,   318,   318,   319,   319,   319,   319,   319,   320,
     321,   321,   321,   322,   322,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   324,   325,   325,   325,   325,   325,
     325,   325,   325,   325,   325,   325,   325,   325,   325,   325,
     325,   325,   325,   325,   325,   325,   325,   325,   325,   325,
     325,   325,   325,   326,   326,   327,   328,   328,   329,   329,
     329,   329,   329,   330,   330,   331,   331,   331,   331,   332,
     332,   332,   332,   332,   332,   333,   333,   333,   333,   334,
     335,   334,   334,   336,   336,   336,   336,   337,   337,   337,
     338,   338,   338,   338,   339,   339,   339,   340,   340,   340,
     340,   340,   340,   341,   341,   341,   342,   342,   343,   343,
     345,   344,   346,   344,   347,   344,   348,   344,   344,   349,
     349,   350,   350,   351,   351,   352,   352,   352,   353,   353,
     353,   353,   353,   353,   353,   353,   354,   354,   355,   355,
     355,   355,   355,   355,   355,   355,   355,   355,   355,   355,
     356,   356,   356,   357,   357,   357,   357,   358,   358,   358,
     359,   360,   360,   361,   361,   362,   362,   363,   364,   364,
     365,   364,   364,   366,   364,   364,   364,   367,   367,   368,
     368,   369,   369,   370,   370,   370,   370,   371,   371,   372,
     372,   372,   373,   373,   373,   373,   374,   374,   374,   374,
     375,   375,   375,   375,   375,   375,   375,   376,   376,   376,
     376,   377,   377,   378,   378,   379,   379,   380,   380,   380,
     380,   380,   381,   381,   381,   381,   381,   382,   382,   383,
     383,   383,   384,   384,   385,   385,   385,   385,   386,   386,
     387,   387,   387,   387,   387,   388,   388,   389,   389,   390,
     390,   390,   390,   390,   391,   391,   392,   392,   394,   393,
     395,   393,   393,   393,   393,   396,   396,   396,   396,   397,
     397,   397,   397,   398,   398,   399,   399,   400,   400,   401,
     401,   401,   401,   402,   402,   402,   403,   403,   404,   404,
     405,   405,   405,   405,   406,   406,   407,   407,   408,   408,
     408,   409,   409,   410,   410,   411,   411,   412,   412,   413,
     414,   415,   415,   415,   415,   415,   415,   415,   415,   415,
     415,   415,   416,   415,   417,   415,   418,   415,   419,   415,
     420,   415,   415,   421,   421,   421,   422,   422,   423,   423,
     423,   423,   423,   423,   423,   423,   423,   423,   424,   424,
     424,   424,   425,   426,   426,   427,   427,   428,   428,   429,
     429,   429,   430,   430,   431,   431,   431,   432,   432,   432,
     433,   433,   434,   434,   434,   434,   435,   435,   435,   435,
     436,   436,   436,   436,   436,   436,   436,   437,   437,   437,
     437,   438,   438,   438,   439,   439,   439,   439,   439,   440,
     440,   440,   440,   441,   441,   441,   441,   441,   441,   442,
     442,   442,   443,   443,   443,   443,   443,   444,   444,   444,
     444,   445,   445,   445,   445,   445,   445,   446,   446,   447,
     447,   447,   447,   448,   448,   448,   448,   449,   449,   449,
     449,   449,   449,   449,   450,   450,   450,   450,   451,   451,
     451,   452,   452,   452,   452,   452,   453,   453,   453,   453,
     454,   454,   454,   454,   454,   454,   455,   455,   455,   455,
     455,   456,   456,   456,   457,   457,   457,   457,   458,   458,
     458,   459,   459,   459,   459,   459,   460,   460,   461,   461,
     461,   462,   462,   463,   463,   464,   464,   464,   465,   465,
     465,   465,   465,   466,   466,   466,   466,   467,   467,   467,
     468,   468,   468,   468,   468,   469,   469,   469,   469,   469,
     469,   470,   470,   471,   471,   471,   471,   472,   472,   473,
     473,   473,   473,   474,   474,   474,   474,   474,   475,   475,
     475,   475,   476,   476,   476,   477,   477,   477,   478,   478,
     478,   478,   478,   478,   479,   479,   479,   480,   480,   480,
     480,   480,   481,   481,   481,   481,   482,   482,   483,   483,
     483,   484,   484,   485,   485,   485,   485,   485,   485,   486,
     486,   486,   486,   486,   486,   486,   486,   486,   486,   487,
     487,   487,   487,   488,   488,   488,   489,   489,   490,   490,
     490,   490,   490,   490,   491,   491,   491,   491,   491,   491,
     492,   492,   492,   493,   493,   493,   494,   494,   495,   495
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
       2,     4,     4,     4,     6,     4,     2,     4,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     5,     5,     4,
       5,     5,     5,     4,     2,     2,     3,     3,     1,     1,
       1,     3,     1,     3,     3,     3,     1,     3,     3,     1,
       3,     3,     1,     3,     3,     3,     3,     1,     3,     3,
       1,     3,     1,     3,     1,     3,     1,     3,     1,     3,
       1,     5,     4,     1,     1,     3,     6,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     7,     1,     1,     3,     3,
       1,     3,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     4,     2,     6,     1,     2,     1,     2,     1,     2,
       1,     1,     2,     2,     2,     5,     7,     5,    10,     7,
       5,    10,     7,     1,     1,     1,     2,     1,     3,     1,
       1,     3,     2,     3,     3,     2,     2,     1,     2,     2,
       0,     1,     2,     3,     4,     6,     5,     7,     6,     7,
       7,     8,     4,     6,     5,     7,     1,     3,     4,     5,
       4,     3,     5,     1,     2,     3,     3,     3,     5,     5,
       5,     5,     3,     5,     5,     5,     3,     4,     5,     5,
       5,     5,     7,     7,     7,     7,     7,     7,     7,     2,
       3,     4,     4,     4,     6,     6,     6,     6,     6,     6,
       6,     3,     4,     1,     2,     2,     1,     1,     1,     1,
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
       1,     3,     2,     2,     2,     6,     4,     1,     1,     1,
       1,     1,     2,     3,     6,     3,     3,     4,     2,     3,
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
       4,     4,     4,     3,     2,     2,     3,     3,     2,     2,
       0,     1,     4,     1,     2,     2,     2,     0,     1,     4,
       1,     2,     3,     1,     2,     0,     1,     2,     7,     8,
       0,     9,     8,     0,    11,    10,     1,     2,     3,     0,
       1,     3,     3,     3,     2,     5,     4,     1,     1,     0,
       2,     5,     0,     1,     1,     3,     1,     1,     3,     3,
       0,     1,     1,     1,     3,     3,     3,     1,     3,     3,
       5,     1,     3,     3,     3,     2,     3,     1,     3,     3,
       4,     1,     1,     1,     1,     2,     1,     1,     3,     1,
       1,     2,     1,     1,     0,     2,     2,     4,     1,     4,
       0,     1,     2,     3,     4,     2,     2,     1,     2,     2,
       5,     5,     7,     6,     1,     3,     0,     2,     0,     5,
       0,     5,     3,     1,     8,     0,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     1,     2,     5,     6,     1,
       1,     3,     3,     2,     3,     3,     2,     4,     1,     4,
       7,     5,    10,     8,     1,     4,     2,     2,     1,     1,
       5,     2,     5,     0,     1,     3,     4,     0,     1,     0,
       0,     1,     1,     2,     2,     2,     2,     2,     2,     1,
       2,     5,     0,     6,     0,     8,     0,     7,     0,     7,
       0,     8,     1,     1,     2,     3,     0,     5,     3,     4,
       4,     4,     4,     5,     5,     5,     5,     6,     1,     1,
       1,     1,     3,     0,     5,     0,     1,     1,     2,     6,
       4,     4,     1,     3,     0,     1,     4,     1,     1,     1,
       1,     3,     2,     1,     2,     2,     2,     3,     4,     5,
       2,     4,     5,     4,     5,     3,     4,     6,     7,     3,
       4,     2,     1,     2,     4,     6,     7,     3,     4,     2,
       3,     4,     5,     4,     5,     4,     5,     3,     4,     1,
       1,     1,     4,     6,     7,     3,     4,     2,     3,     3,
       4,     4,     5,     4,     5,     3,     4,     1,     3,     2,
       1,     2,     2,     2,     3,     4,     5,     2,     4,     5,
       4,     5,     3,     4,     6,     7,     3,     4,     2,     1,
       2,     4,     6,     7,     3,     4,     2,     3,     4,     5,
       4,     5,     4,     5,     3,     4,     2,     4,     1,     2,
       2,     2,     3,     4,     2,     4,     4,     3,     4,     6,
       3,     2,     4,     1,     2,     2,     1,     1,     2,     3,
       4,     2,     4,     4,     6,     1,     2,     2,     1,     2,
       2,     3,     4,     1,     4,     4,     3,     3,     6,     3,
       2,     3,     7,     5,     1,     1,     1,     3,     3,     3,
       5,     1,     1,     5,     5,     6,     6,     0,     1,     1,
       3,     2,     2,     1,     2,     2,     3,     4,     1,     4,
       4,     3,     3,     6,     3,     1,     2,     1,     2,     6,
       5,     6,     7,     7,     1,     2,     2,     1,     2,     2,
       3,     4,     1,     4,     4,     3,     6,     3,     1,     1,
       2,     1,     1,     2,     3,     2,     3,     2,     3,     3,
       2,     4,     3,     2,     3,     2,     4,     3,     2,     6,
       6,     6,     7,     1,     2,     1,     1,     1,     2,     3,
       2,     3,     2,     3,     3,     4,     2,     3,     4,     2,
       5,     6,     7,     5,     6,     6,     0,     1,     0,     2
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
#line 8535 "Parser/parser.cc"
    break;

  case 3:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8541 "Parser/parser.cc"
    break;

  case 4:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8547 "Parser/parser.cc"
    break;

  case 5:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8553 "Parser/parser.cc"
    break;

  case 6:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8559 "Parser/parser.cc"
    break;

  case 7:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8565 "Parser/parser.cc"
    break;

  case 8:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8571 "Parser/parser.cc"
    break;

  case 20:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8577 "Parser/parser.cc"
    break;

  case 24:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8583 "Parser/parser.cc"
    break;

  case 25:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8589 "Parser/parser.cc"
    break;

  case 26:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8599 "Parser/parser.cc"
    break;

  case 27:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8605 "Parser/parser.cc"
    break;

  case 28:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8611 "Parser/parser.cc"
    break;

  case 29:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8617 "Parser/parser.cc"
    break;

  case 31:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8623 "Parser/parser.cc"
    break;

  case 32:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8629 "Parser/parser.cc"
    break;

  case 33:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8635 "Parser/parser.cc"
    break;

  case 34:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8641 "Parser/parser.cc"
    break;

  case 35:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8651 "Parser/parser.cc"
    break;

  case 36:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8657 "Parser/parser.cc"
    break;

  case 37:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8663 "Parser/parser.cc"
    break;

  case 38:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8669 "Parser/parser.cc"
    break;

  case 39:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8675 "Parser/parser.cc"
    break;

  case 40:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8681 "Parser/parser.cc"
    break;

  case 41:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8687 "Parser/parser.cc"
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
#line 8699 "Parser/parser.cc"
    break;

  case 44:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8708 "Parser/parser.cc"
    break;

  case 45:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8714 "Parser/parser.cc"
    break;

  case 47:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8720 "Parser/parser.cc"
    break;

  case 48:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8726 "Parser/parser.cc"
    break;

  case 49:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8732 "Parser/parser.cc"
    break;

  case 50:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8738 "Parser/parser.cc"
    break;

  case 51:
#line 786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8748 "Parser/parser.cc"
    break;

  case 52:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8754 "Parser/parser.cc"
    break;

  case 53:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8761 "Parser/parser.cc"
    break;

  case 54:
#line 798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8767 "Parser/parser.cc"
    break;

  case 55:
#line 800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8773 "Parser/parser.cc"
    break;

  case 56:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8779 "Parser/parser.cc"
    break;

  case 57:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8785 "Parser/parser.cc"
    break;

  case 58:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8791 "Parser/parser.cc"
    break;

  case 59:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8797 "Parser/parser.cc"
    break;

  case 60:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8803 "Parser/parser.cc"
    break;

  case 61:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8809 "Parser/parser.cc"
    break;

  case 62:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8815 "Parser/parser.cc"
    break;

  case 63:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8821 "Parser/parser.cc"
    break;

  case 64:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8827 "Parser/parser.cc"
    break;

  case 65:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8833 "Parser/parser.cc"
    break;

  case 66:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8839 "Parser/parser.cc"
    break;

  case 67:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8845 "Parser/parser.cc"
    break;

  case 68:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8851 "Parser/parser.cc"
    break;

  case 69:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8861 "Parser/parser.cc"
    break;

  case 70:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8867 "Parser/parser.cc"
    break;

  case 73:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8873 "Parser/parser.cc"
    break;

  case 74:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8879 "Parser/parser.cc"
    break;

  case 77:
#line 875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8885 "Parser/parser.cc"
    break;

  case 79:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8891 "Parser/parser.cc"
    break;

  case 80:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8897 "Parser/parser.cc"
    break;

  case 81:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8903 "Parser/parser.cc"
    break;

  case 82:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8909 "Parser/parser.cc"
    break;

  case 83:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8915 "Parser/parser.cc"
    break;

  case 84:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8921 "Parser/parser.cc"
    break;

  case 85:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8927 "Parser/parser.cc"
    break;

  case 86:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8933 "Parser/parser.cc"
    break;

  case 87:
#line 900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8941 "Parser/parser.cc"
    break;

  case 88:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8947 "Parser/parser.cc"
    break;

  case 89:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8956 "Parser/parser.cc"
    break;

  case 92:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8962 "Parser/parser.cc"
    break;

  case 93:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8968 "Parser/parser.cc"
    break;

  case 94:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8988 "Parser/parser.cc"
    break;

  case 95:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8994 "Parser/parser.cc"
    break;

  case 96:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 9000 "Parser/parser.cc"
    break;

  case 97:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 9006 "Parser/parser.cc"
    break;

  case 98:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9012 "Parser/parser.cc"
    break;

  case 99:
#line 952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9018 "Parser/parser.cc"
    break;

  case 100:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9024 "Parser/parser.cc"
    break;

  case 101:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9030 "Parser/parser.cc"
    break;

  case 102:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9036 "Parser/parser.cc"
    break;

  case 103:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9042 "Parser/parser.cc"
    break;

  case 104:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 9048 "Parser/parser.cc"
    break;

  case 105:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9057 "Parser/parser.cc"
    break;

  case 106:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {  (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9063 "Parser/parser.cc"
    break;

  case 107:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9069 "Parser/parser.cc"
    break;

  case 108:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9075 "Parser/parser.cc"
    break;

  case 109:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9081 "Parser/parser.cc"
    break;

  case 110:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9087 "Parser/parser.cc"
    break;

  case 111:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9093 "Parser/parser.cc"
    break;

  case 112:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9099 "Parser/parser.cc"
    break;

  case 113:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9105 "Parser/parser.cc"
    break;

  case 114:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9111 "Parser/parser.cc"
    break;

  case 116:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9117 "Parser/parser.cc"
    break;

  case 117:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9123 "Parser/parser.cc"
    break;

  case 118:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9129 "Parser/parser.cc"
    break;

  case 119:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9135 "Parser/parser.cc"
    break;

  case 120:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9141 "Parser/parser.cc"
    break;

  case 121:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9147 "Parser/parser.cc"
    break;

  case 122:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9153 "Parser/parser.cc"
    break;

  case 123:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9159 "Parser/parser.cc"
    break;

  case 131:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9165 "Parser/parser.cc"
    break;

  case 133:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9171 "Parser/parser.cc"
    break;

  case 134:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9177 "Parser/parser.cc"
    break;

  case 135:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9183 "Parser/parser.cc"
    break;

  case 137:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9189 "Parser/parser.cc"
    break;

  case 138:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9195 "Parser/parser.cc"
    break;

  case 140:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9201 "Parser/parser.cc"
    break;

  case 141:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9207 "Parser/parser.cc"
    break;

  case 143:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9213 "Parser/parser.cc"
    break;

  case 144:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9219 "Parser/parser.cc"
    break;

  case 145:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9225 "Parser/parser.cc"
    break;

  case 146:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9231 "Parser/parser.cc"
    break;

  case 148:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9237 "Parser/parser.cc"
    break;

  case 149:
#line 1075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9243 "Parser/parser.cc"
    break;

  case 151:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9249 "Parser/parser.cc"
    break;

  case 153:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9255 "Parser/parser.cc"
    break;

  case 155:
#line 1093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9261 "Parser/parser.cc"
    break;

  case 157:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9267 "Parser/parser.cc"
    break;

  case 159:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9273 "Parser/parser.cc"
    break;

  case 161:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9279 "Parser/parser.cc"
    break;

  case 162:
#line 1113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9285 "Parser/parser.cc"
    break;

  case 165:
#line 1124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 9297 "Parser/parser.cc"
    break;

  case 166:
#line 1132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9303 "Parser/parser.cc"
    break;

  case 167:
#line 1137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9309 "Parser/parser.cc"
    break;

  case 171:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9315 "Parser/parser.cc"
    break;

  case 172:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9321 "Parser/parser.cc"
    break;

  case 173:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9327 "Parser/parser.cc"
    break;

  case 174:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9333 "Parser/parser.cc"
    break;

  case 175:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9339 "Parser/parser.cc"
    break;

  case 176:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9345 "Parser/parser.cc"
    break;

  case 177:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9351 "Parser/parser.cc"
    break;

  case 178:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9357 "Parser/parser.cc"
    break;

  case 179:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9363 "Parser/parser.cc"
    break;

  case 180:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9369 "Parser/parser.cc"
    break;

  case 181:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9375 "Parser/parser.cc"
    break;

  case 182:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9381 "Parser/parser.cc"
    break;

  case 183:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9387 "Parser/parser.cc"
    break;

  case 184:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9393 "Parser/parser.cc"
    break;

  case 185:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9399 "Parser/parser.cc"
    break;

  case 187:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9405 "Parser/parser.cc"
    break;

  case 188:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9411 "Parser/parser.cc"
    break;

  case 189:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9417 "Parser/parser.cc"
    break;

  case 191:
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9423 "Parser/parser.cc"
    break;

  case 192:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9429 "Parser/parser.cc"
    break;

  case 207:
#line 1217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9435 "Parser/parser.cc"
    break;

  case 209:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9441 "Parser/parser.cc"
    break;

  case 210:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9447 "Parser/parser.cc"
    break;

  case 211:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9458 "Parser/parser.cc"
    break;

  case 212:
#line 1238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9464 "Parser/parser.cc"
    break;

  case 213:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9470 "Parser/parser.cc"
    break;

  case 215:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9476 "Parser/parser.cc"
    break;

  case 216:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9482 "Parser/parser.cc"
    break;

  case 217:
#line 1256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9488 "Parser/parser.cc"
    break;

  case 218:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9494 "Parser/parser.cc"
    break;

  case 219:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9500 "Parser/parser.cc"
    break;

  case 222:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9506 "Parser/parser.cc"
    break;

  case 223:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9513 "Parser/parser.cc"
    break;

  case 224:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9519 "Parser/parser.cc"
    break;

  case 225:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9525 "Parser/parser.cc"
    break;

  case 226:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9531 "Parser/parser.cc"
    break;

  case 227:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9537 "Parser/parser.cc"
    break;

  case 228:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9551 "Parser/parser.cc"
    break;

  case 229:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9557 "Parser/parser.cc"
    break;

  case 230:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9563 "Parser/parser.cc"
    break;

  case 231:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9572 "Parser/parser.cc"
    break;

  case 232:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9578 "Parser/parser.cc"
    break;

  case 233:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9584 "Parser/parser.cc"
    break;

  case 234:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9590 "Parser/parser.cc"
    break;

  case 235:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9596 "Parser/parser.cc"
    break;

  case 236:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9602 "Parser/parser.cc"
    break;

  case 237:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9608 "Parser/parser.cc"
    break;

  case 238:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9614 "Parser/parser.cc"
    break;

  case 240:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9620 "Parser/parser.cc"
    break;

  case 241:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9626 "Parser/parser.cc"
    break;

  case 242:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9632 "Parser/parser.cc"
    break;

  case 243:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9638 "Parser/parser.cc"
    break;

  case 244:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9644 "Parser/parser.cc"
    break;

  case 245:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9650 "Parser/parser.cc"
    break;

  case 246:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9656 "Parser/parser.cc"
    break;

  case 248:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9662 "Parser/parser.cc"
    break;

  case 249:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9668 "Parser/parser.cc"
    break;

  case 250:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9674 "Parser/parser.cc"
    break;

  case 252:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9680 "Parser/parser.cc"
    break;

  case 253:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9686 "Parser/parser.cc"
    break;

  case 254:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9692 "Parser/parser.cc"
    break;

  case 255:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9701 "Parser/parser.cc"
    break;

  case 256:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9707 "Parser/parser.cc"
    break;

  case 257:
#line 1405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9713 "Parser/parser.cc"
    break;

  case 258:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9719 "Parser/parser.cc"
    break;

  case 259:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9728 "Parser/parser.cc"
    break;

  case 260:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9734 "Parser/parser.cc"
    break;

  case 261:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9740 "Parser/parser.cc"
    break;

  case 262:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9746 "Parser/parser.cc"
    break;

  case 263:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9755 "Parser/parser.cc"
    break;

  case 264:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9761 "Parser/parser.cc"
    break;

  case 265:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9767 "Parser/parser.cc"
    break;

  case 267:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9786 "Parser/parser.cc"
    break;

  case 268:
#line 1455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9792 "Parser/parser.cc"
    break;

  case 269:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9800 "Parser/parser.cc"
    break;

  case 270:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9806 "Parser/parser.cc"
    break;

  case 271:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9812 "Parser/parser.cc"
    break;

  case 272:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9818 "Parser/parser.cc"
    break;

  case 273:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9824 "Parser/parser.cc"
    break;

  case 274:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9830 "Parser/parser.cc"
    break;

  case 275:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9836 "Parser/parser.cc"
    break;

  case 276:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9845 "Parser/parser.cc"
    break;

  case 277:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9854 "Parser/parser.cc"
    break;

  case 278:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9860 "Parser/parser.cc"
    break;

  case 279:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9869 "Parser/parser.cc"
    break;

  case 280:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9878 "Parser/parser.cc"
    break;

  case 281:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9884 "Parser/parser.cc"
    break;

  case 282:
#line 1500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9890 "Parser/parser.cc"
    break;

  case 283:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9896 "Parser/parser.cc"
    break;

  case 284:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9902 "Parser/parser.cc"
    break;

  case 285:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9908 "Parser/parser.cc"
    break;

  case 286:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9914 "Parser/parser.cc"
    break;

  case 287:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9920 "Parser/parser.cc"
    break;

  case 288:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9926 "Parser/parser.cc"
    break;

  case 289:
#line 1518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9935 "Parser/parser.cc"
    break;

  case 290:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9945 "Parser/parser.cc"
    break;

  case 291:
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9951 "Parser/parser.cc"
    break;

  case 292:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9957 "Parser/parser.cc"
    break;

  case 293:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9966 "Parser/parser.cc"
    break;

  case 294:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9976 "Parser/parser.cc"
    break;

  case 295:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9982 "Parser/parser.cc"
    break;

  case 296:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9991 "Parser/parser.cc"
    break;

  case 297:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10001 "Parser/parser.cc"
    break;

  case 298:
#line 1558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10007 "Parser/parser.cc"
    break;

  case 299:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 10013 "Parser/parser.cc"
    break;

  case 300:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10019 "Parser/parser.cc"
    break;

  case 301:
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10025 "Parser/parser.cc"
    break;

  case 302:
#line 1568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10034 "Parser/parser.cc"
    break;

  case 303:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10044 "Parser/parser.cc"
    break;

  case 304:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10050 "Parser/parser.cc"
    break;

  case 305:
#line 1582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10059 "Parser/parser.cc"
    break;

  case 306:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10069 "Parser/parser.cc"
    break;

  case 307:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10075 "Parser/parser.cc"
    break;

  case 308:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10084 "Parser/parser.cc"
    break;

  case 309:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10094 "Parser/parser.cc"
    break;

  case 310:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10100 "Parser/parser.cc"
    break;

  case 311:
#line 1609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 10108 "Parser/parser.cc"
    break;

  case 312:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); (yyval.forctl) = nullptr;
				(yyvsp[-1].oper) = OperKinds::GEThan;
			} // if
			(yyval.forctl) = enumRangeCtrl( (yyvsp[-3].expr), (yyvsp[-1].oper), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 10120 "Parser/parser.cc"
    break;

  case 313:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10127 "Parser/parser.cc"
    break;

  case 314:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false ); }
#line 10134 "Parser/parser.cc"
    break;

  case 315:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10141 "Parser/parser.cc"
    break;

  case 316:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10147 "Parser/parser.cc"
    break;

  case 317:
#line 1641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10153 "Parser/parser.cc"
    break;

  case 318:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10159 "Parser/parser.cc"
    break;

  case 319:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10165 "Parser/parser.cc"
    break;

  case 320:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10171 "Parser/parser.cc"
    break;

  case 321:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10177 "Parser/parser.cc"
    break;

  case 322:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10183 "Parser/parser.cc"
    break;

  case 324:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10189 "Parser/parser.cc"
    break;

  case 325:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10195 "Parser/parser.cc"
    break;

  case 326:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10201 "Parser/parser.cc"
    break;

  case 327:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10207 "Parser/parser.cc"
    break;

  case 328:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10213 "Parser/parser.cc"
    break;

  case 329:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10219 "Parser/parser.cc"
    break;

  case 330:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10225 "Parser/parser.cc"
    break;

  case 331:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10231 "Parser/parser.cc"
    break;

  case 332:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10237 "Parser/parser.cc"
    break;

  case 333:
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10243 "Parser/parser.cc"
    break;

  case 334:
#line 1692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10249 "Parser/parser.cc"
    break;

  case 335:
#line 1694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10255 "Parser/parser.cc"
    break;

  case 336:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10261 "Parser/parser.cc"
    break;

  case 337:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10267 "Parser/parser.cc"
    break;

  case 338:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10273 "Parser/parser.cc"
    break;

  case 339:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10279 "Parser/parser.cc"
    break;

  case 340:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10285 "Parser/parser.cc"
    break;

  case 341:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10291 "Parser/parser.cc"
    break;

  case 342:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10297 "Parser/parser.cc"
    break;

  case 343:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10303 "Parser/parser.cc"
    break;

  case 344:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10309 "Parser/parser.cc"
    break;

  case 345:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10315 "Parser/parser.cc"
    break;

  case 348:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10321 "Parser/parser.cc"
    break;

  case 349:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10330 "Parser/parser.cc"
    break;

  case 350:
#line 1737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10336 "Parser/parser.cc"
    break;

  case 351:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10342 "Parser/parser.cc"
    break;

  case 354:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10348 "Parser/parser.cc"
    break;

  case 355:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10354 "Parser/parser.cc"
    break;

  case 358:
#line 1762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10360 "Parser/parser.cc"
    break;

  case 359:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10366 "Parser/parser.cc"
    break;

  case 360:
#line 1770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10372 "Parser/parser.cc"
    break;

  case 361:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10378 "Parser/parser.cc"
    break;

  case 362:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10384 "Parser/parser.cc"
    break;

  case 363:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10390 "Parser/parser.cc"
    break;

  case 364:
#line 1779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10396 "Parser/parser.cc"
    break;

  case 365:
#line 1781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10402 "Parser/parser.cc"
    break;

  case 366:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10408 "Parser/parser.cc"
    break;

  case 369:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10414 "Parser/parser.cc"
    break;

  case 370:
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10420 "Parser/parser.cc"
    break;

  case 371:
#line 1803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10426 "Parser/parser.cc"
    break;

  case 372:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10432 "Parser/parser.cc"
    break;

  case 373:
#line 1810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10438 "Parser/parser.cc"
    break;

  case 374:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10444 "Parser/parser.cc"
    break;

  case 375:
#line 1817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10450 "Parser/parser.cc"
    break;

  case 376:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10456 "Parser/parser.cc"
    break;

  case 377:
#line 1824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10462 "Parser/parser.cc"
    break;

  case 378:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10468 "Parser/parser.cc"
    break;

  case 379:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10474 "Parser/parser.cc"
    break;

  case 380:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10480 "Parser/parser.cc"
    break;

  case 381:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10486 "Parser/parser.cc"
    break;

  case 382:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10492 "Parser/parser.cc"
    break;

  case 383:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10498 "Parser/parser.cc"
    break;

  case 384:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10504 "Parser/parser.cc"
    break;

  case 385:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10510 "Parser/parser.cc"
    break;

  case 386:
#line 1856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10516 "Parser/parser.cc"
    break;

  case 387:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10522 "Parser/parser.cc"
    break;

  case 388:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10528 "Parser/parser.cc"
    break;

  case 389:
#line 1862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10534 "Parser/parser.cc"
    break;

  case 390:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10540 "Parser/parser.cc"
    break;

  case 391:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10546 "Parser/parser.cc"
    break;

  case 393:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10552 "Parser/parser.cc"
    break;

  case 394:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10558 "Parser/parser.cc"
    break;

  case 395:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10564 "Parser/parser.cc"
    break;

  case 400:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10570 "Parser/parser.cc"
    break;

  case 401:
#line 1895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10576 "Parser/parser.cc"
    break;

  case 402:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10582 "Parser/parser.cc"
    break;

  case 403:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10588 "Parser/parser.cc"
    break;

  case 404:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10594 "Parser/parser.cc"
    break;

  case 405:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10600 "Parser/parser.cc"
    break;

  case 406:
#line 1908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10606 "Parser/parser.cc"
    break;

  case 407:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10612 "Parser/parser.cc"
    break;

  case 410:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10618 "Parser/parser.cc"
    break;

  case 411:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10624 "Parser/parser.cc"
    break;

  case 412:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10633 "Parser/parser.cc"
    break;

  case 413:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10639 "Parser/parser.cc"
    break;

  case 414:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10645 "Parser/parser.cc"
    break;

  case 415:
#line 1939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10651 "Parser/parser.cc"
    break;

  case 416:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10660 "Parser/parser.cc"
    break;

  case 417:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10669 "Parser/parser.cc"
    break;

  case 418:
#line 1959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10675 "Parser/parser.cc"
    break;

  case 421:
#line 1966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10681 "Parser/parser.cc"
    break;

  case 422:
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10687 "Parser/parser.cc"
    break;

  case 424:
#line 1977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10693 "Parser/parser.cc"
    break;

  case 425:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10699 "Parser/parser.cc"
    break;

  case 435:
#line 2005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 10705 "Parser/parser.cc"
    break;

  case 436:
#line 2007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10711 "Parser/parser.cc"
    break;

  case 440:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10717 "Parser/parser.cc"
    break;

  case 442:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10723 "Parser/parser.cc"
    break;

  case 443:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10729 "Parser/parser.cc"
    break;

  case 444:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10735 "Parser/parser.cc"
    break;

  case 445:
#line 2044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10741 "Parser/parser.cc"
    break;

  case 446:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10747 "Parser/parser.cc"
    break;

  case 447:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10753 "Parser/parser.cc"
    break;

  case 448:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10759 "Parser/parser.cc"
    break;

  case 449:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10765 "Parser/parser.cc"
    break;

  case 451:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10771 "Parser/parser.cc"
    break;

  case 452:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10777 "Parser/parser.cc"
    break;

  case 453:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10783 "Parser/parser.cc"
    break;

  case 454:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10794 "Parser/parser.cc"
    break;

  case 455:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10800 "Parser/parser.cc"
    break;

  case 456:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10806 "Parser/parser.cc"
    break;

  case 457:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10812 "Parser/parser.cc"
    break;

  case 458:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 459:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10824 "Parser/parser.cc"
    break;

  case 460:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10830 "Parser/parser.cc"
    break;

  case 461:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10839 "Parser/parser.cc"
    break;

  case 462:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10848 "Parser/parser.cc"
    break;

  case 463:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10857 "Parser/parser.cc"
    break;

  case 464:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10868 "Parser/parser.cc"
    break;

  case 465:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10877 "Parser/parser.cc"
    break;

  case 466:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10883 "Parser/parser.cc"
    break;

  case 467:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10889 "Parser/parser.cc"
    break;

  case 468:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10895 "Parser/parser.cc"
    break;

  case 469:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10903 "Parser/parser.cc"
    break;

  case 470:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10911 "Parser/parser.cc"
    break;

  case 471:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10917 "Parser/parser.cc"
    break;

  case 474:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10932 "Parser/parser.cc"
    break;

  case 475:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10938 "Parser/parser.cc"
    break;

  case 476:
#line 2186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10944 "Parser/parser.cc"
    break;

  case 477:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10950 "Parser/parser.cc"
    break;

  case 478:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10956 "Parser/parser.cc"
    break;

  case 479:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10962 "Parser/parser.cc"
    break;

  case 485:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10972 "Parser/parser.cc"
    break;

  case 498:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10978 "Parser/parser.cc"
    break;

  case 501:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10984 "Parser/parser.cc"
    break;

  case 502:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10990 "Parser/parser.cc"
    break;

  case 504:
#line 2273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10996 "Parser/parser.cc"
    break;

  case 505:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 11002 "Parser/parser.cc"
    break;

  case 506:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 11008 "Parser/parser.cc"
    break;

  case 507:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 11014 "Parser/parser.cc"
    break;

  case 508:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 11020 "Parser/parser.cc"
    break;

  case 509:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11026 "Parser/parser.cc"
    break;

  case 511:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11032 "Parser/parser.cc"
    break;

  case 512:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11038 "Parser/parser.cc"
    break;

  case 514:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11044 "Parser/parser.cc"
    break;

  case 515:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 11050 "Parser/parser.cc"
    break;

  case 516:
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 11056 "Parser/parser.cc"
    break;

  case 517:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 11062 "Parser/parser.cc"
    break;

  case 518:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 11068 "Parser/parser.cc"
    break;

  case 519:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 11074 "Parser/parser.cc"
    break;

  case 520:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 11080 "Parser/parser.cc"
    break;

  case 521:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 11086 "Parser/parser.cc"
    break;

  case 522:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 11092 "Parser/parser.cc"
    break;

  case 523:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11098 "Parser/parser.cc"
    break;

  case 524:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11104 "Parser/parser.cc"
    break;

  case 525:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 11110 "Parser/parser.cc"
    break;

  case 526:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11116 "Parser/parser.cc"
    break;

  case 527:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11122 "Parser/parser.cc"
    break;

  case 528:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11128 "Parser/parser.cc"
    break;

  case 529:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11134 "Parser/parser.cc"
    break;

  case 530:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11140 "Parser/parser.cc"
    break;

  case 531:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11146 "Parser/parser.cc"
    break;

  case 532:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11152 "Parser/parser.cc"
    break;

  case 533:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11158 "Parser/parser.cc"
    break;

  case 534:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11164 "Parser/parser.cc"
    break;

  case 535:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11170 "Parser/parser.cc"
    break;

  case 536:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11176 "Parser/parser.cc"
    break;

  case 537:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11182 "Parser/parser.cc"
    break;

  case 538:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11188 "Parser/parser.cc"
    break;

  case 539:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11194 "Parser/parser.cc"
    break;

  case 540:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11200 "Parser/parser.cc"
    break;

  case 541:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11206 "Parser/parser.cc"
    break;

  case 542:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11212 "Parser/parser.cc"
    break;

  case 543:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11218 "Parser/parser.cc"
    break;

  case 544:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11224 "Parser/parser.cc"
    break;

  case 545:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11230 "Parser/parser.cc"
    break;

  case 546:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11236 "Parser/parser.cc"
    break;

  case 547:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11242 "Parser/parser.cc"
    break;

  case 548:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11248 "Parser/parser.cc"
    break;

  case 549:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11254 "Parser/parser.cc"
    break;

  case 550:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11260 "Parser/parser.cc"
    break;

  case 551:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11266 "Parser/parser.cc"
    break;

  case 553:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11272 "Parser/parser.cc"
    break;

  case 555:
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11278 "Parser/parser.cc"
    break;

  case 556:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11284 "Parser/parser.cc"
    break;

  case 557:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11290 "Parser/parser.cc"
    break;

  case 559:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11296 "Parser/parser.cc"
    break;

  case 560:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11302 "Parser/parser.cc"
    break;

  case 561:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11308 "Parser/parser.cc"
    break;

  case 562:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11314 "Parser/parser.cc"
    break;

  case 564:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11320 "Parser/parser.cc"
    break;

  case 566:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11326 "Parser/parser.cc"
    break;

  case 567:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11332 "Parser/parser.cc"
    break;

  case 568:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11338 "Parser/parser.cc"
    break;

  case 569:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11344 "Parser/parser.cc"
    break;

  case 570:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11350 "Parser/parser.cc"
    break;

  case 571:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11356 "Parser/parser.cc"
    break;

  case 572:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11362 "Parser/parser.cc"
    break;

  case 573:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11368 "Parser/parser.cc"
    break;

  case 574:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11374 "Parser/parser.cc"
    break;

  case 576:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11380 "Parser/parser.cc"
    break;

  case 577:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11386 "Parser/parser.cc"
    break;

  case 578:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11392 "Parser/parser.cc"
    break;

  case 580:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11398 "Parser/parser.cc"
    break;

  case 581:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11404 "Parser/parser.cc"
    break;

  case 582:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11413 "Parser/parser.cc"
    break;

  case 584:
#line 2488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11419 "Parser/parser.cc"
    break;

  case 585:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11425 "Parser/parser.cc"
    break;

  case 586:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11431 "Parser/parser.cc"
    break;

  case 588:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11437 "Parser/parser.cc"
    break;

  case 589:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11443 "Parser/parser.cc"
    break;

  case 591:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11449 "Parser/parser.cc"
    break;

  case 592:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11455 "Parser/parser.cc"
    break;

  case 593:
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11461 "Parser/parser.cc"
    break;

  case 594:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11467 "Parser/parser.cc"
    break;

  case 595:
#line 2517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11473 "Parser/parser.cc"
    break;

  case 596:
#line 2519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11479 "Parser/parser.cc"
    break;

  case 597:
#line 2524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11485 "Parser/parser.cc"
    break;

  case 598:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11491 "Parser/parser.cc"
    break;

  case 599:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11497 "Parser/parser.cc"
    break;

  case 601:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11503 "Parser/parser.cc"
    break;

  case 602:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11509 "Parser/parser.cc"
    break;

  case 603:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11515 "Parser/parser.cc"
    break;

  case 604:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11521 "Parser/parser.cc"
    break;

  case 605:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11527 "Parser/parser.cc"
    break;

  case 610:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11533 "Parser/parser.cc"
    break;

  case 611:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11539 "Parser/parser.cc"
    break;

  case 612:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11548 "Parser/parser.cc"
    break;

  case 613:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11556 "Parser/parser.cc"
    break;

  case 614:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11565 "Parser/parser.cc"
    break;

  case 615:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11574 "Parser/parser.cc"
    break;

  case 616:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11583 "Parser/parser.cc"
    break;

  case 617:
#line 2587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11592 "Parser/parser.cc"
    break;

  case 619:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11598 "Parser/parser.cc"
    break;

  case 620:
#line 2598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11604 "Parser/parser.cc"
    break;

  case 621:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11614 "Parser/parser.cc"
    break;

  case 622:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11633 "Parser/parser.cc"
    break;

  case 625:
#line 2632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11639 "Parser/parser.cc"
    break;

  case 626:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11645 "Parser/parser.cc"
    break;

  case 627:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11651 "Parser/parser.cc"
    break;

  case 628:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11657 "Parser/parser.cc"
    break;

  case 629:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11663 "Parser/parser.cc"
    break;

  case 630:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11669 "Parser/parser.cc"
    break;

  case 631:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11678 "Parser/parser.cc"
    break;

  case 632:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11684 "Parser/parser.cc"
    break;

  case 633:
#line 2654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11693 "Parser/parser.cc"
    break;

  case 634:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11699 "Parser/parser.cc"
    break;

  case 635:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11708 "Parser/parser.cc"
    break;

  case 636:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11714 "Parser/parser.cc"
    break;

  case 637:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11720 "Parser/parser.cc"
    break;

  case 638:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11733 "Parser/parser.cc"
    break;

  case 639:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11742 "Parser/parser.cc"
    break;

  case 640:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11748 "Parser/parser.cc"
    break;

  case 641:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11754 "Parser/parser.cc"
    break;

  case 642:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11767 "Parser/parser.cc"
    break;

  case 643:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11773 "Parser/parser.cc"
    break;

  case 646:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11779 "Parser/parser.cc"
    break;

  case 647:
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11785 "Parser/parser.cc"
    break;

  case 650:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11791 "Parser/parser.cc"
    break;

  case 652:
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11797 "Parser/parser.cc"
    break;

  case 653:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11803 "Parser/parser.cc"
    break;

  case 654:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11809 "Parser/parser.cc"
    break;

  case 655:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11815 "Parser/parser.cc"
    break;

  case 656:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11821 "Parser/parser.cc"
    break;

  case 657:
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11827 "Parser/parser.cc"
    break;

  case 659:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11833 "Parser/parser.cc"
    break;

  case 661:
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11839 "Parser/parser.cc"
    break;

  case 662:
#line 2754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11845 "Parser/parser.cc"
    break;

  case 664:
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11851 "Parser/parser.cc"
    break;

  case 665:
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11857 "Parser/parser.cc"
    break;

  case 667:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11863 "Parser/parser.cc"
    break;

  case 668:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11874 "Parser/parser.cc"
    break;

  case 669:
#line 2787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11888 "Parser/parser.cc"
    break;

  case 670:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11894 "Parser/parser.cc"
    break;

  case 671:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11900 "Parser/parser.cc"
    break;

  case 672:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11906 "Parser/parser.cc"
    break;

  case 673:
#line 2805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11917 "Parser/parser.cc"
    break;

  case 674:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11923 "Parser/parser.cc"
    break;

  case 675:
#line 2814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11929 "Parser/parser.cc"
    break;

  case 677:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11935 "Parser/parser.cc"
    break;

  case 678:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11941 "Parser/parser.cc"
    break;

  case 679:
#line 2829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11947 "Parser/parser.cc"
    break;

  case 680:
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11953 "Parser/parser.cc"
    break;

  case 681:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11962 "Parser/parser.cc"
    break;

  case 682:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11971 "Parser/parser.cc"
    break;

  case 683:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11977 "Parser/parser.cc"
    break;

  case 684:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 11987 "Parser/parser.cc"
    break;

  case 685:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11993 "Parser/parser.cc"
    break;

  case 686:
#line 2859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 11999 "Parser/parser.cc"
    break;

  case 688:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 12005 "Parser/parser.cc"
    break;

  case 689:
#line 2870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12011 "Parser/parser.cc"
    break;

  case 690:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12017 "Parser/parser.cc"
    break;

  case 691:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12023 "Parser/parser.cc"
    break;

  case 692:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12029 "Parser/parser.cc"
    break;

  case 693:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12035 "Parser/parser.cc"
    break;

  case 695:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12041 "Parser/parser.cc"
    break;

  case 698:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12047 "Parser/parser.cc"
    break;

  case 699:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12053 "Parser/parser.cc"
    break;

  case 700:
#line 2900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 12059 "Parser/parser.cc"
    break;

  case 701:
#line 2902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12065 "Parser/parser.cc"
    break;

  case 704:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12071 "Parser/parser.cc"
    break;

  case 705:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12077 "Parser/parser.cc"
    break;

  case 706:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12083 "Parser/parser.cc"
    break;

  case 708:
#line 2918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12089 "Parser/parser.cc"
    break;

  case 709:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12095 "Parser/parser.cc"
    break;

  case 710:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12101 "Parser/parser.cc"
    break;

  case 712:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12107 "Parser/parser.cc"
    break;

  case 713:
#line 2937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12113 "Parser/parser.cc"
    break;

  case 714:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12119 "Parser/parser.cc"
    break;

  case 715:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12125 "Parser/parser.cc"
    break;

  case 716:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12131 "Parser/parser.cc"
    break;

  case 718:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12137 "Parser/parser.cc"
    break;

  case 719:
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12143 "Parser/parser.cc"
    break;

  case 720:
#line 2957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12149 "Parser/parser.cc"
    break;

  case 725:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12155 "Parser/parser.cc"
    break;

  case 727:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12161 "Parser/parser.cc"
    break;

  case 728:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12167 "Parser/parser.cc"
    break;

  case 731:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12173 "Parser/parser.cc"
    break;

  case 734:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12179 "Parser/parser.cc"
    break;

  case 735:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12185 "Parser/parser.cc"
    break;

  case 736:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12191 "Parser/parser.cc"
    break;

  case 737:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12197 "Parser/parser.cc"
    break;

  case 738:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12203 "Parser/parser.cc"
    break;

  case 739:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12209 "Parser/parser.cc"
    break;

  case 740:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12215 "Parser/parser.cc"
    break;

  case 742:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12221 "Parser/parser.cc"
    break;

  case 743:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12227 "Parser/parser.cc"
    break;

  case 744:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12233 "Parser/parser.cc"
    break;

  case 746:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12239 "Parser/parser.cc"
    break;

  case 748:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12245 "Parser/parser.cc"
    break;

  case 749:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12251 "Parser/parser.cc"
    break;

  case 750:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12257 "Parser/parser.cc"
    break;

  case 751:
#line 3046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12263 "Parser/parser.cc"
    break;

  case 752:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12269 "Parser/parser.cc"
    break;

  case 753:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12275 "Parser/parser.cc"
    break;

  case 755:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12281 "Parser/parser.cc"
    break;

  case 756:
#line 3079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12287 "Parser/parser.cc"
    break;

  case 757:
#line 3081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12293 "Parser/parser.cc"
    break;

  case 758:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12304 "Parser/parser.cc"
    break;

  case 759:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12310 "Parser/parser.cc"
    break;

  case 760:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12316 "Parser/parser.cc"
    break;

  case 761:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12322 "Parser/parser.cc"
    break;

  case 762:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12331 "Parser/parser.cc"
    break;

  case 763:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12337 "Parser/parser.cc"
    break;

  case 764:
#line 3107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12347 "Parser/parser.cc"
    break;

  case 765:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12353 "Parser/parser.cc"
    break;

  case 766:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12359 "Parser/parser.cc"
    break;

  case 767:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12365 "Parser/parser.cc"
    break;

  case 768:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12371 "Parser/parser.cc"
    break;

  case 769:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12377 "Parser/parser.cc"
    break;

  case 770:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12383 "Parser/parser.cc"
    break;

  case 771:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12389 "Parser/parser.cc"
    break;

  case 772:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12395 "Parser/parser.cc"
    break;

  case 773:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12401 "Parser/parser.cc"
    break;

  case 776:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12407 "Parser/parser.cc"
    break;

  case 777:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12413 "Parser/parser.cc"
    break;

  case 778:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12419 "Parser/parser.cc"
    break;

  case 779:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12425 "Parser/parser.cc"
    break;

  case 781:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12431 "Parser/parser.cc"
    break;

  case 782:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12437 "Parser/parser.cc"
    break;

  case 783:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12443 "Parser/parser.cc"
    break;

  case 784:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12449 "Parser/parser.cc"
    break;

  case 785:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12455 "Parser/parser.cc"
    break;

  case 786:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12461 "Parser/parser.cc"
    break;

  case 787:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12467 "Parser/parser.cc"
    break;

  case 788:
#line 3187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12476 "Parser/parser.cc"
    break;

  case 789:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12485 "Parser/parser.cc"
    break;

  case 790:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12494 "Parser/parser.cc"
    break;

  case 791:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12500 "Parser/parser.cc"
    break;

  case 792:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12509 "Parser/parser.cc"
    break;

  case 793:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12515 "Parser/parser.cc"
    break;

  case 795:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12521 "Parser/parser.cc"
    break;

  case 800:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12527 "Parser/parser.cc"
    break;

  case 801:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12533 "Parser/parser.cc"
    break;

  case 802:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12539 "Parser/parser.cc"
    break;

  case 804:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 12545 "Parser/parser.cc"
    break;

  case 805:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12551 "Parser/parser.cc"
    break;

  case 806:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12557 "Parser/parser.cc"
    break;

  case 807:
#line 3257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12563 "Parser/parser.cc"
    break;

  case 809:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12569 "Parser/parser.cc"
    break;

  case 810:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12575 "Parser/parser.cc"
    break;

  case 811:
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12581 "Parser/parser.cc"
    break;

  case 812:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12597 "Parser/parser.cc"
    break;

  case 813:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12603 "Parser/parser.cc"
    break;

  case 814:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12609 "Parser/parser.cc"
    break;

  case 815:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12615 "Parser/parser.cc"
    break;

  case 816:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12621 "Parser/parser.cc"
    break;

  case 817:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12627 "Parser/parser.cc"
    break;

  case 818:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12633 "Parser/parser.cc"
    break;

  case 820:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12642 "Parser/parser.cc"
    break;

  case 821:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12648 "Parser/parser.cc"
    break;

  case 822:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12657 "Parser/parser.cc"
    break;

  case 823:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12667 "Parser/parser.cc"
    break;

  case 824:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12676 "Parser/parser.cc"
    break;

  case 825:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12686 "Parser/parser.cc"
    break;

  case 826:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12697 "Parser/parser.cc"
    break;

  case 827:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12707 "Parser/parser.cc"
    break;

  case 828:
#line 3341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12718 "Parser/parser.cc"
    break;

  case 829:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12728 "Parser/parser.cc"
    break;

  case 830:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12739 "Parser/parser.cc"
    break;

  case 831:
#line 3361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12749 "Parser/parser.cc"
    break;

  case 832:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12755 "Parser/parser.cc"
    break;

  case 834:
#line 3378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12761 "Parser/parser.cc"
    break;

  case 835:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12767 "Parser/parser.cc"
    break;

  case 836:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12773 "Parser/parser.cc"
    break;

  case 837:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12785 "Parser/parser.cc"
    break;

  case 838:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12796 "Parser/parser.cc"
    break;

  case 839:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12805 "Parser/parser.cc"
    break;

  case 840:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12814 "Parser/parser.cc"
    break;

  case 841:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12820 "Parser/parser.cc"
    break;

  case 842:
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12826 "Parser/parser.cc"
    break;

  case 843:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12832 "Parser/parser.cc"
    break;

  case 844:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12841 "Parser/parser.cc"
    break;

  case 845:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12847 "Parser/parser.cc"
    break;

  case 846:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12853 "Parser/parser.cc"
    break;

  case 847:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12859 "Parser/parser.cc"
    break;

  case 852:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12865 "Parser/parser.cc"
    break;

  case 853:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12871 "Parser/parser.cc"
    break;

  case 854:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12881 "Parser/parser.cc"
    break;

  case 855:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12887 "Parser/parser.cc"
    break;

  case 858:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12893 "Parser/parser.cc"
    break;

  case 859:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12899 "Parser/parser.cc"
    break;

  case 860:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12905 "Parser/parser.cc"
    break;

  case 861:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12911 "Parser/parser.cc"
    break;

  case 863:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12917 "Parser/parser.cc"
    break;

  case 864:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12923 "Parser/parser.cc"
    break;

  case 865:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12929 "Parser/parser.cc"
    break;

  case 866:
#line 3501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12935 "Parser/parser.cc"
    break;

  case 868:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12941 "Parser/parser.cc"
    break;

  case 869:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12947 "Parser/parser.cc"
    break;

  case 870:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12953 "Parser/parser.cc"
    break;

  case 871:
#line 3546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12959 "Parser/parser.cc"
    break;

  case 872:
#line 3551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12965 "Parser/parser.cc"
    break;

  case 874:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12971 "Parser/parser.cc"
    break;

  case 875:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12977 "Parser/parser.cc"
    break;

  case 876:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12983 "Parser/parser.cc"
    break;

  case 877:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12989 "Parser/parser.cc"
    break;

  case 878:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12995 "Parser/parser.cc"
    break;

  case 879:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13001 "Parser/parser.cc"
    break;

  case 880:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13007 "Parser/parser.cc"
    break;

  case 881:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13013 "Parser/parser.cc"
    break;

  case 882:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13019 "Parser/parser.cc"
    break;

  case 883:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13025 "Parser/parser.cc"
    break;

  case 884:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13031 "Parser/parser.cc"
    break;

  case 885:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13037 "Parser/parser.cc"
    break;

  case 886:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13043 "Parser/parser.cc"
    break;

  case 887:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13049 "Parser/parser.cc"
    break;

  case 888:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13055 "Parser/parser.cc"
    break;

  case 889:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13061 "Parser/parser.cc"
    break;

  case 890:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13067 "Parser/parser.cc"
    break;

  case 891:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13073 "Parser/parser.cc"
    break;

  case 893:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13079 "Parser/parser.cc"
    break;

  case 894:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13085 "Parser/parser.cc"
    break;

  case 895:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13091 "Parser/parser.cc"
    break;

  case 896:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13097 "Parser/parser.cc"
    break;

  case 897:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13103 "Parser/parser.cc"
    break;

  case 898:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13109 "Parser/parser.cc"
    break;

  case 899:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13115 "Parser/parser.cc"
    break;

  case 900:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13121 "Parser/parser.cc"
    break;

  case 901:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13127 "Parser/parser.cc"
    break;

  case 902:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13133 "Parser/parser.cc"
    break;

  case 903:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13139 "Parser/parser.cc"
    break;

  case 904:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13145 "Parser/parser.cc"
    break;

  case 905:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13151 "Parser/parser.cc"
    break;

  case 906:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13157 "Parser/parser.cc"
    break;

  case 907:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13163 "Parser/parser.cc"
    break;

  case 908:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13169 "Parser/parser.cc"
    break;

  case 912:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13175 "Parser/parser.cc"
    break;

  case 913:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13181 "Parser/parser.cc"
    break;

  case 914:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13187 "Parser/parser.cc"
    break;

  case 915:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13193 "Parser/parser.cc"
    break;

  case 916:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13199 "Parser/parser.cc"
    break;

  case 917:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13205 "Parser/parser.cc"
    break;

  case 918:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13211 "Parser/parser.cc"
    break;

  case 919:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13217 "Parser/parser.cc"
    break;

  case 920:
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13223 "Parser/parser.cc"
    break;

  case 921:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13229 "Parser/parser.cc"
    break;

  case 922:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13235 "Parser/parser.cc"
    break;

  case 923:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13241 "Parser/parser.cc"
    break;

  case 924:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13247 "Parser/parser.cc"
    break;

  case 925:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13253 "Parser/parser.cc"
    break;

  case 926:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13259 "Parser/parser.cc"
    break;

  case 927:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13268 "Parser/parser.cc"
    break;

  case 928:
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13274 "Parser/parser.cc"
    break;

  case 929:
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13280 "Parser/parser.cc"
    break;

  case 931:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13286 "Parser/parser.cc"
    break;

  case 932:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13292 "Parser/parser.cc"
    break;

  case 933:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13298 "Parser/parser.cc"
    break;

  case 934:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13304 "Parser/parser.cc"
    break;

  case 935:
#line 3734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13310 "Parser/parser.cc"
    break;

  case 936:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13316 "Parser/parser.cc"
    break;

  case 937:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13322 "Parser/parser.cc"
    break;

  case 938:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13328 "Parser/parser.cc"
    break;

  case 939:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13334 "Parser/parser.cc"
    break;

  case 940:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13340 "Parser/parser.cc"
    break;

  case 941:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13346 "Parser/parser.cc"
    break;

  case 942:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13352 "Parser/parser.cc"
    break;

  case 943:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13358 "Parser/parser.cc"
    break;

  case 944:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13364 "Parser/parser.cc"
    break;

  case 945:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13370 "Parser/parser.cc"
    break;

  case 946:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13376 "Parser/parser.cc"
    break;

  case 947:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13382 "Parser/parser.cc"
    break;

  case 948:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13388 "Parser/parser.cc"
    break;

  case 950:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13394 "Parser/parser.cc"
    break;

  case 951:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13400 "Parser/parser.cc"
    break;

  case 952:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13406 "Parser/parser.cc"
    break;

  case 953:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13412 "Parser/parser.cc"
    break;

  case 954:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13418 "Parser/parser.cc"
    break;

  case 955:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13424 "Parser/parser.cc"
    break;

  case 956:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13430 "Parser/parser.cc"
    break;

  case 957:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13436 "Parser/parser.cc"
    break;

  case 958:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13442 "Parser/parser.cc"
    break;

  case 959:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13448 "Parser/parser.cc"
    break;

  case 960:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13454 "Parser/parser.cc"
    break;

  case 961:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13460 "Parser/parser.cc"
    break;

  case 962:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13466 "Parser/parser.cc"
    break;

  case 963:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13472 "Parser/parser.cc"
    break;

  case 964:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13478 "Parser/parser.cc"
    break;

  case 965:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13484 "Parser/parser.cc"
    break;

  case 966:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13490 "Parser/parser.cc"
    break;

  case 967:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13497 "Parser/parser.cc"
    break;

  case 969:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13503 "Parser/parser.cc"
    break;

  case 970:
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13509 "Parser/parser.cc"
    break;

  case 971:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13515 "Parser/parser.cc"
    break;

  case 972:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13521 "Parser/parser.cc"
    break;

  case 973:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13527 "Parser/parser.cc"
    break;

  case 974:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13533 "Parser/parser.cc"
    break;

  case 975:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13539 "Parser/parser.cc"
    break;

  case 976:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13545 "Parser/parser.cc"
    break;

  case 977:
#line 3853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13551 "Parser/parser.cc"
    break;

  case 978:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13557 "Parser/parser.cc"
    break;

  case 979:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13563 "Parser/parser.cc"
    break;

  case 980:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13569 "Parser/parser.cc"
    break;

  case 981:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13575 "Parser/parser.cc"
    break;

  case 982:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13582 "Parser/parser.cc"
    break;

  case 984:
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13588 "Parser/parser.cc"
    break;

  case 985:
#line 3884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13594 "Parser/parser.cc"
    break;

  case 986:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13600 "Parser/parser.cc"
    break;

  case 987:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13606 "Parser/parser.cc"
    break;

  case 988:
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13612 "Parser/parser.cc"
    break;

  case 989:
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13618 "Parser/parser.cc"
    break;

  case 990:
#line 3900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13624 "Parser/parser.cc"
    break;

  case 991:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13630 "Parser/parser.cc"
    break;

  case 992:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13636 "Parser/parser.cc"
    break;

  case 993:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13642 "Parser/parser.cc"
    break;

  case 994:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13648 "Parser/parser.cc"
    break;

  case 996:
#line 3932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13654 "Parser/parser.cc"
    break;

  case 997:
#line 3934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13660 "Parser/parser.cc"
    break;

  case 998:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13666 "Parser/parser.cc"
    break;

  case 999:
#line 3941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13672 "Parser/parser.cc"
    break;

  case 1000:
#line 3943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13678 "Parser/parser.cc"
    break;

  case 1001:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13684 "Parser/parser.cc"
    break;

  case 1002:
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13690 "Parser/parser.cc"
    break;

  case 1004:
#line 3953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13696 "Parser/parser.cc"
    break;

  case 1005:
#line 3955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13702 "Parser/parser.cc"
    break;

  case 1006:
#line 3957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13708 "Parser/parser.cc"
    break;

  case 1007:
#line 3962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13714 "Parser/parser.cc"
    break;

  case 1008:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13720 "Parser/parser.cc"
    break;

  case 1009:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13726 "Parser/parser.cc"
    break;

  case 1010:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13732 "Parser/parser.cc"
    break;

  case 1011:
#line 3974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13738 "Parser/parser.cc"
    break;

  case 1012:
#line 3977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13744 "Parser/parser.cc"
    break;

  case 1013:
#line 3984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13750 "Parser/parser.cc"
    break;

  case 1015:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13756 "Parser/parser.cc"
    break;

  case 1016:
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13762 "Parser/parser.cc"
    break;

  case 1018:
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13768 "Parser/parser.cc"
    break;

  case 1019:
#line 4002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13774 "Parser/parser.cc"
    break;

  case 1021:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13780 "Parser/parser.cc"
    break;

  case 1022:
#line 4010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13786 "Parser/parser.cc"
    break;

  case 1023:
#line 4015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13792 "Parser/parser.cc"
    break;

  case 1024:
#line 4017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13798 "Parser/parser.cc"
    break;

  case 1025:
#line 4019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13804 "Parser/parser.cc"
    break;

  case 1026:
#line 4021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13810 "Parser/parser.cc"
    break;

  case 1027:
#line 4055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13816 "Parser/parser.cc"
    break;

  case 1030:
#line 4062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13823 "Parser/parser.cc"
    break;

  case 1031:
#line 4065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13829 "Parser/parser.cc"
    break;

  case 1032:
#line 4067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13835 "Parser/parser.cc"
    break;

  case 1033:
#line 4072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13841 "Parser/parser.cc"
    break;

  case 1034:
#line 4074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13847 "Parser/parser.cc"
    break;

  case 1035:
#line 4076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13853 "Parser/parser.cc"
    break;

  case 1036:
#line 4078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13859 "Parser/parser.cc"
    break;

  case 1037:
#line 4080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13865 "Parser/parser.cc"
    break;

  case 1039:
#line 4086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13871 "Parser/parser.cc"
    break;

  case 1040:
#line 4088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13877 "Parser/parser.cc"
    break;

  case 1041:
#line 4090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13883 "Parser/parser.cc"
    break;

  case 1042:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13889 "Parser/parser.cc"
    break;

  case 1043:
#line 4097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13895 "Parser/parser.cc"
    break;

  case 1044:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13901 "Parser/parser.cc"
    break;

  case 1046:
#line 4106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13907 "Parser/parser.cc"
    break;

  case 1048:
#line 4117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13913 "Parser/parser.cc"
    break;

  case 1049:
#line 4120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13919 "Parser/parser.cc"
    break;

  case 1050:
#line 4122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13925 "Parser/parser.cc"
    break;

  case 1051:
#line 4125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13931 "Parser/parser.cc"
    break;

  case 1052:
#line 4127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13937 "Parser/parser.cc"
    break;

  case 1053:
#line 4129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13943 "Parser/parser.cc"
    break;

  case 1055:
#line 4144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13949 "Parser/parser.cc"
    break;

  case 1056:
#line 4146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13955 "Parser/parser.cc"
    break;

  case 1057:
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13961 "Parser/parser.cc"
    break;

  case 1058:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13967 "Parser/parser.cc"
    break;

  case 1059:
#line 4155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13973 "Parser/parser.cc"
    break;

  case 1060:
#line 4157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13979 "Parser/parser.cc"
    break;

  case 1061:
#line 4159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13985 "Parser/parser.cc"
    break;

  case 1063:
#line 4165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13991 "Parser/parser.cc"
    break;

  case 1064:
#line 4167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13997 "Parser/parser.cc"
    break;

  case 1065:
#line 4169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14003 "Parser/parser.cc"
    break;

  case 1066:
#line 4174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14009 "Parser/parser.cc"
    break;

  case 1067:
#line 4176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14015 "Parser/parser.cc"
    break;

  case 1070:
#line 4186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14021 "Parser/parser.cc"
    break;

  case 1073:
#line 4197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14027 "Parser/parser.cc"
    break;

  case 1074:
#line 4199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14033 "Parser/parser.cc"
    break;

  case 1075:
#line 4201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14039 "Parser/parser.cc"
    break;

  case 1076:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14045 "Parser/parser.cc"
    break;

  case 1077:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14051 "Parser/parser.cc"
    break;

  case 1078:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14057 "Parser/parser.cc"
    break;

  case 1079:
#line 4214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14063 "Parser/parser.cc"
    break;

  case 1080:
#line 4216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14069 "Parser/parser.cc"
    break;

  case 1081:
#line 4218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14075 "Parser/parser.cc"
    break;

  case 1082:
#line 4220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14081 "Parser/parser.cc"
    break;

  case 1083:
#line 4222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14087 "Parser/parser.cc"
    break;

  case 1084:
#line 4225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14093 "Parser/parser.cc"
    break;

  case 1085:
#line 4227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14099 "Parser/parser.cc"
    break;

  case 1086:
#line 4229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14105 "Parser/parser.cc"
    break;

  case 1087:
#line 4231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14111 "Parser/parser.cc"
    break;

  case 1088:
#line 4233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14117 "Parser/parser.cc"
    break;

  case 1089:
#line 4238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14123 "Parser/parser.cc"
    break;

  case 1090:
#line 4240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14129 "Parser/parser.cc"
    break;

  case 1091:
#line 4245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14135 "Parser/parser.cc"
    break;

  case 1092:
#line 4247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14141 "Parser/parser.cc"
    break;

  case 1094:
#line 4274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14147 "Parser/parser.cc"
    break;

  case 1098:
#line 4285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14153 "Parser/parser.cc"
    break;

  case 1099:
#line 4287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14159 "Parser/parser.cc"
    break;

  case 1100:
#line 4289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14165 "Parser/parser.cc"
    break;

  case 1101:
#line 4291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14171 "Parser/parser.cc"
    break;

  case 1102:
#line 4293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14177 "Parser/parser.cc"
    break;

  case 1103:
#line 4295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14183 "Parser/parser.cc"
    break;

  case 1104:
#line 4302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14189 "Parser/parser.cc"
    break;

  case 1105:
#line 4304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14195 "Parser/parser.cc"
    break;

  case 1106:
#line 4306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14201 "Parser/parser.cc"
    break;

  case 1107:
#line 4308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14207 "Parser/parser.cc"
    break;

  case 1108:
#line 4310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14213 "Parser/parser.cc"
    break;

  case 1109:
#line 4312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14219 "Parser/parser.cc"
    break;

  case 1110:
#line 4317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14225 "Parser/parser.cc"
    break;

  case 1111:
#line 4319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14231 "Parser/parser.cc"
    break;

  case 1112:
#line 4321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14237 "Parser/parser.cc"
    break;

  case 1113:
#line 4326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14243 "Parser/parser.cc"
    break;

  case 1114:
#line 4328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14249 "Parser/parser.cc"
    break;

  case 1115:
#line 4330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14255 "Parser/parser.cc"
    break;

  case 1118:
#line 4354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14261 "Parser/parser.cc"
    break;

  case 1119:
#line 4356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14267 "Parser/parser.cc"
    break;


#line 14271 "Parser/parser.cc"

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
#line 4359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
