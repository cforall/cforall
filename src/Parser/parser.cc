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
#define YYLAST   27674

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  183
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1122
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2194

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

#define YYPACT_NINF (-1867)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1121)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     114, 12344,   157,   194, 20806,   131, -1867, -1867, -1867, -1867,
   -1867, -1867, -1867, -1867, -1867, -1867, -1867, -1867,   -32,   816,
     141, -1867, -1867, -1867, -1867, -1867, -1867, -1867, -1867, -1867,
   -1867, -1867, -1867, -1867, -1867, -1867, -1867, -1867, -1867, -1867,
   -1867, -1867, -1867, -1867, -1867, -1867, -1867, -1867,   297,   290,
   -1867, -1867, -1867, -1867, -1867, -1867,  5712,  5712,   185, 12344,
     201,   219, 24703, -1867,   275, -1867, -1867, -1867, -1867, -1867,
   -1867, -1867, -1867, -1867, -1867,   316,  2552, -1867,   755,   104,
   -1867, -1867,  2366, -1867, -1867, -1867, -1867, 15286, -1867,   336,
     367,   398,   463,    86, -1867,  5834,   441,   450,   456,   455,
    5089,   676,  1164, 12524, -1867, -1867,   580, 15127,  2237, -1867,
   -1867, -1867, -1867,  2788,   693,  7420,  6045,  1031,  2788,  1060,
     505, -1867, -1867, -1867, -1867,    45, -1867, -1867, -1867, -1867,
     547, -1867, -1867, -1867, -1867, -1867,   511,   565,    45, -1867,
      45, 18874, -1867, -1867, -1867, 22480,  5712, -1867, -1867,  5712,
   -1867, 12344, -1867,   583, 22637, -1867, -1867,  5403, 23756, -1867,
   -1867,   917,   917,   614,  2517, -1867, -1867, -1867, -1867,   487,
   17148,    45,  3902,    45, -1867, -1867, -1867, -1867, -1867, -1867,
     619, -1867,   633,   667,  2383, -1867,   683, 26851, -1867, -1867,
   -1867, -1867, -1867, -1867, -1867, 19258,  2626,  2536,  2552,   849,
     691,   741,   758,   765,   790,   799, -1867, -1867, 20963, 13752,
   -1867,   680,   696, -1867, 15604, -1867, -1867, -1867, -1867,   804,
   -1867, -1867, -1867,   812, -1867, 24844,   946, 24998, -1867,   827,
    5712,   565,   836,   843, -1867,  2366,  5403,  2366, -1867, -1867,
   -1867,  3142,  5013,   853,   921,   403,   921, -1867,    45,    45,
     -12, 18548,   554,   921, -1867,    45,    45,   -12,    45, -1867,
      45, -1867,  5202, -1867, -1867,   869,   881,   917, 24510,   903,
   15286, -1867,  5834, -1867,  2788, -1867,  1580,   505,   915,   953,
   18548,  5712,  5712,   463, -1867, 13056, -1867,   917,   917,   926,
     953, 18548,  5712, -1867, 27012, -1867, -1867, -1867,   917, -1867,
   -1867, -1867, -1867,   917, -1867,   961,  4701,  5712, -1867, 20501,
     943, -1867, -1867, -1867, 24371,   565, 18711,   920,  5403, 20362,
   24510,  2788, -1867, -1867,  8565, -1867,   921,   142, -1867, 26851,
   23906,  3369,  5202, -1867,   625, -1867, -1867, -1867, -1867, -1867,
   22637,   921,  5712, -1867,   971,   967, -1867, -1867, -1867, -1867,
    5712,  2759,   245,   717, -1867,  5712,   633, -1867,   970,    45,
   -1867,   977, 22691,   666, 17646, 24564,  2788, -1867,  2788,   917,
    2788,   917, -1867, -1867,    45, -1867, -1867,   986, 22848, -1867,
   -1867, -1867, 22902,   804, -1867,  3938,   416,   665, -1867,   593,
     505,   987,  1005, -1867,  2517,   999,   633,  2517, -1867, -1867,
    2626, -1867,   791, -1867,  1003, -1867,  1054,  1108, 27015,  1079,
   27099,  1082,  1089, 26851, 27176,  1097, 24755, -1867, -1867, -1867,
   -1867, -1867, -1867, 27253, 27253, 19097,  1093,  4726, -1867, -1867,
   -1867, -1867,    39, -1867,   701, -1867,  1588, -1867, 26851, 26851,
   -1867,  1095,   645,  1022,  1070,   492,  1088,  1103,  1113,  1096,
    1157,   103, -1867,   792, -1867,  1102, -1867,  1127,  5756, 19741,
   -1867, -1867,   996,  1102, -1867, -1867,   817, -1867, -1867,   854,
    2536,  1146,  1152,  1155,  1179,  1210,  1220, -1867, -1867,   682,
    1204, -1867,   861,  1204,  1159, -1867,  1227, -1867, 22480, -1867,
    1160,  1234, 19902, -1867, -1867,  5590,  4177,  1261, 17646,  1270,
     899,  1158,  1226,  1257, -1867, -1867, -1867,  5712,  5648, 21479,
   -1867, -1867, -1867, -1867, -1867, -1867, -1867, 20310,  5432,  1093,
   24844,  1264,  1268, -1867, -1867,  1280, 24998,   746, -1867, -1867,
   -1867, 19741,  1295, -1867,   683, -1867, -1867, -1867,  1273,  3142,
     766,  1290,  1303,  1306,   805,  1312,  1317,  1319,  1321,  1324,
    1332,  5013, -1867, -1867, -1867,    45,  1314,  1335, -1867, -1867,
    1343,   463, -1867, -1867,   565,   953, 21129, -1867, -1867,   463,
   -1867, -1867,   565, -1867, -1867,  5202, -1867, 19741, 19741, -1867,
     917,  5403,  7937,  2295, 17812, -1867, -1867, -1867, -1867, -1867,
   -1867,   565,   953,   142,  1348, -1867, -1867,  2788,  1363,   953,
   18548, -1867,   565,   953, -1867, 27414, -1867,   917,   917, -1867,
   -1867,  1371,   347,  1374,   505,  1377, -1867, -1867, -1867, 21425,
    1349,  1381, -1867, -1867,   897, -1867,  1479, -1867,  1373, -1867,
   -1867, -1867, 23068, 27417, -1867, -1867, -1867, -1867, -1867,  3369,
     830,  5202, 21129,   921, 12344, -1867,  5712,  1398, 23122,  1401,
   -1867, -1867, -1867, -1867, -1867,  2517, -1867, -1867,  1489,  4852,
   21636, 13752, -1867, 23279, -1867,   917,   917, -1867, -1867,   804,
   -1867, 16650,  1408,  1558, 26851,   646,  1343,  1406, -1867,    45,
      45, -1867,  1204, -1867, 22691, -1867, -1867, 21425,   917,   917,
   -1867,  4852, -1867, -1867, 23701, -1867, -1867, 22848, -1867,    45,
    1425,    45,  1005,   264,  1434,   923, 22637,   929,   950, -1867,
    2626,  8335,  1423, -1867, 19419, -1867,  4726, 19580, -1867, 23333,
   22637, -1867, 19419, -1867, 26851, -1867, -1867, -1867, -1867, -1867,
   -1867, 19580, -1867, -1867, 21690, 23333, 23333,  1184,  1769,  1793,
     313,  1899, -1867,   979,  1454,  1206,  1459, -1867, 25075, 26851,
   25152,  1461, 26851,  2366, 26851,  2366, -1867,  3227, -1867, -1867,
    8335,  2927, 26851,  8335,  2366, -1867, -1867, 26851, 26851, 26851,
   26851, 26851, 26851, 26851, 26851, 26851, 26851, 26851, 26851, 26851,
   26851, 26851, 26851, 26851, 26851, 26851, 25229, -1867,   683,  5526,
   13752, -1867, -1867, -1867, -1867, -1867, -1867, -1867, -1867, -1867,
   -1867, -1867,  1463, 26851, -1867, -1867, 16816,   733, -1867, -1867,
      45,    45, -1867, -1867, 19741, -1867, -1867,   694,  1204, -1867,
     874,  1204, 21129, -1867, -1867,  1343, 21129, -1867,  1343, -1867,
   27501, -1867, -1867, -1867, 20649, 13752,  1470,  1219,  1473, 12879,
    1619,  4757,   699,  1406, -1867,    45,    45,  1406,   736, -1867,
      45,    45, 26851,  5712, 17812,  1485, 17812,  1495,  1406,    46,
   16982, 16982, 17978, 16982,  5712, -1867, -1867, 26851,  1280, -1867,
   24844,  1506, -1867,  1323, -1867, -1867, -1867,   988, -1867,  1492,
   16982, 26851,  1026,  1507,  1508,  1512,  1033,  1516,  1517,  1518,
    1520,  1522,  1523,   810,  1204, -1867, -1867,   867,  1204, -1867,
   -1867,   924,  1204, -1867, -1867, -1867,  5403,  1662,  1204, 24056,
   -1867, -1867,   565,  1529, -1867, -1867, -1867,  1064,  1532,  1065,
    1537, -1867,  1159,  1527,  1541, -1867,   565, -1867, 21847, -1867,
     565,   953,  1541, -1867,   565,  1536,  1538,  1540, -1867, -1867,
   21286, -1867,  2366,  5712, 11804,  1631, -1867,  1234, -1867, 16982,
    1084, -1867,  1541,  1559, -1867, -1867, -1867,  5403, 21901, -1867,
     391,   447, 19741,  1539, -1867,  1539, -1867, -1867, -1867, -1867,
   22848, -1867, 13922, 20063, -1867,  1560,  1562,  1564,  1566, -1867,
   14841,    45, -1867,   646, -1867, -1867, -1867, -1867,  1343, -1867,
   -1867, -1867,   917, -1867, -1867, -1867, -1867,   264,  1005,  1561,
     487, -1867, -1867,  1568,  5712,   264, -1867, -1867,  1567,  1570,
   -1867,  2366,  1574,  1572, -1867, -1867, -1867,  1577,  1579,  1578,
    1583,  1565,  1589,  1587,  1596,  1597,  1595,  1600, 26851,  1602,
    1604,  1605, 23490, 14092, 26851, -1867, -1867,  1907, -1867, -1867,
   -1867, 26851, -1867,  1607,  1608, 24921, -1867, -1867,  1248, -1867,
    8335,  1606, -1867,  1610, -1867, -1867,  5250, -1867,  1611, -1867,
    5250, -1867, -1867,  1251,  1613, -1867, -1867,  1095,  1095,  1095,
     645,   645,  1022,  1022,  1070,  1070,  1070,  1070,   492,   492,
    1088,  1103,  1113,  1096,  1157, 26851,  1305,  1614,  5250, -1867,
   -1867, 24844, -1867,  1616,  1618,  1620,  1621,   733, -1867, -1867,
   -1867, -1867, -1867, 21129, -1867, -1867,  1343, 21129, -1867,  1343,
    1629,  1633, 16982, 16982, -1867, -1867, 12879,   989,  1636,  1637,
    1638,  1640,  3313,  4757, -1867, -1867, 21129, -1867, -1867, -1867,
   -1867, -1867, -1867, 21129, -1867, -1867, -1867, -1867,  1628, -1867,
    1406,  1650, -1867, -1867, -1867, -1867, -1867, -1867, -1867, -1867,
    1644,  1651,  1653, -1867,  1654, -1867,   463,  5250,  1339,   257,
   -1867, -1867,  1663, -1867, 24998, -1867, 26851,    45, 16982,    45,
   -1867, -1867,   932,  1204, -1867,   957,  1204, -1867, -1867,   959,
    1204, 21129, -1867, -1867,  1343, 21129, -1867, -1867,  1343, 21129,
   -1867, -1867,  1343,   921, -1867,  1343,   337, -1867,  1102,  1658,
   -1867, -1867, -1867, -1867, -1867, -1867,  1666, -1867, -1867, -1867,
   22058,  1541, -1867,   565, -1867, -1867, -1867, -1867, -1867, 10381,
   -1867, -1867, -1867, -1867, -1867,   494,   345,   397, 13582,  1669,
    1673, 18368,  1674,  1678,  3400,  3761,  4109, 25306,  1680, -1867,
   -1867,  1683,  1686, 18368,  1687, -1867, -1867,   565, 26851, 26851,
    1809,  1681,   702, -1867, 18936,  1344,  1685,  1684,  1670, -1867,
   -1867, -1867, 11624, -1867, -1867, -1867, -1867, -1867,  2819, -1867,
   -1867, -1867,  1393,   409, -1867,   440, -1867,   409, -1867, -1867,
   -1867, -1867, -1867,  2366, -1867, -1867, 12702, 15445, -1867,  5712,
    1689,  1691, -1867, -1867, -1867,  5712, -1867, -1867, -1867,  5712,
   -1867,  5403, -1867,  1105, 22637,   633,   633, -1867, -1867,  1093,
    1234, 19902, -1867,  1102, -1867, 14262, -1867,   966,  1204, -1867,
     917, 14966, -1867, -1867,  1005,  1568,  1692,   264,   505,   533,
    1698,  1677,  1568, 22112, -1867,  1679, -1867,  8335,   734, -1867,
   21425,   734, 14092,  2366, -1867,   734, -1867, 22269,   734, -1867,
   26851, 26851, 26851, -1867, -1867, -1867, -1867, 26851, 26851,  1694,
   24844, -1867, -1867, 25383,  1699,  1712, -1867, -1867, -1867,  3875,
   -1867, -1867,  1353, -1867,    51, -1867,  1355, -1867, 25075, -1867,
   -1867, 26851, -1867,  1369,  1372,  1280, -1867,   980,  1204, -1867,
   -1867,  1710,  1711, -1867, -1867, -1867, -1867,  1715,  1006,  1204,
   -1867,  1024,  2842,    45,    45, -1867, -1867,  1716,  1718, -1867,
    1717, -1867, 17812,  1723, -1867, 17314, 17480,  1719, 17978,  1721,
   -1867,  1724, 26851, 26851,  1390,  1729, -1867, -1867, -1867, -1867,
   -1867, -1867,  1733, 21129, -1867, -1867,  1343, 21129, -1867, -1867,
    1343, 21129, -1867, -1867,  1343,  1736,  1739,  1744,   463, -1867,
   -1867,  1394, 26851, 24210,  1743,  1751, -1867, -1867, -1867,  1753,
   16109, 16269, 16429, 23122, 24510, 23333, 23333,  1761,  1745,   519,
     531,  2176, 11021, -1867,   532,  5712,  5712, -1867,  8335,   224,
     237, -1867, -1867, -1867, -1867, 13582, 26851,  1771,  1850, 13411,
   11984, -1867,  1752, -1867,  1754, 26851,  1756, 24844,  1758, 26851,
   19741, 26851, -1867, 12164,   821, -1867,  1759,    26, -1867,   -31,
    1851,   120,    45, -1867,  1789, -1867,  1768, -1867,  1770,  1796,
    1797, 18368, 18368, -1867, -1867,  1864, -1867, -1867,   137,   137,
     573, 13233,   545, -1867, -1867,  1798,  1800,   245, -1867, -1867,
   -1867, -1867, -1867, -1867, 14432,  1801,  1802,  1803, -1867, 21129,
   -1867, -1867,  1343, 26851, 26851,  1234,  1804, -1867,  1807,  1806,
     264,  1568,   487,  5712, -1867, 25460, -1867,  1811, -1867, 22426,
   26851, -1867,  1083,  1810,  1814,  1115, -1867,  1818, -1867, -1867,
   -1867, -1867, -1867, 24844,  1280, -1867, -1867, 25075, -1867,  1849,
    5250, -1867,  1849,  1849, -1867,  5250,  4323,  4524, -1867,  1404,
   -1867, -1867,  1812, 21129, -1867, -1867,  1343, -1867, -1867,  1824,
    1825,    45, 21129, -1867, -1867,  1343, 21129, -1867, -1867,  1828,
   -1867, -1867, -1867, -1867, -1867, -1867, -1867, -1867,  1650, -1867,
   -1867, -1867,  1829, -1867, -1867, -1867, -1867,  1832,  1837,    45,
    1838,  1839,  1841, -1867, -1867, -1867, -1867, 26851, -1867,   337,
   -1867,  1102, -1867, -1867,  1845, -1867,  1761,  1761,  1761,  4625,
    1182,  1826,   579, -1867,  4625,   611, 19741, -1867, -1867, -1867,
   -1867,  4252, 26851,  5677,   449, -1867, -1867,    85,  1843,  1843,
    1843,  5712, -1867, -1867, -1867,  1856, -1867, -1867, -1867, -1867,
    1684,  1857, 26851,   367,  1855,   455, 16596, 23122,  1124,  1862,
   18368,  1861, -1867, -1867, -1867, -1867,  1203, 18368, 26851,  1242,
     540, -1867, 26851, 24762, -1867, -1867,   612, -1867,  1280, -1867,
    1138,  1143,  1149,   628, -1867, -1867, -1867, -1867,   565,   821,
    1865, -1867, -1867, 26851, -1867,  1868,   683, -1867, 11444, -1867,
   -1867, -1867, 26851, 26851, -1867, -1867,   538,   137, -1867,   387,
   -1867, -1867, -1867,    45, -1867,  1539, -1867, -1867, -1867,  1866,
    1867, -1867, -1867,  1871, -1867,  1872,   264, -1867,  1568,  1873,
     505,  1677, 24844, -1867, -1867, -1867, -1867,  1876, -1867, -1867,
   26851, -1867, 22269, 26851,  1280,  1882,  1410, -1867,  1412, -1867,
    5250, -1867,  5250, -1867, -1867, -1867,  1880,    45,    45,  1881,
    1884, -1867,  1879, -1867, -1867, -1867, -1867, -1867,  1418, 26851,
   -1867, -1867, -1867, -1867,   626,  1182,   872,   636, -1867, -1867,
   -1867, -1867,    45,    45, -1867, -1867, -1867,   647, -1867,  1162,
    4252,   949, -1867,  5677, -1867,    45, -1867, -1867, -1867, -1867,
   -1867, -1867, 18368, 18368,  1684, 18144,   369, 25537,  1968, 18368,
   -1867, -1867, -1867, -1867, -1867, 26851, -1867, 25614,  1969,  1875,
   20140, 25691, 18368, 12164,  1684,   570,  1396,  1877, 26851, -1867,
    1894,   469, 18368, -1867, 18368, -1867,  1896, -1867, 23544,  1878,
     683,   739, -1867, -1867,  1902,  1420,  1172, 18368,  1905, 18368,
   18368, 18368, -1867,   633, -1867, -1867,  1903,  1911, -1867, -1867,
    1568,  1908, -1867, -1867,  1280, -1867, -1867, -1867, -1867,  1918,
   -1867, -1867, -1867,  1430,  1441, -1867, -1867, -1867, -1867, -1867,
   -1867, -1867, -1867, -1867,  1916,  1917,  1919,   872, -1867,    45,
   -1867, -1867, -1867, -1867, -1867,  1924,  4625, -1867,  2001,  7576,
     121, 14605, -1867, 18242, -1867,    36,  1174, 18368,  2003,   662,
    1913,   420, 18368, 26851,   570,  1396,  1898, 25773,  1313,   917,
    1914,   453,  2013, -1867, 25850, 25927, 26851,  1684,  1909, 14774,
   -1867, -1867, -1867, 23544,  1910,  4355, 23279,  2366, -1867,  1926,
    1912,    60, -1867, 26851,  8335, -1867, -1867, 26851,   409, -1867,
   -1867, -1867, -1867, -1867,  1939, -1867,  1940, -1867, -1867, -1867,
    1025,  1204, -1867, -1867,  1182, -1867, 18368, -1867,   343, -1867,
     109, -1867, -1867, -1867,  1941, 15782, -1867, -1867, 18368, -1867,
      41, -1867, 18368, 26851,  1943, 26004, -1867, -1867, 26081, 26158,
   26851,  4852,  1684, -1867,  1102, 26235, 26312, 18368,  1925,   510,
    1929,   530, -1867, -1867,  1947, 15782,  1910, 26851,  1945,  2997,
    4907, -1867, -1867, -1867,  1935, -1867,  1998,  1949,   745,  1944,
   -1867, -1867,  1951,  1185,   437, -1867, -1867, 21129, -1867, -1867,
    1343, -1867, -1867, 26851, -1867, 26851, -1867, -1867,  1530, 15949,
   -1867, -1867, 18368, -1867, -1867,  1684, -1867, -1867,  1684,  1942,
     596,  1946,   685, -1867, -1867,   505, -1867,  1684, -1867,  1684,
   -1867,  1955, 26389, 26466, 26543, -1867,  1530,  1965, -1867,   565,
    4907,    60,  1952, 26851,  1950,    60,    60, -1867, -1867, 18368,
    2050,  1972, -1867, -1867, 18242, -1867,  1530, -1867, -1867,  1970,
   26620, 26697, 26774, -1867, -1867,  1684, -1867,  1684, -1867,  1684,
   -1867,   565, -1867,  1971,   683,  1974, -1867,   762, -1867, -1867,
   18368, -1867, -1867, 11153,  1978, 18242, -1867, -1867,  1684, -1867,
    1684, -1867,  1684,  1980, -1867,   683,  1981, -1867,  1957,   683,
   -1867, -1867, -1867, -1867, 11298, -1867, -1867,  1448, 26851, -1867,
    1198,   683,  2366,  1982,  1958, -1867, -1867,  1199, -1867, -1867,
    1960,  2366, -1867, -1867
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
       0,     0,   499,   855,   499,   811,     0,     0,   499,     0,
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
       0,   467,     0,     2,     2,   843,     0,   448,   499,   811,
       0,   838,     2,   844,     0,     0,     0,     0,   637,   897,
     499,   915,     0,     0,   499,   428,   426,   102,     3,   499,
       0,   812,     2,     0,   764,   800,   801,     0,   499,   796,
       0,     0,   499,   758,   757,   758,   558,   556,   682,  1082,
     499,  1087,   500,   499,  1073,     0,     0,     0,     0,  1051,
       0,   857,  1122,  1037,  1038,   718,  1034,  1035,  1049,  1077,
    1081,  1079,   587,   622,  1085,  1090,   674,   680,   680,     0,
       0,   690,   689,  1119,     0,   680,   791,   789,     0,     0,
     865,   162,     0,   159,   160,   164,   823,     0,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   499,   499,     0,   123,   122,     0,   119,   118,
      31,     0,    32,     0,     0,     0,   188,   187,     0,     3,
     158,     0,    55,     0,    56,    63,     0,    62,     0,    58,
       0,    57,    61,     0,     0,    54,   125,   127,   128,   129,
     131,   132,   134,   135,   139,   140,   137,   138,   142,   143,
     145,   147,   149,   151,   153,     0,     0,     0,     0,    33,
       3,   742,   166,     0,     0,     0,     0,  1002,  1003,   999,
    1000,   784,   783,   499,   905,   907,   909,   499,   925,   927,
       0,     0,   499,   499,  1027,  1026,   499,     0,     0,     0,
       0,     0,   857,  1037,   974,   991,   499,   969,   977,   715,
     972,   973,   716,   499,   984,   994,   987,   988,     0,     3,
    1121,     3,   711,   460,   710,   714,  1113,   720,   721,   703,
       0,   704,   705,     3,     3,     3,   736,     0,   157,     0,
       3,     3,     0,   745,     0,   739,     0,   857,   499,   857,
       3,   471,   857,   889,   893,   857,   946,   950,   958,   857,
     968,   499,   881,   884,   886,   499,   938,   941,   943,   499,
     961,   963,   965,   855,   479,  1014,     3,  1018,  1019,     3,
     846,   954,   571,   570,   573,   572,     2,   812,   847,   793,
     499,     2,   845,     0,   812,   848,   637,   637,   637,   499,
     697,   700,   701,   730,   431,     0,     0,     0,   499,     0,
       0,   352,     0,     0,     0,     0,     0,   193,     0,   347,
     348,     0,     0,   352,     0,   400,   399,     0,   168,   168,
     406,   598,   604,   210,   499,     2,     0,   194,     0,   221,
     195,   196,   499,   215,   197,   198,   199,   200,     0,   201,
     202,   353,     0,   367,   203,   373,   375,   378,   204,   205,
     206,   207,   208,     0,   209,   217,   581,   499,   219,     0,
       0,     0,     3,   825,   812,     0,   803,   780,   797,     0,
     798,     0,   799,     0,   499,   775,   775,  1084,  1089,     2,
     102,   499,     3,   596,     3,   500,  1045,   857,  1044,  1047,
     499,     3,  1033,  1039,   680,  1119,     0,   680,   686,   680,
       0,   691,  1119,   499,   861,     0,   868,     0,    93,    96,
     499,   101,   499,     0,    99,    95,    97,   499,     0,   113,
       0,     0,     0,   117,   121,   120,   192,     0,     0,     0,
     742,   110,   185,     0,     0,     0,    49,    50,    82,     0,
      82,    82,     0,    70,    72,    52,     0,    48,     0,    51,
     156,     0,   436,     0,     0,  1119,  1010,   857,  1009,  1012,
    1004,     0,     0,   898,   916,     3,     3,     0,   857,   980,
     983,   857,     0,   857,   857,   975,   992,     0,     0,  1114,
       0,   722,   499,     0,  1116,   499,   499,     0,   499,     0,
     445,     3,     0,     0,     0,     0,   741,   746,     3,   839,
       3,   856,     0,   499,   882,   885,   887,   499,   939,   942,
     944,   499,   962,   964,   966,     0,     0,     0,   736,  1025,
    1024,     0,     0,     0,     0,     0,   795,   812,   849,     0,
     499,   499,   499,   499,   499,   499,   499,   620,     0,     0,
       0,   651,   581,   638,     0,     0,     0,   429,   158,     0,
       0,   338,   339,   218,   220,   499,     0,     0,     0,   499,
     499,   334,     0,   332,     0,     0,     0,   742,     0,     0,
     499,     0,   379,   499,     0,   169,     0,     0,   407,     0,
       0,     0,   857,   225,     0,   216,     0,   329,     0,     0,
       0,   352,   352,   358,   357,   352,   369,   368,   352,   352,
       0,   581,     0,  1029,  1028,     0,     0,   767,   802,   804,
     779,   759,   763,   761,   499,     0,     0,     0,     3,   499,
    1040,  1042,  1043,     0,     0,   102,     0,     3,     0,     0,
     680,  1119,     0,     0,   669,     0,   685,     0,   792,   499,
       0,   161,  1030,     0,     0,     0,    42,     0,   114,   116,
     115,   112,   111,   742,  1119,   190,   189,     0,    69,    79,
       0,    73,    80,    81,    64,     0,     0,     0,    60,     0,
     155,    34,     0,   499,  1005,  1007,  1008,   899,   917,     0,
       0,   857,   499,   976,   978,   979,   499,   993,   995,     0,
     970,   985,   981,   996,  1115,   712,   461,   707,   706,   708,
    1118,  1117,     0,     3,   854,   752,   753,     0,     0,   857,
       0,     0,     0,   890,   947,   955,   480,     0,  1020,     0,
    1021,  1022,  1016,   829,     0,   831,   620,   620,   620,   651,
     658,   625,     0,   664,   651,     0,   499,   612,   650,   649,
     645,     0,     0,     0,     0,   652,   654,   857,   666,   666,
     666,     0,   646,   662,   432,     0,   342,   343,   340,   341,
     234,     0,     0,   236,   440,   235,   581,   499,     0,     0,
     352,     0,   317,   319,   318,   320,     0,   352,   193,   274,
       0,   267,     0,   193,   335,   333,     0,   327,  1119,   336,
       0,     0,     0,     0,   388,   389,   390,   391,     0,   381,
       0,   382,   344,     0,   345,     0,     0,   372,     0,   214,
     331,   330,     0,     0,   361,   371,     0,   352,   374,     0,
     376,   398,   430,   857,   827,   758,  1092,  1093,  1094,     0,
       0,     3,     3,     0,  1053,     0,   680,   670,  1119,     0,
     688,   691,   742,   692,   673,   794,   163,     0,  1031,    98,
       0,    35,   499,     0,  1119,     0,     0,    83,     0,    71,
       0,    77,     0,    75,    47,   167,     0,   857,   857,     0,
       0,   755,     0,   455,   459,   891,   948,   956,     0,     0,
     833,   616,   618,   614,     0,     0,  1060,     0,   659,  1065,
     661,  1057,   857,   857,   644,   665,   648,     0,   647,     0,
       0,     0,   668,     0,   640,   857,   639,   655,   667,   656,
     657,   663,   352,   352,   237,   581,     0,     0,   255,   352,
     322,   325,   323,   326,   321,     0,   324,     0,   263,     0,
     193,     0,   352,   499,   275,     0,   300,     0,     0,   328,
       0,     0,   352,   351,   352,   392,     0,   383,   499,     0,
       0,     0,   212,   211,   354,     0,     0,   352,     0,   352,
     352,   352,   458,   775,  1095,  1046,     0,     0,  1052,  1054,
    1119,     0,   672,   687,  1119,    53,    45,    43,    44,     0,
      67,   186,    74,     0,     0,  1011,   457,   456,   982,   997,
     754,  1015,  1023,   642,     0,     0,     0,  1061,  1062,   857,
     643,  1058,  1059,   641,   621,     0,     0,   350,   226,     0,
       0,     0,   248,   352,   228,     0,     0,   352,   257,   272,
     283,   277,   352,   193,     0,   287,     0,     0,     0,   312,
     278,   276,   265,   268,     0,     0,   193,   301,     0,     0,
     231,   349,   380,   499,   386,   393,   500,   397,   346,     0,
       0,   408,   359,     0,   158,   370,   363,     0,   364,   362,
     377,   766,  1055,  1056,     0,   676,     0,    68,    78,    76,
     857,  1068,  1070,  1063,     0,   653,   352,   243,   238,   241,
       0,   240,   247,   246,     0,   499,   250,   249,   352,   259,
       0,   256,   352,     0,     0,     0,   264,   269,     0,     0,
     193,     0,   288,   313,   314,     0,     0,   352,     0,   303,
     304,   302,   271,   337,     0,   499,   386,     0,     0,     0,
    1060,   394,   395,   396,     0,   401,     0,     0,     0,   409,
     410,   355,     0,     0,     0,   675,   693,   499,  1064,  1066,
    1067,   660,   227,     0,   245,     0,   244,   230,   251,   499,
     421,   260,   352,   261,   258,   273,   286,   284,   280,   292,
     290,   291,   289,   270,   315,   316,   285,   281,   282,   279,
     266,     0,     0,     0,     0,   233,   251,     0,   387,     0,
    1061,   408,     0,     0,     0,   408,     0,   360,   356,   352,
       0,     0,   239,   242,   352,     3,   252,   422,   262,     0,
       0,     0,     0,   311,   309,   306,   310,   307,   308,   305,
       3,     0,   384,     0,     0,     0,   402,     0,   411,   365,
     352,  1069,   222,     0,     0,   352,   299,   297,   294,   298,
     295,   296,   293,     0,   385,   414,     0,   412,     0,   414,
     366,   224,   223,   229,     0,   232,   415,     0,     0,   403,
       0,     0,     0,     0,     0,   416,   417,     0,   413,   404,
       0,     0,   405,   418
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1867,  2539,  2071, -1867,    -1,   870,  2754,  9866,   -44, -1867,
    -382, -1867,   363, -1867,  -995, -1269, -1867,   246,  5873,  2107,
   -1867,  -300, -1867,  1407,   438,   878,   879,   641,   876,  1365,
    1366,  1367,  1375,  1370, -1867,  -165,  -174,  -746, -1867,   814,
    9420,   909, -1867,  1703, -1867, -1867, -1187,  8993, -1176,  3714,
   -1867,  1292, -1867,   902,    10, -1867, -1867,   678,    94, -1867,
   -1866, -1629,   299,    66, -1867, -1867,   672,   314, -1867, -1559,
   -1867, -1493, -1867, -1867, -1867, -1867,   112, -1181, -1867, -1867,
   -1257,   436, -1867, -1867, -1867, -1867, -1867,   132, -1206, -1867,
   -1867, -1867, -1867, -1867,   138,   457,   458,   212, -1867, -1867,
   -1867, -1867,  -779, -1867,    72,    20, -1867,   146, -1867,   -76,
   -1867, -1867, -1867,   904,  -491, -1045,  -125, -1867,    16,     4,
     315,   335,  -931,  -905, -1867,  -102, -1867, -1867,    42, -1867,
    -137,  2636,  -192,  -258,  4312,  6301,  -717,     9,   184,    48,
    2004,  1844, -1867, -1867,  2143, -1867,   500,  5014, -1867,  2079,
   -1867,   205, -1867, -1867,  2502,   566,  5662,  3444,   -19,  1920,
    -266, -1867, -1867, -1867, -1867, -1867,  -254,  8268,  7210, -1867,
    -393,   209, -1867, -1185,   265, -1867,   196,   740, -1867,   -15,
    -190, -1867, -1867, -1867, -1867,  -166,  8662,  -968,   873,   434,
    1173, -1867,  -372,  -179,  -138,   517,  2057,  -780,  -103,   928,
    -128,  -413,  -279,  -204,  -519,  1342, -1867,  1682,   193,  -953,
    1569, -1867, -1867,   679, -1867, -1254,  -171,    47,  -512, -1867,
     271, -1867, -1867,  -875,  -923, -1867, -1867, -1867,  2215,  -786,
    -403, -1046,   -23, -1867, -1867, -1867, -1867, -1867, -1867,   319,
    -836,  -218, -1808,   125,  6688,   -61,  6489,  -105,  1509, -1867,
    3420,   -73,  -216,  -215,  -210,    33,   -74,   -69,   -68,   515,
      12,    14,    19,  -199,   -40,  -194,  -184,  -152,    34,  -130,
    -106,  -100,  -791,  -773,  -735,  -734,  -761,  -108,  -730, -1867,
   -1867,  -737,  1411,  1415,  1417,  3422, -1867,   576,  7887, -1867,
    -603,  -602,  -589,  -574,  -802, -1867, -1673, -1753, -1728, -1712,
    -656,   -51,  -299, -1867, -1867,    -2,    37,  -124, -1867,  9335,
    2043,  -559,  -468
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   857,   432,   433,   179,    86,  1256,   434,   407,
     435,  1575,  1576,   436,  1372,  1373,  1374,  1589,   458,   438,
     439,   440,   740,   741,   441,   442,   443,   444,   445,   446,
     447,   448,   449,   450,   451,   460,  1159,  1012,  1013,  1014,
     742,  1506,   803,   227,   805,   454,  1048,  1257,  1258,  1259,
    1260,  1261,  1262,  1263,  2153,  1264,  1265,  1691,  2009,  2010,
    1942,  1943,  1944,  2125,  2126,  1266,  1710,  1711,  2033,  1712,
    1856,  1857,  1267,  1268,  1269,  1270,  1271,  1272,  1885,  1889,
    1529,  1521,  1273,  1274,  1528,  1522,  1275,  1276,  1277,  1278,
    1279,  1280,  1281,  1729,  2048,  1730,  1731,  1974,  1282,  1283,
    1284,  1509,  2058,  2059,  2060,  2177,  2187,  2078,  2079,   315,
     316,   944,   945,  1225,    88,    89,    90,    91,    92,  1694,
     494,   212,    96,    97,    98,    99,   243,   244,   318,   297,
     496,   462,   497,   102,   330,   104,   105,   159,   365,   321,
     109,   110,   111,   175,   112,   967,   366,   160,   115,   267,
     116,   161,   276,   368,   369,   370,   162,   455,   121,   122,
     372,   123,   615,   937,   935,   936,  1667,   124,   125,   126,
     127,  1219,  1473,  1674,  1675,  1817,  1818,  1474,  1662,  1837,
    1676,   128,   702,  1324,   171,  1002,   129,  1003,  1004,  1566,
     975,   621,  1150,  1151,  1152,   622,   376,   505,   506,   624,
     464,   465,   228,   524,   525,   526,   527,   528,   353,  1305,
     354,   965,   963,   653,   355,   395,   356,   357,   466,   130,
     181,   182,   131,   958,   959,   960,   961,     2,  1206,  1207,
     644,  1293,   132,   343,   344,   278,   289,   598,   133,   231,
     134,   333,  1161,   588,   558,   173,   135,   402,   403,   404,
     136,   335,   247,   248,   249,   336,   138,   139,   140,   141,
     142,   143,   144,   252,   337,   254,   255,   256,   338,   258,
     259,   260,   843,   844,   845,   846,   847,   261,   849,   850,
     851,   808,   809,   810,   811,   559,  1199,  1452,   145,  1777,
     677,   678,   679,   680,   681,   682,  1820,  1821,  1822,  1823,
     667,   507,   380,   381,   382,   467,   218,   147,   148,   149,
     384,   871,   683
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      84,   868,   200,    84,   595,    93,   391,   201,   202,   579,
     106,   396,  1306,   453,  1058,   198,  1525,  1064,   537,   877,
     155,   325,   452,   530,  1039,   541,   542,   246,   990,  1325,
     502,   543,   745,   363,   137,  1298,   185,  1332,   146,   317,
    1128,   146,   544,   100,   576,   751,  1135,   545,  1511,   108,
    1124,  1542,  1543,  1210,  1735,    84,    84,   546,    84,   207,
     253,  1498,  1924,    93,   392,  1376,  1118,   377,   106,   976,
    1098,   503,   984,  1510,  1142,    84,  1144,  2017,   229,   695,
    1125,    84,   977,   698,   489,   217,    84,  1925,   203,   547,
     204,   469,   137,  1383,    84,   205,   146,   978,   685,    84,
    1591,   100,    84,  1926,  1119,  1120,    84,   108,  1288,  1121,
    2074,   548,  2018,   721,  -805,   541,   542,  2082,    58,   379,
     215,   543,  2012,   283,   471,   164,  1736,  1733,   328,   472,
     473,  2011,   544,   250,   257,   549,   279,   545,   765,   766,
     290,   550,   406,  1928,   146,    84,  1214,   546,    84,   317,
      84,   274,   217,  -838,  1868,    93,    84,   150,    58,   765,
     106,  1455,  1421,    84,   918,   920,  1294,   200,  1459,   552,
     662,    84,   201,   202,  1468,   565,   229,  1596,   317,   547,
     539,   405,    75,    84,   137,   107,   693,   924,   146,   317,
     696,  1599,   765,   100,  -806,    84,    84,    84,   932,   108,
     752,   548,   553,   635,   592,   753,   117,    84,  1734,   985,
     474,  1597,   475,    84,   628,   603,  1861,   476,  2019,   700,
     234,  2056,    75,  2083,   521,   549,   704,   674,  1242,    84,
     215,   550,   685,  1523,    84,    84,    84,   976,   984,   785,
      84,    84,   952,   107,   232,   246,   557,   512,  1536,   230,
     977,  -838,   163,   203,  2003,   204,  1524,   200,  2017,   552,
     205,    84,   201,   202,   117,   978,   208,  2011,   586,    84,
     639,    84,  2075,  2076,  1000,   215,   554,   701,   253,  1737,
      84,    84,   910,   786,    84,  2013,   610,  1298,  1469,  2017,
     914,    84,   553,  1007,  1747,   708,  1924,   746,   169,   635,
     662,  1957,  2052,   215,  1365,    84,    84,  1023,    84,  1162,
      20,   668,   287,    84,  1470,  1128,    94,    84,   599,   156,
    1355,  1925,   872,   883,   884,   320,  1789,  1791,  1793,   885,
      84,    84,  1405,   597,  1019,   107,    95,  1926,   586,    95,
     886,    84,   184,  1118,  1746,   887,   117,  1510,  1749,    84,
      84,   630,   631,   685,    84,   888,   117,   916,   186,  1561,
    1390,   971,  1406,   921,   833,   215,   554,   274,  1062,   556,
      58,   561,  1964,  1965,    94,  1129,   187,  1928,   569,  1132,
    1323,  1119,  1120,   650,    84,   489,  1397,   889,   848,   310,
    1147,  1148,  1484,    84,    95,  1422,    84,   685,   995,    84,
    1786,  1102,   310,   657,   165,   502,  1686,   166,   167,   890,
     168,  1654,   274,   651,   652,  1468,  1468,  1468,   271,  1688,
    -684,   685,   284,   883,   884,  1457,  1288,  -684,   685,   885,
    1423,  1001,   195,   891,    75,   320,   388,  2003,   272,   892,
     886,   668,    95,   657,  1330,   887,   503,  1939,  1940,  2124,
    1303,   634,   636,  1285,   170,   888,   117,   217,  1569,  1479,
    1480,  2028,  2029,  1891,   320,   471,    94,  1066,   969,    84,
     472,   473,  1034,   196,  1814,   320,    58,  2124,   903,  1827,
    1520,  2073,   502,  1035,  1036,   117,    95,   889,  1449,  1153,
     879,  1155,   989,   341,    84,    84,   117,  2155,   317,   207,
     320,   113,  1478,   512,   195,   994,    84,    84,  1170,   890,
    1450,   904,  -617,  2119,   976,  1551,  1423,    84,   221,   521,
    1323,   117,  1523,   503,   931,   287,  1211,   977,  1242,  1469,
    1469,  1469,  1859,   891,  1941,   971,   999,  1867,    84,   892,
      75,  1511,   978,  1562,   495,  1524,   274,  1939,  1940,   222,
      84,   474,  1526,   475,  1299,  1470,  1470,  1470,   476,   113,
     309,    -3,   310,   604,   557,   471,  1510,   118,   903,   564,
     472,   473,  1385,  1300,    84,  1527,   572,  1292,   616,  1481,
      84,  -989,    84,  1041,   512,   905,  1017,   705,  -989,  1020,
     707,  1022,  1768,  2025,  1024,  1788,    63,    64,   591,  1618,
     596,   904,   216,  1027,   235,  1834,  1029,  1030,  1031,   602,
    1301,   502,  1835,   236,  1887,   251,  1041,   224,   280,   237,
     272,   951,   291,   775,   776,   118,  2036,    58,   225,  1302,
    1519,  1836,  1625,   668,  1969,  1142,  1144,   238,    84,  1991,
      84,   113,   969,    84,   226,    84,  1298,    78,    93,  1888,
     282,   113,   503,   106,    84,   990,   502,  1476,    84,    14,
      15,    16,    17,    18,   955,   305,  1163,   274,   309,   777,
     778,  1308,  1411,  1041,   512,   905,  1477,   137,  1139,   262,
     597,   146,   235,  2102,  1956,   146,   100,   452,  1193, -1120,
      84,    75,   108,  1041,   236,  1681,  -493,   503,    58,  1862,
    1001,  1669,  1158,  2104,  1863,   388,   685,   118,  1476,    84,
     307,   566,   216,  1670,  1682,   557,  1141,   118,   927,    58,
    1850,  1851,  1852,  1853,  1154,   363,   504,  1752,  1349,   513,
     310,  2064,  1685,   848,  1353,   597,   942,  1483,   310,  1296,
    1395,  1396,  1825,  1854,  1087,  1361,    14,    15,    16,    17,
      18,   113,    84,  1145,    84,    58,    84,   216,  -671,  1041,
      84,  1826,    75,    84,   329,  -671,  1559,    58,   927,  2130,
     610,  1285,    58,  1567,  1681,  1041,   394,  2027,  1307,  -826,
     113,    74,   642,    75,   320,   216,   557,  1874,    84,  1835,
    2042,   113,  1863,  1828,  1869,  1913,  1430,  1914,  1900,  1929,
     600,    74,  1893,   671,   405,   117,    58,   672,  1923,    58,
    1835,   379,   351,   768,    80,   673,   113,   118,  1930,    75,
     769,   770,   195,   671,   397,  1041,  1602,   672,   107,  1933,
    -990,    75,  1326,    84,    80,   673,    75,  -990,    84,   822,
      84,  1584,   509,   557,  2023,   973,   118,   674,  1041,   117,
     478,  1103,    84,   510,  2093,   557,  1126,   118,  2132,   195,
     672,   745,   754,    84,   206,    64,   -23,   755,    74,   521,
      75,    85,    84,    75,   153,   165,   654,  1420,   166,   167,
     655,   168,   118,    58,   912,    14,    15,    16,    17,    18,
     806,  1541,  1359,  1133,   557,  1154,   363,   672,  1980,  1360,
     479,    80,    81,  1981,  2114,    84,   873,   874,   489,  2115,
     875,   926,    14,    15,    16,    17,    18,   480,   930,   495,
    1380,  2168,   934,   309,   481,   477,  2169,   557,   721,    85,
      14,    15,    16,    17,    18,  1724,  1725,  1726,  1727,  1728,
      58,    84,    84,   521,   955,    58,    85,    75,    93,   482,
     709,   787,   210,   106,   710,   788,    84,    85,   483,    94,
    1427,   515,   566,   956,   896,   146,   557,  1181,   529,  1778,
      85,   557,    58,    85,   955,   516,   813,    85,  1718,    95,
     814,   146,   379,    95,   531,   513,   100,   642,  1721,   477,
      58,   557,   108,   534,  1692,   146,   495,    58,  1692,  1713,
     535,  1158,  1769,    84,    75,    58,   309,    74,   477,    75,
      84,   765,  1713,   815,  1404,   848,   555,   710,   826,   973,
     274,    85,   557,   232,  1185,  1785,   577,    85,   557,  1815,
      58,  1107,    58,   557,  -494,   557,    75,  -497,   578,    58,
      80,    81,   389,   229,    14,    15,    16,    17,    18,   685,
    1578,  1579,  1580,    58,    75,    84,   941,  1581,  1582,    84,
     942,    75,  1553,  -495,  1784,   583,   513,    85,    85,    75,
     609,    64,  1661,    14,    15,    16,    17,    18,   486,    58,
     590,  1189,  1006,  1539,    85,   557,   655,    84,  1008,  1433,
     521,   601,   655,   557,    75,    85,    75,    58,    58,   625,
     113,   504,   629,    75,    58,   536,    85,   538,   477,  1009,
     557,    85,    85,   710,  1437,    84,  1441,    75,   557,   989,
     557,    84,    84,  1549,   646,   495,   746,   672,  1287,   658,
     305,    74,    85,    58,   396,   396,   645,  1603,  1040,   660,
      85,   557,  1041,    75,   113,   692,  1126,  1167,   477,   117,
     672,   814,   703,   806,  1829,    85,    84,   557,  1558,  1870,
     711,    75,    75,  1612,    80,    81,   118,   557,    75,  1646,
     495,    14,    15,    16,    17,    18,   613,   388,   504,   618,
     706,  1616,  2067,   309,   153,   672,   557,   557,    85,   495,
     566,   495,   771,   772,   557,   495,   495,    75,   495,   773,
     774,    85,    85,  1572,  1154,   363,  1067,  1068,  1069,  1901,
     118,   263,   264,   712,   265,   495,  1600,  1475,    74,  1577,
     266,   779,   780,  1202,  1204,  1909,   955,  1041,  1041,   713,
     521,    58,    93,    84,    84,    84,   716,   106,  2062,   719,
     671,   642,   521,   956,   672,   557,   720,   146,  1633,  1634,
    1213,    80,   673,  1904,   724,   699,   748,   452,   452,    94,
     668,   521,   789,    95,  1540,   146,    93,    84,   814,   767,
     100,   106,   781,   956,  1781,   783,   108,  1628,  1782,    95,
     782,   363,    84,  1846,   495,    84,    84,  1041,    84,   509,
     748,   379,   784,    95,    84,    75,  -498,  1871,    84,   146,
      84,  1041,  1872,   283,   100,   816,   814,   504,  1873,  1145,
     108,   817,  1041,  1145,   818,  1145,   829,    74,  1448,   279,
     290,  1934,   834,   748,   146,   814,    14,    15,    16,    17,
      18,  1985,  2143,  2020,   274,  1041,  2147,  1041,   819,  1815,
      85,  1994,    84,   557,  2118,  1996,  1032,   748,  1041,   955,
      80,    81,   504,  1850,  1851,  1852,  1853,  2184,  2190,   521,
    1693,  2181,  2191,   597,  1693,   485,    85,   379,    84,   820,
     146,   504,  1713,   504,  1043,  1044,  1854,   504,   504,   821,
     504,   660,   748,  1695,   831,  1855,    58,  1695,    85,   854,
      85,  1773,  1850,  1851,  1852,  1853,    -3,   504,  1678,   852,
     452,    84,  1811,  1812,  1813,  1041,  1154,   363,  -496,    85,
    1362,  1363,  1287,  1377,  1378,  1854,  1074,  1075,  1076,  1077,
     856,    85,    63,    64,  1860,  1460,  1461,  1462,   -18,   311,
     152,  1679,   869,   117,    65,    66,    67,    68,    69,    70,
      71,    72,  1884,   870,   113,    85,  1287,   495,   495,   893,
      75,    85,   878,   486,   881,   541,   542,  1221,  1475,  1475,
    1475,   543,   894,  1663,  1475,   895,   504,   117,  1041,  1381,
      84,   897,   544,    78,    84,    84,   898,   545,   899,   155,
     900,    77,   620,   901,   865,  1519,  1520,   546,  1838,  1838,
    1838,   902,   287,   379,   106,   907,   521,   908,   106,   106,
     146,  -165,  -165,   495,   322,  1680,   208,   748,   939,    85,
     118,    85,   106,   928,    85,  1594,  1595,  1598,  1595,   547,
     521,   521,   146,   533,  2080,   956,   146,   146,   929,   968,
      84,  1601,  1595,   108,  1115,  1587,  -615,   108,   108,  -613,
     146,   548,   938,    94,   940,    95,  1850,  1851,  1852,  1853,
     943,   108,  1635,  1587,  2080,   946,  1115,  1647,   962,  1041,
     954,   993,    84,    95,   599,   549,  1794,  1363,   966,  1854,
     979,   550,  1911,  1363,  1912,  1595,   981,    94,  -194,   597,
    1921,  1041,   521,  1983,  1984,   955,  1678,   674,  2127,    84,
     997,  1678,  1998,  1595,    84,    84,    84,    95,    19,  1005,
     552,   271,   284,  1999,  1595,  1016,   146,   627,  1939,  1940,
    1830,  2181,  2182,  1042,   883,   884,  1592,  1593,  1045,  1679,
     885,   272,    95,  1052,  1679,  1054,  1050,  1057,  1091,   504,
     504,   886,  1114,   553,  1065,  1115,   887,  1122,    48,    49,
      50,    51,    52,    53,    54,    55,   888,  1143,   956,  1070,
    1071,  1169,  1072,  1073,  1577,  1078,  1079,  1146,    84,  1089,
    1748,  1750,  1165,    84,  1839,  1840,  1172,  1173,    95,  1697,
      84,  1174,    84,  1697,  1697,  1175,  1176,  1177,   889,  1178,
      84,  1179,  1180,  2061,  1194,   504,  1975,  1697,  1201,   834,
     117,  1203,  1881,  1680,   117,   117,  1205,  -809,  1680,   521,
     890,  1216,  1289,  1217,   486,  1218,   521,   554,   117,    85,
     685,    85,   756,   283,   757,   758,   759,   744,  1295,  1316,
    1304,  1317,   396,  1318,   891,  1319,  1327,  1032,   113,  1334,
     892,  1329,  1333,  1336,   146,  1337,  1338,   521,  1339,   913,
      85,  1340,  1341,    85,   274,   760,  1344,   495,   761,   762,
     495,   495,  1343,   763,   764,  1345,  1346,  2051,  1347,  1348,
     903,  1350,   113,  1351,  1352,  2008,  1357,  1358,  1366,  1379,
    1375,   521,  1367,  1382,   452,  1386,    85,  1387,   156,  1388,
    1389,  1975,  -123,  -123,  -123,  -123,  -123,  -123,  1393,    84,
    1409,    84,  1394,   904,   118,  1398,  1399,  1400,    95,  1401,
      94,   280,   291,  1414,    94,    94,  -122,  -122,  -122,  -122,
    -122,  -122,  1223,  1412,  1415,   620,  1416,  1418,    94,  1426,
      95,  1453,  -810,  1508,    95,    95,  1485,    85,   118,    84,
    1486,  1489,    84,   541,   542,  1490,  1979,  1499,    95,   543,
    1500,   521,   521,  1501,  1503,   -22,   596,  1041,   521,  1512,
     544,  1533,  1513,  1534,  1564,   545,   911,  1560,  1565,  1583,
    1570,   521,  1587,  1678,   915,   546,   272,   905,  1588,  1607,
    1608,   521,   106,   521,  1611,  1622,  1977,  1623,  1630,  1624,
    1631,  1335,  2108,   925,   956,  1626,   521,  1595,   521,   521,
     521,  1636,  1639,   597,   933,  1643,  1679,   547,  1644,  2122,
     146,  2008,   287,  1645,    95,  1652,   194,  1653,   452,  1655,
     452,   108,    14,    15,    16,    17,    18,  1038,  1666,   548,
      14,    15,    16,    17,    18,  1354,    85,  1668,  1478,   504,
      85,  1699,   504,   504,  1714,    84,  1715,  2057,  1717,  2145,
    1719,  1732,   521,   549,  1520,  1739,   521,   275,   452,   550,
    1740,   521,  1741,  1742,  1743,  1242,  1754,  1753,    85,   296,
     299,    85,  1767,  1756,  1757,  1758,  1764,  1774,  1795,  1779,
    1680,  1977,  1766,  1787,    84,   200,    84,   552,  1780,  1093,
     201,   202,  1783,  1797,  1798,   113,    85,   477,   639,   113,
     113,  1801,    85,    85,  1635,  1110,  1803,  1805,  1806,  1111,
    1807,  1810,   275,   113,  2183,   521,   744,  1672,  1824,   744,
     553,   271,   284,   452,   744,  1842,  1843,   521,   230,  1847,
    1849,   521,  1878,   744,   106,  1880,  1895,    85,  1894,  1902,
      84,   272,    95,  1898,  1899,  1905,   521,   903,  1910,  1915,
    1918,  1920,   744,  1919,  1947,  1952,   600,  1697,    84,    84,
    1968,   118,   146,  1973,   106,   118,   118,  1953,   275,  1966,
    1978,  1982,  1987,   108,  1995,  1992,   193,  2057,   117,   118,
     904,  2057,  2057,  1993,  1997,  2000,  2001,  2006,  2002,  2022,
    2030,   521,   146,   215,   554,   557,  2024,  2035,   106,  2037,
    2054,  2043,  2047,   108,  2055,  2065,  2066,  2077,  2101,  2111,
    2166,  2086,  2103,  2105,  2109,  2112,  2113,  2116,    87,    84,
    2117,   154,   293,  2133,  2144,  2129,   146,   294,   521,  2131,
     298,  2176,   303,   521,  2141,  2176,  2150,   108,  2156,   275,
     220,  2151,  2146,  2167,  2173,  2165,  2175,  2185,  2178,  2179,
    2189,  2188,  2192,   512,   905,  1907,  1080,  1037,  1081,   521,
    1082,  1571,   521,   618,   521,  1084,    85,    85,  1507,  1083,
     596,   804,   223,   275,  1515,  2174,    87,  1701,   275,  2123,
    1970,    85,  2140,   521,   275,  1723,  2120,  1963,    94,   262,
     272,    84,  1890,   197,  2107,  2046,  1876,  1877,  2148,  2180,
      84,  2106,   176,  1532,    87,   300,   589,   220,    95,  1697,
    2071,  2005,  1563,   308,  1200,  1903,  1665,   242,   275,   876,
     270,  1530,  1164,  1573,    87,     3,  1755,  1094,  1208,  1010,
     117,  1095,  1212,  1096,   964,  1809,  1215,     0,     0,  1697,
      85,     0,     0,     0,     0,     0,     0,     0,   468,    85,
       0,   188,     6,     7,     8,     9,    10,    11,    12,    13,
     117,     0,   154,     0,     0,     0,     0,     0,    87,     0,
     730,   154,     0,  1697,   332,   340,     0,     0,     0,     0,
       0,     0,    85,     0,     0,     0,  1391,   362,   293,     0,
    1392,     0,     0,   152,   117,   239,   240,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,  1407,
       0,     0,   459,   292,   197,   197,  1408,     0,     0,     0,
       0,    74,     0,   587,     0,   154,   492,     0,     0,     0,
       0,   270,     0,     0,     0,   293,     0,     0,     0,     0,
      94,     0,     0,  1671,    77,     0,     0,     0,     0,     0,
    1672,    85,   275,   332,    80,    81,     0,     0,   242,   242,
      95,     0,     0,     0,  1445,     0,    82,     0,  1446,     0,
      94,   730,  1447,   113,     0,     0,   623,    85,     0,   332,
     294,     0,   689,     0,   303,     0,     0,    87,     0,     0,
      95,     0,     0,   587,     0,     0,     0,     0,     0,     0,
       0,     0,   270,     0,    94,     0,   398,     0,     0,   637,
       0,    85,   152,     0,   922,   670,    65,    66,    67,    68,
      69,    70,    71,    72,    95,     0,     0,     0,     0,     0,
     729,     0,     0,     0,     0,   332,   275,     0,     0,   118,
       0,   340,     0,     0,     0,     0,     0,   340,   332,   332,
       0,     0,     0,     0,     0,     0,     0,   154,     0,   275,
       0,     0,     0,    85,     0,     0,     0,     0,     0,     0,
      85,     0,     0,   275,     0,    85,    85,    85,     0,   362,
     675,   684,   399,   152,     0,     0,   275,    65,    66,    67,
      68,    69,    70,    71,    72,   362,     0,     0,     0,   362,
     152,     0,   177,   178,    65,    66,    67,    68,    69,    70,
      71,    72,   220,     0,     0,  1458,     0,   275,     0,     0,
       0,     0,     0,     0,     0,   113,     0,     0,     0,  1482,
       0,   729,     0,     0,     0,     0,     0,     0,     0,    85,
       0,   275,   459,     0,    85,   670,     0,     0,   275,  1504,
     400,    85,   151,    85,     0,   113,   744,     0,     0,    14,
      15,    16,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,   836,     0,   838,     0,   459,     0,     0,   807,
       0,     0,     0,   855,   468,   345,     0,   197,     0,   113,
       0,   118,     0,   346,   347,   348,   349,     0,     0,     0,
       0,     0,     0,     0,     0,   154,     0,     0,     0,   492,
       0,   293,     0,   841,     0,   684,  1640,     0,     0,    58,
    1641,   118,     0,     0,  1642,     0,   154,     0,   209,     0,
     468,   468,     0,   623,   152,    58,   177,   178,    65,    66,
      67,    68,    69,    70,    71,    72,     0,   101,   459,   398,
     157,     0,    85,   152,     0,   118,   242,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,   242,   152,
      85,     0,    85,    65,    66,    67,    68,    69,    70,    71,
      72,    74,     0,    75,     0,     0,     0,     0,   350,     0,
       0,     0,   332,     0,   459,   459,     0,    74,   332,    75,
       0,   362,     0,    76,    77,   101,   351,     0,     0,   623,
      85,     0,     0,    85,    80,    81,     0,     0,     0,    76,
      77,   500,     0,     0,     0,   399,    82,     0,     0,     0,
      80,    81,  1760,   213,     0,     0,     0,   991,   623,     0,
       0,     0,    82,   152,     0,   177,   178,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   332,     0,   332,   468,
     209,    87,     0,  1197,     0,   154,     0,  1018,     0,     0,
       0,     0,     0,   468,     0,  1025,     0,   362,   492,     0,
     684,  1687,  1689,     0,     0,     0,  1796,   101,   675,     0,
       0,   323,   675,     0,     0,  1799,     0,   101,     0,  1800,
     213,   362,   188,     6,     7,     8,     9,    10,    11,    12,
      13,   684,     0,     0,   362,     0,    85,     0,   584,     0,
       0,     0,     0,   154,     0,     0,   275,     0,     0,     0,
       0,   459,  1751,     0,   459,     0,   154,   154,     0,   459,
       0,     0,     0,     0,     0,     0,     0,     0,   459,     0,
       0,   154,   154,   154,   487,    85,     0,  2053,     0,   626,
       0,     0,     0,     0,     0,     0,     0,   468,     0,     0,
       0,   633,     0,   623,   665,     0,   152,   688,   177,   178,
      65,    66,    67,    68,    69,    70,    71,    72,   584,   623,
     665,     0,     0,   623,   665,     0,     0,   101,     0,     0,
       0,     0,     0,     0,     0,     0,   623,   492,  1516,     0,
     661,  2094,     0,     0,   581,     0,   585,     0,     0,     0,
       0,     0,     0,   807,   807,     0,   101,     0,     0,    85,
      85,   459,     0,  1138,   648,     0,   152,   101,   177,   178,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   362,   492,     0,     0,    19,   841,     0,   841,   152,
     157,     0,   101,    65,    66,    67,    68,    69,    70,    71,
      72,   362,     0,   362,     0,   749,     0,   362,   362,   362,
     362,     0,     0,     0,     0,     0,   585,     0,     0,   522,
      85,     0,     0,     0,     0,     0,     0,   362,     0,    52,
      53,    54,    55,     0,   665,     0,   790,  1222,     0,  1402,
      77,  1517,     0,     0,     0,   468,     0,     0,     0,     0,
       0,     0,     0,   332,     0,     0,     0,     0,  1290,  1291,
    1875,     0,    82,     0,   830,     0,     0,     0,     0,   835,
       0,     0,     0,     0,   152,   154,   177,   178,    65,    66,
      67,    68,    69,    70,    71,    72,  1059,     0,     0,   861,
     862,   459,  2186,     0,   863,   864,   362,     0,   867,     0,
       0,  2193,     0,     0,   332,   154,     0,     0,     0,   459,
      58,     0,     0,     0,   880,     0,     0,   362,     0,  1311,
       0,     0,     0,     0,     0,     0,   500,     0,  1060,     0,
     675,     0,     0,     0,     0,   213,   909,     0,     0,     0,
       0,     0,     0,     0,   152,     0,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
    1364,     0,     0,     0,   832,     0,     0,     0,     0,     0,
     275,     0,    74,     0,    75,     0,     0,     0,     0,   154,
     492,     0,     0,     0,     0,   487,     0,     0,     0,     0,
       0,     0,     0,     0,  2049,    77,     0,     0,   557,   275,
     623,  1384,   665,   500,   623,    80,    81,     0,     0,     0,
       0,     0,     0,   623,   949,     0,     0,    82,     0,     0,
       0,     0,     0,   623,     0,     0,   665,     0,     0,     0,
     623,     0,     0,     0,     0,     0,     0,     0,     0,   665,
       0,     0,     0,     0,   807,     0,     0,     0,     0,     0,
    1410,   980,  1413,     0,     0,    58,     0,     0,     0,   362,
     362,     0,     0,   841,  1417,     0,  1419,     0,     0,     0,
     841,  1424,  1425,     0,     0,     0,   101,     0,   623,     0,
    2121,  1432,   623,     0,     0,     0,   623,     0,     0,   152,
       0,   239,   240,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,  1451,     0,     0,
    1454,     0,   866,     0,   522,   362,  1033,    74,     0,    75,
     101,     0,     0,     0,   957,     0,     0,     0,     0,     0,
       0,     0,   500,     0,     0,     0,     0,  1650,     0,   241,
      77,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,    81,     0,     0,     0,     0,   275,   154,     0,     0,
       0,     0,    82,     0,     0,     0,   154,     0,     0,     0,
       0,     0,     0,  1514,   152,   459,   665,   500,    65,    66,
      67,    68,    69,    70,    71,    72,  1055,   468,     0,     0,
       0,     0,     0,     0,   991,     0,     0,     0,     0,     0,
       0,   459,     0,  1535,   500,     0,     0,     0,  1112,   459,
    1113,     0,     0,     0,     0,   275,   835,     0,     0,     0,
       0,  1545,     0,  1546,     0,  1547,    58,     0,  1056,     0,
       0,     0,  1556,   270,    87,     0,     0,     0,     0,     0,
       0,  2142,     0,     0,  1157,     0,     0,     0,   332,     0,
       0,   154,     0,  1166,     0,     0,     0,  1168,   492,     0,
     152,     0,   239,   240,    65,    66,    67,    68,    69,    70,
      71,    72,     0,  2164,     0,     0,     0,     0,     0,     0,
     154,     0,    58,     0,     0,   120,     0,     0,   120,   492,
      75,     0,     0,     0,   154,     0,     0,     0,     0,     0,
       0,     0,   661,     0,     0,     0,  1609,  1610,     0,     0,
    1402,    77,   665,     0,     0,   688,   152,     0,   239,   240,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
     623,     0,  1632,    82,   623,     0,   199,     0,   623,  1637,
       0,  1638,     0,   120,    74,     0,    75,   152,     0,   177,
     178,    65,    66,    67,    68,    69,    70,    71,    72,   362,
     245,     0,   362,   362,     0,   362,   331,    77,     0,     0,
       0,   120,     0,     0,     0,   500,     0,    80,    81,     0,
     275,     0,     0,   468,     0,     0,     0,   277,     0,    82,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
    1342,     0,     0,     0,   957,     0,     0,   154,   154,   154,
     154,     0,   154,   154,     0,     0,     0,   334,  1673,   340,
     101,     0,  1491,     0,     0,   120,     0,     0,     0,   120,
       0,     0,   459,     0,   957,   120,   459,   459,   120,     0,
       0,     0,   277,     0,     0,     0,   623,   459,     0,     0,
     459,     0,     0,   358,   120,     0,   390,     0,     0,  1759,
       0,     0,     0,     0,   522,     0,  1763,   866,  1765,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   270,   463,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   492,   120,   463,     0,     0,   334,     0,   277,     0,
     623,   540,   245,     0,     0,     0,     0,     0,   487,   623,
       0,     0,     0,   623,   568,     0,   154,     0,     0,   675,
       0,     0,   334,     0,     0,     0,     0,     0,     0,   275,
       0,     0,     0,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,  1802,     0,     0,     0,     0,   468,
       0,     0,   120,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,   277,
       0,     0,     0,     0,     0,   120,     0,     0,   334,     0,
       0,     0,     0,     0,     0,   151,     0,     0,     0,     0,
     614,   640,   334,   120,     0,     0,     0,     0,   120,     0,
     120,     0,     0,   277,   120,     0,  1673,  1816,   277,     0,
       0,  1673,     0,   459,   277,     0,     0,     0,  1673,     0,
    1673,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   790,     0,     0,     0,     0,     0,
       0,     0,     0,   340,   154,     0,   120,     0,   277,   120,
    1371,     0,     0,   665,  1371,     0,     0,     0,     0,     0,
     275,     0,   120,     0,     0,     0,   120,     0,     0,     0,
       0,     0,  1896,  1897,     0,     0,     0,     0,     0,     0,
       0,     0,  1371,     0,   500,   522,   957,     0,  1544,     0,
       0,     0,     0,     0,     0,  1471,     0,     0,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,   152,   463,
     177,   178,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,   812,     0,     0,     0,     0,   154,
       0,     0,     0,     0,     0,     0,     0,     0,   101,     0,
       0,   824,     0,   463,   827,     0,     0,     0,     0,     0,
       0,  1371,     0,     0,     0,     0,   842,     0,     0,     0,
       0,     0,  1816,  1816,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,   463,  1673,     0,     0,
    1673,     0,   277,  1493,     0,     0,     0,     0,     0,     0,
       0,     0,   340,   120,     0,     0,     0,     0,     0,   882,
       0,     0,     0,     0,     0,     0,   568,     0,     0,   957,
     459,   245,     0,     0,     0,   463,     0,     0,     0,     0,
       0,     0,   152,     0,     0,   154,    65,    66,    67,    68,
      69,    70,    71,    72,  1368,   334,     0,     0,  1369,     0,
    1370,   334,     0,     0,     0,     0,     0,     0,     0,   152,
     120,   386,   387,    65,    66,    67,    68,    69,    70,    71,
      72,   463,   463,     0,     0,     0,   277,     0,   120,     0,
       0,     0,     0,    77,  1816,     0,  1590,     0,     0,     0,
       0,     0,     0,  1673,   120,   152,   500,   239,   240,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,   950,
       0,   334,    78,   277,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   388,     0,   277,     0,     0,     0,
     154,     0,  2050,   340,     0,     0,   120,     0,   120,     0,
       0,     0,   120,     0,     0,     0,  1471,  1471,  1471,   157,
    1659,  1660,  1664,   390,   120,   463,     0,   277,     0,     0,
     388,  1816,     0,     0,   522,   120,     0,     0,     0,     0,
       0,   101,   154,  1371,   623,   101,   101,     0,   120,     0,
       0,   277,     0,     0,     0,   614,     0,     0,   277,   101,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,   154,     0,     0,     0,  2050,  2050,   463,     0,
       0,   463,     0,   120,   120,     0,   463,     0,     0,     0,
       0,     0,     0,     0,     0,   463,     0,     0,   120,   120,
     120,     0,     0,     0,     0,     0,   154,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2154,     0,     0,     0,
       0,     0,     0,     0,     0,   957,     0,     0,     0,     0,
       0,  2163,     0,     0,     0,     0,   152,  2050,   177,   178,
      65,    66,    67,    68,    69,    70,    71,    72,   812,   812,
       0,     0,     0,     0,   463,     0,     0,     0,     0,  1105,
       0,     0,  1108,     0,     0,     0,     0,     0,     0,     0,
     120,   522,     0,     0,     0,     0,     0,     0,   463,  1117,
       0,   842,     0,     0,     0,     0,   120,     0,     0,     0,
     120,     0,     0,     0,     0,     0,     0,  1495,   120,   463,
       0,     0,     0,   120,   152,     0,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   120,     0,
     120,     0,     0,     0,   120,   120,   120,   120,   568,     0,
       0,     0,    74,   103,     0,  1183,   158,     0,     0,  1187,
       0,     0,     0,  1191,   120,    58,   334,     0,     0,     0,
       0,     0,     0,     0,   839,    77,     0,   522,   672,     0,
       0,     0,     0,     0,  1371,    80,   840,     0,     0,  1371,
    1371,  1371,     0,  1198,     0,     0,     0,    82,   674,   152,
       0,   239,   240,    65,    66,    67,    68,    69,    70,    71,
      72,   103,   120,     0,     0,     0,     0,   334,     0,     0,
     665,     0,     0,     0,   120,     0,     0,    74,   463,    75,
       0,     0,     0,   120,     0,     0,     0,     0,     0,   214,
       0,     0,   120,     0,     0,     0,   463,     0,     0,  1671,
      77,     0,     0,     0,   120,     0,  1313,   463,     0,   285,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
     152,     0,    82,     0,    65,    66,    67,    68,    69,    70,
      71,    72,  1368,     0,  1328,     0,  1369,     0,  1370,     0,
       0,     0,     0,   319,     0,     0,     0,   324,     0,     0,
       0,     0,   152,   103,   239,   240,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   665,   120,   463,     0,     0,
    2015,    77,   364,     0,  1790,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,   101,
       0,     0,     0,     0,     0,     0,     0,     0,  2045,   470,
       0,     0,  2049,    77,     0,     0,   557,     0,     0,   812,
     324,   498,     0,    80,    81,     0,   522,     0,     0,     0,
       0,     0,     0,     0,     0,    82,  1117,     0,     0,     0,
       0,     0,  1403,   842,  1371,     0,  1371,   120,     0,     0,
       0,   120,     0,     0,   551,     0,   120,   120,     0,     0,
     120,     0,     0,   319,     0,     0,     0,     0,     0,     0,
     120,     0,     0,     0,   575,     0,     0,   120,     0,   580,
     582,     0,   214,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   319,     0,  1435,     0,     0,  1439,     0,     0,
       0,  1443,     0,   319,     0,     0,   605,     0,     0,     0,
     607,     0,   120,     0,     0,   608,     0,     0,     0,     0,
       0,   619,     0,     0,     0,   120,   582,     0,   319,   120,
       0,   152,   632,   120,     0,    65,    66,    67,    68,    69,
      70,    71,    72,  1368,   641,     0,     0,  1369,     0,  1370,
       0,   101,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
       0,     0,   463,     0,   663,     0,     0,   687,     0,     0,
       0,   101,    77,     0,     0,  1792,     0,     0,     0,     0,
     694,     0,     0,     0,   694,     0,     0,     0,   463,     0,
       0,     0,     0,     0,     0,     0,   463,     0,     0,     0,
       0,     0,     0,     0,     0,   101,     0,     0,     0,     0,
       0,   334,     0,     0,     0,     0,     0,     0,     0,     0,
     277,   120,   152,     0,   239,   240,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,   463,     0,     0,     0,  1313,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,   120,     0,     0,
       0,     0,  1671,    77,   120,     0,   463,     0,     0,  1672,
       0,   120,     0,    80,    81,     0,     0,     0,     0,     0,
     324,     0,     0,     0,   663,    82,     0,     0,   152,  1605,
     611,   612,    65,    66,    67,    68,    69,    70,    71,    72,
    1614,   324,  1619,     0,     0,     0,     0,     0,     0,     0,
      58,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,   120,
     120,    78,   120,     0,   152,     0,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,     0,   120,   619,     0,
       0,   120,     0,   750,     0,   120,    78,   426,     0,     0,
       0,  1677,    74,     0,    75,     0,   498,  1651,     0,     0,
       0,     0,     0,     0,   120,   120,   120,   120,   120,   120,
     120,     0,   319,     0,   839,    77,   277,     0,   672,     0,
      14,    15,    16,    17,    18,    80,   840,     0,     0,   463,
       0,     0,     0,   463,   463,     0,     0,    82,     0,     0,
       0,     0,     0,     0,   463,  1488,     0,   463,     0,     0,
       0,     0,     0,     0,   619,     0,   103,  1502,     0,   152,
     158,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   694,   972,     0,   277,     0,     0,     0,     0,
      58,     0,     0,   619,     0,     0,     0,   983,   463,     0,
       0,     0,     0,   120,     0,     0,   663,     0,     0,     0,
       0,   992,     0,     0,     0,     0,  1770,     0,     0,   694,
       0,     0,    78,   120,   152,   114,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
     120,     0,     0,     0,  2049,    77,     0,     0,   557,     0,
       0,     0,     0,   114,     0,    80,    81,     0,     0,  1677,
       0,     0,  1819,     0,  1677,     0,    58,    82,     0,     0,
       0,  1831,     0,  1677,     0,     0,     0,     0,     0,     0,
       0,     0,   498,     0,     0,     0,     0,     0,     0,     0,
     463,     0,     0,     0,     0,     0,     0,     0,   619,  1097,
     152,   286,   239,   240,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,   619,     0,     0,     0,   619,     0,
     277,   120,     0,     0,     0,     0,   694,   972,    74,     0,
      75,   619,     0,  1123,     0,   114,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   114,   498,     0,   498,     0,
     241,    77,   498,   498,   364,   498,     0,     0,     0,     0,
       0,    80,    81,     0,   367,     0,     0,     0,     0,     0,
       0,     0,   498,    82,     0,     0,   152,     0,   239,   240,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,     0,   499,    74,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,  1744,  1745,  1819,  1819,     0,
     158,     0,     0,     0,     0,     0,   241,    77,     0,     0,
    1935,     0,   619,  1677,     0,     0,  1286,    80,    81,     0,
       0,   498,     0,     0,     0,   114,     0,     0,     0,    82,
     158,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,   694,     0,     0,  1315,     0,     0,     0,   277,
       0,     0,  1321,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   463,   114,     0,   463,   606,   152,
       0,   239,   240,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   120,   367,     0,     0,     0,     0,     0,     0,
     114,     0,     0,     0,   286,     0,     0,    74,     0,    75,
       0,     0,     0,     0,   324,   364,     0,     0,     0,  1819,
       0,     0,     0,     0,     0,     0,  1677,   152,     0,   331,
      77,    65,    66,    67,    68,    69,    70,    71,    72,  1368,
      80,    81,     0,  1369,     0,  1370,   664,     0,     0,   286,
       0,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   664,     0,     0,   334,   664,  1819,     0,     0,
       0,  2034,   277,     0,     0,     0,     0,     0,    77,     0,
       0,     0,     0,     0,  1848,   619,     0,   120,     0,   619,
     277,  1858,  2069,     0,   498,   498,  1819,     0,   619,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   619,     0,
       0,     0,     0,     0,     0,   619,     0,     0,     0,     0,
       0,     0,  1883,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   640,
     334,  1819,  1819,     0,     0,  2095,     0,     0,     0,     0,
     498,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,   619,     0,     0,     0,   619,     0,     0,
       0,   619,     0,     0,     0,     0,   664,     0,     0,     0,
     152,   120,   239,   240,    65,    66,    67,    68,    69,    70,
      71,    72,   158,   120,     0,     0,     0,     0,     0,     0,
     334,  1472,  1819,     0,     0,     0,     0,     0,    74,   152,
    1286,   206,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,  1937,  1938,     0,     0,
     331,    77,     0,  1948,     0,     0,     0,     0,     0,     0,
       0,    80,    81,     0,  1286,     0,  1962,     0,     0,     0,
     367,     0,     0,    82,     0,     0,  1971,     0,  1972,     0,
      77,     0,     0,   865,     0,     0,     0,     0,   499,  1531,
       0,  1986,     0,  1988,  1989,  1990,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,   663,     0,     0,     0,     0,     0,     0,
       0,     0,   580,   152,     0,   609,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   158,     0,     0,     0,     0,
       0,     0,   619,     0,   364,     0,   367,  2016,   114,     0,
       0,  2021,     0,   119,     0,     0,  2026,     0,     0,     0,
       0,     0,     0,     0,   664,   499,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   367,     0,  1088,     0,     0,
      14,    15,    16,    17,    18,     0,     0,   152,   664,   177,
     178,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,   664,     0,     0,     0,     0,     0,     0,     0,     0,
    2072,   119,     0,     0,   498,     0,     0,   498,   498,     0,
     364,     0,  2081,     0,     0,     0,  2084,     0,     0,     0,
       0,     0,     0,     0,     0,   619,     0,   510,     0,   619,
      58,  2100,     0,   619,     0,   152,     0,   177,   178,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,   288,
       0,     0,  1472,  1472,  1472,   158,   582,     0,     0,     0,
       0,     0,     0,     0,   152,     0,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,  2128,  1696,     0,     0,
       0,  1696,  1696,   119,   499,   515,     0,     0,     0,     0,
       0,     0,    74,   119,    75,  1696,     0,     0,     0,   152,
     367,   177,   178,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   371,  2149,  1671,    77,   367,     0,  2152,     0,
     367,     0,     0,     0,     0,    80,    81,     0,   664,   499,
       0,     0,     0,   367,     0,     0,   364,    82,     0,     0,
       0,   619,     0,     0,  2170,     0,     0,  2172,   367,  2152,
     367,   501,     0,     0,   367,   367,   499,   367,     0,     0,
       0,   158,     0,     0,     0,     0,     0,     0,  2172,     0,
       0,     0,     0,     0,   367,   791,   792,   793,   794,   795,
     796,   797,   798,   799,   800,   801,     0,     0,     0,     0,
     224,     0,     0,   119,     0,   619,     0,     0,     0,     0,
       0,     0,     0,     0,   619,     0,     0,     0,   619,     0,
       0,     0,     0,     0,     0,     0,   232,   802,     0,     0,
       0,   152,   119,   177,   178,    65,    66,    67,    68,    69,
      70,    71,    72,   119,   367,     0,     0,     0,   114,     0,
       0,     0,     0,   367,     0,     0,     0,     0,     0,     0,
       0,   371,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,   288,     0,   664,  1833,     0,   286,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1845,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   666,     0,     0,   288,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     666,     0,     0,     0,   666,     0,     0,   499,     0,   188,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
     437,     0,     0,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,   263,   264,     0,   265,    46,     0,    47,     0,
       0,   266,     0,     0,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,   367,     0,     0,
       0,   367,     0,     0,     0,     0,   367,   367,  1927,     0,
     367,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     367,     0,     0,     0,     0,     0,     0,   367,     0,     0,
       0,     0,     0,     0,   666,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1958,     0,     0,  1696,     0,     0,     0,     0,
       0,     0,   367,     0,     0,     0,     0,     0,     0,     0,
    1976,     0,     0,     0,     0,   367,     0,     0,     0,   367,
       0,     0,     0,   367,  -475,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -475,   371,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,   501,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,     0,     0,     0,
       0,   715,     0,   718,     0,  1976,   437,   723,     0,     0,
       0,     0,     0,     0,     0,     0,   732,   733,     0,     0,
       0,   286,     0,     0,   371,     0,   119,     0,     0,     0,
       0,   437,   437,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   666,   501,     0,   664,     0,  1696,     0,     0,
       0,     0,   437,   371,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   666,     0,     0,     0,
       0,     0,     0,     0,   367,     0,   499,  1696,     0,   666,
       0,     0,  2110,   192,     0,   437,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   619,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1696,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   273,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   295,     0,   302,     0,
     304,     0,     0,     0,     0,     0,   367,     0,     0,   367,
     367,     0,   367,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   367,     0,     0,
       0,   367,   501,     0,     0,   367,     0,     0,     0,   273,
       0,     0,   302,   304,     0,     0,     0,     0,   371,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   371,     0,     0,     0,   371,     0,
       0,     0,     0,     0,     0,     0,   666,   501,     0,   114,
       0,   371,     0,   114,   114,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   273,   371,   114,   371,     0,
       0,     0,   371,   371,   501,   371,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,     0,     0,
       0,     0,   371,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   499,     0,
       0,     0,     0,   367,     0,   174,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   273,     0,   302,   304,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   371,     0,     0,     0,   119,     0,     0,     0,
       0,   371,     0,     0,   174,     0,     0,   367,     0,     0,
     273,     0,     0,     0,     0,   273,   367,   174,     0,   174,
     367,   273,   666,     0,     0,   288,     0,     0,     0,     0,
     437,   437,   437,   437,   437,   437,   437,   437,   437,   437,
     437,   437,   437,   437,   437,   437,   437,   437,   437,     0,
     174,     0,   393,     0,     0,   273,     0,     0,     0,     0,
     690,     0,   304,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   393,     0,     0,
       0,     0,     0,     0,     0,   501,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   286,     0,     0,     0,     0,     0,   728,     0,     0,
       0,     0,     0,     0,     0,   437,     0,     0,     0,     0,
     174,     0,     0,     0,   174,     0,   172,   174,   174,     0,
       0,   174,     0,     0,   174,   174,     0,   174,     0,   174,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   371,     0,     0,     0,   371,
       0,   273,     0,     0,   371,   371,     0,     0,   371,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   371,     0,
       0,     0,     0,     0,     0,   371,     0,     0,     0,   273,
       0,   690,   304,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   306,     0,     0,     0,     0,   728,     0,
     174,     0,     0,   174,     0,     0,   312,     0,   313,     0,
     371,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   371,     0,     0,     0,   371,   174,     0,
       0,   371,   273,     0,     0,     0,     0,     0,     0,   385,
       0,     0,     0,   174,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   273,   114,     0,     0,
       0,   273,     0,   273,     0,     0,     0,     0,     0,     0,
     119,     0,   664,     0,     0,     0,     0,     0,     0,     0,
       0,   437,     0,     0,     0,     0,   273,   437,   273,   273,
       0,     0,     0,     0,     0,     0,     0,     0,   437,     0,
     273,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,   273,     0,     0,   562,   563,     0,     0,
     567,     0,   273,   570,   571,     0,   573,     0,   574,   288,
       0,     0,     0,     0,     0,     0,     0,     0,   437,     0,
       0,     0,     0,     0,   273,     0,   690,   304,   174,     0,
       0,     0,     0,   666,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   664,   273,   690,
       0,     0,     0,     0,     0,   273,     0,     0,     0,     0,
       0,     0,   371,     0,   501,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   393,   114,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,     0,     0,   659,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,   691,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   371,     0,     0,   371,   371,     0,
     371,   367,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,     0,   371,     0,     0,     0,   371,
       0,   437,     0,   371,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   393,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,   119,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,   823,   174,   174,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   437,     0,     0,     0,   174,     0,
     174,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   501,     0,     0,     0,
       0,   371,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   437,   437,   437,     0,     0,     0,     0,
     437,   437,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   906,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   437,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   371,     0,     0,     0,     0,
       0,     0,     0,   273,   371,     0,     0,     0,   371,     0,
       0,     0,     0,     0,   273,     0,     0,     0,     0,     0,
       0,     0,     0,   273,     0,   437,   437,     0,     0,   174,
     174,     0,     0,     0,     0,     0,   174,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   301,     0,     0,     0,
       0,   174,     0,     0,   174,   174,     0,   174,     0,   174,
     174,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   288,
       0,     0,     0,     0,     0,     0,     0,   986,   987,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,     0,   174,     0,     0,     0,   174,   996,     0,   998,
     174,     0,     0,     0,     0,     0,     0,     0,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
       0,     0,     0,     0,   273,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,   437,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
     174,    46,     0,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,  1099,  1100,
       0,     0,     0,     0,     0,  1104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
       0,     0,  1959,     0,     0,   119,     0,     0,     0,     0,
    1127,     0,     0,  1130,  1131,     0,  1134,     0,  1136,  1137,
     666,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,   374,   374,     0,  2007,     0,     0,
       0,  1182,     0,     0,     0,  1186,     0,   273,   374,  1190,
       0,     0,   374,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,     0,     0,     0,   437,   273,     0,     0,     0,
       0,     0,   273,     0,     0,   408,     0,     0,   409,     0,
     410,   411,     0,   412,     0,   666,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,     0,     0,   174,     0,   174,     0,
       0,   174,     0,     0,   174,     0,     0,     0,   174,  1322,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,   374,   423,   424,     0,     0,   119,   374,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   371,
       0,     0,     0,   425,     0,     0,    78,   426,     0,     0,
       0,   119,     0,   427,    80,    81,   428,   429,   430,   431,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   273,     0,     0,   374,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,     0,     0,     0,
    1322,     0,   437,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
       0,     0,   273,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,     0,  1429,   437,  1431,     0,     0,
    1434,     0,     0,  1438,     0,     0,     0,  1442,     0,     0,
     374,   374,     0,   374,     0,     0,   174,     0,     0,     0,
       0,   374,     0,     0,     0,     0,     0,   174,     0,     0,
     174,     0,   174,   174,   374,     0,     0,   374,     0,     0,
       0,     0,     0,     0,   374,     0,     0,   374,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     437,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   437,     0,   437,     0,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -500,  -500,   437,  -500,    46,     0,
      47,     0,     0,  -500,     0,     0,     0,   273,     0,     0,
     374,   174,     0,     0,     0,  1550,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,   374,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,     0,     0,   374,     0,     0,     0,
       0,     0,     0,     0,   374,   374,    63,    64,     0,   374,
       0,   437,     0,     0,     0,     0,     0,   378,     0,     0,
       0,     0,     0,     0,   374,     0,   374,     0,     0,     0,
     374,   374,   374,   374,    75,  1604,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1613,     0,     0,  1617,
     374,  1620,  1621,     0,     0,   488,   378,    78,     0,     0,
     174,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,     0,
       0,     0,   560,     0,   273,     0,     0,     0,     0,   560,
       0,     0,     0,     0,     0,     0,   273,     0,     0,     0,
     374,     0,     0,     0,     0,     0,     0,     0,     0,   374,
     174,     0,     0,     0,     0,     0,   174,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,     0,   374,   374,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1738,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   560,     0,     0,     0,     0,     0,     0,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   374,     0,     0,     0,     0,     0,   378,
     676,     0,     0,     0,     0,     0,     0,     0,     0,   273,
     304,     0,     0,     0,     0,     0,     0,     0,     0,   697,
       0,     0,     0,     0,     0,     0,     0,   273,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   174,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1617,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   174,   374,     0,     0,     0,   374,     0,   393,
       0,     0,   374,   374,   174,     0,   374,  1804,     0,     0,
       0,     0,     0,     0,     0,     0,   374,     0,     0,     0,
       0,     0,     0,   374,     0,     0,     0,     0,     0,   560,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   560,   825,     0,   560,
     828,     0,     0,     0,     0,     0,     0,     0,   374,   378,
       0,     0,     0,   676,   408,     0,     0,   409,     0,   410,
     411,   374,   412,     0,     0,   374,   488,     0,     0,   374,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,   273,     0,     0,     0,     0,     0,     0,   174,     0,
       0,     0,     0,     0,     0,     0,     0,   560,     0,     0,
       0,   560,     0,     0,     0,     0,     0,     0,   373,   414,
     415,  1892,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,     0,     0,     0,     0,     0,
      74,   378,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,     0,  1916,  1917,     0,     0,   174,
       0,     0,   425,     0,     0,    78,   426,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,     0,
    1931,  1932,     0,     0,     0,  1011,     0,     0,     0,     0,
       0,   374,     0,  1936,     0,   374,     0,   560,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,     0,
       0,     0,     0,     0,     0,     0,     0,   970,   378,     0,
     374,     0,   374,     0,     0,     0,     0,     0,   676,     0,
       0,     0,   676,     0,     0,     0,     0,     0,     0,   988,
       0,   378,     0,     0,     0,     0,     0,   373,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -500,  -500,     0,  -500,    46,  2004,    47,     0,
       0,  -500,   374,     0,     0,   374,   374,     0,   374,     0,
     373,     0,   373,   373,     0,     0,     0,     0,    58,     0,
       0,     0,     0,   374,     0,     0,   373,   374,     0,     0,
     373,   374,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,   378,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2068,     0,
       0,     0,     0,   560,   560,     0,     0,     0,     0,     0,
      74,     0,    75,     0,   560,  1106,     0,   560,  1109,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   970,   378,     0,     0,    78,   676,     0,   676,   676,
       0,     0,     0,    80,    81,   676,     0,     0,     0,     0,
       0,   378,     0,   378,     0,     0,     0,   378,   378,   378,
     378,     0,     0,     0,   374,     0,     0,     0,     0,   374,
     373,     0,     0,     0,     0,     0,   373,   378,     0,   560,
       0,     0,     0,   560,     0,     0,     0,     0,     0,     0,
     560,  1184,     0,     0,   560,  1188,     0,     0,   560,  1192,
       0,     0,     0,     0,     0,  1195,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,     0,     0,   374,     0,     0,     0,
       0,     0,   375,     0,   373,     0,   378,   560,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     676,   375,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,     0,     0,     0,     0,   488,
     378,     0,     0,     0,     0,     0,     0,     0,   373,   373,
       0,   373,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,   373,     0,     0,     0,     0,
       0,     0,   373,     0,     0,   373,     0,     0,     0,     0,
       0,   375,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   560,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   378,
     378,     0,     0,   676,   676,     0,     0,     0,     0,     0,
     676,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,   375,   375,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     375,     0,     0,     0,   375,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   378,     0,     0,   373,   560,
    1436,     0,   560,  1440,     0,     0,   560,  1444,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,     0,
     373,     0,     0,     0,   373,     0,     0,     0,     0,     0,
       0,     0,   373,   373,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,   373,     0,     0,     0,   373,   373,
     373,   373,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,     0,   375,     0,     0,     0,     0,     0,
     375,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,     0,     0,   374,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   378,     0,
       0,     0,     0,     0,   676,  1552,     0,     0,   373,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,   378,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
     373,   373,     0,     0,     0,     0,   375,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   560,  1606,     0,   374,     0,     0,
       0,   375,     0,     0,     0,   560,  1615,     0,   676,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   378,
       0,   373,   378,   378,   375,   378,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,   375,     0,   375,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,   375,
       0,     0,     0,     0,     0,     0,   375,     0,     0,   375,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,   373,     0,     0,     0,     0,
     373,   373,     0,     0,   373,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   743,     0,
       0,     0,   219,     0,     0,     0,     0,     0,     0,     0,
       0,   378,     0,     0,     0,     0,   373,     0,   281,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,   375,   373,     0,     0,     0,   373,     0,   676,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,     0,   375,   219,
       0,     0,     0,   342,     0,     0,   375,   375,     0,     0,
       0,   375,     0,     0,     0,   383,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,   375,     0,
       0,     0,   375,   375,   375,   375,     0,     0,     0,     0,
     219,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,     0,   508,     0,     0,   560,     0,   514,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   560,     0,     0,     0,     0,     0,
     917,   919,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,   373,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,     0,     0,   219,     0,     0,   373,     0,
     373,   375,     0,     0,     0,   461,     0,     0,     0,     0,
     281,     0,     0,     0,     0,     0,     0,     0,     0,   493,
       0,     0,   375,     0,   375,   375,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   523,     0,   523,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   514,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   219,     0,     0,     0,     0,
     373,     0,     0,   373,   373,     0,   373,     0,     0,     0,
       0,     0,     0,     0,     0,   375,     0,   669,     0,   686,
       0,   373,   560,   560,     0,   373,     0,   743,     0,   373,
     743,     0,     0,     0,     0,   743,     0,     0,   560,     0,
       0,     0,     0,     0,   743,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   743,     0,     0,     0,     0,     0,   638,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     747,     0,     0,     0,     0,   375,     0,     0,     0,   375,
       0,     0,     0,     0,   375,   375,     0,     0,   375,  1086,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,     0,     0,     0,   219,   375,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,   560,     0,     0,   373,     0,     0,
       0,     0,   560,     0,     0,     0,     0,   669,     0,     0,
     375,     0,     0,   853,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,     0,   375,     0,     0,
       0,   375,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   560,     0,     0,     0,   219,     0,     0,     0,
       0,   373,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,   373,     0,     0,   560,  2070,     0,
       0,   560,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   219,   219,     0,     0,     0,     0,     0,   508,
       0,     0,   180,   183,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   560,   560,     0,     0,
     523,     0,     0,     0,     0,     0,   523,     0,     0,     0,
       0,   461,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   233,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,     0,   375,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   508,   560,   974,     0,
       0,     0,   375,     0,   375,     0,     0,     0,     0,     0,
       0,     0,   326,     0,     0,   327,     0,     0,     0,   669,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     352,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   219,     0,     0,     0,     0,     0,     0,     0,   219,
     401,     0,   747,   948,   747,   219,     0,   219,     0,     0,
       0,     0,   401,     0,     0,     0,   747,     0,     0,   747,
     747,   747,     0,     0,   375,     0,     0,   375,   375,     0,
     375,   493,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   982,   375,   532,     0,     0,   375,
       0,     0,     0,   375,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   508,     0,     0,     0,     0,
       0,  1015,     0,     0,     0,     0,     0,     0,   233,     0,
       0,     0,     0,     0,  1026,     0,   373,   593,   594,   219,
       0,     0,     0,     0,     0,     0,     0,     0,   180,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1047,  1049,
     508,     0,  1051,   180,  1053,     0,     0,     0,     0,     0,
    1015,     0,  1063,  1015,     0,     0,     0,     0,     0,   508,
       0,   508,     0,     0,     0,   508,   508,   383,   508,     0,
       0,     0,     0,     0,     0,     0,   375,     0,   643,     0,
    1090,   375,     0,     0,     0,   508,   647,   649,     0,     0,
       0,   656,     0,  1092,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1101,     0,     0,     0,     0,     0,
       0,   373,     0,     0,   373,     0,     0,   743,     0,     0,
     493,     0,     0,     0,     0,  1090,     0,     0,     0,     0,
     352,     0,     0,   352,     0,   375,   401,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,     0,   375,     0,
       0,     0,     0,     0,   508,     0,     0,  1160,     0,     0,
     523,     0,     0,     0,     0,     0,     0,   219,     0,     0,
       0,  1171,     0,     0,     0,     0,     0,   853,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1196,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   233,     0,     0,     0,     0,     0,     0,   383,     0,
       0,     0,     0,   858,   859,     0,     0,     0,     0,     0,
       0,     0,   461,     0,  1463,     0,  1464,     0,     0,     0,
       0,  1465,  1312,  1314,    14,    15,    16,    17,    18,    19,
     493,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,   508,   508,     0,
       0,     0,     0,  1090,    58,  1466,     0,     0,     0,     0,
       0,  1356,     0,     0,     0,     0,     0,     0,     0,     0,
    1015,     0,     0,     0,     0,     0,     0,     0,  1690,  1698,
       0,     0,  1690,  1709,     0,     0,     0,    61,  1716,     0,
      63,    64,  1720,     0,  1722,     0,  1709,     0,     0,     0,
       0,     0,     0,   508,     0,     0,     0,     0,     0,     0,
       0,   523,   953,     0,     0,     0,    74,     0,    75,     0,
       0,   352,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1467,     0,     0,
     375,    78,  1021,     0,     0,     0,     0,     0,     0,    80,
      81,     0,     0,     0,   747,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   401,     0,     0,     0,
       0,     0,     0,     0,   523,     0,  1428,     0,     0,   747,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   281,     0,     0,     0,     0,     0,  1061,     0,     0,
       0,     0,     0,     0,     0,   375,     0,     0,   375,   219,
    1808,     0,     0,     0,     0,     0,   669,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1505,  1505,
       0,     0,     0,     0,     0,     0,     0,   383,     0,     0,
       0,     0,   747,     0,     0,  1844,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1864,  1866,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1140,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   375,
    1156,     0,     0,     0,     0,  1548,  1886,     0,     0,     0,
       0,  1557,     0,     0,     0,     0,     0,   508,     0,     0,
     508,   508,     0,   383,     0,     0,     0,  1015,     0,     0,
       0,     0,   493,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     523,     0,     0,  1586,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   747,   747,   747,  1047,     0,
     747,   747,     0,     0,     0,     0,     0,   514,     0,  1224,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   219,     0,     0,     0,     0,
    1946,     0,     0,     0,     0,     0,     0,     0,  1949,     0,
    1951,     0,     0,  1955,  1961,     0,  1709,     0,     0,     0,
       0,  1967,     0,     0,     0,     0,   281,     0,     0,     0,
    1331,     0,  1648,  1649,     0,     0,     0,     0,     0,   383,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1015,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   523,     0,     0,
     461,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2032,     0,     0,     0,     0,     0,     0,  2039,  2041,     0,
       0,     0,     0,     0,  1049,     0,     0,     0,     0,     0,
       0,     0,     0,  1761,  1762,     0,     0,     0,     0,     0,
    2063,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1776,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   219,     0,   523,     0,     0,     0,  1047,     0,     0,
       0,     0,     0,     0,     0,     0,  2085,     0,  2088,     0,
       0,  2090,  2092,     0,   268,     0,     0,     0,  2097,  2099,
       0,   281,     0,     0,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -500,  -500,
       0,  -500,    46,     0,    47,     0,     0,  -500,     0,     0,
       0,     0,     0,     0,     0,     0,   461,     0,     0,     0,
       0,     0,  1832,     0,    58,  2135,  2137,  2139,     0,     0,
    1492,  1494,  1496,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   747,     0,     0,
       0,     0,     0,  2158,  2160,  2162,     0,     0,     0,     0,
      63,    64,     0,     0,  1518,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1879,  2171,  1224,    74,     0,    75,     0,
       0,  1537,     0,     0,     0,  1538,     0,     0,     0,     0,
       0,  1487,     0,     0,     0,     0,     0,     0,     0,     0,
     281,    78,   511,     0,     0,     0,     0,     0,     0,    80,
      81,     0,   523,     0,     0,     0,     0,     0,     0,     0,
    1906,     0,   408,  1908,     0,   409,     0,   410,   411,     0,
     412,     0,     0,   747,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1227,     0,   413,  1229,  1922,
    1230,  -253,  -253,  1231,  1232,  1233,  1234,  1235,  1236,  1237,
    1238,  1239,  1240,  1241,  1242,  -352,  -352,  1243,  1244,  1245,
    1246,  1247,  1248,  1249,     0,  1250,     0,   414,   415,     0,
     517,   417,  1251,  1252,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,  1253,   420,   421,   422,     0,
     423,   424,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2171,
       0,     0,     0,     0,     0,     0,     0,     0,   747,  -253,
    1254,   514,     0,    78,   426,     0,  1487,     0,   310,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,     0,  -193,     0,     0,     0,     0,
       0,  1683,  1684,     0,     0,     0,     0,   408,     0,     0,
     409,     0,   410,   411,     0,   412,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1227,     0,   413,  1229,     0,  1230,  -254,  -254,  1231,  1232,
    1233,  1234,  1235,  1236,  1237,  1238,  1239,  1240,  1241,  1242,
    -352,  -352,  1243,  1244,  1245,  1246,  1247,  1248,  1249,     0,
    1250,     0,   414,   415,  1015,   517,   417,  1251,  1252,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
    1253,   420,   421,   422,     0,   423,   424,     0,     0,  1771,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1882,     0,     0,     0,     0,
       0,     0,     0,     0,  -254,  1254,     0,     0,    78,   426,
       0,     0,  1487,   310,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,     0,     0,     0,     0,     0,
    -193,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,     0,     0,   409,     0,   410,   411,
       0,   412,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1227,     0,   413,  1229,
       0,  1230,     0,     0,  1231,  1232,  1233,  1234,  1235,  1236,
    1237,  1238,  1239,  1240,  1241,  1242,  -352,  -352,  1243,  1244,
    1245,  1246,  1247,  1248,  1249,     0,  1250,  1841,   414,   415,
       0,   517,   417,  1251,  1252,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,  1253,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1254,     0,     0,    78,   426,     0,     0,     0,   310,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,     0,  -193,     4,   188,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,  1226,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   408,     0,    46,   409,    47,   410,   411,
       0,   412,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,  1227,    58,  1228,  1229,
       0,  1230,     0,     0,  1231,  1232,  1233,  1234,  1235,  1236,
    1237,  1238,  1239,  1240,  1241,  1242,  -352,  -352,  1243,  1244,
    1245,  1246,  1247,  1248,  1249,     0,  1250,     0,   414,   415,
      61,   517,   417,  1251,  1252,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,  1253,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -3,  1254,     0,     0,    78,  1255,     0,     0,     0,   310,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,     0,  -193,     4,   188,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,  1226,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   408,     0,    46,   409,    47,   410,   411,
       0,   412,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,  1227,    58,  1228,  1229,
       0,  1230,     0,     0,  1231,  1232,  1233,  1234,  1235,  1236,
    1237,  1238,  1239,  1240,  1241,  1242,  -352,  -352,  1243,  1244,
    1245,  1246,  1247,  1248,  1249,     0,  1250,     0,   414,   415,
      61,   517,   417,  1251,  1252,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,  1253,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1254,     0,     0,    78,  1255,     0,     0,     0,   310,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,     0,  -193,     4,   188,     6,
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
       0,     0,     0,     0,  1702,  1703,  1704,  1705,     0,     0,
       0,   425,  1706,  1707,    78,  1255,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,     0,  1708,     4,   188,     6,
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
       0,     0,     0,     0,  1702,  1703,  1704,  1705,     0,     0,
       0,   425,  1706,     0,    78,  1255,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,     0,  1708,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,    59,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    60,     0,     0,     0,
      61,    62,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,     0,    78,    79,     0,     0,     0,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    82,     0,    83,   268,   188,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -500,  -500,     0,  -500,    46,     0,    47,     0,     0,
    -500,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   152,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,     0,    78,   269,     0,     0,     0,  -828,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    82,   268,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -500,
    -500,     0,  -500,    46,     0,    47,     0,     0,  -500,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   152,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
      77,     0,    78,   269,     0,     0,     0,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    82,   188,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   359,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   152,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,   617,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1116,    77,  -694,    78,
     672,     0,     0,     0,     0,     0,     0,    80,    81,     0,
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
       0,  -832,     0,     0,    80,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    82,   188,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -500,  -500,     0,  -500,    46,     0,    47,     0,     0,  -500,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     152,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,   269,     0,     0,     0,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    82,     4,   188,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     408,     0,    46,   409,    47,   410,   411,     0,   412,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,   413,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   414,   415,    61,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,     0,   423,   424,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   425,     0,
    1700,    78,  1255,     0,     0,     0,     0,     0,   427,    80,
      81,   428,   429,   430,   431,     4,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   408,     0,    46,   409,    47,   410,   411,     0,   412,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   413,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   414,   415,    61,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,     0,   423,
     424,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   425,
       0,     0,    78,  1255,     0,     0,     0,     0,     0,   427,
      80,    81,   428,   429,   430,   431,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   408,     0,    46,   409,    47,   410,   411,     0,   412,
     359,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   413,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,     0,   423,
     424,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   425,
       0,     0,    78,   490,     0,     0,     0,     0,     0,   427,
     491,    81,   428,   429,   430,   431,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   408,     0,    46,   409,    47,   410,   411,     0,   412,
     359,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   413,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,     0,   423,
     424,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   425,
       0,     0,    78,  1309,     0,     0,     0,     0,     0,   427,
    1310,    81,   428,   429,   430,   431,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   408,     0,    46,   409,    47,   410,   411,     0,   412,
     359,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   413,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,     0,   423,
     424,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   425,
       0,     0,    78,   837,     0,     0,     0,     0,     0,   427,
     491,    81,   428,   429,   430,   431,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   408,     0,    46,   409,    47,   410,   411,     0,   412,
     359,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   413,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,     0,   423,
     424,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   425,
       0,     0,    78,   426,     0,     0,     0,     0,     0,   427,
      80,    81,   428,   429,   430,   431,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   408,     0,    46,   409,    47,   410,   411,     0,   412,
     359,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   413,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,     0,   423,
     424,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   425,
       0,     0,    78,   837,     0,     0,     0,     0,     0,   427,
      80,    81,   428,   429,   430,   431,  2014,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,     0,     0,    -2,     0,     0,    -2,     0,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,    -2,    -2,  2044,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,     0,     0,    -2,     0,  1320,    -2,     0,     0,
       0,     0,    -2,    -2,    14,    15,    16,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
     408,     0,     0,   409,     0,   410,   411,     0,   412,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,    -2,     0,     0,    58,   413,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,    -2,    -2,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,     0,   423,   424,
       0,  1554,     0,     0,     0,     0,    74,     0,    75,    14,
      15,    16,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   425,     0,
       0,    78,   426,     0,     0,     0,     0,     0,   427,   491,
      81,   428,   429,   430,   431,   408,     0,     0,   409,     0,
     410,   411,     0,   412,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
     413,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,     0,   423,   424,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   425,     0,     0,    78,   426,     0,     0,
       0,     0,     0,   427,  1555,    81,   428,   429,   430,   431,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   152,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,     0,    78,    79,     0,
       0,     0,  -830,     0,     0,    80,    81,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,    82,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   152,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,    77,     0,    78,   211,     0,     0,
       0,     0,     0,     0,    80,    81,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,    82,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   152,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,    79,     0,     0,     0,
       0,     0,     0,    80,    81,     0,     0,    14,    15,    16,
      17,    18,     0,     0,    20,    82,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -500,  -500,     0,  -500,    46,     0,    47,     0,     0,
    -500,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   152,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,     0,    78,   511,     0,     0,     0,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    82,     4,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,     0,     0,     0,     0,
    -419,  -419,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -419,     0,
       0,     0,    78,    79,     0,     0,     0,     0,     0,     0,
      80,    81,     4,   188,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,     0,     0,     0,     0,  -420,  -420,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -420,     0,     0,     0,    78,
      79,     0,  1463,     0,  1464,     0,     0,    80,    81,  1465,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,  1466,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1656,     0,     0,     0,    78,
    1021,     0,  1463,     0,  1464,     0,     0,    80,    81,  1465,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,  1466,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1657,     0,     0,     0,    78,
    1021,     0,  1463,     0,  1464,     0,     0,    80,    81,  1465,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,  1466,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1658,     0,     0,     0,    78,
    1021,     0,     0,     0,     0,     0,     0,    80,    81,   268,
     188,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -500,  -500,     0,  -500,    46,     0,    47,
       0,     0,  -500,     0,   188,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,     0,   359,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   269,     0,    63,
      64,     0,     0,     0,    80,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,   617,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   671,     0,  -694,
      78,   672,     0,     0,     0,     0,     0,     0,    80,    81,
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
       0,    74,     0,    75,   617,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   806,     0,  -694,    78,   557,     0,     0,
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
    1149,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -702,    78,   923,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   360,    78,   361,
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
       0,    75,  1627,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   923,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,    74,     0,    75,  1629,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,    78,   511,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   361,
       0,     0,     0,     0,     0,     0,    80,    81,   188,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -500,  -500,     0,  -500,    46,     0,    47,     0,     0,
    -500,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
    1487,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   408,     0,     0,   409,     0,   410,   411,     0,   412,
       0,     0,     0,     0,    78,   269,     0,     0,     0,     0,
       0,     0,    80,    81,  1227,     0,   413,  1229,     0,  1230,
    1939,  1940,  1231,  1232,  1233,  1234,  1235,  1236,  1237,  1238,
    1239,  1240,  1241,  1242,     0,     0,  1243,  1244,  1245,  1246,
    1247,  1248,  1249,     0,  1250,     0,   414,   415,     0,   517,
     417,  1251,  1252,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,  1253,   420,   421,   422,     0,   423,
     424,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,  1487,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1254,
       0,     0,    78,   426,     0,     0,     0,   310,     0,   427,
      80,    81,   428,   429,   430,   431,     0,   408,     0,     0,
     409,     0,   410,   411,  -193,   412,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1227,     0,   413,  1229,     0,  1230,     0,     0,  1231,  1232,
    1233,  1234,  1235,  1236,  1237,  1238,  1239,  1240,  1241,  1242,
       0,     0,  1243,  1244,  1245,  1246,  1247,  1248,  1249,     0,
    1250,     0,   414,   415,     0,   517,   417,  1251,  1252,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
    1253,   420,   421,   422,     0,   423,   424,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1254,     0,     0,    78,   426,
       0,     0,     0,   310,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,     0,     0,     0,     0,     0,
    -193,   314,   188,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -423,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
       0,     0,     0,  -423,   314,   188,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -424,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,     0,     0,  -424,   314,   188,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,    14,
      15,    16,    17,    18,    19,   734,    20,   735,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    63,    64,   408,     0,    46,   409,    47,
     410,   411,     0,   412,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     413,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   736,     0,     0,     0,     0,  1242,     0,  -352,
       0,     0,     0,     0,    78,     0,     0,     0,     0,  -423,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,     0,   423,   424,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1254,     0,     0,    78,   737,     0,     0,
       0,   310,     0,   427,    80,    81,   738,   739,   430,   431,
      14,    15,    16,    17,    18,    19,   734,    20,   735,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   408,     0,    46,   409,
      47,   410,   411,     0,   412,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   413,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   736,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   414,   415,     0,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,     0,   423,   424,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   425,     0,     0,    78,   737,     0,
       0,     0,   310,     0,   427,    80,    81,   738,   739,   430,
     431,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   408,     0,    46,
     409,    47,   410,   411,     0,   412,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   413,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,     0,   423,   424,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   425,     0,   456,    78,   457,
       0,     0,     0,     0,     0,   427,    80,    81,   428,   429,
     430,   431,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   408,     0,
      46,   409,    47,   410,   411,     0,   412,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   413,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,     0,   423,   424,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   425,     0,     0,    78,
     457,     0,     0,     0,   310,     0,   427,    80,    81,   428,
     429,   430,   431,    14,    15,    16,    17,    18,    19,     0,
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
       0,     0,     0,     0,     0,     0,     0,   425,     0,     0,
      78,   737,     0,     0,     0,   310,     0,   427,    80,    81,
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
       0,    78,   457,     0,     0,     0,     0,     0,   427,    80,
      81,   428,   429,   430,   431,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   408,     0,    46,   409,    47,   410,   411,     0,   412,
     359,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   413,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,     0,   423,
     424,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   425,
       0,     0,    78,   837,     0,     0,     0,     0,     0,   427,
      80,    81,   428,   429,   430,   431,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   408,     0,    46,   409,    47,   410,   411,     0,
     412,   359,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   413,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,   408,
     423,   424,   409,     0,   410,   411,     0,   412,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,   413,     0,     0,     0,     0,     0,
     425,     0,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,     0,   423,   424,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1702,  1703,  1704,  1705,     0,     0,     0,   425,  1954,     0,
      78,   426,     0,     0,     0,     0,     0,   427,    80,    81,
     428,   429,   430,   431,   188,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,   268,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,    58,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -500,
    -500,     0,  -500,    46,     0,    47,     0,   725,  -500,   726,
     727,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,   -17,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,   188,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,    78,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,   359,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   152,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   617,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -694,    78,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   359,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   152,     0,   484,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   860,     0,     0,    78,
     485,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   152,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     152,     0,   484,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   485,     0,     0,     0,     0,     0,
       0,    80,    81,   188,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   359,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   617,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -694,    78,
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
       0,     0,     0,    75,  1220,     0,     0,     0,     0,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,    78,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,   359,    49,    50,    51,    52,    53,    54,
      55,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   860,     0,     0,    78,
     485,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,   359,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   860,     0,     0,    78,   485,     0,    63,
      64,     0,     0,     0,    80,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1028,
      78,  1021,     0,     0,     0,     0,     0,     0,    80,    81,
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
       0,     0,     0,  1209,     0,     0,     0,    78,    79,     0,
      63,    64,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1297,     0,     0,
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
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1456,     0,     0,     0,    78,    79,
       0,    63,    64,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1568,     0,
       0,     0,    78,    79,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,  1574,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
    1021,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1775,     0,     0,     0,    78,    79,     0,    63,
      64,     0,     0,     0,    80,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   322,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,     0,   359,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   211,     0,
      63,    64,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   361,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,   359,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,     0,
     359,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   322,
       0,    63,    64,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   485,     0,     0,     0,     0,     0,     0,
      80,    81,   188,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -500,  -500,     0,  -500,    46,
       0,    47,     0,     0,  -500,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,    79,     0,     0,     0,     0,     0,     0,
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
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     511,     0,    63,    64,     0,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,  1021,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,     0,   359,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   485,     0,    63,    64,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,  1021,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,   359,
      49,    50,    51,    52,    53,    54,    55,     0,     0,    14,
      15,    16,    17,    18,    58,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -500,  -500,     0,  -500,    46,     0,    47,
      63,    64,  -500,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,     0,    63,    64,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   339,     0,    14,
      15,    16,    17,    18,    80,    81,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -500,  -500,     0,  -500,    46,     0,    47,
       0,     0,  -500,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   511,     0,     0,
       0,     0,     0,     0,    80,    81,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   408,     0,    46,   409,    47,
     410,   411,     0,   412,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,     0,   423,   424,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   425,     0,     0,    78,   426,     0,     0,
       0,     0,     0,   427,   491,    81,   428,   429,   430,   431,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   408,
       0,    46,   409,    47,   410,   411,     0,   412,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,     0,   423,   424,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   425,     0,     0,
      78,   426,     0,     0,     0,     0,     0,   427,    80,    81,
     428,   429,   430,   431,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   152,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,    78,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,     0,   359,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,     0,   188,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,    78,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,    58,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
     189,   408,   190,   191,   409,     0,   410,   411,     0,   412,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,   413,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   725,     0,   726,   727,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,     0,   423,
     424,     0,    75,   408,     0,     0,   409,    74,   410,   411,
       0,   412,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1702,  1703,  1704,  1705,     0,     0,   413,   425,
    1865,     0,    78,   426,     0,     0,     0,     0,     0,   427,
      80,    81,   428,   429,   430,   431,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   517,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
     408,   423,   424,   409,     0,   410,   411,     0,   412,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   413,     0,     0,     0,     0,
       0,   425,    77,     0,   518,   519,     0,     0,     0,   520,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,   408,   423,   424,
     409,     0,   410,   411,     0,   412,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   413,     0,     0,     0,     0,     0,   425,  1359,
       0,    78,   426,     0,     0,     0,  1360,     0,   427,    80,
      81,   428,   429,   430,   431,     0,     0,     0,     0,     0,
       0,     0,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,   408,   423,   424,   409,     0,   410,
     411,     0,   412,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,     0,     0,     0,     0,   425,     0,     0,    78,   426,
       0,     0,     0,   520,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,   408,   423,   424,   409,     0,   410,   411,     0,   412,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   413,     0,     0,     0,
       0,     0,   425,  1046,     0,    78,   426,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,   408,   423,
     424,   409,     0,   410,   411,     0,   412,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   413,     0,     0,     0,     0,     0,   425,
       0,     0,    78,   426,     0,     0,     0,   310,     0,   427,
      80,    81,   428,   429,   430,   431,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,   408,   423,   424,   409,     0,
     410,   411,     0,   412,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,     0,     0,   425,     0,     0,    78,
     426,     0,     0,  1085,     0,     0,   427,    80,    81,   428,
     429,   430,   431,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,   408,   423,   424,   409,     0,   410,   411,     0,
     412,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,   425,     0,     0,    78,   426,     0,     0,
       0,  1497,     0,   427,    80,    81,   428,   429,   430,   431,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,   408,
     423,   424,   409,     0,   410,   411,     0,   412,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
     425,  1585,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,   408,   423,   424,   409,
       0,   410,   411,     0,   412,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   413,     0,     0,     0,     0,     0,   425,     0,     0,
      78,   426,     0,     0,     0,  1772,     0,   427,    80,    81,
     428,   429,   430,   431,     0,     0,     0,     0,     0,     0,
       0,   414,   415,     0,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,   408,   423,   424,   409,     0,   410,   411,
       0,   412,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   413,     0,
       0,     0,     0,     0,   425,     0,  1945,    78,   426,     0,
       0,     0,     0,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
     408,   423,   424,   409,     0,   410,   411,     0,   412,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   413,     0,     0,     0,     0,
       0,   425,  1950,     0,    78,   426,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,     0,   423,   424,
       0,     0,   408,     0,     0,   409,    74,   410,   411,     0,
     412,  2031,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,   425,  1960,
       0,    78,   426,     0,     0,     0,     0,     0,   427,    80,
      81,   428,   429,   430,   431,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,   408,
     423,   424,   409,     0,   410,   411,     0,   412,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
     425,     0,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,   408,   423,   424,   409,
       0,   410,   411,     0,   412,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   413,     0,     0,     0,     0,     0,   425,  2038,     0,
      78,   426,     0,     0,     0,     0,     0,   427,    80,    81,
     428,   429,   430,   431,     0,     0,     0,     0,     0,     0,
       0,   414,   415,     0,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,   408,   423,   424,   409,     0,   410,   411,
       0,   412,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   413,     0,
       0,     0,     0,     0,   425,  2040,     0,    78,   426,     0,
       0,     0,     0,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
     408,   423,   424,   409,     0,   410,   411,     0,   412,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   413,     0,     0,     0,     0,
       0,   425,  2087,     0,    78,   426,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,   408,   423,   424,
     409,     0,   410,   411,     0,   412,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   413,     0,     0,     0,     0,     0,   425,  2089,
       0,    78,   426,     0,     0,     0,     0,     0,   427,    80,
      81,   428,   429,   430,   431,     0,     0,     0,     0,     0,
       0,     0,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,   408,   423,   424,   409,     0,   410,
     411,     0,   412,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,     0,     0,     0,     0,   425,  2091,     0,    78,   426,
       0,     0,     0,     0,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,   408,   423,   424,   409,     0,   410,   411,     0,   412,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   413,     0,     0,     0,
       0,     0,   425,  2096,     0,    78,   426,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,   408,   423,
     424,   409,     0,   410,   411,     0,   412,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   413,     0,     0,     0,     0,     0,   425,
    2098,     0,    78,   426,     0,     0,     0,     0,     0,   427,
      80,    81,   428,   429,   430,   431,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,   408,   423,   424,   409,     0,
     410,   411,     0,   412,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,     0,     0,   425,  2134,     0,    78,
     426,     0,     0,     0,     0,     0,   427,    80,    81,   428,
     429,   430,   431,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,   408,   423,   424,   409,     0,   410,   411,     0,
     412,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,   425,  2136,     0,    78,   426,     0,     0,
       0,     0,     0,   427,    80,    81,   428,   429,   430,   431,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,   408,
     423,   424,   409,     0,   410,   411,     0,   412,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
     425,  2138,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,   408,   423,   424,   409,
       0,   410,   411,     0,   412,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   413,     0,     0,     0,     0,     0,   425,  2157,     0,
      78,   426,     0,     0,     0,     0,     0,   427,    80,    81,
     428,   429,   430,   431,     0,     0,     0,     0,     0,     0,
       0,   414,   415,     0,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,   408,   423,   424,   409,     0,   410,   411,
       0,   412,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   413,     0,
       0,     0,     0,     0,   425,  2159,     0,    78,   426,     0,
       0,     0,     0,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
     408,   423,   424,   409,     0,   410,   411,     0,   412,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   413,     0,     0,     0,     0,
       0,   425,  2161,     0,    78,   426,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,     0,   423,   424,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   425,     0,
       0,    78,   426,     0,     0,     0,     0,     0,   427,    80,
      81,   428,   429,   430,   431,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -499,
    -499,     0,  -499,    46,   408,    47,     0,   409,  -499,   410,
     411,     0,   412,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,   413,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,     0,     0,     0,   408,    75,
      74,   409,     0,   410,   411,     0,   412,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   714,   413,     0,    78,   426,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,   408,   423,   424,   409,     0,
     410,   411,     0,   412,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,     0,     0,   717,     0,     0,    78,
     426,     0,     0,     0,     0,     0,   427,    80,    81,   428,
     429,   430,   431,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,   408,   423,   424,   409,     0,   410,   411,     0,
     412,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,   722,     0,     0,    78,   426,     0,     0,
       0,     0,     0,   427,    80,    81,   428,   429,   430,   431,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,     0,
     423,   424,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     731,     0,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -500,  -500,     0,  -500,    46,   408,    47,     0,   409,
    -500,   410,   411,     0,   412,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,   413,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   414,   415,     0,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,     0,   423,   424,     0,     0,     0,     0,
     408,    75,    74,   409,     0,   410,   411,     0,   412,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   425,   413,     0,    78,   426,     0,
       0,     0,     0,     0,   427,   947,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,     0,   423,   424,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   425,     0,
       0,    78,   426,     0,     0,     0,     0,     0,   427,   491,
      81,   428,   429,   430,   431
};

static const yytype_int16 yycheck[] =
{
       1,   520,    76,     4,   283,     1,   172,    76,    76,   267,
       1,   182,   965,   187,   760,    76,  1273,   763,   236,   531,
       4,   145,   187,   227,   741,   241,   241,   100,   684,   997,
     209,   241,   425,   170,     1,   958,    59,  1005,     1,   141,
     842,     4,   241,     1,   262,   427,   848,   241,  1254,     1,
     841,  1305,  1306,   928,    85,    56,    57,   241,    59,    78,
     100,  1237,  1815,    59,   172,  1060,   839,   170,    59,   671,
     807,   209,   675,  1254,   854,    76,   856,  1943,    90,   378,
     841,    82,   671,   382,   208,    87,    87,  1815,    76,   241,
      76,   196,    59,  1088,    95,    76,    59,   671,   364,   100,
    1369,    59,   103,  1815,   839,   839,   107,    59,   944,   839,
       1,   241,    76,   413,     0,   331,   331,    76,    73,   170,
      87,   331,     1,   107,   198,   157,   157,   101,   151,   198,
     198,  1939,   331,   100,   100,   241,   103,   331,   438,   439,
     107,   241,   186,  1816,   107,   146,   932,   331,   149,   251,
     151,   103,   154,   165,  1713,   151,   157,     0,    73,   459,
     151,  1207,  1157,   164,   577,   578,   952,   241,  1214,   242,
     362,   172,   241,   241,  1219,   251,    90,   126,   280,   331,
     241,   121,   137,   184,   151,     1,   378,   590,   151,   291,
     382,  1378,   492,   151,     0,   196,   197,   198,   601,   151,
     161,   331,   242,   157,   280,   166,     1,   208,   182,   677,
     198,   160,   198,   214,   316,   291,  1709,   198,   182,   385,
      95,   161,   137,   182,   225,   331,   392,   181,    91,   230,
     197,   331,   498,   113,   235,   236,   237,   839,   841,   136,
     241,   242,   645,    59,   102,   318,   161,   214,  1294,   163,
     839,   165,   121,   241,  1927,   241,   136,   331,  2124,   332,
     241,   262,   331,   331,    59,   839,   162,  2075,   270,   270,
     331,   272,   163,   164,    10,   242,   242,   385,   318,   159,
     281,   282,   561,   180,   285,   164,   305,  1210,  1219,  2155,
     569,   292,   332,   706,   157,   400,  2049,   425,   157,   157,
     492,  1860,  1975,   270,  1050,   306,   307,   720,   309,   868,
      20,   362,   107,   314,  1219,  1117,     1,   318,   285,     4,
    1037,  2049,   526,   539,   539,   141,  1595,  1596,  1597,   539,
     331,   332,  1123,   285,   716,   151,     1,  2049,   340,     4,
     539,   342,   157,  1116,  1525,   539,   141,  1528,  1529,   350,
     351,   318,   318,   619,   355,   539,   151,   575,   157,  1327,
    1097,   660,  1123,   581,   488,   332,   332,   319,   761,   244,
      73,   246,  1865,  1866,    59,   843,   157,  2050,   253,   847,
     983,  1116,  1116,   138,   385,   509,  1116,   539,   496,   165,
     858,   859,  1228,   394,    59,   138,   397,   663,   697,   400,
    1587,   814,   165,   356,    59,   584,   182,    62,    63,   539,
      65,  1457,   364,   168,   169,  1460,  1461,  1462,   103,   182,
     156,   687,   107,   639,   639,  1211,  1262,   163,   694,   639,
     173,   167,   157,   539,   137,   251,   172,  2110,   103,   539,
     639,   492,   107,   396,  1003,   639,   584,    78,    79,  2078,
     962,   326,   327,   944,   157,   639,   251,   459,  1333,    62,
      63,  1954,  1955,    76,   280,   539,   151,   767,   660,   470,
     539,   539,   159,   157,  1659,   291,    73,  2106,   551,  1664,
      93,   138,   661,   170,   171,   280,   151,   639,   151,   861,
     534,   863,   684,   158,   495,   496,   291,  2126,   600,   518,
     316,     1,   157,   470,   157,   697,   507,   508,   880,   639,
     173,   551,   165,    76,  1116,  1317,   173,   518,   182,   520,
    1123,   316,   113,   661,   600,   320,   929,  1116,    91,  1460,
    1461,  1462,  1708,   639,   165,   834,   702,  1713,   539,   639,
     137,  1747,  1116,    10,   209,   136,   498,    78,    79,   182,
     551,   539,   112,   539,   163,  1460,  1461,  1462,   539,    59,
     157,   163,   165,   292,   161,   639,  1747,     1,   641,   250,
     639,   639,  1091,   182,   575,   135,   257,   949,   307,   182,
     581,   165,   583,   163,   551,   551,   714,   394,   172,   717,
     397,   719,  1560,   173,   722,  1590,   109,   110,   279,  1401,
     285,   641,    87,   731,   163,   156,   734,   735,   736,   290,
     163,   790,   163,   163,    76,   100,   163,   154,   103,   163,
     285,   644,   107,   131,   132,    59,   173,    73,   165,   182,
      92,   182,  1412,   684,   165,  1415,  1416,   182,   639,  1893,
     641,   141,   834,   644,   181,   646,  1569,   160,   644,   111,
      70,   151,   790,   644,   655,  1311,   835,   163,   659,    13,
      14,    15,    16,    17,   648,   160,   870,   619,   157,   177,
     178,   970,  1140,   163,   641,   641,   182,   644,   852,     3,
     632,   644,   163,   173,  1860,   648,   644,   852,   906,   156,
     691,   137,   644,   163,   163,   163,     3,   835,    73,   159,
     167,   182,   867,   173,   164,   172,   972,   141,   163,   710,
     163,   157,   197,   182,   182,   161,   854,   151,   593,    73,
     150,   151,   152,   153,   862,   862,   209,   182,  1028,   214,
     165,  1988,  1478,   841,  1034,   687,   163,  1228,   165,   957,
    1112,  1113,   163,   173,   788,  1045,    13,    14,    15,    16,
      17,   251,   753,   856,   755,    73,   757,   242,   165,   163,
     761,   182,   137,   764,   181,   172,  1325,    73,   643,   173,
     789,  1262,    73,  1332,   163,   163,   157,  1953,   970,   165,
     280,   135,   157,   137,   600,   270,   161,   159,   789,   163,
    1966,   291,   164,   182,   182,  1790,  1168,  1792,  1766,   163,
     285,   135,  1755,   157,   121,   600,    73,   161,   182,    73,
     163,   862,   179,   168,   168,   169,   316,   251,   182,   137,
     175,   176,   157,   157,   157,   163,  1385,   161,   644,   182,
     165,   137,   998,   834,   168,   169,   137,   172,   839,   157,
     841,  1360,   162,   161,   182,   661,   280,   181,   163,   644,
     159,   157,   853,   157,  2030,   161,   157,   291,   173,   157,
     161,  1254,   161,   864,   109,   110,   164,   166,   135,   870,
     137,     1,   873,   137,     4,    59,   159,  1156,    62,    63,
     163,    65,   316,    73,   565,    13,    14,    15,    16,    17,
     157,  1304,   158,   157,   161,  1033,  1033,   161,   159,   165,
     159,   168,   169,   164,   159,   906,   160,   161,  1032,   164,
     164,   592,    13,    14,    15,    16,    17,   159,   599,   584,
    1085,   159,   603,   157,   159,   159,   164,   161,  1228,    59,
      13,    14,    15,    16,    17,   114,   115,   116,   117,   118,
      73,   942,   943,   944,   928,    73,    76,   137,   944,   159,
     159,   159,    82,   944,   163,   163,   957,    87,   159,   644,
    1164,   157,   157,   648,   159,   928,   161,   157,    22,  1572,
     100,   161,    73,   103,   958,   163,   159,   107,  1497,   644,
     163,   944,  1033,   648,   157,   470,   944,   157,  1500,   159,
      73,   161,   944,   157,  1485,   958,   661,    73,  1489,  1490,
     157,  1166,  1561,  1004,   137,    73,   157,   135,   159,   137,
    1011,  1311,  1503,   159,  1122,  1123,   163,   163,   157,   835,
     972,   151,   161,   102,   157,  1584,   157,   157,   161,   157,
      73,   157,    73,   161,     3,   161,   137,   138,   157,    73,
     168,   169,   172,    90,    13,    14,    15,    16,    17,  1315,
    1350,  1351,  1352,    73,   137,  1056,   159,  1357,  1358,  1060,
     163,   137,  1320,     3,  1583,   162,   551,   197,   198,   137,
     109,   110,  1465,    13,    14,    15,    16,    17,   208,    73,
     165,   157,   159,  1301,   214,   161,   163,  1088,   159,   157,
    1091,   165,   163,   161,   137,   225,   137,    73,    73,   156,
     600,   584,   182,   137,    73,   235,   236,   237,   159,   159,
     161,   241,   242,   163,   157,  1116,   157,   137,   161,  1311,
     161,  1122,  1123,   157,   157,   790,  1254,   161,   944,   159,
     160,   135,   262,    73,  1305,  1306,   165,   157,   159,   162,
     270,   161,   163,   137,   644,   159,   157,   159,   159,   944,
     161,   163,   165,   157,  1666,   285,  1157,   161,  1324,  1718,
     157,   137,   137,   157,   168,   169,   600,   161,   137,  1448,
     835,    13,    14,    15,    16,    17,   306,   172,   661,   309,
     181,   157,   157,   157,   314,   161,   161,   161,   318,   854,
     157,   856,   170,   171,   161,   860,   861,   137,   863,   129,
     130,   331,   332,  1340,  1342,  1342,   768,   769,   770,  1768,
     644,    47,    48,   159,    50,   880,  1381,  1219,   135,  1347,
      56,   133,   134,   159,   159,  1784,  1210,   163,   163,   121,
    1231,    73,  1228,  1234,  1235,  1236,   157,  1228,  1984,   157,
     157,   157,  1243,   928,   161,   161,   157,  1210,  1422,  1423,
     931,   168,   169,  1772,   157,   385,   163,  1422,  1423,   944,
    1311,  1262,   160,   928,   159,  1228,  1262,  1268,   163,   174,
    1228,  1262,   169,   958,   159,   179,  1228,  1415,   163,   944,
     167,  1418,  1283,   159,   949,  1286,  1287,   163,  1289,   162,
     163,  1342,   135,   958,  1295,   137,   138,   159,  1299,  1262,
    1301,   163,   159,  1287,  1262,   159,   163,   790,   159,  1412,
    1262,   159,   163,  1416,   159,  1418,   157,   135,  1193,  1286,
    1287,   159,   162,   163,  1287,   163,    13,    14,    15,    16,
      17,   159,  2111,   159,  1286,   163,  2115,   163,   159,   157,
     470,  1900,  1343,   161,   159,  1904,   162,   163,   163,  1333,
     168,   169,   835,   150,   151,   152,   153,   159,   159,  1360,
    1485,   163,   163,  1315,  1489,   161,   496,  1418,  1369,   159,
    1333,   854,  1863,   856,   168,   169,   173,   860,   861,   159,
     863,   162,   163,  1485,   157,   182,    73,  1489,   518,   163,
     520,  1565,   150,   151,   152,   153,   162,   880,  1471,   138,
    1565,  1402,  1656,  1657,  1658,   163,  1544,  1544,   138,   539,
     162,   163,  1228,   162,   163,   173,   775,   776,   777,   778,
     163,   551,   109,   110,   182,  1216,  1217,  1218,   164,   137,
     107,  1471,   164,  1228,   111,   112,   113,   114,   115,   116,
     117,   118,  1742,   163,   944,   575,  1262,  1112,  1113,   159,
     137,   581,   157,   583,   181,  1671,  1671,   940,  1460,  1461,
    1462,  1671,   159,  1465,  1466,   159,   949,  1262,   163,   164,
    1471,   159,  1671,   160,  1475,  1476,   159,  1671,   159,  1463,
     159,   158,   309,   159,   161,    92,    93,  1671,  1678,  1679,
    1680,   159,  1287,  1544,  1485,   181,  1497,   162,  1489,  1490,
    1463,   162,   163,  1168,   161,  1471,   162,   163,   159,   639,
     944,   641,  1503,   165,   644,   162,   163,   162,   163,  1671,
    1521,  1522,  1485,   231,  2015,  1210,  1489,  1490,   165,   659,
    1531,   162,   163,  1485,   162,   163,   165,  1489,  1490,   165,
    1503,  1671,   165,  1228,   163,  1210,   150,   151,   152,   153,
      71,  1503,   162,   163,  2045,   182,   162,   163,   157,   163,
     162,   691,  1563,  1228,  1531,  1671,   162,   163,    79,   173,
     162,  1671,   162,   163,   162,   163,    18,  1262,   182,  1531,
     162,   163,  1583,   163,   164,  1569,  1659,   181,  2079,  1590,
     165,  1664,   162,   163,  1595,  1596,  1597,  1262,    18,   165,
    1673,  1286,  1287,   162,   163,   182,  1569,   315,    78,    79,
    1671,   163,   164,   159,  1830,  1830,  1370,  1371,   159,  1659,
    1830,  1286,  1287,   753,  1664,   755,   165,   757,   165,  1112,
    1113,  1830,   162,  1673,   764,   162,  1830,    18,    58,    59,
      60,    61,    62,    63,    64,    65,  1830,   162,  1333,   771,
     772,   159,   773,   774,  1782,   779,   780,   162,  1659,   789,
    1528,  1529,   156,  1664,  1679,  1680,   159,   159,  1333,  1485,
    1671,   159,  1673,  1489,  1490,   159,   159,   159,  1830,   159,
    1681,   159,   159,  1983,    22,  1168,  1878,  1503,   159,   162,
    1485,   159,  1736,  1659,  1489,  1490,   159,   156,  1664,  1700,
    1830,   165,    71,   165,   834,   165,  1707,  1673,  1503,   839,
    1976,   841,   124,  1697,   126,   127,   128,   425,   159,   159,
     181,   159,  1893,   159,  1830,   159,   165,   162,  1228,   159,
    1830,   163,   165,   159,  1697,   163,   159,  1738,   159,   566,
     870,   163,   159,   873,  1696,   157,   159,  1412,   160,   161,
    1415,  1416,   163,   165,   166,   159,   159,  1975,   163,   159,
    1833,   159,  1262,   159,   159,  1939,   159,   159,   162,   156,
     159,  1772,   162,   159,  1939,   159,   906,   159,  1463,   159,
     159,  1973,    13,    14,    15,    16,    17,    18,   159,  1790,
     162,  1792,   159,  1833,  1228,   159,   159,   159,  1463,   159,
    1485,  1286,  1287,   159,  1489,  1490,    13,    14,    15,    16,
      17,    18,   942,   163,   163,   642,   163,   163,  1503,   156,
    1485,   163,   156,    14,  1489,  1490,   157,   957,  1262,  1830,
     157,   157,  1833,  2049,  2049,   157,  1880,   157,  1503,  2049,
     157,  1842,  1843,   157,   157,   164,  1531,   163,  1849,   164,
    2049,   162,   182,   162,   156,  2049,   564,   165,   181,   165,
     181,  1862,   163,  1936,   572,  2049,  1531,  1833,   156,   159,
     159,  1872,  1863,  1874,   159,   159,  1878,   159,   159,   162,
     159,  1011,  2047,   591,  1569,   162,  1887,   163,  1889,  1890,
    1891,   162,   159,  1845,   602,   159,  1936,  2049,   159,  2073,
    1863,  2075,  1697,   159,  1569,   162,    62,   156,  2073,   156,
    2075,  1863,    13,    14,    15,    16,    17,    18,   157,  2049,
      13,    14,    15,    16,    17,    18,  1056,   182,   157,  1412,
    1060,    81,  1415,  1416,   182,  1936,   182,  1981,   182,  2113,
     182,   182,  1943,  2049,    93,   156,  1947,   103,  2113,  2049,
     182,  1952,   182,   157,   157,    91,   156,   159,  1088,   115,
     116,  1091,   156,   162,   162,   162,   162,   156,   156,   159,
    1936,  1973,   165,   124,  1975,  2049,  1977,  2050,   164,   806,
    2049,  2049,   164,   159,   159,  1485,  1116,   159,  2049,  1489,
    1490,   162,  1122,  1123,   162,   822,   159,   159,   159,   826,
     159,   156,   158,  1503,  2178,  2006,   714,   164,   182,   717,
    2050,  1696,  1697,  2178,   722,   159,   159,  2018,   163,   157,
     159,  2022,   157,   731,  2015,   157,   159,  1157,   162,   156,
    2031,  1696,  1697,   162,   162,   159,  2037,  2110,   156,   159,
     159,   162,   750,   159,    76,    76,  1531,  1863,  2049,  2050,
     156,  1485,  2015,   157,  2045,  1489,  1490,   182,   214,   182,
     182,   159,   157,  2015,   156,   162,    62,  2111,  1863,  1503,
    2110,  2115,  2116,   162,   156,   159,   159,    76,   159,    76,
     182,  2082,  2045,  2050,  2050,   161,   173,   173,  2079,    76,
     164,   182,   182,  2045,   182,   156,   156,   156,   173,   164,
    2144,   158,   173,   156,   159,   107,   157,   163,     1,  2110,
     159,     4,   108,   158,   162,   173,  2079,   113,  2119,   173,
     116,  2165,   118,  2124,   159,  2169,    76,  2079,   158,   285,
      87,   159,   182,   159,   156,   164,   156,  2181,   157,   182,
     182,   159,   182,  2110,  2110,  1782,   781,   740,   782,  2150,
     783,  1337,  2153,  1283,  2155,   785,  1286,  1287,  1249,   784,
    1845,   458,    91,   319,  1262,  2155,    59,  1489,   324,  2075,
    1871,  1301,  2106,  2174,   330,  1503,  2064,  1863,  1863,     3,
    1845,  2182,  1746,    76,  2046,  1973,  1729,  1729,  2116,  2169,
    2191,  2045,    49,  1289,    87,   116,   276,   154,  1863,  2015,
    2004,  1936,  1329,   132,   912,  1771,  1466,   100,   364,   527,
     103,  1283,   870,  1343,   107,     0,  1537,   806,   926,   710,
    2015,   806,   930,   806,   655,  1649,   934,    -1,    -1,  2045,
    1360,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   195,  1369,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
    2045,    -1,   145,    -1,    -1,    -1,    -1,    -1,   151,    -1,
     416,   154,    -1,  2079,   157,   158,    -1,    -1,    -1,    -1,
      -1,    -1,  1402,    -1,    -1,    -1,  1103,   170,   274,    -1,
    1107,    -1,    -1,   107,  2079,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,  1126,
      -1,    -1,   195,    66,   197,   198,  1133,    -1,    -1,    -1,
      -1,   135,    -1,   270,    -1,   208,   209,    -1,    -1,    -1,
      -1,   214,    -1,    -1,    -1,   321,    -1,    -1,    -1,    -1,
    2015,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,  1471,   498,   236,   168,   169,    -1,    -1,   241,   242,
    2015,    -1,    -1,    -1,  1181,    -1,   180,    -1,  1185,    -1,
    2045,   517,  1189,  1863,    -1,    -1,   309,  1497,    -1,   262,
     366,    -1,   368,    -1,   370,    -1,    -1,   270,    -1,    -1,
    2045,    -1,    -1,   340,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   285,    -1,  2079,    -1,    13,    -1,    -1,   328,
      -1,  1531,   107,    -1,   109,   362,   111,   112,   113,   114,
     115,   116,   117,   118,  2079,    -1,    -1,    -1,    -1,    -1,
     416,    -1,    -1,    -1,    -1,   318,   582,    -1,    -1,  1863,
      -1,   324,    -1,    -1,    -1,    -1,    -1,   330,   331,   332,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   340,    -1,   605,
      -1,    -1,    -1,  1583,    -1,    -1,    -1,    -1,    -1,    -1,
    1590,    -1,    -1,   619,    -1,  1595,  1596,  1597,    -1,   362,
     363,   364,    89,   107,    -1,    -1,   632,   111,   112,   113,
     114,   115,   116,   117,   118,   378,    -1,    -1,    -1,   382,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   459,    -1,    -1,  1213,    -1,   663,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2015,    -1,    -1,    -1,  1227,
      -1,   517,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1659,
      -1,   687,   425,    -1,  1664,   492,    -1,    -1,   694,  1247,
     157,  1671,     3,  1673,    -1,  2045,  1254,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   491,    -1,   493,    -1,   459,    -1,    -1,   462,
      -1,    -1,    -1,   502,   531,    58,    -1,   470,    -1,  2079,
      -1,  2015,    -1,    66,    67,    68,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,   492,
      -1,   597,    -1,   496,    -1,   498,  1433,    -1,    -1,    73,
    1437,  2045,    -1,    -1,  1441,    -1,   509,    -1,    79,    -1,
     577,   578,    -1,   566,   107,    73,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,     1,   531,    13,
       4,    -1,  1772,   107,    -1,  2079,   539,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,   551,   107,
    1790,    -1,  1792,   111,   112,   113,   114,   115,   116,   117,
     118,   135,    -1,   137,    -1,    -1,    -1,    -1,   161,    -1,
      -1,    -1,   575,    -1,   577,   578,    -1,   135,   581,   137,
      -1,   584,    -1,   157,   158,    59,   179,    -1,    -1,   642,
    1830,    -1,    -1,  1833,   168,   169,    -1,    -1,    -1,   157,
     158,   209,    -1,    -1,    -1,    89,   180,    -1,    -1,    -1,
     168,   169,  1549,    87,    -1,    -1,    -1,   684,   671,    -1,
      -1,    -1,   180,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   639,    -1,   641,   706,
     211,   644,    -1,   909,    -1,   648,    -1,   714,    -1,    -1,
      -1,    -1,    -1,   720,    -1,   722,    -1,   660,   661,    -1,
     663,  1479,  1480,    -1,    -1,    -1,  1603,   141,   671,    -1,
      -1,   145,   675,    -1,    -1,  1612,    -1,   151,    -1,  1616,
     154,   684,     4,     5,     6,     7,     8,     9,    10,    11,
      12,   694,    -1,    -1,   697,    -1,  1936,    -1,   269,    -1,
      -1,    -1,    -1,   706,    -1,    -1,   972,    -1,    -1,    -1,
      -1,   714,  1530,    -1,   717,    -1,   719,   720,    -1,   722,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   731,    -1,
      -1,   734,   735,   736,   208,  1975,    -1,  1977,    -1,   310,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   814,    -1,    -1,
      -1,   322,    -1,   806,   362,    -1,   107,   365,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   339,   822,
     378,    -1,    -1,   826,   382,    -1,    -1,   251,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   839,   790,    79,    -1,
     361,  2031,    -1,    -1,   268,    -1,   270,    -1,    -1,    -1,
      -1,    -1,    -1,   806,   807,    -1,   280,    -1,    -1,  2049,
    2050,   814,    -1,   852,   165,    -1,   107,   291,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,   834,   835,    -1,    -1,    18,   839,    -1,   841,   107,
     314,    -1,   316,   111,   112,   113,   114,   115,   116,   117,
     118,   854,    -1,   856,    -1,   426,    -1,   860,   861,   862,
     863,    -1,    -1,    -1,    -1,    -1,   340,    -1,    -1,   225,
    2110,    -1,    -1,    -1,    -1,    -1,    -1,   880,    -1,    62,
      63,    64,    65,    -1,   492,    -1,   457,   940,    -1,   157,
     158,   182,    -1,    -1,    -1,   962,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   906,    -1,    -1,    -1,    -1,   947,   948,
    1728,    -1,   180,    -1,   485,    -1,    -1,    -1,    -1,   490,
      -1,    -1,    -1,    -1,   107,   928,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,    -1,   510,
     511,   944,  2182,    -1,   515,   516,   949,    -1,   519,    -1,
      -1,  2191,    -1,    -1,   957,   958,    -1,    -1,    -1,   962,
      73,    -1,    -1,    -1,   535,    -1,    -1,   970,    -1,   972,
      -1,    -1,    -1,    -1,    -1,    -1,   584,    -1,   161,    -1,
     983,    -1,    -1,    -1,    -1,   459,   557,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
    1049,    -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,
    1286,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,  1032,
    1033,    -1,    -1,    -1,    -1,   509,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,    -1,   161,  1315,
    1103,  1090,   660,   661,  1107,   168,   169,    -1,    -1,    -1,
      -1,    -1,    -1,  1116,   635,    -1,    -1,   180,    -1,    -1,
      -1,    -1,    -1,  1126,    -1,    -1,   684,    -1,    -1,    -1,
    1133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   697,
      -1,    -1,    -1,    -1,  1097,    -1,    -1,    -1,    -1,    -1,
    1139,   672,  1141,    -1,    -1,    73,    -1,    -1,    -1,  1112,
    1113,    -1,    -1,  1116,  1153,    -1,  1155,    -1,    -1,    -1,
    1123,  1160,  1161,    -1,    -1,    -1,   600,    -1,  1181,    -1,
    2067,  1170,  1185,    -1,    -1,    -1,  1189,    -1,    -1,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,    -1,    -1,    -1,    -1,  1196,    -1,    -1,
    1199,    -1,   518,    -1,   520,  1168,   737,   135,    -1,   137,
     644,    -1,    -1,    -1,   648,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   790,    -1,    -1,    -1,    -1,  1453,    -1,   157,
     158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,  1472,  1210,    -1,    -1,
      -1,    -1,   180,    -1,    -1,    -1,  1219,    -1,    -1,    -1,
      -1,    -1,    -1,  1262,   107,  1228,   834,   835,   111,   112,
     113,   114,   115,   116,   117,   118,   119,  1304,    -1,    -1,
      -1,    -1,    -1,    -1,  1311,    -1,    -1,    -1,    -1,    -1,
      -1,  1254,    -1,  1292,   862,    -1,    -1,    -1,   829,  1262,
     831,    -1,    -1,    -1,    -1,  1531,   837,    -1,    -1,    -1,
      -1,  1310,    -1,  1312,    -1,  1314,    73,    -1,   161,    -1,
      -1,    -1,  1321,  1286,  1287,    -1,    -1,    -1,    -1,    -1,
      -1,  2109,    -1,    -1,   865,    -1,    -1,    -1,  1301,    -1,
      -1,  1304,    -1,   874,    -1,    -1,    -1,   878,  1311,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,  2141,    -1,    -1,    -1,    -1,    -1,    -1,
    1333,    -1,    73,    -1,    -1,     1,    -1,    -1,     4,  1342,
     137,    -1,    -1,    -1,  1347,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   923,    -1,    -1,    -1,  1395,  1396,    -1,    -1,
     157,   158,   970,    -1,    -1,   973,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
    1433,    -1,  1421,   180,  1437,    -1,    76,    -1,  1441,  1428,
      -1,  1430,    -1,    59,   135,    -1,   137,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,  1412,
     100,    -1,  1415,  1416,    -1,  1418,   157,   158,    -1,    -1,
      -1,    87,    -1,    -1,    -1,  1033,    -1,   168,   169,    -1,
    1696,    -1,    -1,  1500,    -1,    -1,    -1,   103,    -1,   180,
      -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1021,    -1,    -1,    -1,   928,    -1,    -1,  1460,  1461,  1462,
    1463,    -1,  1465,  1466,    -1,    -1,    -1,   157,  1471,  1472,
     944,    -1,   182,    -1,    -1,   141,    -1,    -1,    -1,   145,
      -1,    -1,  1485,    -1,   958,   151,  1489,  1490,   154,    -1,
      -1,    -1,   158,    -1,    -1,    -1,  1549,  1500,    -1,    -1,
    1503,    -1,    -1,   169,   170,    -1,   172,    -1,    -1,  1548,
      -1,    -1,    -1,    -1,   870,    -1,  1555,   873,  1557,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1531,   195,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1544,   208,   209,    -1,    -1,   236,    -1,   214,    -1,
    1603,   241,   242,    -1,    -1,    -1,    -1,    -1,  1032,  1612,
      -1,    -1,    -1,  1616,   252,    -1,  1569,    -1,    -1,  1572,
      -1,    -1,   262,    -1,    -1,    -1,    -1,    -1,    -1,  1845,
      -1,    -1,    -1,    -1,    -1,   251,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1633,    -1,    -1,    -1,    -1,  1666,
      -1,    -1,   268,    -1,   270,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   280,    -1,    -1,    -1,    -1,   285,
      -1,    -1,    -1,    -1,    -1,   291,    -1,    -1,   318,    -1,
      -1,    -1,    -1,    -1,    -1,  1206,    -1,    -1,    -1,    -1,
     306,   331,   332,   309,    -1,    -1,    -1,    -1,   314,    -1,
     316,    -1,    -1,   319,   320,    -1,  1659,  1660,   324,    -1,
      -1,  1664,    -1,  1666,   330,    -1,    -1,    -1,  1671,    -1,
    1673,    -1,    -1,    -1,   340,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1255,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1696,  1697,    -1,   362,    -1,   364,   365,
    1056,    -1,    -1,  1311,  1060,    -1,    -1,    -1,    -1,    -1,
    1976,    -1,   378,    -1,    -1,    -1,   382,    -1,    -1,    -1,
      -1,    -1,  1761,  1762,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1088,    -1,  1342,  1091,  1210,    -1,  1309,    -1,
      -1,    -1,    -1,    -1,    -1,  1219,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1228,    -1,    -1,    -1,   107,   425,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,   462,    -1,    -1,    -1,    -1,  1782,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1262,    -1,
      -1,   479,    -1,   459,   482,    -1,    -1,    -1,    -1,    -1,
      -1,  1157,    -1,    -1,    -1,    -1,   496,    -1,    -1,    -1,
      -1,    -1,  1815,  1816,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   488,    -1,    -1,    -1,   492,  1830,    -1,    -1,
    1833,    -1,   498,   182,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1845,   509,    -1,    -1,    -1,    -1,    -1,   539,
      -1,    -1,    -1,    -1,    -1,    -1,   544,    -1,    -1,  1333,
    1863,   551,    -1,    -1,    -1,   531,    -1,    -1,    -1,    -1,
      -1,    -1,   107,    -1,    -1,  1878,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   575,    -1,    -1,   123,    -1,
     125,   581,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
     566,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   577,   578,    -1,    -1,    -1,   582,    -1,   584,    -1,
      -1,    -1,    -1,   158,  1927,    -1,   161,    -1,    -1,    -1,
      -1,    -1,    -1,  1936,   600,   107,  1544,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,   639,
      -1,   641,   160,   619,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   172,    -1,   632,    -1,    -1,    -1,
    1973,    -1,  1975,  1976,    -1,    -1,   642,    -1,   644,    -1,
      -1,    -1,   648,    -1,    -1,    -1,  1460,  1461,  1462,  1463,
    1464,  1465,  1466,   659,   660,   661,    -1,   663,    -1,    -1,
     172,  2004,    -1,    -1,  1360,   671,    -1,    -1,    -1,    -1,
      -1,  1485,  2015,  1369,  2067,  1489,  1490,    -1,   684,    -1,
      -1,   687,    -1,    -1,    -1,   691,    -1,    -1,   694,  1503,
      -1,   697,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     706,    -1,  2045,    -1,    -1,    -1,  2049,  2050,   714,    -1,
      -1,   717,    -1,   719,   720,    -1,   722,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   731,    -1,    -1,   734,   735,
     736,    -1,    -1,    -1,    -1,    -1,  2079,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2125,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1569,    -1,    -1,    -1,    -1,
      -1,  2140,    -1,    -1,    -1,    -1,   107,  2110,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   806,   807,
      -1,    -1,    -1,    -1,   790,    -1,    -1,    -1,    -1,   817,
      -1,    -1,   820,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     806,  1497,    -1,    -1,    -1,    -1,    -1,    -1,   814,   839,
      -1,   841,    -1,    -1,    -1,    -1,   822,    -1,    -1,    -1,
     826,    -1,    -1,    -1,    -1,    -1,    -1,   168,   834,   835,
      -1,    -1,    -1,   839,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,   854,    -1,
     856,    -1,    -1,    -1,   860,   861,   862,   863,   886,    -1,
      -1,    -1,   135,     1,    -1,   893,     4,    -1,    -1,   897,
      -1,    -1,    -1,   901,   880,    73,   906,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,  1583,   161,    -1,
      -1,    -1,    -1,    -1,  1590,   168,   169,    -1,    -1,  1595,
    1596,  1597,    -1,   909,    -1,    -1,    -1,   180,   181,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    59,   928,    -1,    -1,    -1,    -1,   957,    -1,    -1,
    1878,    -1,    -1,    -1,   940,    -1,    -1,   135,   944,   137,
      -1,    -1,    -1,   949,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,   958,    -1,    -1,    -1,   962,    -1,    -1,   157,
     158,    -1,    -1,    -1,   970,    -1,   972,   973,    -1,   107,
     168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,   180,    -1,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,  1000,    -1,   123,    -1,   125,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    -1,   145,    -1,    -1,
      -1,    -1,   107,   151,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,  1973,  1032,  1033,    -1,    -1,
    1941,   158,   170,    -1,   161,    -1,    -1,    -1,    -1,    -1,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1863,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1969,   197,
      -1,    -1,   157,   158,    -1,    -1,   161,    -1,    -1,  1097,
     208,   209,    -1,   168,   169,    -1,  1772,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   180,  1116,    -1,    -1,    -1,
      -1,    -1,  1122,  1123,  1790,    -1,  1792,  1103,    -1,    -1,
      -1,  1107,    -1,    -1,   242,    -1,  1112,  1113,    -1,    -1,
    1116,    -1,    -1,   251,    -1,    -1,    -1,    -1,    -1,    -1,
    1126,    -1,    -1,    -1,   262,    -1,    -1,  1133,    -1,   267,
     268,    -1,   270,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   280,    -1,  1172,    -1,    -1,  1175,    -1,    -1,
      -1,  1179,    -1,   291,    -1,    -1,   294,    -1,    -1,    -1,
     298,    -1,  1168,    -1,    -1,   303,    -1,    -1,    -1,    -1,
      -1,   309,    -1,    -1,    -1,  1181,   314,    -1,   316,  1185,
      -1,   107,   320,  1189,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   332,    -1,    -1,   123,    -1,   125,
      -1,  2015,    -1,    -1,  1210,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1219,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1228,    -1,   362,    -1,    -1,   365,    -1,    -1,
      -1,  2045,   158,    -1,    -1,   161,    -1,    -1,    -1,    -1,
     378,    -1,    -1,    -1,   382,    -1,    -1,    -1,  1254,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1262,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2079,    -1,    -1,    -1,    -1,
      -1,  1301,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1286,  1287,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,  1304,    -1,
      -1,    -1,    -1,    -1,    -1,  1311,    -1,    -1,    -1,  1315,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,  1333,    -1,    -1,
      -1,    -1,   157,   158,  1340,    -1,  1342,    -1,    -1,   164,
      -1,  1347,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,
     488,    -1,    -1,    -1,   492,   180,    -1,    -1,   107,  1387,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
    1398,   509,  1402,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1412,    -1,    -1,  1415,
    1416,   160,  1418,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,  1433,   566,    -1,
      -1,  1437,    -1,   157,    -1,  1441,   160,   161,    -1,    -1,
      -1,  1471,   135,    -1,   137,    -1,   584,  1453,    -1,    -1,
      -1,    -1,    -1,    -1,  1460,  1461,  1462,  1463,  1464,  1465,
    1466,    -1,   600,    -1,   157,   158,  1472,    -1,   161,    -1,
      13,    14,    15,    16,    17,   168,   169,    -1,    -1,  1485,
      -1,    -1,    -1,  1489,  1490,    -1,    -1,   180,    -1,    -1,
      -1,    -1,    -1,    -1,  1500,  1231,    -1,  1503,    -1,    -1,
      -1,    -1,    -1,    -1,   642,    -1,   644,  1243,    -1,   107,
     648,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   660,   661,    -1,  1531,    -1,    -1,    -1,    -1,
      73,    -1,    -1,   671,    -1,    -1,    -1,   675,  1544,    -1,
      -1,    -1,    -1,  1549,    -1,    -1,   684,    -1,    -1,    -1,
      -1,   689,    -1,    -1,    -1,    -1,  1562,    -1,    -1,   697,
      -1,    -1,   160,  1569,   107,     1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,  1603,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1612,    -1,    -1,    -1,
    1616,    -1,    -1,    -1,   157,   158,    -1,    -1,   161,    -1,
      -1,    -1,    -1,    59,    -1,   168,   169,    -1,    -1,  1659,
      -1,    -1,  1660,    -1,  1664,    -1,    73,   180,    -1,    -1,
      -1,  1671,    -1,  1673,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   790,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1666,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   806,   807,
     107,   107,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,   822,    -1,    -1,    -1,   826,    -1,
    1696,  1697,    -1,    -1,    -1,    -1,   834,   835,   135,    -1,
     137,   839,    -1,   841,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,   854,    -1,   856,    -1,
     157,   158,   860,   861,   862,   863,    -1,    -1,    -1,    -1,
      -1,   168,   169,    -1,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   880,   180,    -1,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,   209,   135,    -1,  1782,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1521,  1522,  1815,  1816,    -1,
     928,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,    -1,
    1830,    -1,   940,  1833,    -1,    -1,   944,   168,   169,    -1,
      -1,   949,    -1,    -1,    -1,   251,    -1,    -1,    -1,   180,
     958,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,   970,    -1,    -1,   973,    -1,    -1,    -1,  1845,
      -1,    -1,   980,    -1,   280,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1860,   291,    -1,  1863,   294,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,  1878,   309,    -1,    -1,    -1,    -1,    -1,    -1,
     316,    -1,    -1,    -1,   320,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,  1032,  1033,    -1,    -1,    -1,  1927,
      -1,    -1,    -1,    -1,    -1,    -1,  1936,   107,    -1,   157,
     158,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     168,   169,    -1,   123,    -1,   125,   362,    -1,    -1,   365,
      -1,    -1,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   378,    -1,    -1,  1975,   382,  1975,    -1,    -1,
      -1,  1957,  1958,    -1,    -1,    -1,    -1,    -1,   158,    -1,
      -1,    -1,    -1,    -1,  1700,  1103,    -1,  1973,    -1,  1107,
    1976,  1707,  2000,    -1,  1112,  1113,  2004,    -1,  1116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1126,    -1,
      -1,    -1,    -1,    -1,    -1,  1133,    -1,    -1,    -1,    -1,
      -1,    -1,  1738,    -1,    -1,    -1,    -1,    -1,    -1,  2015,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2049,
    2050,  2049,  2050,    -1,    -1,  2031,    -1,    -1,    -1,    -1,
    1168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2045,
      -1,    -1,    -1,  1181,    -1,    -1,    -1,  1185,    -1,    -1,
      -1,  1189,    -1,    -1,    -1,    -1,   492,    -1,    -1,    -1,
     107,  2067,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,  1210,  2079,    -1,    -1,    -1,    -1,    -1,    -1,
    2110,  1219,  2110,    -1,    -1,    -1,    -1,    -1,   135,   107,
    1228,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,    -1,    -1,    -1,  1842,  1843,    -1,    -1,
     157,   158,    -1,  1849,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    -1,  1262,    -1,  1862,    -1,    -1,    -1,
     566,    -1,    -1,   180,    -1,    -1,  1872,    -1,  1874,    -1,
     158,    -1,    -1,   161,    -1,    -1,    -1,    -1,   584,  1287,
      -1,  1887,    -1,  1889,  1890,  1891,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   600,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1311,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1320,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,  1333,    -1,    -1,    -1,    -1,
      -1,    -1,  1340,    -1,  1342,    -1,   642,  1943,   644,    -1,
      -1,  1947,    -1,     1,    -1,    -1,  1952,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   660,   661,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   671,    -1,   161,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,   107,   684,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,   697,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2006,    59,    -1,    -1,  1412,    -1,    -1,  1415,  1416,    -1,
    1418,    -1,  2018,    -1,    -1,    -1,  2022,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1433,    -1,   157,    -1,  1437,
      73,  2037,    -1,  1441,    -1,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,   107,
      -1,    -1,  1460,  1461,  1462,  1463,  1464,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,  2082,  1485,    -1,    -1,
      -1,  1489,  1490,   141,   790,   157,    -1,    -1,    -1,    -1,
      -1,    -1,   135,   151,   137,  1503,    -1,    -1,    -1,   107,
     806,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   170,  2119,   157,   158,   822,    -1,  2124,    -1,
     826,    -1,    -1,    -1,    -1,   168,   169,    -1,   834,   835,
      -1,    -1,    -1,   839,    -1,    -1,  1544,   180,    -1,    -1,
      -1,  1549,    -1,    -1,  2150,    -1,    -1,  2153,   854,  2155,
     856,   209,    -1,    -1,   860,   861,   862,   863,    -1,    -1,
      -1,  1569,    -1,    -1,    -1,    -1,    -1,    -1,  2174,    -1,
      -1,    -1,    -1,    -1,   880,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,    -1,    -1,    -1,    -1,
     154,    -1,    -1,   251,    -1,  1603,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1612,    -1,    -1,    -1,  1616,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,   181,    -1,    -1,
      -1,   107,   280,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   291,   940,    -1,    -1,    -1,   944,    -1,
      -1,    -1,    -1,   949,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   309,    -1,    -1,    -1,    -1,    -1,    -1,   316,    -1,
      -1,    -1,   320,    -1,   970,  1673,    -1,   973,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1697,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   362,    -1,    -1,   365,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     378,    -1,    -1,    -1,   382,    -1,    -1,  1033,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
     187,    -1,    -1,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      -1,    56,    -1,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,  1103,    -1,    -1,
      -1,  1107,    -1,    -1,    -1,    -1,  1112,  1113,  1816,    -1,
    1116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1126,    -1,    -1,    -1,    -1,    -1,    -1,  1133,    -1,    -1,
      -1,    -1,    -1,    -1,   492,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1860,    -1,    -1,  1863,    -1,    -1,    -1,    -1,
      -1,    -1,  1168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1878,    -1,    -1,    -1,    -1,  1181,    -1,    -1,    -1,  1185,
      -1,    -1,    -1,  1189,   159,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   182,   566,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1228,    -1,    -1,    -1,   584,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   600,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1262,    -1,    -1,    -1,
      -1,   408,    -1,   410,    -1,  1973,   413,   414,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   423,   424,    -1,    -1,
      -1,  1287,    -1,    -1,   642,    -1,   644,    -1,    -1,    -1,
      -1,   438,   439,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   660,   661,    -1,  1311,    -1,  2015,    -1,    -1,
      -1,    -1,   459,   671,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1340,    -1,  1342,  2045,    -1,   697,
      -1,    -1,  2050,    62,    -1,   492,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2067,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2079,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,   117,    -1,
     119,    -1,    -1,    -1,    -1,    -1,  1412,    -1,    -1,  1415,
    1416,    -1,  1418,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1433,    -1,    -1,
      -1,  1437,   790,    -1,    -1,  1441,    -1,    -1,    -1,   158,
      -1,    -1,   161,   162,    -1,    -1,    -1,    -1,   806,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   822,    -1,    -1,    -1,   826,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   834,   835,    -1,  1485,
      -1,   839,    -1,  1489,  1490,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   214,   854,  1503,   856,    -1,
      -1,    -1,   860,   861,   862,   863,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,
      -1,    -1,   880,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1544,    -1,
      -1,    -1,    -1,  1549,    -1,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   285,    -1,   287,   288,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   940,    -1,    -1,    -1,   944,    -1,    -1,    -1,
      -1,   949,    -1,    -1,   125,    -1,    -1,  1603,    -1,    -1,
     319,    -1,    -1,    -1,    -1,   324,  1612,   138,    -1,   140,
    1616,   330,   970,    -1,    -1,   973,    -1,    -1,    -1,    -1,
     767,   768,   769,   770,   771,   772,   773,   774,   775,   776,
     777,   778,   779,   780,   781,   782,   783,   784,   785,    -1,
     171,    -1,   173,    -1,    -1,   364,    -1,    -1,    -1,    -1,
     369,    -1,   371,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   198,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1033,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1697,    -1,    -1,    -1,    -1,    -1,   416,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   852,    -1,    -1,    -1,    -1,
     241,    -1,    -1,    -1,   245,    -1,    48,   248,   249,    -1,
      -1,   252,    -1,    -1,   255,   256,    -1,   258,    -1,   260,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1103,    -1,    -1,    -1,  1107,
      -1,   470,    -1,    -1,  1112,  1113,    -1,    -1,  1116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1126,    -1,
      -1,    -1,    -1,    -1,    -1,  1133,    -1,    -1,    -1,   498,
      -1,   500,   501,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,   517,    -1,
     331,    -1,    -1,   334,    -1,    -1,   138,    -1,   140,    -1,
    1168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1181,    -1,    -1,    -1,  1185,   359,    -1,
      -1,  1189,   551,    -1,    -1,    -1,    -1,    -1,    -1,   171,
      -1,    -1,    -1,   374,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   575,  1863,    -1,    -1,
      -1,   580,    -1,   582,    -1,    -1,    -1,    -1,    -1,    -1,
    1228,    -1,  1878,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1028,    -1,    -1,    -1,    -1,   605,  1034,   607,   608,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1045,    -1,
     619,    -1,    -1,    -1,  1262,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   632,    -1,    -1,   248,   249,    -1,    -1,
     252,    -1,   641,   255,   256,    -1,   258,    -1,   260,  1287,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1085,    -1,
      -1,    -1,    -1,    -1,   663,    -1,   665,   666,   479,    -1,
      -1,    -1,    -1,  1311,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1973,   687,   688,
      -1,    -1,    -1,    -1,    -1,   694,    -1,    -1,    -1,    -1,
      -1,    -1,  1340,    -1,  1342,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   539,  2015,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   555,    -1,    -1,   359,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2045,
      -1,    -1,   374,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1412,    -1,    -1,  1415,  1416,    -1,
    1418,  2067,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2079,    -1,  1433,    -1,    -1,    -1,  1437,
      -1,  1228,    -1,  1441,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   639,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1485,    -1,    -1,
      -1,  1489,  1490,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1503,    -1,   479,   679,   680,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1311,    -1,    -1,    -1,   699,    -1,
     701,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1544,    -1,    -1,    -1,
      -1,  1549,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1350,  1351,  1352,    -1,    -1,    -1,    -1,
    1357,  1358,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   555,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1381,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1603,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   972,  1612,    -1,    -1,    -1,  1616,    -1,
      -1,    -1,    -1,    -1,   983,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   992,    -1,  1422,  1423,    -1,    -1,   810,
     811,    -1,    -1,    -1,    -1,    -1,   817,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,
      -1,   842,    -1,    -1,   845,   846,    -1,   848,    -1,   850,
     851,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1697,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   679,   680,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     170,    -1,   893,    -1,    -1,    -1,   897,   699,    -1,   701,
     901,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1097,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   209,
      -1,    -1,    -1,    -1,  1123,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,  1565,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
     981,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   810,   811,
      -1,    -1,    -1,    -1,    -1,   817,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   309,
      -1,    -1,  1860,    -1,    -1,  1863,    -1,    -1,    -1,    -1,
     842,    -1,    -1,   845,   846,    -1,   848,    -1,   850,   851,
    1878,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   362,    -1,   364,   365,    -1,     1,    -1,    -1,
      -1,   893,    -1,    -1,    -1,   897,    -1,  1286,   378,   901,
      -1,    -1,   382,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1122,    -1,    -1,    -1,  1742,  1315,    -1,    -1,    -1,
      -1,    -1,  1321,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,    55,    -1,    57,    -1,  1973,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,    -1,  1167,    -1,  1169,    -1,
      -1,  1172,    -1,    -1,  1175,    -1,    -1,    -1,  1179,   981,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2015,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,   492,   127,   128,    -1,    -1,  2045,   498,    -1,
      -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2067,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,  2079,    -1,   167,   168,   169,   170,   171,   172,   173,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1472,    -1,    -1,   566,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   584,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1317,    -1,    -1,    -1,
    1122,    -1,  1939,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   619,
      -1,    -1,  1531,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   642,    -1,    -1,  1167,  1983,  1169,    -1,    -1,
    1172,    -1,    -1,  1175,    -1,    -1,    -1,  1179,    -1,    -1,
     660,   661,    -1,   663,    -1,    -1,  1387,    -1,    -1,    -1,
      -1,   671,    -1,    -1,    -1,    -1,    -1,  1398,    -1,    -1,
    1401,    -1,  1403,  1404,   684,    -1,    -1,   687,    -1,    -1,
      -1,    -1,    -1,    -1,   694,    -1,    -1,   697,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2047,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2073,    -1,  2075,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,  2113,    50,    51,    -1,
      53,    -1,    -1,    56,    -1,    -1,    -1,  1696,    -1,    -1,
     790,  1512,    -1,    -1,    -1,  1317,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,   806,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   822,    -1,    -1,    -1,   826,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   834,   835,   109,   110,    -1,   839,
      -1,  2178,    -1,    -1,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,    -1,    -1,   854,    -1,   856,    -1,    -1,    -1,
     860,   861,   862,   863,   137,  1387,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1398,    -1,    -1,  1401,
     880,  1403,  1404,    -1,    -1,   208,   209,   160,    -1,    -1,
    1611,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1639,    -1,
      -1,    -1,   245,    -1,  1833,    -1,    -1,    -1,    -1,   252,
      -1,    -1,    -1,    -1,    -1,    -1,  1845,    -1,    -1,    -1,
     940,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   949,
    1671,    -1,    -1,    -1,    -1,    -1,  1677,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     970,    -1,   972,   973,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1512,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   334,    -1,    -1,    -1,    -1,    -1,    -1,  1927,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1753,  1033,    -1,    -1,    -1,    -1,    -1,   362,
     363,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1958,
    1959,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   382,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1976,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1797,  1798,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1611,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1822,  1823,  1103,    -1,    -1,    -1,  1107,    -1,  1830,
      -1,    -1,  1112,  1113,  1835,    -1,  1116,  1639,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1126,    -1,    -1,    -1,
      -1,    -1,    -1,  1133,    -1,    -1,    -1,    -1,    -1,   462,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   479,   480,    -1,   482,
     483,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1168,   492,
      -1,    -1,    -1,   496,    49,    -1,    -1,    52,    -1,    54,
      55,  1181,    57,    -1,    -1,  1185,   509,    -1,    -1,  1189,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,  2110,    -1,    -1,    -1,    -1,    -1,    -1,  1929,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   540,    -1,    -1,
      -1,   544,    -1,    -1,    -1,    -1,    -1,    -1,   170,   104,
     105,  1753,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,
     135,   584,    -1,    -1,    -1,    -1,    -1,   209,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1797,  1798,    -1,    -1,  2000,
      -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
    1822,  1823,    -1,    -1,    -1,   180,    -1,    -1,    -1,    -1,
      -1,  1311,    -1,  1835,    -1,  1315,    -1,   640,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2049,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   660,   661,    -1,
    1340,    -1,  1342,    -1,    -1,    -1,    -1,    -1,   671,    -1,
      -1,    -1,   675,    -1,    -1,    -1,    -1,    -1,    -1,   682,
      -1,   684,    -1,    -1,    -1,    -1,    -1,   309,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,  1929,    53,    -1,
      -1,    56,  1412,    -1,    -1,  1415,  1416,    -1,  1418,    -1,
     362,    -1,   364,   365,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,  1433,    -1,    -1,   378,  1437,    -1,    -1,
     382,  1441,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,   790,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2000,    -1,
      -1,    -1,    -1,   806,   807,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,   817,   818,    -1,   820,   821,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   834,   835,    -1,    -1,   160,   839,    -1,   841,   842,
      -1,    -1,    -1,   168,   169,   848,    -1,    -1,    -1,    -1,
      -1,   854,    -1,   856,    -1,    -1,    -1,   860,   861,   862,
     863,    -1,    -1,    -1,  1544,    -1,    -1,    -1,    -1,  1549,
     492,    -1,    -1,    -1,    -1,    -1,   498,   880,    -1,   882,
      -1,    -1,    -1,   886,    -1,    -1,    -1,    -1,    -1,    -1,
     893,   894,    -1,    -1,   897,   898,    -1,    -1,   901,   902,
      -1,    -1,    -1,    -1,    -1,   908,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1603,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1612,    -1,    -1,    -1,  1616,    -1,    -1,    -1,
      -1,    -1,   170,    -1,   566,    -1,   949,   950,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   584,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     983,   209,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   619,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     642,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1032,
    1033,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   660,   661,
      -1,   663,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   671,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   684,    -1,    -1,   687,    -1,    -1,    -1,    -1,
      -1,    -1,   694,    -1,    -1,   697,    -1,    -1,    -1,    -1,
      -1,   309,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1097,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1112,
    1113,    -1,    -1,  1116,  1117,    -1,    -1,    -1,    -1,    -1,
    1123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   362,    -1,   364,   365,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     378,    -1,    -1,    -1,   382,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1168,    -1,    -1,   790,  1172,
    1173,    -1,  1175,  1176,    -1,    -1,  1179,  1180,    -1,    -1,
      -1,    -1,    -1,    -1,   806,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1878,    -1,
     822,    -1,    -1,    -1,   826,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   834,   835,    -1,    -1,    -1,   839,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   854,    -1,   856,    -1,    -1,    -1,   860,   861,
     862,   863,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   880,    -1,
      -1,    -1,    -1,    -1,   492,    -1,    -1,    -1,    -1,    -1,
     498,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1973,    -1,    -1,  1976,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1311,    -1,
      -1,    -1,    -1,    -1,  1317,  1318,    -1,    -1,   940,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   949,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   566,  1342,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   970,    -1,
     972,   973,    -1,    -1,    -1,    -1,   584,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1387,  1388,    -1,  2067,    -1,    -1,
      -1,   619,    -1,    -1,    -1,  1398,  1399,    -1,  1401,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1412,
      -1,  1033,  1415,  1416,   642,  1418,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   660,   661,    -1,   663,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   671,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,   687,
      -1,    -1,    -1,    -1,    -1,    -1,   694,    -1,    -1,   697,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1103,    -1,    -1,    -1,  1107,    -1,    -1,    -1,    -1,
    1112,  1113,    -1,    -1,  1116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1126,    -1,    -1,    -1,    -1,    -1,
      -1,  1133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   425,    -1,
      -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1544,    -1,    -1,    -1,    -1,  1168,    -1,   103,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1181,
      -1,    -1,   790,  1185,    -1,    -1,    -1,  1189,    -1,  1572,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   806,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   822,    -1,    -1,    -1,   826,   154,
      -1,    -1,    -1,   158,    -1,    -1,   834,   835,    -1,    -1,
      -1,   839,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   854,    -1,   856,    -1,
      -1,    -1,   860,   861,   862,   863,    -1,    -1,    -1,    -1,
     195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   880,    -1,   209,    -1,    -1,  1660,    -1,   214,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1677,    -1,    -1,    -1,    -1,    -1,
     577,   578,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1311,
      -1,    -1,    -1,  1315,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   940,    -1,    -1,   270,    -1,    -1,  1340,    -1,
    1342,   949,    -1,    -1,    -1,   195,    -1,    -1,    -1,    -1,
     285,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   209,
      -1,    -1,   970,    -1,   972,   973,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   225,    -1,   227,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   330,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   340,    -1,    -1,    -1,    -1,
    1412,    -1,    -1,  1415,  1416,    -1,  1418,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1033,    -1,   362,    -1,   364,
      -1,  1433,  1815,  1816,    -1,  1437,    -1,   714,    -1,  1441,
     717,    -1,    -1,    -1,    -1,   722,    -1,    -1,  1831,    -1,
      -1,    -1,    -1,    -1,   731,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   750,    -1,    -1,    -1,    -1,    -1,   329,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     425,    -1,    -1,    -1,    -1,  1103,    -1,    -1,    -1,  1107,
      -1,    -1,    -1,    -1,  1112,  1113,    -1,    -1,  1116,   786,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1126,    -1,
      -1,    -1,    -1,    -1,   459,  1133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1544,    -1,  1927,    -1,    -1,  1549,    -1,    -1,
      -1,    -1,  1935,    -1,    -1,    -1,    -1,   492,    -1,    -1,
    1168,    -1,    -1,   498,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1181,    -1,    -1,    -1,  1185,    -1,    -1,
      -1,  1189,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1975,    -1,    -1,    -1,   531,    -1,    -1,    -1,
      -1,  1603,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1612,    -1,    -1,    -1,  1616,    -1,    -1,  2000,  2001,    -1,
      -1,  2004,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   577,   578,    -1,    -1,    -1,    -1,    -1,   584,
      -1,    -1,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2049,  2050,    -1,    -1,
     520,    -1,    -1,    -1,    -1,    -1,   526,    -1,    -1,    -1,
      -1,   531,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1311,    -1,    -1,    -1,  1315,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   661,  2110,   663,    -1,
      -1,    -1,  1340,    -1,  1342,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   146,    -1,    -1,   149,    -1,    -1,    -1,   684,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   706,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   714,
     184,    -1,   717,   633,   719,   720,    -1,   722,    -1,    -1,
      -1,    -1,   196,    -1,    -1,    -1,   731,    -1,    -1,   734,
     735,   736,    -1,    -1,  1412,    -1,    -1,  1415,  1416,    -1,
    1418,   661,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   674,  1433,   230,    -1,    -1,  1437,
      -1,    -1,    -1,  1441,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   790,    -1,    -1,    -1,    -1,
      -1,   711,    -1,    -1,    -1,    -1,    -1,    -1,   272,    -1,
      -1,    -1,    -1,    -1,   724,    -1,  1878,   281,   282,   814,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   292,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   748,   749,
     835,    -1,   752,   307,   754,    -1,    -1,    -1,    -1,    -1,
     760,    -1,   762,   763,    -1,    -1,    -1,    -1,    -1,   854,
      -1,   856,    -1,    -1,    -1,   860,   861,   862,   863,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1544,    -1,   342,    -1,
     790,  1549,    -1,    -1,    -1,   880,   350,   351,    -1,    -1,
      -1,   355,    -1,   803,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   814,    -1,    -1,    -1,    -1,    -1,
      -1,  1973,    -1,    -1,  1976,    -1,    -1,  1254,    -1,    -1,
     830,    -1,    -1,    -1,    -1,   835,    -1,    -1,    -1,    -1,
     394,    -1,    -1,   397,    -1,  1603,   400,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1612,    -1,    -1,    -1,  1616,    -1,
      -1,    -1,    -1,    -1,   949,    -1,    -1,   867,    -1,    -1,
     870,    -1,    -1,    -1,    -1,    -1,    -1,   962,    -1,    -1,
      -1,   881,    -1,    -1,    -1,    -1,    -1,   972,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   909,
      -1,    -1,    -1,    -1,    -1,  2067,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   495,    -1,    -1,    -1,    -1,    -1,    -1,  1033,    -1,
      -1,    -1,    -1,   507,   508,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   962,    -1,     3,    -1,     5,    -1,    -1,    -1,
      -1,    10,   972,   973,    13,    14,    15,    16,    17,    18,
     980,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,  1112,  1113,    -1,
      -1,    -1,    -1,  1033,    73,    74,    -1,    -1,    -1,    -1,
      -1,  1041,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1050,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1485,  1486,
      -1,    -1,  1489,  1490,    -1,    -1,    -1,   106,  1495,    -1,
     109,   110,  1499,    -1,  1501,    -1,  1503,    -1,    -1,    -1,
      -1,    -1,    -1,  1168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1091,   646,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,   655,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
    1878,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,    -1,    -1,    -1,  1219,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   710,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1164,    -1,  1166,    -1,    -1,  1254,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1286,    -1,    -1,    -1,    -1,    -1,   761,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1973,    -1,    -1,  1976,  1304,
    1647,    -1,    -1,    -1,    -1,    -1,  1311,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1248,  1249,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1342,    -1,    -1,
      -1,    -1,  1347,    -1,    -1,  1692,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1712,  1713,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   853,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2067,
     864,    -1,    -1,    -1,    -1,  1315,  1743,    -1,    -1,    -1,
      -1,  1321,    -1,    -1,    -1,    -1,    -1,  1412,    -1,    -1,
    1415,  1416,    -1,  1418,    -1,    -1,    -1,  1337,    -1,    -1,
      -1,    -1,  1342,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1360,    -1,    -1,  1363,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1460,  1461,  1462,  1378,    -1,
    1465,  1466,    -1,    -1,    -1,    -1,    -1,  1472,    -1,   943,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1500,    -1,    -1,    -1,    -1,
    1847,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1855,    -1,
    1857,    -1,    -1,  1860,  1861,    -1,  1863,    -1,    -1,    -1,
      -1,  1868,    -1,    -1,    -1,    -1,  1531,    -1,    -1,    -1,
    1004,    -1,  1452,  1453,    -1,    -1,    -1,    -1,    -1,  1544,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1478,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1497,    -1,    -1,
    1500,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1957,    -1,    -1,    -1,    -1,    -1,    -1,  1964,  1965,    -1,
      -1,    -1,    -1,    -1,  1544,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1553,  1554,    -1,    -1,    -1,    -1,    -1,
    1987,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1570,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1666,    -1,  1583,    -1,    -1,    -1,  1587,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2023,    -1,  2025,    -1,
      -1,  2028,  2029,    -1,     3,    -1,    -1,    -1,  2035,  2036,
      -1,  1696,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1666,    -1,    -1,    -1,
      -1,    -1,  1672,    -1,    73,  2102,  2103,  2104,    -1,    -1,
    1234,  1235,  1236,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1782,    -1,    -1,
      -1,    -1,    -1,  2130,  2131,  2132,    -1,    -1,    -1,    -1,
     109,   110,    -1,    -1,  1268,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1733,     1,  1289,   135,    -1,   137,    -1,
      -1,  1295,    -1,    -1,    -1,  1299,    -1,    -1,    -1,    -1,
      -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1845,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,    -1,  1772,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1780,    -1,    49,  1783,    -1,    52,    -1,    54,    55,    -1,
      57,    -1,    -1,  1878,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    74,    75,  1809,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,    -1,   102,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,    -1,
     127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1973,   156,
     157,  1976,    -1,   160,   161,    -1,    18,    -1,   165,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   182,    -1,    -1,    -1,    -1,
      -1,  1475,  1476,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    74,    75,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,    -1,
     102,    -1,   104,   105,  1984,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,    -1,   127,   128,    -1,    -1,  1563,
      -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   156,   157,    -1,    -1,   160,   161,
      -1,    -1,    18,   165,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     182,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    74,    75,
      -1,    77,    -1,    -1,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,    -1,   102,  1681,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   182,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    55,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    70,    -1,    72,    73,    74,    75,
      -1,    77,    -1,    -1,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   182,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    55,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    70,    -1,    72,    73,    74,    75,
      -1,    77,    -1,    -1,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   182,     3,     4,     5,
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
      -1,    -1,    -1,    -1,   150,   151,   152,   153,    -1,    -1,
      -1,   157,   158,   159,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   182,     3,     4,     5,
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
      -1,    -1,    -1,    -1,   150,   151,   152,   153,    -1,    -1,
      -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   182,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    70,    -1,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,
     106,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,   122,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   180,    -1,   182,     3,     4,     5,
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
      -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,   165,
      -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   180,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   180,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
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
      -1,    -1,    -1,    -1,   135,    -1,   137,   138,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,   159,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,
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
      -1,   165,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   180,     4,     5,     6,
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
     157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   180,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    -1,    -1,
      -1,    70,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
     169,   170,   171,   172,   173,     3,     4,     5,     6,     7,
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
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
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
     168,   169,   170,   171,   172,   173,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
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
     168,   169,   170,   171,   172,   173,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
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
     168,   169,   170,   171,   172,   173,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
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
     168,   169,   170,   171,   172,   173,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
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
     168,   169,   170,   171,   172,   173,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      -1,    56,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    70,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    78,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   156,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,
      56,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    70,    -1,     5,    73,    -1,    -1,
      -1,    -1,    78,    79,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    54,    55,    -1,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,     5,    -1,    -1,    -1,    -1,   135,    -1,   137,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
     169,   170,   171,   172,   173,    49,    -1,    -1,    52,    -1,
      54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,   167,   168,   169,   170,   171,   172,   173,
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
      -1,    -1,   165,    -1,    -1,   168,   169,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,   180,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,   180,    22,    23,    24,
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
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    20,   180,    22,    23,    24,    25,
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
      -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   180,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    70,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      78,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    -1,    -1,    -1,    70,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    78,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,   160,
     161,    -1,     3,    -1,     5,    -1,    -1,   168,   169,    10,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,   160,
     161,    -1,     3,    -1,     5,    -1,    -1,   168,   169,    10,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,   160,
     161,    -1,     3,    -1,     5,    -1,    -1,   168,   169,    10,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    -1,    56,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    73,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   109,   110,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,   109,
     110,    -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    72,    -1,    74,    75,    -1,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    -1,    -1,    94,    95,    96,    97,
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
     170,   171,   172,   173,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    73,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,   107,    56,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,   164,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
      -1,    -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,   160,    22,    23,    24,    25,    26,    27,    28,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,   138,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     159,   160,    13,    14,    15,    16,    17,    18,    -1,    20,
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
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   137,   138,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,   160,
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
      -1,    -1,    -1,   137,   138,    -1,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,   160,    22,    23,    24,
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
      -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   160,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    13,    14,    15,    16,    17,    18,    73,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   109,   110,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,   109,
     110,    -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,
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
      -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,
     109,   110,    -1,    -1,    -1,   168,   169,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
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
      -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,
      -1,   109,   110,    -1,    -1,    -1,   168,   169,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    13,    14,    15,    16,    17,    18,    73,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   109,   110,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,   109,
     110,    -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,
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
     168,   169,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      18,    73,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,   109,   110,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      60,    61,    62,    63,    64,    65,    -1,    13,    14,    15,
      16,    17,    18,    73,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   109,
     110,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,   109,   110,    -1,    -1,    -1,   168,   169,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    13,
      14,    15,    16,    17,    73,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
     109,   110,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,    -1,    -1,    -1,   109,   110,    -1,    -1,   168,
     169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    13,
      14,    15,    16,    17,   168,   169,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,   167,   168,   169,   170,   171,   172,   173,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    55,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,   160,    22,    23,    24,    25,    26,    27,    28,    29,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     160,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   137,    -1,    -1,    -1,    -1,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,   160,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    73,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
     107,    49,   109,   110,    52,    -1,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   107,    -1,   109,   110,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,   137,    49,    -1,    -1,    52,   135,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,   152,   153,    -1,    -1,    74,   157,
     158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      49,   127,   128,    52,    -1,    54,    55,    -1,    57,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,   165,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    49,   127,   128,
      52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,   168,
     169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    49,   127,   128,    52,    -1,    54,
      55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,   165,    -1,   167,   168,   169,   170,   171,
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
      -1,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    49,   127,   128,    52,    -1,
      54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,    -1,    -1,   164,    -1,    -1,   167,   168,   169,   170,
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
     157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   157,    -1,   159,   160,   161,    -1,
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
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    49,    -1,    -1,    52,   135,    54,    55,    -1,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,   157,   158,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
     169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,
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
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
     169,   170,   171,   172,   173,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    49,    53,    -1,    52,    56,    54,
      55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    -1,   127,   128,    -1,    -1,    -1,    -1,    49,   137,
     135,    52,    -1,    54,    55,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,    74,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     433,   415,   413,   190,   162,   301,   303,   314,   406,   407,
     408,   409,   157,   395,   393,   394,    79,   328,   188,   315,
     471,   485,   317,   321,   492,   373,   474,   475,   476,   162,
     184,    18,   223,   317,   473,   495,   427,   427,   471,   315,
     483,   493,   317,   188,   315,   485,   427,   165,   427,   368,
      10,   167,   368,   370,   371,   165,   159,   384,   159,   159,
     431,   180,   220,   221,   222,   223,   182,   383,   493,   193,
     383,   161,   383,   384,   383,   493,   223,   383,   159,   383,
     383,   383,   162,   184,   159,   170,   171,   206,    18,   319,
     159,   163,   159,   168,   169,   159,   158,   223,   229,   223,
     165,   223,   188,   223,   188,   119,   161,   188,   220,   119,
     161,   190,   353,   223,   220,   188,   204,   207,   207,   207,
     208,   208,   209,   209,   210,   210,   210,   210,   211,   211,
     212,   213,   214,   215,   216,   164,   230,   191,   161,   188,
     223,   165,   223,   373,   465,   466,   467,   317,   464,   427,
     427,   223,   384,   157,   427,   468,   471,   157,   468,   471,
     373,   373,   184,   184,   162,   162,   157,   433,   456,   457,
     458,   461,    18,   317,   455,   459,   157,   427,   477,   495,
     427,   427,   495,   157,   427,   477,   427,   427,   185,   219,
     190,   377,   380,   162,   380,   381,   162,   495,   495,   138,
     375,   376,   377,   375,   377,   375,   190,   184,   218,   219,
     223,   425,   494,   386,   388,   156,   184,   159,   184,   159,
     375,   223,   159,   159,   159,   159,   159,   159,   159,   159,
     159,   157,   427,   468,   471,   157,   427,   468,   471,   157,
     427,   468,   471,   424,    22,   471,   223,   324,   340,   469,
     234,   159,   159,   159,   159,   159,   411,   412,   234,   156,
     406,   413,   234,   422,   412,   234,   165,   165,   165,   354,
     138,   378,   379,   188,   190,   296,    18,    72,    74,    75,
      77,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    94,    95,    96,    97,    98,    99,   100,
     102,   109,   110,   122,   157,   161,   190,   230,   231,   232,
     233,   234,   235,   236,   238,   239,   248,   255,   256,   257,
     258,   259,   260,   265,   266,   269,   270,   271,   272,   273,
     274,   275,   281,   282,   283,   297,   317,   321,   423,    71,
     185,   185,   375,   414,   412,   159,   424,   156,   407,   163,
     182,   163,   182,   401,   181,   392,   392,   315,   485,   161,
     168,   202,   223,   340,   223,   317,   159,   159,   159,   159,
       5,   317,   427,   473,   366,   370,   368,   165,   340,   163,
     494,   190,   370,   165,   159,   188,   159,   163,   159,   159,
     163,   159,   184,   163,   159,   159,   159,   163,   159,   204,
     159,   159,   159,   204,    18,   319,   223,   159,   159,   158,
     165,   204,   162,   163,   185,   220,   162,   162,   119,   123,
     125,   189,   197,   198,   199,   159,   197,   162,   163,   156,
     218,   164,   159,   197,   185,   387,   159,   159,   159,   159,
     464,   373,   373,   159,   159,   375,   375,   461,   159,   159,
     159,   159,   157,   433,   460,   455,   459,   373,   373,   162,
     185,   495,   163,   185,   159,   163,   163,   185,   163,   185,
     385,   197,   138,   173,   185,   185,   156,   386,   223,   427,
     375,   427,   185,   157,   427,   468,   471,   157,   427,   468,
     471,   157,   427,   468,   471,   373,   373,   373,   426,   151,
     173,   185,   470,   163,   185,   414,   156,   412,   234,   414,
     354,   354,   354,     3,     5,    10,    74,   156,   298,   305,
     306,   314,   317,   355,   360,   488,   163,   182,   157,    62,
      63,   182,   234,   297,   423,   157,   157,    18,   232,   157,
     157,   182,   190,   182,   190,   168,   190,   165,   231,   157,
     157,   157,   232,   157,   234,   223,   224,   224,    14,   284,
     260,   271,   164,   182,   185,   236,    79,   182,   190,    92,
      93,   264,   268,   113,   136,   263,   112,   135,   267,   263,
     382,   317,   296,   162,   162,   185,   414,   190,   190,   424,
     159,   384,   398,   398,   184,   185,   185,   185,   223,   157,
     427,   477,   471,   316,     5,   168,   185,   223,   368,   494,
     165,   370,    10,   371,   156,   181,   372,   494,   156,   406,
     181,   222,   313,   188,    79,   194,   195,   383,   204,   204,
     204,   204,   204,   165,   387,   158,   223,   163,   156,   200,
     161,   198,   200,   200,   162,   163,   126,   160,   162,   229,
     218,   162,   494,   157,   427,   468,   471,   159,   159,   185,
     185,   159,   157,   427,   468,   471,   157,   427,   477,   433,
     427,   427,   159,   159,   162,   380,   162,   138,   377,   138,
     159,   159,   185,   219,   219,   162,   162,   185,   185,   159,
     373,   373,   373,   159,   159,   159,   385,   163,   223,   223,
     324,   340,   162,   156,   414,   156,   156,   156,   156,   314,
     314,   353,   361,   488,   314,   360,   157,   349,   182,   182,
     182,   157,   164,   202,   356,   357,   363,   433,   434,   447,
     451,   163,   182,   190,   190,   220,   182,   234,   182,   234,
     230,   240,   297,   299,   302,   308,   317,   321,   230,    81,
     159,   240,   150,   151,   152,   153,   158,   159,   182,   230,
     249,   250,   252,   297,   182,   182,   230,   182,   387,   182,
     230,   401,   230,   249,   114,   115,   116,   117,   118,   276,
     278,   279,   182,   101,   182,    85,   157,   159,   427,   156,
     182,   182,   157,   157,   232,   232,   260,   157,   270,   260,
     270,   234,   182,   159,   156,   396,   162,   162,   162,   185,
     373,   223,   223,   185,   162,   185,   165,   156,   370,   494,
     340,   190,   165,   219,   156,   156,   223,   472,   473,   159,
     164,   159,   163,   164,   387,   494,   229,   124,   197,   198,
     161,   198,   161,   198,   162,   156,   373,   159,   159,   373,
     373,   162,   185,   159,   427,   159,   159,   159,   230,   470,
     156,   349,   349,   349,   356,   157,   202,   358,   359,   468,
     479,   480,   481,   482,   182,   163,   182,   356,   182,   401,
     428,   433,   223,   317,   156,   163,   182,   362,   363,   362,
     362,   190,   159,   159,   230,   317,   159,   157,   232,   159,
     150,   151,   152,   153,   173,   182,   253,   254,   232,   231,
     182,   254,   159,   164,   230,   158,   230,   231,   252,   182,
     494,   159,   159,   159,   159,   234,   278,   279,   157,   223,
     157,   191,     1,   232,   204,   261,   230,    76,   111,   262,
     264,    76,   427,   392,   162,   159,   185,   185,   162,   162,
     370,   494,   156,   372,   387,   159,   223,   195,   223,   494,
     156,   162,   162,   197,   197,   159,   427,   427,   159,   159,
     162,   162,   223,   182,   480,   481,   482,   317,   479,   163,
     182,   427,   427,   182,   159,   433,   427,   232,   232,    78,
      79,   165,   243,   244,   245,   159,   230,    76,   232,   230,
     158,   230,    76,   182,   158,   230,   231,   252,   317,   339,
     158,   230,   232,   250,   254,   254,   182,   230,   156,   165,
     245,   232,   232,   157,   280,   315,   317,   488,   182,   191,
     159,   164,   159,   163,   164,   159,   232,   157,   232,   232,
     232,   398,   162,   162,   494,   156,   494,   156,   162,   162,
     159,   159,   159,   479,   427,   357,    76,     1,   219,   241,
     242,   425,     1,   164,     1,   184,   232,   243,    76,   182,
     159,   232,    76,   182,   173,   173,   232,   231,   254,   254,
     182,    58,   230,   251,   340,   173,   173,    76,   158,   230,
     158,   230,   231,   182,     1,   184,   280,   182,   277,   157,
     202,   424,   479,   188,   164,   182,   161,   191,   285,   286,
     287,   204,   220,   230,   263,   156,   156,   157,   427,   468,
     471,   359,   232,   138,     1,   163,   164,   156,   290,   291,
     297,   232,    76,   182,   232,   230,   158,   158,   230,   158,
     230,   158,   230,   231,   188,   340,   158,   230,   158,   230,
     232,   173,   173,   173,   173,   156,   290,   277,   218,   159,
     317,   164,   107,   157,   159,   164,   163,   159,   159,    76,
     259,   373,   219,   241,   244,   246,   247,   297,   232,   173,
     173,   173,   173,   158,   158,   230,   158,   230,   158,   230,
     246,   159,   234,   285,   162,   219,   182,   285,   287,   232,
      76,   159,   232,   237,   185,   244,   158,   158,   230,   158,
     230,   158,   230,   185,   234,   164,   191,   159,   159,   164,
     232,     1,   232,   156,   237,   156,   191,   288,   157,   182,
     288,   163,   164,   219,   159,   191,   188,   289,   159,   182,
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
       5,     3,     3,     4,     6,     7,     0,     2,     1,     1,
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
       4,     1,     1,     3,     3,     2,     3,     3,     2,     4,
       1,     4,     7,     5,     8,     6,     1,     2,     2,     2,
       1,     1,     3,     2,     3,     0,     1,     3,     4,     0,
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
#line 8773 "Parser/parser.cc"
    break;

  case 3:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8779 "Parser/parser.cc"
    break;

  case 4:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8785 "Parser/parser.cc"
    break;

  case 5:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8791 "Parser/parser.cc"
    break;

  case 6:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8797 "Parser/parser.cc"
    break;

  case 7:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8803 "Parser/parser.cc"
    break;

  case 8:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8809 "Parser/parser.cc"
    break;

  case 20:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8815 "Parser/parser.cc"
    break;

  case 24:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8821 "Parser/parser.cc"
    break;

  case 25:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8827 "Parser/parser.cc"
    break;

  case 26:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8837 "Parser/parser.cc"
    break;

  case 27:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8843 "Parser/parser.cc"
    break;

  case 28:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8849 "Parser/parser.cc"
    break;

  case 29:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8855 "Parser/parser.cc"
    break;

  case 31:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8861 "Parser/parser.cc"
    break;

  case 32:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8867 "Parser/parser.cc"
    break;

  case 33:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8873 "Parser/parser.cc"
    break;

  case 34:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8879 "Parser/parser.cc"
    break;

  case 35:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8889 "Parser/parser.cc"
    break;

  case 36:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8895 "Parser/parser.cc"
    break;

  case 37:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8901 "Parser/parser.cc"
    break;

  case 38:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8907 "Parser/parser.cc"
    break;

  case 39:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8913 "Parser/parser.cc"
    break;

  case 40:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8919 "Parser/parser.cc"
    break;

  case 41:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8925 "Parser/parser.cc"
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
#line 8937 "Parser/parser.cc"
    break;

  case 44:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8946 "Parser/parser.cc"
    break;

  case 45:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8952 "Parser/parser.cc"
    break;

  case 47:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8958 "Parser/parser.cc"
    break;

  case 48:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8964 "Parser/parser.cc"
    break;

  case 49:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8970 "Parser/parser.cc"
    break;

  case 50:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8976 "Parser/parser.cc"
    break;

  case 51:
#line 786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8986 "Parser/parser.cc"
    break;

  case 52:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8992 "Parser/parser.cc"
    break;

  case 53:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8999 "Parser/parser.cc"
    break;

  case 54:
#line 798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9005 "Parser/parser.cc"
    break;

  case 55:
#line 800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9011 "Parser/parser.cc"
    break;

  case 56:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9017 "Parser/parser.cc"
    break;

  case 57:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9023 "Parser/parser.cc"
    break;

  case 58:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9029 "Parser/parser.cc"
    break;

  case 59:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9035 "Parser/parser.cc"
    break;

  case 60:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9041 "Parser/parser.cc"
    break;

  case 61:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 9047 "Parser/parser.cc"
    break;

  case 62:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9053 "Parser/parser.cc"
    break;

  case 63:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9059 "Parser/parser.cc"
    break;

  case 64:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9065 "Parser/parser.cc"
    break;

  case 65:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 9071 "Parser/parser.cc"
    break;

  case 66:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 9077 "Parser/parser.cc"
    break;

  case 67:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 9083 "Parser/parser.cc"
    break;

  case 68:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 9089 "Parser/parser.cc"
    break;

  case 69:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 9099 "Parser/parser.cc"
    break;

  case 71:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9105 "Parser/parser.cc"
    break;

  case 73:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9111 "Parser/parser.cc"
    break;

  case 74:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9117 "Parser/parser.cc"
    break;

  case 75:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9123 "Parser/parser.cc"
    break;

  case 76:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9129 "Parser/parser.cc"
    break;

  case 77:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9135 "Parser/parser.cc"
    break;

  case 78:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9141 "Parser/parser.cc"
    break;

  case 79:
#line 877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 9147 "Parser/parser.cc"
    break;

  case 80:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 9153 "Parser/parser.cc"
    break;

  case 81:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 9161 "Parser/parser.cc"
    break;

  case 82:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9167 "Parser/parser.cc"
    break;

  case 83:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 9176 "Parser/parser.cc"
    break;

  case 86:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 9182 "Parser/parser.cc"
    break;

  case 87:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 9188 "Parser/parser.cc"
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
#line 9208 "Parser/parser.cc"
    break;

  case 89:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 9214 "Parser/parser.cc"
    break;

  case 90:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 9220 "Parser/parser.cc"
    break;

  case 91:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 9226 "Parser/parser.cc"
    break;

  case 92:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9232 "Parser/parser.cc"
    break;

  case 93:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9238 "Parser/parser.cc"
    break;

  case 94:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9244 "Parser/parser.cc"
    break;

  case 95:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9250 "Parser/parser.cc"
    break;

  case 96:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9256 "Parser/parser.cc"
    break;

  case 97:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9262 "Parser/parser.cc"
    break;

  case 98:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 9268 "Parser/parser.cc"
    break;

  case 99:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9277 "Parser/parser.cc"
    break;

  case 100:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {  (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9283 "Parser/parser.cc"
    break;

  case 101:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9289 "Parser/parser.cc"
    break;

  case 102:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9295 "Parser/parser.cc"
    break;

  case 103:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9301 "Parser/parser.cc"
    break;

  case 104:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9307 "Parser/parser.cc"
    break;

  case 105:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9313 "Parser/parser.cc"
    break;

  case 106:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9319 "Parser/parser.cc"
    break;

  case 107:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9325 "Parser/parser.cc"
    break;

  case 108:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9331 "Parser/parser.cc"
    break;

  case 110:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9337 "Parser/parser.cc"
    break;

  case 111:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9343 "Parser/parser.cc"
    break;

  case 112:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9349 "Parser/parser.cc"
    break;

  case 113:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9355 "Parser/parser.cc"
    break;

  case 114:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9361 "Parser/parser.cc"
    break;

  case 115:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9367 "Parser/parser.cc"
    break;

  case 116:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9373 "Parser/parser.cc"
    break;

  case 117:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9379 "Parser/parser.cc"
    break;

  case 125:
#line 1010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9385 "Parser/parser.cc"
    break;

  case 127:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9391 "Parser/parser.cc"
    break;

  case 128:
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9397 "Parser/parser.cc"
    break;

  case 129:
#line 1020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9403 "Parser/parser.cc"
    break;

  case 131:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9409 "Parser/parser.cc"
    break;

  case 132:
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9415 "Parser/parser.cc"
    break;

  case 134:
#line 1034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9421 "Parser/parser.cc"
    break;

  case 135:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9427 "Parser/parser.cc"
    break;

  case 137:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9433 "Parser/parser.cc"
    break;

  case 138:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9439 "Parser/parser.cc"
    break;

  case 139:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9445 "Parser/parser.cc"
    break;

  case 140:
#line 1048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9451 "Parser/parser.cc"
    break;

  case 142:
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9457 "Parser/parser.cc"
    break;

  case 143:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9463 "Parser/parser.cc"
    break;

  case 145:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9469 "Parser/parser.cc"
    break;

  case 147:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9475 "Parser/parser.cc"
    break;

  case 149:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9481 "Parser/parser.cc"
    break;

  case 151:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9487 "Parser/parser.cc"
    break;

  case 153:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9493 "Parser/parser.cc"
    break;

  case 155:
#line 1092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9499 "Parser/parser.cc"
    break;

  case 156:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9505 "Parser/parser.cc"
    break;

  case 158:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9511 "Parser/parser.cc"
    break;

  case 161:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9517 "Parser/parser.cc"
    break;

  case 162:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 9523 "Parser/parser.cc"
    break;

  case 163:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 9529 "Parser/parser.cc"
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
#line 9541 "Parser/parser.cc"
    break;

  case 167:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9547 "Parser/parser.cc"
    break;

  case 168:
#line 1141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9553 "Parser/parser.cc"
    break;

  case 172:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9559 "Parser/parser.cc"
    break;

  case 173:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9565 "Parser/parser.cc"
    break;

  case 174:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9571 "Parser/parser.cc"
    break;

  case 175:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9577 "Parser/parser.cc"
    break;

  case 176:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9583 "Parser/parser.cc"
    break;

  case 177:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9589 "Parser/parser.cc"
    break;

  case 178:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9595 "Parser/parser.cc"
    break;

  case 179:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9601 "Parser/parser.cc"
    break;

  case 180:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9607 "Parser/parser.cc"
    break;

  case 181:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9613 "Parser/parser.cc"
    break;

  case 182:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9619 "Parser/parser.cc"
    break;

  case 183:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9625 "Parser/parser.cc"
    break;

  case 184:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9631 "Parser/parser.cc"
    break;

  case 185:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9637 "Parser/parser.cc"
    break;

  case 186:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9643 "Parser/parser.cc"
    break;

  case 188:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9649 "Parser/parser.cc"
    break;

  case 189:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9655 "Parser/parser.cc"
    break;

  case 190:
#line 1189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9661 "Parser/parser.cc"
    break;

  case 192:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9667 "Parser/parser.cc"
    break;

  case 193:
#line 1200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9673 "Parser/parser.cc"
    break;

  case 208:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9679 "Parser/parser.cc"
    break;

  case 210:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 211:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9691 "Parser/parser.cc"
    break;

  case 212:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9702 "Parser/parser.cc"
    break;

  case 213:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9708 "Parser/parser.cc"
    break;

  case 214:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9714 "Parser/parser.cc"
    break;

  case 216:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9720 "Parser/parser.cc"
    break;

  case 217:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9726 "Parser/parser.cc"
    break;

  case 218:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9732 "Parser/parser.cc"
    break;

  case 219:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9738 "Parser/parser.cc"
    break;

  case 220:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9744 "Parser/parser.cc"
    break;

  case 223:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9750 "Parser/parser.cc"
    break;

  case 224:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9757 "Parser/parser.cc"
    break;

  case 225:
#line 1279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9763 "Parser/parser.cc"
    break;

  case 226:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9769 "Parser/parser.cc"
    break;

  case 227:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9775 "Parser/parser.cc"
    break;

  case 228:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9781 "Parser/parser.cc"
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
#line 9795 "Parser/parser.cc"
    break;

  case 230:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9801 "Parser/parser.cc"
    break;

  case 231:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9807 "Parser/parser.cc"
    break;

  case 232:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9816 "Parser/parser.cc"
    break;

  case 233:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9822 "Parser/parser.cc"
    break;

  case 234:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9828 "Parser/parser.cc"
    break;

  case 235:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9834 "Parser/parser.cc"
    break;

  case 236:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9840 "Parser/parser.cc"
    break;

  case 237:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9846 "Parser/parser.cc"
    break;

  case 238:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9852 "Parser/parser.cc"
    break;

  case 239:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9858 "Parser/parser.cc"
    break;

  case 241:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9864 "Parser/parser.cc"
    break;

  case 242:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9870 "Parser/parser.cc"
    break;

  case 243:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9876 "Parser/parser.cc"
    break;

  case 244:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9882 "Parser/parser.cc"
    break;

  case 245:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9888 "Parser/parser.cc"
    break;

  case 246:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9894 "Parser/parser.cc"
    break;

  case 247:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9900 "Parser/parser.cc"
    break;

  case 249:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9906 "Parser/parser.cc"
    break;

  case 250:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9912 "Parser/parser.cc"
    break;

  case 251:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9918 "Parser/parser.cc"
    break;

  case 253:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9924 "Parser/parser.cc"
    break;

  case 254:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9930 "Parser/parser.cc"
    break;

  case 255:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9936 "Parser/parser.cc"
    break;

  case 256:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9945 "Parser/parser.cc"
    break;

  case 257:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9951 "Parser/parser.cc"
    break;

  case 258:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9957 "Parser/parser.cc"
    break;

  case 259:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9963 "Parser/parser.cc"
    break;

  case 260:
#line 1413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9972 "Parser/parser.cc"
    break;

  case 261:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9978 "Parser/parser.cc"
    break;

  case 262:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9984 "Parser/parser.cc"
    break;

  case 263:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9990 "Parser/parser.cc"
    break;

  case 264:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9999 "Parser/parser.cc"
    break;

  case 265:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10005 "Parser/parser.cc"
    break;

  case 266:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10011 "Parser/parser.cc"
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
#line 10030 "Parser/parser.cc"
    break;

  case 269:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10036 "Parser/parser.cc"
    break;

  case 270:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 10044 "Parser/parser.cc"
    break;

  case 271:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10050 "Parser/parser.cc"
    break;

  case 272:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 10056 "Parser/parser.cc"
    break;

  case 273:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10062 "Parser/parser.cc"
    break;

  case 274:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 10068 "Parser/parser.cc"
    break;

  case 275:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10074 "Parser/parser.cc"
    break;

  case 276:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10080 "Parser/parser.cc"
    break;

  case 277:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10089 "Parser/parser.cc"
    break;

  case 278:
#line 1485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 10098 "Parser/parser.cc"
    break;

  case 279:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10104 "Parser/parser.cc"
    break;

  case 280:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10113 "Parser/parser.cc"
    break;

  case 281:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 10122 "Parser/parser.cc"
    break;

  case 282:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 10128 "Parser/parser.cc"
    break;

  case 283:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 10134 "Parser/parser.cc"
    break;

  case 284:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 10140 "Parser/parser.cc"
    break;

  case 285:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 10146 "Parser/parser.cc"
    break;

  case 286:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 10152 "Parser/parser.cc"
    break;

  case 287:
#line 1515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 10158 "Parser/parser.cc"
    break;

  case 288:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10164 "Parser/parser.cc"
    break;

  case 289:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10170 "Parser/parser.cc"
    break;

  case 290:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10179 "Parser/parser.cc"
    break;

  case 291:
#line 1527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10189 "Parser/parser.cc"
    break;

  case 292:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10195 "Parser/parser.cc"
    break;

  case 293:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10201 "Parser/parser.cc"
    break;

  case 294:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10210 "Parser/parser.cc"
    break;

  case 295:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10220 "Parser/parser.cc"
    break;

  case 296:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10226 "Parser/parser.cc"
    break;

  case 297:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10235 "Parser/parser.cc"
    break;

  case 298:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10245 "Parser/parser.cc"
    break;

  case 299:
#line 1562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10251 "Parser/parser.cc"
    break;

  case 300:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 10257 "Parser/parser.cc"
    break;

  case 301:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10263 "Parser/parser.cc"
    break;

  case 302:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10269 "Parser/parser.cc"
    break;

  case 303:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10278 "Parser/parser.cc"
    break;

  case 304:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10288 "Parser/parser.cc"
    break;

  case 305:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10294 "Parser/parser.cc"
    break;

  case 306:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10303 "Parser/parser.cc"
    break;

  case 307:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10313 "Parser/parser.cc"
    break;

  case 308:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10319 "Parser/parser.cc"
    break;

  case 309:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10328 "Parser/parser.cc"
    break;

  case 310:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10338 "Parser/parser.cc"
    break;

  case 311:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10344 "Parser/parser.cc"
    break;

  case 312:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 10352 "Parser/parser.cc"
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
#line 10364 "Parser/parser.cc"
    break;

  case 314:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10371 "Parser/parser.cc"
    break;

  case 315:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false ); }
#line 10378 "Parser/parser.cc"
    break;

  case 316:
#line 1634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10385 "Parser/parser.cc"
    break;

  case 317:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10391 "Parser/parser.cc"
    break;

  case 318:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10397 "Parser/parser.cc"
    break;

  case 319:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10403 "Parser/parser.cc"
    break;

  case 320:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10409 "Parser/parser.cc"
    break;

  case 321:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10415 "Parser/parser.cc"
    break;

  case 322:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10421 "Parser/parser.cc"
    break;

  case 323:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10427 "Parser/parser.cc"
    break;

  case 325:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10433 "Parser/parser.cc"
    break;

  case 326:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10439 "Parser/parser.cc"
    break;

  case 327:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10445 "Parser/parser.cc"
    break;

  case 328:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10451 "Parser/parser.cc"
    break;

  case 329:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10457 "Parser/parser.cc"
    break;

  case 330:
#line 1680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10463 "Parser/parser.cc"
    break;

  case 331:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10469 "Parser/parser.cc"
    break;

  case 332:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10475 "Parser/parser.cc"
    break;

  case 333:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10481 "Parser/parser.cc"
    break;

  case 334:
#line 1692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10487 "Parser/parser.cc"
    break;

  case 335:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10493 "Parser/parser.cc"
    break;

  case 336:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10499 "Parser/parser.cc"
    break;

  case 337:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10505 "Parser/parser.cc"
    break;

  case 338:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10511 "Parser/parser.cc"
    break;

  case 339:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10517 "Parser/parser.cc"
    break;

  case 340:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10523 "Parser/parser.cc"
    break;

  case 341:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10529 "Parser/parser.cc"
    break;

  case 342:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10535 "Parser/parser.cc"
    break;

  case 343:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10541 "Parser/parser.cc"
    break;

  case 344:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10547 "Parser/parser.cc"
    break;

  case 345:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10553 "Parser/parser.cc"
    break;

  case 346:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10559 "Parser/parser.cc"
    break;

  case 349:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10565 "Parser/parser.cc"
    break;

  case 350:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10574 "Parser/parser.cc"
    break;

  case 351:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10580 "Parser/parser.cc"
    break;

  case 352:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10586 "Parser/parser.cc"
    break;

  case 355:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10592 "Parser/parser.cc"
    break;

  case 356:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10598 "Parser/parser.cc"
    break;

  case 359:
#line 1766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10604 "Parser/parser.cc"
    break;

  case 360:
#line 1768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10610 "Parser/parser.cc"
    break;

  case 361:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10616 "Parser/parser.cc"
    break;

  case 362:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10622 "Parser/parser.cc"
    break;

  case 363:
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10628 "Parser/parser.cc"
    break;

  case 364:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10634 "Parser/parser.cc"
    break;

  case 365:
#line 1783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10640 "Parser/parser.cc"
    break;

  case 366:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10646 "Parser/parser.cc"
    break;

  case 367:
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10652 "Parser/parser.cc"
    break;

  case 370:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10658 "Parser/parser.cc"
    break;

  case 371:
#line 1805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10664 "Parser/parser.cc"
    break;

  case 372:
#line 1807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10670 "Parser/parser.cc"
    break;

  case 373:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10676 "Parser/parser.cc"
    break;

  case 374:
#line 1814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10682 "Parser/parser.cc"
    break;

  case 375:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10688 "Parser/parser.cc"
    break;

  case 376:
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10694 "Parser/parser.cc"
    break;

  case 377:
#line 1823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10700 "Parser/parser.cc"
    break;

  case 378:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10706 "Parser/parser.cc"
    break;

  case 379:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10712 "Parser/parser.cc"
    break;

  case 380:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10718 "Parser/parser.cc"
    break;

  case 381:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10724 "Parser/parser.cc"
    break;

  case 382:
#line 1845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10730 "Parser/parser.cc"
    break;

  case 383:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10736 "Parser/parser.cc"
    break;

  case 384:
#line 1852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10742 "Parser/parser.cc"
    break;

  case 385:
#line 1854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-6].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10748 "Parser/parser.cc"
    break;

  case 386:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10754 "Parser/parser.cc"
    break;

  case 387:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10760 "Parser/parser.cc"
    break;

  case 388:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10766 "Parser/parser.cc"
    break;

  case 389:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10772 "Parser/parser.cc"
    break;

  case 390:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10778 "Parser/parser.cc"
    break;

  case 391:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10784 "Parser/parser.cc"
    break;

  case 392:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10790 "Parser/parser.cc"
    break;

  case 394:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10796 "Parser/parser.cc"
    break;

  case 395:
#line 1880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10802 "Parser/parser.cc"
    break;

  case 396:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10808 "Parser/parser.cc"
    break;

  case 401:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10814 "Parser/parser.cc"
    break;

  case 402:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10820 "Parser/parser.cc"
    break;

  case 403:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10826 "Parser/parser.cc"
    break;

  case 404:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10832 "Parser/parser.cc"
    break;

  case 405:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10838 "Parser/parser.cc"
    break;

  case 406:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10844 "Parser/parser.cc"
    break;

  case 407:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10850 "Parser/parser.cc"
    break;

  case 408:
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10856 "Parser/parser.cc"
    break;

  case 411:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10862 "Parser/parser.cc"
    break;

  case 412:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10868 "Parser/parser.cc"
    break;

  case 413:
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10877 "Parser/parser.cc"
    break;

  case 414:
#line 1939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10883 "Parser/parser.cc"
    break;

  case 415:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10889 "Parser/parser.cc"
    break;

  case 416:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10895 "Parser/parser.cc"
    break;

  case 417:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10904 "Parser/parser.cc"
    break;

  case 418:
#line 1953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10913 "Parser/parser.cc"
    break;

  case 419:
#line 1963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10919 "Parser/parser.cc"
    break;

  case 422:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10925 "Parser/parser.cc"
    break;

  case 423:
#line 1975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10931 "Parser/parser.cc"
    break;

  case 425:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10937 "Parser/parser.cc"
    break;

  case 426:
#line 1983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10943 "Parser/parser.cc"
    break;

  case 436:
#line 2009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 10949 "Parser/parser.cc"
    break;

  case 437:
#line 2011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10955 "Parser/parser.cc"
    break;

  case 441:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10961 "Parser/parser.cc"
    break;

  case 443:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10967 "Parser/parser.cc"
    break;

  case 444:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10973 "Parser/parser.cc"
    break;

  case 445:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10979 "Parser/parser.cc"
    break;

  case 446:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10985 "Parser/parser.cc"
    break;

  case 447:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10991 "Parser/parser.cc"
    break;

  case 448:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10997 "Parser/parser.cc"
    break;

  case 449:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11003 "Parser/parser.cc"
    break;

  case 450:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11009 "Parser/parser.cc"
    break;

  case 452:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11015 "Parser/parser.cc"
    break;

  case 453:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11021 "Parser/parser.cc"
    break;

  case 454:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11027 "Parser/parser.cc"
    break;

  case 455:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 11038 "Parser/parser.cc"
    break;

  case 456:
#line 2084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11044 "Parser/parser.cc"
    break;

  case 457:
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11050 "Parser/parser.cc"
    break;

  case 458:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11056 "Parser/parser.cc"
    break;

  case 459:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11062 "Parser/parser.cc"
    break;

  case 460:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11068 "Parser/parser.cc"
    break;

  case 461:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 11074 "Parser/parser.cc"
    break;

  case 462:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 11083 "Parser/parser.cc"
    break;

  case 463:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 11092 "Parser/parser.cc"
    break;

  case 464:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 11101 "Parser/parser.cc"
    break;

  case 465:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 11112 "Parser/parser.cc"
    break;

  case 466:
#line 2142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 11121 "Parser/parser.cc"
    break;

  case 467:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11127 "Parser/parser.cc"
    break;

  case 468:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11133 "Parser/parser.cc"
    break;

  case 469:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11139 "Parser/parser.cc"
    break;

  case 470:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 11147 "Parser/parser.cc"
    break;

  case 471:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 11155 "Parser/parser.cc"
    break;

  case 472:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 11161 "Parser/parser.cc"
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
#line 11176 "Parser/parser.cc"
    break;

  case 476:
#line 2188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11182 "Parser/parser.cc"
    break;

  case 477:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11188 "Parser/parser.cc"
    break;

  case 478:
#line 2193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 11194 "Parser/parser.cc"
    break;

  case 479:
#line 2195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 11200 "Parser/parser.cc"
    break;

  case 480:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 11206 "Parser/parser.cc"
    break;

  case 486:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 11216 "Parser/parser.cc"
    break;

  case 499:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11222 "Parser/parser.cc"
    break;

  case 502:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11228 "Parser/parser.cc"
    break;

  case 503:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11234 "Parser/parser.cc"
    break;

  case 505:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 11240 "Parser/parser.cc"
    break;

  case 506:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 11246 "Parser/parser.cc"
    break;

  case 507:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 11252 "Parser/parser.cc"
    break;

  case 508:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 11258 "Parser/parser.cc"
    break;

  case 509:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 11264 "Parser/parser.cc"
    break;

  case 510:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11270 "Parser/parser.cc"
    break;

  case 512:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11276 "Parser/parser.cc"
    break;

  case 513:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11282 "Parser/parser.cc"
    break;

  case 515:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11288 "Parser/parser.cc"
    break;

  case 516:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 11294 "Parser/parser.cc"
    break;

  case 517:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 11300 "Parser/parser.cc"
    break;

  case 518:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 11306 "Parser/parser.cc"
    break;

  case 519:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 11312 "Parser/parser.cc"
    break;

  case 520:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 11318 "Parser/parser.cc"
    break;

  case 521:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 11324 "Parser/parser.cc"
    break;

  case 522:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 11330 "Parser/parser.cc"
    break;

  case 523:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 11336 "Parser/parser.cc"
    break;

  case 524:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11342 "Parser/parser.cc"
    break;

  case 525:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11348 "Parser/parser.cc"
    break;

  case 526:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 11354 "Parser/parser.cc"
    break;

  case 527:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11360 "Parser/parser.cc"
    break;

  case 528:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11366 "Parser/parser.cc"
    break;

  case 529:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11372 "Parser/parser.cc"
    break;

  case 530:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11378 "Parser/parser.cc"
    break;

  case 531:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11384 "Parser/parser.cc"
    break;

  case 532:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11390 "Parser/parser.cc"
    break;

  case 533:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11396 "Parser/parser.cc"
    break;

  case 534:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11402 "Parser/parser.cc"
    break;

  case 535:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11408 "Parser/parser.cc"
    break;

  case 536:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11414 "Parser/parser.cc"
    break;

  case 537:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11420 "Parser/parser.cc"
    break;

  case 538:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11426 "Parser/parser.cc"
    break;

  case 539:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11432 "Parser/parser.cc"
    break;

  case 540:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11438 "Parser/parser.cc"
    break;

  case 541:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11444 "Parser/parser.cc"
    break;

  case 542:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11450 "Parser/parser.cc"
    break;

  case 543:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11456 "Parser/parser.cc"
    break;

  case 544:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11462 "Parser/parser.cc"
    break;

  case 545:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11468 "Parser/parser.cc"
    break;

  case 546:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11474 "Parser/parser.cc"
    break;

  case 547:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11480 "Parser/parser.cc"
    break;

  case 548:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11486 "Parser/parser.cc"
    break;

  case 549:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11492 "Parser/parser.cc"
    break;

  case 550:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11498 "Parser/parser.cc"
    break;

  case 551:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11504 "Parser/parser.cc"
    break;

  case 552:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11510 "Parser/parser.cc"
    break;

  case 554:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11516 "Parser/parser.cc"
    break;

  case 556:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11522 "Parser/parser.cc"
    break;

  case 557:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11528 "Parser/parser.cc"
    break;

  case 558:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11534 "Parser/parser.cc"
    break;

  case 560:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11540 "Parser/parser.cc"
    break;

  case 561:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11546 "Parser/parser.cc"
    break;

  case 562:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11552 "Parser/parser.cc"
    break;

  case 563:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11558 "Parser/parser.cc"
    break;

  case 565:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11564 "Parser/parser.cc"
    break;

  case 567:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11570 "Parser/parser.cc"
    break;

  case 568:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11576 "Parser/parser.cc"
    break;

  case 569:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11582 "Parser/parser.cc"
    break;

  case 570:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11588 "Parser/parser.cc"
    break;

  case 571:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11594 "Parser/parser.cc"
    break;

  case 572:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11600 "Parser/parser.cc"
    break;

  case 573:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11606 "Parser/parser.cc"
    break;

  case 574:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11612 "Parser/parser.cc"
    break;

  case 575:
#line 2463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11618 "Parser/parser.cc"
    break;

  case 577:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11624 "Parser/parser.cc"
    break;

  case 578:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11630 "Parser/parser.cc"
    break;

  case 579:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11636 "Parser/parser.cc"
    break;

  case 581:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11642 "Parser/parser.cc"
    break;

  case 582:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11648 "Parser/parser.cc"
    break;

  case 583:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11657 "Parser/parser.cc"
    break;

  case 585:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11663 "Parser/parser.cc"
    break;

  case 586:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11669 "Parser/parser.cc"
    break;

  case 587:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11675 "Parser/parser.cc"
    break;

  case 589:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11681 "Parser/parser.cc"
    break;

  case 590:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11687 "Parser/parser.cc"
    break;

  case 592:
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11693 "Parser/parser.cc"
    break;

  case 593:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11699 "Parser/parser.cc"
    break;

  case 594:
#line 2514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11705 "Parser/parser.cc"
    break;

  case 595:
#line 2519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11711 "Parser/parser.cc"
    break;

  case 596:
#line 2521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11717 "Parser/parser.cc"
    break;

  case 597:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11723 "Parser/parser.cc"
    break;

  case 598:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11729 "Parser/parser.cc"
    break;

  case 599:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11735 "Parser/parser.cc"
    break;

  case 600:
#line 2532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11741 "Parser/parser.cc"
    break;

  case 602:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11747 "Parser/parser.cc"
    break;

  case 603:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11753 "Parser/parser.cc"
    break;

  case 604:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11759 "Parser/parser.cc"
    break;

  case 605:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11765 "Parser/parser.cc"
    break;

  case 606:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11771 "Parser/parser.cc"
    break;

  case 611:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11777 "Parser/parser.cc"
    break;

  case 612:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11783 "Parser/parser.cc"
    break;

  case 613:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11792 "Parser/parser.cc"
    break;

  case 614:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11800 "Parser/parser.cc"
    break;

  case 615:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11809 "Parser/parser.cc"
    break;

  case 616:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11818 "Parser/parser.cc"
    break;

  case 617:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11827 "Parser/parser.cc"
    break;

  case 618:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11836 "Parser/parser.cc"
    break;

  case 620:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11842 "Parser/parser.cc"
    break;

  case 621:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11848 "Parser/parser.cc"
    break;

  case 622:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11858 "Parser/parser.cc"
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
#line 11877 "Parser/parser.cc"
    break;

  case 626:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11883 "Parser/parser.cc"
    break;

  case 627:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11889 "Parser/parser.cc"
    break;

  case 628:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11895 "Parser/parser.cc"
    break;

  case 629:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11901 "Parser/parser.cc"
    break;

  case 630:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11907 "Parser/parser.cc"
    break;

  case 631:
#line 2649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11913 "Parser/parser.cc"
    break;

  case 632:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11922 "Parser/parser.cc"
    break;

  case 633:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11928 "Parser/parser.cc"
    break;

  case 634:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11937 "Parser/parser.cc"
    break;

  case 635:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11943 "Parser/parser.cc"
    break;

  case 636:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11952 "Parser/parser.cc"
    break;

  case 637:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11958 "Parser/parser.cc"
    break;

  case 638:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11964 "Parser/parser.cc"
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
#line 11977 "Parser/parser.cc"
    break;

  case 640:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11986 "Parser/parser.cc"
    break;

  case 641:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11992 "Parser/parser.cc"
    break;

  case 642:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11998 "Parser/parser.cc"
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
#line 12011 "Parser/parser.cc"
    break;

  case 644:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12017 "Parser/parser.cc"
    break;

  case 647:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 12023 "Parser/parser.cc"
    break;

  case 648:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12029 "Parser/parser.cc"
    break;

  case 651:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12035 "Parser/parser.cc"
    break;

  case 653:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 12041 "Parser/parser.cc"
    break;

  case 654:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 12047 "Parser/parser.cc"
    break;

  case 655:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12053 "Parser/parser.cc"
    break;

  case 656:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12059 "Parser/parser.cc"
    break;

  case 657:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12065 "Parser/parser.cc"
    break;

  case 658:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12071 "Parser/parser.cc"
    break;

  case 660:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 12077 "Parser/parser.cc"
    break;

  case 662:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 12083 "Parser/parser.cc"
    break;

  case 663:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12089 "Parser/parser.cc"
    break;

  case 665:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 12095 "Parser/parser.cc"
    break;

  case 666:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12101 "Parser/parser.cc"
    break;

  case 668:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 12107 "Parser/parser.cc"
    break;

  case 669:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 12118 "Parser/parser.cc"
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
#line 12132 "Parser/parser.cc"
    break;

  case 671:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 12138 "Parser/parser.cc"
    break;

  case 672:
#line 2805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 12144 "Parser/parser.cc"
    break;

  case 673:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 12150 "Parser/parser.cc"
    break;

  case 674:
#line 2809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 12161 "Parser/parser.cc"
    break;

  case 675:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 12167 "Parser/parser.cc"
    break;

  case 676:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12173 "Parser/parser.cc"
    break;

  case 678:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12179 "Parser/parser.cc"
    break;

  case 679:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12185 "Parser/parser.cc"
    break;

  case 680:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 12191 "Parser/parser.cc"
    break;

  case 681:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 12197 "Parser/parser.cc"
    break;

  case 682:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12206 "Parser/parser.cc"
    break;

  case 683:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12215 "Parser/parser.cc"
    break;

  case 684:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 12221 "Parser/parser.cc"
    break;

  case 685:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 12227 "Parser/parser.cc"
    break;

  case 686:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 12237 "Parser/parser.cc"
    break;

  case 687:
#line 2863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 12243 "Parser/parser.cc"
    break;

  case 688:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 12249 "Parser/parser.cc"
    break;

  case 690:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 12255 "Parser/parser.cc"
    break;

  case 691:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12261 "Parser/parser.cc"
    break;

  case 692:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12267 "Parser/parser.cc"
    break;

  case 693:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12273 "Parser/parser.cc"
    break;

  case 694:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12279 "Parser/parser.cc"
    break;

  case 695:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12285 "Parser/parser.cc"
    break;

  case 697:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12291 "Parser/parser.cc"
    break;

  case 700:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12297 "Parser/parser.cc"
    break;

  case 701:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12303 "Parser/parser.cc"
    break;

  case 702:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 12309 "Parser/parser.cc"
    break;

  case 703:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12315 "Parser/parser.cc"
    break;

  case 706:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12321 "Parser/parser.cc"
    break;

  case 707:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12327 "Parser/parser.cc"
    break;

  case 708:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12333 "Parser/parser.cc"
    break;

  case 710:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12339 "Parser/parser.cc"
    break;

  case 711:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12345 "Parser/parser.cc"
    break;

  case 712:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12351 "Parser/parser.cc"
    break;

  case 714:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12357 "Parser/parser.cc"
    break;

  case 715:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12363 "Parser/parser.cc"
    break;

  case 716:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12369 "Parser/parser.cc"
    break;

  case 717:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12375 "Parser/parser.cc"
    break;

  case 718:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12381 "Parser/parser.cc"
    break;

  case 720:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12387 "Parser/parser.cc"
    break;

  case 721:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12393 "Parser/parser.cc"
    break;

  case 722:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12399 "Parser/parser.cc"
    break;

  case 727:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12405 "Parser/parser.cc"
    break;

  case 729:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12411 "Parser/parser.cc"
    break;

  case 730:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12417 "Parser/parser.cc"
    break;

  case 733:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12423 "Parser/parser.cc"
    break;

  case 736:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12429 "Parser/parser.cc"
    break;

  case 737:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12435 "Parser/parser.cc"
    break;

  case 738:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12441 "Parser/parser.cc"
    break;

  case 739:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12447 "Parser/parser.cc"
    break;

  case 740:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12453 "Parser/parser.cc"
    break;

  case 741:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12459 "Parser/parser.cc"
    break;

  case 742:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12465 "Parser/parser.cc"
    break;

  case 744:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12471 "Parser/parser.cc"
    break;

  case 745:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12477 "Parser/parser.cc"
    break;

  case 746:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12483 "Parser/parser.cc"
    break;

  case 748:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12489 "Parser/parser.cc"
    break;

  case 750:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12495 "Parser/parser.cc"
    break;

  case 751:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12501 "Parser/parser.cc"
    break;

  case 752:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12507 "Parser/parser.cc"
    break;

  case 753:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12513 "Parser/parser.cc"
    break;

  case 754:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12519 "Parser/parser.cc"
    break;

  case 755:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12525 "Parser/parser.cc"
    break;

  case 757:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12531 "Parser/parser.cc"
    break;

  case 758:
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12537 "Parser/parser.cc"
    break;

  case 759:
#line 3087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12543 "Parser/parser.cc"
    break;

  case 760:
#line 3092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12554 "Parser/parser.cc"
    break;

  case 761:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12560 "Parser/parser.cc"
    break;

  case 762:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12566 "Parser/parser.cc"
    break;

  case 763:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12572 "Parser/parser.cc"
    break;

  case 764:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12581 "Parser/parser.cc"
    break;

  case 765:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12587 "Parser/parser.cc"
    break;

  case 766:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12597 "Parser/parser.cc"
    break;

  case 767:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12603 "Parser/parser.cc"
    break;

  case 768:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12609 "Parser/parser.cc"
    break;

  case 769:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12615 "Parser/parser.cc"
    break;

  case 770:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12621 "Parser/parser.cc"
    break;

  case 771:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12627 "Parser/parser.cc"
    break;

  case 772:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12633 "Parser/parser.cc"
    break;

  case 773:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12639 "Parser/parser.cc"
    break;

  case 774:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12645 "Parser/parser.cc"
    break;

  case 775:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12651 "Parser/parser.cc"
    break;

  case 778:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12657 "Parser/parser.cc"
    break;

  case 779:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12663 "Parser/parser.cc"
    break;

  case 780:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12669 "Parser/parser.cc"
    break;

  case 781:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12675 "Parser/parser.cc"
    break;

  case 783:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12681 "Parser/parser.cc"
    break;

  case 784:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12687 "Parser/parser.cc"
    break;

  case 785:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12693 "Parser/parser.cc"
    break;

  case 786:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12699 "Parser/parser.cc"
    break;

  case 787:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12705 "Parser/parser.cc"
    break;

  case 788:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12711 "Parser/parser.cc"
    break;

  case 789:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12717 "Parser/parser.cc"
    break;

  case 790:
#line 3193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12726 "Parser/parser.cc"
    break;

  case 791:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12735 "Parser/parser.cc"
    break;

  case 792:
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12744 "Parser/parser.cc"
    break;

  case 793:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12750 "Parser/parser.cc"
    break;

  case 794:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-6].tok), (yyvsp[-4].decl), (yyvsp[-1].decl) );
		}
#line 12759 "Parser/parser.cc"
    break;

  case 795:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-3].tok), (yyvsp[-5].decl), (yyvsp[-1].decl) ); }
#line 12765 "Parser/parser.cc"
    break;

  case 797:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12771 "Parser/parser.cc"
    break;

  case 802:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12777 "Parser/parser.cc"
    break;

  case 803:
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12783 "Parser/parser.cc"
    break;

  case 804:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12789 "Parser/parser.cc"
    break;

  case 806:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 12795 "Parser/parser.cc"
    break;

  case 807:
#line 3256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12801 "Parser/parser.cc"
    break;

  case 808:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12807 "Parser/parser.cc"
    break;

  case 809:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12813 "Parser/parser.cc"
    break;

  case 811:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12819 "Parser/parser.cc"
    break;

  case 812:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12825 "Parser/parser.cc"
    break;

  case 813:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12831 "Parser/parser.cc"
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
#line 12847 "Parser/parser.cc"
    break;

  case 815:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12853 "Parser/parser.cc"
    break;

  case 816:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12859 "Parser/parser.cc"
    break;

  case 817:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12865 "Parser/parser.cc"
    break;

  case 818:
#line 3297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12871 "Parser/parser.cc"
    break;

  case 819:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12877 "Parser/parser.cc"
    break;

  case 820:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12883 "Parser/parser.cc"
    break;

  case 822:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12892 "Parser/parser.cc"
    break;

  case 823:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12898 "Parser/parser.cc"
    break;

  case 824:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12907 "Parser/parser.cc"
    break;

  case 825:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12917 "Parser/parser.cc"
    break;

  case 826:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12926 "Parser/parser.cc"
    break;

  case 827:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12936 "Parser/parser.cc"
    break;

  case 828:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12947 "Parser/parser.cc"
    break;

  case 829:
#line 3341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12957 "Parser/parser.cc"
    break;

  case 830:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12968 "Parser/parser.cc"
    break;

  case 831:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12978 "Parser/parser.cc"
    break;

  case 832:
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12989 "Parser/parser.cc"
    break;

  case 833:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12999 "Parser/parser.cc"
    break;

  case 834:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13005 "Parser/parser.cc"
    break;

  case 836:
#line 3384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 13011 "Parser/parser.cc"
    break;

  case 837:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 13017 "Parser/parser.cc"
    break;

  case 838:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 13023 "Parser/parser.cc"
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
#line 13035 "Parser/parser.cc"
    break;

  case 840:
#line 3404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13046 "Parser/parser.cc"
    break;

  case 841:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 13055 "Parser/parser.cc"
    break;

  case 842:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 13064 "Parser/parser.cc"
    break;

  case 843:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13070 "Parser/parser.cc"
    break;

  case 844:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13076 "Parser/parser.cc"
    break;

  case 845:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13082 "Parser/parser.cc"
    break;

  case 846:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 13091 "Parser/parser.cc"
    break;

  case 847:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13097 "Parser/parser.cc"
    break;

  case 848:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13103 "Parser/parser.cc"
    break;

  case 849:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 13109 "Parser/parser.cc"
    break;

  case 854:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 13115 "Parser/parser.cc"
    break;

  case 855:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13121 "Parser/parser.cc"
    break;

  case 856:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 13131 "Parser/parser.cc"
    break;

  case 857:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13137 "Parser/parser.cc"
    break;

  case 860:
#line 3483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13143 "Parser/parser.cc"
    break;

  case 861:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 13149 "Parser/parser.cc"
    break;

  case 862:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13155 "Parser/parser.cc"
    break;

  case 863:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13161 "Parser/parser.cc"
    break;

  case 865:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13167 "Parser/parser.cc"
    break;

  case 866:
#line 3503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13173 "Parser/parser.cc"
    break;

  case 867:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 13179 "Parser/parser.cc"
    break;

  case 868:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13185 "Parser/parser.cc"
    break;

  case 870:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 13191 "Parser/parser.cc"
    break;

  case 871:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 13197 "Parser/parser.cc"
    break;

  case 872:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13203 "Parser/parser.cc"
    break;

  case 873:
#line 3553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13209 "Parser/parser.cc"
    break;

  case 874:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13215 "Parser/parser.cc"
    break;

  case 875:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13221 "Parser/parser.cc"
    break;

  case 877:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13227 "Parser/parser.cc"
    break;

  case 878:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13233 "Parser/parser.cc"
    break;

  case 879:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13239 "Parser/parser.cc"
    break;

  case 880:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13245 "Parser/parser.cc"
    break;

  case 881:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13251 "Parser/parser.cc"
    break;

  case 882:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13257 "Parser/parser.cc"
    break;

  case 883:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13263 "Parser/parser.cc"
    break;

  case 884:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13269 "Parser/parser.cc"
    break;

  case 885:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13275 "Parser/parser.cc"
    break;

  case 886:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13281 "Parser/parser.cc"
    break;

  case 887:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13287 "Parser/parser.cc"
    break;

  case 888:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13293 "Parser/parser.cc"
    break;

  case 889:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13299 "Parser/parser.cc"
    break;

  case 890:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13305 "Parser/parser.cc"
    break;

  case 891:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13311 "Parser/parser.cc"
    break;

  case 892:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13317 "Parser/parser.cc"
    break;

  case 893:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13323 "Parser/parser.cc"
    break;

  case 894:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13329 "Parser/parser.cc"
    break;

  case 896:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13335 "Parser/parser.cc"
    break;

  case 897:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13341 "Parser/parser.cc"
    break;

  case 898:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13347 "Parser/parser.cc"
    break;

  case 899:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13353 "Parser/parser.cc"
    break;

  case 900:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13359 "Parser/parser.cc"
    break;

  case 901:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13365 "Parser/parser.cc"
    break;

  case 902:
#line 3634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13371 "Parser/parser.cc"
    break;

  case 903:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13377 "Parser/parser.cc"
    break;

  case 904:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13383 "Parser/parser.cc"
    break;

  case 905:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13389 "Parser/parser.cc"
    break;

  case 906:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13395 "Parser/parser.cc"
    break;

  case 907:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13401 "Parser/parser.cc"
    break;

  case 908:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13407 "Parser/parser.cc"
    break;

  case 909:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13413 "Parser/parser.cc"
    break;

  case 910:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13419 "Parser/parser.cc"
    break;

  case 911:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13425 "Parser/parser.cc"
    break;

  case 915:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13431 "Parser/parser.cc"
    break;

  case 916:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13437 "Parser/parser.cc"
    break;

  case 917:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13443 "Parser/parser.cc"
    break;

  case 918:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13449 "Parser/parser.cc"
    break;

  case 919:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13455 "Parser/parser.cc"
    break;

  case 920:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13461 "Parser/parser.cc"
    break;

  case 921:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13467 "Parser/parser.cc"
    break;

  case 922:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13473 "Parser/parser.cc"
    break;

  case 923:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13479 "Parser/parser.cc"
    break;

  case 924:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13485 "Parser/parser.cc"
    break;

  case 925:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13491 "Parser/parser.cc"
    break;

  case 926:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13497 "Parser/parser.cc"
    break;

  case 927:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13503 "Parser/parser.cc"
    break;

  case 928:
#line 3705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13509 "Parser/parser.cc"
    break;

  case 929:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13515 "Parser/parser.cc"
    break;

  case 930:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13524 "Parser/parser.cc"
    break;

  case 931:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13530 "Parser/parser.cc"
    break;

  case 932:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13536 "Parser/parser.cc"
    break;

  case 934:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13542 "Parser/parser.cc"
    break;

  case 935:
#line 3734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13548 "Parser/parser.cc"
    break;

  case 936:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13554 "Parser/parser.cc"
    break;

  case 937:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13560 "Parser/parser.cc"
    break;

  case 938:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13566 "Parser/parser.cc"
    break;

  case 939:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13572 "Parser/parser.cc"
    break;

  case 940:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13578 "Parser/parser.cc"
    break;

  case 941:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13584 "Parser/parser.cc"
    break;

  case 942:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13590 "Parser/parser.cc"
    break;

  case 943:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13596 "Parser/parser.cc"
    break;

  case 944:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13602 "Parser/parser.cc"
    break;

  case 945:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13608 "Parser/parser.cc"
    break;

  case 946:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13614 "Parser/parser.cc"
    break;

  case 947:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13620 "Parser/parser.cc"
    break;

  case 948:
#line 3769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13626 "Parser/parser.cc"
    break;

  case 949:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13632 "Parser/parser.cc"
    break;

  case 950:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13638 "Parser/parser.cc"
    break;

  case 951:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13644 "Parser/parser.cc"
    break;

  case 953:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13650 "Parser/parser.cc"
    break;

  case 954:
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13656 "Parser/parser.cc"
    break;

  case 955:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13662 "Parser/parser.cc"
    break;

  case 956:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13668 "Parser/parser.cc"
    break;

  case 957:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13674 "Parser/parser.cc"
    break;

  case 958:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13680 "Parser/parser.cc"
    break;

  case 959:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13686 "Parser/parser.cc"
    break;

  case 960:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13692 "Parser/parser.cc"
    break;

  case 961:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13698 "Parser/parser.cc"
    break;

  case 962:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13704 "Parser/parser.cc"
    break;

  case 963:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13710 "Parser/parser.cc"
    break;

  case 964:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13716 "Parser/parser.cc"
    break;

  case 965:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13722 "Parser/parser.cc"
    break;

  case 966:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13728 "Parser/parser.cc"
    break;

  case 967:
#line 3822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13734 "Parser/parser.cc"
    break;

  case 968:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13740 "Parser/parser.cc"
    break;

  case 969:
#line 3834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13746 "Parser/parser.cc"
    break;

  case 970:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13753 "Parser/parser.cc"
    break;

  case 972:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13759 "Parser/parser.cc"
    break;

  case 973:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13765 "Parser/parser.cc"
    break;

  case 974:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13771 "Parser/parser.cc"
    break;

  case 975:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13777 "Parser/parser.cc"
    break;

  case 976:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13783 "Parser/parser.cc"
    break;

  case 977:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13789 "Parser/parser.cc"
    break;

  case 978:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13795 "Parser/parser.cc"
    break;

  case 979:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13801 "Parser/parser.cc"
    break;

  case 980:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13807 "Parser/parser.cc"
    break;

  case 981:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13813 "Parser/parser.cc"
    break;

  case 982:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13819 "Parser/parser.cc"
    break;

  case 983:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13825 "Parser/parser.cc"
    break;

  case 984:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13831 "Parser/parser.cc"
    break;

  case 985:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13838 "Parser/parser.cc"
    break;

  case 987:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13844 "Parser/parser.cc"
    break;

  case 988:
#line 3893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13850 "Parser/parser.cc"
    break;

  case 989:
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13856 "Parser/parser.cc"
    break;

  case 990:
#line 3900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13862 "Parser/parser.cc"
    break;

  case 991:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13868 "Parser/parser.cc"
    break;

  case 992:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13874 "Parser/parser.cc"
    break;

  case 993:
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13880 "Parser/parser.cc"
    break;

  case 994:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13886 "Parser/parser.cc"
    break;

  case 995:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13892 "Parser/parser.cc"
    break;

  case 996:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13898 "Parser/parser.cc"
    break;

  case 997:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13904 "Parser/parser.cc"
    break;

  case 999:
#line 3941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13910 "Parser/parser.cc"
    break;

  case 1000:
#line 3943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13916 "Parser/parser.cc"
    break;

  case 1001:
#line 3948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13922 "Parser/parser.cc"
    break;

  case 1002:
#line 3950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13928 "Parser/parser.cc"
    break;

  case 1003:
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13934 "Parser/parser.cc"
    break;

  case 1004:
#line 3954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13940 "Parser/parser.cc"
    break;

  case 1005:
#line 3956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13946 "Parser/parser.cc"
    break;

  case 1007:
#line 3962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13952 "Parser/parser.cc"
    break;

  case 1008:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13958 "Parser/parser.cc"
    break;

  case 1009:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13964 "Parser/parser.cc"
    break;

  case 1010:
#line 3971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13970 "Parser/parser.cc"
    break;

  case 1011:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13976 "Parser/parser.cc"
    break;

  case 1012:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13982 "Parser/parser.cc"
    break;

  case 1013:
#line 3981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13988 "Parser/parser.cc"
    break;

  case 1014:
#line 3983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13994 "Parser/parser.cc"
    break;

  case 1015:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 14000 "Parser/parser.cc"
    break;

  case 1016:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 14006 "Parser/parser.cc"
    break;

  case 1018:
#line 4004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 14012 "Parser/parser.cc"
    break;

  case 1019:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 14018 "Parser/parser.cc"
    break;

  case 1021:
#line 4009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 14024 "Parser/parser.cc"
    break;

  case 1022:
#line 4011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 14030 "Parser/parser.cc"
    break;

  case 1024:
#line 4017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 14036 "Parser/parser.cc"
    break;

  case 1025:
#line 4019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 14042 "Parser/parser.cc"
    break;

  case 1026:
#line 4024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 14048 "Parser/parser.cc"
    break;

  case 1027:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 14054 "Parser/parser.cc"
    break;

  case 1028:
#line 4028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 14060 "Parser/parser.cc"
    break;

  case 1029:
#line 4030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 14066 "Parser/parser.cc"
    break;

  case 1030:
#line 4064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14072 "Parser/parser.cc"
    break;

  case 1033:
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 14079 "Parser/parser.cc"
    break;

  case 1034:
#line 4074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14085 "Parser/parser.cc"
    break;

  case 1035:
#line 4076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14091 "Parser/parser.cc"
    break;

  case 1036:
#line 4081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 14097 "Parser/parser.cc"
    break;

  case 1037:
#line 4083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 14103 "Parser/parser.cc"
    break;

  case 1038:
#line 4085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14109 "Parser/parser.cc"
    break;

  case 1039:
#line 4087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14115 "Parser/parser.cc"
    break;

  case 1040:
#line 4089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14121 "Parser/parser.cc"
    break;

  case 1042:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14127 "Parser/parser.cc"
    break;

  case 1043:
#line 4097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14133 "Parser/parser.cc"
    break;

  case 1044:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14139 "Parser/parser.cc"
    break;

  case 1045:
#line 4104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 14145 "Parser/parser.cc"
    break;

  case 1046:
#line 4106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14151 "Parser/parser.cc"
    break;

  case 1047:
#line 4108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14157 "Parser/parser.cc"
    break;

  case 1049:
#line 4115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14163 "Parser/parser.cc"
    break;

  case 1051:
#line 4126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 14169 "Parser/parser.cc"
    break;

  case 1052:
#line 4129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14175 "Parser/parser.cc"
    break;

  case 1053:
#line 4131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 14181 "Parser/parser.cc"
    break;

  case 1054:
#line 4134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14187 "Parser/parser.cc"
    break;

  case 1055:
#line 4136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14193 "Parser/parser.cc"
    break;

  case 1056:
#line 4138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 14199 "Parser/parser.cc"
    break;

  case 1058:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14205 "Parser/parser.cc"
    break;

  case 1059:
#line 4155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14211 "Parser/parser.cc"
    break;

  case 1060:
#line 4160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 14217 "Parser/parser.cc"
    break;

  case 1061:
#line 4162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 14223 "Parser/parser.cc"
    break;

  case 1062:
#line 4164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14229 "Parser/parser.cc"
    break;

  case 1063:
#line 4166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14235 "Parser/parser.cc"
    break;

  case 1064:
#line 4168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14241 "Parser/parser.cc"
    break;

  case 1066:
#line 4174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14247 "Parser/parser.cc"
    break;

  case 1067:
#line 4176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14253 "Parser/parser.cc"
    break;

  case 1068:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14259 "Parser/parser.cc"
    break;

  case 1069:
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14265 "Parser/parser.cc"
    break;

  case 1070:
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14271 "Parser/parser.cc"
    break;

  case 1073:
#line 4195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14277 "Parser/parser.cc"
    break;

  case 1076:
#line 4206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14283 "Parser/parser.cc"
    break;

  case 1077:
#line 4208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14289 "Parser/parser.cc"
    break;

  case 1078:
#line 4210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14295 "Parser/parser.cc"
    break;

  case 1079:
#line 4212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14301 "Parser/parser.cc"
    break;

  case 1080:
#line 4214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14307 "Parser/parser.cc"
    break;

  case 1081:
#line 4216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14313 "Parser/parser.cc"
    break;

  case 1082:
#line 4223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14319 "Parser/parser.cc"
    break;

  case 1083:
#line 4225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14325 "Parser/parser.cc"
    break;

  case 1084:
#line 4227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14331 "Parser/parser.cc"
    break;

  case 1085:
#line 4229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14337 "Parser/parser.cc"
    break;

  case 1086:
#line 4231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14343 "Parser/parser.cc"
    break;

  case 1087:
#line 4234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14349 "Parser/parser.cc"
    break;

  case 1088:
#line 4236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14355 "Parser/parser.cc"
    break;

  case 1089:
#line 4238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14361 "Parser/parser.cc"
    break;

  case 1090:
#line 4240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14367 "Parser/parser.cc"
    break;

  case 1091:
#line 4242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14373 "Parser/parser.cc"
    break;

  case 1092:
#line 4247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14379 "Parser/parser.cc"
    break;

  case 1093:
#line 4249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14385 "Parser/parser.cc"
    break;

  case 1094:
#line 4254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14391 "Parser/parser.cc"
    break;

  case 1095:
#line 4256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14397 "Parser/parser.cc"
    break;

  case 1097:
#line 4283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14403 "Parser/parser.cc"
    break;

  case 1101:
#line 4294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14409 "Parser/parser.cc"
    break;

  case 1102:
#line 4296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14415 "Parser/parser.cc"
    break;

  case 1103:
#line 4298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14421 "Parser/parser.cc"
    break;

  case 1104:
#line 4300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14427 "Parser/parser.cc"
    break;

  case 1105:
#line 4302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14433 "Parser/parser.cc"
    break;

  case 1106:
#line 4304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14439 "Parser/parser.cc"
    break;

  case 1107:
#line 4311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14445 "Parser/parser.cc"
    break;

  case 1108:
#line 4313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14451 "Parser/parser.cc"
    break;

  case 1109:
#line 4315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14457 "Parser/parser.cc"
    break;

  case 1110:
#line 4317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14463 "Parser/parser.cc"
    break;

  case 1111:
#line 4319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14469 "Parser/parser.cc"
    break;

  case 1112:
#line 4321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14475 "Parser/parser.cc"
    break;

  case 1113:
#line 4326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14481 "Parser/parser.cc"
    break;

  case 1114:
#line 4328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14487 "Parser/parser.cc"
    break;

  case 1115:
#line 4330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14493 "Parser/parser.cc"
    break;

  case 1116:
#line 4335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14499 "Parser/parser.cc"
    break;

  case 1117:
#line 4337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14505 "Parser/parser.cc"
    break;

  case 1118:
#line 4339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14511 "Parser/parser.cc"
    break;

  case 1121:
#line 4363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14517 "Parser/parser.cc"
    break;

  case 1122:
#line 4365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14523 "Parser/parser.cc"
    break;


#line 14527 "Parser/parser.cc"

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
