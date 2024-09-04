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
#define YYLAST   27777

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  183
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1123
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2196

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
     783,   785,   791,   793,   795,   797,   799,   819,   822,   824,
     826,   828,   830,   832,   834,   836,   838,   840,   842,   844,
     853,   854,   858,   859,   861,   863,   865,   867,   869,   874,
     876,   878,   886,   887,   895,   898,   899,   901,   906,   922,
     924,   926,   928,   930,   932,   934,   939,   941,   944,   946,
     951,   953,   958,   959,   961,   965,   966,   967,   968,   972,
     973,   975,   977,   979,   981,   983,   985,   987,   994,   995,
     996,   997,  1001,  1002,  1006,  1007,  1012,  1013,  1015,  1017,
    1022,  1023,  1025,  1030,  1031,  1033,  1038,  1039,  1041,  1043,
    1045,  1050,  1051,  1053,  1058,  1059,  1064,  1065,  1070,  1071,
    1076,  1077,  1082,  1083,  1088,  1089,  1091,  1096,  1101,  1102,
    1106,  1108,  1113,  1116,  1119,  1124,  1125,  1133,  1139,  1140,
    1144,  1145,  1149,  1150,  1154,  1155,  1156,  1157,  1158,  1159,
    1160,  1161,  1162,  1163,  1164,  1174,  1176,  1181,  1182,  1184,
    1186,  1191,  1192,  1198,  1199,  1205,  1206,  1207,  1208,  1209,
    1210,  1211,  1212,  1213,  1214,  1215,  1216,  1217,  1218,  1220,
    1221,  1227,  1229,  1239,  1241,  1249,  1250,  1255,  1257,  1259,
    1261,  1263,  1267,  1268,  1270,  1276,  1305,  1308,  1310,  1312,
    1322,  1324,  1326,  1331,  1336,  1338,  1340,  1342,  1350,  1351,
    1353,  1357,  1359,  1363,  1365,  1366,  1368,  1370,  1375,  1376,
    1380,  1385,  1386,  1390,  1392,  1397,  1399,  1404,  1406,  1408,
    1410,  1415,  1417,  1419,  1421,  1426,  1428,  1433,  1434,  1456,
    1458,  1462,  1465,  1467,  1470,  1472,  1475,  1477,  1482,  1487,
    1489,  1494,  1499,  1501,  1503,  1505,  1507,  1512,  1514,  1517,
    1519,  1524,  1530,  1533,  1535,  1540,  1546,  1548,  1553,  1559,
    1562,  1564,  1567,  1569,  1574,  1581,  1583,  1588,  1594,  1596,
    1601,  1607,  1610,  1614,  1625,  1630,  1635,  1646,  1648,  1650,
    1652,  1657,  1659,  1661,  1666,  1667,  1669,  1674,  1676,  1681,
    1683,  1685,  1687,  1690,  1694,  1697,  1701,  1703,  1705,  1707,
    1709,  1711,  1713,  1715,  1717,  1719,  1721,  1726,  1727,  1731,
    1737,  1745,  1750,  1751,  1755,  1756,  1761,  1765,  1766,  1769,
    1771,  1776,  1779,  1781,  1783,  1786,  1788,  1793,  1798,  1799,
    1803,  1808,  1810,  1815,  1817,  1822,  1824,  1826,  1831,  1836,
    1841,  1846,  1848,  1850,  1855,  1857,  1863,  1864,  1868,  1869,
    1870,  1871,  1875,  1880,  1881,  1883,  1885,  1887,  1891,  1895,
    1896,  1900,  1902,  1904,  1906,  1908,  1914,  1915,  1921,  1922,
    1926,  1927,  1932,  1934,  1943,  1944,  1946,  1951,  1956,  1967,
    1968,  1972,  1973,  1979,  1980,  1984,  1986,  1990,  1992,  1996,
    1997,  2001,  2002,  2006,  2007,  2008,  2012,  2014,  2029,  2030,
    2031,  2032,  2034,  2038,  2040,  2044,  2051,  2053,  2055,  2063,
    2065,  2070,  2071,  2073,  2075,  2077,  2087,  2089,  2101,  2104,
    2109,  2111,  2117,  2122,  2127,  2138,  2145,  2150,  2152,  2154,
    2160,  2164,  2171,  2173,  2174,  2175,  2191,  2193,  2196,  2198,
    2201,  2206,  2207,  2211,  2212,  2213,  2214,  2223,  2224,  2225,
    2234,  2235,  2236,  2240,  2241,  2242,  2251,  2252,  2253,  2258,
    2259,  2268,  2269,  2274,  2276,  2280,  2282,  2284,  2286,  2293,
    2298,  2303,  2304,  2306,  2316,  2317,  2322,  2324,  2326,  2328,
    2330,  2332,  2335,  2337,  2339,  2344,  2350,  2352,  2354,  2356,
    2358,  2360,  2362,  2364,  2366,  2368,  2370,  2372,  2374,  2376,
    2378,  2380,  2382,  2384,  2386,  2388,  2390,  2392,  2394,  2396,
    2398,  2400,  2402,  2404,  2409,  2410,  2414,  2420,  2421,  2427,
    2428,  2430,  2432,  2434,  2439,  2441,  2446,  2447,  2449,  2451,
    2456,  2458,  2460,  2462,  2464,  2466,  2471,  2472,  2474,  2476,
    2481,  2483,  2482,  2486,  2494,  2495,  2497,  2499,  2504,  2505,
    2507,  2512,  2513,  2515,  2517,  2522,  2524,  2526,  2531,  2533,
    2535,  2537,  2538,  2540,  2545,  2547,  2549,  2554,  2555,  2559,
    2560,  2567,  2566,  2571,  2570,  2580,  2579,  2590,  2589,  2599,
    2604,  2605,  2610,  2616,  2634,  2635,  2639,  2641,  2643,  2648,
    2650,  2652,  2654,  2659,  2661,  2666,  2668,  2677,  2678,  2683,
    2692,  2697,  2699,  2701,  2710,  2712,  2713,  2714,  2716,  2718,
    2719,  2724,  2725,  2726,  2731,  2733,  2736,  2739,  2746,  2747,
    2748,  2754,  2759,  2761,  2767,  2768,  2774,  2775,  2779,  2787,
    2794,  2807,  2806,  2810,  2813,  2812,  2821,  2825,  2829,  2831,
    2837,  2838,  2843,  2848,  2857,  2858,  2860,  2866,  2868,  2873,
    2874,  2880,  2881,  2882,  2891,  2892,  2894,  2895,  2900,  2901,
    2902,  2904,  2910,  2911,  2913,  2914,  2915,  2917,  2919,  2926,
    2927,  2929,  2931,  2936,  2937,  2946,  2948,  2953,  2955,  2960,
    2961,  2963,  2966,  2968,  2972,  2973,  2974,  2976,  2978,  2986,
    2988,  2993,  2994,  2995,  3000,  3001,  3006,  3007,  3008,  3009,
    3013,  3014,  3019,  3020,  3021,  3022,  3023,  3037,  3038,  3043,
    3044,  3050,  3052,  3055,  3057,  3059,  3082,  3083,  3089,  3090,
    3096,  3095,  3105,  3104,  3108,  3114,  3116,  3126,  3127,  3129,
    3133,  3138,  3140,  3142,  3144,  3150,  3151,  3155,  3156,  3161,
    3163,  3170,  3172,  3173,  3175,  3180,  3182,  3184,  3189,  3191,
    3196,  3201,  3209,  3214,  3216,  3221,  3226,  3227,  3232,  3233,
    3237,  3238,  3239,  3245,  3247,  3249,  3255,  3257,  3262,  3264,
    3270,  3271,  3275,  3279,  3283,  3285,  3297,  3299,  3301,  3303,
    3305,  3307,  3309,  3310,  3315,  3318,  3317,  3329,  3328,  3341,
    3340,  3354,  3353,  3367,  3366,  3379,  3384,  3390,  3392,  3398,
    3399,  3410,  3417,  3422,  3428,  3431,  3434,  3438,  3444,  3447,
    3450,  3455,  3456,  3457,  3458,  3462,  3470,  3471,  3483,  3484,
    3488,  3489,  3494,  3496,  3498,  3503,  3504,  3510,  3511,  3513,
    3518,  3519,  3521,  3556,  3558,  3561,  3566,  3568,  3569,  3571,
    3576,  3578,  3580,  3582,  3587,  3589,  3591,  3593,  3595,  3597,
    3599,  3604,  3606,  3608,  3610,  3619,  3621,  3622,  3627,  3629,
    3631,  3633,  3635,  3640,  3642,  3644,  3646,  3651,  3653,  3655,
    3657,  3659,  3661,  3673,  3674,  3675,  3679,  3681,  3683,  3685,
    3687,  3692,  3694,  3696,  3698,  3703,  3705,  3707,  3709,  3711,
    3713,  3725,  3730,  3735,  3737,  3738,  3740,  3745,  3747,  3749,
    3751,  3756,  3758,  3760,  3762,  3764,  3766,  3768,  3773,  3775,
    3777,  3779,  3788,  3790,  3791,  3796,  3798,  3800,  3802,  3804,
    3809,  3811,  3813,  3815,  3820,  3822,  3824,  3826,  3828,  3830,
    3840,  3842,  3845,  3846,  3848,  3853,  3855,  3857,  3862,  3864,
    3866,  3868,  3873,  3875,  3877,  3891,  3893,  3896,  3897,  3899,
    3904,  3906,  3911,  3913,  3915,  3920,  3922,  3927,  3929,  3946,
    3947,  3949,  3954,  3956,  3958,  3960,  3962,  3967,  3968,  3970,
    3972,  3977,  3979,  3981,  3987,  3989,  3992,  3999,  4001,  4010,
    4012,  4014,  4015,  4017,  4019,  4023,  4025,  4030,  4032,  4034,
    4036,  4071,  4072,  4076,  4077,  4080,  4082,  4087,  4089,  4091,
    4093,  4095,  4100,  4101,  4103,  4105,  4110,  4112,  4114,  4120,
    4121,  4123,  4132,  4135,  4137,  4140,  4142,  4144,  4158,  4159,
    4161,  4166,  4168,  4170,  4172,  4174,  4179,  4180,  4182,  4184,
    4189,  4191,  4199,  4200,  4201,  4206,  4207,  4212,  4214,  4216,
    4218,  4220,  4222,  4229,  4231,  4233,  4235,  4237,  4240,  4242,
    4244,  4246,  4248,  4253,  4255,  4257,  4262,  4288,  4289,  4291,
    4295,  4296,  4300,  4302,  4304,  4306,  4308,  4310,  4317,  4319,
    4321,  4323,  4325,  4327,  4332,  4334,  4336,  4341,  4343,  4345,
    4363,  4365,  4370,  4371
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

#define YYPACT_NINF (-1842)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1122)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     115, 11413,   168,   181, 20986,   106, -1842, -1842, -1842, -1842,
   -1842, -1842, -1842, -1842, -1842, -1842, -1842, -1842,   -15,  1071,
     118, -1842, -1842, -1842, -1842, -1842, -1842, -1842, -1842, -1842,
   -1842, -1842, -1842, -1842, -1842, -1842, -1842, -1842, -1842, -1842,
   -1842, -1842, -1842, -1842, -1842, -1842, -1842, -1842,   183,   276,
   -1842, -1842, -1842, -1842, -1842, -1842,  5491,  5491,   164, 11413,
     256,   293, 24665, -1842,   319, -1842, -1842, -1842, -1842, -1842,
   -1842, -1842, -1842, -1842, -1842,   327,  3709, -1842,   794,   368,
   -1842, -1842,  2907, -1842, -1842, -1842, -1842, 15139, -1842,   404,
     435,   483,   328,    43, -1842,  5521,   534,   540,   549,   543,
    4352,   765,  1062, 11593, -1842, -1842,   728, 14980,  3272, -1842,
   -1842, -1842, -1842,  3490,   768,  5677,  8210,   762,  3490,  1023,
     679, -1842, -1842, -1842, -1842,   129, -1842, -1842, -1842, -1842,
     685, -1842, -1842, -1842, -1842, -1842,   708,   677,   129, -1842,
     129, 19054, -1842, -1842, -1842, 22184,  5491, -1842, -1842,  5491,
   -1842, 11413, -1842,   686, 22238, -1842, -1842,  4562, 23609, -1842,
   -1842,  1086,  1086,   710,  2669, -1842, -1842, -1842, -1842,   143,
   17328,   129,  4281,   129, -1842, -1842, -1842, -1842, -1842, -1842,
     738, -1842,   744,   797,  2273, -1842,   852, 26954, -1842, -1842,
   -1842, -1842, -1842, -1842, -1842, 19438,  2805,  2382,  3709,   -62,
     831,   838,   845,   847,   862,   890, -1842, -1842, 21143, 12821,
   -1842,   825,   858, -1842, 15457, -1842, -1842, -1842, -1842,   909,
   -1842, -1842, -1842,   915, -1842, 24947,  1058, 25101, -1842,   931,
    5491,   677,   934,   963, -1842,  2907,  4562,  2907, -1842, -1842,
   -1842,  2976,  4964,   959,  1011,   359,  1011, -1842,   129,   129,
      72, 18728,   635,  1011, -1842,   129,   129,    72,   129, -1842,
     129, -1842,  5147, -1842, -1842,   972,   982,  1086, 24418,   984,
   15139, -1842,  5521, -1842,  3490, -1842,  2444,   679,   983,  1067,
   18728,  5491,  5491,   328, -1842, 12125, -1842,  1086,  1086,  1000,
    1067, 18728,  5491, -1842, 27115, -1842, -1842, -1842,  1086, -1842,
   -1842, -1842, -1842,  1086, -1842,   805,  4997,  5491, -1842, 20681,
    1012, -1842, -1842, -1842, 24279,   677, 18891,   998,  4562, 20542,
   24418,  3490, -1842, -1842, 23814, -1842,  1011,    20, -1842, 26954,
   23664,  3237,  5147, -1842,   654, -1842, -1842, -1842, -1842, -1842,
   22238,  1011,  5491, -1842,  1017,  1020, -1842, -1842, -1842, -1842,
    5491,  4250,   437,    21, -1842,  5491,   744, -1842,   801,   129,
   -1842,  1039, 22395,   668, 17826, 24472,  3490, -1842,  3490,  1086,
    3490,  1086, -1842, -1842,   129, -1842, -1842,  1029, 22449, -1842,
   -1842, -1842, 22606,   909, -1842,  3580,   142,   688, -1842,   386,
     679,  1060,  1061, -1842,  2669,  1073,   744,  2669, -1842, -1842,
    2805, -1842,    55, -1842,  1095, -1842,  1098,  1162, 27118,  1129,
   27202,  1153,  1156, 26954, 27279,  1167, 24781, -1842, -1842, -1842,
   -1842, -1842, -1842, 27356, 27356, 19277,  1163,  4979, -1842, -1842,
   -1842, -1842,   647, -1842,   696, -1842,  1335, -1842, 26954, 26954,
   -1842,  1179,   444,   935,  1033,   542,  1131,  1182,  1195,  1186,
    1236,    18, -1842,   594, -1842,  1219, -1842,  1057,  3378, 19921,
   -1842, -1842,   894,  1219, -1842, -1842,   763, -1842, -1842,   771,
    2382,  1237,  1291,  1294,  1298,  1319,  1321, -1842, -1842,   689,
    1229, -1842,   779,  1229,  1268, -1842,  1325, -1842, 22184, -1842,
    1114,  1328, 20082, -1842, -1842,  5350,  3972,  1348, 17826,  1356,
     896,  1068,  1334,  1336, -1842, -1842, -1842,  5491,  5454, 21659,
   -1842, -1842, -1842, -1842, -1842, -1842, -1842, 20490,  3614,  1163,
   24947,  1354,  1357, -1842, -1842,  1377, 25101,   718, -1842, -1842,
   -1842, 19921,  1393, -1842,   852, -1842, -1842, -1842,  1385,  2976,
     841,  1398,  1416,  1418,   884,  1419,  1420,  1421,  1423,  1424,
    1431,  4964, -1842, -1842, -1842,   129,  1414,  1438, -1842, -1842,
    1442,   328, -1842, -1842,   677,  1067, 21309, -1842, -1842,   328,
   -1842, -1842,   677, -1842, -1842,  5147, -1842, 19921, 19921, -1842,
    1086,  4562, 24611,  3151, 17992, -1842, -1842, -1842, -1842, -1842,
   -1842,   677,  1067,    20,  1440, -1842, -1842,  3490,  1444,  1067,
   18728, -1842,   677,  1067, -1842, 27517, -1842,  1086,  1086, -1842,
   -1842,  1445,   289,  1447,   679,  1448, -1842, -1842, -1842, 21605,
    1457,  1455, -1842, -1842,   817, -1842,  1536, -1842,  1441, -1842,
   -1842, -1842, 22772, 27520, -1842, -1842, -1842, -1842, -1842,  3237,
     995,  5147, 21309,  1011, 11413, -1842,  5491,  1460, 14628,  1467,
   -1842, -1842, -1842, -1842, -1842,  2669, -1842, -1842,  1546,  5315,
   21816, 12821, -1842, 22826, -1842,  1086,  1086, -1842, -1842,   909,
   -1842, 16830,  1464,  1611, 26954,  1177,  1442,  1449, -1842,   129,
     129, -1842,  1229, -1842, 22395, -1842, -1842, 21605,  1086,  1086,
   -1842,  5315, -1842, -1842, 23459, -1842, -1842, 22449, -1842,   129,
    1466,   129,  1061,   617,  1469,   869, 22238,   883,   897, -1842,
    2805, 24788,  1450, -1842, 19599, -1842,  4979, 19760, -1842, 22983,
   22238, -1842, 19599, -1842, 26954, -1842, -1842, -1842, -1842, -1842,
   -1842, 19760, -1842, -1842, 21870, 22983, 22983,  1141,  1406,  1426,
     648,  1530, -1842,   906,  1477,  1171,  1478, -1842, 25178, 26954,
   25255,  1474, 26954,  2907, 26954,  2907, -1842,  3984, -1842, -1842,
   24788,  2919, 26954, 24788,  2907, -1842, -1842, 26954, 26954, 26954,
   26954, 26954, 26954, 26954, 26954, 26954, 26954, 26954, 26954, 26954,
   26954, 26954, 26954, 26954, 26954, 26954, 25332, -1842,   852,  4215,
   12821, -1842, -1842, -1842, -1842, -1842, -1842, -1842, -1842, -1842,
   -1842, -1842,  1476, 26954, -1842, -1842, 16996,  2172, -1842, -1842,
     129,   129, -1842, -1842, 19921, -1842, -1842,   694,  1229, -1842,
     790,  1229, 21309, -1842, -1842,  1442, 21309, -1842,  1442, -1842,
   27604, -1842, -1842, -1842, 20829, 12821,  1489,  1180,  1492, 11948,
    1640,  4479,   697,  1449, -1842,   129,   129,  1449,   733, -1842,
     129,   129, 26954,  5491, 17992,  1497, 17992,  1500,  1449,   263,
   17162, 17162, 18158, 17162,  5491, -1842, -1842, 26954,  1377, -1842,
   24947,  1507, -1842,  2608, -1842, -1842, -1842,   956, -1842,  1505,
   17162, 26954,   870,  1509,  1510,  1511,   964,  1512,  1514,  1517,
    1518,  1522,  1524,   788,  1229, -1842, -1842,   791,  1229, -1842,
   -1842,   798,  1229, -1842, -1842, -1842,  4562,  1663,  1229, 23964,
   -1842, -1842,   677,  1527, -1842, -1842, -1842,   988,  1529,  1026,
    1531, -1842,  1268,  1532,  1533, -1842,   677, -1842, 10365, -1842,
     677,  1067,  1533, -1842,   677,  1526,  1537,  1540, -1842, -1842,
   21466, -1842,  2907,  5491, 10873,  1621, -1842,  1328, -1842, 17162,
    1043, -1842,  1533,  1539, -1842, -1842, -1842, -1842,  4562, 23037,
   14000, -1842,   286,   329, 19921,  1515, -1842,  1515, -1842, -1842,
   -1842, -1842, 22449, -1842, 12991, 20243, -1842,  1548,  1550,  1551,
    1552, -1842, 14694,   129, -1842,  1177, -1842, -1842, -1842, -1842,
    1442, -1842, -1842, -1842,  1086, -1842, -1842, -1842, -1842,   617,
    1061,  1547,   143, -1842, -1842,  1553,  5491,   617, -1842, -1842,
    1554,  1556, -1842,  2907,  1558,  1555, -1842, -1842, -1842,  1562,
    1566,  1563,  1570,  1568,  1569,  1572,  1577,  1578,  1575,  1581,
   26954,  1582,  1583,  1584, 23194, 13161, 26954, -1842, -1842,  1631,
   -1842, -1842, -1842, 26954, -1842,  1589,  1590, 25024, -1842, -1842,
    1185, -1842, 24788,  1588, -1842,  1592, -1842, -1842,  4003, -1842,
    1596, -1842,  4003, -1842, -1842,  1231,  1557, -1842, -1842,  1179,
    1179,  1179,   444,   444,   935,   935,  1033,  1033,  1033,  1033,
     542,   542,  1131,  1182,  1195,  1186,  1236, 26954,  1204,  1598,
    4003, -1842, -1842, 24947, -1842,  1599,  1602,  1604,  1606,  2172,
   -1842, -1842, -1842, -1842, -1842, 21309, -1842, -1842,  1442, 21309,
   -1842,  1442,  1607,  1615, 17162, 17162, -1842, -1842, 11948,  1070,
    1618,  1620,  1624,  1627,  2458,  4479, -1842, -1842, 21309, -1842,
   -1842, -1842, -1842, -1842, -1842, 21309, -1842, -1842, -1842, -1842,
    1608, -1842,  1449,  1617, -1842, -1842, -1842, -1842, -1842, -1842,
   -1842, -1842,  1630,  1633,  1634, -1842,  1635, -1842,   328,  4003,
    1238,   155, -1842, -1842,  1595, -1842, 25101, -1842, 26954,   129,
   17162,   129, -1842, -1842,   828,  1229, -1842,   834,  1229, -1842,
   -1842,   846,  1229, 21309, -1842, -1842,  1442, 21309, -1842, -1842,
    1442, 21309, -1842, -1842,  1442,  1011, -1842,  1442,   290, -1842,
    1219,  1637, -1842, -1842, -1842, -1842, -1842, -1842,  1638, -1842,
   -1842, -1842, 14157,  1533, -1842,   677, -1842, -1842, -1842, -1842,
   -1842, 15962, -1842, -1842, -1842, -1842, -1842,   361,   545,   485,
   12651,  1538,  1644, 18548,  1648,  1649,  2487,  3434,  4141, 25409,
    1651, -1842, -1842,  1652,  1653, 18548,  1655, -1842, -1842,   677,
   26954, 26954,  1776,  1650,   629, -1842, 19116,  1245,  1658,  1656,
    1647, -1842, -1842, -1842, 10679, -1842, -1842, -1842, -1842, -1842,
    3249, -1842, -1842, -1842,  1324,   400, -1842,   414, -1842,   400,
   -1842, -1842, -1842, -1842, -1842,  2907, -1842, -1842, 11771, 15298,
   -1842,  5491,  1654,  1668, -1842, -1842, -1842,  5491, -1842, -1842,
   -1842,  5491, -1842,  4562, -1842,  1044, 22238,   744,   744, -1842,
   -1842,  1163,  1328, 20082, -1842,  1219, -1842, 13331, -1842,   856,
    1229, -1842,  1086, 14819, -1842, -1842,  1061,  1553,  1667,   617,
     679,   258,  1677,  1659,  1553, 14314, -1842,  1661, -1842, 24788,
     701, -1842, 21605,   701, 13161,  2907, -1842,   701, -1842, 22027,
     701, -1842, 26954, 26954, 26954, -1842, -1842, -1842, -1842, 26954,
   26954,  1670, 24947, -1842, -1842, 25486,  1674,  1683, -1842, -1842,
   -1842,  5330, -1842, -1842,  1266, -1842,    49, -1842,  1274, -1842,
   25178, -1842, -1842, 26954, -1842,  1284,  1307,  1377, -1842,   911,
    1229, -1842, -1842,  1686,  1688, -1842, -1842, -1842, -1842,  1689,
     936,  1229, -1842,   937,  3069,   129,   129, -1842, -1842,  1690,
    1693, -1842,  1691, -1842, 17992,  1692, -1842, 17494, 17660,  1698,
   18158,  1702, -1842,  1699, 26954, 26954,  1322,  1703, -1842, -1842,
   -1842, -1842, -1842, -1842,  1705, 21309, -1842, -1842,  1442, 21309,
   -1842, -1842,  1442, 21309, -1842, -1842,  1442,  1707,  1711,  1712,
     328, -1842, -1842,  1326, 26954, 24118,  1710,  1718, -1842, -1842,
   -1842,  1720, 16122, 16282, 16442, 23248, 24418, 22983, 22983,  1722,
    1695,   370,   410,  2308, 16664, -1842,   448,  5491,  5491, -1842,
   24788,    35,   397, -1842, -1842, -1842, -1842, 12651, 26954,  1724,
    1801, 12480, 11053, -1842,  1701, -1842,  1713, 26954,  1714, 24947,
    1715, 26954, 19921, 26954, -1842, 11233,  1056, -1842,  1719,   -18,
   -1842,    42,  1792,   395,   129, -1842,  1731, -1842,  1721, -1842,
    1723,  1732,  1737, 18548, 18548, -1842, -1842,  1818, -1842, -1842,
      47,    47,   401, 12302,   466, -1842, -1842,  1751,  1757,   437,
   -1842, -1842, -1842, -1842, -1842, -1842, 13501,  1752,  1754,  1755,
   -1842, 21309, -1842, -1842,  1442, 26954, 26954,  1328,  1756, -1842,
    1758,  1763,   617,  1553,   143,  5491, -1842, 25563, -1842,  1764,
   -1842, 14471, 26954, -1842,   981,  1762,  1766,  1063, -1842,  1768,
   -1842, -1842, -1842, -1842, -1842, 24947,  1377, -1842, -1842, 25178,
   -1842,  1802,  4003, -1842,  1802,  1802, -1842,  4003,  5434,  5468,
   -1842,  1340, -1842, -1842,  1772, 21309, -1842, -1842,  1442, -1842,
   -1842,  1774,  1775,   129, 21309, -1842, -1842,  1442, 21309, -1842,
   -1842,  1779, -1842, -1842, -1842, -1842, -1842, -1842, -1842, -1842,
    1617, -1842, -1842, -1842,  1773, -1842, -1842, -1842, -1842,  1777,
    1781,   129,  1783,  1784,  1786, -1842, -1842, -1842, -1842, 26954,
   -1842,   290, -1842,  1219, -1842, -1842,  1791, -1842,  1722,  1722,
    1722,  3821,  1041,  1770,   503, -1842,  3821,   524, 19921, -1842,
   -1842, -1842, -1842,  3599, 26954,  5160,   223, -1842, -1842,    50,
    1790,  1790,  1790,  5491, -1842, -1842, -1842,  1800, -1842, -1842,
   -1842, -1842,  1656,  1804, 26954,   435,  1787,   543, 16609, 23248,
    1080,  1803, 18548,  1805, -1842, -1842, -1842, -1842,  1222, 18548,
   26954,  1065,   709, -1842, 26954, 24865, -1842, -1842,   532, -1842,
    1377, -1842,  1082,  1083,  1116,   773, -1842, -1842, -1842, -1842,
     677,  1056,  1809, -1842, -1842, 26954, -1842,  1810,   852, -1842,
   10497, -1842, -1842, -1842, 26954, 26954, -1842, -1842,   316,    47,
   -1842,   723, -1842, -1842, -1842,   129, -1842,  1515, -1842, -1842,
   -1842,  1799,  1811, -1842, -1842,  1806, -1842,  1812,   617, -1842,
    1553,  1816,   679,  1659, 24947, -1842, -1842, -1842, -1842,  1814,
   -1842, -1842, 26954, -1842, 22027, 26954,  1377,  1819,  1343, -1842,
    1349, -1842,  4003, -1842,  4003, -1842, -1842, -1842,  1820,   129,
     129,  1822,  1826, -1842,  1825, -1842, -1842, -1842, -1842, -1842,
    1351, 26954, -1842, -1842, -1842, -1842,   567,  1041,  2186,   587,
   -1842, -1842, -1842, -1842,   129,   129, -1842, -1842, -1842,   592,
   -1842,  1130,  3599,   500, -1842,  5160, -1842,   129, -1842, -1842,
   -1842, -1842, -1842, -1842, 18548, 18548,  1656, 18324,   147, 25640,
    1912, 18548, -1842, -1842, -1842, -1842, -1842, 26954, -1842, 25717,
    1913,  1808, 20320, 25794, 18548, 11233,  1656,  1205,  1168,  1815,
   26954, -1842,  1835,   338, 18548, -1842, 18548, -1842,  1836, -1842,
   23405,  1817,   852,   804, -1842, -1842,  1837,  1353,  1135, 18548,
    1841, 18548, 18548, 18548, -1842,   744, -1842, -1842,  1832,  1838,
   -1842, -1842,  1553,  1845, -1842, -1842,  1377, -1842, -1842, -1842,
   -1842,  1847, -1842, -1842, -1842,  1363,  1368, -1842, -1842, -1842,
   -1842, -1842, -1842, -1842, -1842, -1842,  1849,  1851,  1853,  2186,
   -1842,   129, -1842, -1842, -1842, -1842, -1842,  1843,  3821, -1842,
    1930,  7505,   124, 13674, -1842, 18422, -1842,    56,  1136, 18548,
    1938,   619,  1842,   424, 18548, 26954,  1205,  1168,  1840, 25876,
     807,  1086,  1844,   450,  1942, -1842, 25953, 26030, 26954,  1656,
    1848, 13843, -1842, -1842, -1842, 23405,  1852,  4055, 22826,  2907,
   -1842,  1861,  1854,   138, -1842, 26954, 24788, -1842, -1842, 26954,
     400, -1842, -1842, -1842, -1842, -1842,  1875, -1842,  1877, -1842,
   -1842, -1842,   950,  1229, -1842, -1842,  1041, -1842, 18548, -1842,
     237, -1842,   324, -1842, -1842, -1842,  1879, 15635, -1842, -1842,
   18548, -1842,    81, -1842, 18548, 26954,  1882, 26107, -1842, -1842,
   26184, 26261, 26954,  5315,  1656, -1842,  1219, 26338, 26415, 18548,
    1869,   462,  1871,   497, -1842, -1842,  1890, 15635,  1852, 26954,
    1888,  2697,  4892, -1842, -1842, -1842,  1884, -1842,  1949,  1892,
     811,  1894, -1842, -1842,  1900,  1146,   312, -1842, -1842, 21309,
   -1842, -1842,  1442, -1842, -1842, 26954, -1842, 26954, -1842, -1842,
    1456, 15802, -1842, -1842, 18548, -1842, -1842,  1656, -1842, -1842,
    1656,  1887,   519,  1889,   631, -1842, -1842,   679, -1842,  1656,
   -1842,  1656, -1842,  1903, 26492, 26569, 26646, -1842,  1456,  1905,
   -1842,   677,  4892,   138,  1906, 26954,  1883,   138,   138, -1842,
   -1842, 18548,  1991,  1911, -1842, -1842, 18422, -1842,  1456, -1842,
   -1842,  1914, 26723, 26800, 26877, -1842, -1842,  1656, -1842,  1656,
   -1842,  1656, -1842,   677, -1842,  1907,   852,  1915, -1842,   815,
   -1842, -1842, 18548, -1842, -1842, 10060,  1920, 18422, -1842, -1842,
    1656, -1842,  1656, -1842,  1656,  1921, -1842,   852,  1922, -1842,
    1896,   852, -1842, -1842, -1842, -1842, 10204, -1842, -1842,  1374,
   26954, -1842,  1152,   852,  2907,  1928,  1898, -1842, -1842,  1170,
   -1842, -1842,  1908,  2907, -1842, -1842
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
     539,   540,   541,   542,   543,   544,   551,   552,   858,   554,
     627,   628,   631,   633,   629,   635,     0,     0,     0,   499,
       0,     0,    17,   598,   604,     9,    10,    11,    12,    13,
      14,    15,    16,   814,   104,     0,     0,    20,     0,     2,
     102,   103,     0,   835,    18,    19,   873,   499,   815,     0,
       0,   438,   736,   440,   451,   856,   439,   473,   474,     0,
       0,     0,     0,   581,   501,   503,   509,   499,   511,   514,
     566,   525,   553,   483,   559,   564,   485,   576,   484,   591,
     595,   601,   580,   607,   619,   858,   624,   625,   608,   677,
     441,   442,     3,   822,   836,   504,     0,     0,   858,   896,
     858,   499,   913,   914,   915,   499,     0,  1100,  1101,     0,
       1,   499,    17,     0,   499,   462,   463,     0,   581,   509,
     493,   494,   495,   825,     0,   630,   632,   634,   636,     0,
     499,   858,   680,   859,   860,   626,   555,    22,    23,    21,
     790,   785,   775,     0,   867,   823,     0,     0,   516,   816,
     820,   821,   817,   818,   819,   499,   867,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   599,   602,   499,   499,
     874,     2,     0,  1102,   581,   903,   921,  1106,  1099,  1097,
    1104,   435,   434,     0,   173,   742,   172,     0,   443,     0,
       0,     0,     0,     0,   449,     0,     0,     0,   433,   990,
     991,     0,     0,   472,   856,   858,   856,   877,   858,   858,
     482,   499,   858,   856,   934,   858,   858,   481,   858,   953,
     858,   931,     0,   574,   575,     0,     0,   499,   499,     2,
     499,   452,   856,   502,   512,   567,     0,   596,     0,   839,
     499,     0,     0,   736,   453,   581,   560,   577,   592,     0,
     839,   499,     0,   515,   561,   568,   569,   486,   578,   488,
     489,   487,   583,   593,   597,     0,   611,     0,   808,   499,
       2,   837,   895,   897,   499,     0,   499,     0,     0,   581,
     499,   511,     2,  1110,   581,  1113,   856,   856,     3,     0,
     581,     0,     0,   465,   858,   851,   853,   852,   854,     2,
     499,   856,     0,   812,     0,     0,   771,   773,   772,   774,
       0,     0,   767,     0,   756,     0,   765,   777,     0,   858,
     678,     2,   499,  1122,   500,   499,   490,   559,   491,   584,
     492,   591,   588,   609,   858,   610,   724,     0,   499,   725,
    1075,  1076,   499,   726,   728,   680,   598,   604,   681,   682,
     683,     0,   680,   861,     0,   788,   776,     0,   872,   871,
     867,   870,     0,   865,   868,    25,     0,    24,     0,     0,
       0,     0,     0,     0,     0,     0,    27,    29,     4,     8,
       5,     6,     7,     0,     0,   499,     2,     0,   105,   106,
     107,   108,    85,    28,    86,    46,    84,   109,     0,     0,
     124,   126,   130,   133,   136,   141,   144,   146,   148,   150,
     152,   154,   157,     0,    30,     0,   605,     2,   109,   499,
     165,   782,   732,   595,   734,   781,     0,   731,   735,     0,
       0,     0,     0,     0,     0,     0,     0,   875,   901,   858,
     911,   919,   923,   929,   598,     2,     0,  1108,   499,  1111,
       2,   102,   499,     3,   723,     0,  1122,     0,   500,   559,
     584,   591,     3,     3,   719,   709,   713,   725,   726,   499,
       2,     2,   904,   922,  1098,     2,     2,    27,     0,     2,
     742,    28,     0,   740,   743,  1120,     0,     0,   749,   738,
     737,   499,     0,   841,     0,     2,   464,   466,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   880,   937,   960,   858,   478,     2,   876,   884,
    1018,   736,   878,   879,     0,   839,   499,   933,   941,   736,
     935,   936,     0,   952,   954,     0,   468,   499,   499,   565,
     500,     0,   581,     0,   499,  1103,  1107,  1105,   450,   582,
     812,     0,   839,   856,     0,   444,   454,   513,     0,   839,
     499,   812,     0,   839,   786,   562,   563,   579,   594,   600,
     603,   598,   604,   622,   623,     0,   787,   695,   729,   500,
       0,   696,   698,   699,     0,   213,   427,   838,     0,   425,
     482,   481,   581,     0,   446,     2,   447,   809,   470,     0,
       0,     0,   499,   856,   499,   812,     0,     0,     0,     0,
     770,   769,   768,   762,   510,     0,   760,   778,   557,     0,
     499,   499,  1077,   500,   496,   497,   498,  1081,  1072,  1073,
    1079,   499,     2,   103,     0,  1037,  1051,  1122,  1033,   858,
     858,  1042,  1049,   717,   499,   589,   727,   500,   585,   586,
     590,     0,   679,  1087,   500,  1092,  1084,   499,  1089,   858,
       0,   858,   680,   680,     0,     0,   499,     0,     0,   863,
     867,   158,     0,    26,   499,    92,     0,   499,   100,   499,
     499,    87,   499,    94,     0,    36,    40,    41,    37,    38,
      39,   499,    90,    91,   499,   499,   499,     2,   105,   106,
       0,     0,   191,     0,     0,   625,     0,  1097,     0,     0,
       0,     0,     0,     0,     0,     0,    59,     0,    65,    66,
     158,     0,     0,   158,     0,    88,    89,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   437,     0,     0,
     499,   174,   175,   176,   177,   178,   179,   180,   181,   182,
     183,   184,   172,     0,   170,   171,   499,  1002,   733,   999,
     858,   858,  1007,   606,   499,   864,   902,   858,   912,   920,
     924,   930,   499,   905,   907,   909,   499,   925,   927,     2,
       0,     2,  1109,  1112,   499,   499,     0,     2,     0,   499,
     103,  1037,   858,  1122,   972,   858,   858,  1122,   858,   987,
     858,   858,     3,   727,   499,     0,   499,     0,  1122,  1122,
     499,   499,   499,   499,     0,     2,   751,     0,  1120,   748,
    1121,     0,   744,     0,     2,   747,   750,     0,     2,     0,
     499,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   858,   889,   893,   932,   858,   946,   950,
     958,   858,   968,   881,   938,   961,     0,     0,  1014,     0,
     476,   842,     0,     0,   477,   843,   469,     0,     0,     0,
       0,   467,     0,     2,     2,   844,     0,   448,     0,   812,
       0,   839,     2,   845,     0,     0,     0,     0,   637,   898,
     499,   916,     0,     0,   499,   428,   426,   102,     3,   499,
       0,   813,     2,     0,   764,   805,   800,   801,     0,   500,
       0,   796,     0,     0,   499,   758,   757,   758,   558,   556,
     682,  1083,   499,  1088,   500,   499,  1074,     0,     0,     0,
       0,  1052,     0,   858,  1123,  1038,  1039,   718,  1035,  1036,
    1050,  1078,  1082,  1080,   587,   622,  1086,  1091,   674,   680,
     680,     0,     0,   690,   689,  1120,     0,   680,   791,   789,
       0,     0,   866,   162,     0,   159,   160,   164,   824,     0,
       0,     0,     0,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   499,   499,     0,   123,   122,     0,
     119,   118,    31,     0,    32,     0,     0,     0,   188,   187,
       0,     3,   158,     0,    55,     0,    56,    63,     0,    62,
       0,    58,     0,    57,    61,     0,     0,    54,   125,   127,
     128,   129,   131,   132,   134,   135,   139,   140,   137,   138,
     142,   143,   145,   147,   149,   151,   153,     0,     0,     0,
       0,    33,     3,   742,   166,     0,     0,     0,     0,  1003,
    1004,  1000,  1001,   784,   783,   499,   906,   908,   910,   499,
     926,   928,     0,     0,   499,   499,  1028,  1027,   499,     0,
       0,     0,     0,     0,   858,  1038,   975,   992,   499,   970,
     978,   715,   973,   974,   716,   499,   985,   995,   988,   989,
       0,     3,  1122,     3,   711,   460,   710,   714,  1114,   720,
     721,   703,     0,   704,   705,     3,     3,     3,   736,     0,
     157,     0,     3,     3,     0,   745,     0,   739,     0,   858,
     499,   858,     3,   471,   858,   890,   894,   858,   947,   951,
     959,   858,   969,   499,   882,   885,   887,   499,   939,   942,
     944,   499,   962,   964,   966,   856,   479,  1015,     3,  1019,
    1020,     3,   847,   955,   571,   570,   573,   572,     2,   813,
     848,   793,     0,     2,   846,     0,   813,   849,   637,   637,
     637,   499,   697,   700,   701,   730,   431,     0,     0,     0,
     499,     0,     0,   352,     0,     0,     0,     0,     0,   193,
       0,   347,   348,     0,     0,   352,     0,   400,   399,     0,
     168,   168,   406,   598,   604,   210,   499,     2,     0,   194,
       0,   221,   195,   196,   499,   215,   197,   198,   199,   200,
       0,   201,   202,   353,     0,   367,   203,   373,   375,   378,
     204,   205,   206,   207,   208,     0,   209,   217,   581,   499,
     219,     0,     0,     0,     3,   826,   813,     0,   803,   780,
     797,     0,   798,     0,   799,     0,   499,   775,   775,  1085,
    1090,     2,   102,   499,     3,   596,     3,   500,  1046,   858,
    1045,  1048,   499,     3,  1034,  1040,   680,  1120,     0,   680,
     686,   680,     0,   691,  1120,     0,   862,     0,   869,     0,
      93,    96,   499,   101,   499,     0,    99,    95,    97,   499,
       0,   113,     0,     0,     0,   117,   121,   120,   192,     0,
       0,     0,   742,   110,   185,     0,     0,     0,    49,    50,
      82,     0,    82,    82,     0,    70,    72,    52,     0,    48,
       0,    51,   156,     0,   436,     0,     0,  1120,  1011,   858,
    1010,  1013,  1005,     0,     0,   899,   917,     3,     3,     0,
     858,   981,   984,   858,     0,   858,   858,   976,   993,     0,
       0,  1115,     0,   722,   499,     0,  1117,   499,   499,     0,
     499,     0,   445,     3,     0,     0,     0,     0,   741,   746,
       3,   840,     3,   857,     0,   499,   883,   886,   888,   499,
     940,   943,   945,   499,   963,   965,   967,     0,     0,     0,
     736,  1026,  1025,     0,     0,     0,     0,     0,   795,   813,
     850,     0,   499,   499,   499,   499,   499,   499,   499,   620,
       0,     0,     0,   651,   581,   638,     0,     0,     0,   429,
     158,     0,     0,   338,   339,   218,   220,   499,     0,     0,
       0,   499,   499,   334,     0,   332,     0,     0,     0,   742,
       0,     0,   499,     0,   379,   499,     0,   169,     0,     0,
     407,     0,     0,     0,   858,   225,     0,   216,     0,   329,
       0,     0,     0,   352,   352,   358,   357,   352,   369,   368,
     352,   352,     0,   581,     0,  1030,  1029,     0,     0,   767,
     802,   804,   779,   759,   763,   761,   499,     0,     0,     0,
       3,   499,  1041,  1043,  1044,     0,     0,   102,     0,     3,
       0,     0,   680,  1120,     0,     0,   669,     0,   685,     0,
     792,     0,     0,   161,  1031,     0,     0,     0,    42,     0,
     114,   116,   115,   112,   111,   742,  1120,   190,   189,     0,
      69,    79,     0,    73,    80,    81,    64,     0,     0,     0,
      60,     0,   155,    34,     0,   499,  1006,  1008,  1009,   900,
     918,     0,     0,   858,   499,   977,   979,   980,   499,   994,
     996,     0,   971,   986,   982,   997,  1116,   712,   461,   707,
     706,   708,  1119,  1118,     0,     3,   855,   752,   753,     0,
       0,   858,     0,     0,     0,   891,   948,   956,   480,     0,
    1021,     0,  1022,  1023,  1017,   830,     0,   832,   620,   620,
     620,   651,   658,   625,     0,   664,   651,     0,   499,   612,
     650,   649,   645,     0,     0,     0,     0,   652,   654,   858,
     666,   666,   666,     0,   646,   662,   432,     0,   342,   343,
     340,   341,   234,     0,     0,   236,   440,   235,   581,   499,
       0,     0,   352,     0,   317,   319,   318,   320,     0,   352,
     193,   274,     0,   267,     0,   193,   335,   333,     0,   327,
    1120,   336,     0,     0,     0,     0,   388,   389,   390,   391,
       0,   381,     0,   382,   344,     0,   345,     0,     0,   372,
       0,   214,   331,   330,     0,     0,   361,   371,     0,   352,
     374,     0,   376,   398,   430,   858,   828,   758,  1093,  1094,
    1095,     0,     0,     3,     3,     0,  1054,     0,   680,   670,
    1120,     0,   688,   691,   742,   692,   673,   794,   163,     0,
    1032,    98,     0,    35,   499,     0,  1120,     0,     0,    83,
       0,    71,     0,    77,     0,    75,    47,   167,     0,   858,
     858,     0,     0,   755,     0,   455,   459,   892,   949,   957,
       0,     0,   834,   616,   618,   614,     0,     0,  1061,     0,
     659,  1066,   661,  1058,   858,   858,   644,   665,   648,     0,
     647,     0,     0,     0,   668,     0,   640,   858,   639,   655,
     667,   656,   657,   663,   352,   352,   237,   581,     0,     0,
     255,   352,   322,   325,   323,   326,   321,     0,   324,     0,
     263,     0,   193,     0,   352,   499,   275,     0,   300,     0,
       0,   328,     0,     0,   352,   351,   352,   392,     0,   383,
     499,     0,     0,     0,   212,   211,   354,     0,     0,   352,
       0,   352,   352,   352,   458,   775,  1096,  1047,     0,     0,
    1053,  1055,  1120,     0,   672,   687,  1120,    53,    45,    43,
      44,     0,    67,   186,    74,     0,     0,  1012,   457,   456,
     983,   998,   754,  1016,  1024,   642,     0,     0,     0,  1062,
    1063,   858,   643,  1059,  1060,   641,   621,     0,     0,   350,
     226,     0,     0,     0,   248,   352,   228,     0,     0,   352,
     257,   272,   283,   277,   352,   193,     0,   287,     0,     0,
       0,   312,   278,   276,   265,   268,     0,     0,   193,   301,
       0,     0,   231,   349,   380,   499,   386,   393,   500,   397,
     346,     0,     0,   408,   359,     0,   158,   370,   363,     0,
     364,   362,   377,   766,  1056,  1057,     0,   676,     0,    68,
      78,    76,   858,  1069,  1071,  1064,     0,   653,   352,   243,
     238,   241,     0,   240,   247,   246,     0,   499,   250,   249,
     352,   259,     0,   256,   352,     0,     0,     0,   264,   269,
       0,     0,   193,     0,   288,   313,   314,     0,     0,   352,
       0,   303,   304,   302,   271,   337,     0,   499,   386,     0,
       0,     0,  1061,   394,   395,   396,     0,   401,     0,     0,
       0,   409,   410,   355,     0,     0,     0,   675,   693,   499,
    1065,  1067,  1068,   660,   227,     0,   245,     0,   244,   230,
     251,   499,   421,   260,   352,   261,   258,   273,   286,   284,
     280,   292,   290,   291,   289,   270,   315,   316,   285,   281,
     282,   279,   266,     0,     0,     0,     0,   233,   251,     0,
     387,     0,  1062,   408,     0,     0,     0,   408,     0,   360,
     356,   352,     0,     0,   239,   242,   352,     3,   252,   422,
     262,     0,     0,     0,     0,   311,   309,   306,   310,   307,
     308,   305,     3,     0,   384,     0,     0,     0,   402,     0,
     411,   365,   352,  1070,   222,     0,     0,   352,   299,   297,
     294,   298,   295,   296,   293,     0,   385,   414,     0,   412,
       0,   414,   366,   224,   223,   229,     0,   232,   415,     0,
       0,   403,     0,     0,     0,     0,     0,   416,   417,     0,
     413,   404,     0,     0,   405,   418
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1842,  2740,  5889, -1842,    -1,   496,   666,  9269,  -162, -1842,
    -345, -1842,   307, -1842, -1046, -1076, -1842,   180,  6482,  1982,
   -1842,   868, -1842,  1352,   443,   784,   786,   656,   782,  1315,
    1317,  1314,  1316,  1320, -1842,  -169,  -153,  -695, -1842,   774,
    8462,   851, -1842,  1664, -1842, -1842, -1279,  7963, -1079,  2536,
   -1842,   336, -1842,   848,   -47, -1842, -1842,   623,    40, -1842,
   -1841, -1642,   246,    13, -1842, -1842,   618,   259, -1842, -1562,
   -1842, -1234, -1842, -1842, -1842, -1842,    60, -1172, -1842, -1842,
   -1261,   380, -1842, -1842, -1842, -1842, -1842,    38, -1217, -1842,
   -1842, -1842, -1842, -1842,    82,   398,   403,   160, -1842, -1842,
   -1842, -1842,  -883, -1842,    19,   -33, -1842,    94, -1842,  -203,
   -1842, -1842, -1842,   853,  -903,  -799,  -155, -1842,   116,    52,
    1376,   843,  -723,  -719, -1842,   -61, -1842, -1842,   146, -1842,
    -161,  2369,   236,  -263,  4554,  6502,  -677,   188,   151,    37,
    2164,  4084, -1842, -1842,  2094, -1842,   277,  4650, -1842,  2029,
   -1842,   364, -1842, -1842,  3089,   420,  5364,  3321,   -63,  1872,
    -118, -1842, -1842, -1842, -1842, -1842,  -276,  5806,  2848, -1842,
    -413,   247, -1842,  -769,   209, -1842,   144,   681, -1842,  -109,
    -207, -1842, -1842, -1842, -1842,  -102,  6503,  -938,   824,   385,
    2070, -1842,  -493,  -101,  -149,  1897,  3080,  -823,  -140,   874,
    -274,  -419,  -275,  -208,  -477,  1290, -1842,  1636,   294,  -957,
    1506, -1842, -1842,   625, -1842, -1286,  -181,   -23,  -525, -1842,
     233, -1842, -1842,  -871,  -914, -1842, -1842, -1842,  2166,  -860,
    -471, -1023,    -5, -1842, -1842, -1842, -1842, -1842, -1842,  -114,
    -870,  -199, -1827,   154,  7230,   -71,  7167,   -68,  1462, -1842,
    2901,   -60,  -228,  -224,  -215,    34,   -74,   -69,   -49,   167,
     -29,   -25,    24,  -213,    -9,  -205,  -192,  -165,   101,  -110,
     -86,   -80,  -796,  -780,  -749,  -741,  -762,  -106,  -729, -1842,
   -1842,  -757,  1361,  1364,  1367,   927, -1842,   523,  7934, -1842,
    -650,  -619,  -600,  -562,  -775, -1842, -1640, -1705, -1700, -1673,
    -664,  -147,  -293, -1842, -1842,   -76,    28,  -103, -1842,  8225,
     447,    25,  -615
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   857,   432,   433,   179,    86,  1258,   434,   407,
     435,  1577,  1578,   436,  1374,  1375,  1376,  1591,   458,   438,
     439,   440,   740,   741,   441,   442,   443,   444,   445,   446,
     447,   448,   449,   450,   451,   460,  1161,  1014,  1015,  1016,
     742,  1508,   803,   227,   805,   454,  1050,  1259,  1260,  1261,
    1262,  1263,  1264,  1265,  2155,  1266,  1267,  1693,  2011,  2012,
    1944,  1945,  1946,  2127,  2128,  1268,  1712,  1713,  2035,  1714,
    1858,  1859,  1269,  1270,  1271,  1272,  1273,  1274,  1887,  1891,
    1531,  1523,  1275,  1276,  1530,  1524,  1277,  1278,  1279,  1280,
    1281,  1282,  1283,  1731,  2050,  1732,  1733,  1976,  1284,  1285,
    1286,  1511,  2060,  2061,  2062,  2179,  2189,  2080,  2081,   315,
     316,   944,   945,  1227,    88,    89,    90,    91,    92,  1696,
     494,   212,    96,    97,    98,    99,   243,   244,   318,   297,
     496,   462,   497,   102,   330,   104,   105,   159,   365,   321,
     109,   110,   111,   175,   112,   969,   366,   160,   115,   267,
     116,   161,   276,   368,   369,   370,   162,   455,   121,   122,
     372,   123,   615,   937,   935,   936,  1669,   373,   374,   126,
     127,  1221,  1475,  1676,  1677,  1819,  1820,  1476,  1664,  1839,
    1678,   128,   702,  1326,   171,  1004,   375,  1005,  1006,  1568,
     977,   621,  1152,  1153,  1154,   622,   376,   505,   506,   624,
     464,   465,   228,   524,   525,   526,   527,   528,   353,  1307,
     354,   967,   965,   653,   355,   395,   356,   357,   466,   130,
     181,   182,   131,   960,   961,   962,   963,     2,  1208,  1209,
     644,  1295,   132,   343,   344,   278,   289,   598,   133,   231,
     134,   333,  1163,   588,   558,   173,   135,   402,   403,   404,
     136,   335,   247,   248,   249,   336,   138,   139,   140,   141,
     142,   143,   144,   252,   337,   254,   255,   256,   338,   258,
     259,   260,   843,   844,   845,   846,   847,   261,   849,   850,
     851,   808,   809,   810,   811,   559,  1201,  1454,   145,  1779,
     677,   678,   679,   680,   681,   682,  1822,  1823,  1824,  1825,
     667,   507,   380,   381,   382,   467,   218,   147,   148,   149,
     384,   871,   683
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      84,   396,   200,    84,   579,   198,   877,   201,   595,   363,
    1308,   217,   745,   541,  1527,   207,  1378,   542,   452,   530,
     992,  1544,  1545,   379,   406,   986,   543,   202,   544,   146,
     377,  1144,   146,  1146,   453,   137,   545,   537,   108,  1513,
     246,  1287,   325,   868,  1385,  1126,  1300,   203,   565,   546,
    1100,   204,   978,    93,   185,    84,    84,  1212,    84,  1120,
     503,  1327,   987,   576,  1041,  1060,   392,  1130,  1066,  1334,
     391,   979,  1216,  1137,  1290,    84,   547,   592,   217,  1127,
     317,    84,   751,  1735,  1512,   695,    84,   146,   603,   698,
    1121,   253,  1296,   137,    84,   309,   108,   477,  1122,    84,
     205,  1601,    84,   541,  2019,   489,    84,   542,   502,   980,
    1123,    93,  1926,  1423,  2013,  -806,   543,  1927,   544,   924,
     155,   215,   232,    58,   471,  2014,   545,  1737,   469,   472,
     932,   548,  2020,   229,   250,   146,   564,   279,  1244,   546,
     274,   290,   164,   572,  1928,    84,   328,   100,    84,   473,
      84,   746,   107,  1870,   785,   549,    84,  2084,   918,   920,
    1500,   550,   229,    84,  1736,   591,   547,   200,   150,   474,
     539,    84,   201,   475,   952,  1598,   602,   635,  1930,   146,
     654,  -807,   552,    84,   655,   137,  1457,    75,   108,   106,
     317,   986,   202,  1461,   586,    84,    84,    84,   786,  1738,
     310,   257,    58,    93,  1749,   100,   230,    84,  -839,  1599,
     107,   557,   203,    84,   709,   668,   204,  1688,   710,   317,
     978,   548,   476,   283,   521,  1941,  1942,   163,  1131,    84,
     317,   215,  1134,   553,    84,    84,    84,  -839,  2021,   979,
      84,    84,   610,  1149,  1150,   549,   685,   106,   512,   234,
    2013,   550,    63,    64,   216,   628,    58,   200,   246,   405,
     639,    84,   201,  2085,   586,   205,    75,   251,  1564,    84,
     280,    84,   552,  1538,   291,   169,   215,   980,   113,   701,
      84,    84,   202,   700,    84,  2019,   910,  1009,  2015,  2005,
     704,    84,   320,  1424,   914,  1593,    20,   100,  1300,  2058,
    1959,  1025,   107,    78,   215,    84,    84,  -990,    84,   253,
    1788,   883,  1943,    84,  -990,   884,  2019,    84,   872,   599,
      75,   184,   597,   553,   885,  2076,   886,  1485,  1425,  1407,
      84,    84,   708,   657,   887,  1325,   113,  2054,  1120,   106,
     170,    84,  1392,   554,  1130,   668,  1926,   888,  1064,    84,
      84,  1927,   630,  1459,    84,  1748,   274,  1367,  1512,  1751,
    1486,  1287,  1357,  1408,   216,   117,   215,   973,  1155,  1121,
    1157,  1021,   879,   657,   889,  2075,   916,  1122,  1928,  1836,
     685,   513,   921,   217,    84,   833,  1837,  1172,  2121,  1399,
     848,  1563,  1889,    84,  1290,  1104,    84,   931,   556,    84,
     561,   274,   320,  1244,   997,  1838,   489,   569,  1521,   216,
    1425,   883,  1930,   186, -1121,   884,  1941,  1942,   113,   631,
     635,   118,  1470,   117,   885,  1003,   886,  1890,   113,   890,
     388,   320,    58,   554,   887,   503,  1656,   216,  2126,  1305,
    1019,  1451,   320,  1022,   674,  1024,   195,   888,  1026,  1301,
     187,   912,   600,   891,  -617,   207,  1294,  1029,  1213,   892,
    1031,  1032,  1033,  1452,  1571,   471,  2126,   320,  1302,    84,
     472,   287,  2005,   311,   889,  1325,   195,  1863,   926,   118,
     634,   636,   224,   502,   196,   930,  2157,  2077,  2078,   934,
     473,   903,  1303,   225,    84,    84,    75,    85,  1471,   978,
     153,   685,  1472,  1971,   512,   117,    84,    84,  1525,   226,
     474,  1304,   503,  1525,   475,   117,   309,    84,   979,   521,
     557,  1791,  1793,  1795,  1478,   604,  1528,  1413,   113,   890,
     208,  1526,  1513,   235,   220,   274,  1526,   668,    84,   317,
     616,   973,   904,  1479,  1553,   685,  1790,  1481,  1482,  1529,
      84,  -671,  1671,   891,  1739,    85,   980,   113,  -671,   892,
     502,   118,   310,   476,   942,   471,   310,   533,   113,   685,
     472,   118,    85,   236,    84,   650,   685,  1512,   210,  1690,
      84,   903,    84,    85,  1694,   512,   221,  1043,  1694,  1715,
     473,  1627,  1672,   113,  1144,  1146,    85,  2027,   662,    85,
    1001,   220,  1715,    85,   165,   651,   652,   166,   167,  1993,
     168,  1683,   768,  1043,   693,   117,  1387,   222,   696,   769,
     770,  1397,  1398,  2038,  1770,  1043,  1089,  1002,  1620,  1478,
    1684,  1861,   904,  1966,  1967,  2104,  1869,   513,    84,   951,
      84,   503,   468,    84,   117,    84,    -3,    85,  1754,   992,
     310,   627,   905,    85,    84,   117,   274,  1300,    84,   477,
    1043,   557,  1165,  1470,  1470,  1470,  1827,  1483,   389,   597,
    2106,   118,   146,   775,   776,   512,   146,  1432,   137,  1310,
     117,   108,  1043,   452,   287,  1828,   503,  1683,   705,   502,
      84,   707,  2132,    85,    85,  1043,    93,   235,  1160,  1141,
     118,   363,  1480,   236,   486,  1143,  1830,  1195,    58,    84,
      85,   118,   237,  1156,  1871,   379,  1147,   587,   513,   777,
     778,    85,  2030,  2031,   597,   238,   610,    58,   662,  2066,
    1837,   536,    85,   538,   502,   848,   118,    85,    85,  1471,
    1471,  1471,   905,  1472,  1472,  1472,  1915,   927,  1916,  1925,
    1931,   320,    84,   787,    84,  1837,    84,   788,    85,  1298,
      84,   744,    58,    84,   956,  -494,    85,    58,   262,  1932,
      58,  -493,    75,  -684,  1935,    14,    15,    16,    17,    18,
    -684,    85,  1043,  1958,  1003,  1687,   195,   587,    84,   388,
     100,    75,   566,   -23,  1043,   107,   557,   927,   282,  1893,
    1895,  2025,   613,    74,  2134,   618,    58,  1036,   752,   670,
     153,   642,   975,   753,    85,   557,  1522,  1215,  1037,  1038,
      14,    15,    16,    17,    18,   671,    75,    85,    85,   672,
    1902,    75,   106,    84,    75,    58,    80,   673,    84,   305,
      84,   685,   310,   745,    95,   195,   822,    95,   307,   674,
     557,  1105,    84,  -991,  1128,   557,   685,   754,   672,  1361,
    -991,    58,   755,    84,    58,   309,  1362,   329,  1864,   521,
      75,    58,    84,  1865,   363,  -827,  2029,   113,   873,   874,
      58,   699,   875,  1422,   958,  1586,  1156,  1543,   379,  2044,
    1135,   522,  1816,  1164,   672,   394,   971,  1829,  1328,    75,
     911,    58,    95,   206,    64,    84,   220,    58,   915,    14,
      15,    16,    17,    18,   609,    64,    63,    64,  1382,    58,
     991,   113,   813,   351,  1780,    75,   814,   925,    75,    58,
     815,   489,  1876,   996,   710,    75,   826,  1865,   933,   670,
     557,    84,    84,   521,    75,  1183,   272,  1109,  1187,   557,
      95,   557,   557,  2095,   397,  1191,   146,    84,  1429,   557,
     658,   305,  1715,  1982,   117,    75,    85,    78,  1983,    58,
    2116,    75,   146,   405,  2170,  2117,   941,  1723,   468,  2171,
     942,   108,   746,    75,    58,  1435,   975,   509,   146,   557,
     478,  1439,    85,    75,    95,   557,    93,   479,   309,  1160,
     477,   341,   557,  1443,   480,    84,   481,   557,   117,    58,
      58,   274,    84,  1551,    85,   510,    85,   672,  1406,   848,
     118,   482,  1720,    58,   468,   468,  -495,   309,  1008,    74,
    1332,   557,   655,    75,  -497,    85,    14,    15,    16,    17,
      18,   566,  1010,   896,   956,   557,   655,    85,    75,   483,
     744,   806,   495,   744,  1663,   557,  1011,    84,   744,  1555,
     710,    84,    80,    81,   118,  1042,   515,   744,  1605,  1043,
     971,    85,   557,    75,    75,  1579,   956,    85,   516,   486,
     529,    14,    15,    16,    17,    18,   744,    75,   531,    84,
     100,   534,   521,  1614,  1618,  1289,    58,   557,   672,    14,
      15,    16,    17,    18,  1541,   771,   772,  2069,  1786,   263,
     264,   557,   265,   232,  2082,  1169,    74,    84,   266,   814,
     535,   566,   555,    84,    84,   557,   396,   396,   272,   577,
     165,   993,   106,   166,   167,    85,   168,    85,   671,   578,
      85,    58,   672,  1831,  2082,  1477,   583,  1204,   590,    80,
     673,  1043,   642,   468,   477,   970,   557,   229,    84,    58,
      75,  1020,   773,   774,   958,   601,   668,   468,   625,  1027,
    1726,  1727,  1728,  1729,  1730,  1648,    74,   646,  2129,   568,
     629,  1574,   645,   363,   866,  1206,   522,   995,   692,  1043,
      14,    15,    16,    17,    18,  1156,   958,   379,  1817,   685,
     642,   660,   557,  1542,   557,    75,  -498,   814,  1309,    80,
      81,  1069,  1070,  1071,  1602,  1852,  1853,  1854,  1855,   509,
     748,   113,  1783,    75,  1560,   703,  1784,  1128,  1043,   477,
    2145,   672,   521,   388,  2149,    84,    84,    84,  1856,  1848,
     146,  1873,  1874,  1043,   521,  1043,   814,  1862,  1202,  1054,
      58,  1056,   711,  1059,   706,   452,   452,   712,   146,   363,
    1067,   468,  1210,   521,   779,   780,  1214,   108,  1630,    84,
    1217,  1635,  1636,   379,  1147,  1875,   834,   748,  1147,  1043,
    1147,   721,    93,   713,    84,  1091,   716,    84,    84,  1936,
      84,  2064,   146,   814,  1987,  2022,    84,  1906,  1043,  1043,
      84,   108,    84,  1034,   748,  2120,   765,   766,   117,  1043,
     719,  2186,    74,   720,    75,  2183,    93,   146,  1852,  1853,
    1854,  1855,   279,   290,   724,   274,   748,   765,   956,  2192,
     486,  1043,  1695,  2193,   671,    85,  1695,    85,   672,  1045,
    1046,  1856,   660,   748,    84,    80,   673,  1364,  1365,  1450,
    -194,   781,  1561,   767,   597,  1852,  1853,  1854,  1855,  1569,
     765,   521,   782,   146,   118,   783,    85,  1043,  1383,    85,
      84,   784,  1852,  1853,  1854,  1855,   100,    94,  1856,   789,
     156,  1289,  1813,  1814,  1815,   363,  1477,  1477,  1477,   812,
     485,  1665,  1477,  1379,  1380,  1856,   816,  1156,   452,   379,
    -165,  -165,    85,    84,  1857,   283,   824,   208,   748,   827,
     100,   468,  1604,  1680,  1775,  1289,  1521,  1522,   106,  -123,
    -123,  -123,  -123,  -123,  -123,   829,  1697,   495,  1596,  1597,
    1697,  1076,  1077,  1078,  1079,    94,  1600,  1597,  1225,  -122,
    -122,  -122,  -122,  -122,  -122,   541,  1603,  1597,   958,   542,
     817,   956,   106,   818,    85,   280,   291,   819,   543,   756,
     544,   757,   758,   759,  1681,  1462,  1463,  1464,   545,  1117,
    1589,   568,    84,  1840,  1840,  1840,    84,    84,   820,   271,
     821,   546,   831,   284,  1637,  1589,   852,    95,  1117,  1649,
      -3,    95,   760,   146,  -496,   761,   762,   854,   521,   856,
     763,   764,  1796,  1365,   495,  1913,  1365,   113,   547,  1337,
    1579,  1914,  1597,  1923,  1043,   146,  1985,  1986,   -18,   146,
     146,   869,   521,   521,   108,  2000,  1597,    94,   108,   108,
    2001,  1597,    84,   146,  1941,  1942,   522,  2183,  2184,   866,
     870,   113,   108,    14,    15,    16,    17,    18,  1040,   991,
     878,  1460,  1594,  1595,    85,  1072,  1073,   893,    85,  1074,
    1075,  1080,  1081,   548,    84,  1484,   881,   599,  1750,  1752,
     597,   958,  1841,  1842,  1682,   894,  1883,   895,   897,   898,
     899,   155,   900,   901,   521,  1506,    85,   549,  1771,    85,
     902,    84,   744,   550,   117,   907,    84,    84,    84,   146,
     908,  1680,  1832,   322,   883,   928,  1680,   943,   884,   929,
    -615,  1787,  -613,   938,    85,   552,   939,   885,   940,   886,
      85,    85,   954,   946,   964,   968,   981,   887,   117,   983,
     674,   999,  1018,   495,  1007,  1068,  1044,  1047,  1699,  1052,
     888,  1093,  1699,  1699,    14,    15,    16,    17,    18,  1356,
     118,  1116,  1681,   287,  1117,    85,  1699,  1681,  1124,  1145,
      84,   596,  1148,  1167,  1171,    84,   553,   889,  1174,  1175,
    1176,  1177,    84,  1178,    84,   106,  1179,  1180,   495,   106,
     106,  1181,    84,  1182,   118,  1196,  1203,   956,  1205,  -810,
    1207,  1218,  1291,   106,   834,  1487,  1306,   495,  1297,   495,
     600,   521,  1219,   495,   495,  1220,   495,  1318,   521,  1319,
    1320,  1321,  1329,  1381,   396,  1336,  1331,  1338,  1339,  1335,
    1981,  1340,   890,   495,  1373,  1341,  1342,   146,  1373,  1343,
    1034,  1346,  1345,   812,   812,   274,  1347,  1348,  1349,   521,
    1350,  1352,  1353,  1354,  1107,  1872,   891,  1110,  1359,  1360,
    1368,  1428,   892,   468,  1369,  1377,  1373,  1384,  1388,   522,
     993,  1389,  1682,  1390,   113,  1391,  1395,  1682,   113,   113,
    1411,    95,   452,   521,  1396,   903,   554,  1400,  2053,  1401,
    1414,   618,   113,  1402,    85,    85,  1403,    95,  2010,  1416,
    1510,    84,   495,    84,  -811,  1903,  1417,  1418,  1420,    85,
    1455,  1488,   341,    95,  1979,  1491,  1492,   958,  1501,  1502,
    1503,  1911,  1505,   568,   -22,   283,  1535,  1689,  1691,  1043,
    1185,  2059,  1514,   541,  1189,  1373,   904,   542,  1193,  1515,
    1536,    84,  1562,  1566,    84,  1585,   543,  1589,   544,  1590,
    1567,  1575,  1572,   521,   521,  1609,   545,  1610,  1613,  1624,
     521,   117,  1625,  1626,  1628,   117,   117,  1632,    85,   546,
     685,  1633,  1597,   521,  1641,  1638,  1645,    85,  1753,   117,
    1646,  1647,  1654,   521,  1655,   521,  1657,  1670,  1680,  1668,
    2110,  1480,  1701,  1716,   597,  1522,   547,  1741,   521,  1744,
     521,   521,   521,   146,  1745,  1717,  1719,  1721,  1351,  1979,
      85,  1734,   108,  1742,  1355,  1743,   452,   118,   452,  1244,
    1755,   118,   118,  1756,  1758,  1363,  1759,  1760,  1766,  1769,
    1776,  1781,  2124,  1768,  2010,   118,  1789,  1996,  1797,  1681,
    1782,  1998,  1785,  1799,  1800,  1803,   905,    84,   477,  1637,
    1805,   548,  1807,  1808,   521,  1809,   452,  1812,   521,   468,
     230,  2059,  1826,   521,  1674,  2059,  2059,   495,   495,  1844,
    1849,  1896,  2147,  1845,  1851,   549,  1880,  1882,  1900,    85,
    1897,   550,  1904,  1907,  1901,  1912,    84,   200,    84,  1917,
     639,  1920,   201,    87,  2168,  1921,   154,  1922,  1949,  1954,
    1955,  1970,   552,  1975,  1994,    85,  1984,  1968,  1989,  1980,
    1995,  1997,   202,  1999,   557,  2178,  2008,   521,  2002,  2178,
    2003,   452,  2004,   495,  2024,  2026,  1699,  2037,  2039,   521,
      94,  2187,  2032,   521,   957,  2056,   812,  2185,   522,    85,
    2045,  2067,    84,  2068,  2049,  2079,  2057,  1373,   521,  1682,
    2088,    87,  2103,   553,  2105,   146,  2107,  2111,  2113,  2115,
      84,    84,   903,   106,   108,    95,  2114,  2118,   197,  2119,
    2131,  2135,  2133,   287,  2143,  2148,  1877,  2152,  2146,    87,
    2153,  2167,  2158,    95,  2169,   146,  2175,  2177,  2181,  2180,
    2191,    85,   242,   521,   108,   270,   215,  2190,    85,    87,
    2194,  1909,  1039,    85,    85,    85,  1082,  1084,   721,  1083,
    1085,  1437,  1509,   904,  1441,  1086,   504,    95,  1445,   146,
    2176,    84,  1517,  1573,  1703,   468,  1977,  2125,   108,  1972,
     521,  2142,   804,  1725,  1965,   521,  2122,   154,  1892,  1878,
    2109,   272,    95,    87,  1879,  2048,   154,  2150,  2182,   332,
     340,  2108,   113,   176,  1534,   300,   512,  2007,   589,  1667,
    2073,   521,   362,   554,   521,  1565,   521,    85,  1905,  1532,
    1166,   966,    85,   876,  1757,   522,     3,  1096,  1699,    85,
    1097,    85,  1012,  1098,  1811,   521,     0,   459,    95,   197,
     197,   765,     0,    84,     0,    14,    15,    16,    17,    18,
     154,   492,    84,     0,     0,     0,   270,     0,  1699,    14,
      15,    16,    17,    18,     0,   106,     0,     0,     0,     0,
       0,  1977,     0,   905,     0,     0,     0,     0,   332,     0,
    1580,  1581,  1582,   242,   242,     0,   193,  1583,  1584,   117,
       0,     0,  1699,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,   332,    58,     0,     0,     0,     0,
       0,   522,    87,     0,     0,     0,     0,   495,  1373,    58,
     495,   495,     0,  1373,  1373,  1373,     0,   270,     0,   106,
      85,     0,   293,     0,     0,     0,     0,   294,     0,     0,
     298,     0,   303,     0,     0,   118,   398,     0,    85,     0,
      85,     0,     0,     0,   113,     0,     0,     0,     0,     0,
     332,     0,     0,     0,   957,     0,   340,    74,    95,    75,
       0,   262,   340,   332,   332,     0,  1607,     0,     0,     0,
      94,    74,   154,    75,   113,     0,     0,  1616,    85,   806,
      95,    85,     0,   557,    95,    95,   957,     0,     0,     0,
      80,    81,     0,  1817,   362,   675,   684,   557,    95,     0,
       0,     0,     0,     0,    80,    81,     0,     0,   113,     0,
     362,     0,   399,     0,   362,     0,     0,     0,     0,     0,
     101,     0,     0,   157,     0,     0,   272,     0,     0,   620,
     152,   117,   177,   178,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,     0,   459,     0,     0,
       0,   117,     0,     0,    95,   152,     0,   239,   240,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   101,     0,
     400,     0,     0,     0,    85,     0,     0,   118,   293,     0,
     522,   459,     0,    74,   807,   117,     0,  2144,     0,     0,
       0,     0,   197,     0,     0,    58,   213,     0,  1373,     0,
    1373,     0,    19,     0,     0,  1673,    77,   118,     0,     0,
     154,     0,  1674,    85,   492,  2055,    80,    81,   841,  2166,
     684,   504,     0,     0,     0,   293,     0,     0,    82,   152,
       0,   154,     0,    65,    66,    67,    68,    69,    70,    71,
      72,   118,    48,    49,    50,    51,    52,    53,    54,    55,
     101,     0,     0,   459,   323,     0,     0,    74,     0,    75,
     101,   242,     0,   213,     0,     0,     0,     0,     0,  2096,
     294,    58,   689,   242,   303,     0,     0,     0,     0,    76,
      77,   272,    95,     0,     0,     0,     0,    85,    85,     0,
      80,    81,     0,     0,     0,     0,     0,   332,   504,   459,
     459,     0,    82,   332,     0,   152,   362,   239,   240,    65,
      66,    67,    68,    69,    70,    71,    72,   487,     0,     0,
     729,     0,     0,     0,     0,     0,     0,     0,   957,  1821,
       0,     0,     0,     0,   152,    75,   177,   178,    65,    66,
      67,    68,    69,    70,    71,    72,    94,     0,    85,     0,
       0,     0,  1886,     0,     0,  1404,    77,     0,     0,     0,
     101,   332,     0,   332,     0,     0,    87,     0,     0,     0,
     154,     0,     0,     0,     0,     0,   913,   581,    82,   585,
      94,     0,   362,   492,     0,   684,     0,     0,     0,   101,
       0,     0,     0,   675,     0,     0,     0,   675,     0,     0,
     101,     0,     0,     0,   271,   284,   362,     0,     0,  1493,
       0,     0,     0,     0,     0,     0,   684,     0,     0,   362,
    2188,   729,     0,   157,     0,   101,     0,   504,   154,  2195,
     272,     0,     0,     0,     0,     0,   459,     0,     0,   459,
       0,   154,   154,     0,   459,     0,     0,     0,    95,   585,
       0,   957,   620,   459,     0,   152,   154,   154,   154,    65,
      66,    67,    68,    69,    70,    71,    72,   345,     0,     0,
       0,     0,   504,     0,     0,   346,   347,   348,   349,     0,
       0,     0,     0,   151,  1821,  1821,     0,     0,     0,     0,
       0,   504,     0,   504,     0,     0,     0,   504,   504,     0,
     504,   293,     0,     0,     0,     0,    77,     0,     0,   865,
      58,     0,   492,     0,     0,     0,   152,   504,   177,   178,
      65,    66,    67,    68,    69,    70,    71,    72,   807,   807,
       0,     0,     0,     0,     0,     0,   459,     0,     0,     0,
       0,     0,     0,     0,   152,     0,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,   362,   492,   398,   209,
       0,   841,     0,   841,     0,     0,     0,     0,   213,     0,
     350,     0,    74,     0,    75,     0,   362,  1223,   362,     0,
       0,   156,   362,   362,   362,   362,   504,     0,   351,   125,
       0,     0,   125,  2063,  2051,    77,  1821,   832,   557,     0,
      95,     0,   362,    94,     0,    80,    81,    94,    94,     0,
       0,     0,     0,     0,     0,     0,  1095,    82,   487,     0,
       0,    94,     0,     0,     0,     0,     0,     0,   332,     0,
      95,     0,  1112,     0,   399,     0,  1113,     0,     0,     0,
       0,     0,     0,     0,  1821,     0,     0,   125,     0,   596,
     154,     0,   152,     0,   177,   178,    65,    66,    67,    68,
      69,    70,    71,    72,    95,     0,   459,     0,     0,  2071,
       0,   362,     0,  1821,     0,   125,     0,    19,     0,     0,
     332,   340,   154,     0,     0,     0,   459,   957,     0,     0,
       0,   209,     0,     0,   362,   125,  1313,     0,     0,     0,
       0,     0,     0,     0,   301,     0,     0,   675,     0,   101,
       0,     0,     0,     0,     0,     0,     0,   199,  1821,  1821,
       0,    52,    53,    54,    55,     0,     0,     0,     0,   125,
       0,     0,     0,   125,     0,     0,     0,     0,     0,   125,
       0,   245,   125,     0,     0,     0,     0,     0,     0,   584,
       0,   504,   504,   101,   152,     0,   154,   492,    65,    66,
      67,    68,    69,    70,    71,    72,   152,     0,   177,   178,
      65,    66,    67,    68,    69,    70,    71,    72,  1061,  1821,
       0,     0,     0,   125,     0,     0,     0,     0,     0,    58,
     626,     0,     0,     0,     0,     0,   125,     0,   334,     0,
       0,     0,   633,     0,     0,     0,     0,   504,     0,     0,
       0,     0,     0,     0,   271,   284,     0,     0,     0,   584,
    1062,   807,     0,   152,     0,   239,   240,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   362,   362,     0,   125,
     841,   661,     0,     0,     0,     0,     0,   841,     0,     0,
       0,    74,     0,    75,     0,     0,   125,     0,   125,     0,
       0,     0,     0,     0,   125,     0,     0,     0,   125,     0,
       0,     0,     0,   241,    77,     0,     0,   334,     0,   125,
       0,     0,   540,   245,    80,    81,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,    82,     0,     0,     0,
       0,     0,   125,   334,   125,     0,   749,     0,   125,     0,
       0,     0,     0,     0,     0,  1393,   152,     0,     0,  1394,
      65,    66,    67,    68,    69,    70,    71,    72,   125,     0,
       0,     0,     0,     0,   154,     0,     0,   790,  1409,     0,
       0,     0,     0,   154,     0,  1410,     0,     0,     0,     0,
       0,     0,   459,     0,     0,     0,     0,     0,     0,   334,
       0,     0,     0,   596,     0,   830,  1404,    77,     0,     0,
     835,     0,   640,   334,     0,     0,     0,     0,   459,     0,
       0,    94,     0,     0,     0,     0,   459,     0,     0,    82,
     861,   862,     0,  1447,     0,   863,   864,  1448,   152,   867,
     922,  1449,    65,    66,    67,    68,    69,    70,    71,    72,
     270,    87,     0,   125,     0,   880,   188,     6,     7,     8,
       9,    10,    11,    12,    13,   332,     0,     0,   154,     0,
       0,     0,     0,     0,     0,   492,     0,   909,   500,     0,
       0,     0,     0,     0,     0,     0,     0,   125,     0,     0,
      58,   504,     0,   101,   504,   504,     0,   154,     0,     0,
       0,     0,   120,     0,     0,   120,   492,     0,  1518,     0,
       0,   154,     0,     0,     0,     0,   125,     0,   292,     0,
       0,     0,     0,     0,   152,     0,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,   152,   125,   177,   178,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,    74,     0,    75,   949,     0,     0,     0,   125,
     120,     0,     0,     0,     0,     0,     0,     0,     0,   623,
       0,     0,     0,    94,   331,    77,   362,   842,     0,   362,
     362,     0,   362,   487,     0,    80,    81,     0,   120,     0,
       0,     0,   982,     0,     0,     0,     0,    82,     0,     0,
       0,     0,     0,    94,   277,   125,   125,     0,   120,     0,
       0,  1519,     0,     0,     0,     0,     0,     0,     0,     0,
     882,     0,     0,     0,   154,   154,   154,   154,   125,   154,
     154,   665,   245,     0,   688,  1675,   340,    94,     0,     0,
       0,     0,   120,     0,     0,     0,   120,   665,     0,   459,
       0,   665,   120,   459,   459,   120,   334,  1035,     0,   277,
       0,     0,   334,     0,   459,     0,     0,   459,     0,     0,
     358,   120,   125,   390,   188,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,  1642,     0,     0,     0,  1643,
       0,     0,     0,  1644,     0,   270,   463,   791,   792,   793,
     794,   795,   796,   797,   798,   799,   800,   801,   492,   120,
     463,     0,   224,     0,     0,   277,     0,     0,     0,     0,
     950,   152,   334,   177,   178,    65,    66,    67,    68,    69,
      70,    71,    72,   154,   125,     0,   675,     0,     0,   802,
       0,     0,   125,     0,     0,   125,     0,   125,   125,  1114,
     125,  1115,   120,     0,     0,     0,     0,   835,     0,   125,
       0,   665,   125,   125,   125,     0,     0,     0,     0,   120,
    1473,   120,     0,     0,     0,     0,     0,     0,     0,   101,
       0,   120,     0,     0,     0,  1159,   277,     0,     0,     0,
       0,     0,   120,     0,  1168,     0,  1495,     0,  1170,     0,
       0,  1762,     0,     0,     0,     0,     0,   614,     0,     0,
     120,     0,     0,   101,     0,   120,     0,   120,     0,     0,
     277,   120,     0,  1675,  1818,   277,   623,     0,  1675,     0,
     459,   277,     0,     0,     0,  1675,     0,  1675,     0,     0,
       0,   120,   125,   661,     0,     0,     0,     0,     0,     0,
       0,     0,    58,   500,     0,  1798,     0,     0,     0,     0,
     340,   154,     0,   120,  1801,   277,   120,   152,  1802,   239,
     240,    65,    66,    67,    68,    69,    70,    71,    72,   120,
       0,     0,     0,   120,     0,     0,   152,     0,   239,   240,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   152,   623,   206,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,    74,     0,    75,   665,     0,     0,
    1119,     0,   842,     0,     0,     0,   463,     0,     0,   665,
     500,   623,   388,     0,     0,     0,  1673,    77,     0,     0,
       0,     0,     0,  1344,     0,     0,   154,    80,    81,  1490,
       0,     0,    77,   665,     0,   865,     0,     0,     0,    82,
     463,  1504,    58,     0,     0,     0,   665,     0,     0,     0,
       0,     0,   125,     0,     0,     0,     0,     0,     0,  1818,
    1818,     0,     0,     0,     0,     0,     0,   334,     0,   120,
       0,     0,   125,   463,  1675,     0,   152,  1675,     0,   277,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   340,
     120,  1473,  1473,  1473,   157,  1661,  1662,  1666,     0,     0,
       0,     0,     0,     0,    74,     0,    75,   459,     0,     0,
       0,     0,   463,     0,     0,     0,   101,     0,     0,   334,
     101,   101,   154,     0,     0,     0,    76,    77,     0,     0,
       0,     0,     0,     0,   101,     0,     0,    80,    81,   500,
       0,     0,   125,     0,     0,     0,   623,   120,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,   463,   463,
       0,     0,   623,   277,     0,   120,   623,     0,     0,     0,
       0,  1818,     0,     0,     0,     0,     0,     0,     0,   623,
    1675,   120,     0,   665,   500,     0,     0,     0,   152,     0,
     239,   240,    65,    66,    67,    68,    69,    70,    71,    72,
     277,     0,     0,     0,     0,     0,     0,     0,   151,     0,
       0,   500,     0,   277,     0,     0,    74,   154,     0,  2052,
     340,     0,     0,   120,     0,   120,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,  1673,    77,
     390,   120,   463,     0,   277,  1674,     0,     0,  1818,    80,
      81,     0,   120,     0,     0,     0,     0,   790,     0,   154,
       0,    82,     0,     0,     0,   120,     0,     0,   277,     0,
       0,     0,   614,     0,     0,   277,     0,   665,   120,  1119,
    1224,     0,     0,     0,     0,  1405,   842,   120,     0,   154,
       0,     0,     0,  2052,  2052,   463,     0,     0,   463,     0,
     120,   120,     0,   463,     0,     0,     0,     0,     0,   665,
       0,  1546,   463,     0,     0,   120,   120,   120,     0,  1746,
    1747,   665,     0,   154,   688,     0,     0,     0,     0,   125,
       0,     0,     0,     0,     0,     0,     0,     0,   125,   152,
       0,   239,   240,    65,    66,    67,    68,    69,    70,    71,
      72,   152,     0,     0,  2052,    65,    66,    67,    68,    69,
      70,    71,    72,  1057,   125,     0,     0,    74,     0,     0,
     152,   463,   125,     0,    65,    66,    67,    68,    69,    70,
      71,    72,  1370,     0,   500,     0,  1371,   120,  1372,   839,
      77,     0,     0,   672,     0,   463,     0,   125,     0,  2123,
      80,   840,     0,   120,     0,  1058,   194,   120,     0,     0,
       0,     0,    82,   674,   125,   120,   463,     0,     0,     0,
     120,    77,   152,     0,   239,   240,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   120,     0,   120,     0,     0,
       0,   120,   120,   120,   120,   623,     0,   275,     0,   623,
      74,     0,     0,     0,     0,     0,     0,   125,   623,   296,
     299,   120,     0,     0,   334,     0,     0,     0,   623,     0,
       0,     0,  2051,    77,     0,   623,   557,     0,     0,     0,
       0,     0,     0,    80,    81,     0,     0,     0,     0,     0,
    1200,     0,     0,     0,   101,    82,     0,     0,  1850,     0,
       0,     0,   275,     0,     0,  1860,     0,     0,   152,   120,
     177,   178,    65,    66,    67,    68,    69,    70,    71,    72,
       0,   120,     0,   623,     0,   463,     0,   623,     0,     0,
     120,   623,     0,     0,     0,     0,  1885,     0,     0,     0,
     277,   120,     0,     0,     0,   463,     0,     0,     0,     0,
       0,     0,     0,   120,     0,  1315,   463,     0,   275,     0,
       0,   665,     0,     0,     0,  1621,     0,     0,     0,  1497,
     125,   125,   125,   125,   125,   125,   125,     0,     0,     0,
       0,     0,   152,  1330,   609,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   125,     0,     0,     0,   125,
     125,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     125,     0,     0,   125,     0,   120,   463,   152,     0,   177,
     178,    65,    66,    67,    68,    69,    70,    71,    72,   275,
       0,     0,     0,     0,  1679,     0,  1090,     0,     0,     0,
    1939,  1940,     0,     0,     0,     0,   101,  1950,   152,     0,
     386,   387,    65,    66,    67,    68,    69,    70,    71,    72,
    1964,     0,   665,   275,     0,     0,     0,     0,   275,     0,
    1973,     0,  1974,     0,   275,   648,   101,     0,     0,     0,
       0,     0,     0,     0,   665,  1988,   120,  1990,  1991,  1992,
     120,     0,     0,   500,     0,   120,   120,     0,     0,   120,
       0,    78,     0,     0,     0,     0,     0,     0,   275,   120,
     101,     0,     0,   388,     0,     0,   120,     0,     0,   152,
       0,   239,   240,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2018,     0,     0,     0,  2023,     0,    74,     0,     0,
    2028,   120,    14,    15,    16,    17,    18,     0,     0,     0,
     730,     0,     0,     0,   120,     0,     0,     0,   120,   241,
      77,     0,   120,     0,     0,   623,   125,     0,     0,   623,
      80,    81,     0,   623,     0,     0,     0,     0,     0,     0,
       0,     0,    82,   120,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,  2074,     0,     0,   125,     0,     0,
       0,   463,    58,     0,     0,   103,  2083,     0,   158,     0,
    2086,     0,  1679,     0,     0,     0,     0,  1679,     0,     0,
       0,     0,     0,     0,  1833,  2102,  1679,   463,     0,     0,
       0,     0,   275,     0,     0,   463,   152,     0,   239,   240,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   730,     0,     0,     0,     0,     0,     0,     0,   277,
     120,     0,     0,   103,    74,     0,    75,     0,     0,     0,
    2130,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,   623,   125,     0,   463,   500,   839,    77,  1315,     0,
     672,   214,     0,     0,     0,     0,     0,    80,   840,     0,
       0,   114,     0,     0,     0,     0,   120,  2151,     0,    82,
     665,   285,  2154,   120,     0,   463,   275,     0,     0,   152,
     120,   239,   240,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,  2017,     0,   623,     0,     0,  2172,   275,
       0,  2174,     0,  2154,   623,   319,     0,    74,   623,   324,
       0,     0,     0,   275,     0,   103,     0,     0,     0,   114,
       0,  2047,  2174,   125,     0,     0,   275,     0,     0,   331,
      77,     0,     0,     0,   364,     0,     0,     0,     0,     0,
      80,    81,     0,  1937,     0,   120,  1679,     0,   120,   120,
       0,   120,    82,     0,     0,     0,     0,   275,     0,     0,
       0,   470,     0,     0,     0,     0,   120,   286,     0,     0,
     120,     0,   324,   498,   120,     0,     0,     0,     0,     0,
       0,   275,     0,     0,     0,     0,  1653,     0,   275,     0,
       0,     0,     0,   120,   120,   120,   120,   120,   120,   120,
       0,   114,     0,     0,     0,   277,   551,     0,     0,     0,
       0,   114,     0,     0,     0,   319,     0,     0,   463,     0,
       0,     0,   463,   463,     0,     0,   575,     0,     0,     0,
     367,   580,   582,   463,   214,     0,   463,     0,     0,     0,
       0,     0,     0,     0,   319,     0,     0,     0,     0,  1679,
       0,     0,     0,     0,     0,   319,     0,     0,   605,     0,
       0,     0,   607,     0,   277,     0,     0,   608,     0,   499,
       0,     0,     0,   619,     0,   125,     0,   463,   582,     0,
     319,     0,   120,     0,   632,     0,     0,     0,   334,     0,
       0,     0,     0,     0,     0,  1772,   641,     0,     0,     0,
       0,     0,   120,     0,     0,   125,     0,     0,     0,     0,
       0,   114,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,   663,     0,     0,   687,
       0,     0,     0,     0,     0,     0,   120,     0,     0,   125,
     114,     0,   694,     0,     0,   120,   694,     0,     0,   120,
       0,   114,     0,     0,   606,     0,     0,     0,     0,     0,
       0,     0,   640,   334,     0,     0,     0,     0,     0,   367,
       0,     0,     0,     0,     0,    58,   114,     0,     0,   665,
     286,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,   463,
       0,     0,     0,  1199,     0,     0,     0,     0,     0,   152,
       0,   239,   240,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   664,   334,     0,   286,     0,     0,     0,   277,
     120,     0,     0,     0,     0,     0,     0,    74,   664,    75,
       0,     0,   664,     0,     0,     0,     0,    58,     0,     0,
       0,     0,   324,   275,     0,     0,   663,     0,     0,  2051,
      77,     0,     0,   557,     0,     0,     0,     0,   275,     0,
      80,    81,     0,   324,   665,     0,     0,     0,     0,     0,
       0,   152,    82,   239,   240,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,    74,
       0,    75,     0,     0,   152,   120,   611,   612,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
     619,   241,    77,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,    81,     0,     0,   750,     0,   498,    78,
     426,     0,   664,     0,    82,     0,     0,     0,     0,   623,
       0,     0,     0,     0,   319,     0,     0,    78,     0,     0,
      14,    15,    16,    17,    18,     0,     0,     0,   277,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
       0,     0,     0,   463,     0,     0,   463,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   619,     0,   103,     0,
       0,   120,   959,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   694,   974,   367,     0,     0,     0,
      58,     0,     0,     0,     0,   619,     0,     0,     0,   985,
       0,     0,     0,    58,   499,     0,     0,     0,   663,     0,
       0,     0,     0,   994,     0,     0,     0,     0,     0,     0,
     114,   694,     0,     0,   152,     0,   239,   240,    65,    66,
      67,    68,    69,    70,    71,    72,     0,   152,     0,   239,
     240,    65,    66,    67,    68,    69,    70,    71,    72,     0,
    2036,   277,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,   367,     0,   114,    74,   120,    75,   664,   277,
       0,     0,     0,     0,   331,    77,     0,     0,     0,     0,
     664,   499,     0,     0,     0,    80,    81,  1673,    77,     0,
       0,   367,     0,     0,     0,     0,     0,    82,    80,    81,
       0,     0,     0,     0,   664,     0,     0,     0,   120,     0,
      82,     0,     0,     0,   498,     0,     0,   664,     0,     0,
       0,     0,     0,     0,  2097,     0,     0,     0,     0,     0,
     619,  1099,     0,     0,     0,   119,     0,     0,   120,     0,
       0,     0,   275,     0,     0,     0,   619,     0,     0,     0,
     619,     0,     0,     0,     0,     0,     0,     0,   694,   974,
     120,     0,     0,   619,     0,  1125,     0,     0,     0,     0,
       0,   275,   120,     0,     0,     0,     0,     0,   498,     0,
     498,     0,     0,     0,   498,   498,   364,   498,     0,     0,
       0,     0,   152,   119,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   498,     0,     0,   152,     0,     0,
     499,    65,    66,    67,    68,    69,    70,    71,    72,  1370,
       0,     0,     0,  1371,     0,  1372,   367,   152,     0,   177,
     178,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,   288,   367,     0,     0,    78,   367,     0,     0,     0,
       0,     0,   959,     0,   664,   499,     0,     0,    77,   367,
       0,  1592,     0,     0,   619,     0,     0,     0,  1288,     0,
       0,     0,     0,   498,   367,   119,   367,   510,     0,     0,
     367,   367,   499,   367,   959,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   694,     0,     0,  1317,
     367,     0,     0,     0,   371,     0,  1323,     0,     0,  1652,
       0,   152,     0,     0,     0,    65,    66,    67,    68,    69,
      70,    71,    72,  1370,     0,     0,     0,  1371,   275,  1372,
       0,   152,     0,   177,   178,    65,    66,    67,    68,    69,
      70,    71,    72,   501,     0,   152,     0,     0,   664,    65,
      66,    67,    68,    69,    70,    71,    72,  1370,   324,   364,
     367,  1371,    77,  1372,   114,  1792,     0,     0,   152,   367,
     177,   178,    65,    66,    67,    68,    69,    70,    71,    72,
     664,   515,     0,     0,     0,   119,     0,   275,     0,     0,
       0,     0,   664,   232,     0,   286,    77,     0,   152,  1794,
     177,   178,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,   619,
       0,     0,     0,   619,     0,     0,     0,     0,   498,   498,
       0,     0,   619,   371,     0,     0,     0,     0,     0,     0,
     119,     0,   619,     0,   288,   499,     0,     0,     0,   619,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,   498,     0,   666,     0,    46,   288,
      47,     0,     0,     0,     0,     0,     0,   619,     0,     0,
       0,   619,   666,     0,     0,   619,   666,     0,     0,     0,
      58,     0,     0,     0,     0,   367,     0,     0,     0,   367,
       0,     0,     0,     0,   367,   367,   959,     0,   367,     0,
       0,     0,     0,     0,     0,  1474,     0,     0,   367,     0,
       0,     0,   275,     0,  1288,   367,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
     124,     0,     0,     0,    75,     0,     0,     0,  1288,     0,
     367,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   367,     0,     0,     0,   367,     0,     0,
       0,   367,     0,  1533,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   666,     0,     0,     0,
       0,     0,   664,     0,     0,   124,     0,   663,     0,     0,
       0,     0,     0,     0,     0,     0,   580,     0,     0,     0,
     114,     0,     0,     0,     0,     0,     0,     0,     0,   959,
       0,     0,     0,   124,     0,     0,   619,     0,   364,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   124,   114,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     371,   275,     0,     0,     0,     0,     0,     0,     0,   286,
       0,     0,     0,     0,     0,     0,     0,   124,   501,     0,
       0,   124,     0,     0,     0,     0,     0,   124,     0,     0,
     124,     0,     0,   664,   119,     0,     0,     0,   498,     0,
       0,   498,   498,     0,   364,     0,     0,     0,     0,     0,
     223,     0,     0,     0,     0,   664,     0,     0,     0,   619,
       0,     0,   367,   619,   499,     0,     0,   619,     0,     0,
       0,   124,     0,     0,     0,     0,   371,     0,   119,     0,
       0,     0,   666,     0,   124,     0,  1474,  1474,  1474,   158,
     582,   308,     0,     0,   666,   501,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   371,     0,     0,     0,     0,
       0,  1698,     0,     0,     0,  1698,  1698,     0,   666,     0,
       0,     0,     0,     0,     0,     0,     0,   124,     0,  1698,
       0,   666,   275,     0,   367,     0,     0,   367,   367,     0,
     367,     0,     0,     0,   124,     0,   124,     0,     0,     0,
       0,     0,   124,     0,     0,   367,   124,     0,     0,   367,
       0,     0,     0,   367,     0,     0,     0,   124,     0,     0,
     364,     0,     0,     0,     0,   619,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,   124,     0,     0,   959,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   114,     0,     0,
       0,   114,   114,     0,     0,     0,   124,     0,     0,     0,
       0,     0,     0,     0,   501,   114,     0,     0,     0,   619,
       0,     0,     0,     0,     0,     0,     0,     0,   619,     0,
     371,     0,   619,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   371,     0,     0,     0,
     371,     0,     0,     0,     0,     0,   499,     0,   666,   501,
       0,   367,     0,   371,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   637,   371,     0,
     371,   664,     0,     0,   371,   371,   501,   371,     0,  1835,
       0,   124,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   371,     0,     0,     0,     0,     0,
       0,     0,     0,  1847,     0,   367,     0,     0,     0,     0,
       0,     0,     0,     0,   367,   124,     0,     0,   367,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   666,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   371,     0,     0,     0,   119,     0,
       0,     0,     0,   371,     0,   124,     0,     0,     0,     0,
       0,     0,     0,     0,   666,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   666,   124,     0,   288,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   286,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1929,     0,     0,     0,     0,     0,     0,     0,
     836,     0,   838,   124,   124,     0,     0,     0,     0,     0,
       0,   855,     0,     0,     0,     0,     0,     0,     0,   501,
       0,     0,     0,     0,     0,     0,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1960,     0,     0,  1698,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1978,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   371,
       0,     0,     0,   371,     0,     0,     0,     0,   371,   371,
       0,     0,   371,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   371,     0,     0,     0,     0,     0,     0,   371,
       0,     0,     0,     0,   129,     0,     0,   129,     0,     0,
       0,     0,   124,     0,     0,   114,     0,     0,     0,     0,
     124,     0,     0,   124,     0,   124,   124,     0,   124,  1978,
     664,     0,     0,     0,   371,     0,     0,   124,     0,     0,
     124,   124,   124,     0,     0,     0,     0,   371,     0,     0,
       0,   371,     0,     0,     0,   371,     0,     0,     0,     0,
       0,     0,   129,     0,   192,     0,     0,     0,     0,     0,
       0,  1698,     0,     0,     0,     0,   666,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     129,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,  1698,     0,     0,     0,   273,  2112,     0,     0,     0,
     129,     0,     0,     0,     0,     0,     0,   295,     0,   302,
     124,   304,     0,   619,     0,   664,     0,     0,   119,     0,
       0,     0,     0,     0,     0,  1698,     0,     0,     0,     0,
       0,     0,     0,     0,   129,     0,     0,     0,   129,     0,
       0,     0,     0,   288,   129,     0,     0,   129,     0,     0,
     273,     0,     0,   302,   304,     0,     0,   114,     0,   437,
       0,     0,     0,     0,     0,     0,     0,   666,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   114,   129,   666,
       0,     0,     0,     0,     0,     0,   371,     0,   501,     0,
       0,   129,     0,     0,     0,     0,   273,     0,     0,   367,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   114,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1140,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,   129,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,   129,     0,   129,     0,     0,     0,     0,   371,   129,
       0,   371,   371,   129,   371,     0,     0,   273,     0,   302,
     304,     0,     0,     0,   129,     0,     0,     0,     0,   371,
       0,     0,     0,   371,     0,     0,     0,   371,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   129,     0,   129,
       0,   273,     0,   129,     0,     0,   273,     0,     0,     0,
       0,     0,   273,     0,     0,     0,  1292,  1293,     0,     0,
     124,     0,     0,   129,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,   119,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   273,     0,     0,   119,
       0,   690,     0,   304,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     715,     0,   718,     0,     0,   437,   723,     0,     0,     0,
       0,     0,     0,     0,     0,   732,   733,     0,     0,     0,
     501,     0,     0,     0,     0,   371,     0,     0,   728,     0,
     437,   437,     0,     0,     0,     0,     0,     0,   129,     0,
       0,     0,     0,     0,     0,   666,     0,     0,     0,     0,
    1366,   437,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   129,     0,     0,     0,     0,     0,     0,   371,
       0,     0,   273,     0,   437,     0,     0,     0,   371,     0,
       0,  1386,   371,     0,     0,     0,     0,     0,     0,     0,
       0,   129,     0,     0,     0,     0,     0,     0,     0,     0,
     273,     0,   690,   304,     0,     0,     0,     0,     0,     0,
       0,     0,   129,     0,     0,     0,     0,     0,     0,   728,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
    1412,     0,  1415,     0,   129,     0,   124,     0,     0,     0,
       0,     0,     0,     0,  1419,     0,  1421,     0,     0,     0,
       0,  1426,  1427,   273,     0,     0,     0,     0,     0,     0,
       0,  1434,   124,   288,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,   273,     0,     0,
     129,   129,   273,     0,   273,     0,     0,  1453,     0,     0,
    1456,     0,     0,     0,     0,   124,     0,     0,     0,     0,
       0,     0,     0,   129,     0,     0,     0,   273,     0,   273,
     273,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,   273,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   273,     0,     0,     0,     0,     0,
       0,     0,     0,   273,     0,     0,     0,   129,     0,     0,
       0,     0,     0,  1516,     0,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   273,     0,   690,   304,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1537,     0,     0,     0,     0,     0,   273,
     690,     0,     0,     0,     0,     0,   273,     0,     0,     0,
       0,  1547,     0,  1548,     0,  1549,     0,     0,     0,   129,
       0,     0,  1558,     0,     0,   174,     0,   129,     0,     0,
     129,     0,   129,   129,     0,   129,  1961,     0,     0,   119,
       0,     0,     0,     0,   129,     0,     0,   129,   129,   129,
       0,     0,     0,   174,   666,     0,     0,     0,     0,   437,
     437,   437,   437,   437,   437,   437,   437,   437,   437,   437,
     437,   437,   437,   437,   437,   437,   437,   437,   124,   124,
     124,   124,   124,   124,   124,     0,     0,     0,   172,     0,
       0,     0,     0,     0,     0,     0,  1611,  1612,     0,     0,
       0,     0,   174,   124,     0,     0,     0,   124,   124,     0,
       0,     0,     0,     0,     0,   174,     0,   174,   124,     0,
       0,   124,  1634,     0,     0,     0,     0,   129,     0,  1639,
       0,  1640,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   437,     0,     0,     0,   174,   666,
     393,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   306,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   393,     0,     0,   312,     0,
     313,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,     0,     0,     0,     0,   174,     0,
       0,   119,   174,     0,     0,   174,   174,     0,     0,   174,
       0,     0,   174,   174,     0,   174,     0,   174,     0,     0,
       0,     0,     0,   371,     0,     0,     0,     0,     0,  1761,
       0,     0,     0,     0,     0,   119,  1765,   129,  1767,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   273,     0,     0,     0,     0,     0,   129,     0,     0,
       0,     0,     0,     0,   124,     0,   273,     0,   562,   563,
       0,     0,   567,     0,     0,   570,   571,   273,   573,     0,
     574,     0,     0,     0,     0,     0,   273,     0,   174,     0,
       0,   174,     0,     0,     0,   124,  2009,     0,     0,     0,
       0,     0,   437,     0,     0,     0,     0,     0,   437,     0,
       0,     0,     0,     0,  1804,     0,   174,     0,     0,   437,
       0,     0,     0,     0,     0,     0,     0,   129,     0,     0,
       0,   174,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   408,     0,     0,   409,     0,   410,
     411,     0,   412,     0,     0,     0,     0,     0,     0,   437,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   659,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   273,     0,     0,   691,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,   273,   420,   421,
     422,     0,   423,   424,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,   174,     0,     0,     0,
       0,     0,  1898,  1899,     0,     0,     0,     0,     0,     0,
       0,     0,   425,     0,     0,    78,   426,     0,     0,     0,
       0,   124,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   393,     0,     0,   823,
       0,     0,   437,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,     0,   129,     0,     0,     0,     0,     0,
       0,     0,     0,   129,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   129,
       0,     0,     0,     0,     0,     0,     0,   129,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   906,     0,     0,     0,     0,
     273,     0,   129,     0,     0,   437,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   393,     0,     0,   129,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   273,
       0,     0,     0,   124,     0,   273,     0,     0,     0,     0,
       0,     0,     0,     0,   437,   437,   437,     0,     0,     0,
       0,   437,   437,     0,     0,     0,   174,   174,     0,     0,
       0,     0,   129,   124,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   437,   174,     0,   174,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   437,   437,     0,   988,
     989,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   998,
       0,  1000,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   129,   129,   129,   129,   129,
     129,   129,     0,     0,     0,     0,   273,   174,   174,     0,
       0,     0,     0,     0,   174,     0,     0,     0,     0,     0,
     129,     0,     0,     0,   129,   129,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   129,     0,     0,   129,   174,
       0,     0,   174,   174,     0,   174,  2156,   174,   174,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2165,     0,     0,     0,   273,     0,     0,     0,     0,
    1101,  1102,     0,     0,     0,     0,     0,  1106,     0,   437,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,     0,     0,     0,   174,     0,     0,     0,   174,     0,
       0,     0,  1129,     0,     0,  1132,  1133,     0,  1136,     0,
    1138,  1139,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   378,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1184,     0,     0,     0,  1188,     0,     0,
       0,  1192,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   488,   378,     0,     0,     0,     0,     0,     0,
     174,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   129,     0,     0,     0,     0,     0,     0,     0,   560,
       0,     0,     0,     0,     0,     0,   560,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     273,     0,   129,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1324,   188,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,   437,     0,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,   263,   264,     0,
     265,    46,     0,    47,     0,     0,   266,     0,   560,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   129,     0,     0,
       0,   174,     0,     0,     0,     0,   378,   676,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   219,     0,     0,     0,   697,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   281,     0,
       0,     0,     0,     0,     0,     0,   174,   273,   174,     0,
       0,   174,     0,     0,   174,     0,     0,     0,   174,   273,
       0,     0,     0,     0,  1324,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   129,  -475,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   219,
       0,     0,     0,   342,     0,     0,     0,     0,   743,     0,
       0,     0,  -475,     0,     0,   383,   560,     0,     0,  1431,
       0,  1433,     0,     0,  1436,     0,     0,  1440,     0,     0,
       0,  1444,     0,   560,   825,     0,   560,   828,     0,     0,
     219,     0,     0,   437,     0,     0,   378,     0,     0,     0,
     676,   273,     0,     0,   508,     0,     0,     0,     0,   514,
       0,     0,     0,   488,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   273,   304,     0,     0,     0,   437,     0,     0,
       0,     0,     0,     0,   560,     0,     0,     0,   560,     0,
     273,     0,     0,     0,     0,     0,   174,     0,     0,     0,
       0,     0,     0,     0,     0,   219,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     281,     0,     0,     0,     0,     0,     0,     0,   378,     0,
     129,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   437,     0,     0,     0,     0,     0,     0,     0,     0,
     917,   919,     0,     0,     0,     0,     0,     0,     0,  1552,
     129,     0,     0,     0,     0,   514,   174,   437,     0,   437,
       0,     0,     0,     0,     0,   219,     0,   174,     0,     0,
     174,     0,   174,   174,   560,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   129,     0,     0,   669,     0,   686,
       0,     0,     0,     0,   972,   378,     0,   437,     0,     0,
       0,     0,     0,     0,     0,   676,     0,     0,     0,   676,
       0,     0,     0,     0,   273,     0,   990,     0,   378,  1606,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1615,     0,     0,  1619,     0,  1622,  1623,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     747,     0,     0,     0,     0,     0,     0,   461,     0,     0,
       0,     0,   437,     0,     0,     0,     0,     0,     0,     0,
       0,   493,     0,     0,     0,     0,     0,   743,     0,     0,
     743,   174,     0,     0,   219,   743,     0,   523,     0,   523,
       0,     0,     0,     0,   743,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   743,     0,     0,     0,   669,     0,     0,
       0,     0,     0,   853,   378,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     560,   560,     0,     0,  1740,     0,     0,     0,     0,  1088,
       0,   560,  1108,     0,   560,  1111,   219,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   972,   378,
       0,     0,     0,   676,     0,   676,   676,     0,     0,     0,
     174,     0,   676,     0,     0,     0,     0,     0,   378,     0,
     378,   638,     0,     0,   378,   378,   378,   378,     0,     0,
       0,     0,   219,   219,     0,     0,     0,     0,   174,   508,
       0,     0,     0,     0,   378,     0,   560,     0,     0,     0,
     560,     0,     0,     0,     0,     0,     0,   560,  1186,     0,
       0,   560,  1190,     0,     0,   560,  1194,     0,     0,     0,
     174,     0,  1197,  1619,     0,     0,   174,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1806,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   378,   560,     0,   508,     0,   976,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   669,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   676,
       0,     0,   174,     0,     0,     0,     0,     0,     0,     0,
       0,   219,     0,     0,     0,     0,     0,     0,     0,   219,
       0,     0,   747,     0,   747,   219,     0,   219,     0,     0,
       0,     0,     0,     0,     0,     0,   747,     0,     0,   747,
     747,   747,     0,     0,     0,     0,   174,   174,   488,   378,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   523,     0,     0,  1894,     0,     0,   523,     0,
       0,   174,   174,   461,     0,     0,     0,     0,     0,   393,
       0,     0,     0,     0,   174,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   508,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1918,
    1919,     0,     0,   560,     0,     0,     0,     0,     0,   219,
       0,     0,     0,     0,     0,     0,     0,     0,   378,   378,
       0,     0,   676,   676,  1933,  1934,     0,     0,     0,   676,
     508,     0,     0,     0,     0,     0,     0,  1938,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   508,
       0,   508,     0,     0,     0,   508,   508,   383,   508,     0,
       0,     0,     0,     0,     0,   948,     0,     0,   174,     0,
       0,     0,     0,     0,   378,   508,     0,     0,   560,  1438,
       0,   560,  1442,     0,     0,   560,  1446,     0,     0,     0,
       0,     0,     0,   493,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   984,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2006,     0,     0,     0,     0,     0,     0,     0,   174,
       0,     0,     0,  1017,   508,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   342,     0,  1028,     0,     0,   219,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   853,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1049,  1051,     0,     0,  1053,     0,  1055,     0,   174,   743,
       0,     0,  1017,     0,  1065,  1017,     0,     0,     0,     0,
       0,     0,  2070,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   378,     0,     0,
       0,     0,  1092,   676,  1554,     0,     0,     0,     0,     0,
     383,     0,     0,     0,     0,  1094,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1103,     0,   378,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   493,     0,     0,     0,     0,  1092,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   560,  1608,   180,   183,     0,     0,  1162,
       0,     0,   523,     0,   560,  1617,     0,   676,     0,   508,
     508,     0,     0,  1173,     0,     0,     0,     0,   378,     0,
       0,   378,   378,     0,   378,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   233,     0,     0,     0,     0,     0,
       0,  1198,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   508,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   326,     0,     0,   327,     0,
       0,     0,     0,     0,     0,     0,   461,     0,     0,     0,
       0,     0,     0,   352,     0,     0,  1314,  1316,     0,     0,
       0,     0,     0,     0,   493,     0,   747,     0,     0,     0,
    1692,  1700,     0,   401,  1692,  1711,     0,     0,     0,     0,
    1718,     0,     0,     0,  1722,   401,  1724,     0,  1711,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     378,   747,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1092,     0,   532,
       0,     0,     0,     0,     0,  1358,     0,     0,   676,     0,
       0,     0,     0,   281,  1017,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   219,     0,     0,     0,     0,     0,     0,   669,     0,
       0,   233,     0,     0,     0,     0,     0,     0,     0,     0,
     593,   594,     0,     0,     0,   523,     0,     0,     0,     0,
       0,   180,     0,     0,     0,     0,     0,     0,     0,   383,
       0,     0,     0,     0,   747,     0,   180,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   560,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   643,  1810,   560,     0,     0,     0,     0,     0,   647,
     649,     0,     0,     0,   656,     0,     0,     0,   523,     0,
    1430,     0,     0,     0,     0,     0,     0,     0,     0,   508,
       0,     0,   508,   508,     0,   383,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1846,     0,     0,
       0,     0,     0,   352,     0,     0,   352,     0,     0,   401,
       0,     0,     0,     0,     0,     0,     0,  1866,  1868,     0,
       0,     0,     0,     0,     0,     0,     0,   747,   747,   747,
       0,     0,   747,   747,     0,     0,     0,     0,     0,   514,
       0,     0,     0,     0,     0,     0,     0,     0,  1888,     0,
       0,     0,  1507,  1507,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   219,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   560,   560,     0,     0,     0,     0,     0,   281,     0,
       0,     0,     0,     0,   233,     0,     0,   560,     0,     0,
       0,   383,     0,     0,     0,     0,   858,   859,     0,  1550,
       0,     0,     0,     0,     0,  1559,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1017,     0,     0,     0,     0,   493,     0,     0,     0,
       0,     0,  1948,     0,     0,     0,     0,     0,     0,     0,
    1951,     0,  1953,     0,   523,  1957,  1963,  1588,  1711,     0,
       0,     0,     0,  1969,     0,     0,     0,     0,     0,     0,
       0,     0,  1049,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   560,     0,     0,     0,     0,     0,     0,
       0,   560,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   219,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   560,     0,     0,     0,   953,  1650,  1651,     0,     0,
       0,     0,  2034,   281,   352,     0,     0,     0,     0,  2041,
    2043,     0,     0,     0,     0,     0,   560,  2072,     0,     0,
     560,     0,  1017,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2065,     0,     0,     0,     0,     0,     0,     0,
       0,   523,     0,     0,   461,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   401,
       0,     0,     0,     0,     0,   560,   560,     0,  2087,     0,
    2090,     0,     0,  2092,  2094,     0,     0,     0,     0,     0,
    2099,  2101,     0,     0,     0,     0,     0,     0,  1051,   747,
       0,     0,     0,     0,     0,     0,     0,  1763,  1764,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1063,     0,     0,     0,  1778,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   560,   523,     0,     0,
       0,  1049,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2173,     0,     0,     0,     0,     0,  2137,  2139,  2141,
       0,     0,   281,     0,     0,     0,     0,     0,  1489,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2160,  2162,  2164,     0,     0,
       0,     0,     0,     0,     0,   747,     0,     0,     0,   408,
       0,     0,   409,     0,   410,   411,     0,   412,     0,     0,
       0,     0,  1142,     0,     0,     0,     0,     0,     0,     0,
     461,     0,  1229,  1158,   413,  1231,  1834,  1232,  -253,  -253,
    1233,  1234,  1235,  1236,  1237,  1238,  1239,  1240,  1241,  1242,
    1243,  1244,  -352,  -352,  1245,  1246,  1247,  1248,  1249,  1250,
    1251,     0,  1252,     0,   414,   415,     0,   517,   417,  1253,
    1254,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,  1255,   420,   421,   422,     0,   423,   424,     0,
       0,     0,     0,     0,     0,    74,     0,  1881,     0,     0,
     747,     0,     0,   514,     0,  2173,     0,     0,     0,     0,
       0,     0,  1226,     0,     0,     0,  -253,  1256,     0,     0,
      78,   426,  1489,     0,     0,   310,     0,   427,    80,    81,
     428,   429,   430,   431,     0,     0,   523,     0,     0,     0,
       0,     0,  -193,     0,  1908,     0,     0,  1910,     0,     0,
       0,     0,     0,   408,     0,     0,   409,     0,   410,   411,
       0,   412,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1924,     0,  1333,  1229,     0,   413,  1231,
       0,  1232,  -254,  -254,  1233,  1234,  1235,  1236,  1237,  1238,
    1239,  1240,  1241,  1242,  1243,  1244,  -352,  -352,  1245,  1246,
    1247,  1248,  1249,  1250,  1251,     0,  1252,     0,   414,   415,
       0,   517,   417,  1253,  1254,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,  1255,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -254,  1256,     0,     0,    78,   426,   955,     0,     0,   310,
       0,   427,    80,    81,   428,   429,   430,   431,    14,    15,
      16,    17,    18,    19,     0,    20,  -193,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -499,  -499,     0,  -499,    46,     0,    47,     0,
       0,  -499,     0,   359,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1017,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1884,     0,
      74,     0,    75,     0,     0,  1494,  1496,  1498,     0,     0,
       0,     0,     0,     0,     0,  1489,     0,     0,     0,     0,
       0,  1211,     0,     0,     0,    78,    79,     0,     0,     0,
       0,     0,     0,    80,    81,     0,     0,     0,     0,  1520,
       0,     0,     0,     0,     0,     0,   408,     0,     0,   409,
       0,   410,   411,     0,   412,     0,     0,     0,     0,     0,
    1226,     0,     0,     0,     0,     0,  1539,     0,     0,  1229,
    1540,   413,  1231,     0,  1232,     0,     0,  1233,  1234,  1235,
    1236,  1237,  1238,  1239,  1240,  1241,  1242,  1243,  1244,  -352,
    -352,  1245,  1246,  1247,  1248,  1249,  1250,  1251,     0,  1252,
       0,   414,   415,     0,   517,   417,  1253,  1254,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,  1255,
     420,   421,   422,     0,   423,   424,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1256,     0,     0,    78,   426,     0,
       0,     0,   310,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,     0,  -193,
       0,     0,     4,   188,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,  1228,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   408,     0,
      46,   409,    47,   410,   411,     0,   412,    48,    49,    50,
      51,    52,    53,    54,    55,    56,  1685,  1686,     0,    57,
       0,  1229,    58,  1230,  1231,     0,  1232,     0,     0,  1233,
    1234,  1235,  1236,  1237,  1238,  1239,  1240,  1241,  1242,  1243,
    1244,  -352,  -352,  1245,  1246,  1247,  1248,  1249,  1250,  1251,
       0,  1252,     0,   414,   415,    61,   517,   417,  1253,  1254,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,  1255,   420,   421,   422,     0,   423,   424,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1773,    -3,  1256,     0,     0,    78,
    1257,     0,     0,     0,   310,     0,   427,    80,    81,   428,
     429,   430,   431,     0,     0,     0,     0,     0,     0,     0,
       0,  -193,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     4,   188,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,  1228,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   408,     0,    46,   409,    47,   410,   411,     0,
     412,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,  1229,    58,  1230,  1231,     0,
    1232,     0,  1843,  1233,  1234,  1235,  1236,  1237,  1238,  1239,
    1240,  1241,  1242,  1243,  1244,  -352,  -352,  1245,  1246,  1247,
    1248,  1249,  1250,  1251,     0,  1252,     0,   414,   415,    61,
     517,   417,  1253,  1254,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,  1255,   420,   421,   422,     0,
     423,   424,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1256,     0,     0,    78,  1257,     0,     0,     0,   310,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,     0,  -193,     4,   188,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   408,     0,    46,   409,    47,   410,   411,     0,
     412,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   413,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   414,   415,    61,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,     0,
     423,   424,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1704,  1705,  1706,  1707,     0,     0,     0,
     425,  1708,  1709,    78,  1257,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,     0,  1710,     4,   188,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   408,     0,    46,   409,    47,   410,   411,     0,
     412,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   413,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   414,   415,    61,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,     0,
     423,   424,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1704,  1705,  1706,  1707,     0,     0,     0,
     425,  1708,     0,    78,  1257,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,     0,  1710,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,    59,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    61,
      62,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,    79,     0,     0,     0,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    82,     0,    83,   268,   188,     6,     7,
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
      76,    77,     0,    78,   269,     0,     0,     0,  -829,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    82,   268,   188,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -500,  -500,
       0,  -500,    46,     0,    47,     0,     0,  -500,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   152,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,    77,
       0,    78,   269,     0,     0,     0,     0,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    82,   188,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,   359,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   152,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,   617,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1118,    77,  -694,    78,   672,
       0,     0,     0,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    82,   188,
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
    -833,     0,     0,    80,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,   188,     6,     7,     8,
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
       0,     0,    82,     4,   188,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   408,
       0,    46,   409,    47,   410,   411,     0,   412,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,   413,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   414,   415,    61,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,     0,   423,   424,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   425,     0,  1702,
      78,  1257,     0,     0,     0,     0,     0,   427,    80,    81,
     428,   429,   430,   431,     4,   188,     6,     7,     8,     9,
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
       0,    78,  1257,     0,     0,     0,     0,     0,   427,    80,
      81,   428,   429,   430,   431,   188,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     408,     0,    46,   409,    47,   410,   411,     0,   412,   359,
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
       0,    78,   490,     0,     0,     0,     0,     0,   427,   491,
      81,   428,   429,   430,   431,   188,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     408,     0,    46,   409,    47,   410,   411,     0,   412,   359,
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
       0,    78,  1311,     0,     0,     0,     0,     0,   427,  1312,
      81,   428,   429,   430,   431,   188,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     408,     0,    46,   409,    47,   410,   411,     0,   412,   359,
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
       0,    78,   837,     0,     0,     0,     0,     0,   427,   491,
      81,   428,   429,   430,   431,   188,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     408,     0,    46,   409,    47,   410,   411,     0,   412,   359,
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
       0,    78,   426,     0,     0,     0,     0,     0,   427,    80,
      81,   428,   429,   430,   431,   188,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     408,     0,    46,   409,    47,   410,   411,     0,   412,   359,
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
       0,    78,   837,     0,     0,     0,     0,     0,   427,    80,
      81,   428,   429,   430,   431,  2016,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,     0,     0,    -2,     0,     0,    -2,     0,     0,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,    -2,    -2,  2046,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,    -2,     0,    -2,     0,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,     0,     0,    -2,     0,     0,    -2,     0,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,   955,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,    -2,    -2,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -499,  -499,     0,
    -499,    46,     0,    47,     0,     0,  -499,     0,   359,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1299,     0,   955,     0,
      78,    79,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -499,  -499,     0,  -499,    46,     0,
      47,     0,     0,  -499,     0,   359,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1458,     0,   955,     0,    78,    79,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -499,  -499,     0,  -499,    46,     0,    47,     0,     0,
    -499,     0,   359,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1570,     0,   955,     0,    78,    79,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -499,  -499,
       0,  -499,    46,     0,    47,     0,     0,  -499,     0,   359,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1777,     0,   955,
       0,    78,    79,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -499,  -499,     0,  -499,    46,
       0,    47,     0,     0,  -499,     0,   359,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,  1322,
       0,    58,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,   408,     0,     0,   409,     0,   410,   411,
       0,   412,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,    58,   413,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,    79,
       0,     0,     0,     0,     0,     0,    80,    81,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
       0,   423,   424,     0,  1556,     0,     0,     0,     0,    74,
       0,    75,    14,    15,    16,    17,    18,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   425,     0,     0,    78,   426,     0,     0,     0,     0,
       0,   427,   491,    81,   428,   429,   430,   431,   408,     0,
       0,   409,     0,   410,   411,     0,   412,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,   413,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,     0,   423,   424,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   425,     0,     0,    78,
     426,     0,     0,     0,     0,     0,   427,  1557,    81,   428,
     429,   430,   431,    14,    15,    16,    17,    18,    19,     0,
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
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
      78,    79,     0,     0,     0,  -831,     0,     0,    80,    81,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
      82,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   152,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,    78,
     211,     0,     0,     0,     0,     0,     0,    80,    81,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,    82,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   152,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,    77,     0,    78,    79,
       0,     0,     0,     0,     0,     0,    80,    81,     0,     0,
      14,    15,    16,    17,    18,     0,     0,    20,    82,    21,
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
       0,     0,     0,     0,    76,    77,     0,    78,   511,     0,
       0,     0,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    82,     4,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,     0,
       0,     0,     0,  -419,  -419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -419,     0,     0,     0,    78,    79,     0,     0,     0,
       0,     0,     0,    80,    81,     4,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,     0,     0,     0,     0,
    -420,  -420,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -420,     0,
       0,     0,    78,    79,     0,  1465,     0,  1466,     0,     0,
      80,    81,  1467,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1468,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1469,     0,
       0,     0,    78,  1023,     0,  1465,     0,  1466,     0,     0,
      80,    81,  1467,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1468,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1658,     0,
       0,     0,    78,  1023,     0,  1465,     0,  1466,     0,     0,
      80,    81,  1467,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1468,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1659,     0,
       0,     0,    78,  1023,     0,  1465,     0,  1466,     0,     0,
      80,    81,  1467,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1468,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1660,     0,
       0,     0,    78,  1023,     0,     0,     0,     0,     0,     0,
      80,    81,   268,   188,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -500,  -500,     0,  -500,
      46,     0,    47,     0,     0,  -500,     0,   268,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,    58,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -500,  -500,     0,  -500,    46,     0,    47,    63,    64,
    -500,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     269,     0,     0,    63,    64,     0,     0,    80,    81,     0,
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
    1151,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,    75,  1629,     0,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,    74,     0,    75,  1631,     0,
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
    1489,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   408,     0,     0,   409,     0,   410,   411,     0,   412,
       0,     0,     0,     0,    78,   269,     0,     0,     0,     0,
       0,     0,    80,    81,  1229,     0,   413,  1231,     0,  1232,
    1941,  1942,  1233,  1234,  1235,  1236,  1237,  1238,  1239,  1240,
    1241,  1242,  1243,  1244,     0,     0,  1245,  1246,  1247,  1248,
    1249,  1250,  1251,     0,  1252,     0,   414,   415,     0,   517,
     417,  1253,  1254,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,  1255,   420,   421,   422,     0,   423,
     424,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,  1489,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1256,
       0,     0,    78,   426,     0,     0,     0,   310,     0,   427,
      80,    81,   428,   429,   430,   431,     0,   408,     0,     0,
     409,     0,   410,   411,  -193,   412,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1229,     0,   413,  1231,     0,  1232,     0,     0,  1233,  1234,
    1235,  1236,  1237,  1238,  1239,  1240,  1241,  1242,  1243,  1244,
       0,     0,  1245,  1246,  1247,  1248,  1249,  1250,  1251,     0,
    1252,     0,   414,   415,     0,   517,   417,  1253,  1254,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
    1255,   420,   421,   422,     0,   423,   424,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1256,     0,     0,    78,   426,
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
       0,     0,   736,     0,     0,     0,     0,  1244,     0,  -352,
       0,     0,     0,     0,    78,     0,     0,     0,     0,  -423,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,     0,   423,   424,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1256,     0,     0,    78,   737,     0,     0,
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
    1704,  1705,  1706,  1707,     0,     0,     0,   425,  1956,     0,
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
       0,     0,     0,    75,  1222,     0,     0,     0,     0,   188,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1030,
      78,  1023,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,  1576,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,  1023,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   322,     0,    63,    64,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   211,
       0,     0,     0,     0,     0,     0,    80,    81,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,   359,    49,    50,    51,    52,    53,    54,
      55,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,     0,   359,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   361,     0,    63,    64,
       0,     0,     0,    80,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     322,     0,     0,     0,     0,     0,     0,    80,    81,    14,
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
       0,     0,     0,     0,     0,     0,    78,   485,     0,     0,
       0,     0,     0,     0,    80,    81,   188,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -500,
    -500,     0,  -500,    46,     0,    47,     0,     0,  -500,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,     0,   359,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   511,     0,     0,
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
      47,     0,    63,    64,     0,   359,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,  1023,     0,    63,    64,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   339,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   485,     0,    63,    64,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,    79,
       0,     0,     0,     0,     0,     0,    80,    81,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,   359,    49,    50,    51,    52,    53,    54,
      55,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,     0,   359,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,  1023,     0,    63,    64,
       0,     0,     0,    80,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
       0,     0,    14,    15,    16,    17,    18,    80,    81,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -500,  -500,     0,  -500,
      46,     0,    47,     0,     0,  -500,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,    58,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -500,  -500,     0,  -500,    46,     0,    47,    63,    64,
    -500,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     339,     0,     0,    63,    64,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   511,     0,    14,    15,    16,
      17,    18,    80,    81,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -500,  -500,     0,  -500,    46,     0,    47,     0,     0,
    -500,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,     0,     0,     0,     0,     0,
       0,     0,    80,    81,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   408,     0,    46,   409,    47,   410,   411,
       0,   412,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   413,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   425,     0,     0,    78,   426,     0,     0,     0,     0,
       0,   427,   491,    81,   428,   429,   430,   431,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   408,     0,    46,
     409,    47,   410,   411,     0,   412,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   413,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,     0,   423,   424,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   425,     0,     0,    78,   426,
       0,     0,     0,     0,     0,   427,    80,    81,   428,   429,
     430,   431,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   152,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,    78,
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
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,    20,    78,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -500,  -500,
       0,  -500,    46,     0,    47,     0,     0,  -500,     0,   188,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   189,     0,   190,   191,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,    75,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,   408,     0,     0,
     409,     0,   410,   411,     0,   412,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,   413,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   725,     0,
     726,   727,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,   408,   423,   424,   409,    75,   410,
     411,     0,   412,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,     0,     0,     0,     0,   425,     0,     0,    78,   426,
       0,     0,     0,     0,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,     0,     0,     0,  1013,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,     0,   423,   424,     0,     0,   408,     0,     0,   409,
      74,   410,   411,     0,   412,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1704,  1705,  1706,  1707,     0,
       0,   413,   425,  1867,     0,    78,   426,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   414,   415,     0,   517,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,   408,   423,   424,   409,     0,   410,   411,
       0,   412,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   413,     0,
       0,     0,     0,     0,   425,    77,     0,   518,   519,     0,
       0,     0,   520,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
     408,   423,   424,   409,     0,   410,   411,     0,   412,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   413,     0,     0,     0,     0,
       0,   425,  1361,     0,    78,   426,     0,     0,     0,  1362,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,   408,   423,   424,
     409,     0,   410,   411,     0,   412,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   413,     0,     0,     0,     0,     0,   425,     0,
       0,    78,   426,     0,     0,     0,   520,     0,   427,    80,
      81,   428,   429,   430,   431,     0,     0,     0,     0,     0,
       0,     0,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,   408,   423,   424,   409,     0,   410,
     411,     0,   412,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,     0,     0,     0,     0,   425,  1048,     0,    78,   426,
       0,     0,     0,     0,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,   408,   423,   424,   409,     0,   410,   411,     0,   412,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   413,     0,     0,     0,
       0,     0,   425,     0,     0,    78,   426,     0,     0,     0,
     310,     0,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,   408,   423,
     424,   409,     0,   410,   411,     0,   412,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   413,     0,     0,     0,     0,     0,   425,
       0,     0,    78,   426,     0,     0,  1087,     0,     0,   427,
      80,    81,   428,   429,   430,   431,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,   408,   423,   424,   409,     0,
     410,   411,     0,   412,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,     0,     0,   425,     0,     0,    78,
     426,     0,     0,     0,  1499,     0,   427,    80,    81,   428,
     429,   430,   431,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,   408,   423,   424,   409,     0,   410,   411,     0,
     412,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,   425,  1587,     0,    78,   426,     0,     0,
       0,     0,     0,   427,    80,    81,   428,   429,   430,   431,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,   408,
     423,   424,   409,     0,   410,   411,     0,   412,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
     425,     0,     0,    78,   426,     0,     0,     0,  1774,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,   408,   423,   424,   409,
       0,   410,   411,     0,   412,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   413,     0,     0,     0,     0,     0,   425,     0,  1947,
      78,   426,     0,     0,     0,     0,     0,   427,    80,    81,
     428,   429,   430,   431,     0,     0,     0,     0,     0,     0,
       0,   414,   415,     0,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,   408,   423,   424,   409,     0,   410,   411,
       0,   412,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   413,     0,
       0,     0,     0,     0,   425,  1952,     0,    78,   426,     0,
       0,     0,     0,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
       0,   423,   424,     0,     0,   408,     0,     0,   409,    74,
     410,   411,     0,   412,  2033,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,   425,  1962,     0,    78,   426,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,   408,   423,   424,   409,     0,   410,   411,     0,
     412,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,   425,     0,     0,    78,   426,     0,     0,
       0,     0,     0,   427,    80,    81,   428,   429,   430,   431,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,   408,
     423,   424,   409,     0,   410,   411,     0,   412,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
     425,  2040,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,   408,   423,   424,   409,
       0,   410,   411,     0,   412,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   413,     0,     0,     0,     0,     0,   425,  2042,     0,
      78,   426,     0,     0,     0,     0,     0,   427,    80,    81,
     428,   429,   430,   431,     0,     0,     0,     0,     0,     0,
       0,   414,   415,     0,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,   408,   423,   424,   409,     0,   410,   411,
       0,   412,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   413,     0,
       0,     0,     0,     0,   425,  2089,     0,    78,   426,     0,
       0,     0,     0,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
     408,   423,   424,   409,     0,   410,   411,     0,   412,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   413,     0,     0,     0,     0,
       0,   425,  2091,     0,    78,   426,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,     0,     0,
       0,     0,     0,     0,     0,   414,   415,     0,   416,   417,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     418,   419,   405,     0,   420,   421,   422,   408,   423,   424,
     409,     0,   410,   411,     0,   412,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   413,     0,     0,     0,     0,     0,   425,  2093,
       0,    78,   426,     0,     0,     0,     0,     0,   427,    80,
      81,   428,   429,   430,   431,     0,     0,     0,     0,     0,
       0,     0,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,   408,   423,   424,   409,     0,   410,
     411,     0,   412,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,     0,     0,     0,     0,   425,  2098,     0,    78,   426,
       0,     0,     0,     0,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,     0,     0,     0,     0,   414,
     415,     0,   416,   417,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   418,   419,   405,     0,   420,   421,
     422,   408,   423,   424,   409,     0,   410,   411,     0,   412,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   413,     0,     0,     0,
       0,     0,   425,  2100,     0,    78,   426,     0,     0,     0,
       0,     0,   427,    80,    81,   428,   429,   430,   431,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,   408,   423,
     424,   409,     0,   410,   411,     0,   412,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   413,     0,     0,     0,     0,     0,   425,
    2136,     0,    78,   426,     0,     0,     0,     0,     0,   427,
      80,    81,   428,   429,   430,   431,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,   408,   423,   424,   409,     0,
     410,   411,     0,   412,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,     0,     0,   425,  2138,     0,    78,
     426,     0,     0,     0,     0,     0,   427,    80,    81,   428,
     429,   430,   431,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,   408,   423,   424,   409,     0,   410,   411,     0,
     412,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,   425,  2140,     0,    78,   426,     0,     0,
       0,     0,     0,   427,    80,    81,   428,   429,   430,   431,
       0,     0,     0,     0,     0,     0,     0,   414,   415,     0,
     416,   417,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   418,   419,   405,     0,   420,   421,   422,   408,
     423,   424,   409,     0,   410,   411,     0,   412,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
     425,  2159,     0,    78,   426,     0,     0,     0,     0,     0,
     427,    80,    81,   428,   429,   430,   431,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,   408,   423,   424,   409,
       0,   410,   411,     0,   412,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   413,     0,     0,     0,     0,     0,   425,  2161,     0,
      78,   426,     0,     0,     0,     0,     0,   427,    80,    81,
     428,   429,   430,   431,     0,     0,     0,     0,     0,     0,
       0,   414,   415,     0,   416,   417,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   418,   419,   405,     0,
     420,   421,   422,   408,   423,   424,   409,     0,   410,   411,
       0,   412,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   413,     0,
       0,     0,     0,     0,   425,  2163,     0,    78,   426,     0,
       0,     0,     0,     0,   427,    80,    81,   428,   429,   430,
     431,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   425,     0,     0,    78,   426,     0,     0,     0,     0,
       0,   427,    80,    81,   428,   429,   430,   431,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -499,  -499,     0,  -499,    46,   408,    47,     0,
     409,  -499,   410,   411,     0,   412,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,   413,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   414,   415,     0,   416,   417,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   418,   419,   405,
       0,   420,   421,   422,     0,   423,   424,     0,     0,     0,
       0,   408,    75,    74,   409,     0,   410,   411,     0,   412,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   714,   413,     0,    78,   426,
       0,     0,     0,     0,     0,   427,    80,    81,   428,   429,
     430,   431,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   414,   415,     0,   416,
     417,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   418,   419,   405,     0,   420,   421,   422,   408,   423,
     424,   409,     0,   410,   411,     0,   412,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   413,     0,     0,     0,     0,     0,   717,
       0,     0,    78,   426,     0,     0,     0,     0,     0,   427,
      80,    81,   428,   429,   430,   431,     0,     0,     0,     0,
       0,     0,     0,   414,   415,     0,   416,   417,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   418,   419,
     405,     0,   420,   421,   422,   408,   423,   424,   409,     0,
     410,   411,     0,   412,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,     0,     0,   722,     0,     0,    78,
     426,     0,     0,     0,     0,     0,   427,    80,    81,   428,
     429,   430,   431,     0,     0,     0,     0,     0,     0,     0,
     414,   415,     0,   416,   417,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   418,   419,   405,     0,   420,
     421,   422,     0,   423,   424,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   731,     0,     0,    78,   426,     0,     0,
       0,     0,     0,   427,    80,    81,   428,   429,   430,   431,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -500,  -500,     0,  -500,    46,   408,
      47,     0,   409,  -500,   410,   411,     0,   412,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,   413,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   414,   415,     0,   416,   417,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   418,
     419,   405,     0,   420,   421,   422,     0,   423,   424,     0,
       0,     0,     0,   408,    75,    74,   409,     0,   410,   411,
       0,   412,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   425,   413,     0,
      78,   426,     0,     0,     0,     0,     0,   427,   947,    81,
     428,   429,   430,   431,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   414,   415,
       0,   416,   417,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   418,   419,   405,     0,   420,   421,   422,
       0,   423,   424,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   425,     0,     0,    78,   426,     0,     0,     0,     0,
       0,   427,   491,    81,   428,   429,   430,   431
};

static const yytype_int16 yycheck[] =
{
       1,   182,    76,     4,   267,    76,   531,    76,   283,   170,
     967,    87,   425,   241,  1275,    78,  1062,   241,   187,   227,
     684,  1307,  1308,   170,   186,   675,   241,    76,   241,     1,
     170,   854,     4,   856,   187,     1,   241,   236,     1,  1256,
     100,   944,   145,   520,  1090,   841,   960,    76,   251,   241,
     807,    76,   671,     1,    59,    56,    57,   928,    59,   839,
     209,   999,   677,   262,   741,   760,   172,   842,   763,  1007,
     172,   671,   932,   848,   944,    76,   241,   280,   154,   841,
     141,    82,   427,   101,  1256,   378,    87,    59,   291,   382,
     839,   100,   952,    59,    95,   157,    59,   159,   839,   100,
      76,  1380,   103,   331,  1945,   208,   107,   331,   209,   671,
     839,    59,  1817,  1159,  1941,     0,   331,  1817,   331,   590,
       4,    87,   102,    73,   198,     1,   331,    85,   196,   198,
     601,   241,    76,    90,   100,   107,   250,   103,    91,   331,
     103,   107,   157,   257,  1817,   146,   151,     1,   149,   198,
     151,   425,     1,  1715,   136,   241,   157,    76,   577,   578,
    1239,   241,    90,   164,   182,   279,   331,   241,     0,   198,
     241,   172,   241,   198,   645,   126,   290,   157,  1818,   151,
     159,     0,   242,   184,   163,   151,  1209,   137,   151,     1,
     251,   841,   241,  1216,   270,   196,   197,   198,   180,   157,
     165,   100,    73,   151,   157,    59,   163,   208,   165,   160,
      59,   161,   241,   214,   159,   362,   241,   182,   163,   280,
     839,   331,   198,   107,   225,    78,    79,   121,   843,   230,
     291,   197,   847,   242,   235,   236,   237,   165,   182,   839,
     241,   242,   305,   858,   859,   331,   364,    59,   214,    95,
    2077,   331,   109,   110,    87,   316,    73,   331,   318,   121,
     331,   262,   331,   182,   340,   241,   137,   100,    10,   270,
     103,   272,   332,  1296,   107,   157,   242,   839,     1,   385,
     281,   282,   331,   385,   285,  2126,   561,   706,   164,  1929,
     392,   292,   141,   138,   569,  1371,    20,   151,  1212,   161,
    1862,   720,   151,   160,   270,   306,   307,   165,   309,   318,
    1589,   539,   165,   314,   172,   539,  2157,   318,   526,   285,
     137,   157,   285,   332,   539,     1,   539,  1230,   173,  1125,
     331,   332,   400,   356,   539,   985,    59,  1977,  1118,   151,
     157,   342,  1099,   242,  1119,   492,  2051,   539,   761,   350,
     351,  2051,   318,  1213,   355,  1527,   319,  1052,  1530,  1531,
    1230,  1264,  1039,  1125,   197,     1,   332,   660,   861,  1118,
     863,   716,   534,   396,   539,   138,   575,  1118,  2051,   156,
     498,   214,   581,   459,   385,   488,   163,   880,    76,  1118,
     496,  1329,    76,   394,  1264,   814,   397,   600,   244,   400,
     246,   364,   251,    91,   697,   182,   509,   253,    92,   242,
     173,   639,  2052,   157,   156,   639,    78,    79,   141,   318,
     157,     1,  1221,    59,   639,   167,   639,   111,   151,   539,
     172,   280,    73,   332,   639,   584,  1459,   270,  2080,   964,
     714,   151,   291,   717,   181,   719,   157,   639,   722,   163,
     157,   565,   285,   539,   165,   518,   949,   731,   929,   539,
     734,   735,   736,   173,  1335,   539,  2108,   316,   182,   470,
     539,   107,  2112,   137,   639,  1125,   157,  1711,   592,    59,
     326,   327,   154,   584,   157,   599,  2128,   163,   164,   603,
     539,   551,   163,   165,   495,   496,   137,     1,  1221,  1118,
       4,   619,  1221,   165,   470,   141,   507,   508,   113,   181,
     539,   182,   661,   113,   539,   151,   157,   518,  1118,   520,
     161,  1597,  1598,  1599,   163,   292,   112,  1142,   251,   639,
     162,   136,  1749,   163,    87,   498,   136,   684,   539,   600,
     307,   834,   551,   182,  1319,   663,  1592,    62,    63,   135,
     551,   165,   182,   639,   159,    59,  1118,   280,   172,   639,
     661,   141,   165,   539,   163,   639,   165,   231,   291,   687,
     639,   151,    76,   163,   575,   138,   694,  1749,    82,   182,
     581,   641,   583,    87,  1487,   551,   182,   163,  1491,  1492,
     639,  1414,   182,   316,  1417,  1418,   100,   173,   362,   103,
     702,   154,  1505,   107,    59,   168,   169,    62,    63,  1895,
      65,   163,   168,   163,   378,   251,  1093,   182,   382,   175,
     176,  1114,  1115,   173,  1562,   163,   788,    10,  1403,   163,
     182,  1710,   641,  1867,  1868,   173,  1715,   470,   639,   644,
     641,   790,   195,   644,   280,   646,   163,   151,   182,  1313,
     165,   315,   551,   157,   655,   291,   619,  1571,   659,   159,
     163,   161,   870,  1462,  1463,  1464,   163,   182,   172,   632,
     173,   251,   644,   131,   132,   641,   648,  1170,   644,   972,
     316,   644,   163,   852,   320,   182,   835,   163,   394,   790,
     691,   397,   173,   197,   198,   163,   644,   163,   867,   852,
     280,   862,   157,   163,   208,   854,   182,   906,    73,   710,
     214,   291,   163,   862,   182,   862,   856,   270,   551,   177,
     178,   225,  1956,  1957,   687,   182,   789,    73,   492,  1990,
     163,   235,   236,   237,   835,   841,   316,   241,   242,  1462,
    1463,  1464,   641,  1462,  1463,  1464,  1792,   593,  1794,   182,
     163,   600,   753,   159,   755,   163,   757,   163,   262,   958,
     761,   425,    73,   764,   648,     3,   270,    73,     3,   182,
      73,     3,   137,   156,   182,    13,    14,    15,    16,    17,
     163,   285,   163,  1862,   167,  1480,   157,   340,   789,   172,
     644,   137,   157,   164,   163,   644,   161,   643,    70,    76,
    1757,   182,   306,   135,   173,   309,    73,   159,   161,   362,
     314,   157,   661,   166,   318,   161,    93,   931,   170,   171,
      13,    14,    15,    16,    17,   157,   137,   331,   332,   161,
    1768,   137,   644,   834,   137,    73,   168,   169,   839,   160,
     841,   959,   165,  1256,     1,   157,   157,     4,   163,   181,
     161,   157,   853,   165,   157,   161,   974,   161,   161,   158,
     172,    73,   166,   864,    73,   157,   165,   181,   159,   870,
     137,    73,   873,   164,  1035,   165,  1955,   600,   160,   161,
      73,   385,   164,  1158,   648,  1362,  1035,  1306,  1035,  1968,
     157,   225,  1661,   868,   161,   157,   660,  1666,  1000,   137,
     564,    73,    59,   109,   110,   906,   459,    73,   572,    13,
      14,    15,    16,    17,   109,   110,   109,   110,  1087,    73,
     684,   644,   159,   179,  1574,   137,   163,   591,   137,    73,
     159,  1034,   159,   697,   163,   137,   157,   164,   602,   492,
     161,   942,   943,   944,   137,   157,   103,   157,   157,   161,
     107,   161,   161,  2032,   157,   157,   928,   958,  1166,   161,
     159,   160,  1865,   159,   600,   137,   470,   160,   164,    73,
     159,   137,   944,   121,   159,   164,   159,  1502,   531,   164,
     163,   944,  1256,   137,    73,   157,   835,   162,   960,   161,
     159,   157,   496,   137,   151,   161,   944,   159,   157,  1168,
     159,   158,   161,   157,   159,  1006,   159,   161,   644,    73,
      73,   974,  1013,   157,   518,   157,   520,   161,  1124,  1125,
     600,   159,  1499,    73,   577,   578,     3,   157,   159,   135,
    1005,   161,   163,   137,   138,   539,    13,    14,    15,    16,
      17,   157,   159,   159,   928,   161,   163,   551,   137,   159,
     714,   157,   209,   717,  1467,   161,   159,  1058,   722,  1322,
     163,  1062,   168,   169,   644,   159,   157,   731,   157,   163,
     834,   575,   161,   137,   137,  1349,   960,   581,   163,   583,
      22,    13,    14,    15,    16,    17,   750,   137,   157,  1090,
     944,   157,  1093,   157,   157,   944,    73,   161,   161,    13,
      14,    15,    16,    17,  1303,   170,   171,   157,  1585,    47,
      48,   161,    50,   102,  2017,   159,   135,  1118,    56,   163,
     157,   157,   163,  1124,  1125,   161,  1307,  1308,   285,   157,
      59,   684,   944,    62,    63,   639,    65,   641,   157,   157,
     644,    73,   161,  1668,  2047,  1221,   162,   159,   165,   168,
     169,   163,   157,   706,   159,   659,   161,    90,  1159,    73,
     137,   714,   129,   130,   928,   165,  1313,   720,   156,   722,
     114,   115,   116,   117,   118,  1450,   135,   157,  2081,   252,
     182,  1342,   165,  1344,   518,   159,   520,   691,   159,   163,
      13,    14,    15,    16,    17,  1344,   960,  1344,   157,  1317,
     157,   162,   161,   159,   161,   137,   138,   163,   972,   168,
     169,   768,   769,   770,  1383,   150,   151,   152,   153,   162,
     163,   944,   159,   137,  1326,   165,   163,   157,   163,   159,
    2113,   161,  1233,   172,  2117,  1236,  1237,  1238,   173,   159,
    1212,   159,   159,   163,  1245,   163,   163,   182,   912,   753,
      73,   755,   157,   757,   181,  1424,  1425,   159,  1230,  1420,
     764,   814,   926,  1264,   133,   134,   930,  1230,  1417,  1270,
     934,  1424,  1425,  1420,  1414,   159,   162,   163,  1418,   163,
    1420,   413,  1230,   121,  1285,   789,   157,  1288,  1289,   159,
    1291,  1986,  1264,   163,   159,   159,  1297,  1774,   163,   163,
    1301,  1264,  1303,   162,   163,   159,   438,   439,   944,   163,
     157,   159,   135,   157,   137,   163,  1264,  1289,   150,   151,
     152,   153,  1288,  1289,   157,  1288,   163,   459,  1212,   159,
     834,   163,  1487,   163,   157,   839,  1491,   841,   161,   168,
     169,   173,   162,   163,  1345,   168,   169,   162,   163,  1195,
     182,   169,  1327,   174,  1317,   150,   151,   152,   153,  1334,
     492,  1362,   167,  1335,   944,   179,   870,   163,   164,   873,
    1371,   135,   150,   151,   152,   153,  1230,     1,   173,   160,
       4,  1230,  1658,  1659,  1660,  1546,  1462,  1463,  1464,   462,
     161,  1467,  1468,   162,   163,   173,   159,  1546,  1567,  1546,
     162,   163,   906,  1404,   182,  1289,   479,   162,   163,   482,
    1264,   964,  1387,  1473,  1567,  1264,    92,    93,  1230,    13,
      14,    15,    16,    17,    18,   157,  1487,   584,   162,   163,
    1491,   775,   776,   777,   778,    59,   162,   163,   942,    13,
      14,    15,    16,    17,    18,  1673,   162,   163,  1212,  1673,
     159,  1335,  1264,   159,   958,  1288,  1289,   159,  1673,   124,
    1673,   126,   127,   128,  1473,  1218,  1219,  1220,  1673,   162,
     163,   544,  1473,  1680,  1681,  1682,  1477,  1478,   159,   103,
     159,  1673,   157,   107,   162,   163,   138,   644,   162,   163,
     162,   648,   157,  1465,   138,   160,   161,   163,  1499,   163,
     165,   166,   162,   163,   661,   162,   163,  1230,  1673,  1013,
    1784,   162,   163,   162,   163,  1487,   163,   164,   164,  1491,
    1492,   164,  1523,  1524,  1487,   162,   163,   151,  1491,  1492,
     162,   163,  1533,  1505,    78,    79,   870,   163,   164,   873,
     163,  1264,  1505,    13,    14,    15,    16,    17,    18,  1313,
     157,  1215,  1372,  1373,  1058,   771,   772,   159,  1062,   773,
     774,   779,   780,  1673,  1565,  1229,   181,  1533,  1530,  1531,
    1533,  1335,  1681,  1682,  1473,   159,  1738,   159,   159,   159,
     159,  1465,   159,   159,  1585,  1249,  1090,  1673,  1563,  1093,
     159,  1592,  1256,  1673,  1230,   181,  1597,  1598,  1599,  1571,
     162,  1661,  1673,   161,  1832,   165,  1666,    71,  1832,   165,
     165,  1586,   165,   165,  1118,  1675,   159,  1832,   163,  1832,
    1124,  1125,   162,   182,   157,    79,   162,  1832,  1264,    18,
     181,   165,   182,   790,   165,   767,   159,   159,  1487,   165,
    1832,   165,  1491,  1492,    13,    14,    15,    16,    17,    18,
    1230,   162,  1661,  1289,   162,  1159,  1505,  1666,    18,   162,
    1661,   285,   162,   156,   159,  1666,  1675,  1832,   159,   159,
     159,   159,  1673,   159,  1675,  1487,   159,   159,   835,  1491,
    1492,   159,  1683,   159,  1264,    22,   159,  1571,   159,   156,
     159,   165,    71,  1505,   162,   157,   181,   854,   159,   856,
    1533,  1702,   165,   860,   861,   165,   863,   159,  1709,   159,
     159,   159,   165,   156,  1895,   159,   163,   159,   163,   165,
    1882,   159,  1832,   880,  1058,   159,   163,  1699,  1062,   159,
     162,   159,   163,   806,   807,  1698,   159,   159,   163,  1740,
     159,   159,   159,   159,   817,  1720,  1832,   820,   159,   159,
     162,   156,  1832,  1306,   162,   159,  1090,   159,   159,  1093,
    1313,   159,  1661,   159,  1487,   159,   159,  1666,  1491,  1492,
     162,   928,  1941,  1774,   159,  1835,  1675,   159,  1977,   159,
     163,  1285,  1505,   159,  1288,  1289,   159,   944,  1941,   159,
      14,  1792,   949,  1794,   156,  1770,   163,   163,   163,  1303,
     163,   157,   959,   960,  1880,   157,   157,  1571,   157,   157,
     157,  1786,   157,   886,   164,  1699,   162,  1481,  1482,   163,
     893,  1983,   164,  2051,   897,  1159,  1835,  2051,   901,   182,
     162,  1832,   165,   156,  1835,   165,  2051,   163,  2051,   156,
     181,  1345,   181,  1844,  1845,   159,  2051,   159,   159,   159,
    1851,  1487,   159,   162,   162,  1491,  1492,   159,  1362,  2051,
    1978,   159,   163,  1864,   159,   162,   159,  1371,  1532,  1505,
     159,   159,   162,  1874,   156,  1876,   156,   182,  1938,   157,
    2049,   157,    81,   182,  1847,    93,  2051,   156,  1889,   157,
    1891,  1892,  1893,  1865,   157,   182,   182,   182,  1030,  1975,
    1404,   182,  1865,   182,  1036,   182,  2075,  1487,  2077,    91,
     159,  1491,  1492,   156,   162,  1047,   162,   162,   162,   156,
     156,   159,  2075,   165,  2077,  1505,   124,  1902,   156,  1938,
     164,  1906,   164,   159,   159,   162,  1835,  1938,   159,   162,
     159,  2051,   159,   159,  1945,   159,  2115,   156,  1949,  1502,
     163,  2113,   182,  1954,   164,  2117,  2118,  1114,  1115,   159,
     157,   162,  2115,   159,   159,  2051,   157,   157,   162,  1473,
     159,  2051,   156,   159,   162,   156,  1977,  2051,  1979,   159,
    2051,   159,  2051,     1,  2146,   159,     4,   162,    76,    76,
     182,   156,  2052,   157,   162,  1499,   159,   182,   157,   182,
     162,   156,  2051,   156,   161,  2167,    76,  2008,   159,  2171,
     159,  2180,   159,  1170,    76,   173,  1865,   173,    76,  2020,
     644,  2183,   182,  2024,   648,   164,  1099,  2180,  1362,  1533,
     182,   156,  2033,   156,   182,   156,   182,  1371,  2039,  1938,
     158,    59,   173,  2052,   173,  2017,   156,   159,   164,   157,
    2051,  2052,  2112,  1865,  2017,  1212,   107,   163,    76,   159,
     173,   158,   173,  1699,   159,   182,  1730,    76,   162,    87,
     159,   164,   158,  1230,   159,  2047,   156,   156,   182,   157,
     182,  1585,   100,  2084,  2047,   103,  2052,   159,  1592,   107,
     182,  1784,   740,  1597,  1598,  1599,   781,   783,  1230,   782,
     784,  1174,  1251,  2112,  1177,   785,   209,  1264,  1181,  2081,
    2157,  2112,  1264,  1339,  1491,  1668,  1880,  2077,  2081,  1873,
    2121,  2108,   458,  1505,  1865,  2126,  2066,   145,  1748,  1731,
    2048,  1288,  1289,   151,  1731,  1975,   154,  2118,  2171,   157,
     158,  2047,  1865,    49,  1291,   116,  2112,  1938,   276,  1468,
    2006,  2152,   170,  2052,  2155,  1331,  2157,  1661,  1773,  1285,
     870,   655,  1666,   527,  1539,  1499,     0,   806,  2017,  1673,
     806,  1675,   710,   806,  1651,  2176,    -1,   195,  1335,   197,
     198,  1313,    -1,  2184,    -1,    13,    14,    15,    16,    17,
     208,   209,  2193,    -1,    -1,    -1,   214,    -1,  2047,    13,
      14,    15,    16,    17,    -1,  2017,    -1,    -1,    -1,    -1,
      -1,  1975,    -1,  2112,    -1,    -1,    -1,    -1,   236,    -1,
    1352,  1353,  1354,   241,   242,    -1,    62,  1359,  1360,  1865,
      -1,    -1,  2081,    -1,    -1,  2047,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   262,    73,    -1,    -1,    -1,    -1,
      -1,  1585,   270,    -1,    -1,    -1,    -1,  1414,  1592,    73,
    1417,  1418,    -1,  1597,  1598,  1599,    -1,   285,    -1,  2081,
    1774,    -1,   108,    -1,    -1,    -1,    -1,   113,    -1,    -1,
     116,    -1,   118,    -1,    -1,  1865,    13,    -1,  1792,    -1,
    1794,    -1,    -1,    -1,  2017,    -1,    -1,    -1,    -1,    -1,
     318,    -1,    -1,    -1,   928,    -1,   324,   135,  1465,   137,
      -1,     3,   330,   331,   332,    -1,  1389,    -1,    -1,    -1,
     944,   135,   340,   137,  2047,    -1,    -1,  1400,  1832,   157,
    1487,  1835,    -1,   161,  1491,  1492,   960,    -1,    -1,    -1,
     168,   169,    -1,   157,   362,   363,   364,   161,  1505,    -1,
      -1,    -1,    -1,    -1,   168,   169,    -1,    -1,  2081,    -1,
     378,    -1,    89,    -1,   382,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,     4,    -1,    -1,  1533,    -1,    -1,   309,
     107,  2017,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   425,    -1,    -1,
      -1,  2047,    -1,    -1,  1571,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    59,    -1,
     157,    -1,    -1,    -1,  1938,    -1,    -1,  2017,   274,    -1,
    1774,   459,    -1,   135,   462,  2081,    -1,  2111,    -1,    -1,
      -1,    -1,   470,    -1,    -1,    73,    87,    -1,  1792,    -1,
    1794,    -1,    18,    -1,    -1,   157,   158,  2047,    -1,    -1,
     488,    -1,   164,  1977,   492,  1979,   168,   169,   496,  2143,
     498,   584,    -1,    -1,    -1,   321,    -1,    -1,   180,   107,
      -1,   509,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,  2081,    58,    59,    60,    61,    62,    63,    64,    65,
     141,    -1,    -1,   531,   145,    -1,    -1,   135,    -1,   137,
     151,   539,    -1,   154,    -1,    -1,    -1,    -1,    -1,  2033,
     366,    73,   368,   551,   370,    -1,    -1,    -1,    -1,   157,
     158,  1698,  1699,    -1,    -1,    -1,    -1,  2051,  2052,    -1,
     168,   169,    -1,    -1,    -1,    -1,    -1,   575,   661,   577,
     578,    -1,   180,   581,    -1,   107,   584,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   208,    -1,    -1,
     416,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1212,  1662,
      -1,    -1,    -1,    -1,   107,   137,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,  1230,    -1,  2112,    -1,
      -1,    -1,  1744,    -1,    -1,   157,   158,    -1,    -1,    -1,
     251,   639,    -1,   641,    -1,    -1,   644,    -1,    -1,    -1,
     648,    -1,    -1,    -1,    -1,    -1,   566,   268,   180,   270,
    1264,    -1,   660,   661,    -1,   663,    -1,    -1,    -1,   280,
      -1,    -1,    -1,   671,    -1,    -1,    -1,   675,    -1,    -1,
     291,    -1,    -1,    -1,  1288,  1289,   684,    -1,    -1,   182,
      -1,    -1,    -1,    -1,    -1,    -1,   694,    -1,    -1,   697,
    2184,   517,    -1,   314,    -1,   316,    -1,   790,   706,  2193,
    1847,    -1,    -1,    -1,    -1,    -1,   714,    -1,    -1,   717,
      -1,   719,   720,    -1,   722,    -1,    -1,    -1,  1865,   340,
      -1,  1335,   642,   731,    -1,   107,   734,   735,   736,   111,
     112,   113,   114,   115,   116,   117,   118,    58,    -1,    -1,
      -1,    -1,   835,    -1,    -1,    66,    67,    68,    69,    -1,
      -1,    -1,    -1,     3,  1817,  1818,    -1,    -1,    -1,    -1,
      -1,   854,    -1,   856,    -1,    -1,    -1,   860,   861,    -1,
     863,   597,    -1,    -1,    -1,    -1,   158,    -1,    -1,   161,
      73,    -1,   790,    -1,    -1,    -1,   107,   880,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   806,   807,
      -1,    -1,    -1,    -1,    -1,    -1,   814,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   834,   835,    13,    79,
      -1,   839,    -1,   841,    -1,    -1,    -1,    -1,   459,    -1,
     161,    -1,   135,    -1,   137,    -1,   854,   940,   856,    -1,
      -1,  1465,   860,   861,   862,   863,   949,    -1,   179,     1,
      -1,    -1,     4,  1985,   157,   158,  1929,   488,   161,    -1,
    2017,    -1,   880,  1487,    -1,   168,   169,  1491,  1492,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   806,   180,   509,    -1,
      -1,  1505,    -1,    -1,    -1,    -1,    -1,    -1,   906,    -1,
    2047,    -1,   822,    -1,    89,    -1,   826,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1977,    -1,    -1,    59,    -1,  1533,
     928,    -1,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,  2081,    -1,   944,    -1,    -1,  2002,
      -1,   949,    -1,  2006,    -1,    87,    -1,    18,    -1,    -1,
     958,   959,   960,    -1,    -1,    -1,   964,  1571,    -1,    -1,
      -1,   211,    -1,    -1,   972,   107,   974,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   116,    -1,    -1,   985,    -1,   600,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,  2051,  2052,
      -1,    62,    63,    64,    65,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,   145,    -1,    -1,    -1,    -1,    -1,   151,
      -1,   100,   154,    -1,    -1,    -1,    -1,    -1,    -1,   269,
      -1,  1114,  1115,   644,   107,    -1,  1034,  1035,   111,   112,
     113,   114,   115,   116,   117,   118,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,  2112,
      -1,    -1,    -1,   195,    -1,    -1,    -1,    -1,    -1,    73,
     310,    -1,    -1,    -1,    -1,    -1,   208,    -1,   157,    -1,
      -1,    -1,   322,    -1,    -1,    -1,    -1,  1170,    -1,    -1,
      -1,    -1,    -1,    -1,  1698,  1699,    -1,    -1,    -1,   339,
     161,  1099,    -1,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,  1114,  1115,    -1,   251,
    1118,   361,    -1,    -1,    -1,    -1,    -1,  1125,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,   268,    -1,   270,    -1,
      -1,    -1,    -1,    -1,   276,    -1,    -1,    -1,   280,    -1,
      -1,    -1,    -1,   157,   158,    -1,    -1,   236,    -1,   291,
      -1,    -1,   241,   242,   168,   169,    -1,    -1,    -1,    -1,
      -1,    -1,  1170,    -1,    -1,    -1,   180,    -1,    -1,    -1,
      -1,    -1,   314,   262,   316,    -1,   426,    -1,   320,    -1,
      -1,    -1,    -1,    -1,    -1,  1105,   107,    -1,    -1,  1109,
     111,   112,   113,   114,   115,   116,   117,   118,   340,    -1,
      -1,    -1,    -1,    -1,  1212,    -1,    -1,   457,  1128,    -1,
      -1,    -1,    -1,  1221,    -1,  1135,    -1,    -1,    -1,    -1,
      -1,    -1,  1230,    -1,    -1,    -1,    -1,    -1,    -1,   318,
      -1,    -1,    -1,  1847,    -1,   485,   157,   158,    -1,    -1,
     490,    -1,   331,   332,    -1,    -1,    -1,    -1,  1256,    -1,
      -1,  1865,    -1,    -1,    -1,    -1,  1264,    -1,    -1,   180,
     510,   511,    -1,  1183,    -1,   515,   516,  1187,   107,   519,
     109,  1191,   111,   112,   113,   114,   115,   116,   117,   118,
    1288,  1289,    -1,   425,    -1,   535,     4,     5,     6,     7,
       8,     9,    10,    11,    12,  1303,    -1,    -1,  1306,    -1,
      -1,    -1,    -1,    -1,    -1,  1313,    -1,   557,   209,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   459,    -1,    -1,
      73,  1414,    -1,   944,  1417,  1418,    -1,  1335,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,  1344,    -1,    79,    -1,
      -1,  1349,    -1,    -1,    -1,    -1,   488,    -1,    66,    -1,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   107,   509,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,   135,    -1,   137,   635,    -1,    -1,    -1,   531,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   309,
      -1,    -1,    -1,  2017,   157,   158,  1414,   496,    -1,  1417,
    1418,    -1,  1420,  1034,    -1,   168,   169,    -1,    87,    -1,
      -1,    -1,   672,    -1,    -1,    -1,    -1,   180,    -1,    -1,
      -1,    -1,    -1,  2047,   103,   577,   578,    -1,   107,    -1,
      -1,   182,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     539,    -1,    -1,    -1,  1462,  1463,  1464,  1465,   600,  1467,
    1468,   362,   551,    -1,   365,  1473,  1474,  2081,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,   145,   378,    -1,  1487,
      -1,   382,   151,  1491,  1492,   154,   575,   737,    -1,   158,
      -1,    -1,   581,    -1,  1502,    -1,    -1,  1505,    -1,    -1,
     169,   170,   644,   172,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,  1435,    -1,    -1,    -1,  1439,
      -1,    -1,    -1,  1443,    -1,  1533,   195,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,  1546,   208,
     209,    -1,   154,    -1,    -1,   214,    -1,    -1,    -1,    -1,
     639,   107,   641,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,  1571,   706,    -1,  1574,    -1,    -1,   181,
      -1,    -1,   714,    -1,    -1,   717,    -1,   719,   720,   829,
     722,   831,   251,    -1,    -1,    -1,    -1,   837,    -1,   731,
      -1,   492,   734,   735,   736,    -1,    -1,    -1,    -1,   268,
    1221,   270,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1230,
      -1,   280,    -1,    -1,    -1,   865,   285,    -1,    -1,    -1,
      -1,    -1,   291,    -1,   874,    -1,   182,    -1,   878,    -1,
      -1,  1551,    -1,    -1,    -1,    -1,    -1,   306,    -1,    -1,
     309,    -1,    -1,  1264,    -1,   314,    -1,   316,    -1,    -1,
     319,   320,    -1,  1661,  1662,   324,   566,    -1,  1666,    -1,
    1668,   330,    -1,    -1,    -1,  1673,    -1,  1675,    -1,    -1,
      -1,   340,   814,   923,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,   584,    -1,  1605,    -1,    -1,    -1,    -1,
    1698,  1699,    -1,   362,  1614,   364,   365,   107,  1618,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   378,
      -1,    -1,    -1,   382,    -1,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,   107,   642,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   135,    -1,   137,   648,    -1,    -1,
     839,    -1,   841,    -1,    -1,    -1,   425,    -1,    -1,   660,
     661,   671,   172,    -1,    -1,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,  1023,    -1,    -1,  1784,   168,   169,  1233,
      -1,    -1,   158,   684,    -1,   161,    -1,    -1,    -1,   180,
     459,  1245,    73,    -1,    -1,    -1,   697,    -1,    -1,    -1,
      -1,    -1,   944,    -1,    -1,    -1,    -1,    -1,    -1,  1817,
    1818,    -1,    -1,    -1,    -1,    -1,    -1,   906,    -1,   488,
      -1,    -1,   964,   492,  1832,    -1,   107,  1835,    -1,   498,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,  1847,
     509,  1462,  1463,  1464,  1465,  1466,  1467,  1468,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,  1865,    -1,    -1,
      -1,    -1,   531,    -1,    -1,    -1,  1487,    -1,    -1,   958,
    1491,  1492,  1880,    -1,    -1,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,    -1,  1505,    -1,    -1,   168,   169,   790,
      -1,    -1,  1034,    -1,    -1,    -1,   806,   566,    -1,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   577,   578,
      -1,    -1,   822,   582,    -1,   584,   826,    -1,    -1,    -1,
      -1,  1929,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   839,
    1938,   600,    -1,   834,   835,    -1,    -1,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     619,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1208,    -1,
      -1,   862,    -1,   632,    -1,    -1,   135,  1975,    -1,  1977,
    1978,    -1,    -1,   642,    -1,   644,    -1,    -1,    -1,   648,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
     659,   660,   661,    -1,   663,   164,    -1,    -1,  2006,   168,
     169,    -1,   671,    -1,    -1,    -1,    -1,  1257,    -1,  2017,
      -1,   180,    -1,    -1,    -1,   684,    -1,    -1,   687,    -1,
      -1,    -1,   691,    -1,    -1,   694,    -1,   928,   697,  1118,
     940,    -1,    -1,    -1,    -1,  1124,  1125,   706,    -1,  2047,
      -1,    -1,    -1,  2051,  2052,   714,    -1,    -1,   717,    -1,
     719,   720,    -1,   722,    -1,    -1,    -1,    -1,    -1,   960,
      -1,  1311,   731,    -1,    -1,   734,   735,   736,    -1,  1523,
    1524,   972,    -1,  2081,   975,    -1,    -1,    -1,    -1,  1221,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1230,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   107,    -1,    -1,  2112,   111,   112,   113,   114,   115,
     116,   117,   118,   119,  1256,    -1,    -1,   135,    -1,    -1,
     107,   790,  1264,    -1,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,  1035,    -1,   123,   806,   125,   157,
     158,    -1,    -1,   161,    -1,   814,    -1,  1289,    -1,  2069,
     168,   169,    -1,   822,    -1,   161,    62,   826,    -1,    -1,
      -1,    -1,   180,   181,  1306,   834,   835,    -1,    -1,    -1,
     839,   158,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   854,    -1,   856,    -1,    -1,
      -1,   860,   861,   862,   863,  1105,    -1,   103,    -1,  1109,
     135,    -1,    -1,    -1,    -1,    -1,    -1,  1349,  1118,   115,
     116,   880,    -1,    -1,  1303,    -1,    -1,    -1,  1128,    -1,
      -1,    -1,   157,   158,    -1,  1135,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,
     909,    -1,    -1,    -1,  1865,   180,    -1,    -1,  1702,    -1,
      -1,    -1,   158,    -1,    -1,  1709,    -1,    -1,   107,   928,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   940,    -1,  1183,    -1,   944,    -1,  1187,    -1,    -1,
     949,  1191,    -1,    -1,    -1,    -1,  1740,    -1,    -1,    -1,
     959,   960,    -1,    -1,    -1,   964,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   972,    -1,   974,   975,    -1,   214,    -1,
      -1,  1212,    -1,    -1,    -1,  1404,    -1,    -1,    -1,   168,
    1462,  1463,  1464,  1465,  1466,  1467,  1468,    -1,    -1,    -1,
      -1,    -1,   107,  1002,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,  1487,    -1,    -1,    -1,  1491,
    1492,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1502,    -1,    -1,  1505,    -1,  1034,  1035,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   285,
      -1,    -1,    -1,    -1,  1473,    -1,   161,    -1,    -1,    -1,
    1844,  1845,    -1,    -1,    -1,    -1,  2017,  1851,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
    1864,    -1,  1313,   319,    -1,    -1,    -1,    -1,   324,    -1,
    1874,    -1,  1876,    -1,   330,   165,  2047,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1335,  1889,  1105,  1891,  1892,  1893,
    1109,    -1,    -1,  1344,    -1,  1114,  1115,    -1,    -1,  1118,
      -1,   160,    -1,    -1,    -1,    -1,    -1,    -1,   364,  1128,
    2081,    -1,    -1,   172,    -1,    -1,  1135,    -1,    -1,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1945,    -1,    -1,    -1,  1949,    -1,   135,    -1,    -1,
    1954,  1170,    13,    14,    15,    16,    17,    -1,    -1,    -1,
     416,    -1,    -1,    -1,  1183,    -1,    -1,    -1,  1187,   157,
     158,    -1,  1191,    -1,    -1,  1435,  1668,    -1,    -1,  1439,
     168,   169,    -1,  1443,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,  1212,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1221,    -1,  2008,    -1,    -1,  1699,    -1,    -1,
      -1,  1230,    73,    -1,    -1,     1,  2020,    -1,     4,    -1,
    2024,    -1,  1661,    -1,    -1,    -1,    -1,  1666,    -1,    -1,
      -1,    -1,    -1,    -1,  1673,  2039,  1675,  1256,    -1,    -1,
      -1,    -1,   498,    -1,    -1,  1264,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,   517,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1288,
    1289,    -1,    -1,    59,   135,    -1,   137,    -1,    -1,    -1,
    2084,    -1,    -1,    -1,    -1,    -1,    -1,  1306,    -1,    -1,
      -1,  1551,  1784,    -1,  1313,  1546,   157,   158,  1317,    -1,
     161,    87,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,
      -1,     1,    -1,    -1,    -1,    -1,  1335,  2121,    -1,   180,
    1571,   107,  2126,  1342,    -1,  1344,   582,    -1,    -1,   107,
    1349,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,  1943,    -1,  1605,    -1,    -1,  2152,   605,
      -1,  2155,    -1,  2157,  1614,   141,    -1,   135,  1618,   145,
      -1,    -1,    -1,   619,    -1,   151,    -1,    -1,    -1,    59,
      -1,  1971,  2176,  1865,    -1,    -1,   632,    -1,    -1,   157,
     158,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,  1832,    -1,  1414,  1835,    -1,  1417,  1418,
      -1,  1420,   180,    -1,    -1,    -1,    -1,   663,    -1,    -1,
      -1,   197,    -1,    -1,    -1,    -1,  1435,   107,    -1,    -1,
    1439,    -1,   208,   209,  1443,    -1,    -1,    -1,    -1,    -1,
      -1,   687,    -1,    -1,    -1,    -1,  1455,    -1,   694,    -1,
      -1,    -1,    -1,  1462,  1463,  1464,  1465,  1466,  1467,  1468,
      -1,   141,    -1,    -1,    -1,  1474,   242,    -1,    -1,    -1,
      -1,   151,    -1,    -1,    -1,   251,    -1,    -1,  1487,    -1,
      -1,    -1,  1491,  1492,    -1,    -1,   262,    -1,    -1,    -1,
     170,   267,   268,  1502,   270,    -1,  1505,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   280,    -1,    -1,    -1,    -1,  1938,
      -1,    -1,    -1,    -1,    -1,   291,    -1,    -1,   294,    -1,
      -1,    -1,   298,    -1,  1533,    -1,    -1,   303,    -1,   209,
      -1,    -1,    -1,   309,    -1,  2017,    -1,  1546,   314,    -1,
     316,    -1,  1551,    -1,   320,    -1,    -1,    -1,  1977,    -1,
      -1,    -1,    -1,    -1,    -1,  1564,   332,    -1,    -1,    -1,
      -1,    -1,  1571,    -1,    -1,  2047,    -1,    -1,    -1,    -1,
      -1,   251,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,   362,    -1,    -1,   365,
      -1,    -1,    -1,    -1,    -1,    -1,  1605,    -1,    -1,  2081,
     280,    -1,   378,    -1,    -1,  1614,   382,    -1,    -1,  1618,
      -1,   291,    -1,    -1,   294,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2051,  2052,    -1,    -1,    -1,    -1,    -1,   309,
      -1,    -1,    -1,    -1,    -1,    73,   316,    -1,    -1,  1880,
     320,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1668,
      -1,    -1,    -1,   909,    -1,    -1,    -1,    -1,    -1,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   362,  2112,    -1,   365,    -1,    -1,    -1,  1698,
    1699,    -1,    -1,    -1,    -1,    -1,    -1,   135,   378,   137,
      -1,    -1,   382,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   488,   959,    -1,    -1,   492,    -1,    -1,   157,
     158,    -1,    -1,   161,    -1,    -1,    -1,    -1,   974,    -1,
     168,   169,    -1,   509,  1975,    -1,    -1,    -1,    -1,    -1,
      -1,   107,   180,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   135,
      -1,   137,    -1,    -1,   107,  1784,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
     566,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    -1,    -1,   157,    -1,   584,   160,
     161,    -1,   492,    -1,   180,    -1,    -1,    -1,    -1,  2069,
      -1,    -1,    -1,    -1,   600,    -1,    -1,   160,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,  1847,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,  1862,    -1,    -1,  1865,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   642,    -1,   644,    -1,
      -1,  1880,   648,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   660,   661,   566,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   671,    -1,    -1,    -1,   675,
      -1,    -1,    -1,    73,   584,    -1,    -1,    -1,   684,    -1,
      -1,    -1,    -1,   689,    -1,    -1,    -1,    -1,    -1,    -1,
     600,   697,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
    1959,  1960,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   642,    -1,   644,   135,  1975,   137,   648,  1978,
      -1,    -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,
     660,   661,    -1,    -1,    -1,   168,   169,   157,   158,    -1,
      -1,   671,    -1,    -1,    -1,    -1,    -1,   180,   168,   169,
      -1,    -1,    -1,    -1,   684,    -1,    -1,    -1,  2017,    -1,
     180,    -1,    -1,    -1,   790,    -1,    -1,   697,    -1,    -1,
      -1,    -1,    -1,    -1,  2033,    -1,    -1,    -1,    -1,    -1,
     806,   807,    -1,    -1,    -1,     1,    -1,    -1,  2047,    -1,
      -1,    -1,  1288,    -1,    -1,    -1,   822,    -1,    -1,    -1,
     826,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   834,   835,
    2069,    -1,    -1,   839,    -1,   841,    -1,    -1,    -1,    -1,
      -1,  1317,  2081,    -1,    -1,    -1,    -1,    -1,   854,    -1,
     856,    -1,    -1,    -1,   860,   861,   862,   863,    -1,    -1,
      -1,    -1,   107,    59,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   880,    -1,    -1,   107,    -1,    -1,
     790,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,    -1,   123,    -1,   125,   806,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,   107,   822,    -1,    -1,   160,   826,    -1,    -1,    -1,
      -1,    -1,   928,    -1,   834,   835,    -1,    -1,   158,   839,
      -1,   161,    -1,    -1,   940,    -1,    -1,    -1,   944,    -1,
      -1,    -1,    -1,   949,   854,   141,   856,   157,    -1,    -1,
     860,   861,   862,   863,   960,   151,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   972,    -1,    -1,   975,
     880,    -1,    -1,    -1,   170,    -1,   982,    -1,    -1,  1455,
      -1,   107,    -1,    -1,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,    -1,    -1,   123,  1474,   125,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   209,    -1,   107,    -1,    -1,   928,   111,
     112,   113,   114,   115,   116,   117,   118,   119,  1034,  1035,
     940,   123,   158,   125,   944,   161,    -1,    -1,   107,   949,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     960,   157,    -1,    -1,    -1,   251,    -1,  1533,    -1,    -1,
      -1,    -1,   972,   102,    -1,   975,   158,    -1,   107,   161,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,   280,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   291,    -1,    -1,    -1,  1105,
      -1,    -1,    -1,  1109,    -1,    -1,    -1,    -1,  1114,  1115,
      -1,    -1,  1118,   309,    -1,    -1,    -1,    -1,    -1,    -1,
     316,    -1,  1128,    -1,   320,  1035,    -1,    -1,    -1,  1135,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,  1170,    -1,   362,    -1,    51,   365,
      53,    -1,    -1,    -1,    -1,    -1,    -1,  1183,    -1,    -1,
      -1,  1187,   378,    -1,    -1,  1191,   382,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,  1105,    -1,    -1,    -1,  1109,
      -1,    -1,    -1,    -1,  1114,  1115,  1212,    -1,  1118,    -1,
      -1,    -1,    -1,    -1,    -1,  1221,    -1,    -1,  1128,    -1,
      -1,    -1,  1698,    -1,  1230,  1135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
       4,    -1,    -1,    -1,   137,    -1,    -1,    -1,  1264,    -1,
    1170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1183,    -1,    -1,    -1,  1187,    -1,    -1,
      -1,  1191,    -1,  1289,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   492,    -1,    -1,    -1,
      -1,    -1,  1212,    -1,    -1,    59,    -1,  1313,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1322,    -1,    -1,    -1,
    1230,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1335,
      -1,    -1,    -1,    87,    -1,    -1,  1342,    -1,  1344,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,  1264,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     566,  1847,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1289,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,   584,    -1,
      -1,   145,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,
     154,    -1,    -1,  1313,   600,    -1,    -1,    -1,  1414,    -1,
      -1,  1417,  1418,    -1,  1420,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    -1,    -1,  1335,    -1,    -1,    -1,  1435,
      -1,    -1,  1342,  1439,  1344,    -1,    -1,  1443,    -1,    -1,
      -1,   195,    -1,    -1,    -1,    -1,   642,    -1,   644,    -1,
      -1,    -1,   648,    -1,   208,    -1,  1462,  1463,  1464,  1465,
    1466,   132,    -1,    -1,   660,   661,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   671,    -1,    -1,    -1,    -1,
      -1,  1487,    -1,    -1,    -1,  1491,  1492,    -1,   684,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   251,    -1,  1505,
      -1,   697,  1978,    -1,  1414,    -1,    -1,  1417,  1418,    -1,
    1420,    -1,    -1,    -1,   268,    -1,   270,    -1,    -1,    -1,
      -1,    -1,   276,    -1,    -1,  1435,   280,    -1,    -1,  1439,
      -1,    -1,    -1,  1443,    -1,    -1,    -1,   291,    -1,    -1,
    1546,    -1,    -1,    -1,    -1,  1551,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     314,    -1,   316,    -1,    -1,  1571,   320,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1487,    -1,    -1,
      -1,  1491,  1492,    -1,    -1,    -1,   340,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   790,  1505,    -1,    -1,    -1,  1605,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1614,    -1,
     806,    -1,  1618,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   822,    -1,    -1,    -1,
     826,    -1,    -1,    -1,    -1,    -1,  1546,    -1,   834,   835,
      -1,  1551,    -1,   839,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,   854,    -1,
     856,  1571,    -1,    -1,   860,   861,   862,   863,    -1,  1675,
      -1,   425,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   880,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1699,    -1,  1605,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1614,   459,    -1,    -1,  1618,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   928,    -1,   488,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   940,    -1,    -1,    -1,   944,    -1,
      -1,    -1,    -1,   949,    -1,   509,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   960,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   972,   531,    -1,   975,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1699,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1818,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     491,    -1,   493,   577,   578,    -1,    -1,    -1,    -1,    -1,
      -1,   502,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1035,
      -1,    -1,    -1,    -1,    -1,    -1,   600,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1862,    -1,    -1,  1865,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1880,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     644,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1105,
      -1,    -1,    -1,  1109,    -1,    -1,    -1,    -1,  1114,  1115,
      -1,    -1,  1118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1128,    -1,    -1,    -1,    -1,    -1,    -1,  1135,
      -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,   706,    -1,    -1,  1865,    -1,    -1,    -1,    -1,
     714,    -1,    -1,   717,    -1,   719,   720,    -1,   722,  1975,
    1880,    -1,    -1,    -1,  1170,    -1,    -1,   731,    -1,    -1,
     734,   735,   736,    -1,    -1,    -1,    -1,  1183,    -1,    -1,
      -1,  1187,    -1,    -1,    -1,  1191,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,  2017,    -1,    -1,    -1,    -1,  1212,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,  1230,    -1,    -1,    -1,    -1,    -1,
      -1,  2047,    -1,    -1,    -1,   103,  2052,    -1,    -1,    -1,
     107,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,   117,
     814,   119,    -1,  2069,    -1,  1975,    -1,    -1,  1264,    -1,
      -1,    -1,    -1,    -1,    -1,  2081,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,   145,    -1,
      -1,    -1,    -1,  1289,   151,    -1,    -1,   154,    -1,    -1,
     158,    -1,    -1,   161,   162,    -1,    -1,  2017,    -1,   187,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1313,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2047,   195,  1335,
      -1,    -1,    -1,    -1,    -1,    -1,  1342,    -1,  1344,    -1,
      -1,   208,    -1,    -1,    -1,    -1,   214,    -1,    -1,  2069,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2081,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   852,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     944,    -1,    -1,    -1,   251,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     964,   268,    -1,   270,    -1,    -1,    -1,    -1,  1414,   276,
      -1,  1417,  1418,   280,  1420,    -1,    -1,   285,    -1,   287,
     288,    -1,    -1,    -1,   291,    -1,    -1,    -1,    -1,  1435,
      -1,    -1,    -1,  1439,    -1,    -1,    -1,  1443,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   314,    -1,   316,
      -1,   319,    -1,   320,    -1,    -1,   324,    -1,    -1,    -1,
      -1,    -1,   330,    -1,    -1,    -1,   947,   948,    -1,    -1,
    1034,    -1,    -1,   340,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1487,    -1,    -1,    -1,  1491,  1492,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   364,    -1,    -1,  1505,
      -1,   369,    -1,   371,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     408,    -1,   410,    -1,    -1,   413,   414,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   423,   424,    -1,    -1,    -1,
    1546,    -1,    -1,    -1,    -1,  1551,    -1,    -1,   416,    -1,
     438,   439,    -1,    -1,    -1,    -1,    -1,    -1,   425,    -1,
      -1,    -1,    -1,    -1,    -1,  1571,    -1,    -1,    -1,    -1,
    1051,   459,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   459,    -1,    -1,    -1,    -1,    -1,    -1,  1605,
      -1,    -1,   470,    -1,   492,    -1,    -1,    -1,  1614,    -1,
      -1,  1092,  1618,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   488,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     498,    -1,   500,   501,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   509,    -1,    -1,    -1,    -1,    -1,    -1,   517,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1221,    -1,    -1,
    1141,    -1,  1143,    -1,   531,    -1,  1230,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1155,    -1,  1157,    -1,    -1,    -1,
      -1,  1162,  1163,   551,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1172,  1256,  1699,    -1,    -1,    -1,    -1,    -1,    -1,
    1264,    -1,    -1,    -1,    -1,    -1,    -1,   575,    -1,    -1,
     577,   578,   580,    -1,   582,    -1,    -1,  1198,    -1,    -1,
    1201,    -1,    -1,    -1,    -1,  1289,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   600,    -1,    -1,    -1,   605,    -1,   607,
     608,    -1,  1306,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   619,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   632,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   641,    -1,    -1,    -1,   644,    -1,    -1,
      -1,    -1,    -1,  1264,    -1,  1349,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   663,    -1,   665,   666,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1294,    -1,    -1,    -1,    -1,    -1,   687,
     688,    -1,    -1,    -1,    -1,    -1,   694,    -1,    -1,    -1,
      -1,  1312,    -1,  1314,    -1,  1316,    -1,    -1,    -1,   706,
      -1,    -1,  1323,    -1,    -1,    48,    -1,   714,    -1,    -1,
     717,    -1,   719,   720,    -1,   722,  1862,    -1,    -1,  1865,
      -1,    -1,    -1,    -1,   731,    -1,    -1,   734,   735,   736,
      -1,    -1,    -1,    76,  1880,    -1,    -1,    -1,    -1,   767,
     768,   769,   770,   771,   772,   773,   774,   775,   776,   777,
     778,   779,   780,   781,   782,   783,   784,   785,  1462,  1463,
    1464,  1465,  1466,  1467,  1468,    -1,    -1,    -1,    48,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1397,  1398,    -1,    -1,
      -1,    -1,   125,  1487,    -1,    -1,    -1,  1491,  1492,    -1,
      -1,    -1,    -1,    -1,    -1,   138,    -1,   140,  1502,    -1,
      -1,  1505,  1423,    -1,    -1,    -1,    -1,   814,    -1,  1430,
      -1,  1432,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   852,    -1,    -1,    -1,   171,  1975,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   198,    -1,    -1,   138,    -1,
     140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2017,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   171,    -1,    -1,    -1,    -1,    -1,    -1,   241,    -1,
      -1,  2047,   245,    -1,    -1,   248,   249,    -1,    -1,   252,
      -1,    -1,   255,   256,    -1,   258,    -1,   260,    -1,    -1,
      -1,    -1,    -1,  2069,    -1,    -1,    -1,    -1,    -1,  1550,
      -1,    -1,    -1,    -1,    -1,  2081,  1557,   944,  1559,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   959,    -1,    -1,    -1,    -1,    -1,   964,    -1,    -1,
      -1,    -1,    -1,    -1,  1668,    -1,   974,    -1,   248,   249,
      -1,    -1,   252,    -1,    -1,   255,   256,   985,   258,    -1,
     260,    -1,    -1,    -1,    -1,    -1,   994,    -1,   331,    -1,
      -1,   334,    -1,    -1,    -1,  1699,     1,    -1,    -1,    -1,
      -1,    -1,  1030,    -1,    -1,    -1,    -1,    -1,  1036,    -1,
      -1,    -1,    -1,    -1,  1635,    -1,   359,    -1,    -1,  1047,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1034,    -1,    -1,
      -1,   374,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
      55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,  1087,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   359,
    1784,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1099,    -1,    -1,   374,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,  1125,   123,   124,
     125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,    -1,    -1,    -1,    -1,   479,    -1,    -1,    -1,
      -1,    -1,  1763,  1764,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,  1865,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   539,    -1,    -1,   479,
      -1,    -1,  1230,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   555,    -1,  1221,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1230,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1256,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1264,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   555,    -1,    -1,    -1,    -1,
    1288,    -1,  1289,    -1,    -1,  1313,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   639,    -1,    -1,  1306,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1317,
      -1,    -1,    -1,  2017,    -1,  1323,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1352,  1353,  1354,    -1,    -1,    -1,
      -1,  1359,  1360,    -1,    -1,    -1,   679,   680,    -1,    -1,
      -1,    -1,  1349,  2047,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1383,   699,    -1,   701,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2081,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1424,  1425,    -1,   679,
     680,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   699,
      -1,   701,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1462,  1463,  1464,  1465,  1466,
    1467,  1468,    -1,    -1,    -1,    -1,  1474,   810,   811,    -1,
      -1,    -1,    -1,    -1,   817,    -1,    -1,    -1,    -1,    -1,
    1487,    -1,    -1,    -1,  1491,  1492,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1502,    -1,    -1,  1505,   842,
      -1,    -1,   845,   846,    -1,   848,  2127,   850,   851,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2142,    -1,    -1,    -1,  1533,    -1,    -1,    -1,    -1,
     810,   811,    -1,    -1,    -1,    -1,    -1,   817,    -1,  1567,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     893,    -1,    -1,    -1,   897,    -1,    -1,    -1,   901,    -1,
      -1,    -1,   842,    -1,    -1,   845,   846,    -1,   848,    -1,
     850,   851,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   893,    -1,    -1,    -1,   897,    -1,    -1,
      -1,   901,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   208,   209,    -1,    -1,    -1,    -1,    -1,    -1,
     983,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1668,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   245,
      -1,    -1,    -1,    -1,    -1,    -1,   252,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1698,    -1,  1699,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   983,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    -1,  1744,    -1,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    -1,    56,    -1,   334,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1784,    -1,    -1,
      -1,  1124,    -1,    -1,    -1,    -1,   362,   363,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    -1,   382,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1169,  1835,  1171,    -1,
      -1,  1174,    -1,    -1,  1177,    -1,    -1,    -1,  1181,  1847,
      -1,    -1,    -1,    -1,  1124,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1865,   159,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
      -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,   425,    -1,
      -1,    -1,   182,    -1,    -1,   170,   462,    -1,    -1,  1169,
      -1,  1171,    -1,    -1,  1174,    -1,    -1,  1177,    -1,    -1,
      -1,  1181,    -1,   479,   480,    -1,   482,   483,    -1,    -1,
     195,    -1,    -1,  1941,    -1,    -1,   492,    -1,    -1,    -1,
     496,  1929,    -1,    -1,   209,    -1,    -1,    -1,    -1,   214,
      -1,    -1,    -1,   509,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1960,  1961,    -1,    -1,    -1,  1985,    -1,    -1,
      -1,    -1,    -1,    -1,   540,    -1,    -1,    -1,   544,    -1,
    1978,    -1,    -1,    -1,    -1,    -1,  1319,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   270,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     285,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   584,    -1,
    2017,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2049,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     577,   578,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1319,
    2047,    -1,    -1,    -1,    -1,   330,  1389,  2075,    -1,  2077,
      -1,    -1,    -1,    -1,    -1,   340,    -1,  1400,    -1,    -1,
    1403,    -1,  1405,  1406,   640,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2081,    -1,    -1,   362,    -1,   364,
      -1,    -1,    -1,    -1,   660,   661,    -1,  2115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   671,    -1,    -1,    -1,   675,
      -1,    -1,    -1,    -1,  2112,    -1,   682,    -1,   684,  1389,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1400,    -1,    -1,  1403,    -1,  1405,  1406,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     425,    -1,    -1,    -1,    -1,    -1,    -1,   195,    -1,    -1,
      -1,    -1,  2180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   209,    -1,    -1,    -1,    -1,    -1,   714,    -1,    -1,
     717,  1514,    -1,    -1,   459,   722,    -1,   225,    -1,   227,
      -1,    -1,    -1,    -1,   731,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   750,    -1,    -1,    -1,   492,    -1,    -1,
      -1,    -1,    -1,   498,   790,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     806,   807,    -1,    -1,  1514,    -1,    -1,    -1,    -1,   786,
      -1,   817,   818,    -1,   820,   821,   531,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   834,   835,
      -1,    -1,    -1,   839,    -1,   841,   842,    -1,    -1,    -1,
    1613,    -1,   848,    -1,    -1,    -1,    -1,    -1,   854,    -1,
     856,   329,    -1,    -1,   860,   861,   862,   863,    -1,    -1,
      -1,    -1,   577,   578,    -1,    -1,    -1,    -1,  1641,   584,
      -1,    -1,    -1,    -1,   880,    -1,   882,    -1,    -1,    -1,
     886,    -1,    -1,    -1,    -1,    -1,    -1,   893,   894,    -1,
      -1,   897,   898,    -1,    -1,   901,   902,    -1,    -1,    -1,
    1673,    -1,   908,  1613,    -1,    -1,  1679,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1641,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   949,   950,    -1,   661,    -1,   663,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   684,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   985,
      -1,    -1,  1755,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   706,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   714,
      -1,    -1,   717,    -1,   719,   720,    -1,   722,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   731,    -1,    -1,   734,
     735,   736,    -1,    -1,    -1,    -1,  1799,  1800,  1034,  1035,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   520,    -1,    -1,  1755,    -1,    -1,   526,    -1,
      -1,  1824,  1825,   531,    -1,    -1,    -1,    -1,    -1,  1832,
      -1,    -1,    -1,    -1,  1837,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   790,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1799,
    1800,    -1,    -1,  1099,    -1,    -1,    -1,    -1,    -1,   814,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1114,  1115,
      -1,    -1,  1118,  1119,  1824,  1825,    -1,    -1,    -1,  1125,
     835,    -1,    -1,    -1,    -1,    -1,    -1,  1837,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   854,
      -1,   856,    -1,    -1,    -1,   860,   861,   862,   863,    -1,
      -1,    -1,    -1,    -1,    -1,   633,    -1,    -1,  1931,    -1,
      -1,    -1,    -1,    -1,  1170,   880,    -1,    -1,  1174,  1175,
      -1,  1177,  1178,    -1,    -1,  1181,  1182,    -1,    -1,    -1,
      -1,    -1,    -1,   661,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   674,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1931,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2002,
      -1,    -1,    -1,   711,   949,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   959,    -1,   724,    -1,    -1,   964,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   974,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     748,   749,    -1,    -1,   752,    -1,   754,    -1,  2051,  1256,
      -1,    -1,   760,    -1,   762,   763,    -1,    -1,    -1,    -1,
      -1,    -1,  2002,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1313,    -1,    -1,
      -1,    -1,   790,  1319,  1320,    -1,    -1,    -1,    -1,    -1,
    1035,    -1,    -1,    -1,    -1,   803,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   814,    -1,  1344,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   830,    -1,    -1,    -1,    -1,   835,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1389,  1390,    56,    57,    -1,    -1,   867,
      -1,    -1,   870,    -1,  1400,  1401,    -1,  1403,    -1,  1114,
    1115,    -1,    -1,   881,    -1,    -1,    -1,    -1,  1414,    -1,
      -1,  1417,  1418,    -1,  1420,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    95,    -1,    -1,    -1,    -1,    -1,
      -1,   909,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,   149,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   964,    -1,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   974,   975,    -1,    -1,
      -1,    -1,    -1,    -1,   982,    -1,  1221,    -1,    -1,    -1,
    1487,  1488,    -1,   184,  1491,  1492,    -1,    -1,    -1,    -1,
    1497,    -1,    -1,    -1,  1501,   196,  1503,    -1,  1505,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1546,  1256,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1035,    -1,   230,
      -1,    -1,    -1,    -1,    -1,  1043,    -1,    -1,  1574,    -1,
      -1,    -1,    -1,  1288,  1052,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1306,    -1,    -1,    -1,    -1,    -1,    -1,  1313,    -1,
      -1,   272,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     281,   282,    -1,    -1,    -1,  1093,    -1,    -1,    -1,    -1,
      -1,   292,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1344,
      -1,    -1,    -1,    -1,  1349,    -1,   307,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1662,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   342,  1649,  1679,    -1,    -1,    -1,    -1,    -1,   350,
     351,    -1,    -1,    -1,   355,    -1,    -1,    -1,  1166,    -1,
    1168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1414,
      -1,    -1,  1417,  1418,    -1,  1420,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1694,    -1,    -1,
      -1,    -1,    -1,   394,    -1,    -1,   397,    -1,    -1,   400,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1714,  1715,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1462,  1463,  1464,
      -1,    -1,  1467,  1468,    -1,    -1,    -1,    -1,    -1,  1474,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1745,    -1,
      -1,    -1,  1250,  1251,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1502,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1817,  1818,    -1,    -1,    -1,    -1,    -1,  1533,    -1,
      -1,    -1,    -1,    -1,   495,    -1,    -1,  1833,    -1,    -1,
      -1,  1546,    -1,    -1,    -1,    -1,   507,   508,    -1,  1317,
      -1,    -1,    -1,    -1,    -1,  1323,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1339,    -1,    -1,    -1,    -1,  1344,    -1,    -1,    -1,
      -1,    -1,  1849,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1857,    -1,  1859,    -1,  1362,  1862,  1863,  1365,  1865,    -1,
      -1,    -1,    -1,  1870,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1380,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1929,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1937,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1668,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1977,    -1,    -1,    -1,   646,  1454,  1455,    -1,    -1,
      -1,    -1,  1959,  1698,   655,    -1,    -1,    -1,    -1,  1966,
    1967,    -1,    -1,    -1,    -1,    -1,  2002,  2003,    -1,    -1,
    2006,    -1,  1480,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1989,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1499,    -1,    -1,  1502,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   710,
      -1,    -1,    -1,    -1,    -1,  2051,  2052,    -1,  2025,    -1,
    2027,    -1,    -1,  2030,  2031,    -1,    -1,    -1,    -1,    -1,
    2037,  2038,    -1,    -1,    -1,    -1,    -1,    -1,  1546,  1784,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1555,  1556,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     761,    -1,    -1,    -1,  1572,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2112,  1585,    -1,    -1,
      -1,  1589,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,  2104,  2105,  2106,
      -1,    -1,  1847,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2132,  2133,  2134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1880,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    55,    -1,    57,    -1,    -1,
      -1,    -1,   853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1668,    -1,    72,   864,    74,    75,  1674,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,    -1,   102,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,  1735,    -1,    -1,
    1975,    -1,    -1,  1978,    -1,     1,    -1,    -1,    -1,    -1,
      -1,    -1,   943,    -1,    -1,    -1,   156,   157,    -1,    -1,
     160,   161,    18,    -1,    -1,   165,    -1,   167,   168,   169,
     170,   171,   172,   173,    -1,    -1,  1774,    -1,    -1,    -1,
      -1,    -1,   182,    -1,  1782,    -1,    -1,  1785,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1811,    -1,  1006,    72,    -1,    74,    75,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,    -1,   102,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,   157,    -1,    -1,   160,   161,     1,    -1,    -1,   165,
      -1,   167,   168,   169,   170,   171,   172,   173,    13,    14,
      15,    16,    17,    18,    -1,    20,   182,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      -1,    56,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1986,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
     135,    -1,   137,    -1,    -1,  1236,  1237,  1238,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,   156,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,  1270,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,
    1291,    -1,    -1,    -1,    -1,    -1,  1297,    -1,    -1,    72,
    1301,    74,    75,    -1,    77,    -1,    -1,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,    -1,   102,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,
      -1,    -1,   165,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   182,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    55,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,  1477,  1478,    -1,    70,
      -1,    72,    73,    74,    75,    -1,    77,    -1,    -1,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,    -1,   127,   128,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1565,   156,   157,    -1,    -1,   160,
     161,    -1,    -1,    -1,   165,    -1,   167,   168,   169,   170,
     171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   182,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    55,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    -1,    72,    73,    74,    75,    -1,
      77,    -1,  1683,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,    -1,
     127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   182,     3,     4,     5,     6,
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
      -1,    -1,    -1,   150,   151,   152,   153,    -1,    -1,    -1,
     157,   158,   159,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   182,     3,     4,     5,     6,
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
      -1,    -1,    -1,   150,   151,   152,   153,    -1,    -1,    -1,
     157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   182,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    -1,    -1,    73,    74,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,   106,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   180,    -1,   182,     3,     4,     5,     6,
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
      -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   180,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   180,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
      -1,    -1,    -1,   135,    -1,   137,   138,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,   158,   159,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,     4,
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
     165,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   180,     4,     5,     6,     7,
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
      -1,    -1,   180,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    55,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    -1,    -1,    -1,
      70,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,   159,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,     3,     4,     5,     6,     7,     8,
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
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
     169,   170,   171,   172,   173,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
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
     169,   170,   171,   172,   173,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
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
     169,   170,   171,   172,   173,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
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
     169,   170,   171,   172,   173,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
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
     169,   170,   171,   172,   173,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
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
     169,   170,   171,   172,   173,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,
      56,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    70,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    78,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,    56,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    78,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,
      -1,     1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    -1,    56,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,     1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    -1,    56,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,    -1,     1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,
      56,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,     1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,     1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    -1,    56,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,     5,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,     5,    -1,    -1,    -1,    -1,   135,
      -1,   137,    13,    14,    15,    16,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,    49,    -1,
      -1,    52,    -1,    54,    55,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,
     171,   172,   173,    13,    14,    15,    16,    17,    18,    -1,
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
     160,   161,    -1,    -1,    -1,   165,    -1,    -1,   168,   169,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
     180,    22,    23,    24,    25,    26,    27,    28,    29,    30,
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
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,   180,
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
      -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,   180,    22,
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
      -1,    -1,    -1,    -1,    -1,   168,   169,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    58,    59,    60,    61,    62,    63,    64,
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
      -1,    -1,    -1,   168,   169,     3,     4,     5,     6,     7,
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
      -1,    -1,   160,   161,    -1,     3,    -1,     5,    -1,    -1,
     168,   169,    10,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,
      -1,    -1,   160,   161,    -1,     3,    -1,     5,    -1,    -1,
     168,   169,    10,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,
      -1,    -1,   160,   161,    -1,     3,    -1,     5,    -1,    -1,
     168,   169,    10,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,
      -1,    -1,   160,   161,    -1,     3,    -1,     5,    -1,    -1,
     168,   169,    10,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,    -1,     3,    -1,    -1,
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
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      65,    -1,    13,    14,    15,    16,    17,    18,    73,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,   109,   110,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,   109,   110,
      -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    73,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,   109,   110,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,    -1,    -1,   109,   110,    -1,    -1,    -1,
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
      65,    -1,    13,    14,    15,    16,    17,    18,    73,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,   109,   110,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,   109,   110,
      -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
      -1,    -1,    13,    14,    15,    16,    17,   168,   169,    20,
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
      -1,    -1,    -1,    -1,   160,   161,    -1,    13,    14,    15,
      16,    17,   168,   169,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
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
      62,    63,    64,    65,    -1,    13,    14,    15,    16,    17,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,   160,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    73,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   107,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,   137,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,
     109,   110,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    49,   127,   128,    52,   137,    54,
      55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,   180,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    -1,   127,   128,    -1,    -1,    49,    -1,    -1,    52,
     135,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   150,   151,   152,   153,    -1,
      -1,    74,   157,   158,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    49,   127,   128,    52,    -1,    54,    55,
      -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,
      -1,    -1,   165,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
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
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,    -1,
      -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,   168,
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
     165,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    49,   127,
     128,    52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,   164,    -1,    -1,   167,
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
      -1,    74,    -1,    -1,    -1,    -1,    -1,   157,    -1,   159,
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
      -1,   127,   128,    -1,    -1,    49,    -1,    -1,    52,   135,
      54,    55,    -1,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,   157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    49,    53,    -1,
      52,    56,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    49,   137,   135,    52,    -1,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    74,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,   167,   168,   169,   170,   171,   172,   173,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    49,
      53,    -1,    52,    56,    54,    55,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    49,   137,   135,    52,    -1,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    74,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173
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
     433,   415,   413,   190,   162,     1,   301,   303,   315,   317,
     406,   407,   408,   409,   157,   395,   393,   394,    79,   328,
     188,   315,   471,   485,   317,   321,   492,   373,   474,   475,
     476,   162,   184,    18,   223,   317,   473,   495,   427,   427,
     471,   315,   483,   493,   317,   188,   315,   485,   427,   165,
     427,   368,    10,   167,   368,   370,   371,   165,   159,   384,
     159,   159,   431,   180,   220,   221,   222,   223,   182,   383,
     493,   193,   383,   161,   383,   384,   383,   493,   223,   383,
     159,   383,   383,   383,   162,   184,   159,   170,   171,   206,
      18,   319,   159,   163,   159,   168,   169,   159,   158,   223,
     229,   223,   165,   223,   188,   223,   188,   119,   161,   188,
     220,   119,   161,   190,   353,   223,   220,   188,   204,   207,
     207,   207,   208,   208,   209,   209,   210,   210,   210,   210,
     211,   211,   212,   213,   214,   215,   216,   164,   230,   191,
     161,   188,   223,   165,   223,   373,   465,   466,   467,   317,
     464,   427,   427,   223,   384,   157,   427,   468,   471,   157,
     468,   471,   373,   373,   184,   184,   162,   162,   157,   433,
     456,   457,   458,   461,    18,   317,   455,   459,   157,   427,
     477,   495,   427,   427,   495,   157,   427,   477,   427,   427,
     185,   219,   190,   377,   380,   162,   380,   381,   162,   495,
     495,   138,   375,   376,   377,   375,   377,   375,   190,   184,
     218,   219,   223,   425,   494,   386,   388,   156,   184,   159,
     184,   159,   375,   223,   159,   159,   159,   159,   159,   159,
     159,   159,   159,   157,   427,   468,   471,   157,   427,   468,
     471,   157,   427,   468,   471,   424,    22,   471,   223,   324,
     340,   469,   234,   159,   159,   159,   159,   159,   411,   412,
     234,   156,   406,   413,   234,   422,   412,   234,   165,   165,
     165,   354,   138,   378,   379,   188,   190,   296,    18,    72,
      74,    75,    77,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    94,    95,    96,    97,    98,
      99,   100,   102,   109,   110,   122,   157,   161,   190,   230,
     231,   232,   233,   234,   235,   236,   238,   239,   248,   255,
     256,   257,   258,   259,   260,   265,   266,   269,   270,   271,
     272,   273,   274,   275,   281,   282,   283,   297,   317,   321,
     423,    71,   185,   185,   375,   414,   412,   159,   424,   156,
     407,   163,   182,   163,   182,   401,   181,   392,   392,   315,
     485,   161,   168,   202,   223,   340,   223,   317,   159,   159,
     159,   159,     5,   317,   427,   473,   366,   370,   368,   165,
     340,   163,   494,   190,   370,   165,   159,   188,   159,   163,
     159,   159,   163,   159,   184,   163,   159,   159,   159,   163,
     159,   204,   159,   159,   159,   204,    18,   319,   223,   159,
     159,   158,   165,   204,   162,   163,   185,   220,   162,   162,
     119,   123,   125,   189,   197,   198,   199,   159,   197,   162,
     163,   156,   218,   164,   159,   197,   185,   387,   159,   159,
     159,   159,   464,   373,   373,   159,   159,   375,   375,   461,
     159,   159,   159,   159,   157,   433,   460,   455,   459,   373,
     373,   162,   185,   495,   163,   185,   159,   163,   163,   185,
     163,   185,   385,   197,   138,   173,   185,   185,   156,   386,
     223,   427,   375,   427,   185,   157,   427,   468,   471,   157,
     427,   468,   471,   157,   427,   468,   471,   373,   373,   373,
     426,   151,   173,   185,   470,   163,   185,   414,   156,   412,
     234,   414,   354,   354,   354,     3,     5,    10,    74,   156,
     298,   305,   306,   314,   317,   355,   360,   488,   163,   182,
     157,    62,    63,   182,   234,   297,   423,   157,   157,    18,
     232,   157,   157,   182,   190,   182,   190,   168,   190,   165,
     231,   157,   157,   157,   232,   157,   234,   223,   224,   224,
      14,   284,   260,   271,   164,   182,   185,   236,    79,   182,
     190,    92,    93,   264,   268,   113,   136,   263,   112,   135,
     267,   263,   382,   317,   296,   162,   162,   185,   414,   190,
     190,   424,   159,   384,   398,   398,   184,   185,   185,   185,
     223,   157,   427,   477,   471,   316,     5,   168,   185,   223,
     368,   494,   165,   370,    10,   371,   156,   181,   372,   494,
     156,   406,   181,   222,   313,   188,    79,   194,   195,   383,
     204,   204,   204,   204,   204,   165,   387,   158,   223,   163,
     156,   200,   161,   198,   200,   200,   162,   163,   126,   160,
     162,   229,   218,   162,   494,   157,   427,   468,   471,   159,
     159,   185,   185,   159,   157,   427,   468,   471,   157,   427,
     477,   433,   427,   427,   159,   159,   162,   380,   162,   138,
     377,   138,   159,   159,   185,   219,   219,   162,   162,   185,
     185,   159,   373,   373,   373,   159,   159,   159,   385,   163,
     223,   223,   324,   340,   162,   156,   414,   156,   156,   156,
     156,   314,   314,   353,   361,   488,   314,   360,   157,   349,
     182,   182,   182,   157,   164,   202,   356,   357,   363,   433,
     434,   447,   451,   163,   182,   190,   190,   220,   182,   234,
     182,   234,   230,   240,   297,   299,   302,   308,   317,   321,
     230,    81,   159,   240,   150,   151,   152,   153,   158,   159,
     182,   230,   249,   250,   252,   297,   182,   182,   230,   182,
     387,   182,   230,   401,   230,   249,   114,   115,   116,   117,
     118,   276,   278,   279,   182,   101,   182,    85,   157,   159,
     427,   156,   182,   182,   157,   157,   232,   232,   260,   157,
     270,   260,   270,   234,   182,   159,   156,   396,   162,   162,
     162,   185,   373,   223,   223,   185,   162,   185,   165,   156,
     370,   494,   340,   190,   165,   219,   156,   156,   223,   472,
     473,   159,   164,   159,   163,   164,   387,   494,   229,   124,
     197,   198,   161,   198,   161,   198,   162,   156,   373,   159,
     159,   373,   373,   162,   185,   159,   427,   159,   159,   159,
     230,   470,   156,   349,   349,   349,   356,   157,   202,   358,
     359,   468,   479,   480,   481,   482,   182,   163,   182,   356,
     182,   401,   428,   433,   223,   317,   156,   163,   182,   362,
     363,   362,   362,   190,   159,   159,   230,   317,   159,   157,
     232,   159,   150,   151,   152,   153,   173,   182,   253,   254,
     232,   231,   182,   254,   159,   164,   230,   158,   230,   231,
     252,   182,   494,   159,   159,   159,   159,   234,   278,   279,
     157,   223,   157,   191,     1,   232,   204,   261,   230,    76,
     111,   262,   264,    76,   427,   392,   162,   159,   185,   185,
     162,   162,   370,   494,   156,   372,   387,   159,   223,   195,
     223,   494,   156,   162,   162,   197,   197,   159,   427,   427,
     159,   159,   162,   162,   223,   182,   480,   481,   482,   317,
     479,   163,   182,   427,   427,   182,   159,   433,   427,   232,
     232,    78,    79,   165,   243,   244,   245,   159,   230,    76,
     232,   230,   158,   230,    76,   182,   158,   230,   231,   252,
     317,   339,   158,   230,   232,   250,   254,   254,   182,   230,
     156,   165,   245,   232,   232,   157,   280,   315,   317,   488,
     182,   191,   159,   164,   159,   163,   164,   159,   232,   157,
     232,   232,   232,   398,   162,   162,   494,   156,   494,   156,
     162,   162,   159,   159,   159,   479,   427,   357,    76,     1,
     219,   241,   242,   425,     1,   164,     1,   184,   232,   243,
      76,   182,   159,   232,    76,   182,   173,   173,   232,   231,
     254,   254,   182,    58,   230,   251,   340,   173,   173,    76,
     158,   230,   158,   230,   231,   182,     1,   184,   280,   182,
     277,   157,   202,   424,   479,   188,   164,   182,   161,   191,
     285,   286,   287,   204,   220,   230,   263,   156,   156,   157,
     427,   468,   471,   359,   232,   138,     1,   163,   164,   156,
     290,   291,   297,   232,    76,   182,   232,   230,   158,   158,
     230,   158,   230,   158,   230,   231,   188,   340,   158,   230,
     158,   230,   232,   173,   173,   173,   173,   156,   290,   277,
     218,   159,   317,   164,   107,   157,   159,   164,   163,   159,
     159,    76,   259,   373,   219,   241,   244,   246,   247,   297,
     232,   173,   173,   173,   173,   158,   158,   230,   158,   230,
     158,   230,   246,   159,   234,   285,   162,   219,   182,   285,
     287,   232,    76,   159,   232,   237,   185,   244,   158,   158,
     230,   158,   230,   158,   230,   185,   234,   164,   191,   159,
     159,   164,   232,     1,   232,   156,   237,   156,   191,   288,
     157,   182,   288,   163,   164,   219,   159,   191,   188,   289,
     159,   182,   159,   163,   182,   188
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
     408,   408,   408,   409,   409,   409,   410,   410,   411,   411,
     412,   412,   413,   414,   415,   415,   415,   415,   415,   415,
     415,   415,   415,   415,   415,   416,   415,   417,   415,   418,
     415,   419,   415,   420,   415,   415,   421,   421,   421,   422,
     422,   423,   423,   423,   423,   423,   423,   423,   423,   423,
     423,   424,   424,   424,   424,   425,   426,   426,   427,   427,
     428,   428,   429,   429,   429,   430,   430,   431,   431,   431,
     432,   432,   432,   433,   433,   433,   434,   434,   434,   434,
     435,   435,   435,   435,   436,   436,   436,   436,   436,   436,
     436,   437,   437,   437,   437,   438,   438,   438,   439,   439,
     439,   439,   439,   440,   440,   440,   440,   441,   441,   441,
     441,   441,   441,   442,   442,   442,   443,   443,   443,   443,
     443,   444,   444,   444,   444,   445,   445,   445,   445,   445,
     445,   446,   446,   447,   447,   447,   447,   448,   448,   448,
     448,   449,   449,   449,   449,   449,   449,   449,   450,   450,
     450,   450,   451,   451,   451,   452,   452,   452,   452,   452,
     453,   453,   453,   453,   454,   454,   454,   454,   454,   454,
     455,   455,   455,   455,   455,   456,   456,   456,   457,   457,
     457,   457,   458,   458,   458,   459,   459,   459,   459,   459,
     460,   460,   461,   461,   461,   462,   462,   463,   463,   464,
     464,   464,   465,   465,   465,   465,   465,   466,   466,   466,
     466,   467,   467,   467,   468,   468,   468,   468,   468,   469,
     469,   469,   469,   469,   469,   470,   470,   471,   471,   471,
     471,   472,   472,   473,   473,   473,   473,   474,   474,   474,
     474,   474,   475,   475,   475,   475,   476,   476,   476,   477,
     477,   477,   478,   478,   478,   478,   478,   478,   479,   479,
     479,   480,   480,   480,   480,   480,   481,   481,   481,   481,
     482,   482,   483,   483,   483,   484,   484,   485,   485,   485,
     485,   485,   485,   486,   486,   486,   486,   486,   486,   486,
     486,   486,   486,   487,   487,   487,   487,   488,   488,   488,
     489,   489,   490,   490,   490,   490,   490,   490,   491,   491,
     491,   491,   491,   491,   492,   492,   492,   493,   493,   493,
     494,   494,   495,   495
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
       1,     1,     3,     2,     3,     1,     0,     1,     3,     4,
       0,     1,     0,     0,     1,     1,     2,     2,     2,     2,
       2,     2,     1,     2,     5,     0,     6,     0,     8,     0,
       7,     0,     7,     0,     8,     1,     1,     2,     3,     0,
       5,     3,     4,     4,     4,     4,     5,     5,     5,     5,
       6,     1,     1,     1,     1,     3,     0,     5,     0,     1,
       1,     2,     6,     4,     4,     1,     3,     0,     1,     4,
       1,     1,     1,     1,     2,     3,     2,     1,     2,     2,
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
#line 644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 8793 "Parser/parser.cc"
    break;

  case 3:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8799 "Parser/parser.cc"
    break;

  case 4:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8805 "Parser/parser.cc"
    break;

  case 5:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8811 "Parser/parser.cc"
    break;

  case 6:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8817 "Parser/parser.cc"
    break;

  case 7:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8823 "Parser/parser.cc"
    break;

  case 8:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8829 "Parser/parser.cc"
    break;

  case 20:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8835 "Parser/parser.cc"
    break;

  case 24:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8841 "Parser/parser.cc"
    break;

  case 25:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8847 "Parser/parser.cc"
    break;

  case 26:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8857 "Parser/parser.cc"
    break;

  case 27:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8863 "Parser/parser.cc"
    break;

  case 28:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8869 "Parser/parser.cc"
    break;

  case 29:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8875 "Parser/parser.cc"
    break;

  case 31:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8881 "Parser/parser.cc"
    break;

  case 32:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8887 "Parser/parser.cc"
    break;

  case 33:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8893 "Parser/parser.cc"
    break;

  case 34:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8899 "Parser/parser.cc"
    break;

  case 35:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8909 "Parser/parser.cc"
    break;

  case 36:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8915 "Parser/parser.cc"
    break;

  case 37:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8921 "Parser/parser.cc"
    break;

  case 38:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8927 "Parser/parser.cc"
    break;

  case 39:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8933 "Parser/parser.cc"
    break;

  case 40:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8939 "Parser/parser.cc"
    break;

  case 41:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8945 "Parser/parser.cc"
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
#line 8957 "Parser/parser.cc"
    break;

  case 44:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8966 "Parser/parser.cc"
    break;

  case 45:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8972 "Parser/parser.cc"
    break;

  case 47:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8978 "Parser/parser.cc"
    break;

  case 48:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8984 "Parser/parser.cc"
    break;

  case 49:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8990 "Parser/parser.cc"
    break;

  case 50:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8996 "Parser/parser.cc"
    break;

  case 51:
#line 786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 9006 "Parser/parser.cc"
    break;

  case 52:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9012 "Parser/parser.cc"
    break;

  case 53:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_va_arg( yylloc, (yyvsp[-4].expr), ( (yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl) ) ) ); }
#line 9018 "Parser/parser.cc"
    break;

  case 54:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9024 "Parser/parser.cc"
    break;

  case 55:
#line 798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9030 "Parser/parser.cc"
    break;

  case 56:
#line 800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9036 "Parser/parser.cc"
    break;

  case 57:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9042 "Parser/parser.cc"
    break;

  case 58:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9048 "Parser/parser.cc"
    break;

  case 59:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9054 "Parser/parser.cc"
    break;

  case 60:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9060 "Parser/parser.cc"
    break;

  case 61:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 9066 "Parser/parser.cc"
    break;

  case 62:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9072 "Parser/parser.cc"
    break;

  case 63:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9078 "Parser/parser.cc"
    break;

  case 64:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9084 "Parser/parser.cc"
    break;

  case 65:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 9090 "Parser/parser.cc"
    break;

  case 66:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 9096 "Parser/parser.cc"
    break;

  case 67:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 9102 "Parser/parser.cc"
    break;

  case 68:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 9108 "Parser/parser.cc"
    break;

  case 69:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 9118 "Parser/parser.cc"
    break;

  case 71:
#line 854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9124 "Parser/parser.cc"
    break;

  case 73:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9130 "Parser/parser.cc"
    break;

  case 74:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9136 "Parser/parser.cc"
    break;

  case 75:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9142 "Parser/parser.cc"
    break;

  case 76:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9148 "Parser/parser.cc"
    break;

  case 77:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9154 "Parser/parser.cc"
    break;

  case 78:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9160 "Parser/parser.cc"
    break;

  case 79:
#line 875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 9166 "Parser/parser.cc"
    break;

  case 80:
#line 877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 9172 "Parser/parser.cc"
    break;

  case 81:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 9180 "Parser/parser.cc"
    break;

  case 82:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9186 "Parser/parser.cc"
    break;

  case 83:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 9195 "Parser/parser.cc"
    break;

  case 86:
#line 900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 9201 "Parser/parser.cc"
    break;

  case 87:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 9207 "Parser/parser.cc"
    break;

  case 88:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9227 "Parser/parser.cc"
    break;

  case 89:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 9233 "Parser/parser.cc"
    break;

  case 90:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 9239 "Parser/parser.cc"
    break;

  case 91:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 9245 "Parser/parser.cc"
    break;

  case 92:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9251 "Parser/parser.cc"
    break;

  case 93:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9257 "Parser/parser.cc"
    break;

  case 94:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9263 "Parser/parser.cc"
    break;

  case 95:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9269 "Parser/parser.cc"
    break;

  case 96:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9275 "Parser/parser.cc"
    break;

  case 97:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9281 "Parser/parser.cc"
    break;

  case 98:
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 9287 "Parser/parser.cc"
    break;

  case 99:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9296 "Parser/parser.cc"
    break;

  case 100:
#line 952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9302 "Parser/parser.cc"
    break;

  case 101:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9308 "Parser/parser.cc"
    break;

  case 102:
#line 958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9314 "Parser/parser.cc"
    break;

  case 103:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9320 "Parser/parser.cc"
    break;

  case 104:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9326 "Parser/parser.cc"
    break;

  case 105:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9332 "Parser/parser.cc"
    break;

  case 106:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9338 "Parser/parser.cc"
    break;

  case 107:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9344 "Parser/parser.cc"
    break;

  case 108:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9350 "Parser/parser.cc"
    break;

  case 110:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9356 "Parser/parser.cc"
    break;

  case 111:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9362 "Parser/parser.cc"
    break;

  case 112:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9368 "Parser/parser.cc"
    break;

  case 113:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9374 "Parser/parser.cc"
    break;

  case 114:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9380 "Parser/parser.cc"
    break;

  case 115:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9386 "Parser/parser.cc"
    break;

  case 116:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9392 "Parser/parser.cc"
    break;

  case 117:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9398 "Parser/parser.cc"
    break;

  case 125:
#line 1008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9404 "Parser/parser.cc"
    break;

  case 127:
#line 1014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9410 "Parser/parser.cc"
    break;

  case 128:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9416 "Parser/parser.cc"
    break;

  case 129:
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9422 "Parser/parser.cc"
    break;

  case 131:
#line 1024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9428 "Parser/parser.cc"
    break;

  case 132:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9434 "Parser/parser.cc"
    break;

  case 134:
#line 1032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9440 "Parser/parser.cc"
    break;

  case 135:
#line 1034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9446 "Parser/parser.cc"
    break;

  case 137:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9452 "Parser/parser.cc"
    break;

  case 138:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9458 "Parser/parser.cc"
    break;

  case 139:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9464 "Parser/parser.cc"
    break;

  case 140:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9470 "Parser/parser.cc"
    break;

  case 142:
#line 1052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9476 "Parser/parser.cc"
    break;

  case 143:
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9482 "Parser/parser.cc"
    break;

  case 145:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9488 "Parser/parser.cc"
    break;

  case 147:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9494 "Parser/parser.cc"
    break;

  case 149:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9500 "Parser/parser.cc"
    break;

  case 151:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9506 "Parser/parser.cc"
    break;

  case 153:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9512 "Parser/parser.cc"
    break;

  case 155:
#line 1090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9518 "Parser/parser.cc"
    break;

  case 156:
#line 1092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9524 "Parser/parser.cc"
    break;

  case 158:
#line 1101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9530 "Parser/parser.cc"
    break;

  case 161:
#line 1109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9536 "Parser/parser.cc"
    break;

  case 162:
#line 1115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 9542 "Parser/parser.cc"
    break;

  case 163:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 9548 "Parser/parser.cc"
    break;

  case 166:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 9560 "Parser/parser.cc"
    break;

  case 167:
#line 1134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9566 "Parser/parser.cc"
    break;

  case 168:
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9572 "Parser/parser.cc"
    break;

  case 172:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9578 "Parser/parser.cc"
    break;

  case 173:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9584 "Parser/parser.cc"
    break;

  case 174:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9590 "Parser/parser.cc"
    break;

  case 175:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9596 "Parser/parser.cc"
    break;

  case 176:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9602 "Parser/parser.cc"
    break;

  case 177:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9608 "Parser/parser.cc"
    break;

  case 178:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9614 "Parser/parser.cc"
    break;

  case 179:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9620 "Parser/parser.cc"
    break;

  case 180:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9626 "Parser/parser.cc"
    break;

  case 181:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9632 "Parser/parser.cc"
    break;

  case 182:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9638 "Parser/parser.cc"
    break;

  case 183:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9644 "Parser/parser.cc"
    break;

  case 184:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9650 "Parser/parser.cc"
    break;

  case 185:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9656 "Parser/parser.cc"
    break;

  case 186:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9662 "Parser/parser.cc"
    break;

  case 188:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9668 "Parser/parser.cc"
    break;

  case 189:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9674 "Parser/parser.cc"
    break;

  case 190:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9680 "Parser/parser.cc"
    break;

  case 192:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9686 "Parser/parser.cc"
    break;

  case 193:
#line 1198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9692 "Parser/parser.cc"
    break;

  case 208:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9698 "Parser/parser.cc"
    break;

  case 210:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9704 "Parser/parser.cc"
    break;

  case 211:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9710 "Parser/parser.cc"
    break;

  case 212:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9721 "Parser/parser.cc"
    break;

  case 213:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9727 "Parser/parser.cc"
    break;

  case 214:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9733 "Parser/parser.cc"
    break;

  case 216:
#line 1251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9739 "Parser/parser.cc"
    break;

  case 217:
#line 1256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9745 "Parser/parser.cc"
    break;

  case 218:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9751 "Parser/parser.cc"
    break;

  case 219:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9757 "Parser/parser.cc"
    break;

  case 220:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9763 "Parser/parser.cc"
    break;

  case 223:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9769 "Parser/parser.cc"
    break;

  case 224:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9776 "Parser/parser.cc"
    break;

  case 225:
#line 1277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9782 "Parser/parser.cc"
    break;

  case 226:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9788 "Parser/parser.cc"
    break;

  case 227:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9794 "Parser/parser.cc"
    break;

  case 228:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9800 "Parser/parser.cc"
    break;

  case 229:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9814 "Parser/parser.cc"
    break;

  case 230:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9820 "Parser/parser.cc"
    break;

  case 231:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9826 "Parser/parser.cc"
    break;

  case 232:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9835 "Parser/parser.cc"
    break;

  case 233:
#line 1332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9841 "Parser/parser.cc"
    break;

  case 234:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9847 "Parser/parser.cc"
    break;

  case 235:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9853 "Parser/parser.cc"
    break;

  case 236:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9859 "Parser/parser.cc"
    break;

  case 237:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9865 "Parser/parser.cc"
    break;

  case 238:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9871 "Parser/parser.cc"
    break;

  case 239:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9877 "Parser/parser.cc"
    break;

  case 241:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9883 "Parser/parser.cc"
    break;

  case 242:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9889 "Parser/parser.cc"
    break;

  case 243:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9895 "Parser/parser.cc"
    break;

  case 244:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9901 "Parser/parser.cc"
    break;

  case 245:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9907 "Parser/parser.cc"
    break;

  case 246:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9913 "Parser/parser.cc"
    break;

  case 247:
#line 1371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9919 "Parser/parser.cc"
    break;

  case 249:
#line 1376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9925 "Parser/parser.cc"
    break;

  case 250:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9931 "Parser/parser.cc"
    break;

  case 251:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9937 "Parser/parser.cc"
    break;

  case 253:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9943 "Parser/parser.cc"
    break;

  case 254:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9949 "Parser/parser.cc"
    break;

  case 255:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9955 "Parser/parser.cc"
    break;

  case 256:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9964 "Parser/parser.cc"
    break;

  case 257:
#line 1405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9970 "Parser/parser.cc"
    break;

  case 258:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9976 "Parser/parser.cc"
    break;

  case 259:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9982 "Parser/parser.cc"
    break;

  case 260:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9991 "Parser/parser.cc"
    break;

  case 261:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9997 "Parser/parser.cc"
    break;

  case 262:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10003 "Parser/parser.cc"
    break;

  case 263:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10009 "Parser/parser.cc"
    break;

  case 264:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10018 "Parser/parser.cc"
    break;

  case 265:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10024 "Parser/parser.cc"
    break;

  case 266:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10030 "Parser/parser.cc"
    break;

  case 268:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10049 "Parser/parser.cc"
    break;

  case 269:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10055 "Parser/parser.cc"
    break;

  case 270:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 10063 "Parser/parser.cc"
    break;

  case 271:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10069 "Parser/parser.cc"
    break;

  case 272:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 10075 "Parser/parser.cc"
    break;

  case 273:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10081 "Parser/parser.cc"
    break;

  case 274:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 10087 "Parser/parser.cc"
    break;

  case 275:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10093 "Parser/parser.cc"
    break;

  case 276:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10099 "Parser/parser.cc"
    break;

  case 277:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10108 "Parser/parser.cc"
    break;

  case 278:
#line 1483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 10117 "Parser/parser.cc"
    break;

  case 279:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10123 "Parser/parser.cc"
    break;

  case 280:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10132 "Parser/parser.cc"
    break;

  case 281:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 10141 "Parser/parser.cc"
    break;

  case 282:
#line 1500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 10147 "Parser/parser.cc"
    break;

  case 283:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 10153 "Parser/parser.cc"
    break;

  case 284:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 10159 "Parser/parser.cc"
    break;

  case 285:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 10165 "Parser/parser.cc"
    break;

  case 286:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 10171 "Parser/parser.cc"
    break;

  case 287:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 10177 "Parser/parser.cc"
    break;

  case 288:
#line 1515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10183 "Parser/parser.cc"
    break;

  case 289:
#line 1518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10189 "Parser/parser.cc"
    break;

  case 290:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10198 "Parser/parser.cc"
    break;

  case 291:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10208 "Parser/parser.cc"
    break;

  case 292:
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10214 "Parser/parser.cc"
    break;

  case 293:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10220 "Parser/parser.cc"
    break;

  case 294:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10229 "Parser/parser.cc"
    break;

  case 295:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10239 "Parser/parser.cc"
    break;

  case 296:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10245 "Parser/parser.cc"
    break;

  case 297:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10254 "Parser/parser.cc"
    break;

  case 298:
#line 1554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10264 "Parser/parser.cc"
    break;

  case 299:
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10270 "Parser/parser.cc"
    break;

  case 300:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 10276 "Parser/parser.cc"
    break;

  case 301:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10282 "Parser/parser.cc"
    break;

  case 302:
#line 1568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10288 "Parser/parser.cc"
    break;

  case 303:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10297 "Parser/parser.cc"
    break;

  case 304:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10307 "Parser/parser.cc"
    break;

  case 305:
#line 1582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10313 "Parser/parser.cc"
    break;

  case 306:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10322 "Parser/parser.cc"
    break;

  case 307:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10332 "Parser/parser.cc"
    break;

  case 308:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10338 "Parser/parser.cc"
    break;

  case 309:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10347 "Parser/parser.cc"
    break;

  case 310:
#line 1602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10357 "Parser/parser.cc"
    break;

  case 311:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10363 "Parser/parser.cc"
    break;

  case 312:
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 10371 "Parser/parser.cc"
    break;

  case 313:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); (yyval.forctl) = nullptr;
				(yyvsp[-1].oper) = OperKinds::GEThan;
			} // if
			(yyval.forctl) = enumRangeCtrl( (yyvsp[-3].expr), (yyvsp[-1].oper), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 10383 "Parser/parser.cc"
    break;

  case 314:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 10392 "Parser/parser.cc"
    break;

  case 315:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false );
		}
#line 10401 "Parser/parser.cc"
    break;

  case 316:
#line 1636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 10410 "Parser/parser.cc"
    break;

  case 317:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10416 "Parser/parser.cc"
    break;

  case 318:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10422 "Parser/parser.cc"
    break;

  case 319:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10428 "Parser/parser.cc"
    break;

  case 320:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10434 "Parser/parser.cc"
    break;

  case 321:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10440 "Parser/parser.cc"
    break;

  case 322:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10446 "Parser/parser.cc"
    break;

  case 323:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10452 "Parser/parser.cc"
    break;

  case 325:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10458 "Parser/parser.cc"
    break;

  case 326:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10464 "Parser/parser.cc"
    break;

  case 327:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10470 "Parser/parser.cc"
    break;

  case 328:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10476 "Parser/parser.cc"
    break;

  case 329:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10482 "Parser/parser.cc"
    break;

  case 330:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10488 "Parser/parser.cc"
    break;

  case 331:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10494 "Parser/parser.cc"
    break;

  case 332:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10500 "Parser/parser.cc"
    break;

  case 333:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10506 "Parser/parser.cc"
    break;

  case 334:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10512 "Parser/parser.cc"
    break;

  case 335:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10518 "Parser/parser.cc"
    break;

  case 336:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10524 "Parser/parser.cc"
    break;

  case 337:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10530 "Parser/parser.cc"
    break;

  case 338:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10536 "Parser/parser.cc"
    break;

  case 339:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10542 "Parser/parser.cc"
    break;

  case 340:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10548 "Parser/parser.cc"
    break;

  case 341:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10554 "Parser/parser.cc"
    break;

  case 342:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10560 "Parser/parser.cc"
    break;

  case 343:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10566 "Parser/parser.cc"
    break;

  case 344:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10572 "Parser/parser.cc"
    break;

  case 345:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10578 "Parser/parser.cc"
    break;

  case 346:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10584 "Parser/parser.cc"
    break;

  case 349:
#line 1732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10590 "Parser/parser.cc"
    break;

  case 350:
#line 1738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10599 "Parser/parser.cc"
    break;

  case 351:
#line 1745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10605 "Parser/parser.cc"
    break;

  case 352:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10611 "Parser/parser.cc"
    break;

  case 355:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10617 "Parser/parser.cc"
    break;

  case 356:
#line 1761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10623 "Parser/parser.cc"
    break;

  case 359:
#line 1770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10629 "Parser/parser.cc"
    break;

  case 360:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10635 "Parser/parser.cc"
    break;

  case 361:
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10641 "Parser/parser.cc"
    break;

  case 362:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10647 "Parser/parser.cc"
    break;

  case 363:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10653 "Parser/parser.cc"
    break;

  case 364:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10659 "Parser/parser.cc"
    break;

  case 365:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10665 "Parser/parser.cc"
    break;

  case 366:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10671 "Parser/parser.cc"
    break;

  case 367:
#line 1794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10677 "Parser/parser.cc"
    break;

  case 370:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10683 "Parser/parser.cc"
    break;

  case 371:
#line 1809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10689 "Parser/parser.cc"
    break;

  case 372:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10695 "Parser/parser.cc"
    break;

  case 373:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10701 "Parser/parser.cc"
    break;

  case 374:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10707 "Parser/parser.cc"
    break;

  case 375:
#line 1823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10713 "Parser/parser.cc"
    break;

  case 376:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10719 "Parser/parser.cc"
    break;

  case 377:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10725 "Parser/parser.cc"
    break;

  case 378:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10731 "Parser/parser.cc"
    break;

  case 379:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10737 "Parser/parser.cc"
    break;

  case 380:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10743 "Parser/parser.cc"
    break;

  case 381:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10749 "Parser/parser.cc"
    break;

  case 382:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10755 "Parser/parser.cc"
    break;

  case 383:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10761 "Parser/parser.cc"
    break;

  case 384:
#line 1856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10767 "Parser/parser.cc"
    break;

  case 385:
#line 1858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-6].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10773 "Parser/parser.cc"
    break;

  case 386:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10779 "Parser/parser.cc"
    break;

  case 387:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10785 "Parser/parser.cc"
    break;

  case 388:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10791 "Parser/parser.cc"
    break;

  case 389:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10797 "Parser/parser.cc"
    break;

  case 390:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10803 "Parser/parser.cc"
    break;

  case 391:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10809 "Parser/parser.cc"
    break;

  case 392:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10815 "Parser/parser.cc"
    break;

  case 394:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10821 "Parser/parser.cc"
    break;

  case 395:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10827 "Parser/parser.cc"
    break;

  case 396:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10833 "Parser/parser.cc"
    break;

  case 401:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10839 "Parser/parser.cc"
    break;

  case 402:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10845 "Parser/parser.cc"
    break;

  case 403:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10851 "Parser/parser.cc"
    break;

  case 404:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10857 "Parser/parser.cc"
    break;

  case 405:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10863 "Parser/parser.cc"
    break;

  case 406:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10869 "Parser/parser.cc"
    break;

  case 407:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10875 "Parser/parser.cc"
    break;

  case 408:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10881 "Parser/parser.cc"
    break;

  case 411:
#line 1928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10887 "Parser/parser.cc"
    break;

  case 412:
#line 1933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10893 "Parser/parser.cc"
    break;

  case 413:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10902 "Parser/parser.cc"
    break;

  case 414:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10908 "Parser/parser.cc"
    break;

  case 415:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10914 "Parser/parser.cc"
    break;

  case 416:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10920 "Parser/parser.cc"
    break;

  case 417:
#line 1952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10929 "Parser/parser.cc"
    break;

  case 418:
#line 1957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10938 "Parser/parser.cc"
    break;

  case 419:
#line 1967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10944 "Parser/parser.cc"
    break;

  case 422:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10950 "Parser/parser.cc"
    break;

  case 423:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10956 "Parser/parser.cc"
    break;

  case 425:
#line 1985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10962 "Parser/parser.cc"
    break;

  case 426:
#line 1987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10968 "Parser/parser.cc"
    break;

  case 436:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 10974 "Parser/parser.cc"
    break;

  case 437:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10980 "Parser/parser.cc"
    break;

  case 441:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10986 "Parser/parser.cc"
    break;

  case 443:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10992 "Parser/parser.cc"
    break;

  case 444:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10998 "Parser/parser.cc"
    break;

  case 445:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 11004 "Parser/parser.cc"
    break;

  case 446:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11010 "Parser/parser.cc"
    break;

  case 447:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11016 "Parser/parser.cc"
    break;

  case 448:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11022 "Parser/parser.cc"
    break;

  case 449:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11028 "Parser/parser.cc"
    break;

  case 450:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11034 "Parser/parser.cc"
    break;

  case 452:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11040 "Parser/parser.cc"
    break;

  case 453:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11046 "Parser/parser.cc"
    break;

  case 454:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11052 "Parser/parser.cc"
    break;

  case 455:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 11063 "Parser/parser.cc"
    break;

  case 456:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11069 "Parser/parser.cc"
    break;

  case 457:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11075 "Parser/parser.cc"
    break;

  case 458:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11081 "Parser/parser.cc"
    break;

  case 459:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11087 "Parser/parser.cc"
    break;

  case 460:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11093 "Parser/parser.cc"
    break;

  case 461:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 11099 "Parser/parser.cc"
    break;

  case 462:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 11108 "Parser/parser.cc"
    break;

  case 463:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 11117 "Parser/parser.cc"
    break;

  case 464:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 11126 "Parser/parser.cc"
    break;

  case 465:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 11137 "Parser/parser.cc"
    break;

  case 466:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 11146 "Parser/parser.cc"
    break;

  case 467:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11152 "Parser/parser.cc"
    break;

  case 468:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11158 "Parser/parser.cc"
    break;

  case 469:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11164 "Parser/parser.cc"
    break;

  case 470:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 11172 "Parser/parser.cc"
    break;

  case 471:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 11180 "Parser/parser.cc"
    break;

  case 472:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 11186 "Parser/parser.cc"
    break;

  case 475:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11201 "Parser/parser.cc"
    break;

  case 476:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11207 "Parser/parser.cc"
    break;

  case 477:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11213 "Parser/parser.cc"
    break;

  case 478:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 11219 "Parser/parser.cc"
    break;

  case 479:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 11225 "Parser/parser.cc"
    break;

  case 480:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 11231 "Parser/parser.cc"
    break;

  case 486:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 11241 "Parser/parser.cc"
    break;

  case 499:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11247 "Parser/parser.cc"
    break;

  case 502:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11253 "Parser/parser.cc"
    break;

  case 503:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11259 "Parser/parser.cc"
    break;

  case 505:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 11265 "Parser/parser.cc"
    break;

  case 506:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 11271 "Parser/parser.cc"
    break;

  case 507:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 11277 "Parser/parser.cc"
    break;

  case 508:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 11283 "Parser/parser.cc"
    break;

  case 509:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 11289 "Parser/parser.cc"
    break;

  case 510:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11295 "Parser/parser.cc"
    break;

  case 512:
#line 2305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11301 "Parser/parser.cc"
    break;

  case 513:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11307 "Parser/parser.cc"
    break;

  case 515:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11313 "Parser/parser.cc"
    break;

  case 516:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 11319 "Parser/parser.cc"
    break;

  case 517:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 11325 "Parser/parser.cc"
    break;

  case 518:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 11331 "Parser/parser.cc"
    break;

  case 519:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 11337 "Parser/parser.cc"
    break;

  case 520:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 11343 "Parser/parser.cc"
    break;

  case 521:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 11349 "Parser/parser.cc"
    break;

  case 522:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 11355 "Parser/parser.cc"
    break;

  case 523:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 11361 "Parser/parser.cc"
    break;

  case 524:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11367 "Parser/parser.cc"
    break;

  case 525:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11373 "Parser/parser.cc"
    break;

  case 526:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 11379 "Parser/parser.cc"
    break;

  case 527:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11385 "Parser/parser.cc"
    break;

  case 528:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11391 "Parser/parser.cc"
    break;

  case 529:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11397 "Parser/parser.cc"
    break;

  case 530:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11403 "Parser/parser.cc"
    break;

  case 531:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11409 "Parser/parser.cc"
    break;

  case 532:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11415 "Parser/parser.cc"
    break;

  case 533:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11421 "Parser/parser.cc"
    break;

  case 534:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11427 "Parser/parser.cc"
    break;

  case 535:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11433 "Parser/parser.cc"
    break;

  case 536:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11439 "Parser/parser.cc"
    break;

  case 537:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11445 "Parser/parser.cc"
    break;

  case 538:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11451 "Parser/parser.cc"
    break;

  case 539:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11457 "Parser/parser.cc"
    break;

  case 540:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11463 "Parser/parser.cc"
    break;

  case 541:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11469 "Parser/parser.cc"
    break;

  case 542:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11475 "Parser/parser.cc"
    break;

  case 543:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11481 "Parser/parser.cc"
    break;

  case 544:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11487 "Parser/parser.cc"
    break;

  case 545:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11493 "Parser/parser.cc"
    break;

  case 546:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11499 "Parser/parser.cc"
    break;

  case 547:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11505 "Parser/parser.cc"
    break;

  case 548:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11511 "Parser/parser.cc"
    break;

  case 549:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11517 "Parser/parser.cc"
    break;

  case 550:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11523 "Parser/parser.cc"
    break;

  case 551:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11529 "Parser/parser.cc"
    break;

  case 552:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11535 "Parser/parser.cc"
    break;

  case 554:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11541 "Parser/parser.cc"
    break;

  case 556:
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11547 "Parser/parser.cc"
    break;

  case 557:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11553 "Parser/parser.cc"
    break;

  case 558:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11559 "Parser/parser.cc"
    break;

  case 560:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11565 "Parser/parser.cc"
    break;

  case 561:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11571 "Parser/parser.cc"
    break;

  case 562:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11577 "Parser/parser.cc"
    break;

  case 563:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11583 "Parser/parser.cc"
    break;

  case 565:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11589 "Parser/parser.cc"
    break;

  case 567:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11595 "Parser/parser.cc"
    break;

  case 568:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11601 "Parser/parser.cc"
    break;

  case 569:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11607 "Parser/parser.cc"
    break;

  case 570:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11613 "Parser/parser.cc"
    break;

  case 571:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11619 "Parser/parser.cc"
    break;

  case 572:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11625 "Parser/parser.cc"
    break;

  case 573:
#line 2463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11631 "Parser/parser.cc"
    break;

  case 574:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11637 "Parser/parser.cc"
    break;

  case 575:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11643 "Parser/parser.cc"
    break;

  case 577:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11649 "Parser/parser.cc"
    break;

  case 578:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11655 "Parser/parser.cc"
    break;

  case 579:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11661 "Parser/parser.cc"
    break;

  case 581:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11667 "Parser/parser.cc"
    break;

  case 582:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11673 "Parser/parser.cc"
    break;

  case 583:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11682 "Parser/parser.cc"
    break;

  case 585:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11688 "Parser/parser.cc"
    break;

  case 586:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11694 "Parser/parser.cc"
    break;

  case 587:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11700 "Parser/parser.cc"
    break;

  case 589:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11706 "Parser/parser.cc"
    break;

  case 590:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11712 "Parser/parser.cc"
    break;

  case 592:
#line 2514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11718 "Parser/parser.cc"
    break;

  case 593:
#line 2516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11724 "Parser/parser.cc"
    break;

  case 594:
#line 2518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11730 "Parser/parser.cc"
    break;

  case 595:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11736 "Parser/parser.cc"
    break;

  case 596:
#line 2525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11742 "Parser/parser.cc"
    break;

  case 597:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11748 "Parser/parser.cc"
    break;

  case 598:
#line 2532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11754 "Parser/parser.cc"
    break;

  case 599:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11760 "Parser/parser.cc"
    break;

  case 600:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11766 "Parser/parser.cc"
    break;

  case 602:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11772 "Parser/parser.cc"
    break;

  case 603:
#line 2541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11778 "Parser/parser.cc"
    break;

  case 604:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11784 "Parser/parser.cc"
    break;

  case 605:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11790 "Parser/parser.cc"
    break;

  case 606:
#line 2550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11796 "Parser/parser.cc"
    break;

  case 611:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11802 "Parser/parser.cc"
    break;

  case 612:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11808 "Parser/parser.cc"
    break;

  case 613:
#line 2571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11817 "Parser/parser.cc"
    break;

  case 614:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11825 "Parser/parser.cc"
    break;

  case 615:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11834 "Parser/parser.cc"
    break;

  case 616:
#line 2585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11843 "Parser/parser.cc"
    break;

  case 617:
#line 2590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11852 "Parser/parser.cc"
    break;

  case 618:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11861 "Parser/parser.cc"
    break;

  case 620:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11867 "Parser/parser.cc"
    break;

  case 621:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11873 "Parser/parser.cc"
    break;

  case 622:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11883 "Parser/parser.cc"
    break;

  case 623:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11902 "Parser/parser.cc"
    break;

  case 626:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11908 "Parser/parser.cc"
    break;

  case 627:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11914 "Parser/parser.cc"
    break;

  case 628:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11920 "Parser/parser.cc"
    break;

  case 629:
#line 2649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11926 "Parser/parser.cc"
    break;

  case 630:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11932 "Parser/parser.cc"
    break;

  case 631:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11938 "Parser/parser.cc"
    break;

  case 632:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11947 "Parser/parser.cc"
    break;

  case 633:
#line 2660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11953 "Parser/parser.cc"
    break;

  case 634:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11962 "Parser/parser.cc"
    break;

  case 635:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11968 "Parser/parser.cc"
    break;

  case 636:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11977 "Parser/parser.cc"
    break;

  case 637:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11983 "Parser/parser.cc"
    break;

  case 638:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11989 "Parser/parser.cc"
    break;

  case 639:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 12002 "Parser/parser.cc"
    break;

  case 640:
#line 2693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 12011 "Parser/parser.cc"
    break;

  case 641:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 12017 "Parser/parser.cc"
    break;

  case 642:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12023 "Parser/parser.cc"
    break;

  case 643:
#line 2702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 12036 "Parser/parser.cc"
    break;

  case 644:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12042 "Parser/parser.cc"
    break;

  case 647:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 12048 "Parser/parser.cc"
    break;

  case 648:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12054 "Parser/parser.cc"
    break;

  case 651:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12060 "Parser/parser.cc"
    break;

  case 653:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 12066 "Parser/parser.cc"
    break;

  case 654:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 12072 "Parser/parser.cc"
    break;

  case 655:
#line 2735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12078 "Parser/parser.cc"
    break;

  case 656:
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12084 "Parser/parser.cc"
    break;

  case 657:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12090 "Parser/parser.cc"
    break;

  case 658:
#line 2746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12096 "Parser/parser.cc"
    break;

  case 660:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 12102 "Parser/parser.cc"
    break;

  case 662:
#line 2760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 12108 "Parser/parser.cc"
    break;

  case 663:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12114 "Parser/parser.cc"
    break;

  case 665:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 12120 "Parser/parser.cc"
    break;

  case 666:
#line 2774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12126 "Parser/parser.cc"
    break;

  case 668:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 12132 "Parser/parser.cc"
    break;

  case 669:
#line 2788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 12143 "Parser/parser.cc"
    break;

  case 670:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 12157 "Parser/parser.cc"
    break;

  case 671:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 12163 "Parser/parser.cc"
    break;

  case 672:
#line 2809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 12169 "Parser/parser.cc"
    break;

  case 673:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 12175 "Parser/parser.cc"
    break;

  case 674:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 12186 "Parser/parser.cc"
    break;

  case 675:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 12192 "Parser/parser.cc"
    break;

  case 676:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12198 "Parser/parser.cc"
    break;

  case 678:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12204 "Parser/parser.cc"
    break;

  case 679:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12210 "Parser/parser.cc"
    break;

  case 680:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 12216 "Parser/parser.cc"
    break;

  case 681:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 12222 "Parser/parser.cc"
    break;

  case 682:
#line 2844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12231 "Parser/parser.cc"
    break;

  case 683:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12240 "Parser/parser.cc"
    break;

  case 684:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 12246 "Parser/parser.cc"
    break;

  case 685:
#line 2859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 12252 "Parser/parser.cc"
    break;

  case 686:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 12262 "Parser/parser.cc"
    break;

  case 687:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 12268 "Parser/parser.cc"
    break;

  case 688:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 12274 "Parser/parser.cc"
    break;

  case 690:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 12280 "Parser/parser.cc"
    break;

  case 691:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12286 "Parser/parser.cc"
    break;

  case 692:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12292 "Parser/parser.cc"
    break;

  case 693:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12298 "Parser/parser.cc"
    break;

  case 694:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 12304 "Parser/parser.cc"
    break;

  case 695:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12310 "Parser/parser.cc"
    break;

  case 697:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12316 "Parser/parser.cc"
    break;

  case 700:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12322 "Parser/parser.cc"
    break;

  case 701:
#line 2905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12328 "Parser/parser.cc"
    break;

  case 702:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 12334 "Parser/parser.cc"
    break;

  case 703:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12340 "Parser/parser.cc"
    break;

  case 706:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12346 "Parser/parser.cc"
    break;

  case 707:
#line 2918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12352 "Parser/parser.cc"
    break;

  case 708:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12358 "Parser/parser.cc"
    break;

  case 710:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12364 "Parser/parser.cc"
    break;

  case 711:
#line 2930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12370 "Parser/parser.cc"
    break;

  case 712:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12376 "Parser/parser.cc"
    break;

  case 714:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12382 "Parser/parser.cc"
    break;

  case 715:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12388 "Parser/parser.cc"
    break;

  case 716:
#line 2949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12394 "Parser/parser.cc"
    break;

  case 717:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12400 "Parser/parser.cc"
    break;

  case 718:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12406 "Parser/parser.cc"
    break;

  case 720:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12412 "Parser/parser.cc"
    break;

  case 721:
#line 2965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12418 "Parser/parser.cc"
    break;

  case 722:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12424 "Parser/parser.cc"
    break;

  case 727:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12430 "Parser/parser.cc"
    break;

  case 729:
#line 2987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12436 "Parser/parser.cc"
    break;

  case 730:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12442 "Parser/parser.cc"
    break;

  case 733:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12448 "Parser/parser.cc"
    break;

  case 736:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12454 "Parser/parser.cc"
    break;

  case 737:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12460 "Parser/parser.cc"
    break;

  case 738:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12466 "Parser/parser.cc"
    break;

  case 739:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12472 "Parser/parser.cc"
    break;

  case 740:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12478 "Parser/parser.cc"
    break;

  case 741:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12484 "Parser/parser.cc"
    break;

  case 742:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12490 "Parser/parser.cc"
    break;

  case 744:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12496 "Parser/parser.cc"
    break;

  case 745:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12502 "Parser/parser.cc"
    break;

  case 746:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12508 "Parser/parser.cc"
    break;

  case 748:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12514 "Parser/parser.cc"
    break;

  case 750:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12520 "Parser/parser.cc"
    break;

  case 751:
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12526 "Parser/parser.cc"
    break;

  case 752:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12532 "Parser/parser.cc"
    break;

  case 753:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12538 "Parser/parser.cc"
    break;

  case 754:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12544 "Parser/parser.cc"
    break;

  case 755:
#line 3060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12550 "Parser/parser.cc"
    break;

  case 757:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12556 "Parser/parser.cc"
    break;

  case 758:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12562 "Parser/parser.cc"
    break;

  case 759:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12568 "Parser/parser.cc"
    break;

  case 760:
#line 3096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12579 "Parser/parser.cc"
    break;

  case 761:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12585 "Parser/parser.cc"
    break;

  case 762:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12591 "Parser/parser.cc"
    break;

  case 763:
#line 3107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12597 "Parser/parser.cc"
    break;

  case 764:
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12606 "Parser/parser.cc"
    break;

  case 765:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12612 "Parser/parser.cc"
    break;

  case 766:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12622 "Parser/parser.cc"
    break;

  case 767:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12628 "Parser/parser.cc"
    break;

  case 768:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12634 "Parser/parser.cc"
    break;

  case 769:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12640 "Parser/parser.cc"
    break;

  case 770:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12646 "Parser/parser.cc"
    break;

  case 771:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12652 "Parser/parser.cc"
    break;

  case 772:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12658 "Parser/parser.cc"
    break;

  case 773:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12664 "Parser/parser.cc"
    break;

  case 774:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12670 "Parser/parser.cc"
    break;

  case 775:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12676 "Parser/parser.cc"
    break;

  case 778:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12682 "Parser/parser.cc"
    break;

  case 779:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12688 "Parser/parser.cc"
    break;

  case 780:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12694 "Parser/parser.cc"
    break;

  case 781:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12700 "Parser/parser.cc"
    break;

  case 783:
#line 3174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12706 "Parser/parser.cc"
    break;

  case 784:
#line 3176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12712 "Parser/parser.cc"
    break;

  case 785:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12718 "Parser/parser.cc"
    break;

  case 786:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12724 "Parser/parser.cc"
    break;

  case 787:
#line 3185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12730 "Parser/parser.cc"
    break;

  case 788:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12736 "Parser/parser.cc"
    break;

  case 789:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12742 "Parser/parser.cc"
    break;

  case 790:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12751 "Parser/parser.cc"
    break;

  case 791:
#line 3202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12760 "Parser/parser.cc"
    break;

  case 792:
#line 3210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12769 "Parser/parser.cc"
    break;

  case 793:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12775 "Parser/parser.cc"
    break;

  case 794:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-6].tok), (yyvsp[-4].decl), (yyvsp[-1].decl) );
		}
#line 12784 "Parser/parser.cc"
    break;

  case 795:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-3].tok), (yyvsp[-5].decl), (yyvsp[-1].decl) ); }
#line 12790 "Parser/parser.cc"
    break;

  case 797:
#line 3228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12796 "Parser/parser.cc"
    break;

  case 802:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12802 "Parser/parser.cc"
    break;

  case 803:
#line 3246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12808 "Parser/parser.cc"
    break;

  case 804:
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12814 "Parser/parser.cc"
    break;

  case 805:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Possible cause is declaring an aggregate or enumeration type in a trait." ); (yyval.decl) = nullptr; }
#line 12820 "Parser/parser.cc"
    break;

  case 807:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 12826 "Parser/parser.cc"
    break;

  case 808:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12832 "Parser/parser.cc"
    break;

  case 809:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12838 "Parser/parser.cc"
    break;

  case 810:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12844 "Parser/parser.cc"
    break;

  case 812:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12850 "Parser/parser.cc"
    break;

  case 813:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12856 "Parser/parser.cc"
    break;

  case 814:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12862 "Parser/parser.cc"
    break;

  case 815:
#line 3286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12878 "Parser/parser.cc"
    break;

  case 816:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12884 "Parser/parser.cc"
    break;

  case 817:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12890 "Parser/parser.cc"
    break;

  case 818:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12896 "Parser/parser.cc"
    break;

  case 819:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12902 "Parser/parser.cc"
    break;

  case 820:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12908 "Parser/parser.cc"
    break;

  case 821:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12914 "Parser/parser.cc"
    break;

  case 823:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12923 "Parser/parser.cc"
    break;

  case 824:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12929 "Parser/parser.cc"
    break;

  case 825:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12938 "Parser/parser.cc"
    break;

  case 826:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12948 "Parser/parser.cc"
    break;

  case 827:
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12957 "Parser/parser.cc"
    break;

  case 828:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12967 "Parser/parser.cc"
    break;

  case 829:
#line 3341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12978 "Parser/parser.cc"
    break;

  case 830:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12988 "Parser/parser.cc"
    break;

  case 831:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12999 "Parser/parser.cc"
    break;

  case 832:
#line 3361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13009 "Parser/parser.cc"
    break;

  case 833:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 13020 "Parser/parser.cc"
    break;

  case 834:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13030 "Parser/parser.cc"
    break;

  case 835:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13036 "Parser/parser.cc"
    break;

  case 837:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 13042 "Parser/parser.cc"
    break;

  case 838:
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 13048 "Parser/parser.cc"
    break;

  case 839:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 13054 "Parser/parser.cc"
    break;

  case 840:
#line 3400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 13066 "Parser/parser.cc"
    break;

  case 841:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13077 "Parser/parser.cc"
    break;

  case 842:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 13086 "Parser/parser.cc"
    break;

  case 843:
#line 3423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 13095 "Parser/parser.cc"
    break;

  case 844:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13101 "Parser/parser.cc"
    break;

  case 845:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13107 "Parser/parser.cc"
    break;

  case 846:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13113 "Parser/parser.cc"
    break;

  case 847:
#line 3439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 13122 "Parser/parser.cc"
    break;

  case 848:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13128 "Parser/parser.cc"
    break;

  case 849:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13134 "Parser/parser.cc"
    break;

  case 850:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 13140 "Parser/parser.cc"
    break;

  case 855:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 13146 "Parser/parser.cc"
    break;

  case 856:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13152 "Parser/parser.cc"
    break;

  case 857:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 13162 "Parser/parser.cc"
    break;

  case 858:
#line 3483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13168 "Parser/parser.cc"
    break;

  case 861:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13174 "Parser/parser.cc"
    break;

  case 862:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 13180 "Parser/parser.cc"
    break;

  case 863:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13186 "Parser/parser.cc"
    break;

  case 864:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13192 "Parser/parser.cc"
    break;

  case 866:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13198 "Parser/parser.cc"
    break;

  case 867:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13204 "Parser/parser.cc"
    break;

  case 868:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 13210 "Parser/parser.cc"
    break;

  case 869:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13216 "Parser/parser.cc"
    break;

  case 871:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 13222 "Parser/parser.cc"
    break;

  case 872:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 13228 "Parser/parser.cc"
    break;

  case 873:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13234 "Parser/parser.cc"
    break;

  case 874:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13240 "Parser/parser.cc"
    break;

  case 875:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13246 "Parser/parser.cc"
    break;

  case 876:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13252 "Parser/parser.cc"
    break;

  case 878:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13258 "Parser/parser.cc"
    break;

  case 879:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13264 "Parser/parser.cc"
    break;

  case 880:
#line 3577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13270 "Parser/parser.cc"
    break;

  case 881:
#line 3579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13276 "Parser/parser.cc"
    break;

  case 882:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13282 "Parser/parser.cc"
    break;

  case 883:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13288 "Parser/parser.cc"
    break;

  case 884:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13294 "Parser/parser.cc"
    break;

  case 885:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13300 "Parser/parser.cc"
    break;

  case 886:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13306 "Parser/parser.cc"
    break;

  case 887:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13312 "Parser/parser.cc"
    break;

  case 888:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13318 "Parser/parser.cc"
    break;

  case 889:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13324 "Parser/parser.cc"
    break;

  case 890:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13330 "Parser/parser.cc"
    break;

  case 891:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13336 "Parser/parser.cc"
    break;

  case 892:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13342 "Parser/parser.cc"
    break;

  case 893:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13348 "Parser/parser.cc"
    break;

  case 894:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13354 "Parser/parser.cc"
    break;

  case 895:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13360 "Parser/parser.cc"
    break;

  case 897:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13366 "Parser/parser.cc"
    break;

  case 898:
#line 3628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13372 "Parser/parser.cc"
    break;

  case 899:
#line 3630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13378 "Parser/parser.cc"
    break;

  case 900:
#line 3632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13384 "Parser/parser.cc"
    break;

  case 901:
#line 3634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13390 "Parser/parser.cc"
    break;

  case 902:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13396 "Parser/parser.cc"
    break;

  case 903:
#line 3641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13402 "Parser/parser.cc"
    break;

  case 904:
#line 3643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13408 "Parser/parser.cc"
    break;

  case 905:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13414 "Parser/parser.cc"
    break;

  case 906:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13420 "Parser/parser.cc"
    break;

  case 907:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13426 "Parser/parser.cc"
    break;

  case 908:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13432 "Parser/parser.cc"
    break;

  case 909:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13438 "Parser/parser.cc"
    break;

  case 910:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13444 "Parser/parser.cc"
    break;

  case 911:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13450 "Parser/parser.cc"
    break;

  case 912:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13456 "Parser/parser.cc"
    break;

  case 916:
#line 3680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13462 "Parser/parser.cc"
    break;

  case 917:
#line 3682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13468 "Parser/parser.cc"
    break;

  case 918:
#line 3684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13474 "Parser/parser.cc"
    break;

  case 919:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13480 "Parser/parser.cc"
    break;

  case 920:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13486 "Parser/parser.cc"
    break;

  case 921:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13492 "Parser/parser.cc"
    break;

  case 922:
#line 3695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13498 "Parser/parser.cc"
    break;

  case 923:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13504 "Parser/parser.cc"
    break;

  case 924:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13510 "Parser/parser.cc"
    break;

  case 925:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13516 "Parser/parser.cc"
    break;

  case 926:
#line 3706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13522 "Parser/parser.cc"
    break;

  case 927:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13528 "Parser/parser.cc"
    break;

  case 928:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13534 "Parser/parser.cc"
    break;

  case 929:
#line 3712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13540 "Parser/parser.cc"
    break;

  case 930:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13546 "Parser/parser.cc"
    break;

  case 931:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13555 "Parser/parser.cc"
    break;

  case 932:
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13561 "Parser/parser.cc"
    break;

  case 933:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13567 "Parser/parser.cc"
    break;

  case 935:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13573 "Parser/parser.cc"
    break;

  case 936:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13579 "Parser/parser.cc"
    break;

  case 937:
#line 3746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13585 "Parser/parser.cc"
    break;

  case 938:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13591 "Parser/parser.cc"
    break;

  case 939:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13597 "Parser/parser.cc"
    break;

  case 940:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13603 "Parser/parser.cc"
    break;

  case 941:
#line 3757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13609 "Parser/parser.cc"
    break;

  case 942:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13615 "Parser/parser.cc"
    break;

  case 943:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13621 "Parser/parser.cc"
    break;

  case 944:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13627 "Parser/parser.cc"
    break;

  case 945:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13633 "Parser/parser.cc"
    break;

  case 946:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13639 "Parser/parser.cc"
    break;

  case 947:
#line 3769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13645 "Parser/parser.cc"
    break;

  case 948:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13651 "Parser/parser.cc"
    break;

  case 949:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13657 "Parser/parser.cc"
    break;

  case 950:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13663 "Parser/parser.cc"
    break;

  case 951:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13669 "Parser/parser.cc"
    break;

  case 952:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13675 "Parser/parser.cc"
    break;

  case 954:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13681 "Parser/parser.cc"
    break;

  case 955:
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13687 "Parser/parser.cc"
    break;

  case 956:
#line 3799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13693 "Parser/parser.cc"
    break;

  case 957:
#line 3801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13699 "Parser/parser.cc"
    break;

  case 958:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13705 "Parser/parser.cc"
    break;

  case 959:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13711 "Parser/parser.cc"
    break;

  case 960:
#line 3810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13717 "Parser/parser.cc"
    break;

  case 961:
#line 3812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13723 "Parser/parser.cc"
    break;

  case 962:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13729 "Parser/parser.cc"
    break;

  case 963:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13735 "Parser/parser.cc"
    break;

  case 964:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13741 "Parser/parser.cc"
    break;

  case 965:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13747 "Parser/parser.cc"
    break;

  case 966:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13753 "Parser/parser.cc"
    break;

  case 967:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13759 "Parser/parser.cc"
    break;

  case 968:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13765 "Parser/parser.cc"
    break;

  case 969:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13771 "Parser/parser.cc"
    break;

  case 970:
#line 3841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13777 "Parser/parser.cc"
    break;

  case 971:
#line 3843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13784 "Parser/parser.cc"
    break;

  case 973:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13790 "Parser/parser.cc"
    break;

  case 974:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13796 "Parser/parser.cc"
    break;

  case 975:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13802 "Parser/parser.cc"
    break;

  case 976:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13808 "Parser/parser.cc"
    break;

  case 977:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13814 "Parser/parser.cc"
    break;

  case 978:
#line 3863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13820 "Parser/parser.cc"
    break;

  case 979:
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13826 "Parser/parser.cc"
    break;

  case 980:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13832 "Parser/parser.cc"
    break;

  case 981:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13838 "Parser/parser.cc"
    break;

  case 982:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13844 "Parser/parser.cc"
    break;

  case 983:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13850 "Parser/parser.cc"
    break;

  case 984:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13856 "Parser/parser.cc"
    break;

  case 985:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13862 "Parser/parser.cc"
    break;

  case 986:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13869 "Parser/parser.cc"
    break;

  case 988:
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13875 "Parser/parser.cc"
    break;

  case 989:
#line 3900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13881 "Parser/parser.cc"
    break;

  case 990:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13887 "Parser/parser.cc"
    break;

  case 991:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13893 "Parser/parser.cc"
    break;

  case 992:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13899 "Parser/parser.cc"
    break;

  case 993:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13905 "Parser/parser.cc"
    break;

  case 994:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13911 "Parser/parser.cc"
    break;

  case 995:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13917 "Parser/parser.cc"
    break;

  case 996:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13923 "Parser/parser.cc"
    break;

  case 997:
#line 3928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13929 "Parser/parser.cc"
    break;

  case 998:
#line 3930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13935 "Parser/parser.cc"
    break;

  case 1000:
#line 3948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13941 "Parser/parser.cc"
    break;

  case 1001:
#line 3950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13947 "Parser/parser.cc"
    break;

  case 1002:
#line 3955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13953 "Parser/parser.cc"
    break;

  case 1003:
#line 3957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13959 "Parser/parser.cc"
    break;

  case 1004:
#line 3959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13965 "Parser/parser.cc"
    break;

  case 1005:
#line 3961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13971 "Parser/parser.cc"
    break;

  case 1006:
#line 3963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13977 "Parser/parser.cc"
    break;

  case 1008:
#line 3969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13983 "Parser/parser.cc"
    break;

  case 1009:
#line 3971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13989 "Parser/parser.cc"
    break;

  case 1010:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13995 "Parser/parser.cc"
    break;

  case 1011:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 14001 "Parser/parser.cc"
    break;

  case 1012:
#line 3980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14007 "Parser/parser.cc"
    break;

  case 1013:
#line 3982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14013 "Parser/parser.cc"
    break;

  case 1014:
#line 3988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 14019 "Parser/parser.cc"
    break;

  case 1015:
#line 3990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 14025 "Parser/parser.cc"
    break;

  case 1016:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 14031 "Parser/parser.cc"
    break;

  case 1017:
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 14037 "Parser/parser.cc"
    break;

  case 1019:
#line 4011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 14043 "Parser/parser.cc"
    break;

  case 1020:
#line 4013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 14049 "Parser/parser.cc"
    break;

  case 1022:
#line 4016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 14055 "Parser/parser.cc"
    break;

  case 1023:
#line 4018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 14061 "Parser/parser.cc"
    break;

  case 1025:
#line 4024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 14067 "Parser/parser.cc"
    break;

  case 1026:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 14073 "Parser/parser.cc"
    break;

  case 1027:
#line 4031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 14079 "Parser/parser.cc"
    break;

  case 1028:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 14085 "Parser/parser.cc"
    break;

  case 1029:
#line 4035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 14091 "Parser/parser.cc"
    break;

  case 1030:
#line 4037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 14097 "Parser/parser.cc"
    break;

  case 1031:
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14103 "Parser/parser.cc"
    break;

  case 1034:
#line 4078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 14110 "Parser/parser.cc"
    break;

  case 1035:
#line 4081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14116 "Parser/parser.cc"
    break;

  case 1036:
#line 4083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14122 "Parser/parser.cc"
    break;

  case 1037:
#line 4088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 14128 "Parser/parser.cc"
    break;

  case 1038:
#line 4090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 14134 "Parser/parser.cc"
    break;

  case 1039:
#line 4092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14140 "Parser/parser.cc"
    break;

  case 1040:
#line 4094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14146 "Parser/parser.cc"
    break;

  case 1041:
#line 4096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14152 "Parser/parser.cc"
    break;

  case 1043:
#line 4102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14158 "Parser/parser.cc"
    break;

  case 1044:
#line 4104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14164 "Parser/parser.cc"
    break;

  case 1045:
#line 4106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14170 "Parser/parser.cc"
    break;

  case 1046:
#line 4111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 14176 "Parser/parser.cc"
    break;

  case 1047:
#line 4113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14182 "Parser/parser.cc"
    break;

  case 1048:
#line 4115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14188 "Parser/parser.cc"
    break;

  case 1050:
#line 4122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14194 "Parser/parser.cc"
    break;

  case 1052:
#line 4133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 14200 "Parser/parser.cc"
    break;

  case 1053:
#line 4136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14206 "Parser/parser.cc"
    break;

  case 1054:
#line 4138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 14212 "Parser/parser.cc"
    break;

  case 1055:
#line 4141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14218 "Parser/parser.cc"
    break;

  case 1056:
#line 4143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14224 "Parser/parser.cc"
    break;

  case 1057:
#line 4145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 14230 "Parser/parser.cc"
    break;

  case 1059:
#line 4160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14236 "Parser/parser.cc"
    break;

  case 1060:
#line 4162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14242 "Parser/parser.cc"
    break;

  case 1061:
#line 4167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 14248 "Parser/parser.cc"
    break;

  case 1062:
#line 4169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 14254 "Parser/parser.cc"
    break;

  case 1063:
#line 4171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14260 "Parser/parser.cc"
    break;

  case 1064:
#line 4173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14266 "Parser/parser.cc"
    break;

  case 1065:
#line 4175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14272 "Parser/parser.cc"
    break;

  case 1067:
#line 4181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14278 "Parser/parser.cc"
    break;

  case 1068:
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14284 "Parser/parser.cc"
    break;

  case 1069:
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14290 "Parser/parser.cc"
    break;

  case 1070:
#line 4190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14296 "Parser/parser.cc"
    break;

  case 1071:
#line 4192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14302 "Parser/parser.cc"
    break;

  case 1074:
#line 4202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14308 "Parser/parser.cc"
    break;

  case 1077:
#line 4213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14314 "Parser/parser.cc"
    break;

  case 1078:
#line 4215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14320 "Parser/parser.cc"
    break;

  case 1079:
#line 4217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14326 "Parser/parser.cc"
    break;

  case 1080:
#line 4219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14332 "Parser/parser.cc"
    break;

  case 1081:
#line 4221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14338 "Parser/parser.cc"
    break;

  case 1082:
#line 4223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14344 "Parser/parser.cc"
    break;

  case 1083:
#line 4230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14350 "Parser/parser.cc"
    break;

  case 1084:
#line 4232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14356 "Parser/parser.cc"
    break;

  case 1085:
#line 4234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14362 "Parser/parser.cc"
    break;

  case 1086:
#line 4236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14368 "Parser/parser.cc"
    break;

  case 1087:
#line 4238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14374 "Parser/parser.cc"
    break;

  case 1088:
#line 4241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14380 "Parser/parser.cc"
    break;

  case 1089:
#line 4243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14386 "Parser/parser.cc"
    break;

  case 1090:
#line 4245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14392 "Parser/parser.cc"
    break;

  case 1091:
#line 4247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14398 "Parser/parser.cc"
    break;

  case 1092:
#line 4249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14404 "Parser/parser.cc"
    break;

  case 1093:
#line 4254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14410 "Parser/parser.cc"
    break;

  case 1094:
#line 4256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14416 "Parser/parser.cc"
    break;

  case 1095:
#line 4261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14422 "Parser/parser.cc"
    break;

  case 1096:
#line 4263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14428 "Parser/parser.cc"
    break;

  case 1098:
#line 4290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14434 "Parser/parser.cc"
    break;

  case 1102:
#line 4301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14440 "Parser/parser.cc"
    break;

  case 1103:
#line 4303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14446 "Parser/parser.cc"
    break;

  case 1104:
#line 4305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14452 "Parser/parser.cc"
    break;

  case 1105:
#line 4307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14458 "Parser/parser.cc"
    break;

  case 1106:
#line 4309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14464 "Parser/parser.cc"
    break;

  case 1107:
#line 4311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14470 "Parser/parser.cc"
    break;

  case 1108:
#line 4318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14476 "Parser/parser.cc"
    break;

  case 1109:
#line 4320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14482 "Parser/parser.cc"
    break;

  case 1110:
#line 4322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14488 "Parser/parser.cc"
    break;

  case 1111:
#line 4324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14494 "Parser/parser.cc"
    break;

  case 1112:
#line 4326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14500 "Parser/parser.cc"
    break;

  case 1113:
#line 4328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14506 "Parser/parser.cc"
    break;

  case 1114:
#line 4333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14512 "Parser/parser.cc"
    break;

  case 1115:
#line 4335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14518 "Parser/parser.cc"
    break;

  case 1116:
#line 4337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14524 "Parser/parser.cc"
    break;

  case 1117:
#line 4342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14530 "Parser/parser.cc"
    break;

  case 1118:
#line 4344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14536 "Parser/parser.cc"
    break;

  case 1119:
#line 4346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14542 "Parser/parser.cc"
    break;

  case 1122:
#line 4370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14548 "Parser/parser.cc"
    break;

  case 1123:
#line 4372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14554 "Parser/parser.cc"
    break;


#line 14558 "Parser/parser.cc"

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
#line 4375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
