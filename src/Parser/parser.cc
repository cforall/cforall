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
#define YYFINAL  148
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   26431

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  183
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1118
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2203

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
    3354,  3353,  3369,  3375,  3377,  3383,  3384,  3395,  3402,  3407,
    3413,  3416,  3419,  3423,  3429,  3432,  3435,  3440,  3441,  3442,
    3443,  3447,  3455,  3456,  3468,  3469,  3473,  3474,  3479,  3481,
    3483,  3488,  3489,  3495,  3496,  3498,  3503,  3504,  3506,  3541,
    3543,  3548,  3550,  3551,  3553,  3558,  3560,  3562,  3564,  3569,
    3571,  3573,  3575,  3577,  3579,  3581,  3586,  3588,  3590,  3592,
    3601,  3603,  3604,  3609,  3611,  3613,  3615,  3617,  3622,  3624,
    3626,  3628,  3633,  3635,  3637,  3639,  3641,  3643,  3655,  3656,
    3657,  3661,  3663,  3665,  3667,  3669,  3674,  3676,  3678,  3680,
    3685,  3687,  3689,  3691,  3693,  3695,  3707,  3712,  3717,  3719,
    3720,  3722,  3727,  3729,  3731,  3733,  3738,  3740,  3742,  3744,
    3746,  3748,  3750,  3755,  3757,  3759,  3761,  3770,  3772,  3773,
    3778,  3780,  3782,  3784,  3786,  3791,  3793,  3795,  3797,  3802,
    3804,  3806,  3808,  3810,  3812,  3822,  3824,  3827,  3828,  3830,
    3835,  3837,  3839,  3844,  3846,  3848,  3850,  3855,  3857,  3859,
    3873,  3875,  3878,  3879,  3881,  3886,  3888,  3893,  3895,  3897,
    3902,  3904,  3909,  3911,  3928,  3929,  3931,  3936,  3938,  3940,
    3942,  3944,  3949,  3950,  3952,  3954,  3959,  3961,  3963,  3969,
    3971,  3974,  3981,  3983,  3992,  3994,  3996,  3997,  3999,  4001,
    4005,  4007,  4012,  4014,  4016,  4018,  4053,  4054,  4058,  4059,
    4062,  4064,  4069,  4071,  4073,  4075,  4077,  4082,  4083,  4085,
    4087,  4092,  4094,  4096,  4102,  4103,  4105,  4114,  4117,  4119,
    4122,  4124,  4126,  4140,  4141,  4143,  4148,  4150,  4152,  4154,
    4156,  4161,  4162,  4164,  4166,  4171,  4173,  4181,  4182,  4183,
    4188,  4189,  4194,  4196,  4198,  4200,  4202,  4204,  4211,  4213,
    4215,  4217,  4219,  4222,  4224,  4226,  4228,  4230,  4235,  4237,
    4239,  4244,  4270,  4271,  4273,  4277,  4278,  4282,  4284,  4286,
    4288,  4290,  4292,  4299,  4301,  4303,  4305,  4307,  4309,  4314,
    4316,  4318,  4323,  4325,  4327,  4345,  4347,  4352,  4353
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

#define YYPACT_NINF (-1862)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1117)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     121, 13328,   152,   172, 19989,   -71, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,   258,   981,
     335, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,    90,   183,
   -1862, -1862, -1862, -1862, -1862, -1862,  5565,  5565,   349, 13328,
     370,   373, 23465, -1862,   512, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862,   551,  4133, -1862,   202,   194,
   -1862, -1862, -1862, -1862, -1862, 19518, -1862, -1862,   458,   573,
     484,   127, -1862,  5004,   590,   598,   606,   532,  5422,   788,
     895, 13495, -1862, -1862,   748, 19361,  1483, -1862, -1862, -1862,
   -1862,  2687,   823,  6529,  9919,  1044,  2687,  1159,   695, -1862,
   -1862, -1862, -1862,   142, -1862, -1862, -1862, -1862,   665, -1862,
   -1862, -1862, -1862, -1862,   700,   704,   142, -1862,   142, 17586,
   -1862, -1862, -1862, 21187,  5565, -1862, -1862,  5565, -1862, 13328,
   -1862,   685, 21241, -1862, -1862,  5499, 22463, -1862, -1862,  1138,
    1138,   707,  2657, -1862, -1862, -1862, -1862,   483, 15860,   142,
    3900,   142, -1862, -1862, -1862, -1862, -1862, -1862,   735, -1862,
     720,   767,  1367, -1862,   819, 25608, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, 17970,  2248,  3029,  4133,   409,   804,   814,
     825,   836,   838,   849, -1862, -1862, 20146, 11943,   840,   876,
   -1862,  7571, -1862, -1862, -1862, -1862,   896, -1862, -1862,   859,
   -1862, 23524,  1042, 23678, -1862,   912,  5565,   704,   915,   917,
   -1862,  2489,  5499,  2489, -1862, -1862, -1862,  3682,  4660,   914,
     991,    14,   991, -1862,   142,   142,    77, 17260,   621,   991,
   -1862,   142,   142,    77,   142, -1862,   142, -1862,  4830, -1862,
   -1862,   928,   965,  1138, 23272,   941, 19518, -1862,  5004, -1862,
    2687, -1862,  1952,   695,   969,  1052, 17260,  5565,  5565,   484,
   -1862, 15030, -1862,  1138,  1138,   993,  1052, 17260,  5565, -1862,
   25769, -1862, -1862, -1862,  1138, -1862, -1862, -1862, -1862,  1138,
   -1862,   795,  4502,  5565, -1862, 19213,  1013, -1862, -1862, -1862,
   23133,   704, 17423,   979,  5499, 19074, 23272,  2687, -1862, -1862,
   22668, -1862,   991,   137, -1862, 25608, 22613,  4522,  4830, -1862,
     682, -1862, -1862, -1862, -1862, -1862, 21241,   991,  5565, -1862,
    1012,  1027, -1862, -1862, -1862, -1862,  5565,  3929,   513,   111,
   -1862,  5565,   720, -1862,   865,   142, -1862,  1035, 21398,  1194,
   16358, 23326,  2687, -1862,  2687,  1138,  2687,  1138, -1862, -1862,
     142, -1862, -1862,  1030, 21452, -1862, -1862, -1862, 21609,   896,
   -1862,  2133,   371,   750, -1862,   446,   695,  1043,  1014, -1862,
    2657,  1055,   720,  2657, -1862, -1862,  2248, -1862,   796, -1862,
    1097, -1862,  1109,  1176, 25772,  1121, 25856,  1123,  1142, 25608,
   25933,  1149, 23517, -1862, -1862, -1862, -1862, -1862, -1862, 26010,
   26010, 17809,  1156,  4693, -1862, -1862, -1862, -1862,   107, -1862,
     300, -1862,   760, -1862, 25608, 25608, -1862,  1148,   294,   885,
     999,   453,  1015,  1162,  1158,  1161,  1211,     8, -1862,   827,
   -1862,  1188, -1862,  1032,  5506, 18453, -1862, -1862,  1237,  1188,
   -1862, -1862,   833, -1862, -1862,   904,  3029,  1205,  1215,  1227,
    1230,  1243,  1268, -1862, -1862,   701,  1192, -1862,   922,  1192,
    1275, -1862,  1278, -1862, 21187, -1862,  1233,  1288, 18614, -1862,
   -1862,  4955,  4845,  1304, 16358,  1317,  1320,  1395,  1307,  1310,
   -1862, -1862, -1862,  5565,  5313, 20662, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, 19022,  4050,  1156, 23524,  1336,  1358, -1862,
   -1862,  1363, 23678,   792, -1862, -1862, -1862, 18453,  1372, -1862,
     819, -1862, -1862, -1862,  1359,  3682,   773,  1389,  1415,  1418,
     810,  1427,  1431,  1433,  1438,  1440,  1445,  4660, -1862, -1862,
   -1862,   142,  1383,  1419, -1862, -1862,  1448,   484, -1862, -1862,
     704,  1052, 20312, -1862, -1862,   484, -1862, -1862,   704, -1862,
   -1862,  4830, -1862, 18453, 18453, -1862,  1138,  5499,  7132,  3586,
   16524, -1862, -1862, -1862, -1862, -1862, -1862,   704,  1052,   137,
    1459, -1862, -1862,  2687,  1461,  1052, 17260, -1862,   704,  1052,
   -1862, 26171, -1862,  1138,  1138, -1862, -1862,  1466,   412,  1473,
     695,  1475, -1862, -1862, -1862, 20608,  1492,  1494, -1862, -1862,
     945, -1862,  1584, -1862,  1477, -1862, -1862, -1862, 21775, 26174,
   -1862, -1862, -1862, -1862, -1862,  4522,   844,  4830, 20312,   991,
   13328, -1862,  5565,  1501, -1862,  1508, -1862, -1862, -1862, -1862,
   -1862,  2657, -1862, -1862,  1592,  4804, 20819, 11943, -1862, 21829,
   -1862,  1138,  1138, -1862, -1862,   896, -1862, 15362,  1522,  1670,
   25608,   933,  1448,  1513, -1862,   142,   142, -1862,  1192, -1862,
   21398, -1862, -1862, 20608,  1138,  1138, -1862,  4804, -1862, -1862,
   22408, -1862, -1862, 21452, -1862,   142,  1531,   142,  1014,   211,
    1536,   946, 21241,   955,   960, -1862,  2248, 23755,  1524, -1862,
   18131, -1862,  4693, 18292, -1862, 21986, 21241, -1862, 18131, -1862,
   25608, -1862, -1862, -1862, -1862, -1862, -1862, 18292, -1862, -1862,
   20873, 21986, 21986,  1254,  1529,  1632,   693,  1945, -1862,   968,
    1549,  1370,  1552, -1862, 23832, 25608, 23909,  1550, 25608,  2489,
   25608,  2489, -1862,  1887, -1862, -1862, 23755,  3444, 25608, 23755,
    2489, -1862, -1862, 25608, 25608, 25608, 25608, 25608, 25608, 25608,
   25608, 25608, 25608, 25608, 25608, 25608, 25608, 25608, 25608, 25608,
   25608, 25608, 23986,  1538,   819,  4115, 11943, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,  1553, 25608,
   -1862, -1862, 15528,  1022, -1862, -1862,   142,   142, -1862, -1862,
   18453, -1862, -1862,   734,  1192, -1862,   983,  1192, 20312, -1862,
   -1862,  1448, 20312, -1862,  1448, -1862, 26258, -1862, -1862, -1862,
   19832, 11943,  1560,  1373,  1562, 14864,  1710,  3666,   737,  1513,
   -1862,   142,   142,  1513,   772, -1862,   142,   142, 25608,  5565,
   16524,  1571, 16524,  1580,  1513,   297, 15694, 15694, 16690, 15694,
    5565, -1862, -1862, 25608,  1363, -1862, 23524,  1587, -1862,  2158,
   -1862, -1862, -1862,  1005, -1862,  1585, 15694, 25608,  1021,  1586,
    1588,  1589,  1039,  1591,  1595,  1599,  1600,  1603,  1604,   774,
    1192, -1862, -1862,   837,  1192, -1862, -1862,   843,  1192, -1862,
   -1862, -1862,  5499,  1729,  1192, 22818, -1862, -1862,   704,  1605,
   -1862, -1862, -1862,  1050,  1607,  1051,  1621, -1862,  1275,  1594,
    1625, -1862,   704, -1862,  1626, -1862,   704,  1052,  1625, -1862,
     704,  1619,  1622,  1624, -1862, -1862, 20469, -1862,  2489,  5565,
   11062,  1689, -1862,  1288, -1862, 15694,  1096, -1862,  1625,  1635,
   -1862, 22040, 18453,  1610, -1862,  1610, -1862, -1862, -1862, -1862,
   21452, -1862, 12113, 18775, -1862,  1636,  1637,  1638,  1640, -1862,
   13032,   142, -1862,   933, -1862, -1862, -1862, -1862,  1448, -1862,
   -1862, -1862,  1138, -1862, -1862, -1862, -1862,   211,  1014,  1642,
     483, -1862, -1862,  1643,  5565,   211, -1862, -1862,  1644,  1646,
   -1862, -1862,  1075, -1862, -1862, -1862, -1862,  1651,  1653,  1650,
    1655,  1639,  1652,  1657,  1663,  1664,  1662,  1665, 25608,  1667,
    1669,  1672, 22197, 12283, 25608, -1862, -1862,  1963, -1862, -1862,
   -1862, 25608, -1862,  1673,  1674, 23601, -1862, -1862,  1398, -1862,
   23755,  1675, -1862,  1679, -1862, -1862,  3160, -1862,  1101, -1862,
    3160, -1862, -1862,  1400,   499, -1862, -1862,  1148,  1148,  1148,
     294,   294,   885,   885,   999,   999,   999,   999,   453,   453,
    1015,  1162,  1158,  1161,  1211, 25608,  1394, -1862,  1677,  3160,
   -1862, -1862, 23524, -1862,  1683,  1684,  1686,  1690,  1022, -1862,
   -1862, -1862, -1862, -1862, 20312, -1862, -1862,  1448, 20312, -1862,
    1448,  1691,  1692, 15694, 15694, -1862, -1862, 14864,   873,  1694,
    1695,  1697,  1698,  1661,  3666, -1862, -1862, 20312, -1862, -1862,
   -1862, -1862, -1862, -1862, 20312, -1862, -1862, -1862, -1862,  1696,
   -1862,  1513,  1685, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862,  1702,  1703,  1704, -1862,  1705, -1862,   484,  3160,  1405,
      17, -1862, -1862,  1709, -1862, 23678, -1862, 25608,   142, 15694,
     142, -1862, -1862,   891,  1192, -1862,   905,  1192, -1862, -1862,
     954,  1192, 20312, -1862, -1862,  1448, 20312, -1862, -1862,  1448,
   20312, -1862, -1862,  1448,   991, -1862,  1448,    26, -1862,  1188,
    1707, -1862, -1862, -1862, -1862, -1862, -1862,  1716, -1862, -1862,
   -1862, 22040,  1625, -1862,   704, -1862, -1862, -1862, -1862, -1862,
   10455, -1862, -1862, -1862, -1862, -1862,   -16,   378,   344, 11773,
    1717,  1718, 17080,  1722,  1723,  3061,  3697,  4033, 24063,  1724,
   -1862, -1862,  1725,  1728, 17080,  1734, -1862, -1862,   704, 25608,
   25608,  1786,  1733,   536, -1862, 17648,  1408,  1737,  1740,  1726,
   -1862, -1862, -1862, 10882, -1862, -1862, -1862, -1862, -1862,   902,
   -1862, -1862, -1862,  1480,   347, -1862,   352, -1862,   347, -1862,
   -1862, -1862, -1862, -1862,  2489, -1862, -1862, 13662, 19675, -1862,
    5565,  1711,  1744, -1862, -1862, -1862,  5565, -1862, -1862,  5499,
   -1862, -1862,  1732,  1738,  1107, 21241,   720,   720, -1862, -1862,
    1156,  1288, 18614, -1862,  1188, -1862, 12453, -1862,  1009,  1192,
   -1862,  1138, 13157, -1862, -1862,  1014,  1643,  1742,   211,   695,
     126,  1754,  1735,  1643,  1763, -1862, -1862, 23755,   679, -1862,
   20608,   679, 12283,  2489, -1862,   679, -1862, 21030,   679, -1862,
   25608, 25608, 25608, -1862, -1862, -1862, -1862, 25608, 25608,  1756,
   23524, -1862, -1862, 24140,  1759,   697, -1862, -1862, -1862,  2301,
   -1862, -1862,  1417, -1862,   265, -1862,  1422, -1862, 23832, -1862,
   -1862, 25608,  1745,  1432,  1444,  1363, -1862,  1062,  1192, -1862,
   -1862,  1767,  1769, -1862, -1862, -1862, -1862,  1771,  1094,  1192,
   -1862,  1166,  2377,   142,   142, -1862, -1862,  1773,  1775, -1862,
    1774, -1862, 16524,  1776, -1862, 16026, 16192,  1782, 16690,  1783,
   -1862,  1772, 25608, 25608,  1450,  1781, -1862, -1862, -1862, -1862,
   -1862, -1862,  1787, 20312, -1862, -1862,  1448, 20312, -1862, -1862,
    1448, 20312, -1862, -1862,  1448,  1788,  1791,  1792,   484, -1862,
   -1862,  1455, 25608, 22972,  1802,  1789, -1862, -1862, -1862,  1812,
   14156, 14316, 14476, 22040, 23272, 21986, 21986,  1815, -1862,   391,
     427,  2408, 14698, -1862,   431,  5565,  5565, -1862, 23755,   223,
     336, -1862, -1862, -1862, -1862, 11773, 25608,  1825,  1888, 11602,
   11242, -1862,  1803, -1862,  1805, 25608,  1806, 23524,  1810, 25608,
   18453, 25608, -1862, 11422,  1199, -1862,  1838,     7, -1862,    25,
    1891,   192,   142, -1862,  1841, -1862,  1839, -1862,  1840,  1868,
    1870, 17080, 17080, -1862, -1862,  1938, -1862, -1862,   159,   159,
     435, 15196,   463, -1862, -1862,  1871,  1875,   513, -1862,  1878,
   -1862,  1874, -1862,  1883, -1862, -1862, -1862, -1862, 12623,  1885,
    1894,  1896, -1862, 20312, -1862, -1862,  1448, 25608, 25608,  1288,
    1897, -1862,  1895,  1905,   211,  1643,   483,  5565, -1862, 24217,
   -1862,  1908, -1862, 22040, -1862,  1262,  1909,  1889,  1165, -1862,
    1903, -1862, -1862, -1862, -1862, -1862, 23524,  1363, -1862, -1862,
   23832, -1862,  1948,  3160, -1862,  1948,  1948, -1862,  3160,  4303,
    4977, -1862,  1458, -1862, -1862, -1862,  1917, 20312, -1862, -1862,
    1448, -1862, -1862,  1915,  1918,   142, 20312, -1862, -1862,  1448,
   20312, -1862, -1862,  1919, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862,  1685, -1862, -1862, -1862,  1920, -1862, -1862, -1862,
   -1862,  1921,  1927,   142,  1928,  1929,  1931, -1862, -1862, -1862,
   -1862, 25608, -1862,    26, -1862,  1188, -1862, -1862,  1923,  1936,
   -1862,  1815,  1815,  1815,  5183,  1280,  1911,   533, -1862,  5183,
     535, 18453, -1862, -1862, -1862,  5066, 25608,  5145,   497, -1862,
   -1862,   338,  1930,  1930,  1930,  5565, -1862, -1862, -1862,  1179,
   -1862, -1862, -1862, -1862,  1740,  1941, 25608,   458,  1934,   532,
   14643, 22040,  1182,  1944, 17080,  1950, -1862, -1862, -1862, -1862,
    1293, 17080, 25608,   569,   456, -1862, 25608,  9945, -1862, -1862,
     541, -1862,  1363, -1862,  1184,  1197,  1198,   658, -1862, -1862,
   -1862, -1862,   704,  1199,  1953, -1862, -1862, 25608, -1862,  1955,
     819, -1862, 10702, -1862, -1862, -1862, 25608, 25608, -1862, -1862,
     545,   159, -1862,   438, -1862, -1862, -1862,   142, -1862,  1610,
   -1862, 22040, -1862, -1862, -1862, -1862, -1862,  1951,  1957, -1862,
   -1862,  1956, -1862,  1960,   211, -1862,  1643,  1958,   695,  1735,
   23524, -1862, -1862, -1862,  1968, -1862, -1862, 25608, -1862, 21030,
   25608,  1363,  1972,  1467, -1862,  1471, -1862,  3160, -1862,  3160,
   -1862, -1862, -1862,  1974,   142,   142,  1976,  1977, -1862,  1975,
   -1862, -1862, -1862, -1862, -1862,  1474, 25608, -1862, -1862, -1862,
   -1862, -1862,   601,  1280,  1231,   604, -1862, -1862, -1862, -1862,
     142,   142, -1862, -1862, -1862,   610, -1862,  1210,  5066,   616,
   -1862,  5145, -1862,   142, -1862, -1862, -1862, -1862, -1862, -1862,
   17080, 17080,  1740, 16856,   323, 24294,  2062, 17080, -1862, -1862,
   -1862, -1862, -1862, 25608, -1862, 24371,  2063,  1959, 18852, 24448,
   17080, 11422,  1740,  1157,  1077,  1964, 25608, -1862,  1984,   481,
   17080, -1862, 17080, -1862,  1990, -1862, -1862,  1966,   819,   668,
   -1862, -1862,  1991,  1479,  1244, 17080,  1992, 17080, 17080, 17080,
   -1862,   720, -1862,  5565,  5499, -1862, -1862,  1989,  1993, -1862,
   -1862,  1643,  1996, -1862, -1862,  1363,  1998, -1862, -1862, -1862,
   -1862,  2003, -1862, -1862, -1862,  1499,  1505, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862,  2001,  2004,  2007,  1231,
   -1862,   142, -1862, -1862, -1862, -1862, -1862,  2008,  5183, -1862,
    2086,  9678,    80, 12796, -1862, 16954, -1862,    27,  1255, 17080,
    2096,   627,  2000,    -5, 17080, 25608,  1157,  1077,  1999, 24530,
    1083,  1138,  2002,   212,  2102, -1862, 24607, 24684, 25608,  1740,
    2005, 12965, -1862, -1862, -1862, -1862, 22251, -1862,  2020,  2006,
     204, -1862, 25608, 23755, -1862, -1862, 25608,   347, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862,  2029, -1862,  2030, -1862, -1862,
   -1862, -1862,  1222,  1192, -1862, -1862,  1280, -1862, 17080, -1862,
     196, -1862,   122, -1862, -1862, -1862,  2033, 13829, -1862, -1862,
   17080, -1862,    42, -1862, 17080, 25608,  2034, 24761, -1862, -1862,
   24838, 24915, 25608,  4804,  1740, -1862,  1188, 24992, 25069, 17080,
    2018,   224,  2021,   494, -1862, -1862,  2040, 13829, 22251, -1862,
    5286, 21829,  2489,  2036, -1862,  2090,  2041,   714,  2038, -1862,
   -1862,  1263,  1265,   342, -1862, -1862, 20312, -1862, -1862,  1448,
   -1862, -1862, 25608, -1862, 25608, -1862, -1862,  1601, 13996, -1862,
   -1862, 17080, -1862, -1862,  1740, -1862, -1862,  1740,  2035,   529,
    2039,   651, -1862, -1862,   695, -1862,  1740, -1862,  1740, -1862,
    2044, 25146, 25223, 25300, -1862,  1601, -1862,  2024,  3248,  4385,
   -1862, -1862, -1862,   204,  2045, 25608,  2031,   204,   204, -1862,
   -1862, 17080,  2127,  2052, -1862, -1862, 16954, -1862,  1601, -1862,
   -1862,  2057, 25377, 25454, 25531, -1862, -1862,  1740, -1862,  1740,
   -1862,  1740, -1862,  2024, 25608,  2058,  4385,  2054,   819,  2060,
   -1862,   726, -1862, -1862, 17080, -1862, -1862, 10284,  2064, 16954,
   -1862, -1862,  1740, -1862,  1740, -1862,  1740,  2065,  2066, -1862,
     704,   819,  2059, -1862,  2042,   819, -1862, -1862, -1862, -1862,
   10573, -1862,   704, -1862, -1862,  1512, 25608, -1862,  1308, -1862,
     819,  2489,  2070,  2048, -1862, -1862,  1340, -1862, -1862,  2049,
    2489, -1862, -1862
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
     538,   539,   540,   541,   542,   543,   550,   551,   854,   553,
     626,   627,   630,   632,   628,   634,     0,     0,     0,   498,
       0,     0,    17,   597,   603,     9,    10,    11,    12,    13,
      14,    15,    16,   811,   110,     0,     0,    20,     0,     2,
     108,   109,    18,    19,   869,   498,   812,   434,     0,   437,
     734,   439,   450,   852,   438,   472,   473,     0,     0,     0,
       0,   580,   500,   502,   508,   498,   510,   513,   565,   524,
     552,   482,   558,   563,   484,   575,   483,   590,   594,   600,
     579,   606,   618,   854,   623,   624,   607,   676,   440,   441,
       3,   819,   832,   503,     0,     0,   854,   891,   854,   498,
     908,   909,   910,   498,     0,  1095,  1096,     0,     1,   498,
      17,     0,   498,   461,   462,     0,   580,   508,   492,   493,
     494,   822,     0,   629,   631,   633,   635,     0,   498,   854,
     679,   855,   856,   625,   554,    22,    23,    21,   788,   783,
     773,     0,   863,   820,     0,     0,   515,   813,   817,   818,
     814,   815,   816,   498,   863,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   598,   601,   498,   498,     2,     0,
    1097,   580,   898,   916,  1101,  1094,  1092,  1099,   433,     0,
     172,   740,   171,     0,   442,     0,     0,     0,     0,     0,
     448,     0,     0,     0,   432,   985,   986,     0,     0,   471,
     852,   854,   852,   872,   854,   854,   481,   498,   854,   852,
     929,   854,   854,   480,   854,   948,   854,   926,     0,   573,
     574,     0,     0,   498,   498,     2,   498,   451,   852,   501,
     511,   566,     0,   595,     0,   835,   498,     0,     0,   734,
     452,   580,   559,   576,   591,     0,   835,   498,     0,   514,
     560,   567,   568,   485,   577,   487,   488,   486,   582,   592,
     596,     0,   610,     0,   805,   498,     2,   833,   890,   892,
     498,     0,   498,     0,     0,   580,   498,   510,     2,  1105,
     580,  1108,   852,   852,     3,     0,   580,     0,     0,   464,
     854,   847,   849,   848,   850,     2,   498,   852,     0,   809,
       0,     0,   769,   771,   770,   772,     0,     0,   765,     0,
     754,     0,   763,   775,     0,   854,   677,     2,   498,  1117,
     499,   498,   489,   558,   490,   583,   491,   590,   587,   608,
     854,   609,   722,     0,   498,   723,  1070,  1071,   498,   724,
     726,   679,   597,   603,   680,   681,   682,     0,   679,   857,
       0,   786,   774,     0,   868,   867,   863,   866,     0,   861,
     864,    25,     0,    24,     0,     0,     0,     0,     0,     0,
       0,     0,    27,    29,     4,     8,     5,     6,     7,     0,
       0,   498,     2,     0,   111,   112,   113,   114,    91,    28,
      92,    46,    90,   115,     0,     0,   130,   132,   136,   139,
     142,   147,   150,   152,   154,   156,   158,   160,   163,     0,
      30,     0,   604,     2,   115,   498,   164,   780,   730,   594,
     732,   779,     0,   729,   733,     0,     0,     0,     0,     0,
       0,     0,     0,   870,   896,   854,   906,   914,   918,   924,
     597,     2,     0,  1103,   498,  1106,     2,   108,   498,     3,
     721,     0,  1117,     0,   499,   558,   583,   590,     3,     3,
     717,   707,   711,   723,   724,   498,     2,     2,   899,   917,
    1093,     2,     2,    27,     0,     2,   740,    28,     0,   738,
     741,  1115,     0,     0,   747,   736,   735,   498,     0,   837,
       0,     2,   463,   465,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   875,   932,
     955,   854,   477,     2,   871,   879,  1013,   734,   873,   874,
       0,   835,   498,   928,   936,   734,   930,   931,     0,   947,
     949,     0,   467,   498,   498,   564,   499,     0,   580,     0,
     498,  1098,  1102,  1100,   449,   581,   809,     0,   835,   852,
       0,   443,   453,   512,     0,   835,   498,   809,     0,   835,
     784,   561,   562,   578,   593,   599,   602,   597,   603,   621,
     622,     0,   785,   693,   727,   499,     0,   694,   696,   697,
       0,   212,   426,   834,     0,   424,   481,   480,   580,     0,
     445,     2,   446,   806,   469,     0,     0,     0,   498,   852,
     498,   809,     0,     0,     2,     0,   768,   767,   766,   760,
     509,     0,   758,   776,   556,     0,   498,   498,  1072,   499,
     495,   496,   497,  1076,  1067,  1068,  1074,   498,     2,   109,
       0,  1032,  1046,  1117,  1028,   854,   854,  1037,  1044,   715,
     498,   588,   725,   499,   584,   585,   589,     0,   678,  1082,
     499,  1087,  1079,   498,  1084,   854,     0,   854,   679,   679,
       0,     0,   498,     0,     0,   859,   863,    70,     0,    26,
     498,    98,     0,   498,   107,   498,   498,    93,   498,   100,
       0,    36,    40,    41,    37,    38,    39,   498,    96,    97,
     498,   498,   498,     2,   111,   112,     0,     0,   190,     0,
       0,   624,     0,  1092,     0,     0,     0,     0,     0,     0,
       0,     0,    59,     0,    65,    66,    70,     0,     0,    70,
       0,    94,    95,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   498,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,   183,   171,     0,
     169,   170,   498,   997,   731,   994,   854,   854,  1002,   605,
     498,   860,   897,   854,   907,   915,   919,   925,   498,   900,
     902,   904,   498,   920,   922,     2,     0,     2,  1104,  1107,
     498,   498,     0,     2,     0,   498,   109,  1032,   854,  1117,
     967,   854,   854,  1117,   854,   982,   854,   854,     3,   725,
     498,     0,   498,     0,  1117,  1117,   498,   498,   498,   498,
       0,     2,   749,     0,  1115,   746,  1116,     0,   742,     0,
       2,   745,   748,     0,     2,     0,   498,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   854,
     884,   888,   927,   854,   941,   945,   953,   854,   963,   876,
     933,   956,     0,     0,  1009,     0,   475,   838,     0,     0,
     476,   839,   468,     0,     0,     0,     0,   466,     0,     2,
       2,   840,     0,   447,     2,   809,     0,   835,     2,   841,
       0,     0,     0,     0,   636,   893,   498,   911,     0,     0,
     498,   427,   425,   108,     3,   498,     0,   810,     2,     0,
     762,   498,   498,   756,   755,   756,   557,   555,   681,  1078,
     498,  1083,   499,   498,  1069,     0,     0,     0,     0,  1047,
       0,   854,  1118,  1033,  1034,   716,  1030,  1031,  1045,  1073,
    1077,  1075,   586,   621,  1081,  1086,   673,   679,   679,     0,
       0,   688,   687,  1115,     0,   679,   789,   787,     0,     0,
     862,    74,     0,    71,    72,    75,   821,     0,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   498,   498,     0,   129,   128,     0,   125,   124,
      31,     0,    32,     0,     0,     0,   187,   186,     0,     3,
      70,     0,    55,     0,    56,    63,     0,    62,     0,    58,
       0,    57,    61,     0,     0,    54,   131,   133,   134,   135,
     137,   138,   140,   141,   145,   146,   143,   144,   148,   149,
     151,   153,   155,   157,   159,     0,     0,   436,     0,     0,
      33,     3,   740,   165,     0,     0,     0,     0,   998,   999,
     995,   996,   782,   781,   498,   901,   903,   905,   498,   921,
     923,     0,     0,   498,   498,  1023,  1022,   498,     0,     0,
       0,     0,     0,   854,  1033,   970,   987,   498,   965,   973,
     713,   968,   969,   714,   498,   980,   990,   983,   984,     0,
       3,  1117,     3,   709,   459,   708,   712,  1109,   718,   719,
     701,     0,   702,   703,     3,     3,     3,   734,     0,   163,
       0,     3,     3,     0,   743,     0,   737,     0,   854,   498,
     854,     3,   470,   854,   885,   889,   854,   942,   946,   954,
     854,   964,   498,   877,   880,   882,   498,   934,   937,   939,
     498,   957,   959,   961,   852,   478,  1010,     3,  1014,  1015,
       3,   843,   950,   570,   569,   572,   571,     2,   810,   844,
     791,   498,     2,   842,     0,   810,   845,   636,   636,   636,
     498,   695,   698,   699,   728,   430,     0,     0,     0,   498,
       0,     0,   351,     0,     0,     0,     0,     0,   192,     0,
     346,   347,     0,     0,   351,     0,   399,   398,     0,   167,
     167,   405,   597,   603,   209,   498,     2,     0,   193,     0,
     220,   194,   195,   498,   214,   196,   197,   198,   199,     0,
     200,   201,   352,     0,   366,   202,   372,   374,   377,   203,
     204,   205,   206,   207,     0,   208,   216,   580,   498,   218,
       0,     0,     0,     3,   823,   810,     0,   798,   799,     0,
       3,   794,     3,     3,     0,   498,   773,   773,  1080,  1085,
       2,   108,   498,     3,   595,     3,   499,  1041,   854,  1040,
    1043,   498,     3,  1029,  1035,   679,  1115,     0,   679,   684,
     679,     0,   689,  1115,     2,   858,   865,     0,    99,   102,
     498,   106,   498,     0,   105,   101,   103,   498,     0,   119,
       0,     0,     0,   123,   127,   126,   191,     0,     0,     0,
     740,   116,   184,     0,     0,     0,    49,    50,    88,     0,
      88,    88,     0,    76,    78,    52,     0,    48,     0,    51,
     162,     0,     0,     0,     0,  1115,  1006,   854,  1005,  1008,
    1000,     0,     0,   894,   912,     3,     3,     0,   854,   976,
     979,   854,     0,   854,   854,   971,   988,     0,     0,  1110,
       0,   720,   498,     0,  1112,   498,   498,     0,   498,     0,
     444,     3,     0,     0,     0,     0,   739,   744,     3,   836,
       3,   853,     0,   498,   878,   881,   883,   498,   935,   938,
     940,   498,   958,   960,   962,     0,     0,     0,   734,  1021,
    1020,     0,     0,     0,     0,     0,     3,   810,   846,     0,
     498,   498,   498,   498,   498,   498,   498,   619,   649,     0,
       0,   650,   580,   637,     0,     0,     0,   428,    70,     0,
       0,   337,   338,   217,   219,   498,     0,     0,     0,   498,
     498,   333,     0,   331,     0,     0,     0,   740,     0,     0,
     498,     0,   378,   498,     0,   168,     0,     0,   406,     0,
       0,     0,   854,   224,     0,   215,     0,   328,     0,     0,
       0,   351,   351,   357,   356,   351,   368,   367,   351,   351,
       0,   580,     0,  1025,  1024,     0,     0,   765,   801,     2,
     796,     0,   797,     0,   777,   757,   761,   759,   498,     0,
       0,     0,     3,   498,  1036,  1038,  1039,     0,     0,   108,
       0,     3,     0,     0,   679,  1115,     0,     0,   668,     0,
     683,     0,   790,   498,    73,  1026,     0,     0,     0,    42,
       0,   120,   122,   121,   118,   117,   740,  1115,   189,   188,
       0,    69,    85,     0,    79,    86,    87,    64,     0,     0,
       0,    60,     0,   161,   435,    34,     0,   498,  1001,  1003,
    1004,   895,   913,     0,     0,   854,   498,   972,   974,   975,
     498,   989,   991,     0,   966,   981,   977,   992,  1111,   710,
     460,   705,   704,   706,  1114,  1113,     0,     3,   851,   750,
     751,     0,     0,   854,     0,     0,     0,   886,   943,   951,
     479,     0,  1016,     0,  1017,  1018,  1012,   827,     2,     0,
     829,   619,   619,   619,   650,   657,   624,     0,   663,   650,
       0,   498,   611,   648,   644,     0,     0,     0,     0,   651,
     653,   854,   665,   665,   665,     0,   645,   661,   431,     0,
     341,   342,   339,   340,   233,     0,     0,   235,   439,   234,
     580,   498,     0,     0,   351,     0,   316,   318,   317,   319,
       0,   351,   192,   273,     0,   266,     0,   192,   334,   332,
       0,   326,  1115,   335,     0,     0,     0,     0,   387,   388,
     389,   390,     0,   380,     0,   381,   343,     0,   344,     0,
       0,   371,     0,   213,   330,   329,     0,     0,   360,   370,
       0,   351,   373,     0,   375,   397,   429,   854,   825,   756,
     778,   498,     2,     2,  1088,  1089,  1090,     0,     0,     3,
       3,     0,  1049,     0,   679,   669,  1115,     0,   686,   689,
     740,   690,   672,     3,     0,  1027,   104,     0,    35,   498,
       0,  1115,     0,     0,    89,     0,    77,     0,    83,     0,
      81,    47,   166,     0,   854,   854,     0,     0,   753,     0,
     454,   458,   887,   944,   952,     0,     0,   793,   831,   615,
     617,   613,     0,     0,  1056,     0,   658,  1061,   660,  1053,
     854,   854,   643,   664,   647,     0,   646,     0,     0,     0,
     667,     0,   639,   854,   638,   654,   666,   655,   656,   662,
     351,   351,   236,   580,     0,     0,   254,   351,   321,   324,
     322,   325,   320,     0,   323,     0,   262,     0,   192,     0,
     351,   498,   274,     0,   299,     0,     0,   327,     0,     0,
     351,   350,   351,   391,     0,   382,     2,     0,     0,     0,
     211,   210,   353,     0,     0,   351,     0,   351,   351,   351,
     457,   773,   795,     0,     0,  1091,  1042,     0,     0,  1048,
    1050,  1115,     0,   671,   685,  1115,     2,    53,    45,    43,
      44,     0,    67,   185,    80,     0,     0,  1007,   456,   455,
     978,   993,   752,  1011,  1019,   641,     0,     0,     0,  1057,
    1058,   854,   642,  1054,  1055,   640,   620,     0,     0,   349,
     225,     0,     0,     0,   247,   351,   227,     0,     0,   351,
     256,   271,   282,   276,   351,   192,     0,   286,     0,     0,
       0,   311,   277,   275,   264,   267,     0,     0,   192,   300,
       0,     0,   230,   348,   379,     2,   498,   345,     0,     0,
     407,   358,     0,    70,   369,   362,     0,   363,   361,   376,
     764,   800,   802,  1051,  1052,     0,   675,     0,   792,    68,
      84,    82,   854,  1064,  1066,  1059,     0,   652,   351,   242,
     237,   240,     0,   239,   246,   245,     0,   498,   249,   248,
     351,   258,     0,   255,   351,     0,     0,     0,   263,   268,
       0,     0,   192,     0,   287,   312,   313,     0,     0,   351,
       0,   302,   303,   301,   270,   336,     0,   498,   498,     3,
     392,   499,   396,     0,   400,     0,     0,     0,   408,   409,
     354,     0,     0,     0,   674,   691,   498,  1060,  1062,  1063,
     659,   226,     0,   244,     0,   243,   229,   250,   498,   420,
     259,   351,   260,   257,   272,   285,   283,   279,   291,   289,
     290,   288,   269,   314,   315,   284,   280,   281,   278,   265,
       0,     0,     0,     0,   232,   250,     3,   385,     0,  1056,
     393,   394,   395,   407,     0,     0,     0,   407,     0,   359,
     355,   351,     0,     0,   238,   241,   351,     3,   251,   421,
     261,     0,     0,     0,     0,   310,   308,   305,   309,   306,
     307,   304,     3,   385,     0,     0,  1057,     0,     0,     0,
     401,     0,   410,   364,   351,  1065,   221,     0,     0,   351,
     298,   296,   293,   297,   294,   295,   292,     0,     0,   386,
       0,   413,     0,   411,     0,   413,   365,   223,   222,   228,
       0,   231,     0,   383,   414,     0,     0,   402,     0,   384,
       0,     0,     0,     0,   415,   416,     0,   412,   403,     0,
       0,   404,   417
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1862,  5803,  3026, -1862,    -1,   452,  1451,  6635,   -14, -1862,
    -344, -1862,   454, -1862,  -741, -1862,   909,  -988, -1041, -1862,
     326,  5731,  1597, -1862,  1641, -1862,  1502,   117,   924,   935,
     780,   934,  1478,  1484,  1487,  1472,  1482, -1862,    49,  -120,
    8777,  1038, -1862,  1813, -1862, -1862, -1297,  8185, -1183,  3191,
   -1862,  2411, -1862,  1026,   123, -1862, -1862,   805,   215, -1862,
   -1851, -1526,   416,   186, -1862, -1862,   799,   433, -1862, -1580,
   -1862, -1324, -1862, -1862, -1862, -1862,   232, -1125, -1862, -1862,
   -1238,   556, -1862, -1862, -1862, -1862, -1862,   217, -1224, -1862,
   -1862, -1862, -1862, -1862,   154,   575,   577,   255, -1862, -1862,
   -1862, -1862,  -612, -1862,   188,   129, -1862,   261, -1862,  -146,
   -1862, -1862, -1862,  1029,  -876,  -957,    46, -1862,     6,    30,
    1618,  2439,  -705,  -648, -1862,   -41, -1862, -1862,    67, -1862,
    -167,  2512,    50,  -261,  2646,   299,  -661,    37,   210,   148,
     709,  1024, -1862, -1862,  2265, -1862,   425,  4725, -1862,  2204,
   -1862,    48, -1862, -1862,  2375,   564,  5357,  4030,    28,  2051,
    -352, -1862, -1862, -1862, -1862, -1862,  -435,  8224,  7713, -1862,
    -404,   306, -1862,  -746,   383, -1862,   319,   870, -1862,    64,
    -165, -1862, -1862, -1862, -1862,  -127,  8583,  -948,  1011,   563,
    2765, -1862,  -378,    11,  -134,  1525,  3269,  -778,  -108,  1059,
    -283,  -372,  -251,  -190,  -496,  1468, -1862,  1817,   345,  -949,
    1687, -1862, -1862,   808, -1862, -1230,  -164,    21,  -518, -1862,
     309, -1862, -1862, -1144,   585, -1862, -1862, -1862,  2341,  -803,
    -457,  -965,   -32, -1862, -1862, -1862, -1862, -1862, -1862,   199,
    -879,  -173, -1861,   -64,  7292,   -72,  5548,  -140,  1645, -1862,
    2645,   -76,  -232,  -225,  -218,    34,   -63,   -46,   -39,   -15,
     -23,    23,    52,  -214,    58,  -213,  -212,  -196,   193,  -193,
    -168,  -115,  -795,  -723,  -711,  -698,  -732,  -138,  -671, -1862,
   -1862,  -755,  1540,  1541,  1542,  2212, -1862,   702,  6409, -1862,
    -637,  -631,  -627,  -551,  -756, -1862, -1341, -1767, -1761, -1750,
    -673,  -117,  -152, -1862, -1862,   -74,   730,  -129, -1862,  7209,
    2129,  -636,  -310
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   853,   428,   429,   177,    84,  1247,   430,   403,
     431,  1568,  1569,   432,  1002,  1003,  1004,  1362,  1363,  1364,
    1582,   454,   434,   435,   436,   736,   737,   437,   438,   439,
     440,   441,   442,   443,   444,   445,   446,   447,   456,  1150,
     738,  1496,   799,   223,   801,   450,  1038,  1248,  1249,  1250,
    1251,  1252,  1253,  1254,  2157,  1255,  1256,  1685,  2011,  2012,
    1944,  1945,  1946,  2127,  2128,  1257,  1704,  1705,  2035,  1706,
    1854,  1855,  1258,  1259,  1260,  1261,  1262,  1263,  1883,  1887,
    1519,  1511,  1264,  1265,  1518,  1512,  1266,  1267,  1268,  1269,
    1270,  1271,  1272,  1723,  2145,  1724,  1725,  2049,  1273,  1274,
    1275,  1499,  2057,  2058,  2059,  2185,  2196,  2077,  2078,   311,
     312,   940,   941,  1216,    86,    87,    88,    89,    90,  1688,
     490,   209,    94,    95,    96,    97,   239,   240,   314,   293,
     492,   458,   493,   100,   326,   102,   103,   157,   361,   317,
     107,   108,   109,   173,   110,   957,   362,   158,   113,   263,
     114,   159,   272,   364,   365,   366,   160,   451,   119,   120,
     368,   121,   611,   933,   931,   932,  1662,   122,   123,   124,
     125,  1210,  1463,  1668,  1669,  1815,  1816,  1464,  1657,  1835,
    1670,   126,   698,  1315,   169,   992,   127,   993,   994,  1560,
     965,   617,  1141,  1142,  1143,   618,   372,   501,   502,   620,
     460,   461,   224,   520,   521,   522,   523,   524,   349,  1296,
     350,   955,   953,   649,   351,   391,   352,   353,   462,   128,
     179,   180,   129,  1290,  1291,  1292,  1293,     2,  1197,  1198,
     640,  1284,   130,   339,   340,   274,   285,   594,   131,   227,
     132,   329,  1152,   584,   554,   171,   133,   398,   399,   400,
     134,   331,   243,   244,   245,   332,   136,   137,   138,   139,
     140,   141,   142,   248,   333,   250,   251,   252,   334,   254,
     255,   256,   839,   840,   841,   842,   843,   257,   845,   846,
     847,   804,   805,   806,   807,   555,  1190,  1442,   143,  1774,
     673,   674,   675,   676,   677,   678,  1818,  1819,  1820,  1821,
     663,   503,   376,   377,   378,   463,   215,   145,   146,   147,
     380,   867,   679
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      82,   359,   575,    82,   196,   537,  1297,   980,   681,   873,
     153,   214,   538,   198,   321,  1048,   392,   741,  1054,   539,
     864,  1501,   242,   540,   541,   542,  1515,   183,   591,   230,
     199,    91,   388,   526,   974,   135,   966,   200,   104,  1316,
     967,   543,  1115,   387,   544,  1488,  1926,  1323,  1089,   115,
     161,   375,  1927,   201,   465,    82,    82,  1446,    82,   533,
     373,  1279,  1366,  1928,  1276,   449,  1536,  1537,    98,   545,
     213,  1592,  1133,   499,  1135,    82,  1029,   485,   214,   747,
    2013,  2014,  1119,   247,    82,   572,   276,    58,  1126,    91,
     287,  1373,    82,   135,  2019,   537,   104,    82,   313,   202,
      82,   561,   538,  2020,    82,  1116,   205,   115,  1727,   539,
    1729,   279,  1109,   540,   541,   542,   968,   324,  2081,   212,
    1500,  -803,   546,  2073,  1110,  1205,    98,  1866,   203,   920,
     588,   543,   246,   467,   544,   275,  1556,  1111,   742,   286,
     928,   599,   681,    82,   781,  1285,    82,  1466,    82,   106,
     468,    75,   148,   283,    82,  1412,   249,   469,  1031,   545,
    1411,    82,   548,    58,  1112,   535,  1467,   225,  2027,    82,
     402,   305,  -804,   470,   198,   553,   552,  1439,   557,    91,
     213,    82,  1730,   135,   948,   565,   104,   115,   782,  1728,
    1413,   199,   582,    82,    82,    82,   509,   115,   200,  1440,
     974,   914,   916,    20,   966,    82,   313,   106,   967,  2021,
      82,   105,   546,  2013,   201,    58,    98,   225,   498,   471,
     517,   990,   691,   213,  2082,    82,   694,    75,  1153,   212,
      82,    82,    82,  1445,   448,   313,    82,    82,   242,   228,
    1449,   664,  -835,   697,  2015,   508,   313,   168,   472,   270,
    1233,   213,   548,  1458,   696,   635,   704,    82,   630,   632,
     202,   700,   582,   681,   198,    82,   596,    82,   748,   105,
     650,   624,   212,   749,   651,  2019,    82,    82,  1959,    75,
      82,   199, -1116,  1783,   968,  2074,  2075,    82,   200,   203,
     226,   253,  -835,   991,   631,   115,   549,   106,   384,  1355,
     212,    82,    82,   879,    82,  1513,   906,   681,  2019,    82,
     880,   204,    64,    82,   910,   595,  1741,   881,  1584,  1395,
    1526,   882,   883,   884,   115,   401,    82,    82,  1514,   606,
     997,   681,   868,  1380,  2072,   115,  1314,    82,   681,   885,
    1474,  1926,   886,  1473,  1013,    82,    82,  1927,   626,   316,
      82,  1731,  1119,  1052,   844,   829,   206,  1321,  1928,   105,
     115,   190,   212,   975,   283,  2055,  1345,   887,  1009,  1413,
    1555,   664,   249,   653,  1279,  1031,   485,  1276,   991,  1859,
      82,   214,  1396,   384,  1109,  2038,   549,  1031,   306,    82,
    1740,  1589,    82,  1500,  1743,    82,  1110,  2101,   912,  1447,
     269,  1941,  1942,   879,   917,  1680,  1469,  1470,   658,  1111,
     880,    58,   291,   653,   298,   162,   300,   881,  2121,  1773,
     888,   882,   883,   884,   689,  1590,   111,  1007,   692,   593,
    1010,   550,  1012,  1233,  1294,  1014,  1387,   163,  1093,   885,
     164,   165,   886,   166,  1017,   560,   499,  1019,  1020,  1021,
     927,   509,   568,    83,   631,   269,   151,   316,   298,   300,
    1513,   750,   764,   270,  1516,    82,   751,   887,  1202,   765,
     766,   899,   467,  1930,   587,    75,   966,  1314,   670,  1144,
     967,  1146,  1649,  1514,   111,   598,   316,  1517,  1943,   468,
      82,    82,   167,  1458,  1458,  1458,   469,   316,  1161,   553,
     508,   306,    82,    82,   961,  1459,   182,   627,   270,   306,
     269,    83,   470,    82,  1889,   517,   875,  1501,  1682,  1857,
     888,   550,   316,   499,  1865,   923,  1471,   184,    83,  1120,
     185,  1510,   509,  1123,    82,  1468,  -985,    83,   658,  1966,
    1967,   985,   205,  -985,  1138,  1139,    82,  1786,  1788,  1790,
      83,  2126,  1545,    83,   231,   313,   968,    83,   471,  1941,
    1942,   899,  1460,   664,   111,   116,   305,  1283,   473,   193,
      82,   989,   467,  1663,   111,   923,    82,  -616,    82,  2126,
     269,   508,   298,   300,   771,   772,  1375,   472,  2005,   468,
     232,   498,    63,    64,  1675,  1785,   469,   600,   938,  1686,
     306,    83,  2159,  1686,  1707,   900,  1766,    83,   947,  1664,
     681,  -670,   612,  1676,   269,  1860,  1500,  1707,  -670,   269,
    1861,  1885,   385,   116,  1619,   269,  1466,  1133,  1135,   980,
     773,   774,  2030,  2031,    82,  1612,    82,  1509,   220,    82,
     218,    82,   270,    78,   115,  1746,  1971,    83,    83,   221,
      82,   646,   499,  1832,    82,  1369,  1886,  1031,   482,   269,
    1833,  1990,  1327,    83,   686,   222,   300,  2103,   498,   193,
      91,   508,   111,    83,   135,  1958,  1154,   104,   961,  1834,
    1553,   647,   648,   532,    83,   534,    82,  1561,   115,    83,
      83,   359,  1031,   193,    58,   900,  1823,   499,  1675,   844,
     -23,   111,  2132,   116,  1031,    82,   959,    98,   194,  2111,
      83,   724,   111,   116,   234,  1824,  1132,  1826,    83,  1848,
    1849,  1850,  1851,  1867,  1145,  1385,  1386,  1679,  1130,  1184,
     979,   144,  1031,    83,   144,   701,    -3,   111,   703,  1596,
     901,   375,  1852,   984,  1136,  1459,  1459,  1459,    82,  2063,
      82,  1858,    82,   231,   609,    58,    82,   614,    75,    82,
     908,   232,   151,   270,  1833,   269,    83,  1931,  1930,   233,
    1078,   191,  2029,  1833,    58,   473,   593,   553,   562,    83,
      83,  1420,   553,  1925,    82,  2044,  1932,   922,   106,   144,
    1031,   258,  1935,   269,   926,   686,   300,   498,   930,  1915,
    1891,  1916,  1460,  1460,  1460,  2005,   316,    58,  1299,  2025,
      58,   116,   724,   606,  1031,   289,  1901,  1872,   278,    75,
     290,  1401,  1861,   294,  2134,   299,  -492,  1979,   303,    82,
     901,   593,  1980,   695,    82,   144,    82,  1349,    75,   638,
     116,   741,   498,   553,  1350,    58,   269,    58,    82,  2092,
     105,   116,  1024,  1581,  1577,   301,   359,   305,   818,    82,
    1327,  1317,   553,  1025,  1026,   517,   325,   963,    82,   306,
     269,    75,  -824,  2116,    75,   269,   116,   269,  2117,   144,
     959,  1057,  1058,  1059,   752,  2174,   753,   754,   755,  1145,
    2175,  1094,   390,   485,  1117,   553,  1410,   448,   668,   347,
     269,    82,   269,   269,   605,    64,   375,   193,  1812,    75,
      58,    75,  1149,  1825,   269,  -986,    58,   756,    83,  1767,
     757,   758,  -986,  1535,   393,   759,   760,   269,  1775,  1124,
     305,  1172,   473,   668,   553,   553,   269,    82,    82,   517,
     401,  1782,   259,   260,    83,   261,    14,    15,    16,    17,
      18,   262,   869,   870,   681,   705,   871,  1287,   269,   706,
     686,   300,   742,   474,    58,  1417,    83,   562,    83,   892,
      91,   553,  1715,   475,    75,  1394,   844,   104,    58,   289,
      75,  1506,   269,   686,   476,  1707,   783,    83,   115,   269,
     784,  1712,   809,    82,  1176,   477,   810,   478,   553,    83,
    1180,   638,   505,   473,   553,   553,    58,    98,   479,   150,
    1298,   175,   176,    65,    66,    67,    68,    69,    70,    71,
      72,   111,   512,    83,   654,   301,   289,    58,    75,    83,
    1117,   482,   473,   506,   668,    14,    15,    16,    17,    18,
     163,   963,    75,   164,   165,    82,   166,  -493,  1423,    82,
    1547,  1656,   553,   511,  1570,   767,   768,    14,    15,    16,
      17,    18,  1427,   811,   525,   111,   553,   706,    74,   527,
      75,   290,   530,   685,   531,   299,  1868,   551,    82,   822,
    1781,   517,    58,   553,  1507,   573,   192,    83,   106,    83,
     667,    75,    83,   228,   668,    58,    14,    15,    16,    17,
      18,    80,   669,   579,   937,   996,    82,   958,   938,   651,
     270,  1431,    82,    82,   998,   553,  1528,    58,   651,   999,
    1438,   725,   574,   706,  1370,   271,  1204,  1030,   769,   770,
    1902,  1031,   392,   392,   586,    58,  1465,   292,   295,   983,
    1098,  2079,   225,  1827,   553,  1911,    75,    82,   775,   776,
    1278,    14,    15,    16,    17,    18,    58,    74,   597,    75,
     116,   625,  -494,  1565,  1158,   359,  1543,    58,   810,   621,
     668,  2079,    14,    15,    16,    17,    18,   641,   305,   802,
     271,    75,   553,   553,   642,   664,   384,  1640,  1552,   688,
      80,    81,    63,    64,   505,   744,   562,   656,  1145,    75,
     553,  1042,  2129,  1044,   116,  1047,  1149,  1287,   699,  1193,
    1195,    58,  1055,  1031,  1031,   375,  1809,  1810,  1811,  1597,
      75,   517,   725,   553,    82,    82,    82,  1848,  1849,  1850,
    1851,    75,    58,   517,  1326,   271,   702,  1080,  1327,    58,
    1031,   359,  2061,    78,    14,    15,    16,    17,    18,    91,
    1852,  1606,   517,   638,   707,   553,   104,   553,    82,  -193,
    1365,   269,   276,   287,  1327,  1995,  1534,   115,   708,  1997,
     810,  1622,   269,    82,  1905,    75,    82,    82,   712,    82,
     715,   269,   482,    91,   279,    82,    98,    83,    82,    83,
     104,   375,  1627,  1628,  1136,    58,    75,   709,  1136,   716,
    1136,   115,   289,    75,    58,   271,   720,  1848,  1849,  1850,
    1851,   275,   286,  1718,  1719,  1720,  1721,  1722,    83,   744,
      98,    83,   763,  1610,  1778,   778,   283,   668,  1779,    74,
    1852,   777,    82,    14,    15,    16,    17,    18,  1840,   271,
     779,  1844,  1327,  1869,   271,  1031,   780,  1031,   785,   517,
     271,   667,   979,   481,    83,   668,  1870,  1871,    82,    75,
     810,  1031,    80,   669,   812,   111,    74,   106,    75,  1936,
     144,   359,    74,   810,   813,   670,  1465,  1465,  1465,  2066,
     394,  1658,  1465,   553,   271,  1672,   814,   269,  1813,   815,
    1214,    82,   553,    58,   802,   830,   744,    74,   553,    80,
      81,   106,   816,  1984,  1145,    80,    81,  1031,    14,    15,
      16,    17,    18,   269,  2022,    74,  1022,   744,  1031,   667,
    1593,   375,  2119,   668,  2120,   270,  1327,   817,  1031,  1278,
      80,   669,   825,   537,  1689,   827,   726,  1813,  1689,  1771,
     538,   553,   848,  1848,  1849,  1850,  1851,   539,    80,    81,
      -3,   540,   541,   542,   593,  -495,   395,    75,  -496,   153,
      82,   448,   448,  1278,    82,    82,  1852,  2193,    58,   543,
     850,  2190,   544,   852,   150,  1853,   175,   176,    65,    66,
      67,    68,    69,    70,    71,    72,   517,   186,     6,     7,
       8,     9,    10,    11,    12,    13,  1570,   545,    83,  2199,
     -18,  2147,    83,  2200,   116,  2151,   596,  1836,  1836,  1836,
     517,   517,   104,  1450,  1451,  1452,   104,   104,   271,  1673,
      82,  1687,   865,   115,   396,  1687,   866,   115,   115,   874,
     104,    83,    75,  -497,    83,   656,   744,   726,  1033,  1034,
     877,   115,  -129,  -129,  -129,  -129,  -129,  -129,   889,   288,
     546,  1064,  1065,  1066,  1067,   595,    82,  1031,  1371,    83,
    1352,  1353,  1367,  1368,   903,    83,    83,  -164,  -164,  1287,
     206,   744,  1509,  1510,   890,   517,   269,   891,  1672,  1587,
    1588,   904,    82,  1672,  1591,  1588,   893,    82,    82,    82,
     894,   548,   895,  1828,  1595,  1588,   879,   896,    85,   897,
      83,   152,   271,   880,   898,   269,  1106,  1580,   448,   318,
     881,   269,  1629,  1580,   882,   883,   884,  1106,  1641,    92,
    1791,  1353,   154,   106,   924,   271,   925,   106,   106,  1913,
    1353,  -614,   885,  1914,  1588,   886,  1923,  1031,  -612,   271,
     934,   106,  1982,  1983,   111,  -128,  -128,  -128,  -128,  -128,
    -128,   935,   271,    82,  1674,   939,    85,   936,    82,   942,
     887,  2000,  1588,   950,    82,   952,    82,  2001,  1588,   593,
     144,   956,   518,   195,    82,  2190,  2191,    92,   111,  1941,
    1942,   144,    85,   271,   969,  1691,  1585,  1586,   971,  1691,
    1691,  1060,  1061,   517,   670,   238,   987,   279,   266,   681,
     517,   995,    85,  1691,  1062,  1063,  1006,   271,  1032,  1068,
    1069,  1035,  1673,   888,   271,  1040,  1879,  1673,  1082,   267,
    1077,  1992,  1105,   280,  1106,   549,   614,   392,  1113,    83,
      83,   517,   500,  1134,    58,  1742,  1744,  1837,  1838,   283,
     152,    83,  1137,  1156,  1160,  1163,    85,  1164,  1165,   152,
    1166,  1185,   328,   336,  1167,   899,   830,  1287,  1168,  1169,
    1280,   269,  1170,  1171,  1192,   358,  1194,    92,   150,   517,
     235,   236,    65,    66,    67,    68,    69,    70,    71,    72,
    1196,  -807,  1200,   116,  1207,  1566,    82,  1208,    82,  1209,
     455,  1295,   195,   195,  1286,  1307,  1308,  1309,    75,  1310,
    1498,  1022,    83,   152,   488,  1325,  1320,  1318,   266,  1324,
    1328,    83,  1329,  1330,  1331,  1333,  1334,   116,  1392,    77,
     269,  2010,  1335,  1336,  1338,  1337,  1340,    82,  1341,   328,
      82,  1342,  1347,  1348,   238,   238,  1372,  1356,   270,   517,
     517,  1357,  1376,  1377,    83,  1378,   517,  1674,  1402,  1379,
    1383,  1384,  1674,  1388,  1389,   328,  1390,  1391,  1399,   517,
     550,  1404,  1672,    85,  1978,  1416,  1405,  1406,  1408,   517,
    1443,   517,  -808,  1523,  1475,  1476,   537,  2110,   266,  1479,
    1480,  1489,  1490,   538,   517,  1491,   517,   517,   517,   900,
     539,  1493,    82,    82,   540,   541,   542,   -22,   104,   592,
     111,  1502,  2052,  1031,   111,   111,  1524,  1554,  1503,   115,
    1558,   328,   543,    83,  1530,   544,  1559,   336,   111,  1562,
    1532,  1576,  1580,   336,   328,   328,  1601,  1594,  1602,  1188,
    1605,   144,  1616,   152,  1617,  1588,  1618,    82,  1620,    83,
     545,  1624,  1625,  1630,   517,  1647,  1633,  1637,   517,   144,
    1638,  1639,  2124,   517,  2010,   358,   671,   680,    14,    15,
      16,    17,    18,  1028,  1646,   862,  2056,   518,  1650,  1693,
      19,   358,  1661,    83,  2052,   358,    14,    15,    16,    17,
      18,  1344,  1468,   144,  1510,  1708,   271,  1709,  1711,   269,
     448,   593,  1713,   546,   150,  2149,  1673,  1733,    65,    66,
      67,    68,    69,    70,    71,    72,  1045,   517,   144,   106,
      48,    49,    50,    51,    52,    53,    54,    55,   455,   517,
    1726,  1734,  1735,   517,   901,  1736,  2050,  1737,    83,  1233,
    1747,  1748,    82,   548,  1750,    83,   635,  1752,   517,   116,
      83,    83,    83,   116,   116,   198,  1753,  1754,  1046,    82,
     717,    82,   455,  1777,   104,   803,  1755,   116,  1756,  1762,
    1764,  1765,   199,   195,  1772,   115,  2192,  1780,  1776,   200,
     899,  1691,  1784,  1792,  1794,   761,   762,  1795,   473,  1807,
     517,   152,  1798,  1629,   104,   488,  1800,  1802,  1803,   837,
    1804,   680,  1808,  1822,  1666,   115,   761,   226,  2050,  2056,
    1841,  1845,   152,  2056,  2056,   500,    83,    82,    82,  1847,
    1876,    83,  1878,  1895,  1903,   104,  1896,    83,  1899,    83,
     517,   448,  1900,   448,   455,   517,   115,  1907,  1912,   761,
     269,  1674,   238,  1917,  2172,  1920,  1921,  1922,  1949,  1954,
    1970,  1955,   269,   212,   238,    82,  1968,  1975,  1977,  1986,
    1981,  1993,  1996,   517,  1998,  1994,   517,  2184,   517,  1999,
    2002,  2184,  2008,  2003,   448,   106,  2004,   549,   328,   553,
     455,   455,  2024,  2026,   328,  2037,  2194,   358,  2039,   517,
     508,  2032,   500,   144,  2053,  2064,  2065,  2045,  2054,  2076,
      82,  2100,  2085,  2169,  2102,   106,  2104,  2114,  2115,    82,
    2113,  2118,  2135,  2154,   900,   144,  2144,  2148,  2131,   144,
     144,  2155,  2133,  2150,   217,  2160,  2186,  2170,  2171,  2173,
    2179,  2181,    83,   144,  2187,  2182,   106,  1691,   269,  2197,
    2198,  2201,   328,  1909,   328,   448,  1564,    85,  1027,    83,
     150,    83,   235,   236,    65,    66,    67,    68,    69,    70,
      71,    72,  1073,   358,   488,  1070,   680,  1691,    92,   269,
     300,   394,  1071,  1074,   671,   150,  1072,   800,   671,    65,
      66,    67,    68,    69,    70,    71,    72,   358,  1497,  1505,
      83,   217,  2180,    83,  1695,  1972,   111,   680,  1691,  2125,
     358,  2142,  1717,   144,  1965,  2122,  1888,  2168,  1874,   152,
    1875,   271,   550,  2106,  2188,   384,  2152,   455,  2105,  1522,
     455,   500,   152,   152,   174,   455,    77,   518,   296,   861,
     862,  2007,   464,   585,   455,  2070,  1660,   152,   152,   152,
     271,  1557,  1904,  1520,  1155,  1749,  1892,   395,   954,   901,
     872,     3,  1085,  1086,  1087,  1806,    83,     0,     0,     0,
     269,  1000,     0,     0,     0,   150,   500,   175,   176,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,   500,     0,   500,     0,     0,
       0,   500,   500,   488,   500,     0,     0,     0,     0,     0,
      83,     0,     0,     0,     0,   583,     0,     0,     0,   803,
     803,   500,     0,     0,  1056,     0,     0,   455,   150,     0,
       0,   258,    65,    66,    67,    68,    69,    70,    71,    72,
    1358,   144,     0,     0,  1359,   116,  1360,   358,   488,     0,
       0,     0,   837,     0,   837,     0,     0,     0,     0,     0,
      93,     0,   111,    93,     0,   269,     0,   358,     0,   358,
       0,     0,     0,   358,   358,   358,   358,     0,     0,    77,
     564,  1212,  1583,     0,     0,   583,     0,  1644,     0,     0,
     500,     0,   111,   358,     0,     0,     0,     0,     0,     0,
       0,   144,     0,     0,   150,  2093,   271,   666,    65,    66,
      67,    68,    69,    70,    71,    72,     0,  1361,    93,   328,
       0,  1361,    83,   111,  2112,     0,     0,     0,     0,     0,
       0,     0,     0,    99,     0,   150,   155,   235,   236,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
    1361,     0,     0,   518,  1392,    77,     0,   455,     0,     0,
     268,     0,   358,    74,    93,   271,   307,     0,   152,   455,
       0,     0,     0,     0,     0,     0,     0,   358,    92,  1302,
      83,    83,     0,     0,     0,  1665,    77,     0,     0,  1288,
     671,    99,  1666,     0,     0,     0,    80,    81,     0,     0,
       0,   116,   496,     0,   217,     0,     0,     0,    93,     0,
       0,   144,     0,     0,     0,   337,   150,   210,    83,  1361,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   116,     0,     0,     0,     0,     0,   666,     0,   152,
     488,     0,     0,     0,     0,     0,     0,     0,   500,   500,
       0,     0,     0,     0,     0,     0,     0,     0,   529,     0,
       0,     0,   116,  2195,     0,     0,   491,   101,     0,     0,
     156,    99,  2202,     0,     0,   319,   464,     0,     0,  1339,
       0,    99,     0,     0,   210,  1343,     0,     0,     0,     0,
     808,     0,     0,     0,     0,     0,  1351,     0,     0,     0,
       0,     0,     0,     0,   500,   803,     0,   820,     0,     0,
     823,   186,     6,     7,     8,     9,    10,    11,    12,    13,
     358,   358,   464,   464,   837,   101,     0,     0,     0,     0,
       0,   837,     0,     0,   271,   341,     0,     0,   483,     0,
     268,   197,   623,   342,   343,   344,   345,     0,     0,     0,
       0,   211,     0,   661,     0,     0,   684,     0,     0,     0,
       0,     0,     0,   241,     0,     0,     0,   144,     0,   661,
       0,   281,   564,   661,     0,     0,   358,     0,     0,    99,
       0,     0,     0,     0,   150,     0,   175,   176,    65,    66,
      67,    68,    69,    70,    71,    72,   577,   144,   581,     0,
       0,     0,     0,     0,     0,   315,     0,     0,    99,   320,
       0,     0,     0,     0,     0,   101,     0,     0,   152,    99,
     330,   518,     0,     0,     0,     0,     0,   152,   144,   981,
    1361,     0,     0,     0,   360,     0,   455,     0,   346,  1288,
       0,     0,   155,     0,    99,     0,     0,     0,     0,     0,
       0,   464,   740,     0,     0,     0,   347,    92,     0,  1008,
       0,   466,   455,     0,     0,   464,     0,  1015,   581,     0,
     455,     0,   320,   494,     0,     0,     0,     0,     0,     0,
     717,     0,     0,   661,     0,     0,     0,   271,     0,     0,
       0,    92,     0,     0,   266,    85,     0,   330,     0,     0,
       0,     0,   536,   241,   547,     0,   328,     0,     0,     0,
       0,     0,   152,   315,     0,   267,   280,     0,     0,   488,
       0,     0,     0,   330,   571,     0,     0,     0,     0,   576,
     578,     0,   211,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   315,     0,     0,     0,     0,   500,     0,   488,
     500,   500,     0,   315,   152,     0,   601,     0,   518,   464,
     603,     0,     0,   761,     0,   604,     0,     0,     0,     0,
       0,   615,     0,     0,     0,   496,   578,     0,   315,   330,
       0,     0,   628,     0,     0,     0,     0,   210,     0,     0,
       0,   907,   636,   330,   637,     0,     0,     0,     0,   911,
       0,  1571,  1572,  1573,     0,     0,     0,     0,  1574,  1575,
       0,     0,     0,     0,     0,     0,   828,     0,   921,   358,
       0,     0,   358,   358,   659,   358,     0,   683,     0,   929,
       0,     0,     0,     0,   808,   808,     0,   483,     0,   491,
     690,     0,     0,     0,   690,  1096,     0,   518,  1099,     0,
       0,   661,   496,     0,  1361,     0,     0,     0,     0,  1361,
    1361,  1361,    14,    15,    16,    17,    18,   152,   152,   152,
     152,     0,   152,   152,     0,   661,     0,     0,  1667,   336,
       0,     0,     0,     0,     0,     0,     0,     0,   661,     0,
     616,   154,   455,     0,     0,   271,   455,   455,     0,    93,
       0,   464,     0,     0,     0,     0,     0,   455,     0,     0,
     455,     0,     0,    92,   564,     0,   491,    92,    92,     0,
       0,  1174,    58,     0,     0,  1178,     0,     0,    99,  1182,
       0,    92,     0,     0,     0,   219,     0,     0,   266,     0,
       0,   740,     0,     0,   740,     0,     0,     0,     0,   740,
     320,     0,     0,     0,   659,   488,   150,   838,   740,   592,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   320,    99,     0,     0,     0,   304,   740,     0,     0,
     152,   496,   671,     0,    74,     0,    75,     0,   150,     0,
     175,   176,    65,    66,    67,    68,    69,    70,    71,    72,
     878,  1288,     0,     0,     0,     0,    76,    77,     0,     0,
       0,     0,   241,     0,     0,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,   661,   496,     0,   615,     0,
       0,     0,     0,     0,     0,     0,   330,     0,     0,     0,
       0,   518,   330,     0,     0,   491,   494,     0,     0,     0,
       0,     0,     0,   496,     0,     0,     0,     0,  1361,     0,
    1361,     0,   315,  1481,     0,     0,     0,     0,     0,     0,
       0,  1667,  1814,     0,     0,     0,  1667,     0,   455,     0,
       0,     0,  1667,     0,  1667,     0,     0,   150,     0,     0,
     491,    65,    66,    67,    68,    69,    70,    71,    72,  1358,
     946,     0,   330,  1359,   615,  1360,   101,   336,   152,   491,
       0,   491,     0,     0,     0,   491,   491,     0,   491,     0,
     808,     0,   690,   962,     0,     0,     0,     0,   267,   280,
       0,     0,     0,   615,     0,   491,     0,   973,    77,  1191,
       0,    58,     0,     0,     0,     0,   659,   909,     0,     0,
       0,   982,     0,  1199,     0,   661,     0,  1203,   684,   690,
       0,  1206,     0,     0,     0,     0,     0,     0,   152,     0,
     633,     0,     0,     0,     0,   150,     0,   235,   236,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,  1288,
       0,     0,     0,     0,     0,  1425,   152,  1882,  1429,    93,
       0,     0,  1433,    74,   491,    75,     0,     0,     0,     0,
      93,     0,     0,     0,     0,     0,     0,     0,   496,     0,
       0,     0,     0,   616,     0,  2108,    77,     0,     0,   553,
    1814,  1814,     0,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,   464,  1667,     0,     0,  1667,     0,
       0,   981,   494,     0,     0,     0,     0,     0,     0,     0,
     336,     0,     0,     0,     0,     0,     0,     0,   615,  1088,
       0,     0,    99,     0,     0,     0,     0,     0,   455,     0,
       0,   592,    19,  1289,   615,     0,     0,     0,   615,     0,
       0,     0,     0,     0,     0,     0,   690,   962,     0,    92,
    1108,   615,   838,  1114,     0,     0,     0,     0,     0,     0,
       0,   328,     0,     0,     0,     0,   494,     0,   494,     0,
       0,     0,   494,   494,   360,   494,    52,    53,    54,    55,
       0,     0,     0,   832,     0,   834,     0,     0,     0,     0,
       0,     0,   494,     0,   851,     0,  1814,     0,     0,     0,
       0,     0,     0,     0,   483,  1667,     0,     0,     0,     0,
       0,     0,   491,   491,     0,     0,     0,   330,     0,     0,
       0,   150,     0,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,  1049,     0,     0,     0,  1084,     0,     0,
       0,     0,     0,   152,   619,     0,     0,     0,     0,     0,
       0,     0,   615,  1101,     0,     0,  1277,  1102,     0,  1599,
       0,   494,     0,     0,     0,     0,     0,   156,   491,     0,
    1608,     0,     0,  1814,     0,  1050,   690,     0,     0,  1306,
       0,     0,     0,     0,   152,  1448,  1312,     0,     0,   464,
       0,     0,     0,  2060,     0,     0,     0,     0,     0,  1472,
       0,     0,     0,     0,     0,    92,     0,     0,     0,     0,
      93,     0,     0,     0,   152,   152,     0,  2109,   336,  1494,
       0,     0,     0,     0,     0,     0,   740,     0,    93,     0,
       0,     0,     0,     0,     0,    92,     0,     0,   320,   360,
       0,     0,     0,     0,     0,   152,     0,   661,     0,    14,
      15,    16,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,    93,   150,     0,   918,    92,    65,    66,    67,
      68,    69,    70,    71,    72,  2109,  2109,   496,     0,     0,
       0,     0,     0,  1289,     0,     0,   268,    93,     0,     0,
       0,     0,  1461,     0,     0,     0,     0,     0,     0,     0,
       0,    99,     0,     0,     0,     0,     0,     0,     0,    58,
     615,     0,     0,  2109,   615,     0,     0,     0,     0,   494,
     494,     0,  1108,   615,     0,    58,     0,     0,  1393,   838,
       0,     0,     0,   615,     0,    99,     0,     0,     0,     0,
     615,     0,     0,   150,     0,   235,   236,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,   150,
     464,   235,   236,    65,    66,    67,    68,    69,    70,    71,
      72,    74,     0,    75,   150,   494,   175,   176,    65,    66,
      67,    68,    69,    70,    71,    72,     0,    74,   615,    75,
       0,     0,   615,   835,    77,     0,   615,   668,     0,     0,
       0,   619,     0,     0,    80,   836,     0,     0,     0,   237,
      77,   491,     0,     0,   491,   491,     0,   156,     0,     0,
      80,    81,     0,     0,     0,     0,  1462,     0,     0,  1381,
       0,     0,     0,  1382,     0,  1277,     0,  1817,     0,     0,
       0,     0,     0,     0,  1129,     0,     0,     0,     0,  1483,
    1681,  1683,  1397,     0,     0,     0,     0,     0,     0,  1398,
       0,     0,    93,     0,     0,     0,     0,     0,     0,  1277,
       0,     0,     0,     0,     0,     0,     0,   619,     0,     0,
       0,     0,     0,   496,    93,     0,     0,     0,    93,    93,
       0,     0,     0,     0,  1521,     0,     0,     0,     0,     0,
       0,  1745,    93,     0,   330,     0,   619,  1435,     0,     0,
       0,  1436,     0,     0,     0,  1437,     0,     0,   659,     0,
       0,     0,     0,     0,     0,     0,     0,   576,     0,     0,
     268,     0,  1461,  1461,  1461,   155,  1654,  1655,  1659,  1281,
    1282,     0,     0,     0,     0,     0,   615,     0,   360,     0,
       0,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,    99,    99,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    93,     0,     0,    99,     0,   150,     0,   382,
     383,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,  1817,  1817,     0,     0,     0,
       0,   118,     0,     0,   118,     0,   150,  1613,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,   494,     0,
       0,   494,   494,     0,   360,     0,     0,     0,     0,     0,
      78,     0,     0,     0,     0,  1354,     0,     0,     0,   615,
       0,   619,   384,   615,     0,  1289,     0,   615,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   619,     0,   118,
       0,   619,     0,     0,   644,     0,  1462,  1462,  1462,   156,
     578,     0,     0,     0,   619,     0,  1671,  1374,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,  1690,     0,     0,     0,  1690,  1690,     0,     0,   268,
      93,   273,     0,  1873,     0,   118,     0,     0,     0,  1690,
     150,  1817,   175,   176,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,  1400,   150,  1403,   204,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   118,
    1407,     0,  1409,   118,     0,     0,     0,  1414,  1415,   118,
       0,     0,   118,     0,   360,     0,   273,  1422,  1634,   615,
      93,     0,  1635,     0,     0,     0,  1636,   354,   118,     0,
     386,  1485,     0,     0,     0,  1213,    58,     0,    77,   156,
       0,   861,     0,  1441,  2068,     0,  1444,     0,  1817,     0,
       0,     0,   150,   459,   605,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,   118,   459,     0,     0,
     150,   273,     0,   615,    65,    66,    67,    68,    69,    70,
      71,    72,   615,     0,     0,     0,   615,     0,     0,     0,
       0,     0,  1817,  1289,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,  1079,   118,     0,  1504,
       0,     0,   268,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,     0,   118,     0,   118,     0,     0,  1671,
      93,    80,    81,     0,  1671,     0,   118,     0,  1758,  1525,
    1829,   273,  1671,  1831,     0,     0,  1529,   118,  1531,  1533,
    1817,  1817,     0,     0,     0,     0,     0,  1539,     0,  1540,
       0,  1541,   610,     0,     0,   118,     0,  1843,  1550,     0,
     118,     0,   118,     0,     0,   273,   118,     0,     0,     0,
     273,   661,     0,     0,     0,     0,   273,     0,  1817,     0,
       0,     0,  1793,   619,     0,     0,   118,   619,     0,     0,
       0,  1796,     0,    99,     0,  1797,   619,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   619,     0,   118,     0,
     273,   118,     0,   619,     0,     0,     0,   156,    14,    15,
      16,    17,    18,     0,   118,     0,     0,     0,   118,     0,
     150,  1603,  1604,  1478,    65,    66,    67,    68,    69,    70,
      71,    72,  1358,   661,     0,  1492,  1359,     0,  1360,     0,
       0,     0,     0,     0,     0,     0,     0,  1626,     0,     0,
       0,   619,     0,     0,  1631,   619,  1632,     0,     0,   619,
       0,   459,     0,     0,     0,     0,    93,     0,    58,     0,
    1929,    77,     0,     0,  1787,     0,     0,     0,     0,     0,
       0,     0,  1648,  1937,     0,     0,  1671,     0,     0,     0,
       0,     0,     0,     0,     0,   459,    93,     0,     0,     0,
       0,     0,   150,     0,   235,   236,    65,    66,    67,    68,
      69,    70,    71,    72,  1960,     0,     0,  1690,     0,     0,
       0,     0,     0,     0,   118,     0,     0,    93,   459,     0,
      74,     0,    75,     0,   273,     0,     0,     0,     0,    99,
       0,     0,     0,     0,     0,   118,     0,     0,     0,   330,
       0,     0,  2108,    77,     0,     0,   553,     0,     0,     0,
       0,     0,     0,    80,    81,     0,     0,   459,     0,    99,
       0,     0,     0,     0,     0,     0,     0,     0,  1757,     0,
       0,     0,     0,     0,     0,  1761,     0,  1763,     0,     0,
       0,  2183,     0,  1671,     0,     0,     0,     0,     0,     0,
      99,     0,   118,  2189,     0,    58,     0,     0,     0,     0,
       0,     0,     0,   459,   459,     0,     0,     0,   273,   150,
     118,   607,   608,    65,    66,    67,    68,    69,    70,    71,
      72,     0,  2051,     0,     0,     0,   118,     0,     0,   150,
       0,   235,   236,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,   273,     0,     0,     0,     0,
       0,     0,     0,  1799,     0,     0,     0,    74,   273,    75,
       0,     0,    78,  1690,     0,     0,     0,     0,   118,     0,
     118,     0,     0,    14,    15,    16,    17,    18,     0,   327,
      77,     0,     0,     0,     0,   386,   118,   459,     0,   273,
      80,    81,   619,  1690,  2051,   330,   619,   118,     0,     0,
     619,     0,  1738,  1739,     0,     0,     0,     0,     0,     0,
     118,     0,   615,   273,     0,     0,     0,   610,     0,     0,
     273,     0,     0,   118,  1690,     0,   112,     0,     0,     0,
       0,     0,   118,    58,     0,     0,     0,     0,     0,     0,
     459,     0,     0,   459,     0,   118,   118,     0,   459,     0,
       0,     0,     0,   636,   330,  2146,     0,   459,     0,     0,
     118,   118,   118,     0,     0,     0,     0,   150,     0,   235,
     236,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,   112,  1897,  1898,     0,     0,     0,
       0,   330,     0,     0,     0,    74,     0,    75,   411,  1906,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   619,     0,     0,     0,   459,   237,    77,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,    81,
     282,  2123,   118,     0,     0,     0,     0,     0,     0,     0,
     459,     0,     0,    14,    15,    16,    17,    18,   118,     0,
     746,     0,   118,    78,   422,     0,     0,     0,     0,     0,
     118,   459,     0,     0,   112,   118,   619,     0,     0,     0,
       0,     0,     0,     0,   112,   619,     0,     0,     0,   619,
     118,     0,   118,     0,     0,  1846,   118,   118,   118,   118,
       0,     0,  1856,   363,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,   118,     0,     0,     0,
       0,   150,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,  1881,     0,     0,     0,     0,     0,     0,
       0,     0,   495,     0,     0,  1189,     0,   150,     0,   235,
     236,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,   150,     0,   235,   236,    65,    66,    67,    68,
      69,    70,    71,    72,    78,    74,   118,    75,     0,     0,
     459,     0,   112,     0,     0,   118,     0,     0,     0,     0,
      74,   118,   459,     0,     0,     0,     0,   327,    77,     0,
     118,     0,  1304,   459,     0,     0,     0,     0,    80,    81,
       0,   112,   835,    77,     0,     0,   668,     0,     0,     0,
       0,     0,   112,    80,   836,   602,     0,     0,     0,     0,
    1319,     0,     0,     0,     0,     0,   670,     0,     0,     0,
     363,  1939,  1940,     0,     0,     0,     0,   112,  1950,     0,
       0,   282,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1964,   118,   459,     0,     0,     0,     0,     0,     0,
       0,  1973,   150,  1974,   175,   176,    65,    66,    67,    68,
      69,    70,    71,    72,     0,  2107,  1985,     0,  1987,  1988,
    1989,     0,     0,   660,   150,     0,   282,     0,    65,    66,
      67,    68,    69,    70,    71,    72,  1358,     0,     0,   660,
    1359,     0,  1360,   660,     0,     0,   228,     0,     0,     0,
       0,   150,   506,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   118,     0,     0,     0,   118,     0,
       0,     0,  2143,   118,   118,    77,  2018,   118,  1789,    58,
    2023,     0,     0,     0,     0,  2028,     0,   118,     0,     0,
       0,     0,     0,  2158,   118,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,     0,     0,     0,  2167,     0,
       0,     0,     0,   150,     0,   235,   236,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2071,
       0,    74,   118,    75,     0,     0,   118,     0,     0,     0,
     118,  2080,     0,   660,     0,  2083,     0,     0,    58,     0,
       0,     0,     0,  1665,    77,     0,     0,     0,     0,     0,
    2099,   118,     0,     0,    80,    81,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,   459,
       0,     0,   150,     0,   235,   236,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,  2130,     0,     0,   459,     0,     0,     0,     0,
      74,     0,    75,   459,     0,     0,     0,   363,     0,     0,
     150,     0,   235,   236,    65,    66,    67,    68,    69,    70,
      71,    72,  1665,    77,     0,   495,     0,   273,   118,     0,
       0,     0,  2153,    80,    81,     0,     0,  2156,    74,     0,
       0,   112,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,   459,     0,     0,   619,  1304,     0,     0,     0,
    1665,    77,     0,     0,     0,  2176,     0,  1666,  2178,     0,
    2156,    80,    81,     0,     0,     0,     0,     0,   117,     0,
     118,     0,   459,   363,     0,   112,     0,   118,     0,     0,
       0,  2178,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   660,   495,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,   150,     0,   235,   236,    65,    66,    67,
      68,    69,    70,    71,    72,   660,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   117,     0,   660,     0,
     150,    74,   175,   176,    65,    66,    67,    68,    69,    70,
      71,    72,   118,     0,     0,   118,   118,     0,   118,     0,
       0,     0,     0,  2108,    77,     0,     0,   553,     0,     0,
       0,     0,     0,   118,    80,    81,     0,   118,     0,     0,
       0,   118,   284,     0,     0,     0,     0,     0,     0,     0,
     511,     0,     0,  1645,     0,     0,     0,     0,     0,     0,
     118,   118,   118,   118,   118,   118,   118,     0,     0,     0,
       0,     0,   273,     0,     0,     0,   117,     0,     0,     0,
       0,     0,     0,     0,     0,   459,   117,     0,     0,   459,
     459,   495,     0,     0,     0,     0,     0,     0,     0,     0,
     459,     0,     0,   459,     0,   367,     0,   363,     0,   150,
       0,   235,   236,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,   363,     0,     0,     0,   363,     0,     0,
       0,   273,     0,     0,     0,   660,   495,    74,     0,     0,
     363,     0,     0,     0,   497,     0,     0,     0,   459,     0,
       0,     0,     0,   118,     0,   363,     0,   363,     0,   237,
      77,   363,   363,   495,   363,     0,  1768,     0,     0,     0,
      80,    81,     0,   118,     0,     0,   172,     0,     0,     0,
       0,   363,     0,     0,   117,     0,   150,     0,   235,   236,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,   172,     0,     0,   118,     0,     0,
       0,     0,     0,   117,    74,     0,   118,     0,     0,     0,
     118,     0,     0,     0,   117,   787,   788,   789,   790,   791,
     792,   793,   794,   795,   796,   797,   327,    77,     0,     0,
     220,   363,   367,     0,     0,   112,     0,    80,    81,   117,
     363,   172,   150,   284,   175,   176,    65,    66,    67,    68,
      69,    70,    71,    72,   172,   660,   172,   798,   282,     0,
       0,   459,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   662,     0,   172,   284,   389,
     273,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   662,     0,     0,     0,   662,     0,     0,     0,     0,
       0,     0,     0,     0,   389,     0,     0,     0,   495,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,   172,     0,     0,     0,   172,
       0,     0,   172,   172,     0,     0,   172,     0,     0,   172,
     172,     0,   172,     0,   172,     0,   149,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,   363,     0,     0,     0,     0,   363,   363,
       0,     0,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,   662,     0,     0,     0,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   273,     0,   172,     0,     0,   172,     0,
       0,     0,   207,     0,   363,     0,     0,     0,   459,     0,
       0,   459,     0,     0,     0,     0,     0,   363,     0,     0,
       0,   363,     0,   172,     0,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   433,     0,   172,   367,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   497,     0,     0,
       0,     0,     0,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,   117,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   112,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2036,
     273,     0,     0,     0,     0,   367,     0,   117,     0,     0,
       0,     0,     0,   282,     0,     0,   118,     0,     0,     0,
       0,   207,     0,   662,   497,     0,     0,     0,     0,     0,
       0,     0,     0,   172,   367,     0,     0,   660,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   662,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   118,     0,     0,
     662,     0,     0,     0,     0,   363,     0,   495,     0,     0,
       0,     0,     0,  2094,     0,     0,     0,     0,   580,     0,
       0,     0,     0,     0,     0,     0,     0,   118,   118,     0,
       0,   273,     0,   389,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   118,     0,     0,   172,
       0,     0,     0,     0,     0,     0,     0,     0,   118,   622,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   629,     0,     0,     0,     0,     0,   363,     0,     0,
     363,   363,     0,   363,     0,   711,     0,   714,   580,     0,
     433,   719,     0,   497,     0,     0,     0,     0,   363,     0,
     728,   729,   363,     0,     0,     0,   363,     0,     0,   367,
     657,     0,     0,     0,     0,   433,   433,     0,     0,     0,
       0,     0,     0,     0,     0,   367,     0,     0,     0,   367,
       0,     0,     0,   389,     0,     0,   433,   662,   497,     0,
       0,     0,   367,     0,     0,     0,     0,     0,     0,     0,
     112,     0,     0,     0,   112,   112,     0,   367,     0,   367,
       0,     0,     0,   367,   367,   497,   367,     0,   112,   433,
       0,     0,     0,   172,   172,   745,     0,     0,     0,     0,
       0,     0,     0,   367,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   172,     0,   172,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   786,     0,     0,     0,
       0,     0,     0,   495,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   826,     0,     0,     0,     0,   831,
       0,     0,     0,   367,     0,     0,     0,   117,     0,     0,
       0,     0,   367,     0,     0,     0,     0,     0,     0,   857,
     858,     0,     0,     0,   859,   860,     0,   662,   863,     0,
     284,     0,   363,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,   876,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   172,   172,   905,     0,     0,     0,
       0,   172,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     497,     0,     0,     0,     0,     0,   172,     0,     0,   172,
     172,     0,   172,     0,   172,   172,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   282,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   945,     0,     0,   172,     0,     0,
       0,   172,     0,     0,     0,   172,     0,   951,     0,     0,
       0,   367,     0,     0,     0,   367,     0,     0,     0,     0,
     367,   367,     0,     0,   367,     0,     0,     0,     0,     0,
       0,   970,     0,     0,   367,     0,     0,     0,     0,     0,
       0,   367,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   433,   433,   433,   433,   433,   433,
     433,   433,   433,   433,   433,   433,   433,   433,   433,   433,
     433,   433,   433,     0,     0,     0,   367,     0,     0,   172,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   367,
       0,     0,     0,   367,     0,     0,  1023,   367,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,   117,   374,     0,   433,
      46,     0,    47,     0,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
     117,     0,     0,     0,     0,   484,   374,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1103,     0,
    1104,     0,     0,     0,     0,   284,   831,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     556,     0,     0,     0,     0,     0,     0,   556,     0,   662,
       0,   172,     0,     0,  1148,     0,    75,     0,     0,     0,
       0,     0,     0,  1157,     0,     0,     0,  1159,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   367,     0,   497,
       0,   178,   181,     0,     0,     0,     0,     0,     0,     0,
       0,   660,     0,     0,     0,     0,   172,     0,   172,     0,
       0,   172,     0,     0,   172,     0,     0,     0,   172,     0,
       0,     0,   657,     0,     0,     0,     0,  1201,   229,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   556,
       0,     0,   112,     0,     0,     0,     0,     0,     0,   433,
       0,     0,     0,     0,     0,   433,     0,     0,     0,   367,
       0,     0,   367,   367,     0,   367,   433,   374,   672,     0,
       0,     0,   112,   660,     0,     0,     0,     0,     0,   322,
     367,     0,   323,     0,   367,     0,     0,   693,   367,     0,
       0,   363,     0,     0,     0,     0,     0,   348,     0,     0,
       0,     0,     0,   112,     0,     0,   433,     0,     0,     0,
       0,     0,     0,     0,  1332,     0,     0,   397,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   397,
       0,     0,   117,     0,     0,     0,   117,   117,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     117,     0,     0,     0,     0,     0,   172,     0,     0,     0,
       0,   528,     0,     0,     0,     0,     0,   556,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   556,   821,     0,   556,   824,     0,
       0,     0,     0,     0,     0,   497,     0,   374,     0,     0,
     367,   672,     0,   229,     0,     0,     0,     0,     0,     0,
       0,     0,   589,   590,   484,     0,     0,     0,     0,     0,
       0,     0,     0,   178,     0,   172,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   172,     0,   178,   172,
       0,   172,   172,     0,     0,   556,     0,     0,     0,   556,
     433,     0,     0,     0,   367,     0,     0,     0,     0,     0,
       0,     0,     0,   367,     0,     0,     0,   367,     0,     0,
       0,     0,     0,   639,     0,     0,     0,     0,     0,     0,
       0,   643,   645,     0,     0,     0,   652,     0,     0,   374,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     149,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   348,     0,     0,   348,     0,
       0,   397,     0,   433,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   556,     0,     0,   284,   786,
     172,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   960,   374,     0,     0,     0,
       0,   433,   433,   433,     0,     0,   672,     0,   433,   433,
     672,     0,     0,     0,     0,     0,     0,   978,     0,   374,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   433,  1538,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   229,  1563,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   854,   855,
       0,     0,     0,   433,   433,    14,    15,    16,    17,    18,
       0,     0,    20,   172,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -499,
    -499,   172,  -499,    46,     0,    47,     0,     0,  -499,     0,
       0,     0,     0,     0,     0,   374,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,   556,   556,   172,     0,  1961,     0,     0,   117,   172,
       0,     0,   556,  1097,     0,   556,  1100,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   960,
     374,    63,    64,     0,   672,     0,   672,   672,     0,     0,
       0,     0,     0,   672,     0,     0,     0,     0,     0,   374,
       0,   374,     0,     0,     0,   374,   374,   374,   374,    75,
       0,     0,     0,     0,     0,     0,     0,   949,     0,     0,
       0,     0,     0,     0,     0,   374,   348,   556,     0,     0,
     433,   556,    78,     0,   216,   172,     0,     0,   556,  1175,
       0,     0,   556,  1179,     0,     0,   556,  1183,     0,     0,
     277,     0,     0,  1186,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1751,   662,     0,     0,     0,     0,     0,     0,
     170,   397,   172,   172,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,   556,     0,     0,     0,     0,
       0,   216,     0,     0,     0,   338,     0,     0,   172,   172,
       0,     0,     0,     0,   117,     0,   389,   379,     0,     0,
       0,   172,   672,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1051,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   216,     0,   117,   662,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   302,   504,     0,     0,     0,
     510,     0,     0,   367,     0,     0,     0,     0,   308,     0,
     309,   484,   374,     0,     0,   117,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1751,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   381,     0,     0,     0,     0,     0,   433,     0,     0,
       0,     0,     0,     0,     0,   216,     0,     0,     0,   172,
       0,     0,     0,     0,  1131,     0,     0,     0,     0,     0,
     277,     0,     0,     0,     0,  1147,     0,   556,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,   374,     0,     0,   672,   672,     0,     0,
       0,     0,     0,   672,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   510,   558,   559,     0,     0,
     563,     0,     0,   566,   567,   216,   569,     0,   570,     0,
     172,     0,     0,     0,     0,  1893,  1894,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   665,   374,   682,
       0,     0,   556,  1426,  1215,   556,  1430,     0,     0,   556,
    1434,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -499,  -499,
       0,  -499,    46,     0,    47,     0,     0,  -499,     0,  1322,
     743,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,   655,     0,     0,
       0,     0,     0,     0,     0,     0,   172,     0,     0,     0,
       0,     0,   687,     0,   216,     0,     0,     0,     0,     0,
       0,     0,   433,     0,     0,     0,     0,     0,   150,  1976,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,   665,     0,     0,
       0,     0,     0,   849,     0,     0,    74,     0,    75,  1751,
       0,   374,     0,   433,     0,     0,     0,   672,  1546,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,    77,
       0,    78,   507,     0,     0,     0,   216,     0,     0,    80,
      81,   374,     0,     0,     0,     0,  2017,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   819,     0,     0,
       0,     0,     0,     0,  2047,     0,     0,     0,  2048,     0,
       0,     0,   216,   216,     0,     0,   556,  1600,     0,   504,
       0,     0,     0,     0,     0,     0,     0,   556,  1609,     0,
     672,     0,     0,   433,     0,   433,     0,     0,     0,     0,
       0,   374,     0,     0,   374,   374,     0,   374,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   297,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   902,     0,     0,   433,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1482,  1484,  1486,     0,     0,     0,   504,     0,   964,     0,
       0,     0,     0,     0,     0,   433,     0,     0,     0,     0,
       0,   370,     0,     0,     0,     0,     0,     0,     0,   665,
       0,     0,     0,     0,  1508,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   216,     0,     0,     0,  1215,     0,   433,     0,   216,
     370,  1527,   743,     0,   743,   216,     0,   216,     0,     0,
       0,     0,     0,     0,     0,     0,   743,     0,     0,   743,
     743,   743,     0,     0,     0,     0,     0,   374,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   976,   977,     0,
       0,     0,     0,     0,   672,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   986,     0,   988,
       0,     0,     0,     0,     0,   504,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   370,   216,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     504,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   504,
       0,   504,     0,     0,   556,   504,   504,   379,   504,     0,
       0,   370,     0,   370,   370,     0,     0,     0,     0,     0,
     556,     0,     0,     0,     0,   504,     0,   370,     0,     0,
       0,   370,     0,     0,     0,     0,     0,     0,  1090,  1091,
    1677,  1678,     0,     0,     0,  1095,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1118,     0,     0,  1121,  1122,     0,  1125,     0,  1127,  1128,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   504,     0,     0,     0,     0,     0,
       0,   216,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   849,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1173,     0,     0,     0,  1177,     0,     0,     0,  1181,
       0,     0,  1769,     0,     0,     0,     0,     0,     0,     0,
       0,   370,     0,     0,     0,     0,     0,   370,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   556,   556,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,     0,   556,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1313,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   370,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1839,     0,   504,   504,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   370,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   556,     0,
       0,     0,     0,     0,     0,     0,   556,     0,     0,     0,
       0,   370,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   504,   370,
     370,     0,   370,     0,     0,     0,     0,     0,     0,     0,
     370,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   369,   370,     0,     0,   370,     0,     0,     0,
       0,     0,     0,   370,     0,  1313,   370,     0,     0,     0,
       0,   556,  2069,     0,     0,   556,     0,     0,     0,   743,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   369,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1419,     0,  1421,     0,   743,  1424,     0,     0,  1428,   556,
       0,     0,  1432,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   277,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   370,
       0,     0,     0,     0,   216,     0,     0,     0,     0,     0,
       0,   665,     0,     0,     0,   370,     0,   556,   556,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1991,   369,
       0,   370,     0,     0,     0,   370,     0,     0,     0,     0,
       0,   379,     0,   370,   370,     0,   743,     0,   370,     0,
       0,     0,     0,     0,     0,   556,     0,     0,     0,     0,
       0,     0,     0,   370,     0,   370,     0,     0,     0,   370,
     370,   370,   370,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   369,     0,   369,   369,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
    1544,     0,   369,     0,     0,     0,   739,     0,     0,     0,
       0,   504,     0,     0,   504,   504,     0,   379,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,   370,   743,
     743,   743,     0,     0,   743,   743,     0,     0,     0,  1598,
       0,   510,     0,   370,     0,   370,   370,     0,     0,     0,
    1607,     0,     0,  1611,     0,  1614,  1615,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   216,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   369,     0,     0,     0,     0,     0,   369,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     277,     0,     0,     0,     0,     0,   370,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   379,     0,     0,
       0,   371,     0,     0,     0,     0,     0,     0,   913,   915,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,     0,     0,     0,
     371,     0,     0,     0,  1732,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   369,     0,     0,   370,     0,     0,
       0,   370,     0,     0,     0,     0,   370,   370,     0,     0,
     370,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     370,     0,     0,     0,     0,     0,     0,   370,     0,   369,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   369,     0,     0,     0,     0,     0,     0,     0,
     216,     0,   370,     0,     0,     0,     0,     0,     0,     0,
     369,   369,     0,   369,     0,   370,     0,     0,   371,   370,
       0,   369,     0,   370,     0,   739,     0,  1611,   739,   277,
       0,     0,     0,   739,   369,     0,     0,   369,     0,     0,
       0,     0,   739,     0,   369,     0,     0,   369,     0,     0,
       0,     0,     0,     0,     0,  1801,     0,     0,     0,     0,
       0,   739,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   371,     0,   371,   371,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   371,     0,     0,
       0,   371,     0,     0,     0,     0,     0,  1076,     0,     0,
     457,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   489,     0,     0,     0,   743,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   519,     0,
     519,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     369,     0,     0,     0,     0,   370,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,   369,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1890,
       0,     0,   369,   370,     0,   370,   369,     0,     0,     0,
       0,     0,   277,     0,   369,   369,     0,     0,     0,   369,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   371,     0,     0,   369,     0,   369,   371,     0,     0,
     369,   369,   369,   369,     0,     0,  1918,  1919,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     369,     0,   634,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1933,  1934,     0,   370,     0,     0,   370,   370,
       0,   370,     0,     0,     0,  1938,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,     0,
     370,     0,     0,     0,   370,   371,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     369,     0,     0,   371,     0,     0,     0,     0,     0,   369,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   369,   743,   369,   369,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   371,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   371,     0,  2006,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   371,
     371,     0,   371,     0,     0,     0,     0,   369,     0,     0,
     371,   370,     0,     0,     0,     0,   370,   743,     0,     0,
     510,     0,     0,   371,     0,     0,   371,     0,     0,     0,
       0,     0,     0,   371,     0,     0,   371,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   519,  2067,     0,     0,     0,     0,   519,
       0,     0,     0,     0,   457,     0,     0,     0,     0,     0,
     370,     0,     0,     0,     0,     0,     0,     0,   369,   370,
       0,     0,   369,   370,     0,     0,     0,   369,   369,     0,
       0,   369,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   369,     0,     0,     0,     0,     0,     0,   369,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   371,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   369,     0,   371,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,     0,     0,     0,
     369,   371,     0,     0,   369,   371,   944,     0,     0,     0,
       0,     0,     0,   371,   371,     0,     0,     0,   371,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     739,     0,     0,   371,   489,   371,     0,     0,     0,   371,
     371,   371,   371,     0,     0,     0,     0,   972,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   371,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1005,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1016,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   371,
       0,  1037,  1039,     0,     0,  1041,   369,  1043,   371,     0,
     369,     0,     0,  1005,     0,  1053,  1005,     0,     0,     0,
       0,     0,     0,   371,     0,   371,   371,     0,     0,     0,
       0,     0,     0,     0,   369,     0,   369,     0,     0,     0,
       0,     0,     0,  1081,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1083,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1092,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   489,     0,     0,   371,     0,  1081,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,     0,     0,   369,
     369,     0,   369,     0,     0,     0,     0,     0,     0,     0,
    1151,     0,     0,   519,     0,     0,     0,   369,     0,     0,
       0,   369,     0,     0,  1162,   369,     0,     0,     0,     0,
    1684,  1692,     0,     0,  1684,  1703,     0,     0,     0,     0,
    1710,     0,     0,     0,  1714,     0,  1716,   371,  1703,  2009,
       0,   371,  1187,     0,     0,     0,   371,   371,     0,   370,
     371,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     371,     0,     0,     0,     0,     0,     0,   371,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   404,     0,   457,
     405,     0,   406,   407,     0,   408,     0,     0,     0,  1303,
    1305,     0,   371,     0,     0,     0,     0,   489,     0,     0,
       0,     0,   409,     0,     0,   371,     0,     0,     0,   371,
       0,   370,   369,   371,   370,     0,     0,   369,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   370,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
    1081,   416,   417,   418,     0,   419,   420,     0,  1346,     0,
       0,     0,     0,    74,     0,     0,     0,  1005,     0,     0,
       0,   369,     0,     0,     0,     0,  1805,     0,     0,     0,
     369,     0,     0,     0,   369,   421,     0,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,   519,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1842,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   371,     0,     0,     0,   371,
       0,  1862,  1864,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   371,     0,   371,     0,     0,     0,     0,
       0,     0,  1884,   186,     6,     7,     8,     9,    10,    11,
      12,    13,   519,     0,  1418,     0,     0,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,   259,   260,     0,   261,
      46,     0,    47,     0,     0,   262,     0,     0,    49,    50,
      51,    52,    53,    54,    55,   371,     0,     0,   371,   371,
       0,   371,     0,     0,   404,     0,     0,   405,     0,   406,
     407,     0,   408,     0,     0,     0,   371,     0,     0,     0,
     371,     0,     0,     0,   371,     0,  1495,  1495,     0,   409,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1948,     0,     0,     0,     0,     0,     0,     0,  1951,     0,
    1953,     0,     0,  1957,  1963,     0,  1703,     0,     0,   410,
     411,  1969,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,     0,   419,   420,     0,     0,     0,     0,  -474,     0,
      74,     0,     0,  1542,     0,     0,     0,     0,     0,  1551,
       0,     0,     0,     0,     0,  1696,  1697,  1698,  1699,     0,
       0,  -474,   421,  1863,  1005,    78,   422,     0,     0,   489,
       0,     0,   423,    80,    81,   424,   425,   426,   427,     0,
       0,   371,     0,     0,     0,     0,   371,   519,     0,     0,
    1579,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2034,  1037,     0,     0,     0,     0,
       0,  2041,  2043,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2062,     0,     0,     0,     0,     0,     0,     0,     0,
     371,     0,     0,     0,     0,     0,     0,     0,     0,   371,
       0,     0,     0,   371,     0,     0,     0,     0,     0,     0,
     369,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2084,     0,  2087,     0,     0,  2089,  2091,     0,     0,  1642,
    1643,     0,  2096,  2098,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1005,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   519,     0,     0,   457,     0,     0,
       0,     0,   369,     0,     0,   369,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2177,  2137,  2139,  2141,     0,
     369,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1477,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1039,     0,  2162,  2164,  2166,
       0,     0,     0,     0,  1759,  1760,     0,     0,     0,     0,
       0,     0,     0,   404,     0,     0,   405,     0,   406,   407,
       0,   408,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   519,     0,     0,  1218,  1037,   409,  1220,
       0,  1221,  -252,  -252,  1222,  1223,  1224,  1225,  1226,  1227,
    1228,  1229,  1230,  1231,  1232,  1233,  -351,  -351,  1234,  1235,
    1236,  1237,  1238,  1239,  1240,     0,  1241,     0,   410,   411,
       0,   513,   413,  1242,  1243,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,  1244,   416,   417,   418,
       0,   419,   420,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   457,     0,
    -252,  1245,     0,  1830,    78,   422,     0,     0,     0,   306,
       0,   423,    80,    81,   424,   425,   426,   427,  1453,     0,
    1454,     0,     0,     0,     0,  1455,  -192,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,  1877,     0,    46,     0,    47,     0,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1456,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   519,     0,     0,
       0,     0,     0,     0,  1908,     0,     0,  1910,     0,   371,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,  2177,     0,     0,     0,     0,     0,
       0,     0,     0,  1924,     0,     0,     0,     0,     0,     0,
      74,  1477,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1457,     0,     0,     0,    78,  1011,     0,     0,     0,
       0,     0,   404,    80,    81,   405,     0,   406,   407,     0,
     408,   371,     0,     0,   371,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1218,     0,   409,  1220,   371,
    1221,  -253,  -253,  1222,  1223,  1224,  1225,  1226,  1227,  1228,
    1229,  1230,  1231,  1232,  1233,  -351,  -351,  1234,  1235,  1236,
    1237,  1238,  1239,  1240,     0,  1241,     0,   410,   411,     0,
     513,   413,  1242,  1243,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,  1244,   416,   417,   418,     0,
     419,   420,     0,  1880,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1477,     0,     0,     0,     0,     0,     0,     0,     0,  -253,
    1245,     0,     0,    78,   422,     0,     0,     0,   306,     0,
     423,    80,    81,   424,   425,   426,   427,     0,     0,     0,
       0,   404,     0,     0,   405,  -192,   406,   407,     0,   408,
    1005,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1218,     0,   409,  1220,     0,  1221,
       0,     0,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,
    1230,  1231,  1232,  1233,  -351,  -351,  1234,  1235,  1236,  1237,
    1238,  1239,  1240,     0,  1241,     0,   410,   411,     0,   513,
     413,  1242,  1243,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,  1244,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1245,
       0,     0,    78,   422,     0,     0,     0,   306,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,     0,  -192,     4,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    1217,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   404,     0,    46,   405,    47,   406,   407,     0,   408,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,  1218,    58,  1219,  1220,     0,  1221,
       0,     0,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,
    1230,  1231,  1232,  1233,  -351,  -351,  1234,  1235,  1236,  1237,
    1238,  1239,  1240,     0,  1241,     0,   410,   411,    61,   513,
     413,  1242,  1243,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,  1244,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -3,  1245,
       0,     0,    78,  1246,     0,     0,     0,   306,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,     0,  -192,     4,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    1217,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   404,     0,    46,   405,    47,   406,   407,     0,   408,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,  1218,    58,  1219,  1220,     0,  1221,
       0,     0,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,
    1230,  1231,  1232,  1233,  -351,  -351,  1234,  1235,  1236,  1237,
    1238,  1239,  1240,     0,  1241,     0,   410,   411,    61,   513,
     413,  1242,  1243,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,  1244,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1245,
       0,     0,    78,  1246,     0,     0,     0,   306,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,     0,  -192,     4,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   404,     0,    46,   405,    47,   406,   407,     0,   408,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,    61,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1696,  1697,  1698,  1699,     0,     0,     0,   421,
    1700,  1701,    78,  1246,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,     0,  1702,     4,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   404,     0,    46,   405,    47,   406,   407,     0,   408,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,    61,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1696,  1697,  1698,  1699,     0,     0,     0,   421,
    1700,     0,    78,  1246,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,     0,  1702,     4,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   404,     0,    46,   405,    47,   406,   407,     0,   408,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,    61,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   421,
       0,  1694,    78,  1246,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     4,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   404,     0,    46,   405,    47,   406,   407,     0,
     408,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,    61,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,     0,
     419,   420,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     421,     0,     0,    78,  1246,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   404,     0,    46,   405,    47,   406,   407,     0,
     408,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,     0,
     419,   420,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     421,     0,     0,    78,   486,     0,     0,     0,     0,     0,
     423,   487,    81,   424,   425,   426,   427,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   404,     0,    46,   405,    47,   406,   407,     0,
     408,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,     0,
     419,   420,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     421,     0,     0,    78,  1300,     0,     0,     0,     0,     0,
     423,  1301,    81,   424,   425,   426,   427,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   404,     0,    46,   405,    47,   406,   407,     0,
     408,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,     0,
     419,   420,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     421,     0,     0,    78,   833,     0,     0,     0,     0,     0,
     423,   487,    81,   424,   425,   426,   427,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   404,     0,    46,   405,    47,   406,   407,     0,
     408,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,     0,
     419,   420,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     421,     0,     0,    78,   422,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   404,     0,    46,   405,    47,   406,   407,     0,
     408,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,     0,
     419,   420,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     421,     0,     0,    78,   833,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,  2016,     0,    -2,
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
       0,     0,     0,     0,    -2,    -2,  2046,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,     0,     0,    -2,     0,  1311,    -2,     0,
       0,     0,     0,    -2,    -2,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,   404,     0,     0,   405,     0,   406,   407,     0,   408,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,    -2,     0,     0,    58,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,    -2,    -2,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,  1548,     0,     0,     0,     0,    74,     0,    75,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   421,
       0,     0,    78,   422,     0,     0,     0,     0,     0,   423,
     487,    81,   424,   425,   426,   427,   404,     0,     0,   405,
       0,   406,   407,     0,   408,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,   409,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,     0,   419,   420,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   421,     0,     0,    78,   422,     0,
       0,     0,     0,     0,   423,  1549,    81,   424,   425,   426,
     427,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,    59,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      60,     0,     0,     0,    61,    62,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,    77,     0,    78,    79,
       0,     0,     0,     0,     0,     0,    80,    81,   264,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -499,  -499,     0,  -499,    46,     0,    47,     0,
       0,  -499,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   150,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,   265,     0,     0,     0,
    -826,     0,     0,    80,    81,   264,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -499,
    -499,     0,  -499,    46,     0,    47,     0,     0,  -499,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   150,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
      77,     0,    78,   265,     0,     0,     0,     0,     0,     0,
      80,    81,     4,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,     0,     0,     0,     0,  -418,  -418,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -418,     0,     0,     0,    78,
      79,     0,     0,     0,     0,     0,     0,    80,    81,     4,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
       0,     0,     0,     0,  -419,  -419,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -419,     0,     0,     0,    78,    79,     0,  1453,
       0,  1454,     0,     0,    80,    81,  1455,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
    1456,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1651,     0,     0,     0,    78,  1011,     0,  1453,
       0,  1454,     0,     0,    80,    81,  1455,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
    1456,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1652,     0,     0,     0,    78,  1011,     0,  1453,
       0,  1454,     0,     0,    80,    81,  1455,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
    1456,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1653,     0,     0,     0,    78,  1011,     0,     0,
       0,     0,     0,     0,    80,    81,   264,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -499,  -499,     0,  -499,    46,     0,    47,     0,     0,  -499,
       0,   264,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    58,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -499,  -499,     0,  -499,    46,
       0,    47,    63,    64,  -499,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   265,     0,     0,    63,    64,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   507,
       0,     0,     0,     0,     0,     0,    80,    81,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,   613,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1107,    77,  -692,    78,   668,     0,     0,     0,     0,
       0,     0,    80,    81,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -499,  -499,     0,
    -499,    46,     0,    47,     0,     0,  -499,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   150,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
      78,   265,     0,     0,     0,  -830,     0,     0,    80,    81,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -499,  -499,     0,  -499,    46,     0,    47,
       0,     0,  -499,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,    77,     0,    78,   265,     0,     0,
       0,     0,     0,     0,    80,    81,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
     613,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   667,
       0,  -692,    78,   668,     0,     0,     0,     0,     0,     0,
      80,    81,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,   613,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   802,     0,  -692,    78,   553,
       0,     0,     0,     0,     0,     0,    80,    81,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,  1140,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -700,    78,   919,     0,     0,     0,     0,
       0,     0,    80,    81,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,   355,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   356,
      78,   357,     0,     0,     0,     0,     0,     0,    80,    81,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,   355,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,  1621,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   919,     0,     0,
       0,     0,     0,     0,    80,    81,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
    1623,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   919,     0,     0,     0,     0,     0,     0,
      80,    81,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   507,
       0,     0,     0,     0,     0,     0,    80,    81,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   919,     0,     0,     0,     0,
       0,     0,    80,    81,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,   355,    49,
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
      78,   357,     0,     0,     0,     0,     0,     0,    80,    81,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -499,  -499,     0,  -499,    46,     0,    47,
       0,     0,  -499,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,  1477,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   404,     0,     0,   405,     0,   406,   407,
       0,   408,     0,     0,     0,     0,    78,   265,     0,     0,
       0,     0,     0,     0,    80,    81,  1218,     0,   409,  1220,
       0,  1221,  1941,  1942,  1222,  1223,  1224,  1225,  1226,  1227,
    1228,  1229,  1230,  1231,  1232,  1233,     0,     0,  1234,  1235,
    1236,  1237,  1238,  1239,  1240,     0,  1241,     0,   410,   411,
       0,   513,   413,  1242,  1243,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,  1244,   416,   417,   418,
       0,   419,   420,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,  1477,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1245,     0,     0,    78,   422,     0,     0,     0,   306,
       0,   423,    80,    81,   424,   425,   426,   427,     0,   404,
       0,     0,   405,     0,   406,   407,  -192,   408,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1218,     0,   409,  1220,     0,  1221,     0,     0,
    1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,  1231,
    1232,  1233,     0,     0,  1234,  1235,  1236,  1237,  1238,  1239,
    1240,     0,  1241,     0,   410,   411,     0,   513,   413,  1242,
    1243,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,  1244,   416,   417,   418,     0,   419,   420,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1245,     0,     0,
      78,   422,     0,     0,     0,   306,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,     0,  -192,   310,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -422,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,     0,     0,     0,     0,  -422,   310,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -423,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,     0,     0,     0,     0,  -423,   310,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,    14,    15,    16,    17,    18,    19,   730,    20,   731,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    63,    64,   404,     0,    46,
     405,    47,   406,   407,     0,   408,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   409,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   732,     0,     0,     0,     0,  1233,
       0,  -351,     0,     0,     0,     0,    78,     0,     0,     0,
       0,  -422,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1245,     0,     0,    78,   733,
       0,     0,     0,   306,     0,   423,    80,    81,   734,   735,
     426,   427,    14,    15,    16,    17,    18,    19,   730,    20,
     731,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   404,     0,
      46,   405,    47,   406,   407,     0,   408,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   409,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   732,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,     0,   419,   420,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   421,     0,     0,    78,
     733,     0,     0,     0,   306,     0,   423,    80,    81,   734,
     735,   426,   427,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   404,
       0,    46,   405,    47,   406,   407,     0,   408,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   409,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,     0,   419,   420,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   421,     0,   452,
      78,   453,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     404,     0,    46,   405,    47,   406,   407,     0,   408,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   421,     0,
       0,    78,   453,     0,     0,     0,   306,     0,   423,    80,
      81,   424,   425,   426,   427,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   404,     0,    46,   405,    47,   406,   407,     0,   408,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   421,
       0,     0,    78,   733,     0,     0,     0,   306,     0,   423,
      80,    81,   424,   425,   426,   427,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   404,     0,    46,   405,    47,   406,   407,     0,
     408,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,     0,
     419,   420,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     421,     0,     0,    78,   453,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   404,     0,    46,   405,    47,   406,   407,
       0,   408,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   409,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
       0,   419,   420,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   421,     0,     0,    78,   833,     0,     0,     0,     0,
       0,   423,    80,    81,   424,   425,   426,   427,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   404,     0,    46,   405,    47,   406,
     407,     0,   408,   355,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   409,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   410,
     411,     0,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,   404,   419,   420,   405,     0,   406,   407,     0,   408,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,   409,     0,     0,     0,
       0,     0,   421,     0,     0,    78,   422,     0,     0,     0,
       0,     0,   423,    80,    81,   424,   425,   426,   427,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1696,  1697,  1698,  1699,     0,     0,     0,   421,
    1956,     0,    78,   422,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,   264,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,    58,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -499,  -499,     0,  -499,    46,     0,    47,     0,   721,
    -499,   722,   723,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,   -17,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,     0,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,    78,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     150,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   613,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -692,    78,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   150,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,    77,
       0,    78,    79,     0,     0,     0,  -828,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   150,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,    77,     0,    78,   208,
       0,     0,     0,     0,     0,     0,    80,    81,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   150,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,    79,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   150,
       0,   480,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   856,
       0,     0,    78,   481,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   150,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
      79,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,   480,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   481,     0,     0,
       0,     0,     0,     0,    80,    81,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     613,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -692,    78,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   355,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,  1211,     0,     0,
       0,     0,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,    78,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,   355,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,   856,
       0,     0,    78,   481,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   355,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   856,     0,     0,    78,
     481,     0,    63,    64,     0,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1018,    78,  1011,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,  1567,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,  1011,     0,     0,     0,     0,     0,     0,    80,    81,
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
       0,     0,     0,     0,     0,     0,     0,    78,   318,     0,
      63,    64,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   208,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,   355,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   357,
       0,    63,    64,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   318,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   355,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     481,     0,     0,     0,     0,     0,     0,    80,    81,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -499,  -499,     0,  -499,    46,     0,    47,     0,
       0,  -499,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,     0,   355,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     507,     0,     0,     0,     0,     0,     0,    80,    81,    14,
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
       0,     0,     0,     0,     0,     0,    78,  1011,     0,    63,
      64,     0,     0,     0,    80,    81,     0,     0,     0,     0,
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
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,     0,   355,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   481,     0,
      63,    64,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,  1011,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,    14,    15,    16,    17,
      18,    58,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -499,  -499,     0,  -499,    46,     0,    47,    63,    64,  -499,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
       0,     0,    63,    64,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   335,     0,    14,    15,    16,    17,
      18,    80,    81,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -499,  -499,     0,  -499,    46,     0,    47,     0,     0,  -499,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    58,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -499,  -499,     0,  -499,    46,
       0,    47,    63,    64,  -499,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   507,     0,     0,    63,    64,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
       0,     0,     0,     0,     0,     0,    80,    81,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   404,     0,    46,
     405,    47,   406,   407,     0,   408,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,   422,
       0,     0,     0,     0,     0,   423,   487,    81,   424,   425,
     426,   427,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   404,     0,    46,   405,    47,   406,   407,     0,   408,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   421,
       0,     0,    78,   422,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     150,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,    78,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,     0,   355,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,    78,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,   187,   404,   188,   189,   405,     0,   406,   407,
       0,   408,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   721,     0,   722,   723,   410,   411,
       0,   513,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
     404,   419,   420,   405,    75,   406,   407,     0,   408,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   409,     0,     0,     0,     0,
       0,   421,    77,     0,   514,   515,     0,     0,     0,   516,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,   404,   419,   420,
     405,     0,   406,   407,     0,   408,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,     0,     0,     0,     0,     0,   421,  1349,
       0,    78,   422,     0,     0,     0,  1350,     0,   423,    80,
      81,   424,   425,   426,   427,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,   404,   419,   420,   405,     0,   406,
     407,     0,   408,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   409,
       0,     0,     0,     0,     0,   421,     0,     0,    78,   422,
       0,     0,     0,   516,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,   410,
     411,     0,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,   404,   419,   420,   405,     0,   406,   407,     0,   408,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,     0,     0,     0,
       0,     0,   421,  1001,     0,    78,   422,     0,     0,     0,
       0,     0,   423,    80,    81,   424,   425,   426,   427,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,   404,   419,
     420,   405,     0,   406,   407,     0,   408,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,     0,   421,
    1036,     0,    78,   422,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,   404,   419,   420,   405,     0,
     406,   407,     0,   408,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,     0,   421,     0,     0,    78,
     422,     0,     0,     0,   306,     0,   423,    80,    81,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,   404,   419,   420,   405,     0,   406,   407,     0,
     408,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,     0,     0,
       0,     0,     0,   421,     0,     0,    78,   422,     0,     0,
    1075,     0,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,   404,
     419,   420,   405,     0,   406,   407,     0,   408,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,     0,     0,     0,     0,     0,
     421,     0,     0,    78,   422,     0,     0,     0,  1487,     0,
     423,    80,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,   404,   419,   420,   405,
       0,   406,   407,     0,   408,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,     0,     0,   421,  1578,     0,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,   404,   419,   420,   405,     0,   406,   407,
       0,   408,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,     0,   421,     0,     0,    78,   422,     0,
       0,     0,  1770,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
     404,   419,   420,   405,     0,   406,   407,     0,   408,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   409,     0,     0,     0,     0,
       0,   421,     0,  1947,    78,   422,     0,     0,     0,     0,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,   404,   419,   420,
     405,     0,   406,   407,     0,   408,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,     0,     0,     0,     0,     0,   421,  1952,
       0,    78,   422,     0,     0,     0,     0,     0,   423,    80,
      81,   424,   425,   426,   427,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,   404,
       0,     0,   405,    74,   406,   407,     0,   408,  2033,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,   421,  1962,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,   404,   419,   420,   405,
       0,   406,   407,     0,   408,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,     0,     0,   421,     0,     0,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,   404,   419,   420,   405,     0,   406,   407,
       0,   408,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,     0,   421,  2040,     0,    78,   422,     0,
       0,     0,     0,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
     404,   419,   420,   405,     0,   406,   407,     0,   408,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   409,     0,     0,     0,     0,
       0,   421,  2042,     0,    78,   422,     0,     0,     0,     0,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,   404,   419,   420,
     405,     0,   406,   407,     0,   408,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,     0,     0,     0,     0,     0,   421,  2086,
       0,    78,   422,     0,     0,     0,     0,     0,   423,    80,
      81,   424,   425,   426,   427,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,   404,   419,   420,   405,     0,   406,
     407,     0,   408,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   409,
       0,     0,     0,     0,     0,   421,  2088,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,   410,
     411,     0,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,   404,   419,   420,   405,     0,   406,   407,     0,   408,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,     0,     0,     0,
       0,     0,   421,  2090,     0,    78,   422,     0,     0,     0,
       0,     0,   423,    80,    81,   424,   425,   426,   427,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,   404,   419,
     420,   405,     0,   406,   407,     0,   408,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,     0,   421,
    2095,     0,    78,   422,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,   404,   419,   420,   405,     0,
     406,   407,     0,   408,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,     0,   421,  2097,     0,    78,
     422,     0,     0,     0,     0,     0,   423,    80,    81,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,   404,   419,   420,   405,     0,   406,   407,     0,
     408,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,     0,     0,
       0,     0,     0,   421,  2136,     0,    78,   422,     0,     0,
       0,     0,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,   404,
     419,   420,   405,     0,   406,   407,     0,   408,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,     0,     0,     0,     0,     0,
     421,  2138,     0,    78,   422,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,   404,   419,   420,   405,
       0,   406,   407,     0,   408,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,     0,     0,   421,  2140,     0,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,   404,   419,   420,   405,     0,   406,   407,
       0,   408,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,     0,   421,  2161,     0,    78,   422,     0,
       0,     0,     0,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
     404,   419,   420,   405,     0,   406,   407,     0,   408,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   409,     0,     0,     0,     0,
       0,   421,  2163,     0,    78,   422,     0,     0,     0,     0,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,   404,   419,   420,
     405,     0,   406,   407,     0,   408,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,     0,     0,     0,     0,     0,   421,  2165,
       0,    78,   422,     0,     0,     0,     0,     0,   423,    80,
      81,   424,   425,   426,   427,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -498,  -498,     0,  -498,
      46,   404,    47,     0,   405,  -498,   406,   407,     0,   408,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,   404,    75,    74,   405,     0,
     406,   407,     0,   408,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   710,
     409,     0,    78,   422,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,   404,   419,   420,   405,     0,   406,   407,     0,
     408,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,     0,     0,
       0,     0,     0,   713,     0,     0,    78,   422,     0,     0,
       0,     0,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,   404,
     419,   420,   405,     0,   406,   407,     0,   408,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,     0,     0,     0,     0,     0,
     718,     0,     0,    78,   422,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,     0,   419,   420,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   727,     0,     0,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -499,  -499,
       0,  -499,    46,   404,    47,     0,   405,  -499,   406,   407,
       0,   408,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,   409,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
       0,   419,   420,     0,     0,     0,     0,   404,    75,    74,
     405,     0,   406,   407,     0,   408,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   421,   409,     0,    78,   422,     0,     0,     0,     0,
       0,   423,   943,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,   422,
       0,     0,     0,     0,     0,   423,   487,    81,   424,   425,
     426,   427
};

static const yytype_int16 yycheck[] =
{
       1,   168,   263,     4,    76,   237,   955,   680,   360,   527,
       4,    85,   237,    76,   143,   756,   180,   421,   759,   237,
     516,  1245,    98,   237,   237,   237,  1264,    59,   279,    93,
      76,     1,   170,   223,   671,     1,   667,    76,     1,   987,
     667,   237,   837,   170,   237,  1228,  1813,   995,   803,     1,
     121,   168,  1813,    76,   194,    56,    57,  1201,    59,   232,
     168,   940,  1050,  1813,   940,   185,  1296,  1297,     1,   237,
      85,  1368,   850,   207,   852,    76,   737,   206,   152,   423,
    1941,     1,   838,    98,    85,   258,   101,    73,   844,    59,
     105,  1079,    93,    59,  1945,   327,    59,    98,   139,    76,
     101,   247,   327,    76,   105,   837,    78,    59,   101,   327,
      85,   105,   835,   327,   327,   327,   667,   149,    76,    85,
    1245,     0,   237,     1,   835,   928,    59,  1707,    76,   586,
     276,   327,    98,   196,   327,   101,    10,   835,   421,   105,
     597,   287,   494,   144,   136,   948,   147,   163,   149,     1,
     196,   137,     0,   105,   155,   138,    98,   196,   163,   327,
    1148,   162,   238,    73,   835,   237,   182,    90,   173,   170,
     184,   157,     0,   196,   237,   161,   240,   151,   242,   149,
     195,   182,   157,   149,   641,   249,   149,   139,   180,   182,
     173,   237,   266,   194,   195,   196,   211,   149,   237,   173,
     837,   573,   574,    20,   835,   206,   247,    59,   835,   182,
     211,     1,   327,  2074,   237,    73,   149,    90,   207,   196,
     221,    10,   374,   238,   182,   226,   378,   137,   864,   195,
     231,   232,   233,  1198,   185,   276,   237,   238,   314,   102,
    1205,   358,   165,   381,   164,   211,   287,   157,   196,   101,
      91,   266,   328,  1210,   381,   327,   396,   258,   322,   323,
     237,   388,   336,   615,   327,   266,   281,   268,   161,    59,
     159,   312,   238,   166,   163,  2126,   277,   278,  1858,   137,
     281,   327,   156,  1580,   835,   163,   164,   288,   327,   237,
     163,    98,   165,   167,   157,   247,   238,   149,   172,  1040,
     266,   302,   303,   535,   305,   113,   557,   659,  2159,   310,
     535,   109,   110,   314,   565,   281,   157,   535,  1359,  1114,
    1285,   535,   535,   535,   276,   121,   327,   328,   136,   301,
     702,   683,   522,  1088,   138,   287,   973,   338,   690,   535,
    1219,  2108,   535,  1219,   716,   346,   347,  2108,   314,   139,
     351,   159,  1108,   757,   492,   484,   162,   993,  2108,   149,
     312,    62,   328,   673,   316,   161,  1027,   535,   712,   173,
    1318,   488,   314,   352,  1253,   163,   505,  1253,   167,  1703,
     381,   455,  1114,   172,  1107,   173,   328,   163,   165,   390,
    1515,   126,   393,  1518,  1519,   396,  1107,   173,   571,  1202,
     101,    78,    79,   635,   577,   182,    62,    63,   358,  1107,
     635,    73,   113,   392,   115,   157,   117,   635,    76,  1563,
     535,   635,   635,   635,   374,   160,     1,   710,   378,   281,
     713,   238,   715,    91,   952,   718,  1107,    59,   810,   635,
      62,    63,   635,    65,   727,   246,   580,   730,   731,   732,
     596,   466,   253,     1,   157,   156,     4,   247,   159,   160,
     113,   161,   168,   315,   112,   466,   166,   635,   925,   175,
     176,   547,   535,  1814,   275,   137,  1107,  1114,   181,   857,
    1107,   859,  1447,   136,    59,   286,   276,   135,   165,   535,
     491,   492,   157,  1450,  1451,  1452,   535,   287,   876,   161,
     466,   165,   503,   504,   656,  1210,   157,   314,   360,   165,
     211,    59,   535,   514,    76,   516,   530,  1741,   182,  1702,
     635,   328,   312,   657,  1707,   589,   182,   157,    76,   839,
     157,    93,   547,   843,   535,   157,   165,    85,   488,  1863,
    1864,   693,   514,   172,   854,   855,   547,  1588,  1589,  1590,
      98,  2077,  1308,   101,   163,   596,  1107,   105,   535,    78,
      79,   637,  1210,   680,   139,     1,   157,   945,   159,   157,
     571,   698,   635,   182,   149,   639,   577,   165,   579,  2105,
     281,   547,   283,   284,   131,   132,  1082,   535,  1929,   635,
     163,   580,   109,   110,   163,  1583,   635,   288,   163,  1475,
     165,   149,  2128,  1479,  1480,   547,  1554,   155,   640,   182,
     962,   165,   303,   182,   315,   159,  1741,  1493,   172,   320,
     164,    76,   170,    59,  1402,   326,   163,  1405,  1406,  1302,
     177,   178,  1956,  1957,   635,  1391,   637,    92,   154,   640,
     182,   642,   494,   160,   596,   182,   165,   195,   196,   165,
     651,   138,   786,   156,   655,   156,   111,   163,   206,   360,
     163,  1891,   163,   211,   365,   181,   367,   173,   657,   157,
     640,   637,   247,   221,   640,  1858,   866,   640,   830,   182,
    1316,   168,   169,   231,   232,   233,   687,  1323,   640,   237,
     238,   858,   163,   157,    73,   637,   163,   831,   163,   837,
     164,   276,   173,   139,   163,   706,   656,   640,   157,  2050,
     258,   412,   287,   149,   182,   182,   850,   182,   266,   150,
     151,   152,   153,   182,   858,  1103,  1104,  1468,   848,   902,
     680,     1,   163,   281,     4,   390,   163,   312,   393,  1375,
     547,   858,   173,   693,   852,  1450,  1451,  1452,   749,  1987,
     751,   182,   753,   163,   302,    73,   757,   305,   137,   760,
     561,   163,   310,   615,   163,   466,   314,   163,  2109,   163,
     784,    62,  1955,   163,    73,   159,   628,   161,   157,   327,
     328,  1159,   161,   182,   785,  1968,   182,   588,   640,    59,
     163,     3,   182,   494,   595,   496,   497,   786,   599,  1787,
    1749,  1789,  1450,  1451,  1452,  2146,   596,    73,   960,   182,
      73,   247,   513,   785,   163,   106,  1764,   159,    70,   137,
     111,  1131,   164,   114,   173,   116,     3,   159,   163,   830,
     637,   683,   164,   381,   835,   105,   837,   158,   137,   157,
     276,  1245,   831,   161,   165,    73,   547,    73,   849,  2032,
     640,   287,   159,   156,  1350,   160,  1023,   157,   157,   860,
     163,   988,   161,   170,   171,   866,   181,   657,   869,   165,
     571,   137,   165,   159,   137,   576,   312,   578,   164,   149,
     830,   764,   765,   766,   124,   159,   126,   127,   128,  1023,
     164,   157,   157,  1022,   157,   161,  1147,   848,   161,   179,
     601,   902,   603,   604,   109,   110,  1023,   157,  1654,   137,
      73,   137,   863,  1659,   615,   165,    73,   157,   466,  1555,
     160,   161,   172,  1295,   157,   165,   166,   628,  1565,   157,
     157,   157,   159,   161,   161,   161,   637,   938,   939,   940,
     121,  1577,    47,    48,   492,    50,    13,    14,    15,    16,
      17,    56,   160,   161,  1306,   159,   164,   951,   659,   163,
     661,   662,  1245,   159,    73,  1155,   514,   157,   516,   159,
     940,   161,  1490,   159,   137,  1113,  1114,   940,    73,   270,
     137,    79,   683,   684,   159,  1861,   159,   535,   940,   690,
     163,  1487,   159,   994,   157,   159,   163,   159,   161,   547,
     157,   157,   162,   159,   161,   161,    73,   940,   159,   107,
     960,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   596,   163,   571,   159,   160,   317,    73,   137,   577,
     157,   579,   159,   157,   161,    13,    14,    15,    16,    17,
      59,   831,   137,    62,    63,  1046,    65,     3,   157,  1050,
    1311,  1455,   161,   157,  1337,   170,   171,    13,    14,    15,
      16,    17,   157,   159,    22,   640,   161,   163,   135,   157,
     137,   362,   157,   364,   157,   366,  1712,   163,  1079,   157,
    1576,  1082,    73,   161,   182,   157,    62,   635,   940,   637,
     157,   137,   640,   102,   161,    73,    13,    14,    15,    16,
      17,   168,   169,   162,   159,   159,  1107,   655,   163,   163,
     962,   157,  1113,  1114,   159,   161,  1289,    73,   163,   159,
    1184,   412,   157,   163,  1075,   101,   927,   159,   129,   130,
    1766,   163,  1296,  1297,   165,    73,  1210,   113,   114,   687,
     157,  2017,    90,  1661,   161,  1781,   137,  1148,   133,   134,
     940,    13,    14,    15,    16,    17,    73,   135,   165,   137,
     596,   182,     3,  1330,   159,  1332,   157,    73,   163,   156,
     161,  2047,    13,    14,    15,    16,    17,   165,   157,   157,
     156,   137,   161,   161,   157,  1302,   172,  1438,  1315,   159,
     168,   169,   109,   110,   162,   163,   157,   162,  1332,   137,
     161,   749,  2078,   751,   640,   753,  1157,  1201,   165,   159,
     159,    73,   760,   163,   163,  1332,  1651,  1652,  1653,   157,
     137,  1222,   513,   161,  1225,  1226,  1227,   150,   151,   152,
     153,   137,    73,  1234,   159,   211,   181,   785,   163,    73,
     163,  1408,  1983,   160,    13,    14,    15,    16,    17,  1219,
     173,   157,  1253,   157,   157,   161,  1219,   161,  1259,   182,
     159,   962,  1277,  1278,   163,  1901,   159,  1219,   159,  1905,
     163,  1405,   973,  1274,  1770,   137,  1277,  1278,   157,  1280,
     157,   982,   830,  1253,  1278,  1286,  1219,   835,  1289,   837,
    1253,  1408,  1412,  1413,  1402,    73,   137,   121,  1406,   157,
    1408,  1253,   593,   137,    73,   281,   157,   150,   151,   152,
     153,  1277,  1278,   114,   115,   116,   117,   118,   866,   163,
    1253,   869,   174,   157,   159,   167,  1278,   161,   163,   135,
     173,   169,  1333,    13,    14,    15,    16,    17,   159,   315,
     179,   159,   163,   159,   320,   163,   135,   163,   160,  1350,
     326,   157,  1302,   161,   902,   161,   159,   159,  1359,   137,
     163,   163,   168,   169,   159,   940,   135,  1219,   137,   159,
     640,  1538,   135,   163,   159,   181,  1450,  1451,  1452,   157,
      13,  1455,  1456,   161,   360,  1461,   159,  1088,   157,   159,
     938,  1392,   161,    73,   157,   162,   163,   135,   161,   168,
     169,  1253,   159,   159,  1538,   168,   169,   163,    13,    14,
      15,    16,    17,  1114,   159,   135,   162,   163,   163,   157,
    1371,  1538,   159,   161,   159,  1277,   163,   159,   163,  1219,
     168,   169,   157,  1665,  1475,   157,   412,   157,  1479,  1559,
    1665,   161,   138,   150,   151,   152,   153,  1665,   168,   169,
     162,  1665,  1665,  1665,  1306,   138,    89,   137,   138,  1453,
    1461,  1412,  1413,  1253,  1465,  1466,   173,   159,    73,  1665,
     163,   163,  1665,   163,   107,   182,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,  1487,     4,     5,     6,
       7,     8,     9,    10,    11,    12,  1779,  1665,  1046,   159,
     164,  2113,  1050,   163,   940,  2117,  1521,  1672,  1673,  1674,
    1511,  1512,  1475,  1207,  1208,  1209,  1479,  1480,   494,  1461,
    1521,  1475,   164,  1475,   157,  1479,   163,  1479,  1480,   157,
    1493,  1079,   137,   138,  1082,   162,   163,   513,   168,   169,
     181,  1493,    13,    14,    15,    16,    17,    18,   159,    66,
    1665,   771,   772,   773,   774,  1521,  1557,   163,   164,  1107,
     162,   163,   162,   163,   181,  1113,  1114,   162,   163,  1563,
     162,   163,    92,    93,   159,  1576,  1277,   159,  1654,   162,
     163,   162,  1583,  1659,   162,   163,   159,  1588,  1589,  1590,
     159,  1667,   159,  1665,   162,   163,  1828,   159,     1,   159,
    1148,     4,   578,  1828,   159,  1306,   162,   163,  1559,   161,
    1828,  1312,   162,   163,  1828,  1828,  1828,   162,   163,     1,
     162,   163,     4,  1475,   165,   601,   165,  1479,  1480,   162,
     163,   165,  1828,   162,   163,  1828,   162,   163,   165,   615,
     165,  1493,   163,   164,  1219,    13,    14,    15,    16,    17,
      18,   159,   628,  1654,  1461,    71,    59,   163,  1659,   182,
    1828,   162,   163,   162,  1665,   157,  1667,   162,   163,  1521,
     940,    79,   221,    76,  1675,   163,   164,    59,  1253,    78,
      79,   951,    85,   659,   162,  1475,  1360,  1361,    18,  1479,
    1480,   767,   768,  1694,   181,    98,   165,  1691,   101,  2051,
    1701,   165,   105,  1493,   769,   770,   182,   683,   159,   775,
     776,   159,  1654,  1828,   690,   165,  1730,  1659,   165,   101,
     182,  1894,   162,   105,   162,  1667,  1274,  1891,    18,  1277,
    1278,  1732,   207,   162,    73,  1518,  1519,  1673,  1674,  1691,
     143,  1289,   162,   156,   159,   159,   149,   159,   159,   152,
     159,    22,   155,   156,   159,  1831,   162,  1751,   159,   159,
      71,  1462,   159,   159,   159,   168,   159,   149,   107,  1770,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     159,   156,   156,  1219,   165,  1333,  1787,   165,  1789,   165,
     193,   181,   195,   196,   159,   159,   159,   159,   137,   159,
      14,   162,  1350,   206,   207,   159,   163,   165,   211,   165,
     159,  1359,   159,   163,   159,   163,   159,  1253,   157,   158,
    1521,  1941,   159,   159,   159,   163,   159,  1828,   159,   232,
    1831,   159,   159,   159,   237,   238,   159,   162,  1690,  1840,
    1841,   162,   159,   159,  1392,   159,  1847,  1654,   163,   159,
     159,   159,  1659,   159,   159,   258,   159,   159,   162,  1860,
    1667,   159,  1938,   266,  1878,   156,   163,   163,   163,  1870,
     163,  1872,   156,   162,   157,   157,  2108,  2050,   281,   157,
     157,   157,   157,  2108,  1885,   157,  1887,  1888,  1889,  1831,
    2108,   157,  1893,  1894,  2108,  2108,  2108,   164,  1861,   281,
    1475,   164,  1976,   163,  1479,  1480,   162,   165,   182,  1861,
     156,   314,  2108,  1461,   182,  2108,   181,   320,  1493,   156,
     182,   165,   163,   326,   327,   328,   159,   182,   159,   905,
     159,  1201,   159,   336,   159,   163,   162,  1938,   162,  1487,
    2108,   159,   159,   162,  1945,   156,   159,   159,  1949,  1219,
     159,   159,  2072,  1954,  2074,   358,   359,   360,    13,    14,
      15,    16,    17,    18,   162,   514,  1980,   516,   156,    81,
      18,   374,   157,  1521,  2048,   378,    13,    14,    15,    16,
      17,    18,   157,  1253,    93,   182,   962,   182,   182,  1690,
    1941,  1843,   182,  2108,   107,  2115,  1938,   156,   111,   112,
     113,   114,   115,   116,   117,   118,   119,  2008,  1278,  1861,
      58,    59,    60,    61,    62,    63,    64,    65,   421,  2020,
     182,   182,   182,  2024,  1831,   157,  1976,   157,  1576,    91,
     159,   156,  2033,  2109,   156,  1583,  2108,   163,  2039,  1475,
    1588,  1589,  1590,  1479,  1480,  2108,   163,   162,   161,  2050,
     409,  2052,   455,   164,  2017,   458,   162,  1493,   162,   162,
     165,   156,  2108,   466,   156,  2017,  2186,   164,   159,  2108,
    2146,  1861,   124,   156,   159,   434,   435,   159,   159,   156,
    2081,   484,   162,   162,  2047,   488,   159,   159,   159,   492,
     159,   494,   156,   182,   164,  2047,   455,   163,  2048,  2113,
     159,   157,   505,  2117,  2118,   580,  1654,  2108,  2109,   159,
     157,  1659,   157,   162,   156,  2078,   159,  1665,   162,  1667,
    2121,  2072,   162,  2074,   527,  2126,  2078,   159,   156,   488,
    1831,  1938,   535,   159,  2148,   159,   159,   162,    76,    76,
     156,   182,  1843,  2109,   547,  2146,   182,   157,   182,   157,
     159,   162,   156,  2154,   156,   162,  2157,  2171,  2159,   156,
     159,  2175,    76,   159,  2115,  2017,   159,  2109,   571,   161,
     573,   574,    76,   173,   577,   173,  2190,   580,    76,  2180,
    2146,   182,   657,  1453,   164,   156,   156,   182,   182,   156,
    2191,   173,   158,  2144,   173,  2047,   156,   107,   157,  2200,
     164,   163,   158,    76,  2146,  1475,   182,   162,   173,  1479,
    1480,   159,   173,   182,    85,   158,   157,   159,   164,   159,
     156,   156,  1770,  1493,   182,   159,  2078,  2017,  1929,   159,
     182,   182,   635,  1779,   637,  2186,  1327,   640,   736,  1787,
     107,  1789,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   780,   656,   657,   777,   659,  2047,   640,  1960,
    1961,    13,   778,   781,   667,   107,   779,   454,   671,   111,
     112,   113,   114,   115,   116,   117,   118,   680,  1240,  1253,
    1828,   152,  2159,  1831,  1479,  1869,  1861,   690,  2078,  2074,
     693,  2105,  1493,  1563,  1861,  2063,  1740,  2143,  1723,   702,
    1723,  1277,  2109,  2048,  2175,   172,  2118,   710,  2047,  1280,
     713,   786,   715,   716,    49,   718,   158,   866,   114,   161,
     869,  1938,   193,   272,   727,  2006,  1456,   730,   731,   732,
    1306,  1320,  1769,  1274,   866,  1527,  1751,    89,   651,  2146,
     523,     0,   802,   802,   802,  1643,  1894,    -1,    -1,    -1,
    2051,   706,    -1,    -1,    -1,   107,   831,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   850,    -1,   852,    -1,    -1,
      -1,   856,   857,   786,   859,    -1,    -1,    -1,    -1,    -1,
    1938,    -1,    -1,    -1,    -1,   266,    -1,    -1,    -1,   802,
     803,   876,    -1,    -1,   763,    -1,    -1,   810,   107,    -1,
      -1,     3,   111,   112,   113,   114,   115,   116,   117,   118,
     119,  1691,    -1,    -1,   123,  1861,   125,   830,   831,    -1,
      -1,    -1,   835,    -1,   837,    -1,    -1,    -1,    -1,    -1,
       1,    -1,  2017,     4,    -1,  2146,    -1,   850,    -1,   852,
      -1,    -1,    -1,   856,   857,   858,   859,    -1,    -1,   158,
     248,   936,   161,    -1,    -1,   336,    -1,  1443,    -1,    -1,
     945,    -1,  2047,   876,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1751,    -1,    -1,   107,  2033,  1462,   358,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,  1046,    59,   902,
      -1,  1050,  2050,  2078,  2052,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,   107,     4,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
    1079,    -1,    -1,  1082,   157,   158,    -1,   940,    -1,    -1,
     101,    -1,   945,   135,   105,  1521,   135,    -1,   951,   952,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   960,   940,   962,
    2108,  2109,    -1,    -1,    -1,   157,   158,    -1,    -1,   951,
     973,    59,   164,    -1,    -1,    -1,   168,   169,    -1,    -1,
      -1,  2017,   207,    -1,   455,    -1,    -1,    -1,   149,    -1,
      -1,  1861,    -1,    -1,    -1,   156,   107,    85,  2146,  1148,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,  2047,    -1,    -1,    -1,    -1,    -1,   488,    -1,  1022,
    1023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1103,  1104,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   227,    -1,
      -1,    -1,  2078,  2191,    -1,    -1,   207,     1,    -1,    -1,
       4,   139,  2200,    -1,    -1,   143,   527,    -1,    -1,  1018,
      -1,   149,    -1,    -1,   152,  1024,    -1,    -1,    -1,    -1,
     458,    -1,    -1,    -1,    -1,    -1,  1035,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1159,  1088,    -1,   475,    -1,    -1,
     478,     4,     5,     6,     7,     8,     9,    10,    11,    12,
    1103,  1104,   573,   574,  1107,    59,    -1,    -1,    -1,    -1,
      -1,  1114,    -1,    -1,  1690,    58,    -1,    -1,   206,    -1,
     281,    76,   311,    66,    67,    68,    69,    -1,    -1,    -1,
      -1,    85,    -1,   358,    -1,    -1,   361,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,  2017,    -1,   374,
      -1,   105,   540,   378,    -1,    -1,  1159,    -1,    -1,   247,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   264,  2047,   266,    -1,
      -1,    -1,    -1,    -1,    -1,   139,    -1,    -1,   276,   143,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,  1201,   287,
     155,  1350,    -1,    -1,    -1,    -1,    -1,  1210,  2078,   680,
    1359,    -1,    -1,    -1,   168,    -1,  1219,    -1,   161,  1201,
      -1,    -1,   310,    -1,   312,    -1,    -1,    -1,    -1,    -1,
      -1,   702,   421,    -1,    -1,    -1,   179,  1219,    -1,   710,
      -1,   195,  1245,    -1,    -1,   716,    -1,   718,   336,    -1,
    1253,    -1,   206,   207,    -1,    -1,    -1,    -1,    -1,    -1,
    1219,    -1,    -1,   488,    -1,    -1,    -1,  1843,    -1,    -1,
      -1,  1253,    -1,    -1,  1277,  1278,    -1,   232,    -1,    -1,
      -1,    -1,   237,   238,   238,    -1,  1289,    -1,    -1,    -1,
      -1,    -1,  1295,   247,    -1,  1277,  1278,    -1,    -1,  1302,
      -1,    -1,    -1,   258,   258,    -1,    -1,    -1,    -1,   263,
     264,    -1,   266,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   276,    -1,    -1,    -1,    -1,  1402,    -1,  1332,
    1405,  1406,    -1,   287,  1337,    -1,   290,    -1,  1487,   810,
     294,    -1,    -1,  1302,    -1,   299,    -1,    -1,    -1,    -1,
      -1,   305,    -1,    -1,    -1,   580,   310,    -1,   312,   314,
      -1,    -1,   316,    -1,    -1,    -1,    -1,   455,    -1,    -1,
      -1,   560,   327,   328,   328,    -1,    -1,    -1,    -1,   568,
      -1,  1340,  1341,  1342,    -1,    -1,    -1,    -1,  1347,  1348,
      -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,   587,  1402,
      -1,    -1,  1405,  1406,   358,  1408,    -1,   361,    -1,   598,
      -1,    -1,    -1,    -1,   802,   803,    -1,   505,    -1,   580,
     374,    -1,    -1,    -1,   378,   813,    -1,  1576,   816,    -1,
      -1,   656,   657,    -1,  1583,    -1,    -1,    -1,    -1,  1588,
    1589,  1590,    13,    14,    15,    16,    17,  1450,  1451,  1452,
    1453,    -1,  1455,  1456,    -1,   680,    -1,    -1,  1461,  1462,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   693,    -1,
     305,  1453,  1475,    -1,    -1,  2051,  1479,  1480,    -1,   640,
      -1,   952,    -1,    -1,    -1,    -1,    -1,  1490,    -1,    -1,
    1493,    -1,    -1,  1475,   882,    -1,   657,  1479,  1480,    -1,
      -1,   889,    73,    -1,    -1,   893,    -1,    -1,   596,   897,
      -1,  1493,    -1,    -1,    -1,    89,    -1,    -1,  1521,    -1,
      -1,   710,    -1,    -1,   713,    -1,    -1,    -1,    -1,   718,
     484,    -1,    -1,    -1,   488,  1538,   107,   492,   727,  1521,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,   505,   640,    -1,    -1,    -1,   130,   746,    -1,    -1,
    1563,   786,  1565,    -1,   135,    -1,   137,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     535,  1563,    -1,    -1,    -1,    -1,   157,   158,    -1,    -1,
      -1,    -1,   547,    -1,    -1,    -1,    -1,   168,   169,    -1,
      -1,    -1,    -1,    -1,    -1,   830,   831,    -1,   562,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   571,    -1,    -1,    -1,
      -1,  1770,   577,    -1,    -1,   786,   580,    -1,    -1,    -1,
      -1,    -1,    -1,   858,    -1,    -1,    -1,    -1,  1787,    -1,
    1789,    -1,   596,   182,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1654,  1655,    -1,    -1,    -1,  1659,    -1,  1661,    -1,
      -1,    -1,  1665,    -1,  1667,    -1,    -1,   107,    -1,    -1,
     831,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     635,    -1,   637,   123,   638,   125,   640,  1690,  1691,   850,
      -1,   852,    -1,    -1,    -1,   856,   857,    -1,   859,    -1,
    1088,    -1,   656,   657,    -1,    -1,    -1,    -1,  1690,  1691,
      -1,    -1,    -1,   667,    -1,   876,    -1,   671,   158,   908,
      -1,    73,    -1,    -1,    -1,    -1,   680,   562,    -1,    -1,
      -1,   685,    -1,   922,    -1,   960,    -1,   926,   963,   693,
      -1,   930,    -1,    -1,    -1,    -1,    -1,    -1,  1751,    -1,
     324,    -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,  1751,
      -1,    -1,    -1,    -1,    -1,  1163,  1779,  1736,  1166,   940,
      -1,    -1,  1170,   135,   945,   137,    -1,    -1,    -1,    -1,
     951,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1023,    -1,
      -1,    -1,    -1,   638,    -1,   157,   158,    -1,    -1,   161,
    1813,  1814,    -1,    -1,    -1,    -1,   168,   169,    -1,    -1,
      -1,    -1,    -1,    -1,  1295,  1828,    -1,    -1,  1831,    -1,
      -1,  1302,   786,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1843,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   802,   803,
      -1,    -1,   940,    -1,    -1,    -1,    -1,    -1,  1861,    -1,
      -1,  1843,    18,   951,   818,    -1,    -1,    -1,   822,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   830,   831,    -1,  1861,
     835,   835,   837,   837,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1894,    -1,    -1,    -1,    -1,   850,    -1,   852,    -1,
      -1,    -1,   856,   857,   858,   859,    62,    63,    64,    65,
      -1,    -1,    -1,   487,    -1,   489,    -1,    -1,    -1,    -1,
      -1,    -1,   876,    -1,   498,    -1,  1929,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1022,  1938,    -1,    -1,    -1,    -1,
      -1,    -1,  1103,  1104,    -1,    -1,    -1,   902,    -1,    -1,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,    -1,    -1,   802,    -1,    -1,
      -1,    -1,    -1,  1976,   305,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   936,   818,    -1,    -1,   940,   822,    -1,  1377,
      -1,   945,    -1,    -1,    -1,    -1,    -1,   951,  1159,    -1,
    1388,    -1,    -1,  2006,    -1,   161,   960,    -1,    -1,   963,
      -1,    -1,    -1,    -1,  2017,  1204,   970,    -1,    -1,  1490,
      -1,    -1,    -1,  1982,    -1,    -1,    -1,    -1,    -1,  1218,
      -1,    -1,    -1,    -1,    -1,  2017,    -1,    -1,    -1,    -1,
    1201,    -1,    -1,    -1,  2047,  2048,    -1,  2050,  2051,  1238,
      -1,    -1,    -1,    -1,    -1,    -1,  1245,    -1,  1219,    -1,
      -1,    -1,    -1,    -1,    -1,  2047,    -1,    -1,  1022,  1023,
      -1,    -1,    -1,    -1,    -1,  2078,    -1,  1302,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1253,   107,    -1,   109,  2078,   111,   112,   113,
     114,   115,   116,   117,   118,  2108,  2109,  1332,    -1,    -1,
      -1,    -1,    -1,  1201,    -1,    -1,  1277,  1278,    -1,    -1,
      -1,    -1,  1210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1219,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
    1094,    -1,    -1,  2146,  1098,    -1,    -1,    -1,    -1,  1103,
    1104,    -1,  1107,  1107,    -1,    73,    -1,    -1,  1113,  1114,
      -1,    -1,    -1,  1117,    -1,  1253,    -1,    -1,    -1,    -1,
    1124,    -1,    -1,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,   107,
    1661,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   135,    -1,   137,   107,  1159,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   135,  1172,   137,
      -1,    -1,  1176,   157,   158,    -1,  1180,   161,    -1,    -1,
      -1,   562,    -1,    -1,   168,   169,    -1,    -1,    -1,   157,
     158,  1402,    -1,    -1,  1405,  1406,    -1,  1201,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,  1210,    -1,    -1,  1094,
      -1,    -1,    -1,  1098,    -1,  1219,    -1,  1655,    -1,    -1,
      -1,    -1,    -1,    -1,   848,    -1,    -1,    -1,    -1,   182,
    1469,  1470,  1117,    -1,    -1,    -1,    -1,    -1,    -1,  1124,
      -1,    -1,  1453,    -1,    -1,    -1,    -1,    -1,    -1,  1253,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   638,    -1,    -1,
      -1,    -1,    -1,  1538,  1475,    -1,    -1,    -1,  1479,  1480,
      -1,    -1,    -1,    -1,  1278,    -1,    -1,    -1,    -1,    -1,
      -1,  1520,  1493,    -1,  1289,    -1,   667,  1172,    -1,    -1,
      -1,  1176,    -1,    -1,    -1,  1180,    -1,    -1,  1302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1311,    -1,    -1,
    1521,    -1,  1450,  1451,  1452,  1453,  1454,  1455,  1456,   943,
     944,    -1,    -1,    -1,    -1,    -1,  1330,    -1,  1332,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1475,    -1,    -1,
      -1,  1479,  1480,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1563,    -1,    -1,  1493,    -1,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    -1,  1813,  1814,    -1,    -1,    -1,
      -1,     1,    -1,    -1,     4,    -1,   107,  1392,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,  1402,    -1,
      -1,  1405,  1406,    -1,  1408,    -1,    -1,    -1,    -1,    -1,
     160,    -1,    -1,    -1,    -1,  1039,    -1,    -1,    -1,  1423,
      -1,   802,   172,  1427,    -1,  1563,    -1,  1431,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   818,    -1,    59,
      -1,   822,    -1,    -1,   165,    -1,  1450,  1451,  1452,  1453,
    1454,    -1,    -1,    -1,   835,    -1,  1461,  1081,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,
      -1,  1475,    -1,    -1,    -1,  1479,  1480,    -1,    -1,  1690,
    1691,   101,    -1,  1722,    -1,   105,    -1,    -1,    -1,  1493,
     107,  1929,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,  1130,   107,  1132,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   139,
    1144,    -1,  1146,   143,    -1,    -1,    -1,  1151,  1152,   149,
      -1,    -1,   152,    -1,  1538,    -1,   156,  1161,  1423,  1543,
    1751,    -1,  1427,    -1,    -1,    -1,  1431,   167,   168,    -1,
     170,   168,    -1,    -1,    -1,   936,    73,    -1,   158,  1563,
      -1,   161,    -1,  1187,  2002,    -1,  1190,    -1,  2006,    -1,
      -1,    -1,   107,   193,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,   206,   207,    -1,    -1,
     107,   211,    -1,  1597,   111,   112,   113,   114,   115,   116,
     117,   118,  1606,    -1,    -1,    -1,  1610,    -1,    -1,    -1,
      -1,    -1,  2050,  1751,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,   161,   247,    -1,  1253,
      -1,    -1,  1843,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,    -1,   264,    -1,   266,    -1,    -1,  1654,
    1861,   168,   169,    -1,  1659,    -1,   276,    -1,  1543,  1283,
    1665,   281,  1667,  1667,    -1,    -1,  1290,   287,  1292,  1293,
    2108,  2109,    -1,    -1,    -1,    -1,    -1,  1301,    -1,  1303,
      -1,  1305,   302,    -1,    -1,   305,    -1,  1691,  1312,    -1,
     310,    -1,   312,    -1,    -1,   315,   316,    -1,    -1,    -1,
     320,  1976,    -1,    -1,    -1,    -1,   326,    -1,  2146,    -1,
      -1,    -1,  1597,  1094,    -1,    -1,   336,  1098,    -1,    -1,
      -1,  1606,    -1,  1861,    -1,  1610,  1107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1117,    -1,   358,    -1,
     360,   361,    -1,  1124,    -1,    -1,    -1,  1751,    13,    14,
      15,    16,    17,    -1,   374,    -1,    -1,    -1,   378,    -1,
     107,  1385,  1386,  1222,   111,   112,   113,   114,   115,   116,
     117,   118,   119,  2048,    -1,  1234,   123,    -1,   125,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1411,    -1,    -1,
      -1,  1172,    -1,    -1,  1418,  1176,  1420,    -1,    -1,  1180,
      -1,   421,    -1,    -1,    -1,    -1,  2017,    -1,    73,    -1,
    1814,   158,    -1,    -1,   161,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1446,  1828,    -1,    -1,  1831,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   455,  2047,    -1,    -1,    -1,
      -1,    -1,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,  1858,    -1,    -1,  1861,    -1,    -1,
      -1,    -1,    -1,    -1,   484,    -1,    -1,  2078,   488,    -1,
     135,    -1,   137,    -1,   494,    -1,    -1,    -1,    -1,  2017,
      -1,    -1,    -1,    -1,    -1,   505,    -1,    -1,    -1,  1894,
      -1,    -1,   157,   158,    -1,    -1,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,    -1,   527,    -1,  2047,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1542,    -1,
      -1,    -1,    -1,    -1,    -1,  1549,    -1,  1551,    -1,    -1,
      -1,  2170,    -1,  1938,    -1,    -1,    -1,    -1,    -1,    -1,
    2078,    -1,   562,  2182,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   573,   574,    -1,    -1,    -1,   578,   107,
     580,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,  1976,    -1,    -1,    -1,   596,    -1,    -1,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,    -1,    -1,   615,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1627,    -1,    -1,    -1,   135,   628,   137,
      -1,    -1,   160,  2017,    -1,    -1,    -1,    -1,   638,    -1,
     640,    -1,    -1,    13,    14,    15,    16,    17,    -1,   157,
     158,    -1,    -1,    -1,    -1,   655,   656,   657,    -1,   659,
     168,   169,  1423,  2047,  2048,  2050,  1427,   667,    -1,    -1,
    1431,    -1,  1511,  1512,    -1,    -1,    -1,    -1,    -1,    -1,
     680,    -1,  2066,   683,    -1,    -1,    -1,   687,    -1,    -1,
     690,    -1,    -1,   693,  2078,    -1,     1,    -1,    -1,    -1,
      -1,    -1,   702,    73,    -1,    -1,    -1,    -1,    -1,    -1,
     710,    -1,    -1,   713,    -1,   715,   716,    -1,   718,    -1,
      -1,    -1,    -1,  2108,  2109,  2109,    -1,   727,    -1,    -1,
     730,   731,   732,    -1,    -1,    -1,    -1,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    59,  1759,  1760,    -1,    -1,    -1,
      -1,  2146,    -1,    -1,    -1,   135,    -1,   137,   105,  1773,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,  1543,    -1,    -1,    -1,   786,   157,   158,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
     105,  2066,   802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     810,    -1,    -1,    13,    14,    15,    16,    17,   818,    -1,
     157,    -1,   822,   160,   161,    -1,    -1,    -1,    -1,    -1,
     830,   831,    -1,    -1,   139,   835,  1597,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,  1606,    -1,    -1,    -1,  1610,
     850,    -1,   852,    -1,    -1,  1694,   856,   857,   858,   859,
      -1,    -1,  1701,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,   876,    -1,    -1,    -1,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,  1732,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   207,    -1,    -1,   905,    -1,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   160,   135,   936,   137,    -1,    -1,
     940,    -1,   247,    -1,    -1,   945,    -1,    -1,    -1,    -1,
     135,   951,   952,    -1,    -1,    -1,    -1,   157,   158,    -1,
     960,    -1,   962,   963,    -1,    -1,    -1,    -1,   168,   169,
      -1,   276,   157,   158,    -1,    -1,   161,    -1,    -1,    -1,
      -1,    -1,   287,   168,   169,   290,    -1,    -1,    -1,    -1,
     990,    -1,    -1,    -1,    -1,    -1,   181,    -1,    -1,    -1,
     305,  1840,  1841,    -1,    -1,    -1,    -1,   312,  1847,    -1,
      -1,   316,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1860,  1022,  1023,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1870,   107,  1872,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,  2049,  1885,    -1,  1887,  1888,
    1889,    -1,    -1,   358,   107,    -1,   361,    -1,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,    -1,   374,
     123,    -1,   125,   378,    -1,    -1,   102,    -1,    -1,    -1,
      -1,   107,   157,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,  1094,    -1,    -1,    -1,  1098,    -1,
      -1,    -1,  2106,  1103,  1104,   158,  1945,  1107,   161,    73,
    1949,    -1,    -1,    -1,    -1,  1954,    -1,  1117,    -1,    -1,
      -1,    -1,    -1,  2127,  1124,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,  2142,    -1,
      -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,  1159,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2008,
      -1,   135,  1172,   137,    -1,    -1,  1176,    -1,    -1,    -1,
    1180,  2020,    -1,   488,    -1,  2024,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
    2039,  1201,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,
    1210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1219,
      -1,    -1,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2081,    -1,    -1,  1245,    -1,    -1,    -1,    -1,
     135,    -1,   137,  1253,    -1,    -1,    -1,   562,    -1,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   157,   158,    -1,   580,    -1,  1277,  1278,    -1,
      -1,    -1,  2121,   168,   169,    -1,    -1,  2126,   135,    -1,
      -1,   596,    -1,    -1,    -1,  1295,    -1,    -1,    -1,    -1,
      -1,    -1,  1302,    -1,    -1,  2066,  1306,    -1,    -1,    -1,
     157,   158,    -1,    -1,    -1,  2154,    -1,   164,  2157,    -1,
    2159,   168,   169,    -1,    -1,    -1,    -1,    -1,     1,    -1,
    1330,    -1,  1332,   638,    -1,   640,    -1,  1337,    -1,    -1,
      -1,  2180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   656,   657,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   667,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   680,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,   693,    -1,
     107,   135,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,  1402,    -1,    -1,  1405,  1406,    -1,  1408,    -1,
      -1,    -1,    -1,   157,   158,    -1,    -1,   161,    -1,    -1,
      -1,    -1,    -1,  1423,   168,   169,    -1,  1427,    -1,    -1,
      -1,  1431,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,  1443,    -1,    -1,    -1,    -1,    -1,    -1,
    1450,  1451,  1452,  1453,  1454,  1455,  1456,    -1,    -1,    -1,
      -1,    -1,  1462,    -1,    -1,    -1,   139,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1475,   149,    -1,    -1,  1479,
    1480,   786,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1490,    -1,    -1,  1493,    -1,   168,    -1,   802,    -1,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,   818,    -1,    -1,    -1,   822,    -1,    -1,
      -1,  1521,    -1,    -1,    -1,   830,   831,   135,    -1,    -1,
     835,    -1,    -1,    -1,   207,    -1,    -1,    -1,  1538,    -1,
      -1,    -1,    -1,  1543,    -1,   850,    -1,   852,    -1,   157,
     158,   856,   857,   858,   859,    -1,  1556,    -1,    -1,    -1,
     168,   169,    -1,  1563,    -1,    -1,    48,    -1,    -1,    -1,
      -1,   876,    -1,    -1,   247,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    -1,    -1,  1597,    -1,    -1,
      -1,    -1,    -1,   276,   135,    -1,  1606,    -1,    -1,    -1,
    1610,    -1,    -1,    -1,   287,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   157,   158,    -1,    -1,
     154,   936,   305,    -1,    -1,   940,    -1,   168,   169,   312,
     945,   123,   107,   316,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   136,   960,   138,   181,   963,    -1,
      -1,  1661,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   358,    -1,   169,   361,   171,
    1690,  1691,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   374,    -1,    -1,    -1,   378,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   196,    -1,    -1,    -1,  1023,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1751,    -1,    -1,    -1,   237,    -1,    -1,    -1,   241,
      -1,    -1,   244,   245,    -1,    -1,   248,    -1,    -1,   251,
     252,    -1,   254,    -1,   256,    -1,     3,    -1,    -1,  1779,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1094,
      -1,    -1,    -1,  1098,    -1,    -1,    -1,    -1,  1103,  1104,
      -1,    -1,  1107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1117,    -1,    -1,   488,    -1,    -1,    -1,  1124,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1843,    -1,   327,    -1,    -1,   330,    -1,
      -1,    -1,    79,    -1,  1159,    -1,    -1,    -1,  1858,    -1,
      -1,  1861,    -1,    -1,    -1,    -1,    -1,  1172,    -1,    -1,
      -1,  1176,    -1,   355,    -1,  1180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   185,    -1,   370,   562,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   580,    -1,    -1,
      -1,    -1,    -1,    -1,  1219,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   596,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1253,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1959,
    1960,    -1,    -1,    -1,    -1,   638,    -1,   640,    -1,    -1,
      -1,    -1,    -1,  1278,    -1,    -1,  1976,    -1,    -1,    -1,
      -1,   208,    -1,   656,   657,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   475,   667,    -1,    -1,  1302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   680,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2017,    -1,    -1,
     693,    -1,    -1,    -1,    -1,  1330,    -1,  1332,    -1,    -1,
      -1,    -1,    -1,  2033,    -1,    -1,    -1,    -1,   265,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2047,  2048,    -1,
      -1,  2051,    -1,   535,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2066,    -1,    -1,   551,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2078,   306,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   318,    -1,    -1,    -1,    -1,    -1,  1402,    -1,    -1,
    1405,  1406,    -1,  1408,    -1,   404,    -1,   406,   335,    -1,
     409,   410,    -1,   786,    -1,    -1,    -1,    -1,  1423,    -1,
     419,   420,  1427,    -1,    -1,    -1,  1431,    -1,    -1,   802,
     357,    -1,    -1,    -1,    -1,   434,   435,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   818,    -1,    -1,    -1,   822,
      -1,    -1,    -1,   635,    -1,    -1,   455,   830,   831,    -1,
      -1,    -1,   835,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1475,    -1,    -1,    -1,  1479,  1480,    -1,   850,    -1,   852,
      -1,    -1,    -1,   856,   857,   858,   859,    -1,  1493,   488,
      -1,    -1,    -1,   675,   676,   422,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   876,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   695,    -1,   697,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   453,    -1,    -1,    -1,
      -1,    -1,    -1,  1538,    -1,    -1,    -1,    -1,  1543,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   481,    -1,    -1,    -1,    -1,   486,
      -1,    -1,    -1,   936,    -1,    -1,    -1,   940,    -1,    -1,
      -1,    -1,   945,    -1,    -1,    -1,    -1,    -1,    -1,   506,
     507,    -1,    -1,    -1,   511,   512,    -1,   960,   515,    -1,
     963,    -1,  1597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1606,    -1,    -1,   531,  1610,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   806,   807,   553,    -1,    -1,    -1,
      -1,   813,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1023,    -1,    -1,    -1,    -1,    -1,   838,    -1,    -1,   841,
     842,    -1,   844,    -1,   846,   847,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1691,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   631,    -1,    -1,   889,    -1,    -1,
      -1,   893,    -1,    -1,    -1,   897,    -1,   644,    -1,    -1,
      -1,  1094,    -1,    -1,    -1,  1098,    -1,    -1,    -1,    -1,
    1103,  1104,    -1,    -1,  1107,    -1,    -1,    -1,    -1,    -1,
      -1,   668,    -1,    -1,  1117,    -1,    -1,    -1,    -1,    -1,
      -1,  1124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   763,   764,   765,   766,   767,   768,
     769,   770,   771,   772,   773,   774,   775,   776,   777,   778,
     779,   780,   781,    -1,    -1,    -1,  1159,    -1,    -1,   971,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1172,
      -1,    -1,    -1,  1176,    -1,    -1,   733,  1180,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,  1219,   168,    -1,   848,
      51,    -1,    53,    -1,    -1,    -1,  1861,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1253,    -1,    -1,    -1,    -1,   206,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   825,    -1,
     827,    -1,    -1,    -1,    -1,  1278,   833,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     241,    -1,    -1,    -1,    -1,    -1,    -1,   248,    -1,  1302,
      -1,  1113,    -1,    -1,   861,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,   870,    -1,    -1,    -1,   874,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1330,    -1,  1332,
      -1,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1976,    -1,    -1,    -1,    -1,  1158,    -1,  1160,    -1,
      -1,  1163,    -1,    -1,  1166,    -1,    -1,    -1,  1170,    -1,
      -1,    -1,   919,    -1,    -1,    -1,    -1,   924,    93,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   330,
      -1,    -1,  2017,    -1,    -1,    -1,    -1,    -1,    -1,  1018,
      -1,    -1,    -1,    -1,    -1,  1024,    -1,    -1,    -1,  1402,
      -1,    -1,  1405,  1406,    -1,  1408,  1035,   358,   359,    -1,
      -1,    -1,  2047,  2048,    -1,    -1,    -1,    -1,    -1,   144,
    1423,    -1,   147,    -1,  1427,    -1,    -1,   378,  1431,    -1,
      -1,  2066,    -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,
      -1,    -1,    -1,  2078,    -1,    -1,  1075,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1011,    -1,    -1,   182,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   194,
      -1,    -1,  1475,    -1,    -1,    -1,  1479,  1480,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1493,    -1,    -1,    -1,    -1,    -1,  1308,    -1,    -1,    -1,
      -1,   226,    -1,    -1,    -1,    -1,    -1,   458,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   475,   476,    -1,   478,   479,    -1,
      -1,    -1,    -1,    -1,    -1,  1538,    -1,   488,    -1,    -1,
    1543,   492,    -1,   268,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   277,   278,   505,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   288,    -1,  1377,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1388,    -1,   303,  1391,
      -1,  1393,  1394,    -1,    -1,   536,    -1,    -1,    -1,   540,
    1219,    -1,    -1,    -1,  1597,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1606,    -1,    -1,    -1,  1610,    -1,    -1,
      -1,    -1,    -1,   338,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   346,   347,    -1,    -1,    -1,   351,    -1,    -1,   580,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1197,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   390,    -1,    -1,   393,    -1,
      -1,   396,    -1,  1302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   636,    -1,    -1,  1691,  1246,
    1502,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   656,   657,    -1,    -1,    -1,
      -1,  1340,  1341,  1342,    -1,    -1,   667,    -1,  1347,  1348,
     671,    -1,    -1,    -1,    -1,    -1,    -1,   678,    -1,   680,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1371,  1300,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   491,  1324,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   503,   504,
      -1,    -1,    -1,  1412,  1413,    13,    14,    15,    16,    17,
      -1,    -1,    20,  1605,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,  1633,    50,    51,    -1,    53,    -1,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,   786,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,   802,   803,  1665,    -1,  1858,    -1,    -1,  1861,  1671,
      -1,    -1,   813,   814,    -1,   816,   817,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,
     831,   109,   110,    -1,   835,    -1,   837,   838,    -1,    -1,
      -1,    -1,    -1,   844,    -1,    -1,    -1,    -1,    -1,   850,
      -1,   852,    -1,    -1,    -1,   856,   857,   858,   859,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   642,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   876,   651,   878,    -1,    -1,
    1559,   882,   160,    -1,    85,  1747,    -1,    -1,   889,   890,
      -1,    -1,   893,   894,    -1,    -1,   897,   898,    -1,    -1,
     101,    -1,    -1,   904,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1529,  1976,    -1,    -1,    -1,    -1,    -1,    -1,
      48,   706,  1794,  1795,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   945,   946,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,    -1,   156,    -1,    -1,  1820,  1821,
      -1,    -1,    -1,    -1,  2017,    -1,  1828,   168,    -1,    -1,
      -1,  1833,   973,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   757,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   193,    -1,  2047,  2048,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   123,   207,    -1,    -1,    -1,
     211,    -1,    -1,  2066,    -1,    -1,    -1,    -1,   136,    -1,
     138,  1022,  1023,    -1,    -1,  2078,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1648,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   169,    -1,    -1,    -1,    -1,    -1,  1736,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   266,    -1,    -1,    -1,  1931,
      -1,    -1,    -1,    -1,   849,    -1,    -1,    -1,    -1,    -1,
     281,    -1,    -1,    -1,    -1,   860,    -1,  1088,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1103,  1104,    -1,    -1,  1107,  1108,    -1,    -1,
      -1,    -1,    -1,  1114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   326,   244,   245,    -1,    -1,
     248,    -1,    -1,   251,   252,   336,   254,    -1,   256,    -1,
    2002,    -1,    -1,    -1,    -1,  1752,  1753,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   358,  1159,   360,
      -1,    -1,  1163,  1164,   939,  1166,  1167,    -1,    -1,  1170,
    1171,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,   994,
     421,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,   355,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2108,    -1,    -1,    -1,
      -1,    -1,   370,    -1,   455,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1941,    -1,    -1,    -1,    -1,    -1,   107,  1876,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   488,    -1,    -1,
      -1,    -1,    -1,   494,    -1,    -1,   135,    -1,   137,  1906,
      -1,  1302,    -1,  1982,    -1,    -1,    -1,  1308,  1309,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,   160,   161,    -1,    -1,    -1,   527,    -1,    -1,   168,
     169,  1332,    -1,    -1,    -1,    -1,  1943,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   475,    -1,    -1,
      -1,    -1,    -1,    -1,  1971,    -1,    -1,    -1,  1975,    -1,
      -1,    -1,   573,   574,    -1,    -1,  1377,  1378,    -1,   580,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1388,  1389,    -1,
    1391,    -1,    -1,  2072,    -1,  2074,    -1,    -1,    -1,    -1,
      -1,  1402,    -1,    -1,  1405,  1406,    -1,  1408,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   551,    -1,    -1,  2115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1225,  1226,  1227,    -1,    -1,    -1,   657,    -1,   659,    -1,
      -1,    -1,    -1,    -1,    -1,  2144,    -1,    -1,    -1,    -1,
      -1,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   680,
      -1,    -1,    -1,    -1,  1259,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   702,    -1,    -1,    -1,  1280,    -1,  2186,    -1,   710,
     207,  1286,   713,    -1,   715,   716,    -1,   718,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   727,    -1,    -1,   730,
     731,   732,    -1,    -1,    -1,    -1,    -1,  1538,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   675,   676,    -1,
      -1,    -1,    -1,    -1,  1565,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   695,    -1,   697,
      -1,    -1,    -1,    -1,    -1,   786,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   305,   810,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     831,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   850,
      -1,   852,    -1,    -1,  1655,   856,   857,   858,   859,    -1,
      -1,   358,    -1,   360,   361,    -1,    -1,    -1,    -1,    -1,
    1671,    -1,    -1,    -1,    -1,   876,    -1,   374,    -1,    -1,
      -1,   378,    -1,    -1,    -1,    -1,    -1,    -1,   806,   807,
    1465,  1466,    -1,    -1,    -1,   813,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     838,    -1,    -1,   841,   842,    -1,   844,    -1,   846,   847,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   945,    -1,    -1,    -1,    -1,    -1,
      -1,   952,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   962,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   889,    -1,    -1,    -1,   893,    -1,    -1,    -1,   897,
      -1,    -1,  1557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   488,    -1,    -1,    -1,    -1,    -1,   494,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1813,  1814,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1023,    -1,    -1,    -1,    -1,    -1,  1829,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   971,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   562,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   580,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1675,    -1,  1103,  1104,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   615,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1929,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1937,    -1,    -1,    -1,
      -1,   638,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1159,   656,
     657,    -1,   659,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     667,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   680,    -1,    -1,   683,    -1,    -1,    -1,
      -1,    -1,    -1,   690,    -1,  1113,   693,    -1,    -1,    -1,
      -1,  2002,  2003,    -1,    -1,  2006,    -1,    -1,    -1,  1210,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1158,    -1,  1160,    -1,  1245,  1163,    -1,    -1,  1166,  2050,
      -1,    -1,  1170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1277,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   786,
      -1,    -1,    -1,    -1,  1295,    -1,    -1,    -1,    -1,    -1,
      -1,  1302,    -1,    -1,    -1,   802,    -1,  2108,  2109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1893,   305,
      -1,   818,    -1,    -1,    -1,   822,    -1,    -1,    -1,    -1,
      -1,  1332,    -1,   830,   831,    -1,  1337,    -1,   835,    -1,
      -1,    -1,    -1,    -1,    -1,  2146,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   850,    -1,   852,    -1,    -1,    -1,   856,
     857,   858,   859,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   358,    -1,   360,   361,    -1,    -1,    -1,   876,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   374,    -1,
    1308,    -1,   378,    -1,    -1,    -1,   421,    -1,    -1,    -1,
      -1,  1402,    -1,    -1,  1405,  1406,    -1,  1408,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   936,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   945,  1450,
    1451,  1452,    -1,    -1,  1455,  1456,    -1,    -1,    -1,  1377,
      -1,  1462,    -1,   960,    -1,   962,   963,    -1,    -1,    -1,
    1388,    -1,    -1,  1391,    -1,  1393,  1394,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1490,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,   494,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1521,    -1,    -1,    -1,    -1,    -1,  1023,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1538,    -1,    -1,
      -1,   168,    -1,    -1,    -1,    -1,    -1,    -1,   573,   574,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   562,    -1,    -1,    -1,
     207,    -1,    -1,    -1,  1502,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   580,    -1,    -1,  1094,    -1,    -1,
      -1,  1098,    -1,    -1,    -1,    -1,  1103,  1104,    -1,    -1,
    1107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1117,    -1,    -1,    -1,    -1,    -1,    -1,  1124,    -1,   615,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   638,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1661,    -1,  1159,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     656,   657,    -1,   659,    -1,  1172,    -1,    -1,   305,  1176,
      -1,   667,    -1,  1180,    -1,   710,    -1,  1605,   713,  1690,
      -1,    -1,    -1,   718,   680,    -1,    -1,   683,    -1,    -1,
      -1,    -1,   727,    -1,   690,    -1,    -1,   693,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1633,    -1,    -1,    -1,    -1,
      -1,   746,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   358,    -1,   360,   361,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   374,    -1,    -1,
      -1,   378,    -1,    -1,    -1,    -1,    -1,   782,    -1,    -1,
     193,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   207,    -1,    -1,    -1,  1779,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   221,    -1,
     223,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     786,    -1,    -1,    -1,    -1,  1302,    -1,    -1,    -1,  1306,
      -1,    -1,    -1,    -1,    -1,    -1,   802,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1747,
      -1,    -1,   818,  1330,    -1,  1332,   822,    -1,    -1,    -1,
      -1,    -1,  1843,    -1,   830,   831,    -1,    -1,    -1,   835,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   488,    -1,    -1,   850,    -1,   852,   494,    -1,    -1,
     856,   857,   858,   859,    -1,    -1,  1794,  1795,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     876,    -1,   325,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1820,  1821,    -1,  1402,    -1,    -1,  1405,  1406,
      -1,  1408,    -1,    -1,    -1,  1833,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1423,    -1,    -1,    -1,
    1427,    -1,    -1,    -1,  1431,   562,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     936,    -1,    -1,   580,    -1,    -1,    -1,    -1,    -1,   945,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   960,  1976,   962,   963,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   615,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   638,    -1,  1931,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   656,
     657,    -1,   659,    -1,    -1,    -1,    -1,  1023,    -1,    -1,
     667,  1538,    -1,    -1,    -1,    -1,  1543,  2048,    -1,    -1,
    2051,    -1,    -1,   680,    -1,    -1,   683,    -1,    -1,    -1,
      -1,    -1,    -1,   690,    -1,    -1,   693,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   516,  2002,    -1,    -1,    -1,    -1,   522,
      -1,    -1,    -1,    -1,   527,    -1,    -1,    -1,    -1,    -1,
    1597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1094,  1606,
      -1,    -1,  1098,  1610,    -1,    -1,    -1,  1103,  1104,    -1,
      -1,  1107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1117,    -1,    -1,    -1,    -1,    -1,    -1,  1124,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   786,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1159,    -1,   802,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1172,    -1,    -1,    -1,
    1176,   818,    -1,    -1,  1180,   822,   629,    -1,    -1,    -1,
      -1,    -1,    -1,   830,   831,    -1,    -1,    -1,   835,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1245,    -1,    -1,   850,   657,   852,    -1,    -1,    -1,   856,
     857,   858,   859,    -1,    -1,    -1,    -1,   670,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   876,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   707,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   720,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   936,
      -1,   744,   745,    -1,    -1,   748,  1302,   750,   945,    -1,
    1306,    -1,    -1,   756,    -1,   758,   759,    -1,    -1,    -1,
      -1,    -1,    -1,   960,    -1,   962,   963,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1330,    -1,  1332,    -1,    -1,    -1,
      -1,    -1,    -1,   786,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   799,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   810,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   826,    -1,    -1,  1023,    -1,   831,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1402,    -1,    -1,  1405,
    1406,    -1,  1408,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     863,    -1,    -1,   866,    -1,    -1,    -1,  1423,    -1,    -1,
      -1,  1427,    -1,    -1,   877,  1431,    -1,    -1,    -1,    -1,
    1475,  1476,    -1,    -1,  1479,  1480,    -1,    -1,    -1,    -1,
    1485,    -1,    -1,    -1,  1489,    -1,  1491,  1094,  1493,     1,
      -1,  1098,   905,    -1,    -1,    -1,  1103,  1104,    -1,  1976,
    1107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1117,    -1,    -1,    -1,    -1,    -1,    -1,  1124,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,   952,
      52,    -1,    54,    55,    -1,    57,    -1,    -1,    -1,   962,
     963,    -1,  1159,    -1,    -1,    -1,    -1,   970,    -1,    -1,
      -1,    -1,    74,    -1,    -1,  1172,    -1,    -1,    -1,  1176,
      -1,  2048,  1538,  1180,  2051,    -1,    -1,  1543,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2066,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
    1023,   123,   124,   125,    -1,   127,   128,    -1,  1031,    -1,
      -1,    -1,    -1,   135,    -1,    -1,    -1,  1040,    -1,    -1,
      -1,  1597,    -1,    -1,    -1,    -1,  1641,    -1,    -1,    -1,
    1606,    -1,    -1,    -1,  1610,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1082,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1686,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1302,    -1,    -1,    -1,  1306,
      -1,  1706,  1707,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1330,    -1,  1332,    -1,    -1,    -1,    -1,
      -1,    -1,  1737,     4,     5,     6,     7,     8,     9,    10,
      11,    12,  1155,    -1,  1157,    -1,    -1,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,    -1,    -1,    59,    60,
      61,    62,    63,    64,    65,  1402,    -1,    -1,  1405,  1406,
      -1,  1408,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
      55,    -1,    57,    -1,    -1,    -1,  1423,    -1,    -1,    -1,
    1427,    -1,    -1,    -1,  1431,    -1,  1239,  1240,    -1,    74,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1853,    -1,
    1855,    -1,    -1,  1858,  1859,    -1,  1861,    -1,    -1,   104,
     105,  1866,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    -1,   127,   128,    -1,    -1,    -1,    -1,   159,    -1,
     135,    -1,    -1,  1306,    -1,    -1,    -1,    -1,    -1,  1312,
      -1,    -1,    -1,    -1,    -1,   150,   151,   152,   153,    -1,
      -1,   182,   157,   158,  1327,   160,   161,    -1,    -1,  1332,
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,  1538,    -1,    -1,    -1,    -1,  1543,  1350,    -1,    -1,
    1353,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1959,  1368,    -1,    -1,    -1,    -1,
      -1,  1966,  1967,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1986,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1606,
      -1,    -1,    -1,  1610,    -1,    -1,    -1,    -1,    -1,    -1,
    1976,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2025,    -1,  2027,    -1,    -1,  2030,  2031,    -1,    -1,  1442,
    1443,    -1,  2037,  2038,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1468,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1487,    -1,    -1,  1490,    -1,    -1,
      -1,    -1,  2048,    -1,    -1,  2051,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,  2101,  2102,  2103,    -1,
    2066,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1538,    -1,  2132,  2133,  2134,
      -1,    -1,    -1,    -1,  1547,  1548,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1576,    -1,    -1,    72,  1580,    74,    75,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,    -1,   102,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1661,    -1,
     156,   157,    -1,  1666,   160,   161,    -1,    -1,    -1,   165,
      -1,   167,   168,   169,   170,   171,   172,   173,     3,    -1,
       5,    -1,    -1,    -1,    -1,    10,   182,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,  1727,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1770,    -1,    -1,
      -1,    -1,    -1,    -1,  1777,    -1,    -1,  1780,    -1,  1976,
      -1,   106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1806,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    18,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   156,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    49,   168,   169,    52,    -1,    54,    55,    -1,
      57,  2048,    -1,    -1,  2051,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    74,    75,  2066,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,    -1,   102,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,    -1,
     127,   128,    -1,     1,    -1,    -1,    -1,    -1,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,   182,    54,    55,    -1,    57,
    1983,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    74,    75,    -1,    77,
      -1,    -1,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,    -1,   102,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,
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
      -1,    -1,    70,    -1,    72,    73,    74,    75,    -1,    77,
      -1,    -1,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   182,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
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
     128,    -1,     5,    -1,    -1,    -1,    -1,   135,    -1,   137,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    49,    -1,    -1,    52,
      -1,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,   172,
     173,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,
      -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     102,    -1,    -1,    -1,   106,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
     122,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     3,     4,
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
     165,    -1,    -1,   168,   169,     3,     4,     5,     6,     7,
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
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    78,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,     3,
      -1,     5,    -1,    -1,   168,   169,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,     3,
      -1,     5,    -1,    -1,   168,   169,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,     3,
      -1,     5,    -1,    -1,   168,   169,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,    56,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    73,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,   109,   110,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    -1,    -1,   109,   110,    -1,
      -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,   159,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,     4,     5,     6,     7,     8,     9,
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
     160,   161,    -1,    -1,    -1,   165,    -1,    -1,   168,   169,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
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
      -1,    -1,    -1,   159,   160,   161,    -1,    -1,    -1,    -1,
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
      -1,   135,    -1,   137,   138,    -1,    -1,    -1,    -1,    -1,
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
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    72,    -1,    74,    75,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    -1,    -1,    94,    95,
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
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    49,   127,   128,    52,    -1,    54,    55,    -1,    57,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,   152,   153,    -1,    -1,    -1,   157,
     158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    73,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,   107,
      56,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,   164,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   137,    -1,    -1,    -1,    -1,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,   160,    22,    23,    24,    25,    26,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     137,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   159,   160,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,   160,   161,    -1,    -1,    -1,   165,    -1,    -1,   168,
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
      -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,
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
      -1,    -1,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    13,    14,    15,    16,    17,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    13,    14,    15,    16,    17,    18,    -1,    20,
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
      -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
     138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   159,   160,     4,     5,     6,     7,     8,     9,    10,
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
      -1,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,   160,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
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
      -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,    -1,   109,   110,    -1,    -1,    -1,   168,   169,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
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
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    18,    73,    20,
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
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,   109,
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
      62,    63,    64,    65,    -1,    -1,    13,    14,    15,    16,
      17,    73,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,   109,   110,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,    -1,
      -1,    -1,   109,   110,    -1,    -1,   168,   169,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    -1,    13,    14,    15,    16,
      17,   168,   169,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    73,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,   109,   110,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    -1,    -1,   109,   110,    -1,
      -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    20,    -1,
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
     172,   173,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    13,    14,    15,    16,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,   160,    22,    23,    24,    25,    26,    27,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,   160,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    73,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,   107,    49,   109,   110,    52,    -1,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      49,   127,   128,    52,   137,    54,    55,    -1,    57,   135,
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
     158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
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
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
     164,    -1,    -1,   167,   168,   169,   170,   171,   172,   173,
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
      -1,    74,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
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
      -1,   157,    -1,   159,   160,   161,    -1,    -1,    -1,    -1,
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
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    49,
      -1,    -1,    52,   135,    54,    55,    -1,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,   157,   158,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    49,   127,   128,    52,
      -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,
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
     119,   120,   121,    -1,   123,   124,   125,    49,   127,   128,
      52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
     169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    49,    53,    -1,    52,    56,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    49,   137,   135,    52,    -1,
      54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      74,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,
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
     168,   169,   187,   188,   189,   205,   297,   298,   299,   300,
     301,   302,   303,   304,   305,   306,   307,   308,   311,   314,
     316,   317,   318,   319,   320,   321,   322,   323,   324,   325,
     327,   329,   330,   331,   333,   334,   338,   339,   340,   341,
     342,   344,   350,   351,   352,   353,   364,   369,   402,   405,
     415,   421,   423,   429,   433,   438,   439,   440,   441,   442,
     443,   444,   445,   471,   489,   490,   491,   492,     0,   184,
     107,   188,   205,   301,   303,   314,   317,   320,   330,   334,
     339,   121,   157,    59,    62,    63,    65,   157,   157,   367,
     427,   428,   429,   326,   327,   109,   110,   188,   190,   403,
     404,   190,   157,   415,   157,   157,     4,   107,   109,   110,
     318,   323,   324,   157,   157,   205,   428,   433,   439,   440,
     441,   443,   444,   445,   109,   341,   162,   184,   161,   304,
     314,   317,   438,   442,   488,   489,   492,   493,   182,   185,
     154,   165,   181,   226,   385,    90,   163,   422,   102,   190,
     426,   163,   163,   163,   182,   109,   110,   157,   205,   309,
     310,   433,   434,   435,   436,   437,   438,   442,   446,   447,
     448,   449,   450,   451,   452,   453,   454,   460,     3,    47,
      48,    50,    56,   332,     3,   161,   205,   303,   304,   318,
     322,   324,   335,   340,   418,   438,   442,   492,    70,   301,
     303,   317,   330,   334,   339,   419,   438,   442,    66,   323,
     323,   318,   324,   312,   323,   324,   332,   351,   318,   323,
     318,   160,   427,   163,   185,   157,   165,   234,   427,   427,
       3,   292,   293,   308,   311,   317,   321,   322,   161,   314,
     317,   490,   190,   190,   415,   181,   317,   157,   205,   424,
     433,   434,   438,   447,   451,   161,   205,   304,   492,   416,
     417,    58,    66,    67,    68,    69,   161,   179,   190,   391,
     393,   397,   399,   400,   340,    58,   159,   161,   205,   313,
     317,   321,   329,   330,   336,   337,   338,   339,   343,   350,
     351,   369,   379,   381,   471,   484,   485,   486,   487,   492,
     493,   427,   109,   110,   172,   188,   340,   368,   460,   429,
     157,   398,   399,   157,    13,    89,   157,   190,   430,   431,
     432,   121,   191,   192,    49,    52,    54,    55,    57,    74,
     104,   105,   107,   108,   119,   120,   123,   124,   125,   127,
     128,   157,   161,   167,   170,   171,   172,   173,   186,   187,
     191,   193,   196,   204,   205,   206,   207,   210,   211,   212,
     213,   214,   215,   216,   217,   218,   219,   220,   221,   222,
     228,   340,   159,   161,   204,   205,   221,   223,   314,   340,
     383,   384,   401,   488,   493,   430,   317,   439,   440,   441,
     443,   444,   445,   159,   159,   159,   159,   159,   159,   159,
     109,   161,   188,   314,   471,   490,   161,   168,   205,   223,
     303,   304,   313,   315,   317,   330,   337,   339,   376,   377,
     378,   380,   381,   484,   492,   162,   157,   161,   438,   442,
     492,   157,   163,   107,   160,   161,   165,   187,   189,   223,
     386,   387,   388,   389,   390,    22,   386,   157,   190,   234,
     157,   157,   188,   424,   188,   428,   433,   435,   436,   437,
     446,   448,   449,   450,   452,   453,   454,   317,   434,   447,
     451,   163,   426,   161,   427,   468,   471,   426,   427,   427,
     422,   292,   157,   427,   468,   426,   427,   427,   422,   427,
     427,   317,   424,   157,   157,   316,   317,   314,   317,   162,
     184,   314,   488,   493,   426,   342,   165,   422,   292,   190,
     190,   385,   303,   322,   420,   438,   442,   165,   422,   292,
     403,   317,   330,   317,   317,   109,   341,   109,   110,   188,
     340,   345,   403,   138,   188,   317,   373,   374,   378,   379,
     382,   156,   184,   234,   308,   182,   438,   451,   317,   184,
     426,   157,   426,   185,   223,   428,   433,   317,   157,   190,
     413,   165,   157,   190,   165,   190,   138,   168,   169,   396,
     159,   163,   190,   400,   159,   427,   162,   184,   315,   317,
     330,   337,   339,   483,   484,   492,   493,   157,   161,   169,
     181,   205,   471,   473,   474,   475,   476,   477,   478,   495,
     205,   343,   492,   317,   337,   323,   318,   427,   159,   315,
     317,   485,   315,   471,   485,   188,   368,   460,   365,   165,
     368,   391,   181,   391,   430,   159,   163,   157,   159,   121,
     157,   204,   157,   157,   204,   157,   157,   207,   157,   204,
     157,   107,   109,   110,   318,   323,   324,   157,   204,   204,
      19,    21,    86,   161,   170,   171,   208,   209,   223,   230,
     234,   353,   383,   492,   163,   184,   157,   193,   161,   166,
     161,   166,   124,   126,   127,   128,   157,   160,   161,   165,
     166,   207,   207,   174,   168,   175,   176,   170,   171,   129,
     130,   131,   132,   177,   178,   133,   134,   169,   167,   179,
     135,   136,   180,   159,   163,   160,   184,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   181,   225,
     226,   227,   157,   205,   464,   465,   466,   467,   468,   159,
     163,   159,   159,   159,   159,   159,   159,   159,   157,   427,
     468,   471,   157,   468,   471,   157,   184,   157,   314,   490,
     162,   184,   185,   161,   185,   157,   169,   205,   433,   455,
     456,   457,   458,   459,   460,   461,   462,   463,   138,   492,
     163,   185,   163,   185,   190,   190,   157,   184,   184,   184,
     184,   161,   189,   184,   387,   164,   163,   494,   386,   160,
     161,   164,   390,   401,   157,   191,   184,   181,   433,   435,
     436,   437,   446,   448,   449,   450,   452,   453,   454,   159,
     159,   159,   159,   159,   159,   159,   159,   159,   159,   434,
     447,   451,   427,   181,   162,   184,   385,   234,   422,   373,
     385,   234,   424,   230,   384,   230,   384,   424,   109,   161,
     413,   234,   422,   426,   165,   165,   422,   292,   413,   234,
     422,   347,   348,   346,   165,   159,   163,   159,   163,    71,
     294,   295,   182,   168,   223,   184,   433,   415,   413,   190,
     162,   184,   157,   395,   393,   394,    79,   328,   188,   315,
     471,   485,   317,   321,   492,   373,   474,   475,   476,   162,
     184,    18,   223,   317,   473,   495,   427,   427,   471,   315,
     483,   493,   317,   188,   315,   485,   427,   165,   427,   368,
      10,   167,   368,   370,   371,   165,   159,   384,   159,   159,
     431,   158,   197,   198,   199,   223,   182,   383,   493,   193,
     383,   161,   383,   384,   383,   493,   223,   383,   159,   383,
     383,   383,   162,   184,   159,   170,   171,   209,    18,   319,
     159,   163,   159,   168,   169,   159,   158,   223,   229,   223,
     165,   223,   188,   223,   188,   119,   161,   188,   197,   119,
     161,   190,   353,   223,   197,   188,   207,   210,   210,   210,
     211,   211,   212,   212,   213,   213,   213,   213,   214,   214,
     215,   216,   217,   218,   219,   164,   230,   182,   191,   161,
     188,   223,   165,   223,   373,   465,   466,   467,   317,   464,
     427,   427,   223,   384,   157,   427,   468,   471,   157,   468,
     471,   373,   373,   184,   184,   162,   162,   157,   433,   456,
     457,   458,   461,    18,   317,   455,   459,   157,   427,   477,
     495,   427,   427,   495,   157,   427,   477,   427,   427,   185,
     222,   190,   377,   380,   162,   380,   381,   162,   495,   495,
     138,   375,   376,   377,   375,   377,   375,   190,   184,   221,
     222,   223,   425,   494,   386,   388,   156,   184,   159,   184,
     159,   375,   223,   159,   159,   159,   159,   159,   159,   159,
     159,   159,   157,   427,   468,   471,   157,   427,   468,   471,
     157,   427,   468,   471,   424,    22,   471,   223,   324,   340,
     469,   234,   159,   159,   159,   159,   159,   411,   412,   234,
     156,   184,   413,   234,   422,   412,   234,   165,   165,   165,
     354,   138,   378,   379,   188,   190,   296,    18,    72,    74,
      75,    77,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    94,    95,    96,    97,    98,    99,
     100,   102,   109,   110,   122,   157,   161,   190,   230,   231,
     232,   233,   234,   235,   236,   238,   239,   248,   255,   256,
     257,   258,   259,   260,   265,   266,   269,   270,   271,   272,
     273,   274,   275,   281,   282,   283,   297,   317,   321,   423,
      71,   185,   185,   375,   414,   412,   159,   301,   303,   314,
     406,   407,   408,   409,   401,   181,   392,   392,   315,   485,
     161,   168,   205,   223,   340,   223,   317,   159,   159,   159,
     159,     5,   317,   427,   473,   366,   370,   368,   165,   340,
     163,   494,   190,   370,   165,   159,   159,   163,   159,   159,
     163,   159,   184,   163,   159,   159,   159,   163,   159,   207,
     159,   159,   159,   207,    18,   319,   223,   159,   159,   158,
     165,   207,   162,   163,   185,   197,   162,   162,   119,   123,
     125,   189,   200,   201,   202,   159,   200,   162,   163,   156,
     221,   164,   159,   200,   185,   387,   159,   159,   159,   159,
     464,   373,   373,   159,   159,   375,   375,   461,   159,   159,
     159,   159,   157,   433,   460,   455,   459,   373,   373,   162,
     185,   495,   163,   185,   159,   163,   163,   185,   163,   185,
     385,   200,   138,   173,   185,   185,   156,   386,   223,   427,
     375,   427,   185,   157,   427,   468,   471,   157,   427,   468,
     471,   157,   427,   468,   471,   373,   373,   373,   426,   151,
     173,   185,   470,   163,   185,   414,   406,   412,   234,   414,
     354,   354,   354,     3,     5,    10,    74,   156,   298,   305,
     306,   314,   317,   355,   360,   488,   163,   182,   157,    62,
      63,   182,   234,   297,   423,   157,   157,    18,   232,   157,
     157,   182,   190,   182,   190,   168,   190,   165,   231,   157,
     157,   157,   232,   157,   234,   223,   224,   224,    14,   284,
     260,   271,   164,   182,   185,   236,    79,   182,   190,    92,
      93,   264,   268,   113,   136,   263,   112,   135,   267,   263,
     382,   317,   296,   162,   162,   185,   414,   190,   424,   185,
     182,   185,   182,   185,   159,   384,   398,   398,   184,   185,
     185,   185,   223,   157,   427,   477,   471,   316,     5,   168,
     185,   223,   368,   494,   165,   370,    10,   371,   156,   181,
     372,   494,   156,   184,   199,   313,   188,    79,   194,   195,
     383,   207,   207,   207,   207,   207,   165,   387,   158,   223,
     163,   156,   203,   161,   201,   203,   203,   162,   163,   126,
     160,   162,   229,   221,   182,   162,   494,   157,   427,   468,
     471,   159,   159,   185,   185,   159,   157,   427,   468,   471,
     157,   427,   477,   433,   427,   427,   159,   159,   162,   380,
     162,   138,   377,   138,   159,   159,   185,   222,   222,   162,
     162,   185,   185,   159,   373,   373,   373,   159,   159,   159,
     385,   163,   223,   223,   324,   340,   162,   156,   185,   414,
     156,   156,   156,   156,   314,   314,   353,   361,   488,   314,
     360,   157,   349,   182,   182,   157,   164,   205,   356,   357,
     363,   433,   434,   447,   451,   163,   182,   190,   190,   197,
     182,   234,   182,   234,   230,   240,   297,   299,   302,   308,
     317,   321,   230,    81,   159,   240,   150,   151,   152,   153,
     158,   159,   182,   230,   249,   250,   252,   297,   182,   182,
     230,   182,   387,   182,   230,   401,   230,   249,   114,   115,
     116,   117,   118,   276,   278,   279,   182,   101,   182,    85,
     157,   159,   427,   156,   182,   182,   157,   157,   232,   232,
     260,   157,   270,   260,   270,   234,   182,   159,   156,   396,
     156,   184,   163,   163,   162,   162,   162,   185,   373,   223,
     223,   185,   162,   185,   165,   156,   370,   494,   340,   190,
     165,   222,   156,   406,   472,   473,   159,   164,   159,   163,
     164,   387,   494,   229,   124,   200,   201,   161,   201,   161,
     201,   162,   156,   373,   159,   159,   373,   373,   162,   185,
     159,   427,   159,   159,   159,   230,   470,   156,   156,   349,
     349,   349,   356,   157,   205,   358,   359,   468,   479,   480,
     481,   482,   182,   163,   182,   356,   182,   401,   428,   433,
     223,   317,   156,   163,   182,   362,   363,   362,   362,   190,
     159,   159,   230,   317,   159,   157,   232,   159,   150,   151,
     152,   153,   173,   182,   253,   254,   232,   231,   182,   254,
     159,   164,   230,   158,   230,   231,   252,   182,   494,   159,
     159,   159,   159,   234,   278,   279,   157,   223,   157,   191,
       1,   232,   207,   261,   230,    76,   111,   262,   264,    76,
     427,   392,   407,   184,   184,   162,   159,   185,   185,   162,
     162,   370,   494,   156,   372,   387,   185,   159,   223,   195,
     223,   494,   156,   162,   162,   200,   200,   159,   427,   427,
     159,   159,   162,   162,   223,   182,   480,   481,   482,   317,
     479,   163,   182,   427,   427,   182,   159,   433,   427,   232,
     232,    78,    79,   165,   243,   244,   245,   159,   230,    76,
     232,   230,   158,   230,    76,   182,   158,   230,   231,   252,
     317,   339,   158,   230,   232,   250,   254,   254,   182,   230,
     156,   165,   245,   232,   232,   157,   184,   182,   191,   159,
     164,   159,   163,   164,   159,   232,   157,   232,   232,   232,
     398,   190,   424,   162,   162,   494,   156,   494,   156,   156,
     162,   162,   159,   159,   159,   479,   427,   357,    76,     1,
     222,   241,   242,   425,     1,   164,     1,   184,   232,   243,
      76,   182,   159,   232,    76,   182,   173,   173,   232,   231,
     254,   254,   182,    58,   230,   251,   340,   173,   173,    76,
     158,   230,   158,   230,   231,   182,     1,   184,   184,   280,
     315,   317,   488,   164,   182,   161,   191,   285,   286,   287,
     207,   197,   230,   263,   156,   156,   157,   427,   468,   471,
     359,   232,   138,     1,   163,   164,   156,   290,   291,   297,
     232,    76,   182,   232,   230,   158,   158,   230,   158,   230,
     158,   230,   231,   188,   340,   158,   230,   158,   230,   232,
     173,   173,   173,   173,   156,   290,   280,   185,   157,   205,
     424,   479,   188,   164,   107,   157,   159,   164,   163,   159,
     159,    76,   259,   373,   222,   241,   244,   246,   247,   297,
     232,   173,   173,   173,   173,   158,   158,   230,   158,   230,
     158,   230,   246,   185,   182,   277,   317,   285,   162,   222,
     182,   285,   287,   232,    76,   159,   232,   237,   185,   244,
     158,   158,   230,   158,   230,   158,   230,   185,   277,   221,
     159,   164,   191,   159,   159,   164,   232,     1,   232,   156,
     237,   156,   159,   234,   191,   288,   157,   182,   288,   234,
     163,   164,   222,   159,   191,   188,   289,   159,   182,   159,
     163,   182,   188
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
     420,   415,   421,   421,   421,   422,   422,   423,   423,   423,
     423,   423,   423,   423,   423,   423,   423,   424,   424,   424,
     424,   425,   426,   426,   427,   427,   428,   428,   429,   429,
     429,   430,   430,   431,   431,   431,   432,   432,   432,   433,
     433,   434,   434,   434,   434,   435,   435,   435,   435,   436,
     436,   436,   436,   436,   436,   436,   437,   437,   437,   437,
     438,   438,   438,   439,   439,   439,   439,   439,   440,   440,
     440,   440,   441,   441,   441,   441,   441,   441,   442,   442,
     442,   443,   443,   443,   443,   443,   444,   444,   444,   444,
     445,   445,   445,   445,   445,   445,   446,   446,   447,   447,
     447,   447,   448,   448,   448,   448,   449,   449,   449,   449,
     449,   449,   449,   450,   450,   450,   450,   451,   451,   451,
     452,   452,   452,   452,   452,   453,   453,   453,   453,   454,
     454,   454,   454,   454,   454,   455,   455,   455,   455,   455,
     456,   456,   456,   457,   457,   457,   457,   458,   458,   458,
     459,   459,   459,   459,   459,   460,   460,   461,   461,   461,
     462,   462,   463,   463,   464,   464,   464,   465,   465,   465,
     465,   465,   466,   466,   466,   466,   467,   467,   467,   468,
     468,   468,   468,   468,   469,   469,   469,   469,   469,   469,
     470,   470,   471,   471,   471,   471,   472,   472,   473,   473,
     473,   473,   474,   474,   474,   474,   474,   475,   475,   475,
     475,   476,   476,   476,   477,   477,   477,   478,   478,   478,
     478,   478,   478,   479,   479,   479,   480,   480,   480,   480,
     480,   481,   481,   481,   481,   482,   482,   483,   483,   483,
     484,   484,   485,   485,   485,   485,   485,   485,   486,   486,
     486,   486,   486,   486,   486,   486,   486,   486,   487,   487,
     487,   487,   488,   488,   488,   489,   489,   490,   490,   490,
     490,   490,   490,   491,   491,   491,   491,   491,   491,   492,
     492,   492,   493,   493,   493,   494,   494,   495,   495
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
       2,     4,     4,     4,     6,     4,     4,     2,     1,     1,
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
       1,     3,     2,     2,     1,     7,     5,     1,     1,     1,
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
       4,     4,     4,     3,     2,     2,     3,     3,     2,     1,
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
       0,     8,     1,     2,     3,     0,     5,     3,     4,     4,
       4,     4,     5,     5,     5,     5,     6,     1,     1,     1,
       1,     3,     0,     5,     0,     1,     1,     2,     6,     4,
       4,     1,     3,     0,     1,     4,     1,     1,     1,     1,
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
#line 644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 8525 "Parser/parser.cc"
    break;

  case 3:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8531 "Parser/parser.cc"
    break;

  case 4:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8537 "Parser/parser.cc"
    break;

  case 5:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8543 "Parser/parser.cc"
    break;

  case 6:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8549 "Parser/parser.cc"
    break;

  case 7:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8555 "Parser/parser.cc"
    break;

  case 8:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8561 "Parser/parser.cc"
    break;

  case 20:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8567 "Parser/parser.cc"
    break;

  case 24:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8573 "Parser/parser.cc"
    break;

  case 25:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8579 "Parser/parser.cc"
    break;

  case 26:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8589 "Parser/parser.cc"
    break;

  case 27:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8595 "Parser/parser.cc"
    break;

  case 28:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8601 "Parser/parser.cc"
    break;

  case 29:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8607 "Parser/parser.cc"
    break;

  case 31:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8613 "Parser/parser.cc"
    break;

  case 32:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8619 "Parser/parser.cc"
    break;

  case 33:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8625 "Parser/parser.cc"
    break;

  case 34:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8631 "Parser/parser.cc"
    break;

  case 35:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8641 "Parser/parser.cc"
    break;

  case 36:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8647 "Parser/parser.cc"
    break;

  case 37:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8653 "Parser/parser.cc"
    break;

  case 38:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8659 "Parser/parser.cc"
    break;

  case 39:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8665 "Parser/parser.cc"
    break;

  case 40:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8671 "Parser/parser.cc"
    break;

  case 41:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8677 "Parser/parser.cc"
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
#line 8689 "Parser/parser.cc"
    break;

  case 44:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8698 "Parser/parser.cc"
    break;

  case 45:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8704 "Parser/parser.cc"
    break;

  case 47:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8710 "Parser/parser.cc"
    break;

  case 48:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8716 "Parser/parser.cc"
    break;

  case 49:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8722 "Parser/parser.cc"
    break;

  case 50:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8728 "Parser/parser.cc"
    break;

  case 51:
#line 786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8738 "Parser/parser.cc"
    break;

  case 52:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8744 "Parser/parser.cc"
    break;

  case 53:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8751 "Parser/parser.cc"
    break;

  case 54:
#line 798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8757 "Parser/parser.cc"
    break;

  case 55:
#line 800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8763 "Parser/parser.cc"
    break;

  case 56:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8769 "Parser/parser.cc"
    break;

  case 57:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8775 "Parser/parser.cc"
    break;

  case 58:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8781 "Parser/parser.cc"
    break;

  case 59:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8787 "Parser/parser.cc"
    break;

  case 60:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8793 "Parser/parser.cc"
    break;

  case 61:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8799 "Parser/parser.cc"
    break;

  case 62:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8805 "Parser/parser.cc"
    break;

  case 63:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8811 "Parser/parser.cc"
    break;

  case 64:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8817 "Parser/parser.cc"
    break;

  case 65:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8823 "Parser/parser.cc"
    break;

  case 66:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8829 "Parser/parser.cc"
    break;

  case 67:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8835 "Parser/parser.cc"
    break;

  case 68:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8841 "Parser/parser.cc"
    break;

  case 69:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8851 "Parser/parser.cc"
    break;

  case 70:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8857 "Parser/parser.cc"
    break;

  case 73:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8863 "Parser/parser.cc"
    break;

  case 74:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8869 "Parser/parser.cc"
    break;

  case 77:
#line 875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8875 "Parser/parser.cc"
    break;

  case 79:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8881 "Parser/parser.cc"
    break;

  case 80:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8887 "Parser/parser.cc"
    break;

  case 81:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8893 "Parser/parser.cc"
    break;

  case 82:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8899 "Parser/parser.cc"
    break;

  case 83:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8905 "Parser/parser.cc"
    break;

  case 84:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8911 "Parser/parser.cc"
    break;

  case 85:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8917 "Parser/parser.cc"
    break;

  case 86:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8923 "Parser/parser.cc"
    break;

  case 87:
#line 900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8931 "Parser/parser.cc"
    break;

  case 88:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8937 "Parser/parser.cc"
    break;

  case 89:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8946 "Parser/parser.cc"
    break;

  case 92:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8952 "Parser/parser.cc"
    break;

  case 93:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8958 "Parser/parser.cc"
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
#line 8978 "Parser/parser.cc"
    break;

  case 95:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8984 "Parser/parser.cc"
    break;

  case 96:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8990 "Parser/parser.cc"
    break;

  case 97:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8996 "Parser/parser.cc"
    break;

  case 98:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9002 "Parser/parser.cc"
    break;

  case 99:
#line 952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9008 "Parser/parser.cc"
    break;

  case 100:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9014 "Parser/parser.cc"
    break;

  case 101:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9020 "Parser/parser.cc"
    break;

  case 102:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9026 "Parser/parser.cc"
    break;

  case 103:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9032 "Parser/parser.cc"
    break;

  case 104:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 9038 "Parser/parser.cc"
    break;

  case 105:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9047 "Parser/parser.cc"
    break;

  case 106:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9053 "Parser/parser.cc"
    break;

  case 107:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "countof for expressions is currently unimplemented. "); (yyval.expr) = nullptr; }
#line 9059 "Parser/parser.cc"
    break;

  case 108:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9065 "Parser/parser.cc"
    break;

  case 109:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9071 "Parser/parser.cc"
    break;

  case 110:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9077 "Parser/parser.cc"
    break;

  case 111:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9083 "Parser/parser.cc"
    break;

  case 112:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9089 "Parser/parser.cc"
    break;

  case 113:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9095 "Parser/parser.cc"
    break;

  case 114:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9101 "Parser/parser.cc"
    break;

  case 116:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9107 "Parser/parser.cc"
    break;

  case 117:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9113 "Parser/parser.cc"
    break;

  case 118:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9119 "Parser/parser.cc"
    break;

  case 119:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9125 "Parser/parser.cc"
    break;

  case 120:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9131 "Parser/parser.cc"
    break;

  case 121:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9137 "Parser/parser.cc"
    break;

  case 122:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9143 "Parser/parser.cc"
    break;

  case 123:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9149 "Parser/parser.cc"
    break;

  case 131:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9155 "Parser/parser.cc"
    break;

  case 133:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9161 "Parser/parser.cc"
    break;

  case 134:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9167 "Parser/parser.cc"
    break;

  case 135:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9173 "Parser/parser.cc"
    break;

  case 137:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9179 "Parser/parser.cc"
    break;

  case 138:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9185 "Parser/parser.cc"
    break;

  case 140:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9191 "Parser/parser.cc"
    break;

  case 141:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9197 "Parser/parser.cc"
    break;

  case 143:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9203 "Parser/parser.cc"
    break;

  case 144:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9209 "Parser/parser.cc"
    break;

  case 145:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9215 "Parser/parser.cc"
    break;

  case 146:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9221 "Parser/parser.cc"
    break;

  case 148:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9227 "Parser/parser.cc"
    break;

  case 149:
#line 1075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9233 "Parser/parser.cc"
    break;

  case 151:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9239 "Parser/parser.cc"
    break;

  case 153:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9245 "Parser/parser.cc"
    break;

  case 155:
#line 1093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9251 "Parser/parser.cc"
    break;

  case 157:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9257 "Parser/parser.cc"
    break;

  case 159:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9263 "Parser/parser.cc"
    break;

  case 161:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9269 "Parser/parser.cc"
    break;

  case 162:
#line 1113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9275 "Parser/parser.cc"
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
#line 9287 "Parser/parser.cc"
    break;

  case 166:
#line 1132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9293 "Parser/parser.cc"
    break;

  case 167:
#line 1137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9299 "Parser/parser.cc"
    break;

  case 171:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9305 "Parser/parser.cc"
    break;

  case 172:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9311 "Parser/parser.cc"
    break;

  case 173:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9317 "Parser/parser.cc"
    break;

  case 174:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9323 "Parser/parser.cc"
    break;

  case 175:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9329 "Parser/parser.cc"
    break;

  case 176:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9335 "Parser/parser.cc"
    break;

  case 177:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9341 "Parser/parser.cc"
    break;

  case 178:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9347 "Parser/parser.cc"
    break;

  case 179:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9353 "Parser/parser.cc"
    break;

  case 180:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9359 "Parser/parser.cc"
    break;

  case 181:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9365 "Parser/parser.cc"
    break;

  case 182:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9371 "Parser/parser.cc"
    break;

  case 183:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9377 "Parser/parser.cc"
    break;

  case 184:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9383 "Parser/parser.cc"
    break;

  case 185:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9389 "Parser/parser.cc"
    break;

  case 187:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9395 "Parser/parser.cc"
    break;

  case 188:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9401 "Parser/parser.cc"
    break;

  case 189:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9407 "Parser/parser.cc"
    break;

  case 191:
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9413 "Parser/parser.cc"
    break;

  case 192:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9419 "Parser/parser.cc"
    break;

  case 207:
#line 1217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9425 "Parser/parser.cc"
    break;

  case 209:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9431 "Parser/parser.cc"
    break;

  case 210:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9437 "Parser/parser.cc"
    break;

  case 211:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9448 "Parser/parser.cc"
    break;

  case 212:
#line 1238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9454 "Parser/parser.cc"
    break;

  case 213:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9460 "Parser/parser.cc"
    break;

  case 215:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9466 "Parser/parser.cc"
    break;

  case 216:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9472 "Parser/parser.cc"
    break;

  case 217:
#line 1256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9478 "Parser/parser.cc"
    break;

  case 218:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9484 "Parser/parser.cc"
    break;

  case 219:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9490 "Parser/parser.cc"
    break;

  case 222:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9496 "Parser/parser.cc"
    break;

  case 223:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9503 "Parser/parser.cc"
    break;

  case 224:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9509 "Parser/parser.cc"
    break;

  case 225:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9515 "Parser/parser.cc"
    break;

  case 226:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9521 "Parser/parser.cc"
    break;

  case 227:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9527 "Parser/parser.cc"
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
#line 9541 "Parser/parser.cc"
    break;

  case 229:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9547 "Parser/parser.cc"
    break;

  case 230:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9553 "Parser/parser.cc"
    break;

  case 231:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9562 "Parser/parser.cc"
    break;

  case 232:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9568 "Parser/parser.cc"
    break;

  case 233:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9574 "Parser/parser.cc"
    break;

  case 234:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9580 "Parser/parser.cc"
    break;

  case 235:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9586 "Parser/parser.cc"
    break;

  case 236:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9592 "Parser/parser.cc"
    break;

  case 237:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9598 "Parser/parser.cc"
    break;

  case 238:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9604 "Parser/parser.cc"
    break;

  case 240:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9610 "Parser/parser.cc"
    break;

  case 241:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9616 "Parser/parser.cc"
    break;

  case 242:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9622 "Parser/parser.cc"
    break;

  case 243:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9628 "Parser/parser.cc"
    break;

  case 244:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9634 "Parser/parser.cc"
    break;

  case 245:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9640 "Parser/parser.cc"
    break;

  case 246:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9646 "Parser/parser.cc"
    break;

  case 248:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9652 "Parser/parser.cc"
    break;

  case 249:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9658 "Parser/parser.cc"
    break;

  case 250:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9664 "Parser/parser.cc"
    break;

  case 252:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9670 "Parser/parser.cc"
    break;

  case 253:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9676 "Parser/parser.cc"
    break;

  case 254:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9682 "Parser/parser.cc"
    break;

  case 255:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9691 "Parser/parser.cc"
    break;

  case 256:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9697 "Parser/parser.cc"
    break;

  case 257:
#line 1405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9703 "Parser/parser.cc"
    break;

  case 258:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9709 "Parser/parser.cc"
    break;

  case 259:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9718 "Parser/parser.cc"
    break;

  case 260:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9724 "Parser/parser.cc"
    break;

  case 261:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9730 "Parser/parser.cc"
    break;

  case 262:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9736 "Parser/parser.cc"
    break;

  case 263:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9745 "Parser/parser.cc"
    break;

  case 264:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9751 "Parser/parser.cc"
    break;

  case 265:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9757 "Parser/parser.cc"
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
#line 9776 "Parser/parser.cc"
    break;

  case 268:
#line 1455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9782 "Parser/parser.cc"
    break;

  case 269:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9790 "Parser/parser.cc"
    break;

  case 270:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9796 "Parser/parser.cc"
    break;

  case 271:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9802 "Parser/parser.cc"
    break;

  case 272:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9808 "Parser/parser.cc"
    break;

  case 273:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9814 "Parser/parser.cc"
    break;

  case 274:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9820 "Parser/parser.cc"
    break;

  case 275:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9826 "Parser/parser.cc"
    break;

  case 276:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9835 "Parser/parser.cc"
    break;

  case 277:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9844 "Parser/parser.cc"
    break;

  case 278:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9850 "Parser/parser.cc"
    break;

  case 279:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9859 "Parser/parser.cc"
    break;

  case 280:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9868 "Parser/parser.cc"
    break;

  case 281:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9874 "Parser/parser.cc"
    break;

  case 282:
#line 1500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9880 "Parser/parser.cc"
    break;

  case 283:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9886 "Parser/parser.cc"
    break;

  case 284:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9892 "Parser/parser.cc"
    break;

  case 285:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9898 "Parser/parser.cc"
    break;

  case 286:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9904 "Parser/parser.cc"
    break;

  case 287:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9910 "Parser/parser.cc"
    break;

  case 288:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9916 "Parser/parser.cc"
    break;

  case 289:
#line 1518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9925 "Parser/parser.cc"
    break;

  case 290:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9935 "Parser/parser.cc"
    break;

  case 291:
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9941 "Parser/parser.cc"
    break;

  case 292:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9947 "Parser/parser.cc"
    break;

  case 293:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9956 "Parser/parser.cc"
    break;

  case 294:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9966 "Parser/parser.cc"
    break;

  case 295:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9972 "Parser/parser.cc"
    break;

  case 296:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9981 "Parser/parser.cc"
    break;

  case 297:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9991 "Parser/parser.cc"
    break;

  case 298:
#line 1558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9997 "Parser/parser.cc"
    break;

  case 299:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 10003 "Parser/parser.cc"
    break;

  case 300:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10009 "Parser/parser.cc"
    break;

  case 301:
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10015 "Parser/parser.cc"
    break;

  case 302:
#line 1568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10024 "Parser/parser.cc"
    break;

  case 303:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10034 "Parser/parser.cc"
    break;

  case 304:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10040 "Parser/parser.cc"
    break;

  case 305:
#line 1582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10049 "Parser/parser.cc"
    break;

  case 306:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10059 "Parser/parser.cc"
    break;

  case 307:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10065 "Parser/parser.cc"
    break;

  case 308:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10074 "Parser/parser.cc"
    break;

  case 309:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10084 "Parser/parser.cc"
    break;

  case 310:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10090 "Parser/parser.cc"
    break;

  case 311:
#line 1609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 10098 "Parser/parser.cc"
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
#line 10110 "Parser/parser.cc"
    break;

  case 313:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10117 "Parser/parser.cc"
    break;

  case 314:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false ); }
#line 10124 "Parser/parser.cc"
    break;

  case 315:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10131 "Parser/parser.cc"
    break;

  case 316:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10137 "Parser/parser.cc"
    break;

  case 317:
#line 1641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10143 "Parser/parser.cc"
    break;

  case 318:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10149 "Parser/parser.cc"
    break;

  case 319:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10155 "Parser/parser.cc"
    break;

  case 320:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10161 "Parser/parser.cc"
    break;

  case 321:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10167 "Parser/parser.cc"
    break;

  case 322:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10173 "Parser/parser.cc"
    break;

  case 324:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10179 "Parser/parser.cc"
    break;

  case 325:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10185 "Parser/parser.cc"
    break;

  case 326:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10191 "Parser/parser.cc"
    break;

  case 327:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10197 "Parser/parser.cc"
    break;

  case 328:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10203 "Parser/parser.cc"
    break;

  case 329:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10209 "Parser/parser.cc"
    break;

  case 330:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10215 "Parser/parser.cc"
    break;

  case 331:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10221 "Parser/parser.cc"
    break;

  case 332:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10227 "Parser/parser.cc"
    break;

  case 333:
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10233 "Parser/parser.cc"
    break;

  case 334:
#line 1692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10239 "Parser/parser.cc"
    break;

  case 335:
#line 1694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10245 "Parser/parser.cc"
    break;

  case 336:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10251 "Parser/parser.cc"
    break;

  case 337:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10257 "Parser/parser.cc"
    break;

  case 338:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10263 "Parser/parser.cc"
    break;

  case 339:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10269 "Parser/parser.cc"
    break;

  case 340:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10275 "Parser/parser.cc"
    break;

  case 341:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10281 "Parser/parser.cc"
    break;

  case 342:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10287 "Parser/parser.cc"
    break;

  case 343:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10293 "Parser/parser.cc"
    break;

  case 344:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10299 "Parser/parser.cc"
    break;

  case 345:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10305 "Parser/parser.cc"
    break;

  case 348:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10311 "Parser/parser.cc"
    break;

  case 349:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10320 "Parser/parser.cc"
    break;

  case 350:
#line 1737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10326 "Parser/parser.cc"
    break;

  case 351:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10332 "Parser/parser.cc"
    break;

  case 354:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10338 "Parser/parser.cc"
    break;

  case 355:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10344 "Parser/parser.cc"
    break;

  case 358:
#line 1762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10350 "Parser/parser.cc"
    break;

  case 359:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10356 "Parser/parser.cc"
    break;

  case 360:
#line 1770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10362 "Parser/parser.cc"
    break;

  case 361:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10368 "Parser/parser.cc"
    break;

  case 362:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10374 "Parser/parser.cc"
    break;

  case 363:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10380 "Parser/parser.cc"
    break;

  case 364:
#line 1779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10386 "Parser/parser.cc"
    break;

  case 365:
#line 1781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10392 "Parser/parser.cc"
    break;

  case 366:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10398 "Parser/parser.cc"
    break;

  case 369:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10404 "Parser/parser.cc"
    break;

  case 370:
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10410 "Parser/parser.cc"
    break;

  case 371:
#line 1803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10416 "Parser/parser.cc"
    break;

  case 372:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10422 "Parser/parser.cc"
    break;

  case 373:
#line 1810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10428 "Parser/parser.cc"
    break;

  case 374:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10434 "Parser/parser.cc"
    break;

  case 375:
#line 1817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10440 "Parser/parser.cc"
    break;

  case 376:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10446 "Parser/parser.cc"
    break;

  case 377:
#line 1824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10452 "Parser/parser.cc"
    break;

  case 378:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10458 "Parser/parser.cc"
    break;

  case 379:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10464 "Parser/parser.cc"
    break;

  case 380:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10470 "Parser/parser.cc"
    break;

  case 381:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10476 "Parser/parser.cc"
    break;

  case 382:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10482 "Parser/parser.cc"
    break;

  case 383:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10488 "Parser/parser.cc"
    break;

  case 384:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10494 "Parser/parser.cc"
    break;

  case 385:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10500 "Parser/parser.cc"
    break;

  case 386:
#line 1856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10506 "Parser/parser.cc"
    break;

  case 387:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10512 "Parser/parser.cc"
    break;

  case 388:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10518 "Parser/parser.cc"
    break;

  case 389:
#line 1862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10524 "Parser/parser.cc"
    break;

  case 390:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10530 "Parser/parser.cc"
    break;

  case 391:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10536 "Parser/parser.cc"
    break;

  case 393:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10542 "Parser/parser.cc"
    break;

  case 394:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10548 "Parser/parser.cc"
    break;

  case 395:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10554 "Parser/parser.cc"
    break;

  case 400:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10560 "Parser/parser.cc"
    break;

  case 401:
#line 1895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10566 "Parser/parser.cc"
    break;

  case 402:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10572 "Parser/parser.cc"
    break;

  case 403:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10578 "Parser/parser.cc"
    break;

  case 404:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10584 "Parser/parser.cc"
    break;

  case 405:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10590 "Parser/parser.cc"
    break;

  case 406:
#line 1908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10596 "Parser/parser.cc"
    break;

  case 407:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10602 "Parser/parser.cc"
    break;

  case 410:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10608 "Parser/parser.cc"
    break;

  case 411:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10614 "Parser/parser.cc"
    break;

  case 412:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10623 "Parser/parser.cc"
    break;

  case 413:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10629 "Parser/parser.cc"
    break;

  case 414:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10635 "Parser/parser.cc"
    break;

  case 415:
#line 1939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10641 "Parser/parser.cc"
    break;

  case 416:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10650 "Parser/parser.cc"
    break;

  case 417:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10659 "Parser/parser.cc"
    break;

  case 418:
#line 1959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10665 "Parser/parser.cc"
    break;

  case 421:
#line 1966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10671 "Parser/parser.cc"
    break;

  case 422:
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10677 "Parser/parser.cc"
    break;

  case 424:
#line 1977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10683 "Parser/parser.cc"
    break;

  case 425:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10689 "Parser/parser.cc"
    break;

  case 435:
#line 2005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10695 "Parser/parser.cc"
    break;

  case 436:
#line 2007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10701 "Parser/parser.cc"
    break;

  case 440:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10707 "Parser/parser.cc"
    break;

  case 442:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10713 "Parser/parser.cc"
    break;

  case 443:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10719 "Parser/parser.cc"
    break;

  case 444:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10725 "Parser/parser.cc"
    break;

  case 445:
#line 2044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10731 "Parser/parser.cc"
    break;

  case 446:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10737 "Parser/parser.cc"
    break;

  case 447:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10743 "Parser/parser.cc"
    break;

  case 448:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10749 "Parser/parser.cc"
    break;

  case 449:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10755 "Parser/parser.cc"
    break;

  case 451:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10761 "Parser/parser.cc"
    break;

  case 452:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10767 "Parser/parser.cc"
    break;

  case 453:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10773 "Parser/parser.cc"
    break;

  case 454:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10784 "Parser/parser.cc"
    break;

  case 455:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10790 "Parser/parser.cc"
    break;

  case 456:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10796 "Parser/parser.cc"
    break;

  case 457:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10802 "Parser/parser.cc"
    break;

  case 458:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10808 "Parser/parser.cc"
    break;

  case 459:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10814 "Parser/parser.cc"
    break;

  case 460:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10820 "Parser/parser.cc"
    break;

  case 461:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10829 "Parser/parser.cc"
    break;

  case 462:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10838 "Parser/parser.cc"
    break;

  case 463:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10847 "Parser/parser.cc"
    break;

  case 464:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10858 "Parser/parser.cc"
    break;

  case 465:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10867 "Parser/parser.cc"
    break;

  case 466:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10873 "Parser/parser.cc"
    break;

  case 467:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10879 "Parser/parser.cc"
    break;

  case 468:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10885 "Parser/parser.cc"
    break;

  case 469:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10893 "Parser/parser.cc"
    break;

  case 470:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10901 "Parser/parser.cc"
    break;

  case 471:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10907 "Parser/parser.cc"
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
#line 10922 "Parser/parser.cc"
    break;

  case 475:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10928 "Parser/parser.cc"
    break;

  case 476:
#line 2186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10934 "Parser/parser.cc"
    break;

  case 477:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10940 "Parser/parser.cc"
    break;

  case 478:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10946 "Parser/parser.cc"
    break;

  case 479:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10952 "Parser/parser.cc"
    break;

  case 485:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10962 "Parser/parser.cc"
    break;

  case 498:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10968 "Parser/parser.cc"
    break;

  case 501:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10974 "Parser/parser.cc"
    break;

  case 502:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10980 "Parser/parser.cc"
    break;

  case 504:
#line 2273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10986 "Parser/parser.cc"
    break;

  case 505:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 10992 "Parser/parser.cc"
    break;

  case 506:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 10998 "Parser/parser.cc"
    break;

  case 507:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 11004 "Parser/parser.cc"
    break;

  case 508:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 11010 "Parser/parser.cc"
    break;

  case 509:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11016 "Parser/parser.cc"
    break;

  case 511:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11022 "Parser/parser.cc"
    break;

  case 512:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11028 "Parser/parser.cc"
    break;

  case 514:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11034 "Parser/parser.cc"
    break;

  case 515:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 11040 "Parser/parser.cc"
    break;

  case 516:
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 11046 "Parser/parser.cc"
    break;

  case 517:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 11052 "Parser/parser.cc"
    break;

  case 518:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 11058 "Parser/parser.cc"
    break;

  case 519:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 11064 "Parser/parser.cc"
    break;

  case 520:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 11070 "Parser/parser.cc"
    break;

  case 521:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 11076 "Parser/parser.cc"
    break;

  case 522:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 11082 "Parser/parser.cc"
    break;

  case 523:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11088 "Parser/parser.cc"
    break;

  case 524:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11094 "Parser/parser.cc"
    break;

  case 525:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 11100 "Parser/parser.cc"
    break;

  case 526:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11106 "Parser/parser.cc"
    break;

  case 527:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11112 "Parser/parser.cc"
    break;

  case 528:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11118 "Parser/parser.cc"
    break;

  case 529:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11124 "Parser/parser.cc"
    break;

  case 530:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11130 "Parser/parser.cc"
    break;

  case 531:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11136 "Parser/parser.cc"
    break;

  case 532:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11142 "Parser/parser.cc"
    break;

  case 533:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11148 "Parser/parser.cc"
    break;

  case 534:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11154 "Parser/parser.cc"
    break;

  case 535:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11160 "Parser/parser.cc"
    break;

  case 536:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11166 "Parser/parser.cc"
    break;

  case 537:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11172 "Parser/parser.cc"
    break;

  case 538:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11178 "Parser/parser.cc"
    break;

  case 539:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11184 "Parser/parser.cc"
    break;

  case 540:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11190 "Parser/parser.cc"
    break;

  case 541:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11196 "Parser/parser.cc"
    break;

  case 542:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11202 "Parser/parser.cc"
    break;

  case 543:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11208 "Parser/parser.cc"
    break;

  case 544:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11214 "Parser/parser.cc"
    break;

  case 545:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11220 "Parser/parser.cc"
    break;

  case 546:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11226 "Parser/parser.cc"
    break;

  case 547:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11232 "Parser/parser.cc"
    break;

  case 548:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11238 "Parser/parser.cc"
    break;

  case 549:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11244 "Parser/parser.cc"
    break;

  case 550:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11250 "Parser/parser.cc"
    break;

  case 551:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11256 "Parser/parser.cc"
    break;

  case 553:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11262 "Parser/parser.cc"
    break;

  case 555:
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11268 "Parser/parser.cc"
    break;

  case 556:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11274 "Parser/parser.cc"
    break;

  case 557:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11280 "Parser/parser.cc"
    break;

  case 559:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11286 "Parser/parser.cc"
    break;

  case 560:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11292 "Parser/parser.cc"
    break;

  case 561:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11298 "Parser/parser.cc"
    break;

  case 562:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11304 "Parser/parser.cc"
    break;

  case 564:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11310 "Parser/parser.cc"
    break;

  case 566:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11316 "Parser/parser.cc"
    break;

  case 567:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11322 "Parser/parser.cc"
    break;

  case 568:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11328 "Parser/parser.cc"
    break;

  case 569:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11334 "Parser/parser.cc"
    break;

  case 570:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11340 "Parser/parser.cc"
    break;

  case 571:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11346 "Parser/parser.cc"
    break;

  case 572:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11352 "Parser/parser.cc"
    break;

  case 573:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11358 "Parser/parser.cc"
    break;

  case 574:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11364 "Parser/parser.cc"
    break;

  case 576:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11370 "Parser/parser.cc"
    break;

  case 577:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11376 "Parser/parser.cc"
    break;

  case 578:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11382 "Parser/parser.cc"
    break;

  case 580:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11388 "Parser/parser.cc"
    break;

  case 581:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11394 "Parser/parser.cc"
    break;

  case 582:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11403 "Parser/parser.cc"
    break;

  case 584:
#line 2488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11409 "Parser/parser.cc"
    break;

  case 585:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11415 "Parser/parser.cc"
    break;

  case 586:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11421 "Parser/parser.cc"
    break;

  case 588:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11427 "Parser/parser.cc"
    break;

  case 589:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11433 "Parser/parser.cc"
    break;

  case 591:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11439 "Parser/parser.cc"
    break;

  case 592:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11445 "Parser/parser.cc"
    break;

  case 593:
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11451 "Parser/parser.cc"
    break;

  case 594:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11457 "Parser/parser.cc"
    break;

  case 595:
#line 2517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11463 "Parser/parser.cc"
    break;

  case 596:
#line 2519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11469 "Parser/parser.cc"
    break;

  case 597:
#line 2524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11475 "Parser/parser.cc"
    break;

  case 598:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11481 "Parser/parser.cc"
    break;

  case 599:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11487 "Parser/parser.cc"
    break;

  case 601:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11493 "Parser/parser.cc"
    break;

  case 602:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11499 "Parser/parser.cc"
    break;

  case 603:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11505 "Parser/parser.cc"
    break;

  case 604:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11511 "Parser/parser.cc"
    break;

  case 605:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11517 "Parser/parser.cc"
    break;

  case 610:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11523 "Parser/parser.cc"
    break;

  case 611:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11529 "Parser/parser.cc"
    break;

  case 612:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11538 "Parser/parser.cc"
    break;

  case 613:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11546 "Parser/parser.cc"
    break;

  case 614:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11555 "Parser/parser.cc"
    break;

  case 615:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11564 "Parser/parser.cc"
    break;

  case 616:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11573 "Parser/parser.cc"
    break;

  case 617:
#line 2587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11582 "Parser/parser.cc"
    break;

  case 619:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11588 "Parser/parser.cc"
    break;

  case 620:
#line 2598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11594 "Parser/parser.cc"
    break;

  case 621:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11604 "Parser/parser.cc"
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
#line 11623 "Parser/parser.cc"
    break;

  case 625:
#line 2632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11629 "Parser/parser.cc"
    break;

  case 626:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11635 "Parser/parser.cc"
    break;

  case 627:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11641 "Parser/parser.cc"
    break;

  case 628:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11647 "Parser/parser.cc"
    break;

  case 629:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11653 "Parser/parser.cc"
    break;

  case 630:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11659 "Parser/parser.cc"
    break;

  case 631:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11668 "Parser/parser.cc"
    break;

  case 632:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11674 "Parser/parser.cc"
    break;

  case 633:
#line 2654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11683 "Parser/parser.cc"
    break;

  case 634:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11689 "Parser/parser.cc"
    break;

  case 635:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11698 "Parser/parser.cc"
    break;

  case 636:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11704 "Parser/parser.cc"
    break;

  case 637:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11710 "Parser/parser.cc"
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
#line 11723 "Parser/parser.cc"
    break;

  case 639:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11732 "Parser/parser.cc"
    break;

  case 640:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11738 "Parser/parser.cc"
    break;

  case 641:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11744 "Parser/parser.cc"
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
#line 11757 "Parser/parser.cc"
    break;

  case 643:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11763 "Parser/parser.cc"
    break;

  case 646:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11769 "Parser/parser.cc"
    break;

  case 647:
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11775 "Parser/parser.cc"
    break;

  case 650:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11781 "Parser/parser.cc"
    break;

  case 652:
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11787 "Parser/parser.cc"
    break;

  case 653:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11793 "Parser/parser.cc"
    break;

  case 654:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11799 "Parser/parser.cc"
    break;

  case 655:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11805 "Parser/parser.cc"
    break;

  case 656:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11811 "Parser/parser.cc"
    break;

  case 657:
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11817 "Parser/parser.cc"
    break;

  case 659:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11823 "Parser/parser.cc"
    break;

  case 661:
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11829 "Parser/parser.cc"
    break;

  case 662:
#line 2754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11835 "Parser/parser.cc"
    break;

  case 664:
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11841 "Parser/parser.cc"
    break;

  case 665:
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11847 "Parser/parser.cc"
    break;

  case 667:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11853 "Parser/parser.cc"
    break;

  case 668:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11864 "Parser/parser.cc"
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
#line 11878 "Parser/parser.cc"
    break;

  case 670:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11884 "Parser/parser.cc"
    break;

  case 671:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11890 "Parser/parser.cc"
    break;

  case 672:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11896 "Parser/parser.cc"
    break;

  case 673:
#line 2805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11907 "Parser/parser.cc"
    break;

  case 674:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11913 "Parser/parser.cc"
    break;

  case 675:
#line 2814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11919 "Parser/parser.cc"
    break;

  case 677:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11925 "Parser/parser.cc"
    break;

  case 678:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11931 "Parser/parser.cc"
    break;

  case 679:
#line 2829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11937 "Parser/parser.cc"
    break;

  case 680:
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11943 "Parser/parser.cc"
    break;

  case 681:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11952 "Parser/parser.cc"
    break;

  case 682:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11961 "Parser/parser.cc"
    break;

  case 683:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11967 "Parser/parser.cc"
    break;

  case 684:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 11977 "Parser/parser.cc"
    break;

  case 685:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11983 "Parser/parser.cc"
    break;

  case 686:
#line 2859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 11989 "Parser/parser.cc"
    break;

  case 688:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11995 "Parser/parser.cc"
    break;

  case 689:
#line 2870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12001 "Parser/parser.cc"
    break;

  case 690:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12007 "Parser/parser.cc"
    break;

  case 691:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12013 "Parser/parser.cc"
    break;

  case 692:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12019 "Parser/parser.cc"
    break;

  case 693:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12025 "Parser/parser.cc"
    break;

  case 695:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12031 "Parser/parser.cc"
    break;

  case 698:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12037 "Parser/parser.cc"
    break;

  case 699:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12043 "Parser/parser.cc"
    break;

  case 700:
#line 2900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 12049 "Parser/parser.cc"
    break;

  case 701:
#line 2902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12055 "Parser/parser.cc"
    break;

  case 704:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12061 "Parser/parser.cc"
    break;

  case 705:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12067 "Parser/parser.cc"
    break;

  case 706:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12073 "Parser/parser.cc"
    break;

  case 708:
#line 2918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12079 "Parser/parser.cc"
    break;

  case 709:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12085 "Parser/parser.cc"
    break;

  case 710:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12091 "Parser/parser.cc"
    break;

  case 712:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12097 "Parser/parser.cc"
    break;

  case 713:
#line 2937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12103 "Parser/parser.cc"
    break;

  case 714:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12109 "Parser/parser.cc"
    break;

  case 715:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12115 "Parser/parser.cc"
    break;

  case 716:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12121 "Parser/parser.cc"
    break;

  case 718:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12127 "Parser/parser.cc"
    break;

  case 719:
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12133 "Parser/parser.cc"
    break;

  case 720:
#line 2957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12139 "Parser/parser.cc"
    break;

  case 725:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12145 "Parser/parser.cc"
    break;

  case 727:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12151 "Parser/parser.cc"
    break;

  case 728:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12157 "Parser/parser.cc"
    break;

  case 731:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12163 "Parser/parser.cc"
    break;

  case 734:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12169 "Parser/parser.cc"
    break;

  case 735:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12175 "Parser/parser.cc"
    break;

  case 736:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12181 "Parser/parser.cc"
    break;

  case 737:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12187 "Parser/parser.cc"
    break;

  case 738:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12193 "Parser/parser.cc"
    break;

  case 739:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12199 "Parser/parser.cc"
    break;

  case 740:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12205 "Parser/parser.cc"
    break;

  case 742:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12211 "Parser/parser.cc"
    break;

  case 743:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12217 "Parser/parser.cc"
    break;

  case 744:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12223 "Parser/parser.cc"
    break;

  case 746:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12229 "Parser/parser.cc"
    break;

  case 748:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12235 "Parser/parser.cc"
    break;

  case 749:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12241 "Parser/parser.cc"
    break;

  case 750:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12247 "Parser/parser.cc"
    break;

  case 751:
#line 3046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12253 "Parser/parser.cc"
    break;

  case 752:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12259 "Parser/parser.cc"
    break;

  case 753:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12265 "Parser/parser.cc"
    break;

  case 755:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12271 "Parser/parser.cc"
    break;

  case 756:
#line 3079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12277 "Parser/parser.cc"
    break;

  case 757:
#line 3081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12283 "Parser/parser.cc"
    break;

  case 758:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12294 "Parser/parser.cc"
    break;

  case 759:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12300 "Parser/parser.cc"
    break;

  case 760:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12306 "Parser/parser.cc"
    break;

  case 761:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12312 "Parser/parser.cc"
    break;

  case 762:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12321 "Parser/parser.cc"
    break;

  case 763:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12327 "Parser/parser.cc"
    break;

  case 764:
#line 3107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12337 "Parser/parser.cc"
    break;

  case 765:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12343 "Parser/parser.cc"
    break;

  case 766:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12349 "Parser/parser.cc"
    break;

  case 767:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12355 "Parser/parser.cc"
    break;

  case 768:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12361 "Parser/parser.cc"
    break;

  case 769:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12367 "Parser/parser.cc"
    break;

  case 770:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12373 "Parser/parser.cc"
    break;

  case 771:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12379 "Parser/parser.cc"
    break;

  case 772:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12385 "Parser/parser.cc"
    break;

  case 773:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12391 "Parser/parser.cc"
    break;

  case 776:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12397 "Parser/parser.cc"
    break;

  case 777:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12403 "Parser/parser.cc"
    break;

  case 778:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12409 "Parser/parser.cc"
    break;

  case 779:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12415 "Parser/parser.cc"
    break;

  case 781:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12421 "Parser/parser.cc"
    break;

  case 782:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12427 "Parser/parser.cc"
    break;

  case 783:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12433 "Parser/parser.cc"
    break;

  case 784:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12439 "Parser/parser.cc"
    break;

  case 785:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12445 "Parser/parser.cc"
    break;

  case 786:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12451 "Parser/parser.cc"
    break;

  case 787:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12457 "Parser/parser.cc"
    break;

  case 788:
#line 3187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12466 "Parser/parser.cc"
    break;

  case 789:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12475 "Parser/parser.cc"
    break;

  case 790:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12484 "Parser/parser.cc"
    break;

  case 791:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12490 "Parser/parser.cc"
    break;

  case 792:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12499 "Parser/parser.cc"
    break;

  case 793:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12505 "Parser/parser.cc"
    break;

  case 795:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12511 "Parser/parser.cc"
    break;

  case 800:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12517 "Parser/parser.cc"
    break;

  case 801:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12523 "Parser/parser.cc"
    break;

  case 802:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12529 "Parser/parser.cc"
    break;

  case 804:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 12535 "Parser/parser.cc"
    break;

  case 805:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12541 "Parser/parser.cc"
    break;

  case 806:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12547 "Parser/parser.cc"
    break;

  case 807:
#line 3257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12553 "Parser/parser.cc"
    break;

  case 809:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12559 "Parser/parser.cc"
    break;

  case 810:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12565 "Parser/parser.cc"
    break;

  case 811:
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12571 "Parser/parser.cc"
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
#line 12587 "Parser/parser.cc"
    break;

  case 813:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12593 "Parser/parser.cc"
    break;

  case 814:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12599 "Parser/parser.cc"
    break;

  case 815:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12605 "Parser/parser.cc"
    break;

  case 816:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12611 "Parser/parser.cc"
    break;

  case 817:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12617 "Parser/parser.cc"
    break;

  case 818:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12623 "Parser/parser.cc"
    break;

  case 820:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12632 "Parser/parser.cc"
    break;

  case 821:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12638 "Parser/parser.cc"
    break;

  case 822:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12647 "Parser/parser.cc"
    break;

  case 823:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12657 "Parser/parser.cc"
    break;

  case 824:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12666 "Parser/parser.cc"
    break;

  case 825:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12676 "Parser/parser.cc"
    break;

  case 826:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12687 "Parser/parser.cc"
    break;

  case 827:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12697 "Parser/parser.cc"
    break;

  case 828:
#line 3341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12708 "Parser/parser.cc"
    break;

  case 829:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12718 "Parser/parser.cc"
    break;

  case 830:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12729 "Parser/parser.cc"
    break;

  case 831:
#line 3361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12739 "Parser/parser.cc"
    break;

  case 833:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12745 "Parser/parser.cc"
    break;

  case 834:
#line 3378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12751 "Parser/parser.cc"
    break;

  case 835:
#line 3383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12757 "Parser/parser.cc"
    break;

  case 836:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12769 "Parser/parser.cc"
    break;

  case 837:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12780 "Parser/parser.cc"
    break;

  case 838:
#line 3403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12789 "Parser/parser.cc"
    break;

  case 839:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12798 "Parser/parser.cc"
    break;

  case 840:
#line 3414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12804 "Parser/parser.cc"
    break;

  case 841:
#line 3417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12810 "Parser/parser.cc"
    break;

  case 842:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12816 "Parser/parser.cc"
    break;

  case 843:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12825 "Parser/parser.cc"
    break;

  case 844:
#line 3430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12831 "Parser/parser.cc"
    break;

  case 845:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12837 "Parser/parser.cc"
    break;

  case 846:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12843 "Parser/parser.cc"
    break;

  case 851:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12849 "Parser/parser.cc"
    break;

  case 852:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12855 "Parser/parser.cc"
    break;

  case 853:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12865 "Parser/parser.cc"
    break;

  case 854:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12871 "Parser/parser.cc"
    break;

  case 857:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12877 "Parser/parser.cc"
    break;

  case 858:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12883 "Parser/parser.cc"
    break;

  case 859:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12889 "Parser/parser.cc"
    break;

  case 860:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12895 "Parser/parser.cc"
    break;

  case 862:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12901 "Parser/parser.cc"
    break;

  case 863:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12907 "Parser/parser.cc"
    break;

  case 864:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12913 "Parser/parser.cc"
    break;

  case 865:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12919 "Parser/parser.cc"
    break;

  case 867:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12925 "Parser/parser.cc"
    break;

  case 868:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12931 "Parser/parser.cc"
    break;

  case 869:
#line 3542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12937 "Parser/parser.cc"
    break;

  case 870:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12943 "Parser/parser.cc"
    break;

  case 871:
#line 3549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12949 "Parser/parser.cc"
    break;

  case 873:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12955 "Parser/parser.cc"
    break;

  case 874:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12961 "Parser/parser.cc"
    break;

  case 875:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12967 "Parser/parser.cc"
    break;

  case 876:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12973 "Parser/parser.cc"
    break;

  case 877:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12979 "Parser/parser.cc"
    break;

  case 878:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12985 "Parser/parser.cc"
    break;

  case 879:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12991 "Parser/parser.cc"
    break;

  case 880:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12997 "Parser/parser.cc"
    break;

  case 881:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13003 "Parser/parser.cc"
    break;

  case 882:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13009 "Parser/parser.cc"
    break;

  case 883:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13015 "Parser/parser.cc"
    break;

  case 884:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13021 "Parser/parser.cc"
    break;

  case 885:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13027 "Parser/parser.cc"
    break;

  case 886:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13033 "Parser/parser.cc"
    break;

  case 887:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13039 "Parser/parser.cc"
    break;

  case 888:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13045 "Parser/parser.cc"
    break;

  case 889:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13051 "Parser/parser.cc"
    break;

  case 890:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13057 "Parser/parser.cc"
    break;

  case 892:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13063 "Parser/parser.cc"
    break;

  case 893:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13069 "Parser/parser.cc"
    break;

  case 894:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13075 "Parser/parser.cc"
    break;

  case 895:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13081 "Parser/parser.cc"
    break;

  case 896:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13087 "Parser/parser.cc"
    break;

  case 897:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13093 "Parser/parser.cc"
    break;

  case 898:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13099 "Parser/parser.cc"
    break;

  case 899:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13105 "Parser/parser.cc"
    break;

  case 900:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13111 "Parser/parser.cc"
    break;

  case 901:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13117 "Parser/parser.cc"
    break;

  case 902:
#line 3634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13123 "Parser/parser.cc"
    break;

  case 903:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13129 "Parser/parser.cc"
    break;

  case 904:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13135 "Parser/parser.cc"
    break;

  case 905:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13141 "Parser/parser.cc"
    break;

  case 906:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13147 "Parser/parser.cc"
    break;

  case 907:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13153 "Parser/parser.cc"
    break;

  case 911:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13159 "Parser/parser.cc"
    break;

  case 912:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13165 "Parser/parser.cc"
    break;

  case 913:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13171 "Parser/parser.cc"
    break;

  case 914:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13177 "Parser/parser.cc"
    break;

  case 915:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13183 "Parser/parser.cc"
    break;

  case 916:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13189 "Parser/parser.cc"
    break;

  case 917:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13195 "Parser/parser.cc"
    break;

  case 918:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13201 "Parser/parser.cc"
    break;

  case 919:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13207 "Parser/parser.cc"
    break;

  case 920:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13213 "Parser/parser.cc"
    break;

  case 921:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13219 "Parser/parser.cc"
    break;

  case 922:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13225 "Parser/parser.cc"
    break;

  case 923:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13231 "Parser/parser.cc"
    break;

  case 924:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13237 "Parser/parser.cc"
    break;

  case 925:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13243 "Parser/parser.cc"
    break;

  case 926:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13252 "Parser/parser.cc"
    break;

  case 927:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13258 "Parser/parser.cc"
    break;

  case 928:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13264 "Parser/parser.cc"
    break;

  case 930:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13270 "Parser/parser.cc"
    break;

  case 931:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13276 "Parser/parser.cc"
    break;

  case 932:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13282 "Parser/parser.cc"
    break;

  case 933:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13288 "Parser/parser.cc"
    break;

  case 934:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13294 "Parser/parser.cc"
    break;

  case 935:
#line 3734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13300 "Parser/parser.cc"
    break;

  case 936:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13306 "Parser/parser.cc"
    break;

  case 937:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13312 "Parser/parser.cc"
    break;

  case 938:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13318 "Parser/parser.cc"
    break;

  case 939:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13324 "Parser/parser.cc"
    break;

  case 940:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13330 "Parser/parser.cc"
    break;

  case 941:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13336 "Parser/parser.cc"
    break;

  case 942:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13342 "Parser/parser.cc"
    break;

  case 943:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13348 "Parser/parser.cc"
    break;

  case 944:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13354 "Parser/parser.cc"
    break;

  case 945:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13360 "Parser/parser.cc"
    break;

  case 946:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13366 "Parser/parser.cc"
    break;

  case 947:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13372 "Parser/parser.cc"
    break;

  case 949:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13378 "Parser/parser.cc"
    break;

  case 950:
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13384 "Parser/parser.cc"
    break;

  case 951:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13390 "Parser/parser.cc"
    break;

  case 952:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13396 "Parser/parser.cc"
    break;

  case 953:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13402 "Parser/parser.cc"
    break;

  case 954:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13408 "Parser/parser.cc"
    break;

  case 955:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13414 "Parser/parser.cc"
    break;

  case 956:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13420 "Parser/parser.cc"
    break;

  case 957:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13426 "Parser/parser.cc"
    break;

  case 958:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13432 "Parser/parser.cc"
    break;

  case 959:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13438 "Parser/parser.cc"
    break;

  case 960:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13444 "Parser/parser.cc"
    break;

  case 961:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13450 "Parser/parser.cc"
    break;

  case 962:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13456 "Parser/parser.cc"
    break;

  case 963:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13462 "Parser/parser.cc"
    break;

  case 964:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13468 "Parser/parser.cc"
    break;

  case 965:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13474 "Parser/parser.cc"
    break;

  case 966:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13481 "Parser/parser.cc"
    break;

  case 968:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13487 "Parser/parser.cc"
    break;

  case 969:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13493 "Parser/parser.cc"
    break;

  case 970:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13499 "Parser/parser.cc"
    break;

  case 971:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13505 "Parser/parser.cc"
    break;

  case 972:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13511 "Parser/parser.cc"
    break;

  case 973:
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13517 "Parser/parser.cc"
    break;

  case 974:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13523 "Parser/parser.cc"
    break;

  case 975:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13529 "Parser/parser.cc"
    break;

  case 976:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13535 "Parser/parser.cc"
    break;

  case 977:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13541 "Parser/parser.cc"
    break;

  case 978:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13547 "Parser/parser.cc"
    break;

  case 979:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13553 "Parser/parser.cc"
    break;

  case 980:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13559 "Parser/parser.cc"
    break;

  case 981:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13566 "Parser/parser.cc"
    break;

  case 983:
#line 3880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13572 "Parser/parser.cc"
    break;

  case 984:
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13578 "Parser/parser.cc"
    break;

  case 985:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13584 "Parser/parser.cc"
    break;

  case 986:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13590 "Parser/parser.cc"
    break;

  case 987:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13596 "Parser/parser.cc"
    break;

  case 988:
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13602 "Parser/parser.cc"
    break;

  case 989:
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13608 "Parser/parser.cc"
    break;

  case 990:
#line 3903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13614 "Parser/parser.cc"
    break;

  case 991:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13620 "Parser/parser.cc"
    break;

  case 992:
#line 3910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13626 "Parser/parser.cc"
    break;

  case 993:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13632 "Parser/parser.cc"
    break;

  case 995:
#line 3930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13638 "Parser/parser.cc"
    break;

  case 996:
#line 3932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13644 "Parser/parser.cc"
    break;

  case 997:
#line 3937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13650 "Parser/parser.cc"
    break;

  case 998:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13656 "Parser/parser.cc"
    break;

  case 999:
#line 3941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13662 "Parser/parser.cc"
    break;

  case 1000:
#line 3943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13668 "Parser/parser.cc"
    break;

  case 1001:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13674 "Parser/parser.cc"
    break;

  case 1003:
#line 3951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13680 "Parser/parser.cc"
    break;

  case 1004:
#line 3953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13686 "Parser/parser.cc"
    break;

  case 1005:
#line 3955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13692 "Parser/parser.cc"
    break;

  case 1006:
#line 3960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13698 "Parser/parser.cc"
    break;

  case 1007:
#line 3962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13704 "Parser/parser.cc"
    break;

  case 1008:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13710 "Parser/parser.cc"
    break;

  case 1009:
#line 3970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13716 "Parser/parser.cc"
    break;

  case 1010:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13722 "Parser/parser.cc"
    break;

  case 1011:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13728 "Parser/parser.cc"
    break;

  case 1012:
#line 3982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13734 "Parser/parser.cc"
    break;

  case 1014:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13740 "Parser/parser.cc"
    break;

  case 1015:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13746 "Parser/parser.cc"
    break;

  case 1017:
#line 3998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13752 "Parser/parser.cc"
    break;

  case 1018:
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13758 "Parser/parser.cc"
    break;

  case 1020:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13764 "Parser/parser.cc"
    break;

  case 1021:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13770 "Parser/parser.cc"
    break;

  case 1022:
#line 4013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13776 "Parser/parser.cc"
    break;

  case 1023:
#line 4015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13782 "Parser/parser.cc"
    break;

  case 1024:
#line 4017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13788 "Parser/parser.cc"
    break;

  case 1025:
#line 4019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13794 "Parser/parser.cc"
    break;

  case 1026:
#line 4053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13800 "Parser/parser.cc"
    break;

  case 1029:
#line 4060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13807 "Parser/parser.cc"
    break;

  case 1030:
#line 4063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13813 "Parser/parser.cc"
    break;

  case 1031:
#line 4065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13819 "Parser/parser.cc"
    break;

  case 1032:
#line 4070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13825 "Parser/parser.cc"
    break;

  case 1033:
#line 4072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13831 "Parser/parser.cc"
    break;

  case 1034:
#line 4074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13837 "Parser/parser.cc"
    break;

  case 1035:
#line 4076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13843 "Parser/parser.cc"
    break;

  case 1036:
#line 4078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13849 "Parser/parser.cc"
    break;

  case 1038:
#line 4084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13855 "Parser/parser.cc"
    break;

  case 1039:
#line 4086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13861 "Parser/parser.cc"
    break;

  case 1040:
#line 4088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13867 "Parser/parser.cc"
    break;

  case 1041:
#line 4093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13873 "Parser/parser.cc"
    break;

  case 1042:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13879 "Parser/parser.cc"
    break;

  case 1043:
#line 4097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13885 "Parser/parser.cc"
    break;

  case 1045:
#line 4104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13891 "Parser/parser.cc"
    break;

  case 1047:
#line 4115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13897 "Parser/parser.cc"
    break;

  case 1048:
#line 4118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13903 "Parser/parser.cc"
    break;

  case 1049:
#line 4120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13909 "Parser/parser.cc"
    break;

  case 1050:
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13915 "Parser/parser.cc"
    break;

  case 1051:
#line 4125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13921 "Parser/parser.cc"
    break;

  case 1052:
#line 4127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13927 "Parser/parser.cc"
    break;

  case 1054:
#line 4142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13933 "Parser/parser.cc"
    break;

  case 1055:
#line 4144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13939 "Parser/parser.cc"
    break;

  case 1056:
#line 4149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13945 "Parser/parser.cc"
    break;

  case 1057:
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13951 "Parser/parser.cc"
    break;

  case 1058:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13957 "Parser/parser.cc"
    break;

  case 1059:
#line 4155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13963 "Parser/parser.cc"
    break;

  case 1060:
#line 4157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13969 "Parser/parser.cc"
    break;

  case 1062:
#line 4163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13975 "Parser/parser.cc"
    break;

  case 1063:
#line 4165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13981 "Parser/parser.cc"
    break;

  case 1064:
#line 4167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13987 "Parser/parser.cc"
    break;

  case 1065:
#line 4172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13993 "Parser/parser.cc"
    break;

  case 1066:
#line 4174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13999 "Parser/parser.cc"
    break;

  case 1069:
#line 4184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14005 "Parser/parser.cc"
    break;

  case 1072:
#line 4195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14011 "Parser/parser.cc"
    break;

  case 1073:
#line 4197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14017 "Parser/parser.cc"
    break;

  case 1074:
#line 4199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14023 "Parser/parser.cc"
    break;

  case 1075:
#line 4201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14029 "Parser/parser.cc"
    break;

  case 1076:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14035 "Parser/parser.cc"
    break;

  case 1077:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14041 "Parser/parser.cc"
    break;

  case 1078:
#line 4212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14047 "Parser/parser.cc"
    break;

  case 1079:
#line 4214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14053 "Parser/parser.cc"
    break;

  case 1080:
#line 4216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14059 "Parser/parser.cc"
    break;

  case 1081:
#line 4218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14065 "Parser/parser.cc"
    break;

  case 1082:
#line 4220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14071 "Parser/parser.cc"
    break;

  case 1083:
#line 4223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14077 "Parser/parser.cc"
    break;

  case 1084:
#line 4225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14083 "Parser/parser.cc"
    break;

  case 1085:
#line 4227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14089 "Parser/parser.cc"
    break;

  case 1086:
#line 4229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14095 "Parser/parser.cc"
    break;

  case 1087:
#line 4231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14101 "Parser/parser.cc"
    break;

  case 1088:
#line 4236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14107 "Parser/parser.cc"
    break;

  case 1089:
#line 4238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14113 "Parser/parser.cc"
    break;

  case 1090:
#line 4243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14119 "Parser/parser.cc"
    break;

  case 1091:
#line 4245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14125 "Parser/parser.cc"
    break;

  case 1093:
#line 4272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14131 "Parser/parser.cc"
    break;

  case 1097:
#line 4283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14137 "Parser/parser.cc"
    break;

  case 1098:
#line 4285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14143 "Parser/parser.cc"
    break;

  case 1099:
#line 4287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14149 "Parser/parser.cc"
    break;

  case 1100:
#line 4289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14155 "Parser/parser.cc"
    break;

  case 1101:
#line 4291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14161 "Parser/parser.cc"
    break;

  case 1102:
#line 4293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14167 "Parser/parser.cc"
    break;

  case 1103:
#line 4300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14173 "Parser/parser.cc"
    break;

  case 1104:
#line 4302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14179 "Parser/parser.cc"
    break;

  case 1105:
#line 4304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14185 "Parser/parser.cc"
    break;

  case 1106:
#line 4306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14191 "Parser/parser.cc"
    break;

  case 1107:
#line 4308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14197 "Parser/parser.cc"
    break;

  case 1108:
#line 4310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14203 "Parser/parser.cc"
    break;

  case 1109:
#line 4315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14209 "Parser/parser.cc"
    break;

  case 1110:
#line 4317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14215 "Parser/parser.cc"
    break;

  case 1111:
#line 4319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14221 "Parser/parser.cc"
    break;

  case 1112:
#line 4324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14227 "Parser/parser.cc"
    break;

  case 1113:
#line 4326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14233 "Parser/parser.cc"
    break;

  case 1114:
#line 4328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14239 "Parser/parser.cc"
    break;

  case 1117:
#line 4352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14245 "Parser/parser.cc"
    break;

  case 1118:
#line 4354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14251 "Parser/parser.cc"
    break;


#line 14255 "Parser/parser.cc"

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
#line 4357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
