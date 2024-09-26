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
    FLOAT80 = 291,
    uuFLOAT128 = 292,
    FLOAT16 = 293,
    FLOAT32 = 294,
    FLOAT32X = 295,
    FLOAT64 = 296,
    FLOAT64X = 297,
    FLOAT128 = 298,
    FLOAT128X = 299,
    FLOAT32X4 = 300,
    FLOAT64X2 = 301,
    SVFLOAT32 = 302,
    SVFLOAT64 = 303,
    SVBOOL = 304,
    DECIMAL32 = 305,
    DECIMAL64 = 306,
    DECIMAL128 = 307,
    ZERO_T = 308,
    ONE_T = 309,
    SIZEOF = 310,
    TYPEOF = 311,
    VA_LIST = 312,
    VA_ARG = 313,
    AUTO_TYPE = 314,
    COUNTOF = 315,
    OFFSETOF = 316,
    BASETYPEOF = 317,
    TYPEID = 318,
    ENUM = 319,
    STRUCT = 320,
    UNION = 321,
    EXCEPTION = 322,
    GENERATOR = 323,
    COROUTINE = 324,
    MONITOR = 325,
    THREAD = 326,
    OTYPE = 327,
    FTYPE = 328,
    DTYPE = 329,
    TTYPE = 330,
    TRAIT = 331,
    LABEL = 332,
    SUSPEND = 333,
    ATTRIBUTE = 334,
    EXTENSION = 335,
    IF = 336,
    ELSE = 337,
    SWITCH = 338,
    CASE = 339,
    DEFAULT = 340,
    DO = 341,
    WHILE = 342,
    FOR = 343,
    BREAK = 344,
    CONTINUE = 345,
    GOTO = 346,
    RETURN = 347,
    CHOOSE = 348,
    FALLTHRU = 349,
    FALLTHROUGH = 350,
    WITH = 351,
    WHEN = 352,
    WAITFOR = 353,
    WAITUNTIL = 354,
    CORUN = 355,
    COFOR = 356,
    DISABLE = 357,
    ENABLE = 358,
    TRY = 359,
    THROW = 360,
    THROWRESUME = 361,
    AT = 362,
    ASM = 363,
    ALIGNAS = 364,
    ALIGNOF = 365,
    GENERIC = 366,
    STATICASSERT = 367,
    IDENTIFIER = 368,
    TYPEDIMname = 369,
    TYPEDEFname = 370,
    TYPEGENname = 371,
    TIMEOUT = 372,
    WAND = 373,
    WOR = 374,
    CATCH = 375,
    RECOVER = 376,
    CATCHRESUME = 377,
    FIXUP = 378,
    FINALLY = 379,
    INTEGERconstant = 380,
    CHARACTERconstant = 381,
    STRINGliteral = 382,
    DIRECTIVE = 383,
    FLOATING_DECIMALconstant = 384,
    FLOATING_FRACTIONconstant = 385,
    FLOATINGconstant = 386,
    ARROW = 387,
    ICR = 388,
    DECR = 389,
    LS = 390,
    RS = 391,
    LE = 392,
    GE = 393,
    EQ = 394,
    NE = 395,
    ANDAND = 396,
    OROR = 397,
    ATTR = 398,
    ELLIPSIS = 399,
    EXPassign = 400,
    MULTassign = 401,
    DIVassign = 402,
    MODassign = 403,
    PLUSassign = 404,
    MINUSassign = 405,
    LSassign = 406,
    RSassign = 407,
    ANDassign = 408,
    ERassign = 409,
    ORassign = 410,
    ErangeUp = 411,
    ErangeUpEq = 412,
    ErangeDown = 413,
    ErangeDownEq = 414,
    ATassign = 415,
    THEN = 416
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
#define FLOAT80 291
#define uuFLOAT128 292
#define FLOAT16 293
#define FLOAT32 294
#define FLOAT32X 295
#define FLOAT64 296
#define FLOAT64X 297
#define FLOAT128 298
#define FLOAT128X 299
#define FLOAT32X4 300
#define FLOAT64X2 301
#define SVFLOAT32 302
#define SVFLOAT64 303
#define SVBOOL 304
#define DECIMAL32 305
#define DECIMAL64 306
#define DECIMAL128 307
#define ZERO_T 308
#define ONE_T 309
#define SIZEOF 310
#define TYPEOF 311
#define VA_LIST 312
#define VA_ARG 313
#define AUTO_TYPE 314
#define COUNTOF 315
#define OFFSETOF 316
#define BASETYPEOF 317
#define TYPEID 318
#define ENUM 319
#define STRUCT 320
#define UNION 321
#define EXCEPTION 322
#define GENERATOR 323
#define COROUTINE 324
#define MONITOR 325
#define THREAD 326
#define OTYPE 327
#define FTYPE 328
#define DTYPE 329
#define TTYPE 330
#define TRAIT 331
#define LABEL 332
#define SUSPEND 333
#define ATTRIBUTE 334
#define EXTENSION 335
#define IF 336
#define ELSE 337
#define SWITCH 338
#define CASE 339
#define DEFAULT 340
#define DO 341
#define WHILE 342
#define FOR 343
#define BREAK 344
#define CONTINUE 345
#define GOTO 346
#define RETURN 347
#define CHOOSE 348
#define FALLTHRU 349
#define FALLTHROUGH 350
#define WITH 351
#define WHEN 352
#define WAITFOR 353
#define WAITUNTIL 354
#define CORUN 355
#define COFOR 356
#define DISABLE 357
#define ENABLE 358
#define TRY 359
#define THROW 360
#define THROWRESUME 361
#define AT 362
#define ASM 363
#define ALIGNAS 364
#define ALIGNOF 365
#define GENERIC 366
#define STATICASSERT 367
#define IDENTIFIER 368
#define TYPEDIMname 369
#define TYPEDEFname 370
#define TYPEGENname 371
#define TIMEOUT 372
#define WAND 373
#define WOR 374
#define CATCH 375
#define RECOVER 376
#define CATCHRESUME 377
#define FIXUP 378
#define FINALLY 379
#define INTEGERconstant 380
#define CHARACTERconstant 381
#define STRINGliteral 382
#define DIRECTIVE 383
#define FLOATING_DECIMALconstant 384
#define FLOATING_FRACTIONconstant 385
#define FLOATINGconstant 386
#define ARROW 387
#define ICR 388
#define DECR 389
#define LS 390
#define RS 391
#define LE 392
#define GE 393
#define EQ 394
#define NE 395
#define ANDAND 396
#define OROR 397
#define ATTR 398
#define ELLIPSIS 399
#define EXPassign 400
#define MULTassign 401
#define DIVassign 402
#define MODassign 403
#define PLUSassign 404
#define MINUSassign 405
#define LSassign 406
#define RSassign 407
#define ANDassign 408
#define ERassign 409
#define ORassign 410
#define ErangeUp 411
#define ErangeUpEq 412
#define ErangeDown 413
#define ErangeDownEq 414
#define ATassign 415
#define THEN 416

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
	CondCtrl * ifctrl;
	ForCtrl * forctrl;
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

#line 756 "Parser/parser.cc"

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
#define YYFINAL  156
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   29310

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  189
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1129
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2202

#define YYUNDEFTOK  2
#define YYMAXUTOK   416


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
       2,     2,     2,   178,     2,     2,     2,   182,   175,     2,
     163,   165,   174,   176,   169,   177,   166,   181,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   170,   188,
     183,   187,   184,   186,   164,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   167,   180,   168,   173,     2,   172,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   171,   185,   162,   179,     2,     2,     2,
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
     155,   156,   157,   158,   159,   160,   161
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   645,   645,   649,   656,   657,   658,   659,   660,   664,
     665,   666,   667,   668,   669,   670,   671,   675,   676,   680,
     681,   686,   687,   688,   692,   696,   697,   708,   710,   712,
     714,   715,   717,   719,   721,   723,   733,   735,   737,   739,
     741,   743,   748,   749,   760,   765,   770,   771,   776,   782,
     784,   786,   792,   794,   796,   798,   800,   820,   823,   825,
     827,   829,   831,   833,   835,   837,   839,   841,   843,   845,
     854,   855,   859,   860,   862,   864,   866,   868,   870,   875,
     877,   879,   887,   888,   896,   899,   900,   902,   907,   923,
     925,   927,   929,   931,   933,   935,   940,   942,   945,   947,
     952,   954,   959,   960,   962,   966,   967,   968,   969,   973,
     974,   976,   978,   980,   982,   984,   986,   988,   995,   996,
     997,   998,  1002,  1003,  1007,  1008,  1013,  1014,  1016,  1018,
    1023,  1024,  1026,  1031,  1032,  1034,  1039,  1040,  1042,  1044,
    1046,  1051,  1052,  1054,  1059,  1060,  1065,  1066,  1071,  1072,
    1077,  1078,  1083,  1084,  1089,  1090,  1092,  1097,  1102,  1103,
    1107,  1109,  1114,  1117,  1120,  1125,  1126,  1134,  1140,  1141,
    1145,  1146,  1150,  1151,  1155,  1156,  1157,  1158,  1159,  1160,
    1161,  1162,  1163,  1164,  1165,  1175,  1177,  1182,  1183,  1185,
    1187,  1192,  1193,  1199,  1200,  1206,  1207,  1208,  1209,  1210,
    1211,  1212,  1213,  1214,  1215,  1216,  1217,  1218,  1219,  1221,
    1222,  1228,  1230,  1240,  1242,  1250,  1251,  1256,  1258,  1260,
    1262,  1264,  1268,  1269,  1271,  1277,  1306,  1309,  1311,  1313,
    1323,  1325,  1327,  1332,  1337,  1339,  1341,  1343,  1351,  1352,
    1354,  1358,  1360,  1364,  1366,  1367,  1369,  1371,  1376,  1377,
    1381,  1386,  1387,  1391,  1393,  1398,  1400,  1405,  1407,  1409,
    1411,  1416,  1418,  1420,  1422,  1427,  1429,  1434,  1435,  1457,
    1459,  1463,  1466,  1468,  1471,  1473,  1476,  1478,  1483,  1488,
    1490,  1495,  1500,  1502,  1504,  1506,  1508,  1513,  1515,  1518,
    1520,  1525,  1531,  1534,  1536,  1541,  1547,  1549,  1554,  1560,
    1564,  1566,  1569,  1571,  1576,  1583,  1585,  1590,  1596,  1598,
    1603,  1609,  1612,  1616,  1627,  1632,  1637,  1648,  1650,  1652,
    1654,  1659,  1661,  1663,  1668,  1669,  1671,  1676,  1678,  1683,
    1685,  1687,  1689,  1692,  1696,  1699,  1703,  1705,  1707,  1709,
    1711,  1713,  1715,  1717,  1719,  1721,  1723,  1728,  1729,  1733,
    1739,  1747,  1752,  1753,  1757,  1758,  1763,  1767,  1768,  1771,
    1773,  1778,  1781,  1783,  1785,  1788,  1790,  1795,  1800,  1801,
    1805,  1810,  1812,  1817,  1819,  1824,  1826,  1828,  1833,  1838,
    1843,  1848,  1850,  1852,  1857,  1859,  1865,  1866,  1870,  1871,
    1872,  1873,  1877,  1882,  1883,  1885,  1887,  1889,  1893,  1897,
    1898,  1902,  1904,  1906,  1908,  1910,  1916,  1917,  1923,  1924,
    1928,  1929,  1934,  1936,  1945,  1946,  1948,  1953,  1958,  1969,
    1970,  1974,  1975,  1981,  1982,  1986,  1988,  1992,  1994,  1998,
    1999,  2003,  2004,  2008,  2009,  2010,  2014,  2016,  2031,  2032,
    2033,  2034,  2036,  2040,  2042,  2046,  2053,  2055,  2057,  2065,
    2067,  2072,  2073,  2075,  2077,  2079,  2089,  2091,  2103,  2106,
    2111,  2113,  2119,  2124,  2129,  2140,  2147,  2152,  2154,  2156,
    2162,  2166,  2173,  2175,  2176,  2177,  2193,  2195,  2198,  2200,
    2203,  2208,  2209,  2213,  2214,  2215,  2216,  2225,  2226,  2227,
    2236,  2237,  2238,  2242,  2243,  2244,  2253,  2254,  2255,  2260,
    2261,  2270,  2271,  2276,  2278,  2282,  2284,  2286,  2288,  2295,
    2300,  2305,  2306,  2308,  2318,  2319,  2324,  2326,  2328,  2330,
    2332,  2334,  2337,  2339,  2341,  2346,  2352,  2354,  2356,  2358,
    2360,  2362,  2364,  2366,  2368,  2370,  2372,  2374,  2376,  2378,
    2380,  2382,  2385,  2387,  2389,  2391,  2393,  2395,  2397,  2399,
    2401,  2403,  2405,  2407,  2409,  2411,  2413,  2415,  2417,  2419,
    2424,  2425,  2429,  2435,  2436,  2442,  2443,  2445,  2447,  2449,
    2454,  2456,  2461,  2462,  2464,  2466,  2471,  2473,  2475,  2477,
    2479,  2481,  2486,  2487,  2489,  2491,  2496,  2498,  2497,  2501,
    2509,  2510,  2512,  2514,  2519,  2520,  2522,  2527,  2528,  2530,
    2532,  2537,  2539,  2541,  2546,  2548,  2550,  2552,  2553,  2555,
    2560,  2562,  2564,  2569,  2570,  2574,  2575,  2582,  2581,  2586,
    2585,  2595,  2594,  2605,  2604,  2614,  2619,  2620,  2625,  2631,
    2649,  2650,  2654,  2656,  2658,  2663,  2665,  2667,  2669,  2674,
    2676,  2681,  2683,  2692,  2693,  2698,  2707,  2712,  2714,  2716,
    2725,  2727,  2728,  2729,  2731,  2733,  2734,  2739,  2740,  2741,
    2746,  2748,  2751,  2754,  2761,  2762,  2763,  2769,  2774,  2776,
    2782,  2783,  2789,  2790,  2794,  2802,  2809,  2822,  2821,  2825,
    2828,  2827,  2836,  2840,  2844,  2846,  2852,  2853,  2858,  2863,
    2872,  2873,  2875,  2881,  2883,  2888,  2889,  2895,  2896,  2897,
    2906,  2907,  2909,  2910,  2915,  2916,  2917,  2919,  2925,  2926,
    2928,  2929,  2930,  2932,  2934,  2941,  2942,  2944,  2946,  2951,
    2952,  2961,  2963,  2968,  2970,  2975,  2976,  2978,  2981,  2983,
    2987,  2988,  2989,  2991,  2993,  3001,  3003,  3008,  3009,  3010,
    3015,  3016,  3021,  3022,  3023,  3024,  3028,  3029,  3034,  3035,
    3036,  3037,  3038,  3052,  3053,  3058,  3059,  3065,  3067,  3070,
    3072,  3074,  3097,  3098,  3104,  3105,  3111,  3110,  3120,  3119,
    3123,  3129,  3131,  3141,  3142,  3144,  3148,  3153,  3155,  3157,
    3159,  3165,  3166,  3170,  3171,  3176,  3178,  3185,  3187,  3188,
    3190,  3195,  3197,  3199,  3204,  3206,  3211,  3216,  3224,  3229,
    3231,  3236,  3241,  3242,  3247,  3248,  3252,  3253,  3254,  3260,
    3262,  3264,  3270,  3272,  3277,  3279,  3285,  3286,  3290,  3294,
    3298,  3300,  3312,  3314,  3316,  3318,  3320,  3322,  3324,  3325,
    3330,  3333,  3332,  3344,  3343,  3356,  3355,  3369,  3368,  3382,
    3381,  3394,  3399,  3405,  3407,  3413,  3414,  3425,  3432,  3437,
    3443,  3446,  3449,  3453,  3459,  3462,  3465,  3470,  3471,  3472,
    3473,  3477,  3485,  3486,  3498,  3499,  3503,  3504,  3509,  3511,
    3513,  3518,  3519,  3525,  3526,  3528,  3533,  3534,  3536,  3571,
    3573,  3576,  3581,  3583,  3584,  3586,  3591,  3593,  3595,  3597,
    3602,  3604,  3606,  3608,  3610,  3612,  3614,  3619,  3621,  3623,
    3625,  3634,  3636,  3637,  3642,  3644,  3646,  3648,  3650,  3655,
    3657,  3659,  3661,  3666,  3668,  3670,  3672,  3674,  3676,  3688,
    3689,  3690,  3694,  3696,  3698,  3700,  3702,  3707,  3709,  3711,
    3713,  3718,  3720,  3722,  3724,  3726,  3728,  3740,  3745,  3750,
    3752,  3753,  3755,  3760,  3762,  3764,  3766,  3771,  3773,  3775,
    3777,  3779,  3781,  3783,  3788,  3790,  3792,  3794,  3803,  3805,
    3806,  3811,  3813,  3815,  3817,  3819,  3824,  3826,  3828,  3830,
    3835,  3837,  3839,  3841,  3843,  3845,  3855,  3857,  3860,  3861,
    3863,  3868,  3870,  3872,  3877,  3879,  3881,  3883,  3888,  3890,
    3892,  3906,  3908,  3911,  3912,  3914,  3919,  3921,  3926,  3928,
    3930,  3935,  3937,  3942,  3944,  3961,  3962,  3964,  3969,  3971,
    3973,  3975,  3977,  3982,  3983,  3985,  3987,  3992,  3994,  3996,
    4002,  4004,  4007,  4014,  4016,  4025,  4027,  4029,  4030,  4032,
    4034,  4038,  4040,  4045,  4047,  4049,  4051,  4086,  4087,  4091,
    4092,  4095,  4097,  4102,  4104,  4106,  4108,  4110,  4115,  4116,
    4118,  4120,  4125,  4127,  4129,  4135,  4136,  4138,  4147,  4150,
    4152,  4155,  4157,  4159,  4173,  4174,  4176,  4181,  4183,  4185,
    4187,  4189,  4194,  4195,  4197,  4199,  4204,  4206,  4214,  4215,
    4216,  4221,  4222,  4227,  4229,  4231,  4233,  4235,  4237,  4244,
    4246,  4248,  4250,  4252,  4255,  4257,  4259,  4261,  4263,  4268,
    4270,  4272,  4277,  4303,  4304,  4306,  4310,  4311,  4315,  4317,
    4319,  4321,  4323,  4325,  4332,  4334,  4336,  4338,  4340,  4342,
    4347,  4349,  4351,  4356,  4358,  4360,  4378,  4380,  4385,  4386
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
  "INT128", "UINT128", "FLOAT80", "uuFLOAT128", "FLOAT16", "FLOAT32",
  "FLOAT32X", "FLOAT64", "FLOAT64X", "FLOAT128", "FLOAT128X", "FLOAT32X4",
  "FLOAT64X2", "SVFLOAT32", "SVFLOAT64", "SVBOOL", "DECIMAL32",
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
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   125,    40,    64,    41,    46,    91,    93,    44,
      58,   123,    96,    94,    42,    38,    43,    45,    33,   126,
      92,    47,    37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1862)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1128)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     124, 11466,   158,   262, 21768,    53, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,   141,   982,
     172, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862,   206,   381, -1862, -1862, -1862, -1862,
   -1862, -1862,  5285,  5285,   249, 11466,   330,   367, 26346, -1862,
     385, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862,   391,  4616, -1862,   685,   274, -1862, -1862,  2493, -1862,
   -1862, -1862, -1862, 15494, -1862,   372,   395,   291,   508,   279,
   -1862,  5479,   435,   444,   484,   403,  4257,   673,   942, 11712,
   -1862, -1862,   612, 15329,  1907, -1862, -1862, -1862, -1862,  3576,
     691, 22326, 11638,  1061,  3576,  1087,   545, -1862, -1862, -1862,
   -1862,   236, -1862, -1862, -1862, -1862,   554, -1862, -1862, -1862,
   -1862, -1862,   592,   572,   236, -1862,   236, 19643, -1862, -1862,
   -1862, 23123,  5285, -1862, -1862,  5285, -1862, 11466, -1862,   623,
   23286, -1862, -1862,  5330, 25141, -1862, -1862,   718,   718,   665,
    4292, -1862, -1862, -1862, -1862,   452, 17862,   236,  3480,   236,
   -1862, -1862, -1862, -1862, -1862, -1862,   679, -1862,   681,   710,
    1057, -1862,   756, 28669, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, 20136,  1579,  4629,  4616,   464,   735,   738,   744,   780,
     796,   806, -1862, -1862, 21931, 12982, -1862,   755,   828, -1862,
   15824, -1862, -1862, -1862, -1862,   856, -1862, -1862, -1862,   858,
   -1862, 26651,  1020, 26816, -1862,   882,  5285,   572,   885,   891,
   -1862,  2493,  5330,  2493, -1862, -1862, -1862,  4812,  5890,   904,
     978,   314,   978, -1862,   236,   236,   -36, 19305,   366,   978,
   -1862,   236,   236,   -36,   236, -1862,   236, -1862,  6479, -1862,
   -1862,   933,   943,   718, 26081,   948, 15494, -1862,  5479, -1862,
    3576, -1862,  1378,   545,   953,  1017, 19305,  5285,  5285,   508,
   -1862, 12262, -1862,   718,   718,   989,  1017, 19305,  5285, -1862,
   26590, -1862, -1862, -1862,   718, -1862, -1862, -1862, -1862,   718,
   -1862,   973,  4825,  5285, -1862, 21451,   957, -1862, -1862, -1862,
   25936,   572, 19474,   970,  5330, 21391, 26081,  3576, -1862, -1862,
   25453, -1862,   978,     1, -1862, 28669, 25297,  4948,  6479, -1862,
     418, -1862, -1862, -1862, -1862, -1862, 23286,   978,  5285, -1862,
     997,  1002, -1862, -1862, -1862, -1862,  5285,  4188,   426,   657,
   -1862,  5285,   681, -1862,   961,   236, -1862,  1050, 23449,  1166,
   18378, 26141,  3576, -1862,  3576,   718,  3576,   718, -1862, -1862,
     236, -1862, -1862,  1072, 23612, -1862, -1862, -1862, 23775,   856,
   -1862,  2781,    34,   708, -1862,   599,   545,  1081,  1086, -1862,
    4292,  1083,   681,  4292, -1862, -1862,  1579, -1862,   688, -1862,
    1116, -1862,  1117,  1165, 28746,  1153, 28823,  1167,  1172, 28669,
   28900,  1186, 26468, -1862, -1862, -1862, -1862, -1862, -1862, 28977,
   28977, 19969,  1130,  4496, -1862, -1862, -1862, -1862,   350, -1862,
     538, -1862,  1251, -1862, 28669, 28669, -1862,  1142,   731,   965,
    1075,   838,  1122,  1176,  1181,  1175,  1225,    40, -1862,   712,
   -1862,  1208, -1862,  1058,  5268, 20637, -1862, -1862,   814,  1208,
   -1862, -1862,   728, -1862, -1862,   791,  4629,  1232,  1234,  1259,
    1261,  1263,  1265, -1862, -1862,   485,  1210, -1862,   801,  1210,
    1231, -1862,  1252, -1862, 23123, -1862,  1159,  1270, 20804, -1862,
   -1862,  4981,  3082,  1289, 18378,  1315,  1296,  1342,  1281,  1297,
   -1862, -1862, -1862,  5285,  5078, 22471, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, 21147,  3846,  1130, 26651,  1302,  1307, -1862,
   -1862,  1301, 26816,   772, -1862, -1862, -1862, 20637,  1316, -1862,
     756, -1862, -1862, -1862,  1294,  4812,   787,  1331,  1333,  1341,
     861,  1347,  1349,  1351,  1354,  1357,  1403,  5890, -1862, -1862,
   -1862,   236,  1355,  1384, -1862, -1862,  1392,   508, -1862, -1862,
     572,  1017, 22103, -1862, -1862,   508, -1862, -1862,   572, -1862,
   -1862,  6479, -1862, 20637, 20637, -1862,   718,  5330, 26286,  2190,
   18550, -1862, -1862, -1862, -1862, -1862, -1862,   572,  1017,     1,
    1401, -1862, -1862,  3576,  1405,  1017, 19305, -1862,   572,  1017,
   -1862, 26648, -1862,   718,   718, -1862, -1862,  1408,    85,  1412,
     545,  1416, -1862, -1862, -1862, 22411,  1426,  1425, -1862, -1862,
     841, -1862,  1524, -1862,  1419, -1862, -1862, -1862, 23947, 29054,
   -1862, -1862, -1862, -1862, -1862,  4948,  1034,  6479, 22103,   978,
   11466, -1862,  5285,  1444, 15018,  1457, -1862, -1862, -1862, -1862,
   -1862,  4292, -1862, -1862,  1538,  4963, 22634, 12982, -1862, 24007,
   -1862,   718,   718, -1862, -1862,   856, -1862, 17346,  1460,  1615,
   28669,  1494,  1392,  1454, -1862,   236,   236, -1862,  1210, -1862,
   23449, -1862, -1862, 22411,   718,   718, -1862,  4963, -1862, -1862,
   24985, -1862, -1862, 23612, -1862,   236,  1474,   236,  1086,   605,
    1476,   844, 23286,   871,   890, -1862,  1579,  8185,  1461, -1862,
   20303, -1862,  4496, 20470, -1862, 24170, 23286, -1862, 20303, -1862,
   28669, -1862, -1862, -1862, -1862, -1862, -1862, 20470, -1862, -1862,
   22797, 24170, 24170,  1211,  1815,  2250,   308,  2264, -1862,   897,
    1483,  1229,  1486, -1862, 26893, 28669, 26970,  1481, 28669,  2493,
   28669,  2493, -1862,  2537, -1862, -1862,  8185,  3029, 28669,  8185,
    2493, -1862, -1862, 28669, 28669, 28669, 28669, 28669, 28669, 28669,
   28669, 28669, 28669, 28669, 28669, 28669, 28669, 28669, 28669, 28669,
   28669, 28669, 27047, -1862,   756,  4800, 12982, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,  1484, 28669,
   -1862, -1862, 17518,  2332, -1862, -1862,   236,   236, -1862, -1862,
   20637, -1862, -1862,   618,  1210, -1862,   902,  1210, 22103, -1862,
   -1862,  1392, 22103, -1862,  1392, -1862, 29131, -1862, -1862, -1862,
   21605, 12982,  1490,  1239,  1491, 12079,  1642,  3157,   629,  1454,
   -1862,   236,   236,  1454,   646, -1862,   236,   236, 28669,  5285,
   18550,  1495, 18550,  1496,  1454,   -50, 17690, 17690, 18722, 17690,
    5285, -1862, -1862, 28669,  1301, -1862, 26651,  1500, -1862,  3575,
   -1862, -1862, -1862,   903, -1862,  1502, 17690, 28669,   944,  1505,
    1508,  1510,   945,  1512,  1514,  1516,  1517,  1519,  1522,   648,
    1210, -1862, -1862,   692,  1210, -1862, -1862,   707,  1210, -1862,
   -1862, -1862,  5330,  1643,  1210, 25609, -1862, -1862,   572,  1526,
   -1862, -1862, -1862,   979,  1539,   988,  1540, -1862,  1231,  1521,
    1548, -1862,   572, -1862, 14203, -1862,   572,  1017,  1548, -1862,
     572,  1544,  1547,  1554, -1862, -1862, 22266, -1862,  2493,  5285,
   10908,  1634, -1862,  1270, -1862, 17690,   984, -1862,  1548,  1561,
   -1862, -1862, -1862, -1862,  5330, 24333, 14366, -1862,    65,   423,
   20637,  1532, -1862,  1532, -1862, -1862, -1862, -1862, 23612, -1862,
   13158, 20971, -1862,  1562,  1566,  1571,  1572, -1862, 15085,   236,
   -1862,  1494, -1862, -1862, -1862, -1862,  1392, -1862, -1862, -1862,
     718, -1862, -1862, -1862, -1862,   605,  1086,  1559,   452, -1862,
   -1862,  1570,  5285,   605, -1862, -1862,  1576,  1577, -1862,  2493,
    1578,  1580, -1862, -1862, -1862,  1583,  1585,  1588,  1594,  1592,
    1598,  1603,  1604,  1609,  1606,  1613, 28669,  1616,  1629,  1633,
   24496, 13334, 28669, -1862, -1862,  2547, -1862, -1862, -1862, 28669,
   -1862,  1635,  1637, 26739, -1862, -1862,  1243, -1862,  8185,  1595,
   -1862,  1636, -1862, -1862,  3594, -1862,  1638, -1862,  3594, -1862,
   -1862,  1287,  1644, -1862, -1862,  1142,  1142,  1142,   731,   731,
     965,   965,  1075,  1075,  1075,  1075,   838,   838,  1122,  1176,
    1181,  1175,  1225, 28669,  1250,  1645,  3594, -1862, -1862, 26651,
   -1862,  1646,  1649,  1652,  1656,  2332, -1862, -1862, -1862, -1862,
   -1862, 22103, -1862, -1862,  1392, 22103, -1862,  1392,  1657,  1658,
   17690, 17690, -1862, -1862, 12079,  1040,  1659,  1660,  1671,  1676,
    2910,  3157, -1862, -1862, 22103, -1862, -1862, -1862, -1862, -1862,
   -1862, 22103, -1862, -1862, -1862, -1862,  1639, -1862,  1454,  1673,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,  1679,  1678,
    1682, -1862,  1683, -1862,   508,  3594,  1300,    41, -1862, -1862,
    1691, -1862, 26816, -1862, 28669,   236, 17690,   236, -1862, -1862,
     724,  1210, -1862,   729,  1210, -1862, -1862,   739,  1210, 22103,
   -1862, -1862,  1392, 22103, -1862, -1862,  1392, 22103, -1862, -1862,
    1392,   978, -1862,  1392,   -53, -1862,  1208,  1686, -1862, -1862,
   -1862, -1862, -1862, -1862,  1696, -1862, -1862, -1862, 14529,  1548,
   -1862,   572, -1862, -1862, -1862, -1862, -1862, 16347, -1862, -1862,
   -1862, -1862, -1862,   451,   682,   125, 12806,  1698,  1699, 19119,
    1701,  1702,  1669,  3312,  3941, 27124,  1703, -1862, -1862,  1704,
    1707, 19119,  1709, -1862, -1862,   572, 28669, 28669,  1860,  1706,
     625, -1862, 19802,  1306,  1708,  1711,  1689, -1862, -1862, -1862,
   10722, -1862, -1862, -1862, -1862, -1862,  2310, -1862, -1862, -1862,
    1390,   180, -1862,   333, -1862,   180, -1862, -1862, -1862, -1862,
   -1862,  2493, -1862, -1862, 11896, 15659, -1862,  5285,  1714,  1715,
   -1862, -1862, -1862,  5285, -1862, -1862, -1862,  5285, -1862,  5330,
   -1862,   994, 23286,   681,   681, -1862, -1862,  1130,  1270, 20804,
   -1862,  1208, -1862, 13510, -1862,   741,  1210, -1862,   718, 15162,
   -1862, -1862,  1086,  1570,  1713,   605,   545,   493,  1723,  1712,
    1570, 14692, -1862,  1717, -1862,  8185,   634, -1862, 22411,   634,
   13334,  2493, -1862,   634, -1862, 22960,   634, -1862, 28669, 28669,
   28669, -1862, -1862, -1862, -1862, 28669, 28669,  1718, 26651, -1862,
   -1862, 27201,  1719,  1729, -1862, -1862, -1862,  2999, -1862, -1862,
    1322, -1862,   106, -1862,  1325, -1862, 26893, -1862, -1862, 28669,
   -1862,  1356,  1358,  1301, -1862,   758,  1210, -1862, -1862,  1728,
    1736, -1862, -1862, -1862, -1862,  1737,   815,  1210, -1862,   862,
    1070,   236,   236, -1862, -1862,  1740,  1742, -1862,  1752, -1862,
   18550,  1754, -1862, 18034, 18206,  1743, 18722,  1758, -1862,  1755,
   28669, 28669,  1365,  1757, -1862, -1862, -1862, -1862, -1862, -1862,
    1762, 22103, -1862, -1862,  1392, 22103, -1862, -1862,  1392, 22103,
   -1862, -1862,  1392,  1763,  1764,  1769,   508, -1862, -1862,  1368,
   28669, 25769,  1767,  1774, -1862, -1862, -1862,  1775, 16513, 16679,
   16845, 24659, 26081, 24170, 24170,  1777,  1753,   457,   487,  2806,
   17174, -1862,   494,  5285,  5285, -1862,  8185,   179,   203, -1862,
   -1862, -1862, -1862, 12806, 28669,  1782,  1859, 12629, 11094, -1862,
    1760, -1862,  1768, 28669,  1770, 26651,  1778, 28669, 20637, 28669,
   -1862, 11280,  1014, -1862,  1780,   -10, -1862,    36,  1852,   313,
     236, -1862,  1790, -1862,  1784, -1862,  1786,  1792,  1799, 19119,
   19119, -1862, -1862,  1866, -1862, -1862,    20,    20,   912, 12445,
     496, -1862, -1862,  1805,  1813,   426, -1862, -1862, -1862, -1862,
   -1862, -1862, 13686,  1809,  1819,  1823, -1862, 22103, -1862, -1862,
    1392, 28669, 28669,  1270,  1824, -1862,  1810,  1831,   605,  1570,
     452,  5285, -1862, 27278, -1862,  1832, -1862, 14855, 28669, -1862,
     987,  1833,  1826,  1031, -1862,  1830, -1862, -1862, -1862, -1862,
   -1862, 26651,  1301, -1862, -1862, 26893, -1862,  1874,  3594, -1862,
    1874,  1874, -1862,  3594,  5057,  5262, -1862,  1372, -1862, -1862,
    1835, 22103, -1862, -1862,  1392, -1862, -1862,  1841,  1842,   236,
   22103, -1862, -1862,  1392, 22103, -1862, -1862,  1844, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862,  1673, -1862, -1862, -1862,
    1847, -1862, -1862, -1862, -1862,  1848,  1845,   236,  1855,  1856,
    1857, -1862, -1862, -1862, -1862, 28669, -1862,   -53, -1862,  1208,
   -1862, -1862,  1850, -1862,  1777,  1777,  1777,  4159,  1054,  1851,
     575, -1862,  4159,   577, 20637, -1862, -1862, -1862, -1862,  5156,
   28669,  6866,   450, -1862, -1862,    31,  1853,  1853,  1853,  5285,
   -1862, -1862, -1862,  1870, -1862, -1862, -1862, -1862,  1711,  1875,
   28669,   395,  1863,   403, 17018, 24659,  1044,  1882, 19119,  1881,
   -1862, -1862, -1862, -1862,  1066, 19119, 28669,  1344,   587, -1862,
   28669,  9766, -1862, -1862,   635, -1862,  1301, -1862,  1047,  1082,
    1088,   695, -1862, -1862, -1862, -1862,   572,  1014,  1884, -1862,
   -1862, 28669, -1862,  1886,   756, -1862, 10514, -1862, -1862, -1862,
   28669, 28669, -1862, -1862,   465,    20, -1862,   365, -1862, -1862,
   -1862,   236, -1862,  1532, -1862, -1862, -1862,  1883,  1887, -1862,
   -1862,  1892, -1862,  1893,   605, -1862,  1570,  1903,   545,  1712,
   26651, -1862, -1862, -1862, -1862,  1890, -1862, -1862, 28669, -1862,
   22960, 28669,  1301,  1905,  1377, -1862,  1381, -1862,  3594, -1862,
    3594, -1862, -1862, -1862,  1912,   236,   236,  1913,  1914, -1862,
    1904, -1862, -1862, -1862, -1862, -1862,  1387, 28669, -1862, -1862,
   -1862, -1862,   637,  1054,  2384,   652, -1862, -1862, -1862, -1862,
     236,   236, -1862, -1862, -1862,   661, -1862,  1090,  5156,   827,
   -1862,  6866, -1862,   236, -1862, -1862, -1862, -1862, -1862, -1862,
   19119, 19119,  1711, 18894,    60, 27355,  1998, 19119, -1862, -1862,
   -1862, -1862, -1862, 28669, -1862, 27432,  2001,  1896, 21214, 27509,
   19119, 11280,  1711,   935,  1565,  1897, 28669, -1862,  1924,   343,
   19119, -1862, 19119, -1862,  1928, -1862, 24822,  1906,   756,   698,
   -1862, -1862,  1927,  1388,  1107, 19119,  1933, 19119, 19119, 19119,
   -1862,   681, -1862, -1862,  1929,  1930, -1862, -1862,  1570,  1937,
   -1862, -1862,  1301, -1862, -1862, -1862, -1862,  1939, -1862, -1862,
   -1862,  1394,  1406, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862,  1940,  1942,  1943,  2384, -1862,   236, -1862, -1862,
   -1862, -1862, -1862,  1935,  4159, -1862,  2028,  5695,   118, 13865,
   -1862, 18993, -1862,     0,  1112, 19119,  2034,   664,  1944,   225,
   19119, 28669,   935,  1565,  1931, 27591,   918,   718,  1945,   373,
    2036, -1862, 27668, 27745, 28669,  1711,  1934, 14040, -1862, -1862,
   -1862, 24822,  1941,  5185, 24007,  2493, -1862,  1951,  1946,    33,
   -1862, 28669,  8185, -1862, -1862, 28669,   180, -1862, -1862, -1862,
   -1862, -1862,  1965, -1862,  1966, -1862, -1862, -1862,   872,  1210,
   -1862, -1862,  1054, -1862, 19119, -1862,   251, -1862,   140, -1862,
   -1862, -1862,  1968, 16008, -1862, -1862, 19119, -1862,    83, -1862,
   19119, 28669,  1969, 27822, -1862, -1862, 27899, 27976, 28669,  4963,
    1711, -1862,  1208, 28053, 28130, 19119,  1953,   455,  1957,   462,
   -1862, -1862,  1977, 16008,  1941, 28669,  1975,  4374,  3499, -1862,
   -1862, -1862,  1974, -1862,  2043,  1984,   757,  1980, -1862, -1862,
    1994,  1135,   413, -1862, -1862, 22103, -1862, -1862,  1392, -1862,
   -1862, 28669, -1862, 28669, -1862, -1862,  1497, 16181, -1862, -1862,
   19119, -1862, -1862,  1711, -1862, -1862,  1711,  1982,   549,  1987,
     580, -1862, -1862,   545, -1862,  1711, -1862,  1711, -1862,  1999,
   28207, 28284, 28361, -1862,  1497,  2002, -1862,   572,  3499,    33,
    2004, 28669,  1981,    33,    33, -1862, -1862, 19119,  2088,  2008,
   -1862, -1862, 18993, -1862,  1497, -1862, -1862,  2011, 28438, 28515,
   28592, -1862, -1862,  1711, -1862,  1711, -1862,  1711, -1862,   572,
   -1862,  2010,   756,  2013, -1862,   759, -1862, -1862, 19119, -1862,
   -1862, 10215,  2020, 18993, -1862, -1862,  1711, -1862,  1711, -1862,
    1711,  2021, -1862,   756,  2024, -1862,  2000,   756, -1862, -1862,
   -1862, -1862, 10362, -1862, -1862,  1436, 28669, -1862,  1136,   756,
    2493,  2025,  2006, -1862, -1862,  1177, -1862, -1862,  2012,  2493,
   -1862, -1862
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   499,     0,     2,   499,   516,   517,   518,   519,   520,
     521,   522,   523,   524,   505,   507,   506,   508,     0,     0,
       0,   526,   528,   555,   529,   556,   532,   533,   553,   554,
     527,   551,   552,   530,   531,   534,   535,   536,   537,   538,
     539,   540,   541,   542,   543,   544,   545,   546,   547,   548,
     549,   550,   557,   558,   864,   560,   633,   634,   637,   639,
     635,   641,     0,     0,     0,   499,     0,     0,    17,   604,
     610,     9,    10,    11,    12,    13,    14,    15,    16,   820,
     104,     0,     0,    20,     0,     2,   102,   103,     0,   841,
      18,    19,   879,   499,   821,     0,     0,   438,   742,   440,
     451,   862,   439,   473,   474,     0,     0,     0,     0,   587,
     501,   503,   509,   499,   511,   514,   572,   525,   559,   483,
     565,   570,   485,   582,   484,   597,   601,   607,   586,   613,
     625,   864,   630,   631,   614,   683,   441,   442,     3,   828,
     842,   504,     0,     0,   864,   902,   864,   499,   919,   920,
     921,   499,     0,  1106,  1107,     0,     1,   499,    17,     0,
     499,   462,   463,     0,   587,   509,   493,   494,   495,   831,
       0,   636,   638,   640,   642,     0,   499,   864,   686,   865,
     866,   632,   561,    22,    23,    21,   796,   791,   781,     0,
     873,   829,     0,     0,   516,   822,   826,   827,   823,   824,
     825,   499,   873,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   605,   608,   499,   499,   880,     2,     0,  1108,
     587,   909,   927,  1112,  1105,  1103,  1110,   435,   434,     0,
     173,   748,   172,     0,   443,     0,     0,     0,     0,     0,
     449,     0,     0,     0,   433,   996,   997,     0,     0,   472,
     862,   864,   862,   883,   864,   864,   482,   499,   864,   862,
     940,   864,   864,   481,   864,   959,   864,   937,     0,   580,
     581,     0,     0,   499,   499,     2,   499,   452,   862,   502,
     512,   573,     0,   602,     0,   845,   499,     0,     0,   742,
     453,   587,   566,   583,   598,     0,   845,   499,     0,   515,
     567,   574,   575,   486,   584,   488,   489,   487,   589,   599,
     603,     0,   617,     0,   814,   499,     2,   843,   901,   903,
     499,     0,   499,     0,     0,   587,   499,   511,     2,  1116,
     587,  1119,   862,   862,     3,     0,   587,     0,     0,   465,
     864,   857,   859,   858,   860,     2,   499,   862,     0,   818,
       0,     0,   777,   779,   778,   780,     0,     0,   773,     0,
     762,     0,   771,   783,     0,   864,   684,     2,   499,  1128,
     500,   499,   490,   565,   491,   590,   492,   597,   594,   615,
     864,   616,   730,     0,   499,   731,  1081,  1082,   499,   732,
     734,   686,   604,   610,   687,   688,   689,     0,   686,   867,
       0,   794,   782,     0,   878,   877,   873,   876,     0,   871,
     874,    25,     0,    24,     0,     0,     0,     0,     0,     0,
       0,     0,    27,    29,     4,     8,     5,     6,     7,     0,
       0,   499,     2,     0,   105,   106,   107,   108,    85,    28,
      86,    46,    84,   109,     0,     0,   124,   126,   130,   133,
     136,   141,   144,   146,   148,   150,   152,   154,   157,     0,
      30,     0,   611,     2,   109,   499,   165,   788,   738,   601,
     740,   787,     0,   737,   741,     0,     0,     0,     0,     0,
       0,     0,     0,   881,   907,   864,   917,   925,   929,   935,
     604,     2,     0,  1114,   499,  1117,     2,   102,   499,     3,
     729,     0,  1128,     0,   500,   565,   590,   597,     3,     3,
     725,   715,   719,   731,   732,   499,     2,     2,   910,   928,
    1104,     2,     2,    27,     0,     2,   748,    28,     0,   746,
     749,  1126,     0,     0,   755,   744,   743,   499,     0,   847,
       0,     2,   464,   466,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   886,   943,
     966,   864,   478,     2,   882,   890,  1024,   742,   884,   885,
       0,   845,   499,   939,   947,   742,   941,   942,     0,   958,
     960,     0,   468,   499,   499,   571,   500,     0,   587,     0,
     499,  1109,  1113,  1111,   450,   588,   818,     0,   845,   862,
       0,   444,   454,   513,     0,   845,   499,   818,     0,   845,
     792,   568,   569,   585,   600,   606,   609,   604,   610,   628,
     629,     0,   793,   701,   735,   500,     0,   702,   704,   705,
       0,   213,   427,   844,     0,   425,   482,   481,   587,     0,
     446,     2,   447,   815,   470,     0,     0,     0,   499,   862,
     499,   818,     0,     0,     0,     0,   776,   775,   774,   768,
     510,     0,   766,   784,   563,     0,   499,   499,  1083,   500,
     496,   497,   498,  1087,  1078,  1079,  1085,   499,     2,   103,
       0,  1043,  1057,  1128,  1039,   864,   864,  1048,  1055,   723,
     499,   595,   733,   500,   591,   592,   596,     0,   685,  1093,
     500,  1098,  1090,   499,  1095,   864,     0,   864,   686,   686,
       0,     0,   499,     0,     0,   869,   873,   158,     0,    26,
     499,    92,     0,   499,   100,   499,   499,    87,   499,    94,
       0,    36,    40,    41,    37,    38,    39,   499,    90,    91,
     499,   499,   499,     2,   105,   106,     0,     0,   191,     0,
       0,   631,     0,  1103,     0,     0,     0,     0,     0,     0,
       0,     0,    59,     0,    65,    66,   158,     0,     0,   158,
       0,    88,    89,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   437,     0,     0,   499,   174,   175,   176,
     177,   178,   179,   180,   181,   182,   183,   184,   172,     0,
     170,   171,   499,  1008,   739,  1005,   864,   864,  1013,   612,
     499,   870,   908,   864,   918,   926,   930,   936,   499,   911,
     913,   915,   499,   931,   933,     2,     0,     2,  1115,  1118,
     499,   499,     0,     2,     0,   499,   103,  1043,   864,  1128,
     978,   864,   864,  1128,   864,   993,   864,   864,     3,   733,
     499,     0,   499,     0,  1128,  1128,   499,   499,   499,   499,
       0,     2,   757,     0,  1126,   754,  1127,     0,   750,     0,
       2,   753,   756,     0,     2,     0,   499,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   864,
     895,   899,   938,   864,   952,   956,   964,   864,   974,   887,
     944,   967,     0,     0,  1020,     0,   476,   848,     0,     0,
     477,   849,   469,     0,     0,     0,     0,   467,     0,     2,
       2,   850,     0,   448,     0,   818,     0,   845,     2,   851,
       0,     0,     0,     0,   643,   904,   499,   922,     0,     0,
     499,   428,   426,   102,     3,   499,     0,   819,     2,     0,
     770,   811,   806,   807,     0,   500,     0,   802,     0,     0,
     499,   764,   763,   764,   564,   562,   688,  1089,   499,  1094,
     500,   499,  1080,     0,     0,     0,     0,  1058,     0,   864,
    1129,  1044,  1045,   724,  1041,  1042,  1056,  1084,  1088,  1086,
     593,   628,  1092,  1097,   680,   686,   686,     0,     0,   696,
     695,  1126,     0,   686,   797,   795,     0,     0,   872,   162,
       0,   159,   160,   164,   830,     0,     0,     0,     0,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     499,   499,     0,   123,   122,     0,   119,   118,    31,     0,
      32,     0,     0,     0,   188,   187,     0,     3,   158,     0,
      55,     0,    56,    63,     0,    62,     0,    58,     0,    57,
      61,     0,     0,    54,   125,   127,   128,   129,   131,   132,
     134,   135,   139,   140,   137,   138,   142,   143,   145,   147,
     149,   151,   153,     0,     0,     0,     0,    33,     3,   748,
     166,     0,     0,     0,     0,  1009,  1010,  1006,  1007,   790,
     789,   499,   912,   914,   916,   499,   932,   934,     0,     0,
     499,   499,  1034,  1033,   499,     0,     0,     0,     0,     0,
     864,  1044,   981,   998,   499,   976,   984,   721,   979,   980,
     722,   499,   991,  1001,   994,   995,     0,     3,  1128,     3,
     717,   460,   716,   720,  1120,   726,   727,   709,     0,   710,
     711,     3,     3,     3,   742,     0,   157,     0,     3,     3,
       0,   751,     0,   745,     0,   864,   499,   864,     3,   471,
     864,   896,   900,   864,   953,   957,   965,   864,   975,   499,
     888,   891,   893,   499,   945,   948,   950,   499,   968,   970,
     972,   862,   479,  1021,     3,  1025,  1026,     3,   853,   961,
     577,   576,   579,   578,     2,   819,   854,   799,     0,     2,
     852,     0,   819,   855,   643,   643,   643,   499,   703,   706,
     707,   736,   431,     0,     0,     0,   499,     0,     0,   352,
       0,     0,     0,     0,     0,   193,     0,   347,   348,     0,
       0,   352,     0,   400,   399,     0,   168,   168,   406,   604,
     610,   210,   499,     2,     0,   194,     0,   221,   195,   196,
     499,   215,   197,   198,   199,   200,     0,   201,   202,   353,
       0,   367,   203,   373,   375,   378,   204,   205,   206,   207,
     208,     0,   209,   217,   587,   499,   219,     0,     0,     0,
       3,   832,   819,     0,   809,   786,   803,     0,   804,     0,
     805,     0,   499,   781,   781,  1091,  1096,     2,   102,   499,
       3,   602,     3,   500,  1052,   864,  1051,  1054,   499,     3,
    1040,  1046,   686,  1126,     0,   686,   692,   686,     0,   697,
    1126,     0,   868,     0,   875,     0,    93,    96,   499,   101,
     499,     0,    99,    95,    97,   499,     0,   113,     0,     0,
       0,   117,   121,   120,   192,     0,     0,     0,   748,   110,
     185,     0,     0,     0,    49,    50,    82,     0,    82,    82,
       0,    70,    72,    52,     0,    48,     0,    51,   156,     0,
     436,     0,     0,  1126,  1017,   864,  1016,  1019,  1011,     0,
       0,   905,   923,     3,     3,     0,   864,   987,   990,   864,
       0,   864,   864,   982,   999,     0,     0,  1121,     0,   728,
     499,     0,  1123,   499,   499,     0,   499,     0,   445,     3,
       0,     0,     0,     0,   747,   752,     3,   846,     3,   863,
       0,   499,   889,   892,   894,   499,   946,   949,   951,   499,
     969,   971,   973,     0,     0,     0,   742,  1032,  1031,     0,
       0,     0,     0,     0,   801,   819,   856,     0,   499,   499,
     499,   499,   499,   499,   499,   626,     0,     0,     0,   657,
     587,   644,     0,     0,     0,   429,   158,     0,     0,   338,
     339,   218,   220,   499,     0,     0,     0,   499,   499,   334,
       0,   332,     0,     0,     0,   748,     0,     0,   499,     0,
     379,   499,     0,   169,     0,     0,   407,     0,     0,     0,
     864,   225,     0,   216,     0,   329,     0,     0,     0,   352,
     352,   358,   357,   352,   369,   368,   352,   352,     0,   587,
       0,  1036,  1035,     0,     0,   773,   808,   810,   785,   765,
     769,   767,   499,     0,     0,     0,     3,   499,  1047,  1049,
    1050,     0,     0,   102,     0,     3,     0,     0,   686,  1126,
       0,     0,   675,     0,   691,     0,   798,     0,     0,   161,
    1037,     0,     0,     0,    42,     0,   114,   116,   115,   112,
     111,   748,  1126,   190,   189,     0,    69,    79,     0,    73,
      80,    81,    64,     0,     0,     0,    60,     0,   155,    34,
       0,   499,  1012,  1014,  1015,   906,   924,     0,     0,   864,
     499,   983,   985,   986,   499,  1000,  1002,     0,   977,   992,
     988,  1003,  1122,   718,   461,   713,   712,   714,  1125,  1124,
       0,     3,   861,   758,   759,     0,     0,   864,     0,     0,
       0,   897,   954,   962,   480,     0,  1027,     0,  1028,  1029,
    1023,   836,     0,   838,   626,   626,   626,   657,   664,   631,
       0,   670,   657,     0,   499,   618,   656,   655,   651,     0,
       0,     0,     0,   658,   660,   864,   672,   672,   672,     0,
     652,   668,   432,     0,   342,   343,   340,   341,   234,     0,
       0,   236,   440,   235,   587,   499,     0,     0,   352,     0,
     317,   319,   318,   320,     0,   352,   193,   274,     0,   267,
       0,   193,   335,   333,     0,   327,  1126,   336,     0,     0,
       0,     0,   388,   389,   390,   391,     0,   381,     0,   382,
     344,     0,   345,     0,     0,   372,     0,   214,   331,   330,
       0,     0,   361,   371,     0,   352,   374,     0,   376,   398,
     430,   864,   834,   764,  1099,  1100,  1101,     0,     0,     3,
       3,     0,  1060,     0,   686,   676,  1126,     0,   694,   697,
     748,   698,   679,   800,   163,     0,  1038,    98,     0,    35,
     499,     0,  1126,     0,     0,    83,     0,    71,     0,    77,
       0,    75,    47,   167,     0,   864,   864,     0,     0,   761,
       0,   455,   459,   898,   955,   963,     0,     0,   840,   622,
     624,   620,     0,     0,  1067,     0,   665,  1072,   667,  1064,
     864,   864,   650,   671,   654,     0,   653,     0,     0,     0,
     674,     0,   646,   864,   645,   661,   673,   662,   663,   669,
     352,   352,   237,   587,     0,     0,   255,   352,   322,   325,
     323,   326,   321,     0,   324,     0,   263,     0,   193,     0,
     352,   499,   275,     0,   300,     0,     0,   328,     0,     0,
     352,   351,   352,   392,     0,   383,   499,     0,     0,     0,
     212,   211,   354,     0,     0,   352,     0,   352,   352,   352,
     458,   781,  1102,  1053,     0,     0,  1059,  1061,  1126,     0,
     678,   693,  1126,    53,    45,    43,    44,     0,    67,   186,
      74,     0,     0,  1018,   457,   456,   989,  1004,   760,  1022,
    1030,   648,     0,     0,     0,  1068,  1069,   864,   649,  1065,
    1066,   647,   627,     0,     0,   350,   226,     0,     0,     0,
     248,   352,   228,     0,     0,   352,   257,   272,   283,   277,
     352,   193,     0,   287,     0,     0,     0,   312,   278,   276,
     265,   268,     0,     0,   193,   301,     0,     0,   231,   349,
     380,   499,   386,   393,   500,   397,   346,     0,     0,   408,
     359,     0,   158,   370,   363,     0,   364,   362,   377,   772,
    1062,  1063,     0,   682,     0,    68,    78,    76,   864,  1075,
    1077,  1070,     0,   659,   352,   243,   238,   241,     0,   240,
     247,   246,     0,   499,   250,   249,   352,   259,     0,   256,
     352,     0,     0,     0,   264,   269,     0,     0,   193,     0,
     288,   313,   314,     0,     0,   352,     0,   303,   304,   302,
     271,   337,     0,   499,   386,     0,     0,     0,  1067,   394,
     395,   396,     0,   401,     0,     0,     0,   409,   410,   355,
       0,     0,     0,   681,   699,   499,  1071,  1073,  1074,   666,
     227,     0,   245,     0,   244,   230,   251,   499,   421,   260,
     352,   261,   258,   273,   286,   284,   280,   292,   290,   291,
     289,   270,   315,   316,   285,   281,   282,   279,   266,     0,
       0,     0,     0,   233,   251,     0,   387,     0,  1068,   408,
       0,     0,     0,   408,     0,   360,   356,   352,     0,     0,
     239,   242,   352,     3,   252,   422,   262,     0,     0,     0,
       0,   311,   309,   306,   310,   307,   308,   305,     3,     0,
     384,     0,     0,     0,   402,     0,   411,   365,   352,  1076,
     222,     0,     0,   352,   299,   297,   294,   298,   295,   296,
     293,     0,   385,   414,     0,   412,     0,   414,   366,   224,
     223,   229,     0,   232,   415,     0,     0,   403,     0,     0,
       0,     0,     0,   416,   417,     0,   413,   404,     0,     0,
     405,   418
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1862,  2618,  1319, -1862,    -1,   167,  2664,  7474,    22, -1862,
    -380, -1862,   402, -1862, -1025, -1029, -1862,   232,  3796,  1867,
   -1862,  1264, -1862,  1450,   596,   837,   842,   581,   831,  1415,
    1420,  1418,  1424,  1427, -1862,   165,  -191,  -756, -1862,   864,
    8412,   958, -1862,  1746, -1862, -1862, -1331,  8150, -1123,  3762,
   -1862,  1456, -1862,   947,    58, -1862, -1862,   725,   144, -1862,
   -1858, -1537,   344,   115, -1862, -1862,   720,   361, -1862, -1643,
   -1862, -1645, -1862, -1862, -1862, -1862,   161, -1160, -1862, -1862,
   -1259,   480, -1862, -1862, -1862, -1862, -1862,    95, -1221, -1862,
   -1862, -1862, -1862, -1862,   184,   502,   503,   261, -1862, -1862,
   -1862, -1862,  -776, -1862,   119,    68, -1862,   191, -1862,  -190,
   -1862, -1862, -1862,   950,  -839,  -929,  -145, -1862,   349,    49,
      50,   565,  -861,  -729, -1862,  -107, -1862, -1862,    51, -1862,
    -142,   823,  2071,  -272,  4016,  6625,  -657,    67,   132,   315,
    1212,  2433, -1862, -1862,  2194, -1862,   235,  4722, -1862,  2128,
   -1862,   992, -1862, -1862,  2074,   763,  5394,  3206,   -59,  1970,
    -366, -1862, -1862, -1862, -1862, -1862,  -229,  6201,  6124, -1862,
    -415,   228, -1862,  -953,   310, -1862,   241,   783, -1862,   -48,
    -223, -1862, -1862, -1862, -1862,  -130,  6763,  -984,   922,   492,
    2646, -1862,  -400,  -149,  -179,  3308,  2120,  -787,  -153,   993,
    -303,  -340,  -274,  -203,  -520,  1407, -1862,  1766,   384,  -956,
    1641, -1862, -1862,   745, -1862, -1244,  -181,   149,  -518, -1862,
     389, -1862, -1862,  -901,  -927, -1862, -1862, -1862,  2304,  -855,
    -465, -1121,   -28, -1862, -1862, -1862, -1862, -1862, -1862,   309,
    -874,  -222, -1861,   194,  7160,   -71,  6961,  -122,  1599, -1862,
    1827,  -101,  -238,  -219,  -212,    30,   -74,   -56,   -55,   422,
     -38,   -37,    -8,  -184,   272,  -182,  -162,  -126,   297,   -86,
     -43,   -29,  -791,  -739,  -711,  -688,  -788,  -164,  -675, -1862,
   -1862,  -766,  1506,  1507,  1513,   218, -1862,   667,  8015, -1862,
    -663,  -628,  -600,  -588,  -702, -1862, -1652, -1765, -1732, -1725,
    -658,  -152,  -346, -1862, -1862,   -81,   830,   -94, -1862,  8740,
    3286,  -790,  -595
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   863,   438,   439,   185,    92,  1264,   440,   413,
     441,  1583,  1584,   442,  1380,  1381,  1382,  1597,   464,   444,
     445,   446,   746,   747,   447,   448,   449,   450,   451,   452,
     453,   454,   455,   456,   457,   466,  1167,  1020,  1021,  1022,
     748,  1514,   809,   233,   811,   460,  1056,  1265,  1266,  1267,
    1268,  1269,  1270,  1271,  2161,  1272,  1273,  1699,  2017,  2018,
    1950,  1951,  1952,  2133,  2134,  1274,  1718,  1719,  2041,  1720,
    1864,  1865,  1275,  1276,  1277,  1278,  1279,  1280,  1893,  1897,
    1537,  1529,  1281,  1282,  1536,  1530,  1283,  1284,  1285,  1286,
    1287,  1288,  1289,  1737,  2056,  1738,  1739,  1982,  1290,  1291,
    1292,  1517,  2066,  2067,  2068,  2185,  2195,  2086,  2087,   321,
     322,   950,   951,  1233,    94,    95,    96,    97,    98,  1702,
     500,   218,   102,   103,   104,   105,   249,   250,   324,   303,
     502,   468,   503,   108,   336,   110,   111,   165,   371,   327,
     115,   116,   117,   181,   118,   975,   372,   166,   121,   273,
     122,   167,   282,   374,   375,   376,   168,   461,   127,   128,
     378,   129,   621,   943,   941,   942,  1675,   379,   380,   132,
     133,  1227,  1481,  1682,  1683,  1825,  1826,  1482,  1670,  1845,
    1684,   134,   708,  1332,   177,  1010,   381,  1011,  1012,  1574,
     983,   627,  1158,  1159,  1160,   628,   382,   511,   512,   630,
     470,   471,   234,   530,   531,   532,   533,   534,   359,  1313,
     360,   973,   971,   659,   361,   401,   362,   363,   472,   136,
     187,   188,   137,   966,   967,   968,   969,     2,  1214,  1215,
     650,  1301,   138,   349,   350,   284,   295,   604,   139,   237,
     140,   339,  1169,   594,   564,   179,   141,   408,   409,   410,
     142,   341,   253,   254,   255,   342,   144,   145,   146,   147,
     148,   149,   150,   258,   343,   260,   261,   262,   344,   264,
     265,   266,   849,   850,   851,   852,   853,   267,   855,   856,
     857,   814,   815,   816,   817,   565,  1207,  1460,   151,  1785,
     683,   684,   685,   686,   687,   688,  1828,  1829,  1830,  1831,
     673,   513,   386,   387,   388,   473,   224,   153,   154,   155,
     390,   877,   689
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      90,   585,   459,    90,   691,   252,   874,   402,   206,   547,
    1066,   204,   223,  1072,   398,   601,   751,  1314,   992,   883,
     543,  1333,  1533,   383,   385,   213,   207,   208,   548,  1340,
     536,   143,   998,  1218,   369,   549,   509,   191,   701,  1306,
     323,  1519,   704,  1384,   209,   210,   582,  1106,   397,   984,
      99,   100,   106,   757,   162,  1607,  1132,   331,  1932,  1133,
     235,    90,    90,   550,    90,   551,   508,   571,   112,  1550,
    1551,  1391,  1869,  1150,   211,  1152,  1296,   985,  1876,   223,
     475,    90,  2026,  1222,  1170,   552,  2019,    90,   993,   986,
    1047,  1933,    90,  2025,  1463,   143,   598,  1741,  1934,   547,
      90,  1467,  1518,  1302,  1457,    90,  1126,   609,    90,   238,
      64,  1293,    90,   641,    99,   100,   106,  1250,   548,  2020,
     495,   553,  1506,   221,  -812,   549,  1458,  1743,   752,   334,
     477,   930,   112,   113,  1127,  -845,   256,   680,   691,   285,
    1429,  2082,   938,   296,  1947,  1948,  1136,   558,   478,   479,
     323,    90,  1143,   550,    90,   551,    90,  1128,   156,   277,
     411,   554,    90,   290,   641,  2090,   480,   481,    91,    90,
    1129,   159,  1936,   206,    81,   552,   545,    90,  1742,   323,
     169,  1544,   791,  1755,   992,  1430,   958,   143,  2027,    90,
     323,   207,   208,  1487,  1488,   592,   482,   113,   563,  1744,
    2064,    90,    90,    90,   555,  -996,    99,   100,   106,   209,
     210,   553,  -996,    90,   412,   634,   674,   984,   556,    90,
    1431,  1338,  2019,   252,   112,  1965,   792,   707,  1972,  1973,
     527,  1949,    91,   221,  1307,    90,   119,   558,  1604,   211,
      90,    90,    90,   924,   926,   985,    90,    90,   201,    91,
     518,   554,   616,  1308,  1137,   216,  -623,   986,  1140,   691,
      91,   706,  -813,   206,  1794,   592,   645,    90,   710,  1155,
    1156,  2091,  1605,    91,  2025,    90,    91,    90,   221,   326,
      91,   207,   208,  2011,   714,    64,    90,    90,  2021,   113,
      90,  1306,  1932,   916,   555,   240,   316,    90,  1476,  1531,
     119,   920,  1373,   691,   170,  2025,   221,   889,   556,  2083,
    2084,    90,    90,  1489,    90,    64,   114,  2036,  2037,    90,
     979,   605,  1532,    90,    91,  1933,   890,   691,  1331,   878,
      91,  2060,  1934,   891,   691,   175,    90,    90,   854,  1398,
    1413,   602,  1027,  1414,  1662,   395,   674,    90,  1599,    81,
     316,  1569,  1070,   161,   636,    90,    90,  1003,   458,   922,
      90,   892,  1492,   893,  1465,   927,  1477,  1694,   221,   176,
      91,    91,  1015,  1754,   316,   235,  1518,  1757,   259,    81,
     114,   492,   119,   894,   223,  1126,  1031,    91,  1363,   326,
      90,  1696,   119,    64,  1049,  2081,  1296,  1491,    91,    90,
     839,    20,    90,   263,  2033,    90,  1936,   889,   542,    91,
     544,   509,   190,  1127,    91,    91,   937,  1025,   326,   895,
    1028,   495,  1030,  1136,   280,  1032,   890,  1947,  1948,   326,
    1431,  1293,  1531,   891,  1035,    91,  1128,  1037,  1038,  1039,
    1577,   508,   214,    91,   562,    64,   567,  1899,   236,  1405,
    -845,  1534,  1311,   575,   326,  1532,   909,    81,    91,   896,
      -3,   892,   289,   893,  1528,   213,  2011,  1161,  1331,  1163,
    1219,   477,   114,  1042,  1535,    90,   574,   315,  1745,   619,
    1110,   563,   624,   894,  1043,  1044,  1178,   159,   509,   478,
     479,    91,   119,   192,   979,  2127,   984,    64,  1478,   323,
      90,    90,   897,  1570,    91,    91,   518,   480,   481,    81,
    1250,   663,    90,    90,  1977,   222,   898,   758,   508,   895,
     559,   119,   759,    90,   985,   527,   640,   642,   257,   572,
     193,   286,   119,   563,  1519,   297,   986,   482,   674,  1476,
    1476,  1476,  1049,  1567,    90,   560,   909,  1895,   201,  2132,
    1575,   663,  2044,  1419,   202,  1300,    90,   119,   705,   896,
     227,    81,   885,  1527,    64,   570,   101,    69,    70,   101,
     656,   477,   578,  1796,  1797,  1799,  1801,  2132,  1007,  1393,
      90,   648,  1896,   228,  1776,   563,    90,   518,    90,   478,
     479,   244,  1309,  1867,   597,  1518,   259,  2163,  1875,   691,
     657,   658,   897,  1610,   241,   608,   603,  1477,  1477,  1477,
     559,  1310,  1842,   242,   691,  1008,   898,   509,    84,  1843,
    1484,   637,   957,  1559,  1049,   222,   241,   315,    81,   483,
     101,  1049,  1316,  1633,  2110,   560,  1150,  1152,  1844,  1485,
     280,  2112,   519,    91,    90,  1677,    90,   508,   828,    90,
    1306,    90,   563,   243,  1700, -1127,   242,  1999,  1700,  1721,
      90,   998,   509,  1689,    90,  1484,  1009,  1147,   230,    91,
     222,   394,  1721,  1171,   278,  1678,   268,   518,   101,   231,
     143,  1149,  1690,   854,  1760,   280,   818,   610,   288,  1162,
    1201,    91,   508,    91,  -493,   232,    90,    64,   222,    99,
     100,   106,   622,   830,   963,   760,   833,  1626,    64,  1153,
     761,   311,    91,   606,  1822,    90,   385,   112,  1049,  1835,
    1403,  1404,   101,   313,    91,    64,   369,    64,  2138,   347,
    1693,    14,    15,    16,    17,    18,   616,  2072,   326,  1478,
    1478,  1478,  1304,   316,  1833,  1964,  1689,   171,    91,  1049,
     172,   173,  1870,   174,    91,   315,   492,  1871,    90,  2140,
      90,    81,    90,  1834,   124,  1836,    90,  -690,   574,    90,
    -677,    64,    81,  1921,  -690,  1922,  1438,  -677,  1009,  1777,
     501,  1111,   113,   394,   711,   563,    64,   713,   201,    81,
    1908,    81,  1134,   933,    90,   -23,   678,    64,  1367,   981,
     212,    70,  1793,    64,  1049,  1368,  1843,  1901,    64,  1141,
     335,  1189,    91,   678,    91,   563,  1095,    91,    64,   280,
      64,  1937,   660,  1877,   107,  1931,   661,   163,   124,   910,
    1843,   152,   976,  1049,   152,    81,  -833,    64,  2035,    90,
    1938,   119,   400,   933,    90,  1486,    90,   751,  1592,  1941,
      81,  2050,  2031,   715,   911,  1193,   278,   716,    90,   563,
    1882,    81,  1162,  1988,  1001,  1871,   357,    81,  1989,    90,
    1197,   201,    81,   403,   563,   527,  1334,   793,    90,  -997,
     918,   794,    81,   411,    81,   119,  -997,  1441,   107,   385,
    1428,   563,  1445,   819,    64,   152,   563,   820,   519,   369,
     484,    81,  1449,   485,  1557,   774,   563,   932,   678,   486,
     124,    90,   775,   776,   936,  2101,   219,  1786,   940,   910,
     124,  1611,  2122,   515,  2176,   563,  1060,  2123,  1062,  2177,
    1065,    14,    15,    16,    17,    18,  1878,  1073,   879,   880,
     280,    64,   881,   152,   911,   487,   495,    90,    90,   527,
     315,    64,   483,   603,   563,    80,   821,   691,    81,   752,
     716,   488,  1097,    90,   832,   114,  1412,   854,   563,  1435,
     107,   489,  1549,   981,   329,   781,   782,   812,  1620,   519,
     107,   563,   563,   219,   963,  1726,  1909,   152,    86,    87,
    1729,   516,   483,   123,   563,   269,   270,    64,   271,    99,
     100,   106,  1917,   962,   272,    81,   947,   492,   603,  1014,
     948,    90,    91,   661,    91,    81,   963,   112,    90,   521,
     124,   783,   784,   458,   572,  1624,   902,   522,   563,   678,
     818,   818,  1721,    69,    70,  2075,  1016,   493,  1166,   563,
     661,  1113,   535,    91,  1116,   537,    91,   171,   540,   124,
     172,   173,  1585,   174,   541,  1017,  1561,   123,  1669,   716,
     124,    81,  1048,    90,  -494,  1115,  1049,    90,  1175,   563,
     404,  1792,   820,   561,    14,    15,    16,    17,    18,    91,
     107,   948,  1295,   316,    84,   124,   238,  1547,   615,    70,
    -495,  1858,  1859,  1860,  1861,    90,   583,   587,   527,   591,
      14,    15,    16,    17,    18,   293,   584,   315,   572,   107,
     574,   563,   563,   235,  1862,  1231,   589,  1191,  2002,   631,
     107,  1195,  2004,    90,   596,  1199,   664,   311,    80,    90,
      90,    91,   402,   402,  1732,  1733,  1734,  1735,  1736,   123,
      64,   777,   778,   163,  1210,   107,  1483,   648,  1049,   123,
     677,   563,   405,  1212,   678,   501,  1837,  1049,   635,  1548,
     607,    86,   679,   820,    90,   652,    64,   674,   651,   591,
     158,  1162,   183,   184,    71,    72,    73,    74,    75,    76,
      77,    78,  1654,   158,  2088,   119,  1343,    71,    72,    73,
      74,    75,    76,    77,    78,    80,  1789,   648,   385,   483,
    1790,   563,  1566,  1134,    81,   483,  1580,   678,   369,  1854,
     779,   780,  1879,  1049,  2088,   101,  1049,  1823,   666,   101,
     406,   563,  1858,  1859,  1860,  1861,   515,   754,    86,    87,
      81,    91,   501,  1410,    83,    91,  2070,   698,   527,  1641,
    1642,    90,    90,    90,  1636,  1862,  1221,  1880,  2135,   123,
     527,   820,   709,  1881,  1863,  1942,    88,  1049,  1388,   820,
    1912,   785,   786,    91,   394,   114,    91,  1153,   963,   527,
     712,  1153,  1993,  1153,   385,    90,  1049,  2028,   123,   717,
     199,  1049,   718,   962,   369,    99,   100,   106,   219,   123,
      90,    91,   719,    90,    90,   280,    90,    91,    91,   754,
    2126,  2192,    90,   112,  1049,  2189,    90,    80,    90,    14,
      15,    16,    17,    18,   123,   962,   722,   838,   293,    99,
     100,   106,   773,   818,   285,   296,   299,   840,   754,   677,
     725,   300,    91,   678,   304,   726,   309,   112,   493,  1166,
      86,   679,  2198,  2151,   277,   290,  2199,  2155,  1701,   730,
      90,   787,  1701,   680,   788,    14,    15,    16,    17,    18,
     789,   501,  1082,  1083,  1084,  1085,   790,   527,  1295,   124,
    1075,  1076,  1077,  1162,   795,    64,    90,   491,  1686,  1040,
     754,   762,  1781,   763,   764,   765,  1703,  1483,  1483,  1483,
    1703,   963,  1671,  1483,   835,  1456,    19,   822,  1443,   823,
     385,  1447,  1295,  1051,  1052,  1451,   501,   666,   754,    90,
     369,  1370,  1371,   124,   766,   837,   229,   767,   768,  1049,
    1389,    64,   769,   770,   824,   501,   825,   501,   826,   107,
     827,   501,   501,   858,   501,  1819,  1820,  1821,    -3,    81,
    -497,   547,    54,    55,    56,    57,    58,    59,    60,    61,
     860,   501,  1468,  1469,  1470,  1385,  1386,   314,   624,  -496,
     548,    91,    91,  1846,  1846,  1846,   862,   549,  -165,  -165,
     876,   119,   -18,   107,   214,   754,    91,   875,    90,   884,
     152,   887,    90,    90,   152,    81,  -498,  1585,  1527,  1528,
    1602,  1603,   299,  1606,  1603,   550,   899,   551,   900,   101,
    1858,  1859,  1860,  1861,   527,   119,   901,    14,    15,    16,
      17,    18,   903,  1049,   904,   101,   905,   552,  1581,   906,
     501,   162,   907,  1862,  1609,  1603,  1123,  1595,   527,   527,
     347,   101,  1868,  1643,  1595,    91,  1123,  1655,    90,   299,
    1802,  1371,   913,   100,    91,  1919,  1371,   100,   100,  1920,
    1603,   114,   914,   553,  1608,  1929,  1049,  1991,  1992,   328,
     112,   100,  2006,  1603,   112,   112,  1686,   962,   908,   605,
      90,  1686,   934,    64,  2007,  1603,   935,    91,   112,  -621,
     558,  1947,  1948,  -619,   300,   114,   695,   944,   309,   602,
     527,   945,   404,   554,   946,   458,   458,    90,   123,   317,
     889,   949,    90,    90,    90,  2189,  2190,   952,  1838,   280,
    1600,  1601,   960,  1613,  1078,  1079,  1086,  1087,   691,   890,
     970,  1080,  1081,   974,  1622,  1705,   891,   963,   987,  1705,
    1705,  1756,  1758,   989,   735,    80,   555,    81,   603,  1847,
    1848,   680,   123,  1705,   289,  1005,    91,  1013,  1050,  1024,
     556,  1053,  1058,   643,   892,  1099,   893,   677,  1122,  1123,
    1130,   678,  1173,  1151,  1154,  1202,    90,  1177,    86,   679,
    1180,    90,    91,  1181,   405,  1182,   894,  1183,    90,  1184,
      90,  1185,  1186,   727,  1187,   501,   501,  1188,    90,   840,
     962,  1209,   158,   539,   183,   184,    71,    72,    73,    74,
      75,    76,    77,    78,  1211,  1213,    91,   527,   771,   772,
    -816,  1297,   895,   124,   527,  1224,   286,   297,  1225,  1312,
     402,  1858,  1859,  1860,  1861,  1226,  1303,  1324,   119,   771,
    1335,  1325,   119,   119,  1049,   735,  1326,  1327,   458,  1337,
     909,   501,  1342,  1344,  1862,   527,   119,  1341,  1346,  1345,
    1347,  1687,   896,  -194,   277,   290,  2016,  1348,    91,  1349,
    1040,  2059,   771,  1374,   152,    91,  1889,  1351,  1352,  1353,
      91,    91,    91,   107,  1354,  1355,  1688,   633,  1356,   527,
     152,  1358,   158,   101,   183,   184,    71,    72,    73,    74,
      75,    76,    77,    78,  1359,   897,   152,    90,  1360,    90,
    1365,   101,  1366,  1383,  1375,  1985,  1387,  1417,   114,   898,
    1390,  1394,   114,   114,  1395,   299,   842,  1396,   844,   547,
     161,  1397,  1401,  1402,  1406,  1407,   114,   861,  -123,  -123,
    -123,  -123,  -123,  -123,    91,   101,  1408,    90,   548,    91,
      90,  1409,  1420,  1686,  1422,   549,    91,  1423,    91,   527,
     527,  1424,  1426,  1434,   603,  1461,   527,  1499,  -817,   278,
     101,  1493,  1494,   493,  1497,  1498,  1507,  1508,    93,   527,
    1509,   160,  1511,   550,  1516,   551,   -22,  1521,  1520,   527,
    1049,   527,  1541,  1542,  1568,  1572,  1827,   750,  1595,  1591,
    2130,  1596,  2016,  1615,   527,   552,   527,   527,   527,  1573,
    1985,  1616,  1619,   602,  1578,  1630,   101,  1631,  1638,   205,
    1987,   194,     6,     7,     8,     9,    10,    11,    12,    13,
    1632,   100,  1634,  1639,  1603,  1644,   962,  1647,  1651,  1652,
    2153,   553,    93,   251,  1653,  1660,  1661,  1663,   112,  1687,
    1674,  1676,   123,    90,  1687,  1486,  1707,    91,  1722,   203,
     527,  1528,  1747,   559,   527,  1750,  1723,   558,  1725,   527,
      93,   606,  1751,  1250,  1688,    91,  1727,    91,  1740,  1688,
    1761,   554,  1748,   248,  1749,  1762,   276,  1764,   560,   298,
      93,  1774,    90,   206,    90,   501,   645,  1765,   501,   501,
     340,  1766,  1772,  1775,  1782,  2191,  1788,  1803,  1787,   124,
    1791,   207,   208,  1705,  1795,    91,  1805,  1806,    91,   483,
    1811,  2065,  1818,   527,   555,  1809,  1643,   909,   160,   280,
    1813,  1814,  1815,  1680,    93,   527,   917,   160,   556,   527,
     338,   346,   236,   124,   921,  1850,   101,  1074,    90,  1832,
    1851,  1827,  1827,   368,   527,  1855,  1857,  1886,   152,  1888,
    1479,  1902,  1903,   931,   289,  1913,    90,    90,   101,   107,
    1906,  1907,   101,   101,   939,  1910,   152,  1918,   465,   340,
     203,   203,  1928,   100,   546,   251,   101,  1923,  1926,  1927,
    1955,   160,   498,  1960,  1961,  1974,  1976,   276,   221,   527,
     112,  1981,  1990,   107,  1986,   340,  1995,  2000,  2001,  2003,
     152,  2005,   563,   100,   278,  2008,   119,  2009,  2010,   338,
    2014,    91,   458,   910,   248,   248,  2030,    90,  2045,  2038,
     112,  2062,  2051,  2032,  2043,   152,   527,  2073,  2074,  2055,
    2085,   527,  2109,  2094,  2063,   338,  2111,   100,   911,  2113,
    2117,  2065,   101,    93,  2119,  2065,  2065,  2121,   518,  2124,
      91,   340,  2061,  1827,   112,  1705,  2120,   527,   276,  2125,
     527,  2137,   527,  2141,   646,   340,  2139,  2149,   603,  2154,
    2158,   152,  2152,  2159,  2174,  2164,   750,  1146,  2175,   750,
    2173,   527,  2181,  2183,   750,  1705,   114,  2186,  2187,    90,
    2196,   338,  1915,   750,  2197,  2184,  1045,   346,    90,  2184,
    2200,  1827,  1088,   346,   338,   338,  2102,  1090,  1089,  1579,
     810,  2193,   750,   160,  1091,  1515,  1687,  1523,  1092,  1705,
    2116,  2182,  1709,  1978,    91,    91,  2077,  2131,   123,  2148,
    1827,  1731,  1971,  2128,  1898,   368,   681,   690,  2115,  1884,
    1885,  1688,  2054,  2156,  2114,  2188,   458,  1540,   458,   182,
     306,   368,   595,  2079,  2013,   368,   124,  1673,   119,  1571,
     124,   124,   123,  -122,  -122,  -122,  -122,  -122,  -122,   278,
     101,  1911,  1298,  1299,   124,  1827,  1827,    14,    15,    16,
      17,    18,  1046,  1172,  1538,    91,   458,   293,   119,   506,
    1763,  1479,  1479,  1479,   163,  1667,  1668,  1672,   465,   882,
    1357,   152,   972,   158,     3,   928,  1361,    71,    72,    73,
      74,    75,    76,    77,    78,  1018,   107,  1369,  1102,  1103,
     107,   107,   119,   152,  1817,  1104,     0,   152,   152,   848,
     559,     0,   465,     0,   107,   813,  1827,     0,   114,     0,
       0,   152,     0,   203,     0,    14,    15,    16,    17,    18,
       0,   458,     0,     0,     0,   560,     0,  2194,     0,     0,
       0,   160,     0,     0,     0,   498,  2201,     0,   114,   847,
       0,   690,   888,     0,  1208,     0,  1372,     0,     0,     0,
       0,     0,   160,     0,   251,     0,     0,     0,  1216,     0,
     910,     0,  1220,     0,     0,  1524,  1223,    14,    15,    16,
      17,    18,   114,     0,   465,     0,     0,   152,   340,     0,
       0,    64,   248,     0,   340,   911,     0,  1392,   278,     0,
       0,     0,     0,   158,   248,   183,   184,    71,    72,    73,
      74,    75,    76,    77,    78,   629,   101,     0,     0,   668,
       0,     0,   671,     0,     0,   694,     0,     0,   338,     0,
     465,   465,     0,     0,   338,   699,     0,   368,   671,   702,
       0,     0,   671,    64,     0,     0,  1418,     0,  1421,     0,
       0,     0,   956,    80,   340,    81,     0,     0,     0,     0,
    1425,     0,  1427,     0,     0,   123,     0,  1432,  1433,   123,
     123,     0,     0,     0,     0,   812,     0,  1440,  1525,   563,
     727,   200,     0,   123,     0,     0,    86,    87,     0,     0,
       0,     0,   338,     0,   338,     0,     0,    93,     0,     0,
       0,   160,     0,  1459,     0,    80,  1462,    81,     0,     0,
       0,     0,     0,   368,   498,   152,   690,     0,     0,     0,
       0,     0,   281,     0,   681,     0,     0,  1823,   681,     0,
       0,   563,     0,     0,   302,   305,     0,   368,    86,    87,
      14,    15,    16,    17,    18,  1362,     0,   690,     0,   668,
     368,     0,   671,     0,     0,     0,     0,     0,     0,   160,
       0,     0,     0,   771,     0,     0,     0,   465,   101,  1522,
     465,     0,   160,   160,     0,   465,     0,   281,     0,     0,
       0,     0,     0,     0,   465,     0,   158,   160,   160,   160,
      71,    72,    73,    74,    75,    76,    77,    78,   101,  1543,
       0,   157,  1586,  1587,  1588,     0,     0,     0,     0,  1589,
    1590,     0,     0,     0,   124,     0,     0,  1553,     0,  1554,
       0,  1555,     0,     0,     0,     0,     0,     0,  1564,     0,
     158,     0,   101,   281,    71,    72,    73,    74,    75,    76,
      77,    78,  1063,   498,   506,     0,     0,     0,     0,     0,
       0,     0,  1125,     0,   848,     0,     0,  1466,     0,   813,
     813,     0,     0,     0,     0,     0,     0,   465,     0,     0,
       0,  1490,   629,     0,   107,     0,     0,   293,     0,     0,
       0,   152,     0,   215,  1064,     0,     0,   368,   498,     0,
       0,  1512,   847,     0,   847,     0,     0,     0,   750,     0,
       0,     0,  1617,  1618,   281,   964,     0,   368,   671,   368,
       0,     0,     0,   368,   368,   368,   368,   977,     0,   340,
     671,   506,     0,     0,     0,     0,     0,     0,  1640,     0,
       0,     0,     0,   368,     0,  1645,     0,  1646,   281,     0,
       0,   997,     0,   281,   671,     0,     0,     0,   629,   281,
       0,     0,     0,     0,  1002,     0,     0,   671,     0,   338,
       0,     0,     0,     0,     0,     0,   124,     0,     0,     0,
       0,   340,     0,     0,     0,     0,     0,   629,     0,     0,
       0,   160,     0,   281,     0,     0,     0,     0,     0,   268,
       0,     0,     0,     0,     0,     0,   124,   465,     0,     0,
       0,     0,   368,     0,     0,     0,     0,     0,     0,     0,
       0,   338,   346,   160,     0,   215,     0,   465,     0,     0,
       0,     0,     0,     0,     0,   368,   107,  1319,     0,     0,
     124,     0,     0,   152,     0,   736,     0,     0,   681,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
     506,     0,     0,     0,     0,  1767,   107,     0,     0,     0,
       0,     0,  1771,   152,  1773,     0,     0,     0,     0,     0,
       0,     0,     0,   590,   158,   528,   245,   246,    71,    72,
      73,    74,    75,    76,    77,    78,     0,   160,   498,     0,
     107,   977,     0,     0,   671,   506,     0,   152,     0,   158,
       0,   245,   246,    71,    72,    73,    74,    75,    76,    77,
      78,     0,   629,     0,   632,     0,     0,   281,     0,     0,
       0,     0,   506,  1695,  1697,     0,   639,    80,   629,     0,
       0,  1125,   629,     0,     0,     0,   736,  1411,   848,   394,
    1810,   626,     0,   590,     0,   629,     0,     0,     0,  1679,
      83,     0,   813,     0,     0,     0,  1680,     0,     0,     0,
      86,    87,     0,     0,     0,   667,     0,   368,   368,    64,
       0,   847,    88,     0,  1759,     0,     0,     0,   847,     0,
       0,     0,     0,     0,     0,   964,     0,     0,   671,     0,
       0,     0,     0,     0,  1892,   123,     0,     0,     0,     0,
       0,   281,     0,   158,     0,   245,   246,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,   964,     0,     0,
     671,     0,     0,   368,   281,   123,     0,    19,     0,  1315,
     755,     0,   671,    81,     0,   694,     0,     0,   281,     0,
       0,     0,     0,     0,     0,     0,  1230,     0,     0,     0,
       0,   281,     0,  1410,    83,     0,     0,     0,     0,   123,
       0,   796,     0,     0,     0,   160,     0,     0,  1904,  1905,
       0,     0,     0,     0,   160,     0,    88,    58,    59,    60,
      61,     0,   281,   465,     0,     0,     0,     0,     0,   836,
       0,     0,   158,     0,   841,   506,    71,    72,    73,    74,
      75,    76,    77,    78,  1376,     0,   281,     0,  1377,   465,
    1378,     0,     0,   281,   867,   868,   340,   465,     0,   869,
     870,     0,   158,   873,   183,   184,    71,    72,    73,    74,
      75,    76,    77,    78,  1067,     0,     0,     0,     0,   886,
       0,   276,    93,    83,     0,     0,  1598,     0,     0,     0,
      14,    15,    16,    17,    18,     0,   338,     0,     0,   160,
       0,   915,     0,     0,     0,     0,   498,     0,   872,     0,
     528,     0,  1883,     0,     0,   158,  1068,   245,   246,    71,
      72,    73,    74,    75,    76,    77,    78,   126,   160,     0,
     126,     0,     0,     0,     0,     0,     0,   498,   919,     0,
       0,     0,   160,    80,     0,     0,     0,     0,     0,     0,
       0,   629,     0,     0,     0,   629,    64,  1627,     0,     0,
       0,     0,     0,     0,   629,   845,    83,     0,     0,   678,
       0,     0,     0,     0,   629,  2069,    86,   846,     0,   955,
       0,   629,     0,     0,     0,     0,     0,     0,    88,   680,
     158,   126,   245,   246,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,     0,     0,     0,   368,     0,   964,
     368,   368,   671,   368,   626,     0,   988,     0,    80,   126,
      81,     0,     0,     0,     0,     0,  1685,     0,     0,   629,
       0,     0,     0,   629,     0,   283,     0,   629,     0,   126,
     845,    83,     0,     0,   678,     0,     0,     0,     0,     0,
       0,    86,   846,     0,     0,   160,   160,   160,   160,     0,
     160,   160,     0,    88,     0,     0,  1681,   346,  1205,     0,
       0,     0,     0,   126,     0,     0,     0,   126,     0,     0,
     465,  1041,     0,   126,   465,   465,   126,     0,     0,     0,
     283,     0,     0,     0,     0,   465,     0,     0,   465,   226,
       0,   364,   126,     0,   396,     0,     0,     0,     0,     0,
     997,     0,     0,   671,     0,     0,     0,     0,   281,     0,
       0,     0,     0,     0,     0,     0,   276,   469,     0,     0,
       0,     0,   964,   281,     0,   671,     0,     0,     0,   498,
     126,   469,     0,     0,   506,   158,   283,   183,   184,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,     0,
       0,     0,     0,     0,   160,     0,   226,   681,     0,     0,
       0,     0,  2162,  1120,     0,  1121,     0,     0,  1101,     0,
       0,   841,     0,   126,     0,     0,     0,  2171,     0,     0,
       0,     0,     0,     0,  1118,     0,     0,     0,  1119,     0,
     126,     0,   126,     0,     0,     0,     0,   474,     0,  1165,
       0,     0,   126,     0,  1685,     0,     0,   283,  1174,  1685,
    1501,     0,  1176,   126,     0,     0,  1839,     0,  1685,     0,
       0,     0,    14,    15,    16,    17,    18,     0,   620,     0,
       0,   126,     0,   510,     0,     0,   126,     0,   126,     0,
       0,   283,   126,     0,  1681,  1824,   283,     0,     0,  1681,
     528,   465,   283,   872,     0,     0,  1681,   667,  1681,     0,
       0,     0,   126,     0,     0,     0,     0,     0,     0,     0,
       0,   629,   593,     0,     0,   629,     0,     0,     0,   629,
       0,   346,   160,  2150,   126,     0,   283,   126,    64,     0,
     194,     6,     7,     8,     9,    10,    11,    12,    13,     0,
     126,     0,     0,   158,   126,   392,   393,    71,    72,    73,
      74,    75,    76,    77,    78,  2172,     0,     0,     0,     0,
       0,     0,   158,     0,   245,   246,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,   506,     0,     0,     0,
       0,     0,   593,     0,     0,     0,     0,   469,     0,     0,
      80,     0,    81,     0,     0,     0,    84,  1350,   964,     0,
       0,   671,     0,     0,   676,     0,     0,   160,   394,     0,
       0,     0,  2057,    83,     0,  1943,   563,     0,  1685,     0,
       0,   469,     0,    86,    87,     0,     0,   629,     0,     0,
       0,     0,     0,     0,     0,    88,     0,     0,   158,     0,
    1824,  1824,    71,    72,    73,    74,    75,    76,    77,    78,
     126,     0,     0,     0,   469,  1681,     0,   158,  1681,     0,
     283,    71,    72,    73,    74,    75,    76,    77,    78,  1376,
     346,   126,     0,  1377,     0,  1378,     0,   281,  1379,     0,
       0,   629,  1379,     0,     0,     0,     0,     0,   465,    83,
     629,     0,   871,   469,   629,     0,     0,     0,     0,     0,
       0,   226,     0,   160,     0,     0,   281,  1399,    83,     0,
    1379,  1400,     0,   528,     0,     0,     0,     0,     0,     0,
       0,  1685,     0,     0,     0,     0,     0,     0,   126,     0,
    1415,     0,     0,     0,   676,     0,     0,  1416,     0,   469,
     469,     0,     0,     0,   283,     0,   126,     0,     0,     0,
       0,     0,  1824,     0,     0,     0,     0,     0,     0,     0,
     340,  1681,   126,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   474,     0,     0,     0,     0,     0,  1379,
       0,   283,   157,     0,     0,  1453,     0,     0,     0,  1454,
       0,     0,     0,  1455,   283,     0,     0,     0,   160,     0,
    2058,   346,     0,     0,   126,     0,   126,     0,     0,     0,
     126,     0,     0,     0,     0,     0,     0,     0,     0,   474,
     474,   396,   126,   469,     0,   283,     0,     0,     0,  1824,
       0,   796,     0,   126,   646,   340,     0,     0,     0,     0,
     160,     0,     0,     0,  1658,     0,   126,     0,   510,   283,
       0,     0,     0,   620,     0,     0,   283,     0,     0,   126,
       0,     0,     0,   281,     0,     0,     0,     0,   126,     0,
     160,     0,     0,     0,  2058,  2058,   469,     0,     0,   469,
       0,   126,   126,     0,   469,  1552,     0,     0,     0,     0,
       0,     0,     0,   469,     0,   340,   126,   126,   126,     0,
       0,     0,     0,     0,   160,     0,     0,  1983,     0,   158,
     671,   212,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,   281,     0,     0,   510,   999,     0,     0,     0,
       0,     0,     0,     0,     0,  2058,     0,     0,     0,   443,
       0,     0,     0,     0,     0,     0,     0,     0,   474,     0,
       0,     0,   469,     0,     0,     0,  1026,     0,     0,     0,
      83,     0,   474,   871,  1033,     0,     0,   109,   126,     0,
     164,     0,     0,     0,     0,     0,   469,     0,     0,     0,
       0,     0,   528,     0,   126,     0,     0,     0,   126,     0,
       0,  1379,     0,     0,     0,     0,   126,   469,     0,     0,
       0,   126,  1983,     0,   158,   671,   183,   184,    71,    72,
      73,    74,    75,    76,    77,    78,   126,     0,   126,     0,
       0,     0,   126,   126,   126,   126,     0,     0,     0,     0,
       0,   109,     0,     0,     0,     0,     0,  1648,     0,     0,
       0,  1649,   126,     0,     0,  1650,     0,     0,     0,     0,
       0,     0,     0,     0,   510,     0,   474,     0,     0,   220,
       0,     0,     0,     0,     0,  1503,     0,     0,     0,     0,
       0,  1206,     0,     0,     0,     0,     0,     0,     0,   291,
       0,     0,     0,     0,     0,     0,     0,   281,     0,     0,
     126,     0,     0,     0,     0,     0,     0,     0,     0,   510,
       0,     0,   126,     0,     0,     0,   469,     0,     0,     0,
       0,   126,     0,   325,     0,     0,     0,   330,   510,   528,
     510,   283,   126,   109,   510,   510,   469,   510,     0,     0,
       0,     0,     0,     0,   126,     0,  1321,   469,     0,     0,
       0,     0,   370,     0,   510,   629,     0,     0,     0,     0,
       0,     0,     0,  1768,     0,     0,     0,     0,     0,     0,
     721,     0,   724,     0,  1336,   443,   729,     0,     0,   476,
       0,     0,     0,     0,     0,   738,   739,     0,     0,     0,
     330,   504,     0,     0,     0,     0,     0,     0,     0,     0,
     443,   443,     0,     0,     0,     0,   126,   469,     0,     0,
       0,     0,     0,     0,  1229,   528,   474,  1804,     0,     0,
       0,   443,  1379,   510,   557,     0,  1807,  1379,  1379,  1379,
    1808,     0,   158,   325,   245,   246,    71,    72,    73,    74,
      75,    76,    77,    78,   581,     0,   281,     0,     0,   586,
     588,     0,   220,     0,   443,     0,     0,     0,     0,     0,
      80,   158,   325,   183,   184,    71,    72,    73,    74,    75,
      76,    77,    78,   325,     0,     0,   611,   126,     0,     0,
     613,   126,  1679,    83,     0,   614,   126,   126,     0,  1680,
     126,   625,     0,    86,    87,     0,   588,     0,   325,     0,
     126,     0,   638,     0,     0,    88,     0,   126,     0,     0,
       0,     0,     0,     0,   647,     0,   351,     0,     0,   654,
       0,     0,     0,     0,   352,   353,   354,   355,     0,     0,
     158,     0,   245,   246,    71,    72,    73,    74,    75,    76,
      77,    78,   126,     0,   669,     0,     0,   693,     0,     0,
       0,     0,     0,     0,     0,   126,     0,     0,    80,   126,
     700,     0,     0,   126,   700,   158,     0,   183,   184,    71,
      72,    73,    74,    75,    76,    77,    78,   281,     0,     0,
     247,    83,     0,     0,   126,     0,     0,     0,   510,   510,
       0,    86,    87,   126,     0,     0,     0,     0,     0,     0,
       0,     0,   469,    88,   528,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,   356,
       0,     0,  1379,     0,  1379,     0,     0,     0,   469,     0,
       0,     0,     0,     0,     0,     0,   469,   357,     0,     0,
       0,     0,     0,     0,   510,     0,     0,   158,     0,   245,
     246,    71,    72,    73,    74,    75,    76,    77,    78,     0,
     283,   126,     0,     0,     0,     0,     0,     0,     0,     0,
     330,     0,     0,     0,   669,    80,     0,    81,   126,     0,
       0,     0,     0,     0,     0,   469,     0,     0,     0,  1321,
       0,   330,     0,     0,     0,     0,     0,  2057,    83,     0,
       0,   563,     0,     0,     0,     0,     0,   126,    86,    87,
       0,     0,     0,     0,   126,     0,   469,     0,     0,     0,
      88,   126,     0,     0,     0,     0,     0,  2023,     0,   443,
     443,   443,   443,   443,   443,   443,   443,   443,   443,   443,
     443,   443,   443,   443,   443,   443,   443,   443,   625,     0,
       0,     0,     0,     0,     0,  2053,     0,     0,   474,     0,
       0,     0,     0,     0,     0,   999,   504,   421,     0,   422,
     423,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,   325,     0,     0,     0,   126,     0,     0,   126,
     126,     0,   126,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,   126,     0,     0,
       0,   126,     0,     0,   443,   126,     0,     0,     0,   756,
       0,     0,    84,   432,   625,     0,   109,  1659,     0,     0,
     965,     0,     0,     0,   126,   126,   126,   126,   126,   126,
     126,     0,   700,   980,     0,     0,   283,     0,     0,     0,
       0,     0,     0,   625,     0,    64,     0,   991,     0,   469,
       0,     0,     0,   469,   469,     0,   669,     0,    64,     0,
       0,  1000,     0,     0,   469,     0,     0,   469,     0,   700,
       0,  2129,     0,   120,     0,     0,     0,     0,   510,   158,
       0,   510,   510,    71,    72,    73,    74,    75,    76,    77,
      78,     0,   158,     0,     0,   283,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,    80,   469,    81,
       0,     0,     0,   126,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,  1778,     0,     0,    82,
      83,     0,     0,   126,     0,     0,     0,   120,     0,     0,
      86,    87,    82,    83,   474,     0,     0,     0,     0,     0,
       0,     0,    88,    86,    87,     0,     0,     0,     0,     0,
       0,     0,   504,     0,     0,    88,     0,   126,     0,     0,
       0,     0,     0,     0,     0,     0,   126,     0,   625,  1105,
     126,     0,   443,     0,     0,   292,     0,     0,   443,     0,
       0,     0,     0,     0,   625,     0,     0,     0,   625,   443,
       0,     0,     0,     0,     0,     0,   700,   980,     0,     0,
       0,   625,     0,  1131,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,   504,     0,   504,   120,
     469,     0,   504,   504,   370,   504,     0,     0,     0,   443,
       0,    64,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,   504,     0,     0,     0,     0,     0,     0,     0,
     283,   126,     0,   158,     0,   615,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   158,     0,   245,   246,    71,
      72,    73,    74,    75,    76,    77,    78,   505,   158,     0,
     617,   618,    71,    72,    73,    74,    75,    76,    77,    78,
     965,     0,     0,    80,     0,    81,     0,     0,     0,     0,
     474,     0,   625,     0,     0,     0,  1294,  1096,     0,     0,
       0,   504,     0,     0,     0,   247,    83,     0,     0,   120,
       0,     0,   965,     0,     0,     0,    86,    87,     0,     0,
       0,    84,     0,     0,   700,     0,   126,  1323,    88,     0,
       0,  1496,     0,     0,  1329,     0,     0,     0,   120,     0,
       0,     0,     0,  1510,     0,     0,     0,     0,     0,   120,
       0,     0,   612,     0,     0,     0,     0,    64,     0,     0,
       0,     0,   443,     0,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,   292,     0,
       0,     0,     0,     0,     0,     0,   330,   370,     0,   283,
       0,   158,     0,   245,   246,    71,    72,    73,    74,    75,
      76,    77,    78,     0,   469,     0,   158,   469,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,     0,    80,
     670,    81,   126,   292,   158,     0,   183,   184,    71,    72,
      73,    74,    75,    76,    77,    78,   670,     0,     0,     0,
     670,   337,    83,     0,     0,   443,     0,     0,     0,     0,
       0,     0,    86,    87,     0,     0,     0,   625,     0,    84,
       0,   625,     0,     0,    88,     0,   504,   504,     0,     0,
     625,     0,     0,     0,   516,     0,     0,     0,     0,     0,
     625,     0,     0,     0,   443,   443,   443,   625,     0,     0,
       0,   443,   443,     0,     0,     0,     0,     0,     0,     0,
     158,  2042,   283,     0,    71,    72,    73,    74,    75,    76,
      77,    78,  1376,     0,     0,   443,  1377,   126,  1378,     0,
     283,   158,   504,   183,   184,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,   625,     0,     0,     0,   625,
       0,     0,     0,   625,     0,     0,     0,     0,     0,     0,
     670,    83,     0,     0,  1798,     0,   443,   443,     0,   126,
       0,     0,     0,     0,   965,    64,     0,     0,     0,     0,
       0,   521,     0,  1480,     0,  2103,     0,     0,     0,     0,
       0,     0,  1294,     0,     0,     0,     0,     0,     0,   126,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   158,
       0,   245,   246,    71,    72,    73,    74,    75,    76,    77,
      78,   126,     0,     0,     0,     0,  1294,     0,     0,     0,
       0,  1752,  1753,   126,   373,     0,     0,    80,   158,    81,
     245,   246,    71,    72,    73,    74,    75,    76,    77,    78,
       0,  1539,   505,     0,     0,     0,     0,     0,     0,  1679,
      83,     0,     0,     0,     0,     0,    80,     0,   120,     0,
      86,    87,     0,     0,     0,   669,     0,     0,     0,     0,
       0,     0,    88,     0,   586,     0,     0,     0,  2057,    83,
       0,     0,   563,     0,     0,     0,     0,   965,     0,    86,
      87,     0,     0,     0,   625,     0,   370,     0,     0,   443,
     373,    88,   120,     0,     0,   158,   670,     0,     0,    71,
      72,    73,    74,    75,    76,    77,    78,  1376,   670,   505,
       0,  1377,     0,  1378,     0,   125,     0,     0,   158,   373,
     183,   184,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,   670,   797,   798,   799,   800,   801,   802,   803,
     804,   805,   806,   807,     0,   670,    83,     0,   230,  1800,
       0,     0,     0,     0,     0,     0,   504,     0,     0,   504,
     504,     0,   370,   158,     0,   245,   246,    71,    72,    73,
      74,    75,    76,    77,    78,   808,     0,   625,     0,   125,
       0,   625,     0,     0,     0,   625,     0,     0,     0,     0,
    1856,    80,     0,     0,     0,     0,     0,  1866,     0,     0,
       0,     0,     0,     0,  1480,  1480,  1480,   164,   588,     0,
       0,     0,     0,   337,    83,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    86,    87,     0,   294,  1891,  1704,
       0,     0,     0,  1704,  1704,     0,    88,     0,   505,     0,
       0,     0,     0,     0,     0,     0,     0,  1704,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,     0,
       0,   125,     0,     0,     0,     0,   443,     0,     0,     0,
     373,   125,     0,     0,   373,     0,     0,     0,     0,     0,
       0,     0,   670,   505,     0,     0,     0,   373,   370,     0,
     377,     0,     0,   625,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,   373,     0,     0,   238,   373,   373,
     505,   373,   158,   965,   183,   184,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,   373,   507,
       0,     0,  1945,  1946,     0,     0,     0,     0,     0,  1956,
       0,     0,     0,     0,     0,     0,     0,   625,     0,     0,
       0,     0,  1970,     0,     0,     0,   625,     0,     0,     0,
     625,     0,  1979,     0,  1980,     0,     0,     0,     0,     0,
       0,   125,     0,     0,     0,     0,   670,  1994,     0,  1996,
    1997,  1998,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,   120,     0,     0,     0,     0,   373,     0,     0,
     125,     0,     0,     0,     0,     0,     0,     0,   670,     0,
       0,   125,     0,     0,     0,     0,  2015,  1841,     0,     0,
     670,     0,     0,   292,     0,     0,     0,     0,     0,   377,
       0,     0,     0,  2024,     0,     0,   125,  2029,     0,     0,
     294,  1853,  2034,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   443,     0,     0,     0,     0,     0,     0,
     414,     0,     0,   415,     0,   416,   417,     0,   418,     0,
       0,     0,   672,   505,     0,   294,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   419,  2080,     0,   672,     0,
       0,     0,   672,     0,     0,     0,     0,   443,  2089,     0,
       0,     0,  2092,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   420,   421,  2108,   422,   423,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     424,   425,   411,     0,   426,   427,   428,     0,   429,   430,
       0,     0,     0,   373,     0,     0,    80,   373,     0,     0,
    1935,     0,   373,   373,     0,     0,   373,     0,     0,     0,
       0,   443,  2136,     0,     0,     0,   373,     0,   431,     0,
       0,    84,   432,   373,     0,     0,     0,     0,   433,    86,
      87,   434,   435,   436,   437,     0,     0,   443,     0,   443,
       0,     0,     0,     0,  1966,     0,     0,  1704,     0,  2157,
       0,     0,   672,     0,  2160,     0,     0,     0,   373,     0,
       0,     0,  1984,    14,    15,    16,    17,    18,     0,     0,
       0,   373,     0,     0,     0,   373,     0,   443,     0,   373,
    2178,     0,     0,  2180,     0,  2160,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     670,     0,     0,     0,  2180,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,     0,   377,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   443,     0,   507,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,  1984,     0,     0,
     125,     0,     0,   158,     0,   245,   246,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,   292,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,  1704,
       0,   670,   377,     0,   125,     0,     0,     0,   672,     0,
       0,     0,     0,   247,    83,     0,     0,     0,     0,     0,
     672,   507,     0,   670,    86,    87,     0,     0,     0,  1704,
     373,   377,   505,     0,  2118,     0,    88,     0,     0,     0,
       0,     0,     0,     0,   672,     0,     0,     0,     0,     0,
       0,   625,     0,     0,     0,     0,     0,   672,     0,     0,
       0,     0,     0,  1704,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   131,     0,     0,   131,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,   373,   373,     0,   373,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,   373,     0,     0,
       0,   373,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   131,
     507,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   130,     0,     0,   130,   377,     0,     0,     0,
       0,     0,     0,     0,     0,   120,     0,   131,     0,   120,
     120,     0,   377,     0,     0,     0,   377,     0,     0,     0,
       0,     0,     0,   120,   672,   507,     0,   131,     0,   377,
       0,     0,     0,     0,     0,     0,   307,     0,     0,     0,
       0,     0,     0,     0,   377,     0,   377,     0,     0,     0,
     377,   377,   507,   377,     0,     0,   130,     0,     0,     0,
       0,   131,     0,     0,   505,   131,     0,     0,     0,   373,
     377,   131,     0,     0,   131,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   130,     0,     0,     0,     0,   670,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   130,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   131,     0,     0,   672,     0,
       0,     0,     0,   373,     0,     0,     0,     0,   131,     0,
     377,     0,   373,     0,   125,     0,   373,     0,   130,   377,
       0,     0,   130,     0,     0,     0,     0,     0,   130,     0,
     672,   130,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   672,     0,     0,   294,     0,     0,     0,     0,
       0,   131,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   131,     0,
     131,     0,   130,     0,     0,     0,   131,     0,     0,     0,
     131,     0,     0,     0,     0,   130,     0,     0,     0,     0,
       0,   131,     0,     0,     0,     0,     0,   292,     0,     0,
       0,     0,     0,     0,     0,   507,     0,     0,     0,     0,
       0,     0,     0,     0,   131,     0,   131,     0,     0,     0,
     131,     0,     0,     0,     0,     0,     0,     0,   130,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     131,     0,     0,     0,     0,   130,     0,   130,     0,     0,
       0,     0,     0,   130,     0,     0,     0,   130,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,   130,     0,
       0,     0,     0,     0,     0,   377,     0,     0,     0,   377,
       0,     0,     0,     0,   377,   377,     0,     0,   377,     0,
       0,   130,     0,   130,     0,     0,     0,   130,   377,     0,
       0,     0,     0,     0,     0,   377,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   130,     0,     0,
       0,     0,     0,     0,     0,   131,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     377,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   377,     0,     0,     0,   377,     0,   131,
       0,   377,   158,   120,   245,   246,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,   670,     0,
       0,     0,   672,     0,     0,     0,     0,     0,   131,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
     125,     0,   130,     0,     0,     0,     0,     0,     0,   131,
       0,     0,   337,    83,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    86,    87,     0,     0,     0,     0,     0,
       0,   131,     0,     0,   125,    88,   130,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   294,
       0,     0,     0,   198,     0,   130,     0,     0,     0,     0,
       0,     0,     0,   670,     0,     0,     0,   131,   131,     0,
       0,     0,     0,   672,     0,     0,   130,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     131,     0,     0,     0,   279,   672,     0,     0,   130,     0,
       0,     0,   377,     0,   507,   120,   301,     0,   308,     0,
     310,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   135,     0,     0,   135,     0,     0,
       0,     0,     0,     0,   131,   120,     0,     0,     0,     0,
       0,     0,     0,     0,   130,   130,     0,     0,     0,   279,
       0,     0,   308,   310,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   130,     0,   120,
       0,     0,     0,     0,   377,     0,     0,   377,   377,     0,
     377,     0,     0,     0,     0,     0,     0,     0,   135,     0,
       0,     0,     0,     0,     0,   377,   131,     0,     0,   377,
       0,     0,     0,   377,   131,   279,     0,   131,     0,   131,
     131,   130,   131,     0,     0,     0,   135,     0,     0,     0,
       0,   131,     0,     0,   131,   131,   131,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   135,     0,     0,    14,
      15,    16,    17,    18,     0,     0,     0,   125,     0,     0,
       0,   125,   125,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   125,     0,     0,     0,     0,
     135,     0,     0,   130,   135,     0,   279,     0,   308,   310,
     135,   130,     0,   135,   130,     0,   130,   130,     0,   130,
       0,     0,     0,     0,     0,     0,     0,     0,   130,     0,
       0,   130,   130,   130,   131,    64,   507,     0,     0,     0,
     279,   377,     0,     0,     0,   279,     0,     0,     0,     0,
       0,   279,     0,     0,   135,     0,     0,     0,     0,     0,
       0,   672,     0,     0,     0,     0,     0,   135,     0,   158,
       0,   245,   246,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,     0,   279,     0,     0,     0,     0,
     696,     0,   310,     0,     0,   377,     0,    80,     0,    81,
       0,     0,     0,     0,   377,   180,     0,     0,   377,     0,
     135,   130,     0,     0,     0,     0,     0,     0,     0,  1679,
      83,     0,     0,     0,     0,     0,     0,   135,     0,   135,
      86,    87,     0,   180,     0,   135,     0,   734,     0,   135,
       0,     0,    88,     0,     0,     0,     0,     0,     0,     0,
     135,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   131,     0,     0,     0,     0,     0,
       0,     0,     0,   135,     0,   135,     0,     0,     0,   135,
       0,     0,   180,     0,   131,     0,     0,     0,     0,   294,
       0,   279,     0,     0,     0,   180,     0,   180,     0,   135,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   279,
       0,   696,   310,     0,     0,     0,     0,     0,   180,     0,
     399,     0,     0,     0,     0,     0,     0,     0,   734,     0,
       0,   130,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   131,   399,     0,     0,     0,     0,
       0,   130,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   279,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   135,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   279,     0,   180,     0,
       0,   279,   180,   279,   178,   180,   180,     0,     0,   180,
       0,     0,   180,   180,     0,   180,     0,   180,   135,     0,
       0,     0,     0,     0,     0,     0,   279,     0,   279,   279,
       0,   130,     0,     0,     0,     0,     0,     0,     0,     0,
     279,     0,     0,     0,     0,     0,     0,   135,     0,     0,
       0,     0,  1967,   279,     0,   125,     0,     0,     0,     0,
       0,     0,   279,     0,     0,     0,     0,     0,   135,     0,
     672,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   312,     0,     0,   279,     0,   696,   310,   180,     0,
     135,   180,     0,     0,   318,     0,   319,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   279,   696,
       0,     0,     0,     0,     0,   279,   180,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,   180,     0,     0,     0,     0,   135,   135,     0,     0,
       0,   131,     0,     0,     0,     0,     0,     0,     0,     0,
     131,     0,     0,     0,     0,     0,     0,     0,     0,   135,
       0,     0,     0,     0,     0,   672,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   131,     0,     0,     0,
       0,     0,     0,     0,   131,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   135,   568,   569,     0,   125,   573,   131,
       0,   576,   577,     0,   579,     0,   580,     0,   130,     0,
       0,     0,     0,     0,     0,     0,   131,   130,     0,     0,
       0,     0,     0,     0,     0,     0,   180,   125,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   130,     0,     0,     0,     0,     0,   377,
       0,   130,     0,     0,     0,   135,     0,     0,     0,   131,
       0,   125,     0,   135,     0,     0,   135,     0,   135,   135,
       0,   135,     0,     0,     0,     0,   130,     0,     0,     0,
     135,     0,     0,   135,   135,   135,   399,     0,     0,     0,
       0,     0,     0,   130,     0,     0,     0,     0,     0,     0,
       0,     0,   180,     0,     0,   665,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   186,   189,     0,     0,
     697,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   130,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   239,     0,     0,     0,     0,
       0,     0,     0,   135,     0,     0,     0,     0,     0,     0,
     279,     0,   131,   131,   131,   131,   131,   131,   131,     0,
       0,     0,     0,     0,     0,   279,   399,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   279,   131,     0,     0,
       0,   131,   131,     0,     0,   279,   332,     0,     0,   333,
       0,     0,   131,     0,     0,   131,     0,     0,     0,     0,
       0,     0,     0,     0,   358,   829,   180,   180,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,   180,     0,   180,   130,
     130,   130,   130,   130,   130,   130,   407,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   130,     0,     0,     0,   130,   130,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   130,
     538,     0,   130,   135,     0,     0,     0,     0,     0,     0,
       0,   912,     0,     0,     0,     0,     0,     0,     0,     0,
     279,     0,     0,   135,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   239,     0,     0,     0,   279,     0,     0,     0,
       0,   599,   600,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   186,     0,     0,     0,     0,   180,   180,     0,
       0,     0,     0,     0,   180,     0,     0,   186,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   131,     0,
       0,     0,     0,   135,     0,     0,     0,     0,     0,   180,
       0,     0,   180,   180,     0,   180,     0,   180,   180,     0,
       0,     0,   649,     0,     0,     0,     0,     0,     0,   131,
     653,   655,     0,     0,     0,   662,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   994,   995,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,     0,   180,  1004,     0,  1006,   180,     0,
       0,     0,     0,     0,   358,   130,     0,   358,     0,     0,
     407,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   130,     0,     0,     0,
       0,     0,     0,     0,   131,     0,     0,     0,     0,   279,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   279,     0,
     180,     0,     0,     0,   279,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   239,  1107,  1108,     0,     0,
       0,     0,     0,  1112,     0,     0,     0,   864,   865,     0,
     135,   130,     0,     0,     0,   131,     0,     0,     0,   135,
       0,     0,     0,     0,     0,     0,     0,     0,  1135,     0,
       0,  1138,  1139,     0,  1142,     0,  1144,  1145,     0,     0,
       0,     0,     0,     0,     0,   135,     0,     0,     0,     0,
       0,     0,     0,   135,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   135,  1190,
       0,     0,     0,  1194,     0,     0,     0,  1198,     0,     0,
       0,     0,   130,     0,     0,   135,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   279,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   135,     0,
       0,     0,     0,     0,     0,     0,   959,     0,     0,     0,
       0,     0,     0,     0,     0,   358,   180,     0,   180,     0,
       0,   180,     0,     0,   180,     0,     0,   131,   180,  1330,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   279,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   131,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,   384,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   131,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   130,     0,     0,     0,     0,   494,
     384,   135,   135,   135,   135,   135,   135,   135,     0,     0,
     414,  1069,     0,   415,     0,   416,   417,     0,   418,     0,
       0,     0,     0,     0,   130,     0,   135,     0,     0,     0,
     135,   135,     0,     0,     0,   419,   566,     0,     0,     0,
       0,   135,     0,   566,   135,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,     0,   130,     0,
    1330,     0,     0,     0,     0,   420,   421,     0,   422,   423,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     424,   425,   411,     0,   426,   427,   428,     0,   429,   430,
       0,     0,     0,     0,     0,     0,    80,     0,     0,   279,
       0,     0,     0,  1148,     0,  1437,     0,  1439,     0,     0,
    1442,     0,     0,  1446,  1164,     0,     0,  1450,   431,     0,
       0,    84,   432,     0,     0,   566,   180,     0,   433,    86,
      87,   434,   435,   436,   437,     0,     0,   180,     0,     0,
     180,  1019,   180,   180,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   384,   682,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   703,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1232,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   135,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   279,     0,   135,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   279,     0,
       0,   180,     0,   566,     0,  1558,  1339,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     566,   831,     0,   566,   834,     0,     0,     0,     0,     0,
       0,     0,     0,   384,     0,     0,     0,   682,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     494,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   135,     0,  1612,     0,     0,     0,     0,
     279,   566,     0,     0,     0,   566,  1621,     0,     0,  1625,
       0,  1628,  1629,     0,     0,     0,     0,     0,     0,     0,
     180,   749,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   279,   310,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   384,     0,     0,   180,   279,
       0,     0,     0,   467,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   499,     0,     0,
       0,     0,     0,     0,   135,     0,     0,     0,     0,     0,
     180,     0,     0,   529,     0,   529,   180,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   566,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1746,   978,   384,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   682,     0,     0,     0,   682,     0,     0,     0,
       0,     0,     0,   996,     0,   384,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1500,  1502,  1504,     0,
       0,     0,   180,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   923,   925,     0,     0,     0,     0,     0,
       0,     0,     0,   279,     0,     0,     0,   644,     0,     0,
    1526,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,   180,     0,     0,
       0,  1232,     0,     0,     0,     0,     0,  1545,     0,  1625,
       0,  1546,     0,     0,     0,     0,   135,     0,     0,     0,
       0,   180,   180,     0,     0,     0,     0,     0,     0,   399,
       0,     0,     0,     0,   180,     0,     0,  1812,     0,     0,
       0,   384,     0,     0,     0,     0,   135,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   566,   566,     0,
       0,     0,     0,   225,     0,     0,     0,     0,   566,  1114,
       0,   566,  1117,     0,     0,     0,     0,     0,     0,   287,
     135,     0,     0,     0,     0,   978,   384,     0,     0,     0,
     682,     0,   682,   682,     0,     0,     0,     0,     0,   682,
     749,     0,     0,   749,     0,   384,     0,   384,   749,     0,
       0,   384,   384,   384,   384,     0,     0,   749,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   180,     0,
     225,   384,     0,   566,   348,     0,   749,   566,     0,     0,
       0,     0,     0,     0,   566,  1192,   389,     0,   566,  1196,
       0,  1900,   566,  1200,     0,     0,     0,     0,     0,  1203,
       0,     0,     0,     0,     0,     0,     0,     0,   529,     0,
       0,   225,  1094,     0,   529,     0,     0,     0,     0,   467,
       0,     0,     0,     0,     0,   514,     0,  1691,  1692,     0,
     520,     0,     0,     0,     0,  1924,  1925,     0,     0,   180,
     384,   566,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1939,  1940,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1944,     0,     0,   682,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   225,     0,   180,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   287,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1779,     0,     0,     0,     0,
       0,   954,     0,     0,     0,   494,   384,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   520,     0,     0,   499,
       0,     0,     0,     0,     0,     0,   225,     0,     0,     0,
       0,     0,   990,     0,     0,     0,     0,  2012,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   675,     0,
     692,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     566,     0,     0,     0,     0,     0,     0,     0,     0,  1023,
       0,     0,     0,     0,     0,   384,   384,     0,     0,   682,
     682,     0,  1034,     0,     0,     0,   682,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1849,     0,     0,  1055,  1057,  2076,     0,
    1059,   753,  1061,     0,     0,     0,     0,     0,  1023,     0,
    1071,  1023,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   384,     0,     0,     0,   566,  1444,     0,   566,  1448,
       0,     0,   566,  1452,     0,   225,     0,     0,  1098,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1100,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1109,     0,     0,     0,     0,     0,   675,     0,
       0,     0,     0,     0,   859,     0,     0,     0,   499,     0,
       0,     0,     0,  1098,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   225,     0,     0,
       0,     0,     0,     0,     0,  1168,     0,     0,   529,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   225,   225,     0,     0,  1204,     0,     0,
     514,     0,     0,     0,   384,     0,     0,     0,     0,     0,
     682,  1560,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   384,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   467,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1320,  1322,     0,     0,     0,     0,     0,     0,
     499,     0,     0,     0,     0,     0,     0,   514,     0,   982,
     566,  1614,   749,     0,     0,     0,     0,     0,     0,     0,
       0,   566,  1623,     0,   682,     0,     0,     0,     0,     0,
     675,     0,     0,     0,     0,   384,     0,     0,   384,   384,
       0,   384,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   225,  1098,     0,     0,     0,     0,     0,     0,
     225,  1364,     0,   753,     0,   753,   225,     0,   225,     0,
    1023,     0,     0,     0,     0,     0,     0,   753,     0,     0,
     753,   753,   753,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   529,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   514,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     225,     0,     0,     0,     0,     0,     0,   384,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   514,     0,     0,   529,     0,  1436,     0,     0,     0,
       0,     0,     0,     0,     0,   682,     0,     0,     0,     0,
     514,     0,   514,     0,     0,     0,   514,   514,   389,   514,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   514,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1698,  1706,     0,     0,  1698,  1717,     0,
       0,     0,     0,  1724,     0,     0,     0,  1728,     0,  1730,
       0,  1717,     0,     0,     0,     0,     0,     0,  1513,  1513,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   566,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   514,     0,     0,     0,     0,
     566,     0,     0,     0,     0,   348,     0,     0,     0,     0,
     225,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     859,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1556,     0,     0,     0,     0,
       0,  1565,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1023,     0,     0,
       0,     0,   499,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     529,   389,     0,  1594,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1055,     0,
       0,     0,     0,     0,     0,  1816,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   414,     0,     0,   415,     0,   416,   417,     0,   418,
       0,     0,     0,     0,     0,     0,     0,     0,   566,   566,
       0,     0,     0,     0,     0,     0,   419,     0,     0,     0,
    1852,     0,     0,     0,   566,     0,     0,     0,     0,     0,
     514,   514,     0,     0,     0,     0,     0,     0,     0,     0,
    1872,  1874,  1656,  1657,     0,     0,   420,   421,     0,   422,
     423,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   424,   425,   411,     0,   426,   427,   428,  1023,   429,
     430,  1894,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,   514,   529,     0,     0,
     467,     0,  1710,  1711,  1712,  1713,     0,     0,     0,   431,
    1873,     0,    84,   432,     0,     0,     0,     0,     0,   433,
      86,    87,   434,   435,   436,   437,     0,     0,     0,     0,
     566,     0,     0,     0,     0,     0,     0,     0,   566,     0,
       0,     0,     0,     0,  1057,     0,     0,   753,     0,     0,
       0,     0,     0,  1769,  1770,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1784,     0,     0,     0,     0,     0,     0,     0,   566,     0,
       0,     0,   753,   529,     0,  1954,     0,  1055,     0,     0,
       0,     0,     0,  1957,     0,  1959,     0,     0,  1963,  1969,
       0,  1717,     0,   566,  2078,     0,  1975,   566,     0,     0,
       0,     0,     0,     0,   287,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   225,     0,     0,     0,     0,     0,     0,   675,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   566,   566,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   467,     0,     0,     0,
     389,     0,  1840,     0,     0,   753,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2040,     0,     0,     0,     0,
       0,     0,  2047,  2049,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   566,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2071,     0,     0,     0,     0,
       0,     0,     0,  1887,     0,     0,     0,     0,     0,     0,
     514,     0,     0,   514,   514,     0,   389,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2093,     0,  2096,     0,     0,  2098,  2100,     0,     0,
       0,     0,   529,  2105,  2107,     0,     0,     0,     0,     0,
    1914,     0,     0,  1916,     0,     0,     0,     0,   753,   753,
     753,     0,     0,   753,   753,     0,  2179,     0,     0,     0,
     520,     0,     0,     0,     0,     0,     0,     0,     0,  1930,
       0,     0,     0,  1495,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   225,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2143,  2145,  2147,     0,     0,     0,     0,     0,     0,     0,
     414,     0,     0,   415,     0,   416,   417,     0,   418,   287,
       0,     0,     0,     0,     0,     0,     0,     0,  2166,  2168,
    2170,     0,   389,  1235,     0,   419,  1237,     0,  1238,  -253,
    -253,  1239,  1240,  1241,  1242,  1243,  1244,  1245,  1246,  1247,
    1248,  1249,  1250,  -352,  -352,  1251,  1252,  1253,  1254,  1255,
    1256,  1257,     0,  1258,     0,   420,   421,     0,   523,   423,
    1259,  1260,    71,    72,    73,    74,    75,    76,    77,    78,
     424,   425,   411,  1261,   426,   427,   428,     0,   429,   430,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,  2179,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -253,  1262,     0,
    1495,    84,   432,     0,     0,     0,   316,     0,   433,    86,
      87,   434,   435,   436,   437,     0,     0,     0,     0,     0,
       0,     0,     0,  -193,  1023,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   225,     0,     0,   414,     0,     0,
     415,     0,   416,   417,     0,   418,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1235,     0,   419,  1237,   287,  1238,  -254,  -254,  1239,  1240,
    1241,  1242,  1243,  1244,  1245,  1246,  1247,  1248,  1249,  1250,
    -352,  -352,  1251,  1252,  1253,  1254,  1255,  1256,  1257,     0,
    1258,     0,   420,   421,     0,   523,   423,  1259,  1260,    71,
      72,    73,    74,    75,    76,    77,    78,   424,   425,   411,
    1261,   426,   427,   428,     0,   429,   430,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1890,     0,     0,     0,     0,
       0,     0,     0,     0,  -254,  1262,     0,     0,    84,   432,
     753,     0,  1495,   316,     0,   433,    86,    87,   434,   435,
     436,   437,     0,     0,     0,     0,     0,     0,     0,     0,
    -193,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
       0,     0,   415,     0,   416,   417,     0,   418,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1235,   287,   419,  1237,     0,  1238,     0,     0,
    1239,  1240,  1241,  1242,  1243,  1244,  1245,  1246,  1247,  1248,
    1249,  1250,  -352,  -352,  1251,  1252,  1253,  1254,  1255,  1256,
    1257,     0,  1258,     0,   420,   421,   753,   523,   423,  1259,
    1260,    71,    72,    73,    74,    75,    76,    77,    78,   424,
     425,   411,  1261,   426,   427,   428,     0,   429,   430,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1262,     0,     0,
      84,   432,     0,     0,     0,   316,     0,   433,    86,    87,
     434,   435,   436,   437,     0,     0,     0,     0,     0,     0,
       0,     0,  -193,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   753,     0,     0,   520,     4,   194,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    1234,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   414,     0,    52,
     415,    53,   416,   417,     0,   418,    54,    55,    56,    57,
      58,    59,    60,    61,    62,     0,     0,     0,    63,     0,
    1235,    64,  1236,  1237,     0,  1238,     0,     0,  1239,  1240,
    1241,  1242,  1243,  1244,  1245,  1246,  1247,  1248,  1249,  1250,
    -352,  -352,  1251,  1252,  1253,  1254,  1255,  1256,  1257,     0,
    1258,     0,   420,   421,    67,   523,   423,  1259,  1260,    71,
      72,    73,    74,    75,    76,    77,    78,   424,   425,   411,
    1261,   426,   427,   428,     0,   429,   430,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -3,  1262,     0,     0,    84,  1263,
       0,     0,     0,   316,     0,   433,    86,    87,   434,   435,
     436,   437,     0,     0,     0,     0,     0,     0,     0,     0,
    -193,     4,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,  1234,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   414,     0,    52,   415,    53,   416,   417,
       0,   418,    54,    55,    56,    57,    58,    59,    60,    61,
      62,     0,     0,     0,    63,     0,  1235,    64,  1236,  1237,
       0,  1238,     0,     0,  1239,  1240,  1241,  1242,  1243,  1244,
    1245,  1246,  1247,  1248,  1249,  1250,  -352,  -352,  1251,  1252,
    1253,  1254,  1255,  1256,  1257,     0,  1258,     0,   420,   421,
      67,   523,   423,  1259,  1260,    71,    72,    73,    74,    75,
      76,    77,    78,   424,   425,   411,  1261,   426,   427,   428,
       0,   429,   430,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1262,     0,     0,    84,  1263,     0,     0,     0,   316,
       0,   433,    86,    87,   434,   435,   436,   437,     0,     0,
       0,     0,     0,     0,     0,     0,  -193,     4,   194,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   414,
       0,    52,   415,    53,   416,   417,     0,   418,    54,    55,
      56,    57,    58,    59,    60,    61,    62,     0,     0,     0,
      63,     0,     0,    64,   419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   420,   421,    67,   422,   423,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   424,
     425,   411,     0,   426,   427,   428,     0,   429,   430,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1710,  1711,  1712,  1713,     0,     0,     0,   431,  1714,  1715,
      84,  1263,     0,     0,     0,     0,     0,   433,    86,    87,
     434,   435,   436,   437,     0,     0,     0,     0,     0,     0,
       0,     0,  1716,     4,   194,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   414,     0,    52,   415,    53,
     416,   417,     0,   418,    54,    55,    56,    57,    58,    59,
      60,    61,    62,     0,     0,     0,    63,     0,     0,    64,
     419,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     420,   421,    67,   422,   423,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   424,   425,   411,     0,   426,
     427,   428,     0,   429,   430,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1710,  1711,  1712,  1713,
       0,     0,     0,   431,  1714,     0,    84,  1263,     0,     0,
       0,     0,     0,   433,    86,    87,   434,   435,   436,   437,
       0,     0,     0,     0,     0,     0,     0,     0,  1716,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,    62,     0,
       0,     0,    63,     0,     0,    64,    65,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    66,     0,     0,     0,    67,    68,
       0,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,    79,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    82,
      83,     0,    84,    85,     0,     0,     0,     0,     0,     0,
      86,    87,   194,     6,     7,     8,     9,    10,    11,    12,
      13,     0,    88,     0,    89,     0,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,   269,   270,     0,   271,    52,     0,    53,     0,     0,
     272,     0,     0,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,   274,   194,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -500,  -500,     0,  -500,    52,
       0,    53,     0,     0,  -500,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -475,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   158,  -475,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,    83,     0,    84,   275,
       0,     0,     0,  -835,     0,     0,    86,    87,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    88,   274,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,  -500,
    -500,     0,  -500,    52,     0,    53,     0,     0,  -500,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   158,
       0,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    82,
      83,     0,    84,   275,     0,     0,     0,     0,     0,     0,
      86,    87,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    88,   194,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   365,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,     0,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,    81,   623,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1124,    83,  -700,    84,   678,     0,     0,     0,
       0,     0,     0,    86,    87,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    88,   194,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -500,  -500,     0,  -500,    52,
       0,    53,     0,     0,  -500,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   158,     0,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,    83,     0,    84,   275,
       0,     0,     0,  -839,     0,     0,    86,    87,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    88,   194,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -500,  -500,
       0,  -500,    52,     0,    53,     0,     0,  -500,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   158,     0,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    82,    83,
       0,    84,   275,     0,     0,     0,     0,     0,     0,    86,
      87,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    88,     4,   194,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   414,     0,    52,   415,    53,   416,
     417,     0,   418,    54,    55,    56,    57,    58,    59,    60,
      61,    62,     0,     0,     0,    63,     0,     0,    64,   419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   420,
     421,    67,   422,   423,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   424,   425,   411,     0,   426,   427,
     428,     0,   429,   430,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   431,     0,  1708,    84,  1263,     0,     0,     0,
       0,     0,   433,    86,    87,   434,   435,   436,   437,     4,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   414,     0,    52,   415,    53,   416,   417,     0,   418,
      54,    55,    56,    57,    58,    59,    60,    61,    62,     0,
       0,     0,    63,     0,     0,    64,   419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   420,   421,    67,   422,
     423,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   424,   425,   411,     0,   426,   427,   428,     0,   429,
     430,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   431,
       0,     0,    84,  1263,     0,     0,     0,     0,     0,   433,
      86,    87,   434,   435,   436,   437,   194,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   414,     0,    52,
     415,    53,   416,   417,     0,   418,   365,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   419,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   420,   421,     0,   422,   423,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   424,   425,   411,
       0,   426,   427,   428,     0,   429,   430,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   431,     0,     0,    84,   496,
       0,     0,     0,     0,     0,   433,   497,    87,   434,   435,
     436,   437,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   414,     0,    52,   415,    53,   416,   417,
       0,   418,   365,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   419,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   420,   421,
       0,   422,   423,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   424,   425,   411,     0,   426,   427,   428,
       0,   429,   430,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   431,     0,     0,    84,  1317,     0,     0,     0,     0,
       0,   433,  1318,    87,   434,   435,   436,   437,   194,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   414,
       0,    52,   415,    53,   416,   417,     0,   418,   365,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   420,   421,     0,   422,   423,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   424,
     425,   411,     0,   426,   427,   428,     0,   429,   430,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   431,     0,     0,
      84,   843,     0,     0,     0,     0,     0,   433,   497,    87,
     434,   435,   436,   437,   194,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   414,     0,    52,   415,    53,
     416,   417,     0,   418,   365,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     419,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     420,   421,     0,   422,   423,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   424,   425,   411,     0,   426,
     427,   428,     0,   429,   430,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   431,     0,     0,    84,   432,     0,     0,
       0,     0,     0,   433,    86,    87,   434,   435,   436,   437,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   414,     0,    52,   415,    53,   416,   417,     0,   418,
     365,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   420,   421,     0,   422,
     423,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   424,   425,   411,     0,   426,   427,   428,     0,   429,
     430,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   431,
       0,     0,    84,   843,     0,     0,     0,     0,     0,   433,
      86,    87,   434,   435,   436,   437,  2022,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,     0,    -2,     0,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,
       0,    -2,     0,     0,    -2,     0,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,    -2,
      -2,  2052,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
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
       0,     0,    -2,     0,   961,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,  -499,  -499,     0,  -499,
      52,     0,    53,     0,     0,  -499,     0,   365,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1217,     0,   961,     0,    84,
      85,     0,     0,     0,     0,     0,     0,    86,    87,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,  -499,
    -499,     0,  -499,    52,     0,    53,     0,     0,  -499,     0,
     365,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1305,     0,
     961,     0,    84,    85,     0,     0,     0,     0,     0,     0,
      86,    87,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,  -499,  -499,     0,  -499,    52,     0,    53,     0,
       0,  -499,     0,   365,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1464,     0,   961,     0,    84,    85,     0,     0,     0,
       0,     0,     0,    86,    87,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -499,  -499,     0,  -499,    52,
       0,    53,     0,     0,  -499,     0,   365,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1576,     0,   961,     0,    84,    85,
       0,     0,     0,     0,     0,     0,    86,    87,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -499,  -499,
       0,  -499,    52,     0,    53,     0,     0,  -499,     0,   365,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1783,     0,   961,
       0,    84,    85,     0,     0,     0,     0,     0,     0,    86,
      87,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -499,  -499,     0,  -499,    52,     0,    53,     0,     0,
    -499,     0,   365,    55,    56,    57,    58,    59,    60,    61,
    1328,     0,     0,     0,     0,     0,     0,    64,    14,    15,
      16,    17,    18,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
     414,     0,     0,   415,     0,   416,   417,     0,   418,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,    64,   419,     0,  1562,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,     0,     0,    84,    85,     0,     0,     0,     0,
       0,     0,    86,    87,     0,   420,   421,     0,   422,   423,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     424,   425,   411,     0,   426,   427,   428,   414,   429,   430,
     415,     0,   416,   417,     0,   418,    80,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,   419,     0,     0,     0,     0,     0,   431,     0,
       0,    84,   432,     0,     0,     0,     0,     0,   433,   497,
      87,   434,   435,   436,   437,     0,     0,     0,     0,     0,
       0,     0,   420,   421,     0,   422,   423,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   424,   425,   411,
       0,   426,   427,   428,     0,   429,   430,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   431,     0,     0,    84,   432,
       0,     0,     0,     0,     0,   433,  1563,    87,   434,   435,
     436,   437,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,     0,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    82,    83,     0,    84,    85,     0,     0,     0,
    -837,     0,     0,    86,    87,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,    88,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   158,     0,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    82,    83,     0,
      84,   217,     0,     0,     0,     0,     0,     0,    86,    87,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
      88,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,     0,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    82,    83,     0,    84,    85,     0,     0,     0,
       0,     0,     0,    86,    87,     0,     0,    14,    15,    16,
      17,    18,     0,     0,    20,    88,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,  -500,  -500,     0,
    -500,    52,     0,    53,     0,     0,  -500,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   158,     0,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    82,    83,     0,
      84,   517,     0,     0,     0,     0,     0,     0,    86,    87,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      88,     4,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,    54,    55,    56,    57,    58,    59,    60,    61,
      62,     0,     0,     0,    63,     0,     0,    64,     0,     0,
       0,     0,  -419,  -419,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -419,     0,     0,     0,    84,    85,     0,     0,     0,     0,
       0,     0,    86,    87,     4,   194,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,    62,     0,     0,     0,    63,     0,     0,
      64,     0,     0,     0,     0,  -420,  -420,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    67,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -420,     0,     0,     0,    84,    85,     0,
    1471,     0,  1472,     0,     0,    86,    87,  1473,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,  1474,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    67,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1475,
       0,     0,     0,    84,  1029,     0,  1471,     0,  1472,     0,
       0,    86,    87,  1473,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,  1474,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    67,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1664,     0,     0,     0,    84,
    1029,     0,  1471,     0,  1472,     0,     0,    86,    87,  1473,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,  1474,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    67,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1665,     0,     0,     0,    84,  1029,     0,  1471,     0,
    1472,     0,     0,    86,    87,  1473,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,  1474,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    67,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1666,     0,     0,
       0,    84,  1029,     0,     0,     0,     0,     0,     0,    86,
      87,   274,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -500,  -500,     0,  -500,    52,     0,    53,     0,     0,
    -500,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   274,     0,     0,
       0,     0,     0,     0,    84,   275,     0,    14,    15,    16,
      17,    18,    86,    87,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,  -500,  -500,     0,
    -500,    52,     0,    53,     0,     0,  -500,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      84,   517,     0,     0,     0,     0,     0,     0,    86,    87,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     365,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
     623,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   677,
       0,  -700,    84,   678,     0,     0,     0,     0,     0,     0,
      86,    87,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   365,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,   623,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   812,     0,  -700,    84,   563,     0,     0,     0,     0,
       0,     0,    86,    87,   194,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   365,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,    81,  1157,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -708,    84,   929,     0,     0,
       0,     0,     0,     0,    86,    87,   194,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   365,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   366,    84,   367,
       0,     0,     0,     0,     0,     0,    86,    87,   194,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   365,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,    81,  1635,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      84,   929,     0,     0,     0,     0,     0,     0,    86,    87,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     365,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
    1637,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    84,   929,     0,     0,     0,     0,     0,     0,
      86,    87,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   365,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    84,   517,     0,     0,     0,     0,
       0,     0,    86,    87,   194,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   365,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,   929,     0,     0,
       0,     0,     0,     0,    86,    87,   194,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   365,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    84,   367,
       0,     0,     0,     0,     0,     0,    86,    87,   194,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,  -500,  -500,     0,
    -500,    52,     0,    53,     0,     0,  -500,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,  1495,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   414,     0,
       0,   415,     0,   416,   417,     0,   418,     0,     0,     0,
      84,   275,     0,     0,     0,     0,     0,     0,    86,    87,
       0,  1235,     0,   419,  1237,     0,  1238,  1947,  1948,  1239,
    1240,  1241,  1242,  1243,  1244,  1245,  1246,  1247,  1248,  1249,
    1250,     0,     0,  1251,  1252,  1253,  1254,  1255,  1256,  1257,
       0,  1258,     0,   420,   421,     0,   523,   423,  1259,  1260,
      71,    72,    73,    74,    75,    76,    77,    78,   424,   425,
     411,  1261,   426,   427,   428,     0,   429,   430,     0,     0,
       0,     0,     0,     0,    80,     0,     0,  1495,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1262,     0,     0,    84,
     432,     0,     0,     0,   316,     0,   433,    86,    87,   434,
     435,   436,   437,     0,   414,     0,     0,   415,     0,   416,
     417,  -193,   418,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1235,     0,   419,
    1237,     0,  1238,     0,     0,  1239,  1240,  1241,  1242,  1243,
    1244,  1245,  1246,  1247,  1248,  1249,  1250,     0,     0,  1251,
    1252,  1253,  1254,  1255,  1256,  1257,     0,  1258,     0,   420,
     421,     0,   523,   423,  1259,  1260,    71,    72,    73,    74,
      75,    76,    77,    78,   424,   425,   411,  1261,   426,   427,
     428,     0,   429,   430,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1262,     0,     0,    84,   432,     0,     0,     0,
     316,     0,   433,    86,    87,   434,   435,   436,   437,     0,
       0,     0,     0,     0,     0,     0,     0,  -193,   320,   194,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -423,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    84,     0,     0,     0,     0,  -423,   320,   194,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -424,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      84,     0,     0,     0,     0,  -424,   320,   194,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    84,
       0,     0,     0,     0,  -423,    14,    15,    16,    17,    18,
      19,   740,    20,   741,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   414,     0,    52,
     415,    53,   416,   417,     0,   418,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   419,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   742,     0,     0,     0,     0,  1250,
       0,  -352,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   420,   421,     0,   422,   423,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   424,   425,   411,
       0,   426,   427,   428,     0,   429,   430,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1262,     0,     0,    84,   743,
       0,     0,     0,   316,     0,   433,    86,    87,   744,   745,
     436,   437,    14,    15,    16,    17,    18,    19,   740,    20,
     741,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   414,     0,    52,   415,    53,   416,
     417,     0,   418,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   742,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   420,
     421,     0,   422,   423,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   424,   425,   411,     0,   426,   427,
     428,     0,   429,   430,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   431,     0,     0,    84,   743,     0,     0,     0,
     316,     0,   433,    86,    87,   744,   745,   436,   437,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   414,     0,    52,   415,    53,   416,   417,     0,   418,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   420,   421,     0,   422,
     423,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   424,   425,   411,     0,   426,   427,   428,     0,   429,
     430,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   431,
       0,   462,    84,   463,     0,     0,     0,     0,     0,   433,
      86,    87,   434,   435,   436,   437,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   414,     0,
      52,   415,    53,   416,   417,     0,   418,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,   419,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   420,   421,     0,   422,   423,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   424,   425,
     411,     0,   426,   427,   428,     0,   429,   430,     0,     0,
       0,     0,     0,     0,    80,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   431,     0,     0,    84,
     463,     0,     0,     0,   316,     0,   433,    86,    87,   434,
     435,   436,   437,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   414,     0,    52,   415,    53,
     416,   417,     0,   418,    54,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     419,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     420,   421,     0,   422,   423,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   424,   425,   411,     0,   426,
     427,   428,     0,   429,   430,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   431,     0,     0,    84,   743,     0,     0,
       0,   316,     0,   433,    86,    87,   434,   435,   436,   437,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   414,     0,    52,   415,    53,   416,   417,     0,
     418,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,   419,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   420,   421,     0,
     422,   423,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   424,   425,   411,     0,   426,   427,   428,     0,
     429,   430,     0,     0,     0,     0,     0,     0,    80,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     431,     0,     0,    84,   463,     0,     0,     0,     0,     0,
     433,    86,    87,   434,   435,   436,   437,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   414,
       0,    52,   415,    53,   416,   417,     0,   418,   365,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   420,   421,     0,   422,   423,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   424,
     425,   411,     0,   426,   427,   428,     0,   429,   430,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   431,     0,     0,
      84,   843,     0,     0,     0,     0,     0,   433,    86,    87,
     434,   435,   436,   437,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   414,     0,    52,   415,
      53,   416,   417,     0,   418,   365,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,   419,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   420,   421,     0,   422,   423,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   424,   425,   411,     0,
     426,   427,   428,     0,   429,   430,     0,     0,     0,     0,
       0,     0,    80,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   431,     0,     0,    84,   432,     0,
       0,     0,     0,     0,   433,    86,    87,   434,   435,   436,
     437,   194,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    64,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     731,     0,   732,   733,     0,     0,     0,     0,     0,   414,
       0,     0,   415,     0,   416,   417,     0,   418,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,     0,    64,   419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   -17,     0,     0,
       0,     0,     0,     0,   420,   421,     0,   422,   423,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   424,
     425,   411,     0,   426,   427,   428,     0,   429,   430,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1710,  1711,  1712,  1713,     0,     0,     0,   431,  1962,     0,
      84,   432,     0,     0,     0,     0,     0,   433,    86,    87,
     434,   435,   436,   437,   274,   194,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -500,  -500,     0,  -500,    52,     0,
      53,     0,     0,  -500,     0,   194,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      64,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,    69,    70,    52,     0,
      53,     0,     0,     0,     0,   365,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    84,     0,     0,
       0,     0,     0,     0,   158,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,   623,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -700,    84,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   365,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   158,     0,
     490,    70,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   866,     0,
       0,    84,   491,     0,     0,     0,     0,     0,     0,    86,
      87,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,    54,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   158,     0,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    84,    85,     0,     0,     0,     0,
       0,     0,    86,    87,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   158,     0,   490,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    84,   491,     0,
       0,     0,     0,     0,     0,    86,    87,   194,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,   365,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,   623,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -700,    84,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     365,    55,    56,    57,    58,    59,    60,    61,     0,    14,
      15,    16,    17,    18,     0,    64,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,    69,    70,    52,     0,    53,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,    81,
    1228,     0,     0,     0,     0,   194,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,    84,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,    81,
      53,     0,     0,     0,     0,   365,    55,    56,    57,    58,
      59,    60,    61,     0,    14,    15,    16,    17,    18,    19,
      64,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,    69,    70,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    84,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   866,     0,     0,    84,   491,     0,
       0,     0,     0,     0,     0,    86,    87,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   365,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   866,     0,     0,
      84,   491,     0,     0,     0,     0,     0,     0,    86,    87,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1036,    84,  1029,     0,     0,     0,     0,     0,
       0,    86,    87,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,    54,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,  1582,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,  1029,     0,     0,
       0,     0,     0,     0,    86,    87,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    84,
     328,     0,     0,     0,     0,     0,     0,    86,    87,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    84,   217,     0,     0,     0,     0,     0,     0,
      86,    87,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   365,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    84,   367,     0,     0,     0,
       0,     0,     0,    86,    87,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   365,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    84,   328,
       0,     0,     0,     0,     0,     0,    86,    87,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   365,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    84,   491,     0,     0,     0,     0,     0,     0,    86,
      87,   194,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    -500,  -500,     0,  -500,    52,     0,    53,     0,     0,  -500,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    19,    64,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,    69,    70,    52,     0,    53,     0,     0,     0,
       0,   365,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    84,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    84,   517,     0,     0,     0,     0,     0,
       0,    86,    87,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,    54,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,  1029,     0,     0,
       0,     0,     0,     0,    86,    87,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,   365,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    84,
     345,     0,     0,     0,     0,     0,     0,    86,    87,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    84,   491,     0,     0,     0,     0,     0,     0,
      86,    87,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    84,    85,     0,     0,     0,
       0,     0,     0,    86,    87,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   365,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    84,  1029,
       0,     0,     0,     0,     0,     0,    86,    87,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   365,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    84,     0,     0,    14,    15,    16,    17,    18,    86,
      87,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -500,  -500,     0,  -500,    52,     0,
      53,     0,     0,  -500,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    84,   345,     0,
      14,    15,    16,    17,    18,    86,    87,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    -500,  -500,     0,  -500,    52,     0,    53,     0,     0,  -500,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    84,   517,     0,    14,    15,    16,    17,
      18,    86,    87,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,  -500,  -500,     0,  -500,
      52,     0,    53,     0,     0,  -500,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    84,
       0,     0,     0,     0,     0,     0,     0,    86,    87,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   414,     0,    52,   415,    53,   416,
     417,     0,   418,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   420,
     421,     0,   422,   423,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   424,   425,   411,     0,   426,   427,
     428,     0,   429,   430,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   431,     0,     0,    84,   432,     0,     0,     0,
       0,     0,   433,   497,    87,   434,   435,   436,   437,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   414,     0,    52,   415,    53,   416,
     417,     0,   418,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   420,
     421,     0,   422,   423,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   424,   425,   411,     0,   426,   427,
     428,     0,   429,   430,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   431,     0,     0,    84,   432,     0,     0,     0,
       0,     0,   433,    86,    87,   434,   435,   436,   437,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   158,
       0,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,    84,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,    14,    15,    16,    17,    18,    19,
      64,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,    69,    70,    52,     0,
      53,     0,     0,     0,     0,   365,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    84,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,    20,    84,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,  -500,
    -500,     0,  -500,    52,     0,    53,     0,     0,  -500,     0,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    64,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,    69,    70,    52,     0,    53,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    84,     0,     0,     0,     0,     0,     0,   195,
       0,   196,   197,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,    81,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   731,     0,   732,   733,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
      20,    81,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,  -499,  -499,     0,  -499,    52,     0,    53,
       0,     0,  -499,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,    20,    64,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -500,  -500,     0,  -500,    52,   414,    53,     0,   415,
    -500,   416,   417,     0,   418,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,   419,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   420,   421,     0,   523,   423,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   424,   425,   411,     0,
     426,   427,   428,     0,   429,   430,     0,     0,     0,     0,
       0,    81,    80,     0,   414,     0,     0,   415,     0,   416,
     417,     0,   418,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   431,    83,     0,   524,   525,   419,
       0,     0,   526,     0,   433,    86,    87,   434,   435,   436,
     437,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   420,
     421,     0,   422,   423,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   424,   425,   411,     0,   426,   427,
     428,   414,   429,   430,   415,     0,   416,   417,     0,   418,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   419,     0,     0,     0,
       0,     0,   431,  1367,     0,    84,   432,     0,     0,     0,
    1368,     0,   433,    86,    87,   434,   435,   436,   437,     0,
       0,     0,     0,     0,     0,     0,   420,   421,     0,   422,
     423,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   424,   425,   411,     0,   426,   427,   428,   414,   429,
     430,   415,     0,   416,   417,     0,   418,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   419,     0,     0,     0,     0,     0,   431,
       0,     0,    84,   432,     0,     0,     0,   526,     0,   433,
      86,    87,   434,   435,   436,   437,     0,     0,     0,     0,
       0,     0,     0,   420,   421,     0,   422,   423,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   424,   425,
     411,     0,   426,   427,   428,   414,   429,   430,   415,     0,
     416,   417,     0,   418,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,     0,     0,     0,   431,  1054,     0,    84,
     432,     0,     0,     0,     0,     0,   433,    86,    87,   434,
     435,   436,   437,     0,     0,     0,     0,     0,     0,     0,
     420,   421,     0,   422,   423,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   424,   425,   411,     0,   426,
     427,   428,   414,   429,   430,   415,     0,   416,   417,     0,
     418,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   419,     0,     0,
       0,     0,     0,   431,     0,     0,    84,   432,     0,     0,
       0,   316,     0,   433,    86,    87,   434,   435,   436,   437,
       0,     0,     0,     0,     0,     0,     0,   420,   421,     0,
     422,   423,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   424,   425,   411,     0,   426,   427,   428,   414,
     429,   430,   415,     0,   416,   417,     0,   418,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   419,     0,     0,     0,     0,     0,
     431,     0,     0,    84,   432,     0,     0,  1093,     0,     0,
     433,    86,    87,   434,   435,   436,   437,     0,     0,     0,
       0,     0,     0,     0,   420,   421,     0,   422,   423,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   424,
     425,   411,     0,   426,   427,   428,   414,   429,   430,   415,
       0,   416,   417,     0,   418,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,     0,     0,     0,     0,     0,   431,     0,     0,
      84,   432,     0,     0,     0,  1505,     0,   433,    86,    87,
     434,   435,   436,   437,     0,     0,     0,     0,     0,     0,
       0,   420,   421,     0,   422,   423,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   424,   425,   411,     0,
     426,   427,   428,   414,   429,   430,   415,     0,   416,   417,
       0,   418,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,     0,     0,     0,   431,  1593,     0,    84,   432,     0,
       0,     0,     0,     0,   433,    86,    87,   434,   435,   436,
     437,     0,     0,     0,     0,     0,     0,     0,   420,   421,
       0,   422,   423,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   424,   425,   411,     0,   426,   427,   428,
     414,   429,   430,   415,     0,   416,   417,     0,   418,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   419,     0,     0,     0,     0,
       0,   431,     0,     0,    84,   432,     0,     0,     0,  1780,
       0,   433,    86,    87,   434,   435,   436,   437,     0,     0,
       0,     0,     0,     0,     0,   420,   421,     0,   422,   423,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     424,   425,   411,     0,   426,   427,   428,   414,   429,   430,
     415,     0,   416,   417,     0,   418,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,     0,     0,     0,   431,     0,
    1953,    84,   432,     0,     0,     0,     0,     0,   433,    86,
      87,   434,   435,   436,   437,     0,     0,     0,     0,     0,
       0,     0,   420,   421,     0,   422,   423,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   424,   425,   411,
       0,   426,   427,   428,   414,   429,   430,   415,     0,   416,
     417,     0,   418,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,     0,     0,     0,   431,  1958,     0,    84,   432,
       0,     0,     0,     0,     0,   433,    86,    87,   434,   435,
     436,   437,     0,     0,     0,     0,     0,     0,     0,   420,
     421,     0,   422,   423,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   424,   425,   411,     0,   426,   427,
     428,     0,   429,   430,     0,     0,   414,     0,     0,   415,
      80,   416,   417,     0,   418,  2039,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,   431,  1968,     0,    84,   432,     0,     0,     0,
       0,     0,   433,    86,    87,   434,   435,   436,   437,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   420,   421,     0,   422,   423,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   424,   425,   411,     0,
     426,   427,   428,   414,   429,   430,   415,     0,   416,   417,
       0,   418,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,     0,     0,     0,   431,     0,     0,    84,   432,     0,
       0,     0,     0,     0,   433,    86,    87,   434,   435,   436,
     437,     0,     0,     0,     0,     0,     0,     0,   420,   421,
       0,   422,   423,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   424,   425,   411,     0,   426,   427,   428,
     414,   429,   430,   415,     0,   416,   417,     0,   418,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   419,     0,     0,     0,     0,
       0,   431,  2046,     0,    84,   432,     0,     0,     0,     0,
       0,   433,    86,    87,   434,   435,   436,   437,     0,     0,
       0,     0,     0,     0,     0,   420,   421,     0,   422,   423,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     424,   425,   411,     0,   426,   427,   428,   414,   429,   430,
     415,     0,   416,   417,     0,   418,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,     0,     0,     0,   431,  2048,
       0,    84,   432,     0,     0,     0,     0,     0,   433,    86,
      87,   434,   435,   436,   437,     0,     0,     0,     0,     0,
       0,     0,   420,   421,     0,   422,   423,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   424,   425,   411,
       0,   426,   427,   428,   414,   429,   430,   415,     0,   416,
     417,     0,   418,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,     0,     0,     0,   431,  2095,     0,    84,   432,
       0,     0,     0,     0,     0,   433,    86,    87,   434,   435,
     436,   437,     0,     0,     0,     0,     0,     0,     0,   420,
     421,     0,   422,   423,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   424,   425,   411,     0,   426,   427,
     428,   414,   429,   430,   415,     0,   416,   417,     0,   418,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   419,     0,     0,     0,
       0,     0,   431,  2097,     0,    84,   432,     0,     0,     0,
       0,     0,   433,    86,    87,   434,   435,   436,   437,     0,
       0,     0,     0,     0,     0,     0,   420,   421,     0,   422,
     423,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   424,   425,   411,     0,   426,   427,   428,   414,   429,
     430,   415,     0,   416,   417,     0,   418,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   419,     0,     0,     0,     0,     0,   431,
    2099,     0,    84,   432,     0,     0,     0,     0,     0,   433,
      86,    87,   434,   435,   436,   437,     0,     0,     0,     0,
       0,     0,     0,   420,   421,     0,   422,   423,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   424,   425,
     411,     0,   426,   427,   428,   414,   429,   430,   415,     0,
     416,   417,     0,   418,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,     0,     0,     0,   431,  2104,     0,    84,
     432,     0,     0,     0,     0,     0,   433,    86,    87,   434,
     435,   436,   437,     0,     0,     0,     0,     0,     0,     0,
     420,   421,     0,   422,   423,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   424,   425,   411,     0,   426,
     427,   428,   414,   429,   430,   415,     0,   416,   417,     0,
     418,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   419,     0,     0,
       0,     0,     0,   431,  2106,     0,    84,   432,     0,     0,
       0,     0,     0,   433,    86,    87,   434,   435,   436,   437,
       0,     0,     0,     0,     0,     0,     0,   420,   421,     0,
     422,   423,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   424,   425,   411,     0,   426,   427,   428,   414,
     429,   430,   415,     0,   416,   417,     0,   418,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   419,     0,     0,     0,     0,     0,
     431,  2142,     0,    84,   432,     0,     0,     0,     0,     0,
     433,    86,    87,   434,   435,   436,   437,     0,     0,     0,
       0,     0,     0,     0,   420,   421,     0,   422,   423,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   424,
     425,   411,     0,   426,   427,   428,   414,   429,   430,   415,
       0,   416,   417,     0,   418,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,     0,     0,     0,     0,     0,   431,  2144,     0,
      84,   432,     0,     0,     0,     0,     0,   433,    86,    87,
     434,   435,   436,   437,     0,     0,     0,     0,     0,     0,
       0,   420,   421,     0,   422,   423,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   424,   425,   411,     0,
     426,   427,   428,   414,   429,   430,   415,     0,   416,   417,
       0,   418,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,     0,     0,     0,   431,  2146,     0,    84,   432,     0,
       0,     0,     0,     0,   433,    86,    87,   434,   435,   436,
     437,     0,     0,     0,     0,     0,     0,     0,   420,   421,
       0,   422,   423,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   424,   425,   411,     0,   426,   427,   428,
     414,   429,   430,   415,     0,   416,   417,     0,   418,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   419,     0,     0,     0,     0,
       0,   431,  2165,     0,    84,   432,     0,     0,     0,     0,
       0,   433,    86,    87,   434,   435,   436,   437,     0,     0,
       0,     0,     0,     0,     0,   420,   421,     0,   422,   423,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     424,   425,   411,     0,   426,   427,   428,   414,   429,   430,
     415,     0,   416,   417,     0,   418,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,     0,     0,     0,   431,  2167,
       0,    84,   432,     0,     0,     0,     0,     0,   433,    86,
      87,   434,   435,   436,   437,     0,     0,     0,     0,     0,
       0,     0,   420,   421,     0,   422,   423,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   424,   425,   411,
       0,   426,   427,   428,   414,   429,   430,   415,     0,   416,
     417,     0,   418,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,     0,     0,     0,   431,  2169,     0,    84,   432,
       0,     0,     0,     0,     0,   433,    86,    87,   434,   435,
     436,   437,     0,     0,     0,     0,     0,     0,     0,   420,
     421,     0,   422,   423,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   424,   425,   411,     0,   426,   427,
     428,   414,   429,   430,   415,     0,   416,   417,     0,   418,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   419,     0,     0,     0,
       0,     0,   431,     0,     0,    84,   432,     0,     0,     0,
       0,     0,   433,    86,    87,   434,   435,   436,   437,     0,
       0,     0,     0,     0,     0,     0,   420,   421,     0,   422,
     423,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   424,   425,   411,     0,   426,   427,   428,   414,   429,
     430,   415,     0,   416,   417,     0,   418,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   419,     0,     0,     0,     0,     0,   720,
       0,     0,    84,   432,     0,     0,     0,     0,     0,   433,
      86,    87,   434,   435,   436,   437,     0,     0,     0,     0,
       0,     0,     0,   420,   421,     0,   422,   423,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   424,   425,
     411,     0,   426,   427,   428,   414,   429,   430,   415,     0,
     416,   417,     0,   418,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,     0,     0,     0,   723,     0,     0,    84,
     432,     0,     0,     0,     0,     0,   433,    86,    87,   434,
     435,   436,   437,     0,     0,     0,     0,     0,     0,     0,
     420,   421,     0,   422,   423,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   424,   425,   411,     0,   426,
     427,   428,   414,   429,   430,   415,     0,   416,   417,     0,
     418,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   419,     0,     0,
       0,     0,     0,   728,     0,     0,    84,   432,     0,     0,
       0,     0,     0,   433,    86,    87,   434,   435,   436,   437,
       0,     0,     0,     0,     0,     0,     0,   420,   421,     0,
     422,   423,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   424,   425,   411,     0,   426,   427,   428,   414,
     429,   430,   415,     0,   416,   417,     0,   418,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   419,     0,     0,     0,     0,     0,
     737,     0,     0,    84,   432,     0,     0,     0,     0,     0,
     433,    86,    87,   434,   435,   436,   437,     0,     0,     0,
       0,     0,     0,     0,   420,   421,     0,   422,   423,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   424,
     425,   411,     0,   426,   427,   428,   414,   429,   430,   415,
       0,   416,   417,     0,   418,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,     0,     0,     0,     0,     0,   431,     0,     0,
      84,   432,     0,     0,     0,     0,     0,   433,   953,    87,
     434,   435,   436,   437,     0,     0,     0,     0,     0,     0,
       0,   420,   421,     0,   422,   423,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   424,   425,   411,     0,
     426,   427,   428,     0,   429,   430,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   431,     0,     0,    84,   432,     0,
       0,     0,     0,     0,   433,   497,    87,   434,   435,   436,
     437
};

static const yytype_int16 yycheck[] =
{
       1,   273,   193,     4,   370,   106,   526,   188,    82,   247,
     766,    82,    93,   769,   178,   289,   431,   973,   681,   537,
     242,  1005,  1281,   176,   176,    84,    82,    82,   247,  1013,
     233,     1,   690,   934,   176,   247,   215,    65,   384,   966,
     147,  1262,   388,  1068,    82,    82,   268,   813,   178,   677,
       1,     1,     1,   433,     4,  1386,   847,   151,  1823,   847,
      96,    62,    63,   247,    65,   247,   215,   257,     1,  1313,
    1314,  1096,  1717,   860,    82,   862,   950,   677,  1721,   160,
     202,    82,    82,   938,   874,   247,  1947,    88,   683,   677,
     747,  1823,    93,  1951,  1215,    65,   286,   107,  1823,   337,
     101,  1222,  1262,   958,   157,   106,   845,   297,   109,   108,
      79,   950,   113,   163,    65,    65,    65,    97,   337,     1,
     214,   247,  1245,    93,     0,   337,   179,    91,   431,   157,
     204,   596,    65,     1,   845,   171,   106,   187,   504,   109,
    1165,     1,   607,   113,    84,    85,   848,   248,   204,   204,
     257,   152,   854,   337,   155,   337,   157,   845,     0,   109,
     127,   247,   163,   113,   163,    82,   204,   204,     1,   170,
     845,     4,  1824,   247,   143,   337,   247,   178,   188,   286,
     127,  1302,   142,   163,   847,   144,   651,   157,   188,   190,
     297,   247,   247,    68,    69,   276,   204,    65,   167,   163,
     167,   202,   203,   204,   247,   171,   157,   157,   157,   247,
     247,   337,   178,   214,   192,   322,   368,   845,   247,   220,
     179,  1011,  2083,   324,   157,  1868,   186,   391,  1873,  1874,
     231,   171,    65,   203,   169,   236,     1,   338,   132,   247,
     241,   242,   243,   583,   584,   845,   247,   248,   163,    82,
     220,   337,   311,   188,   849,    88,   171,   845,   853,   625,
      93,   391,     0,   337,  1595,   346,   337,   268,   398,   864,
     865,   188,   166,   106,  2132,   276,   109,   278,   248,   147,
     113,   337,   337,  1935,   406,    79,   287,   288,   170,   157,
     291,  1218,  2057,   567,   337,   101,   171,   298,  1227,   119,
      65,   575,  1058,   669,   163,  2163,   276,   545,   337,   169,
     170,   312,   313,   188,   315,    79,     1,  1962,  1963,   320,
     666,   291,   142,   324,   157,  2057,   545,   693,   991,   532,
     163,  1983,  2057,   545,   700,   163,   337,   338,   502,  1105,
    1131,   291,   722,  1131,  1465,   178,   498,   348,  1377,   143,
     171,  1335,   767,     4,   324,   356,   357,   703,   193,   581,
     361,   545,  1236,   545,  1219,   587,  1227,   188,   338,   163,
     203,   204,   712,  1533,   171,    96,  1536,  1537,   106,   143,
      65,   214,   147,   545,   465,  1124,   726,   220,  1045,   257,
     391,   188,   157,    79,   169,   144,  1270,  1236,   231,   400,
     494,    20,   403,   106,   179,   406,  2058,   645,   241,   242,
     243,   590,   163,  1124,   247,   248,   606,   720,   286,   545,
     723,   515,   725,  1125,   109,   728,   645,    84,    85,   297,
     179,  1270,   119,   645,   737,   268,  1124,   740,   741,   742,
    1341,   590,   168,   276,   250,    79,   252,    82,   169,  1124,
     171,   118,   970,   259,   322,   142,   557,   143,   291,   545,
     169,   645,   113,   645,    99,   524,  2118,   867,  1131,   869,
     935,   545,   157,   165,   141,   476,   258,   163,   165,   312,
     820,   167,   315,   645,   176,   177,   886,   320,   667,   545,
     545,   324,   257,   163,   840,    82,  1124,    79,  1227,   606,
     501,   502,   545,    10,   337,   338,   476,   545,   545,   143,
      97,   362,   513,   514,   171,    93,   545,   167,   667,   645,
     248,   286,   172,   524,  1124,   526,   332,   333,   106,   163,
     163,   109,   297,   167,  1755,   113,  1124,   545,   690,  1468,
    1469,  1470,   169,  1333,   545,   248,   647,    82,   163,  2086,
    1340,   402,   179,  1148,   163,   955,   557,   322,   391,   645,
     188,   143,   540,    98,    79,   256,     1,   115,   116,     4,
     144,   645,   263,  1598,  1603,  1604,  1605,  2114,   708,  1099,
     581,   163,   117,   188,  1568,   167,   587,   557,   589,   645,
     645,   188,   169,  1716,   285,  1755,   324,  2134,  1721,   965,
     174,   175,   645,  1393,   169,   296,   291,  1468,  1469,  1470,
     338,   188,   162,   169,   980,    10,   645,   796,   166,   169,
     169,   324,   650,  1325,   169,   203,   169,   163,   143,   165,
      65,   169,   978,  1420,   179,   338,  1423,  1424,   188,   188,
     325,   179,   220,   476,   645,   188,   647,   796,   163,   650,
    1577,   652,   167,   169,  1493,   162,   169,  1901,  1497,  1498,
     661,  1319,   841,   169,   665,   169,   173,   858,   160,   502,
     248,   178,  1511,   876,   109,   188,     3,   647,   113,   171,
     650,   860,   188,   847,   188,   370,   468,   298,    76,   868,
     912,   524,   841,   526,     3,   187,   697,    79,   276,   650,
     650,   650,   313,   485,   654,   167,   488,  1409,    79,   862,
     172,   166,   545,   291,  1667,   716,   868,   650,   169,  1672,
    1120,  1121,   157,   169,   557,    79,   868,    79,   179,   164,
    1486,    13,    14,    15,    16,    17,   795,  1996,   606,  1468,
    1469,  1470,   964,   171,   169,  1868,   169,    65,   581,   169,
      68,    69,   165,    71,   587,   163,   589,   170,   759,   179,
     761,   143,   763,   188,     1,   188,   767,   162,   550,   770,
     171,    79,   143,  1798,   169,  1800,  1176,   178,   173,  1569,
     215,   163,   650,   178,   400,   167,    79,   403,   163,   143,
    1774,   143,   163,   599,   795,   170,   167,    79,   164,   667,
     115,   116,  1592,    79,   169,   171,   169,  1763,    79,   163,
     187,   163,   645,   167,   647,   167,   794,   650,    79,   504,
      79,   169,   165,   188,     1,   188,   169,     4,    65,   557,
     169,     1,   665,   169,     4,   143,   171,    79,  1961,   840,
     188,   606,   163,   649,   845,   163,   847,  1262,  1368,   188,
     143,  1974,   188,   165,   557,   163,   291,   169,   859,   167,
     165,   143,  1041,   165,   697,   170,   185,   143,   170,   870,
     163,   163,   143,   163,   167,   876,  1006,   165,   879,   171,
     571,   169,   143,   127,   143,   650,   178,   163,    65,  1041,
    1164,   167,   163,   165,    79,    65,   167,   169,   476,  1041,
     165,   143,   163,   165,   163,   174,   167,   598,   167,   165,
     147,   912,   181,   182,   605,  2038,    93,  1580,   609,   647,
     157,   163,   165,   168,   165,   167,   759,   170,   761,   170,
     763,    13,    14,    15,    16,    17,  1726,   770,   166,   167,
     625,    79,   170,   113,   647,   165,  1040,   948,   949,   950,
     163,    79,   165,   638,   167,   141,   165,  1323,   143,  1262,
     169,   165,   795,   964,   163,   650,  1130,  1131,   167,  1172,
     147,   165,  1312,   841,   151,   137,   138,   163,   163,   557,
     157,   167,   167,   160,   934,  1505,  1776,   157,   174,   175,
    1508,   163,   165,     1,   167,    53,    54,    79,    56,   950,
     950,   950,  1792,   654,    62,   143,   165,   840,   693,   165,
     169,  1012,   845,   169,   847,   143,   966,   950,  1019,   163,
     257,   183,   184,   858,   163,   163,   165,   169,   167,   167,
     812,   813,  1871,   115,   116,   163,   165,   214,   873,   167,
     169,   823,    22,   876,   826,   163,   879,    65,   163,   286,
      68,    69,  1355,    71,   163,   165,  1328,    65,  1473,   169,
     297,   143,   165,  1064,     3,   163,   169,  1068,   165,   167,
      13,  1591,   169,   169,    13,    14,    15,    16,    17,   912,
     257,   169,   950,   171,   166,   322,   108,  1309,   115,   116,
       3,   156,   157,   158,   159,  1096,   163,   274,  1099,   276,
      13,    14,    15,    16,    17,   113,   163,   163,   163,   286,
     892,   167,   167,    96,   179,   948,   168,   899,  1908,   162,
     297,   903,  1912,  1124,   171,   907,   165,   166,   141,  1130,
    1131,   964,  1313,  1314,   120,   121,   122,   123,   124,   147,
      79,   176,   177,   320,   165,   322,  1227,   163,   169,   157,
     163,   167,    95,   165,   167,   590,  1674,   169,   188,   165,
     171,   174,   175,   169,  1165,   163,    79,  1319,   171,   346,
     113,  1350,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,  1456,   113,  2023,   950,  1019,   117,   118,   119,
     120,   121,   122,   123,   124,   141,   165,   163,  1350,   165,
     169,   167,  1332,   163,   143,   165,  1348,   167,  1350,   165,
     135,   136,   165,   169,  2053,   650,   169,   163,   168,   654,
     163,   167,   156,   157,   158,   159,   168,   169,   174,   175,
     143,  1064,   667,   163,   164,  1068,  1992,   165,  1239,  1430,
    1431,  1242,  1243,  1244,  1423,   179,   937,   165,  2087,   257,
    1251,   169,   171,   165,   188,   165,   186,   169,  1093,   169,
    1780,   139,   140,  1096,   178,   950,  1099,  1420,  1218,  1270,
     187,  1424,   165,  1426,  1426,  1276,   169,   165,   286,   163,
      68,   169,   165,   934,  1426,  1236,  1236,  1236,   465,   297,
    1291,  1124,   127,  1294,  1295,   980,  1297,  1130,  1131,   169,
     165,   165,  1303,  1236,   169,   169,  1307,   141,  1309,    13,
      14,    15,    16,    17,   322,   966,   163,   494,   326,  1270,
    1270,  1270,   180,  1105,  1294,  1295,   114,   168,   169,   163,
     163,   119,  1165,   167,   122,   163,   124,  1270,   515,  1174,
     174,   175,   165,  2119,  1294,  1295,   169,  2123,  1493,   163,
    1351,   175,  1497,   187,   173,    13,    14,    15,    16,    17,
     185,   796,   781,   782,   783,   784,   141,  1368,  1236,   606,
     774,   775,   776,  1552,   166,    79,  1377,   167,  1479,   168,
     169,   130,  1573,   132,   133,   134,  1493,  1468,  1469,  1470,
    1497,  1341,  1473,  1474,   163,  1201,    18,   165,  1180,   165,
    1552,  1183,  1270,   174,   175,  1187,   841,   168,   169,  1410,
    1552,   168,   169,   650,   163,   163,    97,   166,   167,   169,
     170,    79,   171,   172,   165,   860,   165,   862,   165,   606,
     165,   866,   867,   144,   869,  1664,  1665,  1666,   168,   143,
     144,  1679,    64,    65,    66,    67,    68,    69,    70,    71,
     169,   886,  1224,  1225,  1226,   168,   169,   138,  1291,   144,
    1679,  1294,  1295,  1686,  1687,  1688,   169,  1679,   168,   169,
     169,  1236,   170,   650,   168,   169,  1309,   170,  1479,   163,
     650,   187,  1483,  1484,   654,   143,   144,  1790,    98,    99,
     168,   169,   280,   168,   169,  1679,   165,  1679,   165,   934,
     156,   157,   158,   159,  1505,  1270,   165,    13,    14,    15,
      16,    17,   165,   169,   165,   950,   165,  1679,  1351,   165,
     955,  1471,   165,   179,   168,   169,   168,   169,  1529,  1530,
     965,   966,   188,   168,   169,  1368,   168,   169,  1539,   327,
     168,   169,   187,  1493,  1377,   168,   169,  1497,  1498,   168,
     169,  1236,   168,  1679,  1389,   168,   169,   169,   170,   167,
    1493,  1511,   168,   169,  1497,  1498,  1667,  1218,   165,  1539,
    1571,  1672,   171,    79,   168,   169,   171,  1410,  1511,   171,
    1681,    84,    85,   171,   372,  1270,   374,   171,   376,  1539,
    1591,   165,    13,  1679,   169,  1430,  1431,  1598,   606,   143,
    1838,    77,  1603,  1604,  1605,   169,   170,   188,  1679,  1294,
    1378,  1379,   168,  1395,   777,   778,   785,   786,  1984,  1838,
     163,   779,   780,    85,  1406,  1493,  1838,  1577,   168,  1497,
    1498,  1536,  1537,    18,   422,   141,  1679,   143,  1323,  1687,
    1688,   187,   650,  1511,  1295,   171,  1479,   171,   165,   188,
    1679,   165,   171,   334,  1838,   171,  1838,   163,   168,   168,
      18,   167,   162,   168,   168,    22,  1667,   165,   174,   175,
     165,  1672,  1505,   165,    95,   165,  1838,   165,  1679,   165,
    1681,   165,   165,   419,   165,  1120,  1121,   165,  1689,   168,
    1341,   165,   113,   237,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   165,   165,  1539,  1708,   444,   445,
     162,    77,  1838,   950,  1715,   171,  1294,  1295,   171,   187,
    1901,   156,   157,   158,   159,   171,   165,   165,  1493,   465,
     171,   165,  1497,  1498,   169,   523,   165,   165,  1573,   169,
    1841,  1176,   165,   165,   179,  1746,  1511,   171,   165,   169,
     165,  1479,  1838,   188,  1704,  1705,  1947,   169,  1591,   165,
     168,  1983,   498,   168,   934,  1598,  1744,   169,   165,   165,
    1603,  1604,  1605,   950,   165,   169,  1479,   321,   165,  1780,
     950,   165,   113,  1218,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   165,  1838,   966,  1798,   165,  1800,
     165,  1236,   165,   165,   168,  1886,   162,   168,  1493,  1838,
     165,   165,  1497,  1498,   165,   603,   497,   165,   499,  2057,
    1471,   165,   165,   165,   165,   165,  1511,   508,    13,    14,
      15,    16,    17,    18,  1667,  1270,   165,  1838,  2057,  1672,
    1841,   165,   169,  1944,   165,  2057,  1679,   169,  1681,  1850,
    1851,   169,   169,   162,  1539,   169,  1857,   188,   162,  1294,
    1295,   163,   163,  1040,   163,   163,   163,   163,     1,  1870,
     163,     4,   163,  2057,    14,  2057,   170,   188,   170,  1880,
     169,  1882,   168,   168,   171,   162,  1668,   431,   169,   171,
    2081,   162,  2083,   165,  1895,  2057,  1897,  1898,  1899,   187,
    1981,   165,   165,  1853,   187,   165,  1341,   165,   165,    82,
    1888,     4,     5,     6,     7,     8,     9,    10,    11,    12,
     168,  1871,   168,   165,   169,   168,  1577,   165,   165,   165,
    2121,  2057,    65,   106,   165,   168,   162,   162,  1871,  1667,
     163,   188,   950,  1944,  1672,   163,    87,  1780,   188,    82,
    1951,    99,   162,  1681,  1955,   163,   188,  2058,   188,  1960,
      93,  1539,   163,    97,  1667,  1798,   188,  1800,   188,  1672,
     165,  2057,   188,   106,   188,   162,   109,   168,  1681,    72,
     113,   171,  1983,  2057,  1985,  1420,  2057,   168,  1423,  1424,
     163,   168,   168,   162,   162,  2186,   170,   162,   165,  1236,
     170,  2057,  2057,  1871,   130,  1838,   165,   165,  1841,   165,
     165,  1989,   162,  2014,  2057,   168,   168,  2118,   151,  1704,
     165,   165,   165,   170,   157,  2026,   570,   160,  2057,  2030,
     163,   164,   169,  1270,   578,   165,  1471,   773,  2039,   188,
     165,  1823,  1824,   176,  2045,   163,   165,   163,  1218,   163,
    1227,   168,   165,   597,  1705,   165,  2057,  2058,  1493,  1236,
     168,   168,  1497,  1498,   608,   162,  1236,   162,   201,   242,
     203,   204,   168,  2023,   247,   248,  1511,   165,   165,   165,
      82,   214,   215,    82,   188,   188,   162,   220,  2058,  2090,
    2023,   163,   165,  1270,   188,   268,   163,   168,   168,   162,
    1270,   162,   167,  2053,  1539,   165,  1871,   165,   165,   242,
      82,  1944,  1947,  1841,   247,   248,    82,  2118,    82,   188,
    2053,   170,   188,   179,   179,  1295,  2127,   162,   162,   188,
     162,  2132,   179,   164,   188,   268,   179,  2087,  1841,   162,
     165,  2119,  1577,   276,   170,  2123,  2124,   163,  2118,   169,
    1983,   324,  1985,  1935,  2087,  2023,   113,  2158,   291,   165,
    2161,   179,  2163,   164,   337,   338,   179,   165,  1853,   188,
      82,  1341,   168,   165,  2152,   164,   720,   858,   165,   723,
     170,  2182,   162,   162,   728,  2053,  1871,   163,   188,  2190,
     165,   324,  1790,   737,   188,  2173,   746,   330,  2199,  2177,
     188,  1983,   787,   336,   337,   338,  2039,   789,   788,  1345,
     464,  2189,   756,   346,   790,  1257,  1944,  1270,   791,  2087,
    2055,  2163,  1497,  1879,  2057,  2058,  2008,  2083,  1236,  2114,
    2012,  1511,  1871,  2072,  1754,   368,   369,   370,  2054,  1737,
    1737,  1944,  1981,  2124,  2053,  2177,  2081,  1297,  2083,    55,
     122,   384,   282,  2012,  1944,   388,  1493,  1474,  2023,  1337,
    1497,  1498,  1270,    13,    14,    15,    16,    17,    18,  1704,
    1705,  1779,   953,   954,  1511,  2057,  2058,    13,    14,    15,
      16,    17,    18,   876,  1291,  2118,  2121,  1295,  2053,   215,
    1545,  1468,  1469,  1470,  1471,  1472,  1473,  1474,   431,   533,
    1036,  1471,   661,   113,     0,   115,  1042,   117,   118,   119,
     120,   121,   122,   123,   124,   716,  1493,  1053,   812,   812,
    1497,  1498,  2087,  1493,  1657,   812,    -1,  1497,  1498,   502,
    2058,    -1,   465,    -1,  1511,   468,  2118,    -1,  2023,    -1,
      -1,  1511,    -1,   476,    -1,    13,    14,    15,    16,    17,
      -1,  2186,    -1,    -1,    -1,  2058,    -1,  2190,    -1,    -1,
      -1,   494,    -1,    -1,    -1,   498,  2199,    -1,  2053,   502,
      -1,   504,   545,    -1,   918,    -1,  1057,    -1,    -1,    -1,
      -1,    -1,   515,    -1,   557,    -1,    -1,    -1,   932,    -1,
    2118,    -1,   936,    -1,    -1,    85,   940,    13,    14,    15,
      16,    17,  2087,    -1,   537,    -1,    -1,  1577,   581,    -1,
      -1,    79,   545,    -1,   587,  2118,    -1,  1098,  1853,    -1,
      -1,    -1,    -1,   113,   557,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   315,  1871,    -1,    -1,   368,
      -1,    -1,   368,    -1,    -1,   371,    -1,    -1,   581,    -1,
     583,   584,    -1,    -1,   587,   384,    -1,   590,   384,   388,
      -1,    -1,   388,    79,    -1,    -1,  1147,    -1,  1149,    -1,
      -1,    -1,   645,   141,   647,   143,    -1,    -1,    -1,    -1,
    1161,    -1,  1163,    -1,    -1,  1493,    -1,  1168,  1169,  1497,
    1498,    -1,    -1,    -1,    -1,   163,    -1,  1178,   188,   167,
    1236,    68,    -1,  1511,    -1,    -1,   174,   175,    -1,    -1,
      -1,    -1,   645,    -1,   647,    -1,    -1,   650,    -1,    -1,
      -1,   654,    -1,  1204,    -1,   141,  1207,   143,    -1,    -1,
      -1,    -1,    -1,   666,   667,  1705,   669,    -1,    -1,    -1,
      -1,    -1,   109,    -1,   677,    -1,    -1,   163,   681,    -1,
      -1,   167,    -1,    -1,   121,   122,    -1,   690,   174,   175,
      13,    14,    15,    16,    17,    18,    -1,   700,    -1,   498,
     703,    -1,   498,    -1,    -1,    -1,    -1,    -1,    -1,   712,
      -1,    -1,    -1,  1319,    -1,    -1,    -1,   720,  2023,  1270,
     723,    -1,   725,   726,    -1,   728,    -1,   164,    -1,    -1,
      -1,    -1,    -1,    -1,   737,    -1,   113,   740,   741,   742,
     117,   118,   119,   120,   121,   122,   123,   124,  2053,  1300,
      -1,     3,  1358,  1359,  1360,    -1,    -1,    -1,    -1,  1365,
    1366,    -1,    -1,    -1,  1871,    -1,    -1,  1318,    -1,  1320,
      -1,  1322,    -1,    -1,    -1,    -1,    -1,    -1,  1329,    -1,
     113,    -1,  2087,   220,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   796,   590,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   845,    -1,   847,    -1,    -1,  1221,    -1,   812,
     813,    -1,    -1,    -1,    -1,    -1,    -1,   820,    -1,    -1,
      -1,  1235,   572,    -1,  1871,    -1,    -1,  1705,    -1,    -1,
      -1,  1871,    -1,    85,   167,    -1,    -1,   840,   841,    -1,
      -1,  1255,   845,    -1,   847,    -1,    -1,    -1,  1262,    -1,
      -1,    -1,  1403,  1404,   291,   654,    -1,   860,   654,   862,
      -1,    -1,    -1,   866,   867,   868,   869,   666,    -1,   912,
     666,   667,    -1,    -1,    -1,    -1,    -1,    -1,  1429,    -1,
      -1,    -1,    -1,   886,    -1,  1436,    -1,  1438,   325,    -1,
      -1,   690,    -1,   330,   690,    -1,    -1,    -1,   648,   336,
      -1,    -1,    -1,    -1,   703,    -1,    -1,   703,    -1,   912,
      -1,    -1,    -1,    -1,    -1,    -1,  2023,    -1,    -1,    -1,
      -1,   964,    -1,    -1,    -1,    -1,    -1,   677,    -1,    -1,
      -1,   934,    -1,   370,    -1,    -1,    -1,    -1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,    -1,  2053,   950,    -1,    -1,
      -1,    -1,   955,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   964,   965,   966,    -1,   217,    -1,   970,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   978,  2023,   980,    -1,    -1,
    2087,    -1,    -1,  2023,    -1,   422,    -1,    -1,   991,    -1,
      -1,    -1,    -1,  1871,    -1,    -1,    -1,    -1,    -1,    -1,
     796,    -1,    -1,    -1,    -1,  1556,  2053,    -1,    -1,    -1,
      -1,    -1,  1563,  2053,  1565,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   275,   113,   231,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,  1040,  1041,    -1,
    2087,   840,    -1,    -1,   840,   841,    -1,  2087,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   812,    -1,   316,    -1,    -1,   504,    -1,    -1,
      -1,    -1,   868,  1487,  1488,    -1,   328,   141,   828,    -1,
      -1,  1124,   832,    -1,    -1,    -1,   523,  1130,  1131,   178,
    1641,   315,    -1,   345,    -1,   845,    -1,    -1,    -1,   163,
     164,    -1,  1105,    -1,    -1,    -1,   170,    -1,    -1,    -1,
     174,   175,    -1,    -1,    -1,   367,    -1,  1120,  1121,    79,
      -1,  1124,   186,    -1,  1538,    -1,    -1,    -1,  1131,    -1,
      -1,    -1,    -1,    -1,    -1,   934,    -1,    -1,   934,    -1,
      -1,    -1,    -1,    -1,  1750,  2023,    -1,    -1,    -1,    -1,
      -1,   588,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,   966,    -1,    -1,
     966,    -1,    -1,  1176,   611,  2053,    -1,    18,    -1,   978,
     432,    -1,   978,   143,    -1,   981,    -1,    -1,   625,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   946,    -1,    -1,    -1,
      -1,   638,    -1,   163,   164,    -1,    -1,    -1,    -1,  2087,
      -1,   463,    -1,    -1,    -1,  1218,    -1,    -1,  1769,  1770,
      -1,    -1,    -1,    -1,  1227,    -1,   186,    68,    69,    70,
      71,    -1,   669,  1236,    -1,    -1,    -1,    -1,    -1,   491,
      -1,    -1,   113,    -1,   496,  1041,   117,   118,   119,   120,
     121,   122,   123,   124,   125,    -1,   693,    -1,   129,  1262,
     131,    -1,    -1,   700,   516,   517,  1309,  1270,    -1,   521,
     522,    -1,   113,   525,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,    -1,    -1,    -1,    -1,   541,
      -1,  1294,  1295,   164,    -1,    -1,   167,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,  1309,    -1,    -1,  1312,
      -1,   563,    -1,    -1,    -1,    -1,  1319,    -1,   524,    -1,
     526,    -1,  1736,    -1,    -1,   113,   167,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,     1,  1341,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,  1350,   572,    -1,
      -1,    -1,  1355,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1111,    -1,    -1,    -1,  1115,    79,  1410,    -1,    -1,
      -1,    -1,    -1,    -1,  1124,   163,   164,    -1,    -1,   167,
      -1,    -1,    -1,    -1,  1134,  1991,   174,   175,    -1,   641,
      -1,  1141,    -1,    -1,    -1,    -1,    -1,    -1,   186,   187,
     113,    65,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,    -1,    -1,    -1,  1420,    -1,  1218,
    1423,  1424,  1218,  1426,   648,    -1,   678,    -1,   141,    93,
     143,    -1,    -1,    -1,    -1,    -1,  1479,    -1,    -1,  1189,
      -1,    -1,    -1,  1193,    -1,   109,    -1,  1197,    -1,   113,
     163,   164,    -1,    -1,   167,    -1,    -1,    -1,    -1,    -1,
      -1,   174,   175,    -1,    -1,  1468,  1469,  1470,  1471,    -1,
    1473,  1474,    -1,   186,    -1,    -1,  1479,  1480,   915,    -1,
      -1,    -1,    -1,   147,    -1,    -1,    -1,   151,    -1,    -1,
    1493,   743,    -1,   157,  1497,  1498,   160,    -1,    -1,    -1,
     164,    -1,    -1,    -1,    -1,  1508,    -1,    -1,  1511,    93,
      -1,   175,   176,    -1,   178,    -1,    -1,    -1,    -1,    -1,
    1319,    -1,    -1,  1319,    -1,    -1,    -1,    -1,   965,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1539,   201,    -1,    -1,
      -1,    -1,  1341,   980,    -1,  1341,    -1,    -1,    -1,  1552,
     214,   215,    -1,    -1,  1350,   113,   220,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1577,    -1,   160,  1580,    -1,    -1,
      -1,    -1,  2133,   835,    -1,   837,    -1,    -1,   812,    -1,
      -1,   843,    -1,   257,    -1,    -1,    -1,  2148,    -1,    -1,
      -1,    -1,    -1,    -1,   828,    -1,    -1,    -1,   832,    -1,
     274,    -1,   276,    -1,    -1,    -1,    -1,   201,    -1,   871,
      -1,    -1,   286,    -1,  1667,    -1,    -1,   291,   880,  1672,
     188,    -1,   884,   297,    -1,    -1,  1679,    -1,  1681,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,   312,    -1,
      -1,   315,    -1,   215,    -1,    -1,   320,    -1,   322,    -1,
      -1,   325,   326,    -1,  1667,  1668,   330,    -1,    -1,  1672,
     876,  1674,   336,   879,    -1,    -1,  1679,   929,  1681,    -1,
      -1,    -1,   346,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1441,   276,    -1,    -1,  1445,    -1,    -1,    -1,  1449,
      -1,  1704,  1705,  2117,   368,    -1,   370,   371,    79,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
     384,    -1,    -1,   113,   388,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,  2149,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,  1552,    -1,    -1,    -1,
      -1,    -1,   346,    -1,    -1,    -1,    -1,   431,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,   166,  1029,  1577,    -1,
      -1,  1577,    -1,    -1,   368,    -1,    -1,  1790,   178,    -1,
      -1,    -1,   163,   164,    -1,  1838,   167,    -1,  1841,    -1,
      -1,   465,    -1,   174,   175,    -1,    -1,  1557,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   186,    -1,    -1,   113,    -1,
    1823,  1824,   117,   118,   119,   120,   121,   122,   123,   124,
     494,    -1,    -1,    -1,   498,  1838,    -1,   113,  1841,    -1,
     504,   117,   118,   119,   120,   121,   122,   123,   124,   125,
    1853,   515,    -1,   129,    -1,   131,    -1,  1294,  1064,    -1,
      -1,  1611,  1068,    -1,    -1,    -1,    -1,    -1,  1871,   164,
    1620,    -1,   167,   537,  1624,    -1,    -1,    -1,    -1,    -1,
      -1,   465,    -1,  1886,    -1,    -1,  1323,  1111,   164,    -1,
    1096,  1115,    -1,  1099,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1944,    -1,    -1,    -1,    -1,    -1,    -1,   572,    -1,
    1134,    -1,    -1,    -1,   498,    -1,    -1,  1141,    -1,   583,
     584,    -1,    -1,    -1,   588,    -1,   590,    -1,    -1,    -1,
      -1,    -1,  1935,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1983,  1944,   606,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   537,    -1,    -1,    -1,    -1,    -1,  1165,
      -1,   625,  1214,    -1,    -1,  1189,    -1,    -1,    -1,  1193,
      -1,    -1,    -1,  1197,   638,    -1,    -1,    -1,  1981,    -1,
    1983,  1984,    -1,    -1,   648,    -1,   650,    -1,    -1,    -1,
     654,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   583,
     584,   665,   666,   667,    -1,   669,    -1,    -1,    -1,  2012,
      -1,  1263,    -1,   677,  2057,  2058,    -1,    -1,    -1,    -1,
    2023,    -1,    -1,    -1,  1461,    -1,   690,    -1,   590,   693,
      -1,    -1,    -1,   697,    -1,    -1,   700,    -1,    -1,   703,
      -1,    -1,    -1,  1480,    -1,    -1,    -1,    -1,   712,    -1,
    2053,    -1,    -1,    -1,  2057,  2058,   720,    -1,    -1,   723,
      -1,   725,   726,    -1,   728,  1317,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   737,    -1,  2118,   740,   741,   742,    -1,
      -1,    -1,    -1,    -1,  2087,    -1,    -1,  1886,    -1,   113,
    1886,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,  1539,    -1,    -1,   667,   690,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2118,    -1,    -1,    -1,   193,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   712,    -1,
      -1,    -1,   796,    -1,    -1,    -1,   720,    -1,    -1,    -1,
     164,    -1,   726,   167,   728,    -1,    -1,     1,   812,    -1,
       4,    -1,    -1,    -1,    -1,    -1,   820,    -1,    -1,    -1,
      -1,    -1,  1368,    -1,   828,    -1,    -1,    -1,   832,    -1,
      -1,  1377,    -1,    -1,    -1,    -1,   840,   841,    -1,    -1,
      -1,   845,  1981,    -1,   113,  1981,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   860,    -1,   862,    -1,
      -1,    -1,   866,   867,   868,   869,    -1,    -1,    -1,    -1,
      -1,    65,    -1,    -1,    -1,    -1,    -1,  1441,    -1,    -1,
      -1,  1445,   886,    -1,    -1,  1449,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   796,    -1,   820,    -1,    -1,    93,
      -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,    -1,    -1,
      -1,   915,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1704,    -1,    -1,
     934,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   841,
      -1,    -1,   946,    -1,    -1,    -1,   950,    -1,    -1,    -1,
      -1,   955,    -1,   147,    -1,    -1,    -1,   151,   860,  1505,
     862,   965,   966,   157,   866,   867,   970,   869,    -1,    -1,
      -1,    -1,    -1,    -1,   978,    -1,   980,   981,    -1,    -1,
      -1,    -1,   176,    -1,   886,  2075,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1557,    -1,    -1,    -1,    -1,    -1,    -1,
     414,    -1,   416,    -1,  1008,   419,   420,    -1,    -1,   203,
      -1,    -1,    -1,    -1,    -1,   429,   430,    -1,    -1,    -1,
     214,   215,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     444,   445,    -1,    -1,    -1,    -1,  1040,  1041,    -1,    -1,
      -1,    -1,    -1,    -1,   946,  1591,   970,  1611,    -1,    -1,
      -1,   465,  1598,   955,   248,    -1,  1620,  1603,  1604,  1605,
    1624,    -1,   113,   257,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   268,    -1,  1853,    -1,    -1,   273,
     274,    -1,   276,    -1,   498,    -1,    -1,    -1,    -1,    -1,
     141,   113,   286,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   297,    -1,    -1,   300,  1111,    -1,    -1,
     304,  1115,   163,   164,    -1,   309,  1120,  1121,    -1,   170,
    1124,   315,    -1,   174,   175,    -1,   320,    -1,   322,    -1,
    1134,    -1,   326,    -1,    -1,   186,    -1,  1141,    -1,    -1,
      -1,    -1,    -1,    -1,   338,    -1,    64,    -1,    -1,   171,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    -1,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,  1176,    -1,   368,    -1,    -1,   371,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1189,    -1,    -1,   141,  1193,
     384,    -1,    -1,  1197,   388,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,  1984,    -1,    -1,
     163,   164,    -1,    -1,  1218,    -1,    -1,    -1,  1120,  1121,
      -1,   174,   175,  1227,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1236,   186,  1780,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,   167,
      -1,    -1,  1798,    -1,  1800,    -1,    -1,    -1,  1262,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1270,   185,    -1,    -1,
      -1,    -1,    -1,    -1,  1176,    -1,    -1,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
    1294,  1295,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     494,    -1,    -1,    -1,   498,   141,    -1,   143,  1312,    -1,
      -1,    -1,    -1,    -1,    -1,  1319,    -1,    -1,    -1,  1323,
      -1,   515,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,
      -1,   167,    -1,    -1,    -1,    -1,    -1,  1341,   174,   175,
      -1,    -1,    -1,    -1,  1348,    -1,  1350,    -1,    -1,    -1,
     186,  1355,    -1,    -1,    -1,    -1,    -1,  1949,    -1,   773,
     774,   775,   776,   777,   778,   779,   780,   781,   782,   783,
     784,   785,   786,   787,   788,   789,   790,   791,   572,    -1,
      -1,    -1,    -1,    -1,    -1,  1977,    -1,    -1,  1312,    -1,
      -1,    -1,    -1,    -1,    -1,  1319,   590,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   606,    -1,    -1,    -1,  1420,    -1,    -1,  1423,
    1424,    -1,  1426,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,  1441,    -1,    -1,
      -1,  1445,    -1,    -1,   858,  1449,    -1,    -1,    -1,   163,
      -1,    -1,   166,   167,   648,    -1,   650,  1461,    -1,    -1,
     654,    -1,    -1,    -1,  1468,  1469,  1470,  1471,  1472,  1473,
    1474,    -1,   666,   667,    -1,    -1,  1480,    -1,    -1,    -1,
      -1,    -1,    -1,   677,    -1,    79,    -1,   681,    -1,  1493,
      -1,    -1,    -1,  1497,  1498,    -1,   690,    -1,    79,    -1,
      -1,   695,    -1,    -1,  1508,    -1,    -1,  1511,    -1,   703,
      -1,  2075,    -1,     1,    -1,    -1,    -1,    -1,  1420,   113,
      -1,  1423,  1424,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   113,    -1,    -1,  1539,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,   141,  1552,   143,
      -1,    -1,    -1,  1557,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,  1570,    -1,    -1,   163,
     164,    -1,    -1,  1577,    -1,    -1,    -1,    65,    -1,    -1,
     174,   175,   163,   164,  1508,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   186,   174,   175,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   796,    -1,    -1,   186,    -1,  1611,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1620,    -1,   812,   813,
    1624,    -1,  1036,    -1,    -1,   113,    -1,    -1,  1042,    -1,
      -1,    -1,    -1,    -1,   828,    -1,    -1,    -1,   832,  1053,
      -1,    -1,    -1,    -1,    -1,    -1,   840,   841,    -1,    -1,
      -1,   845,    -1,   847,    -1,    -1,    -1,    -1,    -1,   147,
      -1,    -1,    -1,    -1,    -1,    -1,   860,    -1,   862,   157,
    1674,    -1,   866,   867,   868,   869,    -1,    -1,    -1,  1093,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,   176,    -1,
      -1,    -1,   886,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1704,  1705,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   215,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     934,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
    1674,    -1,   946,    -1,    -1,    -1,   950,   167,    -1,    -1,
      -1,   955,    -1,    -1,    -1,   163,   164,    -1,    -1,   257,
      -1,    -1,   966,    -1,    -1,    -1,   174,   175,    -1,    -1,
      -1,   166,    -1,    -1,   978,    -1,  1790,   981,   186,    -1,
      -1,  1239,    -1,    -1,   988,    -1,    -1,    -1,   286,    -1,
      -1,    -1,    -1,  1251,    -1,    -1,    -1,    -1,    -1,   297,
      -1,    -1,   300,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,  1236,    -1,    -1,    -1,    -1,   315,    -1,    -1,
      -1,    -1,    -1,    -1,   322,    -1,    -1,    -1,   326,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1040,  1041,    -1,  1853,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,  1868,    -1,   113,  1871,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,   141,
     368,   143,  1886,   371,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   384,    -1,    -1,    -1,
     388,   163,   164,    -1,    -1,  1319,    -1,    -1,    -1,    -1,
      -1,    -1,   174,   175,    -1,    -1,    -1,  1111,    -1,   166,
      -1,  1115,    -1,    -1,   186,    -1,  1120,  1121,    -1,    -1,
    1124,    -1,    -1,    -1,   163,    -1,    -1,    -1,    -1,    -1,
    1134,    -1,    -1,    -1,  1358,  1359,  1360,  1141,    -1,    -1,
      -1,  1365,  1366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,  1965,  1966,    -1,   117,   118,   119,   120,   121,   122,
     123,   124,   125,    -1,    -1,  1389,   129,  1981,   131,    -1,
    1984,   113,  1176,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,  1189,    -1,    -1,    -1,  1193,
      -1,    -1,    -1,  1197,    -1,    -1,    -1,    -1,    -1,    -1,
     498,   164,    -1,    -1,   167,    -1,  1430,  1431,    -1,  2023,
      -1,    -1,    -1,    -1,  1218,    79,    -1,    -1,    -1,    -1,
      -1,   163,    -1,  1227,    -1,  2039,    -1,    -1,    -1,    -1,
      -1,    -1,  1236,    -1,    -1,    -1,    -1,    -1,    -1,  2053,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,  2075,    -1,    -1,    -1,    -1,  1270,    -1,    -1,    -1,
      -1,  1529,  1530,  2087,   572,    -1,    -1,   141,   113,   143,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,  1295,   590,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,    -1,    -1,    -1,    -1,   141,    -1,   606,    -1,
     174,   175,    -1,    -1,    -1,  1319,    -1,    -1,    -1,    -1,
      -1,    -1,   186,    -1,  1328,    -1,    -1,    -1,   163,   164,
      -1,    -1,   167,    -1,    -1,    -1,    -1,  1341,    -1,   174,
     175,    -1,    -1,    -1,  1348,    -1,  1350,    -1,    -1,  1573,
     648,   186,   650,    -1,    -1,   113,   654,    -1,    -1,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   666,   667,
      -1,   129,    -1,   131,    -1,     1,    -1,    -1,   113,   677,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,   690,   145,   146,   147,   148,   149,   150,   151,
     152,   153,   154,   155,    -1,   703,   164,    -1,   160,   167,
      -1,    -1,    -1,    -1,    -1,    -1,  1420,    -1,    -1,  1423,
    1424,    -1,  1426,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   187,    -1,  1441,    -1,    65,
      -1,  1445,    -1,    -1,    -1,  1449,    -1,    -1,    -1,    -1,
    1708,   141,    -1,    -1,    -1,    -1,    -1,  1715,    -1,    -1,
      -1,    -1,    -1,    -1,  1468,  1469,  1470,  1471,  1472,    -1,
      -1,    -1,    -1,   163,   164,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   174,   175,    -1,   113,  1746,  1493,
      -1,    -1,    -1,  1497,  1498,    -1,   186,    -1,   796,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1511,    -1,    -1,
      -1,    -1,    -1,    -1,   812,    -1,    -1,    -1,    -1,    -1,
      -1,   147,    -1,    -1,    -1,    -1,  1750,    -1,    -1,    -1,
     828,   157,    -1,    -1,   832,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   840,   841,    -1,    -1,    -1,   845,  1552,    -1,
     176,    -1,    -1,  1557,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   860,    -1,   862,    -1,    -1,   108,   866,   867,
     868,   869,   113,  1577,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,   886,   215,
      -1,    -1,  1850,  1851,    -1,    -1,    -1,    -1,    -1,  1857,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1611,    -1,    -1,
      -1,    -1,  1870,    -1,    -1,    -1,  1620,    -1,    -1,    -1,
    1624,    -1,  1880,    -1,  1882,    -1,    -1,    -1,    -1,    -1,
      -1,   257,    -1,    -1,    -1,    -1,   934,  1895,    -1,  1897,
    1898,  1899,    -1,    -1,    -1,    -1,    -1,    -1,   946,    -1,
      -1,    -1,   950,    -1,    -1,    -1,    -1,   955,    -1,    -1,
     286,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   966,    -1,
      -1,   297,    -1,    -1,    -1,    -1,     1,  1681,    -1,    -1,
     978,    -1,    -1,   981,    -1,    -1,    -1,    -1,    -1,   315,
      -1,    -1,    -1,  1951,    -1,    -1,   322,  1955,    -1,    -1,
     326,  1705,  1960,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1947,    -1,    -1,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    -1,    60,    61,    -1,    63,    -1,
      -1,    -1,   368,  1041,    -1,   371,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,  2014,    -1,   384,    -1,
      -1,    -1,   388,    -1,    -1,    -1,    -1,  1991,  2026,    -1,
      -1,    -1,  2030,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,  2045,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,    -1,   133,   134,
      -1,    -1,    -1,  1111,    -1,    -1,   141,  1115,    -1,    -1,
    1824,    -1,  1120,  1121,    -1,    -1,  1124,    -1,    -1,    -1,
      -1,  2055,  2090,    -1,    -1,    -1,  1134,    -1,   163,    -1,
      -1,   166,   167,  1141,    -1,    -1,    -1,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,  2081,    -1,  2083,
      -1,    -1,    -1,    -1,  1868,    -1,    -1,  1871,    -1,  2127,
      -1,    -1,   498,    -1,  2132,    -1,    -1,    -1,  1176,    -1,
      -1,    -1,  1886,    13,    14,    15,    16,    17,    -1,    -1,
      -1,  1189,    -1,    -1,    -1,  1193,    -1,  2121,    -1,  1197,
    2158,    -1,    -1,  2161,    -1,  2163,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1218,    -1,    -1,    -1,  2182,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1236,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   572,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2186,    -1,   590,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1270,    -1,    -1,    -1,    -1,  1981,    -1,    -1,
     606,    -1,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,  1295,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,  2023,
      -1,  1319,   648,    -1,   650,    -1,    -1,    -1,   654,    -1,
      -1,    -1,    -1,   163,   164,    -1,    -1,    -1,    -1,    -1,
     666,   667,    -1,  1341,   174,   175,    -1,    -1,    -1,  2053,
    1348,   677,  1350,    -1,  2058,    -1,   186,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   690,    -1,    -1,    -1,    -1,    -1,
      -1,  2075,    -1,    -1,    -1,    -1,    -1,   703,    -1,    -1,
      -1,    -1,    -1,  2087,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1420,    -1,    -1,  1423,  1424,    -1,  1426,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1441,    -1,    -1,    -1,  1445,    -1,    -1,
      -1,  1449,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    65,
     796,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,   812,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1493,    -1,    93,    -1,  1497,
    1498,    -1,   828,    -1,    -1,    -1,   832,    -1,    -1,    -1,
      -1,    -1,    -1,  1511,   840,   841,    -1,   113,    -1,   845,
      -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   860,    -1,   862,    -1,    -1,    -1,
     866,   867,   868,   869,    -1,    -1,    65,    -1,    -1,    -1,
      -1,   147,    -1,    -1,  1552,   151,    -1,    -1,    -1,  1557,
     886,   157,    -1,    -1,   160,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,  1577,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   113,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   201,    -1,    -1,   934,    -1,
      -1,    -1,    -1,  1611,    -1,    -1,    -1,    -1,   214,    -1,
     946,    -1,  1620,    -1,   950,    -1,  1624,    -1,   147,   955,
      -1,    -1,   151,    -1,    -1,    -1,    -1,    -1,   157,    -1,
     966,   160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   978,    -1,    -1,   981,    -1,    -1,    -1,    -1,
      -1,   257,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   274,    -1,
     276,    -1,   201,    -1,    -1,    -1,   282,    -1,    -1,    -1,
     286,    -1,    -1,    -1,    -1,   214,    -1,    -1,    -1,    -1,
      -1,   297,    -1,    -1,    -1,    -1,    -1,  1705,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1041,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   320,    -1,   322,    -1,    -1,    -1,
     326,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   257,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     346,    -1,    -1,    -1,    -1,   274,    -1,   276,    -1,    -1,
      -1,    -1,    -1,   282,    -1,    -1,    -1,   286,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,   297,    -1,
      -1,    -1,    -1,    -1,    -1,  1111,    -1,    -1,    -1,  1115,
      -1,    -1,    -1,    -1,  1120,  1121,    -1,    -1,  1124,    -1,
      -1,   320,    -1,   322,    -1,    -1,    -1,   326,  1134,    -1,
      -1,    -1,    -1,    -1,    -1,  1141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   346,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   431,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1189,    -1,    -1,    -1,  1193,    -1,   465,
      -1,  1197,   113,  1871,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,  1886,    -1,
      -1,    -1,  1218,    -1,    -1,    -1,    -1,    -1,   494,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1236,    -1,   431,    -1,    -1,    -1,    -1,    -1,    -1,   515,
      -1,    -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   174,   175,    -1,    -1,    -1,    -1,    -1,
      -1,   537,    -1,    -1,  1270,   186,   465,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1295,
      -1,    -1,    -1,    68,    -1,   494,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1981,    -1,    -1,    -1,   583,   584,    -1,
      -1,    -1,    -1,  1319,    -1,    -1,   515,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     606,    -1,    -1,    -1,   109,  1341,    -1,    -1,   537,    -1,
      -1,    -1,  1348,    -1,  1350,  2023,   121,    -1,   123,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,   650,  2053,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   583,   584,    -1,    -1,    -1,   164,
      -1,    -1,   167,   168,    -1,    -1,    -1,  2075,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   606,    -1,  2087,
      -1,    -1,    -1,    -1,  1420,    -1,    -1,  1423,  1424,    -1,
    1426,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    65,    -1,
      -1,    -1,    -1,    -1,    -1,  1441,   712,    -1,    -1,  1445,
      -1,    -1,    -1,  1449,   720,   220,    -1,   723,    -1,   725,
     726,   650,   728,    -1,    -1,    -1,    93,    -1,    -1,    -1,
      -1,   737,    -1,    -1,   740,   741,   742,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,  1493,    -1,    -1,
      -1,  1497,  1498,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1511,    -1,    -1,    -1,    -1,
     147,    -1,    -1,   712,   151,    -1,   291,    -1,   293,   294,
     157,   720,    -1,   160,   723,    -1,   725,   726,    -1,   728,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   737,    -1,
      -1,   740,   741,   742,   820,    79,  1552,    -1,    -1,    -1,
     325,  1557,    -1,    -1,    -1,   330,    -1,    -1,    -1,    -1,
      -1,   336,    -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,
      -1,  1577,    -1,    -1,    -1,    -1,    -1,   214,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,    -1,   370,    -1,    -1,    -1,    -1,
     375,    -1,   377,    -1,    -1,  1611,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,  1620,    54,    -1,    -1,  1624,    -1,
     257,   820,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,    -1,    -1,    -1,    -1,    -1,   274,    -1,   276,
     174,   175,    -1,    82,    -1,   282,    -1,   422,    -1,   286,
      -1,    -1,   186,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     297,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   950,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   320,    -1,   322,    -1,    -1,    -1,   326,
      -1,    -1,   131,    -1,   970,    -1,    -1,    -1,    -1,  1705,
      -1,   476,    -1,    -1,    -1,   144,    -1,   146,    -1,   346,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   504,
      -1,   506,   507,    -1,    -1,    -1,    -1,    -1,   177,    -1,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   523,    -1,
      -1,   950,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1040,   204,    -1,    -1,    -1,    -1,
      -1,   970,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   431,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   581,    -1,   247,    -1,
      -1,   586,   251,   588,    54,   254,   255,    -1,    -1,   258,
      -1,    -1,   261,   262,    -1,   264,    -1,   266,   465,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   611,    -1,   613,   614,
      -1,  1040,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     625,    -1,    -1,    -1,    -1,    -1,    -1,   494,    -1,    -1,
      -1,    -1,  1868,   638,    -1,  1871,    -1,    -1,    -1,    -1,
      -1,    -1,   647,    -1,    -1,    -1,    -1,    -1,   515,    -1,
    1886,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,   669,    -1,   671,   672,   337,    -1,
     537,   340,    -1,    -1,   144,    -1,   146,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   693,   694,
      -1,    -1,    -1,    -1,    -1,   700,   365,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,    -1,    -1,
      -1,   380,    -1,    -1,    -1,    -1,   583,   584,    -1,    -1,
      -1,  1227,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1236,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   606,
      -1,    -1,    -1,    -1,    -1,  1981,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1262,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1270,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   650,   254,   255,    -1,  2023,   258,  1295,
      -1,   261,   262,    -1,   264,    -1,   266,    -1,  1227,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1312,  1236,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   485,  2053,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1262,    -1,    -1,    -1,    -1,    -1,  2075,
      -1,  1270,    -1,    -1,    -1,   712,    -1,    -1,    -1,  1355,
      -1,  2087,    -1,   720,    -1,    -1,   723,    -1,   725,   726,
      -1,   728,    -1,    -1,    -1,    -1,  1295,    -1,    -1,    -1,
     737,    -1,    -1,   740,   741,   742,   545,    -1,    -1,    -1,
      -1,    -1,    -1,  1312,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   561,    -1,    -1,   365,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
     380,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1355,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   820,    -1,    -1,    -1,    -1,    -1,    -1,
     965,    -1,  1468,  1469,  1470,  1471,  1472,  1473,  1474,    -1,
      -1,    -1,    -1,    -1,    -1,   980,   645,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   991,  1493,    -1,    -1,
      -1,  1497,  1498,    -1,    -1,  1000,   152,    -1,    -1,   155,
      -1,    -1,  1508,    -1,    -1,  1511,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,   485,   685,   686,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   190,    -1,   705,    -1,   707,  1468,
    1469,  1470,  1471,  1472,  1473,  1474,   202,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1493,    -1,    -1,    -1,  1497,  1498,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1508,
     236,    -1,  1511,   950,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   561,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1105,    -1,    -1,   970,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   278,    -1,    -1,    -1,  1131,    -1,    -1,    -1,
      -1,   287,   288,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   298,    -1,    -1,    -1,    -1,   816,   817,    -1,
      -1,    -1,    -1,    -1,   823,    -1,    -1,   313,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1674,    -1,
      -1,    -1,    -1,  1040,    -1,    -1,    -1,    -1,    -1,   848,
      -1,    -1,   851,   852,    -1,   854,    -1,   856,   857,    -1,
      -1,    -1,   348,    -1,    -1,    -1,    -1,    -1,    -1,  1705,
     356,   357,    -1,    -1,    -1,   361,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   685,   686,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     899,    -1,    -1,    -1,   903,   705,    -1,   707,   907,    -1,
      -1,    -1,    -1,    -1,   400,  1674,    -1,   403,    -1,    -1,
     406,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1705,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1790,    -1,    -1,    -1,    -1,  1294,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1323,    -1,
     989,    -1,    -1,    -1,  1329,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   501,   816,   817,    -1,    -1,
      -1,    -1,    -1,   823,    -1,    -1,    -1,   513,   514,    -1,
    1227,  1790,    -1,    -1,    -1,  1871,    -1,    -1,    -1,  1236,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   848,    -1,
      -1,   851,   852,    -1,   854,    -1,   856,   857,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1262,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1270,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1295,   899,
      -1,    -1,    -1,   903,    -1,    -1,    -1,   907,    -1,    -1,
      -1,    -1,  1871,    -1,    -1,  1312,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1480,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1355,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   652,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   661,  1175,    -1,  1177,    -1,
      -1,  1180,    -1,    -1,  1183,    -1,    -1,  2023,  1187,   989,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1539,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2053,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     716,   176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2087,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2023,    -1,    -1,    -1,    -1,   214,
     215,  1468,  1469,  1470,  1471,  1472,  1473,  1474,    -1,    -1,
      55,   767,    -1,    58,    -1,    60,    61,    -1,    63,    -1,
      -1,    -1,    -1,    -1,  2053,    -1,  1493,    -1,    -1,    -1,
    1497,  1498,    -1,    -1,    -1,    80,   251,    -1,    -1,    -1,
      -1,  1508,    -1,   258,  1511,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1325,    -1,  2087,    -1,
    1130,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,    -1,   133,   134,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,  1704,
      -1,    -1,    -1,   859,    -1,  1175,    -1,  1177,    -1,    -1,
    1180,    -1,    -1,  1183,   870,    -1,    -1,  1187,   163,    -1,
      -1,   166,   167,    -1,    -1,   340,  1395,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,  1406,    -1,    -1,
    1409,   186,  1411,  1412,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   368,   369,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   388,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   949,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1674,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1841,    -1,  1705,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1853,    -1,
      -1,  1520,    -1,   468,    -1,  1325,  1012,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     485,   486,    -1,   488,   489,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   498,    -1,    -1,    -1,   502,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     515,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1790,    -1,  1395,    -1,    -1,    -1,    -1,
    1935,   546,    -1,    -1,    -1,   550,  1406,    -1,    -1,  1409,
      -1,  1411,  1412,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1619,   431,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1966,  1967,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   590,    -1,    -1,  1647,  1984,
      -1,    -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   215,    -1,    -1,
      -1,    -1,    -1,    -1,  1871,    -1,    -1,    -1,    -1,    -1,
    1679,    -1,    -1,   231,    -1,   233,  1685,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   646,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1520,   666,   667,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   677,    -1,    -1,    -1,   681,    -1,    -1,    -1,
      -1,    -1,    -1,   688,    -1,   690,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1242,  1243,  1244,    -1,
      -1,    -1,  1761,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   583,   584,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2118,    -1,    -1,    -1,   335,    -1,    -1,
    1276,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1805,  1806,    -1,    -1,
      -1,  1297,    -1,    -1,    -1,    -1,    -1,  1303,    -1,  1619,
      -1,  1307,    -1,    -1,    -1,    -1,  2023,    -1,    -1,    -1,
      -1,  1830,  1831,    -1,    -1,    -1,    -1,    -1,    -1,  1838,
      -1,    -1,    -1,    -1,  1843,    -1,    -1,  1647,    -1,    -1,
      -1,   796,    -1,    -1,    -1,    -1,  2053,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   812,   813,    -1,
      -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,   823,   824,
      -1,   826,   827,    -1,    -1,    -1,    -1,    -1,    -1,   109,
    2087,    -1,    -1,    -1,    -1,   840,   841,    -1,    -1,    -1,
     845,    -1,   847,   848,    -1,    -1,    -1,    -1,    -1,   854,
     720,    -1,    -1,   723,    -1,   860,    -1,   862,   728,    -1,
      -1,   866,   867,   868,   869,    -1,    -1,   737,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1937,    -1,
     160,   886,    -1,   888,   164,    -1,   756,   892,    -1,    -1,
      -1,    -1,    -1,    -1,   899,   900,   176,    -1,   903,   904,
      -1,  1761,   907,   908,    -1,    -1,    -1,    -1,    -1,   914,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   526,    -1,
      -1,   201,   792,    -1,   532,    -1,    -1,    -1,    -1,   537,
      -1,    -1,    -1,    -1,    -1,   215,    -1,  1483,  1484,    -1,
     220,    -1,    -1,    -1,    -1,  1805,  1806,    -1,    -1,  2008,
     955,   956,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1830,  1831,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1843,    -1,    -1,   991,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   276,    -1,  2057,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1571,    -1,    -1,    -1,    -1,
      -1,   639,    -1,    -1,    -1,  1040,  1041,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   336,    -1,    -1,   667,
      -1,    -1,    -1,    -1,    -1,    -1,   346,    -1,    -1,    -1,
      -1,    -1,   680,    -1,    -1,    -1,    -1,  1937,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   368,    -1,
     370,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   717,
      -1,    -1,    -1,    -1,    -1,  1120,  1121,    -1,    -1,  1124,
    1125,    -1,   730,    -1,    -1,    -1,  1131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1689,    -1,    -1,   754,   755,  2008,    -1,
     758,   431,   760,    -1,    -1,    -1,    -1,    -1,   766,    -1,
     768,   769,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1176,    -1,    -1,    -1,  1180,  1181,    -1,  1183,  1184,
      -1,    -1,  1187,  1188,    -1,   465,    -1,    -1,   796,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   809,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   820,    -1,    -1,    -1,    -1,    -1,   498,    -1,
      -1,    -1,    -1,    -1,   504,    -1,    -1,    -1,   836,    -1,
      -1,    -1,    -1,   841,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   537,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   873,    -1,    -1,   876,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   887,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   583,   584,    -1,    -1,   915,    -1,    -1,
     590,    -1,    -1,    -1,  1319,    -1,    -1,    -1,    -1,    -1,
    1325,  1326,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1350,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   970,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   980,   981,    -1,    -1,    -1,    -1,    -1,    -1,
     988,    -1,    -1,    -1,    -1,    -1,    -1,   667,    -1,   669,
    1395,  1396,  1262,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1406,  1407,    -1,  1409,    -1,    -1,    -1,    -1,    -1,
     690,    -1,    -1,    -1,    -1,  1420,    -1,    -1,  1423,  1424,
      -1,  1426,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   712,  1041,    -1,    -1,    -1,    -1,    -1,    -1,
     720,  1049,    -1,   723,    -1,   725,   726,    -1,   728,    -1,
    1058,    -1,    -1,    -1,    -1,    -1,    -1,   737,    -1,    -1,
     740,   741,   742,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1099,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   796,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     820,    -1,    -1,    -1,    -1,    -1,    -1,  1552,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   841,    -1,    -1,  1172,    -1,  1174,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1580,    -1,    -1,    -1,    -1,
     860,    -1,   862,    -1,    -1,    -1,   866,   867,   868,   869,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   886,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1493,  1494,    -1,    -1,  1497,  1498,    -1,
      -1,    -1,    -1,  1503,    -1,    -1,    -1,  1507,    -1,  1509,
      -1,  1511,    -1,    -1,    -1,    -1,    -1,    -1,  1256,  1257,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1668,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   955,    -1,    -1,    -1,    -1,
    1685,    -1,    -1,    -1,    -1,   965,    -1,    -1,    -1,    -1,
     970,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     980,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1323,    -1,    -1,    -1,    -1,
      -1,  1329,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1345,    -1,    -1,
      -1,    -1,  1350,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1368,  1041,    -1,  1371,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1386,    -1,
      -1,    -1,    -1,    -1,    -1,  1655,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    55,    -1,    -1,    58,    -1,    60,    61,    -1,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1823,  1824,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
    1700,    -1,    -1,    -1,  1839,    -1,    -1,    -1,    -1,    -1,
    1120,  1121,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1720,  1721,  1460,  1461,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,  1486,   133,
     134,  1751,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1176,  1505,    -1,    -1,
    1508,    -1,   156,   157,   158,   159,    -1,    -1,    -1,   163,
     164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
    1935,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1943,    -1,
      -1,    -1,    -1,    -1,  1552,    -1,    -1,  1227,    -1,    -1,
      -1,    -1,    -1,  1561,  1562,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1578,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1983,    -1,
      -1,    -1,  1262,  1591,    -1,  1855,    -1,  1595,    -1,    -1,
      -1,    -1,    -1,  1863,    -1,  1865,    -1,    -1,  1868,  1869,
      -1,  1871,    -1,  2008,  2009,    -1,  1876,  2012,    -1,    -1,
      -1,    -1,    -1,    -1,  1294,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1312,    -1,    -1,    -1,    -1,    -1,    -1,  1319,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2057,  2058,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1674,    -1,    -1,    -1,
    1350,    -1,  1680,    -1,    -1,  1355,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1965,    -1,    -1,    -1,    -1,
      -1,    -1,  1972,  1973,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1995,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1741,    -1,    -1,    -1,    -1,    -1,    -1,
    1420,    -1,    -1,  1423,  1424,    -1,  1426,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2031,    -1,  2033,    -1,    -1,  2036,  2037,    -1,    -1,
      -1,    -1,  1780,  2043,  2044,    -1,    -1,    -1,    -1,    -1,
    1788,    -1,    -1,  1791,    -1,    -1,    -1,    -1,  1468,  1469,
    1470,    -1,    -1,  1473,  1474,    -1,     1,    -1,    -1,    -1,
    1480,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1817,
      -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1508,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2110,  2111,  2112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    -1,    60,    61,    -1,    63,  1539,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2138,  2139,
    2140,    -1,  1552,    78,    -1,    80,    81,    -1,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,    -1,   108,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,    -1,   133,   134,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,   163,    -1,
      18,   166,   167,    -1,    -1,    -1,   171,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   188,  1992,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1674,    -1,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    -1,    80,    81,  1704,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,    -1,
     108,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   162,   163,    -1,    -1,   166,   167,
    1790,    -1,    18,   171,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     188,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,
      -1,    -1,    58,    -1,    60,    61,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    78,  1853,    80,    81,    -1,    83,    -1,    -1,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,    -1,   108,    -1,   110,   111,  1886,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,    -1,   171,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   188,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1981,    -1,    -1,  1984,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,
      78,    79,    80,    81,    -1,    83,    -1,    -1,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,    -1,
     108,    -1,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   162,   163,    -1,    -1,   166,   167,
      -1,    -1,    -1,   171,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     188,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    -1,    -1,    -1,    76,    -1,    78,    79,    80,    81,
      -1,    83,    -1,    -1,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,    -1,   108,    -1,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,   171,
      -1,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   188,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    -1,    -1,    -1,
      76,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,   157,   158,   159,    -1,    -1,    -1,   163,   164,   165,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   188,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,    79,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   156,   157,   158,   159,
      -1,    -1,    -1,   163,   164,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,   173,   174,   175,   176,   177,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   188,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    -1,
      -1,    -1,    76,    -1,    -1,    79,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
     174,   175,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,   186,    -1,   188,    -1,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    -1,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,   188,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,
      -1,    -1,    -1,   171,    -1,    -1,   174,   175,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   186,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
     174,   175,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   186,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,   165,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,   174,   175,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   186,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,
      -1,    -1,    -1,   171,    -1,    -1,   174,   175,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   186,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,
     175,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   186,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    -1,    -1,    -1,    76,    -1,    -1,    79,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,   165,   166,   167,    -1,    -1,    -1,
      -1,    -1,   173,   174,   175,   176,   177,   178,   179,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    -1,
      -1,    -1,    76,    -1,    -1,    79,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,   173,   174,   175,   176,   177,   178,   179,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,   173,   174,   175,   176,   177,   178,   179,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    -1,    -1,
      -1,    76,    -1,    -1,    79,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,
     175,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   162,    -1,     1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,   174,   175,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   162,    -1,     1,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,    -1,
       1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
     174,   175,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   162,    -1,     1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,   174,   175,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   162,    -1,     1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,   174,   175,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,    -1,     1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,
     175,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
       5,    -1,    -1,    -1,    -1,    -1,    -1,    79,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    -1,    60,    61,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    79,    80,    -1,     5,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,   174,   175,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,    55,   133,   134,
      58,    -1,    60,    61,    -1,    63,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    80,    -1,    -1,    -1,    -1,    -1,   163,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,
     171,    -1,    -1,   174,   175,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,   186,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
     186,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,   174,   175,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    20,   186,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     186,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    -1,    -1,    -1,    76,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     112,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     162,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,   174,   175,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   112,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   162,    -1,    -1,    -1,   166,   167,    -1,
       3,    -1,     5,    -1,    -1,   174,   175,    10,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,
      -1,    -1,    -1,   166,   167,    -1,     3,    -1,     5,    -1,
      -1,   174,   175,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,    -1,   166,
     167,    -1,     3,    -1,     5,    -1,    -1,   174,   175,    10,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   112,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   162,    -1,    -1,    -1,   166,   167,    -1,     3,    -1,
       5,    -1,    -1,   174,   175,    10,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,
     175,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,    -1,    13,    14,    15,
      16,    17,   174,   175,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
     174,   175,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,   165,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,   174,   175,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,   174,   175,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,   174,   175,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
     174,   175,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,   174,   175,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,   174,   175,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,   174,   175,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
     116,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,
      -1,    78,    -1,    80,    81,    -1,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    -1,    -1,   100,   101,   102,   103,   104,   105,   106,
      -1,   108,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,    -1,   133,   134,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,
     167,    -1,    -1,    -1,   171,    -1,   173,   174,   175,   176,
     177,   178,   179,    -1,    55,    -1,    -1,    58,    -1,    60,
      61,   188,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    80,
      81,    -1,    83,    -1,    -1,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    -1,    -1,   100,
     101,   102,   103,   104,   105,   106,    -1,   108,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,
     171,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   188,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   166,    -1,    -1,    -1,    -1,   171,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,    -1,    -1,    -1,    -1,   171,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,
      -1,    -1,    -1,    -1,   171,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,    97,
      -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,
      -1,    -1,    -1,   171,    -1,   173,   174,   175,   176,   177,
     178,   179,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,
     171,    -1,   173,   174,   175,   176,   177,   178,   179,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,
     167,    -1,    -1,    -1,   171,    -1,   173,   174,   175,   176,
     177,   178,   179,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,
      -1,   171,    -1,   173,   174,   175,   176,   177,   178,   179,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,    -1,
     133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
     173,   174,   175,   176,   177,   178,   179,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,   178,
     179,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    55,
      -1,    -1,    58,    -1,    60,    61,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     143,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,   157,   158,   159,    -1,    -1,    -1,   163,   164,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      79,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,   115,   116,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,
      -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   165,   166,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,
     175,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,   174,   175,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,   174,   175,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   143,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    13,
      14,    15,    16,    17,    -1,    79,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,   115,   116,    57,    -1,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,   143,
     144,    -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,   166,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,   143,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    13,    14,    15,    16,    17,    18,
      79,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,   115,   116,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,   174,   175,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,   174,   175,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,   174,   175,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
     174,   175,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,   174,   175,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,   174,   175,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,
     175,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    18,    79,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,   115,   116,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,   174,   175,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,   174,   175,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
     174,   175,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,   174,   175,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,   174,   175,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   166,    -1,    -1,    13,    14,    15,    16,    17,   174,
     175,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,
      13,    14,    15,    16,    17,   174,   175,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,    -1,    13,    14,    15,    16,
      17,   174,   175,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,   173,   174,   175,   176,   177,   178,   179,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,   173,   174,   175,   176,   177,   178,   179,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,   166,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    13,    14,    15,    16,    17,    18,
      79,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,   115,   116,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,   166,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    79,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,   115,   116,    57,    -1,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   166,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,   143,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      20,   143,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    20,    79,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    55,    59,    -1,    58,
      62,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    80,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,   143,   141,    -1,    55,    -1,    -1,    58,    -1,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,    80,
      -1,    -1,   171,    -1,   173,   174,   175,   176,   177,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,    55,   133,   134,    58,    -1,    60,    61,    -1,    63,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,
     171,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,    55,   133,
     134,    58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,
      -1,    -1,   166,   167,    -1,    -1,    -1,   171,    -1,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,
     177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,
      -1,   171,    -1,   173,   174,   175,   176,   177,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,    55,
     133,   134,    58,    -1,    60,    61,    -1,    63,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     163,    -1,    -1,   166,   167,    -1,    -1,   170,    -1,    -1,
     173,   174,   175,   176,   177,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,    55,   133,   134,    58,
      -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,    -1,   171,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,   171,
      -1,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,    55,   133,   134,
      58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,    -1,
     165,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,    55,   133,   134,    58,    -1,    60,
      61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    55,    -1,    -1,    58,
     141,    60,    61,    -1,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,   163,   164,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,    55,   133,   134,
      58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,   164,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,    55,   133,   134,    58,    -1,    60,
      61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,    55,   133,   134,    58,    -1,    60,    61,    -1,    63,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,    55,   133,
     134,    58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,
     177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   163,   164,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,   173,   174,   175,   176,   177,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,    55,
     133,   134,    58,    -1,    60,    61,    -1,    63,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     163,   164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
     173,   174,   175,   176,   177,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,    55,   133,   134,    58,
      -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,    55,   133,   134,
      58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,   164,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,    55,   133,   134,    58,    -1,    60,
      61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,    55,   133,   134,    58,    -1,    60,    61,    -1,    63,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,    55,   133,
     134,    58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,
      -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,
     177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,   173,   174,   175,   176,   177,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,    55,
     133,   134,    58,    -1,    60,    61,    -1,    63,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     163,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
     173,   174,   175,   176,   177,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,    55,   133,   134,    58,
      -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,   178,
     179
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   190,   416,   417,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    57,    59,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    76,    79,    80,   108,   112,   113,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   128,
     141,   143,   163,   164,   166,   167,   174,   175,   186,   188,
     193,   194,   195,   208,   303,   304,   305,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   317,   320,   322,   323,
     324,   325,   326,   327,   328,   329,   330,   331,   333,   335,
     336,   337,   339,   340,   344,   345,   346,   347,   348,   350,
     356,   357,   358,   359,   370,   375,   408,   411,   421,   427,
     429,   435,   439,   444,   445,   446,   447,   448,   449,   450,
     451,   477,   495,   496,   497,   498,     0,   190,   113,   194,
     208,   307,   309,   320,   323,   326,   336,   340,   345,   127,
     163,    65,    68,    69,    71,   163,   163,   373,   433,   434,
     435,   332,   333,   115,   116,   194,   196,   409,   410,   196,
     163,   421,   163,   163,     4,   113,   115,   116,   324,   329,
     330,   163,   163,   208,   434,   439,   445,   446,   447,   449,
     450,   451,   115,   347,   168,   190,   194,   167,   310,   320,
     323,   444,   448,   494,   495,   498,   499,   188,   188,   191,
     160,   171,   187,   232,   391,    96,   169,   428,   108,   196,
     432,   169,   169,   169,   188,   115,   116,   163,   208,   315,
     316,   439,   440,   441,   442,   443,   444,   448,   452,   453,
     454,   455,   456,   457,   458,   459,   460,   466,     3,    53,
      54,    56,    62,   338,     3,   167,   208,   309,   310,   324,
     328,   330,   341,   346,   424,   444,   448,   498,    76,   307,
     309,   323,   336,   340,   345,   425,   444,   448,    72,   329,
     329,   324,   330,   318,   329,   330,   338,   357,   324,   329,
     324,   166,   433,   169,   191,   163,   171,   240,   433,   433,
       3,   298,   299,   314,   317,   323,   327,   328,   167,   320,
     323,   496,   196,   196,   421,   187,   323,   163,   208,   430,
     439,   440,   444,   453,   457,   167,   208,   310,   498,   422,
     423,    64,    72,    73,    74,    75,   167,   185,   196,   397,
     399,   403,   405,   406,   346,    64,   165,   167,   208,   319,
     323,   327,   335,   336,   342,   343,   344,   345,   349,   356,
     357,   375,   385,   387,   477,   490,   491,   492,   493,   498,
     499,   433,   115,   116,   178,   194,   346,   374,   466,   435,
     163,   404,   405,   163,    13,    95,   163,   196,   436,   437,
     438,   127,   197,   198,    55,    58,    60,    61,    63,    80,
     110,   111,   113,   114,   125,   126,   129,   130,   131,   133,
     134,   163,   167,   173,   176,   177,   178,   179,   192,   193,
     197,   199,   202,   207,   208,   209,   210,   213,   214,   215,
     216,   217,   218,   219,   220,   221,   222,   223,   224,   225,
     234,   346,   165,   167,   207,   208,   224,   229,   320,   346,
     389,   390,   407,   494,   499,   436,   323,   445,   446,   447,
     449,   450,   451,   165,   165,   165,   165,   165,   165,   165,
     115,   167,   194,   320,   477,   496,   167,   174,   208,   229,
     309,   310,   319,   321,   323,   336,   343,   345,   382,   383,
     384,   386,   387,   490,   498,   168,   163,   167,   444,   448,
     498,   163,   169,   113,   166,   167,   171,   193,   195,   229,
     392,   393,   394,   395,   396,    22,   392,   163,   196,   240,
     163,   163,   194,   430,   194,   434,   439,   441,   442,   443,
     452,   454,   455,   456,   458,   459,   460,   323,   440,   453,
     457,   169,   432,   167,   433,   474,   477,   432,   433,   433,
     428,   298,   163,   433,   474,   432,   433,   433,   428,   433,
     433,   323,   430,   163,   163,   322,   323,   320,   323,   168,
     190,   320,   494,   499,   432,   348,   171,   428,   298,   196,
     196,   391,   309,   328,   426,   444,   448,   171,   428,   298,
     409,   323,   336,   323,   323,   115,   347,   115,   116,   194,
     346,   351,   409,   144,   194,   323,   379,   380,   384,   385,
     388,   162,   190,   240,   314,   188,   444,   457,   323,   190,
     432,   163,   432,   191,   229,   434,   439,   323,   163,   196,
     419,   171,   163,   196,   171,   196,   144,   174,   175,   402,
     165,   169,   196,   406,   165,   433,   168,   190,   321,   323,
     336,   343,   345,   489,   490,   498,   499,   163,   167,   175,
     187,   208,   477,   479,   480,   481,   482,   483,   484,   501,
     208,   349,   498,   323,   343,   329,   324,   433,   165,   321,
     323,   491,   321,   477,   491,   194,   374,   466,   371,   171,
     374,   397,   187,   397,   436,   165,   169,   163,   165,   127,
     163,   207,   163,   163,   207,   163,   163,   210,   163,   207,
     163,   113,   115,   116,   324,   329,   330,   163,   207,   207,
      19,    21,    92,   167,   176,   177,   211,   212,   229,   236,
     240,   359,   389,   498,   169,   190,   163,   199,   167,   172,
     167,   172,   130,   132,   133,   134,   163,   166,   167,   171,
     172,   210,   210,   180,   174,   181,   182,   176,   177,   135,
     136,   137,   138,   183,   184,   139,   140,   175,   173,   185,
     141,   142,   186,   165,   169,   166,   190,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   187,   231,
     232,   233,   163,   208,   470,   471,   472,   473,   474,   165,
     169,   165,   165,   165,   165,   165,   165,   165,   163,   433,
     474,   477,   163,   474,   477,   163,   190,   163,   320,   496,
     168,   190,   191,   167,   191,   163,   175,   208,   439,   461,
     462,   463,   464,   465,   466,   467,   468,   469,   144,   498,
     169,   191,   169,   191,   196,   196,   163,   190,   190,   190,
     190,   167,   195,   190,   393,   170,   169,   500,   392,   166,
     167,   170,   396,   407,   163,   197,   190,   187,   439,   441,
     442,   443,   452,   454,   455,   456,   458,   459,   460,   165,
     165,   165,   165,   165,   165,   165,   165,   165,   165,   440,
     453,   457,   433,   187,   168,   190,   391,   240,   428,   379,
     391,   240,   430,   236,   390,   236,   390,   430,   115,   167,
     419,   240,   428,   432,   171,   171,   428,   298,   419,   240,
     428,   353,   354,   352,   171,   165,   169,   165,   169,    77,
     300,   301,   188,   174,   229,   190,   439,   421,   419,   196,
     168,     1,   307,   309,   321,   323,   412,   413,   414,   415,
     163,   401,   399,   400,    85,   334,   194,   321,   477,   491,
     323,   327,   498,   379,   480,   481,   482,   168,   190,    18,
     229,   323,   479,   501,   433,   433,   477,   321,   489,   499,
     323,   194,   321,   491,   433,   171,   433,   374,    10,   173,
     374,   376,   377,   171,   165,   390,   165,   165,   437,   186,
     226,   227,   228,   229,   188,   389,   499,   199,   389,   167,
     389,   390,   389,   499,   229,   389,   165,   389,   389,   389,
     168,   190,   165,   176,   177,   212,    18,   325,   165,   169,
     165,   174,   175,   165,   164,   229,   235,   229,   171,   229,
     194,   229,   194,   125,   167,   194,   226,   125,   167,   196,
     359,   229,   226,   194,   210,   213,   213,   213,   214,   214,
     215,   215,   216,   216,   216,   216,   217,   217,   218,   219,
     220,   221,   222,   170,   236,   197,   167,   194,   229,   171,
     229,   379,   471,   472,   473,   323,   470,   433,   433,   229,
     390,   163,   433,   474,   477,   163,   474,   477,   379,   379,
     190,   190,   168,   168,   163,   439,   462,   463,   464,   467,
      18,   323,   461,   465,   163,   433,   483,   501,   433,   433,
     501,   163,   433,   483,   433,   433,   191,   225,   196,   383,
     386,   168,   386,   387,   168,   501,   501,   144,   381,   382,
     383,   381,   383,   381,   196,   190,   224,   225,   229,   431,
     500,   392,   394,   162,   190,   165,   190,   165,   381,   229,
     165,   165,   165,   165,   165,   165,   165,   165,   165,   163,
     433,   474,   477,   163,   433,   474,   477,   163,   433,   474,
     477,   430,    22,   477,   229,   330,   346,   475,   240,   165,
     165,   165,   165,   165,   417,   418,   240,   162,   412,   419,
     240,   428,   418,   240,   171,   171,   171,   360,   144,   384,
     385,   194,   196,   302,    18,    78,    80,    81,    83,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,   100,   101,   102,   103,   104,   105,   106,   108,   115,
     116,   128,   163,   167,   196,   236,   237,   238,   239,   240,
     241,   242,   244,   245,   254,   261,   262,   263,   264,   265,
     266,   271,   272,   275,   276,   277,   278,   279,   280,   281,
     287,   288,   289,   303,   323,   327,   429,    77,   191,   191,
     381,   420,   418,   165,   430,   162,   413,   169,   188,   169,
     188,   407,   187,   398,   398,   321,   491,   167,   174,   208,
     229,   346,   229,   323,   165,   165,   165,   165,     5,   323,
     433,   479,   372,   376,   374,   171,   346,   169,   500,   196,
     376,   171,   165,   194,   165,   169,   165,   165,   169,   165,
     190,   169,   165,   165,   165,   169,   165,   210,   165,   165,
     165,   210,    18,   325,   229,   165,   165,   164,   171,   210,
     168,   169,   191,   226,   168,   168,   125,   129,   131,   195,
     203,   204,   205,   165,   203,   168,   169,   162,   224,   170,
     165,   203,   191,   393,   165,   165,   165,   165,   470,   379,
     379,   165,   165,   381,   381,   467,   165,   165,   165,   165,
     163,   439,   466,   461,   465,   379,   379,   168,   191,   501,
     169,   191,   165,   169,   169,   191,   169,   191,   391,   203,
     144,   179,   191,   191,   162,   392,   229,   433,   381,   433,
     191,   163,   433,   474,   477,   163,   433,   474,   477,   163,
     433,   474,   477,   379,   379,   379,   432,   157,   179,   191,
     476,   169,   191,   420,   162,   418,   240,   420,   360,   360,
     360,     3,     5,    10,    80,   162,   304,   311,   312,   320,
     323,   361,   366,   494,   169,   188,   163,    68,    69,   188,
     240,   303,   429,   163,   163,    18,   238,   163,   163,   188,
     196,   188,   196,   174,   196,   171,   237,   163,   163,   163,
     238,   163,   240,   229,   230,   230,    14,   290,   266,   277,
     170,   188,   191,   242,    85,   188,   196,    98,    99,   270,
     274,   119,   142,   269,   118,   141,   273,   269,   388,   323,
     302,   168,   168,   191,   420,   196,   196,   430,   165,   390,
     404,   404,   190,   191,   191,   191,   229,   163,   433,   483,
     477,   322,     5,   174,   191,   229,   374,   500,   171,   376,
      10,   377,   162,   187,   378,   500,   162,   412,   187,   228,
     319,   194,    85,   200,   201,   389,   210,   210,   210,   210,
     210,   171,   393,   164,   229,   169,   162,   206,   167,   204,
     206,   206,   168,   169,   132,   166,   168,   235,   224,   168,
     500,   163,   433,   474,   477,   165,   165,   191,   191,   165,
     163,   433,   474,   477,   163,   433,   483,   439,   433,   433,
     165,   165,   168,   386,   168,   144,   383,   144,   165,   165,
     191,   225,   225,   168,   168,   191,   191,   165,   379,   379,
     379,   165,   165,   165,   391,   169,   229,   229,   330,   346,
     168,   162,   420,   162,   162,   162,   162,   320,   320,   359,
     367,   494,   320,   366,   163,   355,   188,   188,   188,   163,
     170,   208,   362,   363,   369,   439,   440,   453,   457,   169,
     188,   196,   196,   226,   188,   240,   188,   240,   236,   246,
     303,   305,   308,   314,   323,   327,   236,    87,   165,   246,
     156,   157,   158,   159,   164,   165,   188,   236,   255,   256,
     258,   303,   188,   188,   236,   188,   393,   188,   236,   407,
     236,   255,   120,   121,   122,   123,   124,   282,   284,   285,
     188,   107,   188,    91,   163,   165,   433,   162,   188,   188,
     163,   163,   238,   238,   266,   163,   276,   266,   276,   240,
     188,   165,   162,   402,   168,   168,   168,   191,   379,   229,
     229,   191,   168,   191,   171,   162,   376,   500,   346,   196,
     171,   225,   162,   162,   229,   478,   479,   165,   170,   165,
     169,   170,   393,   500,   235,   130,   203,   204,   167,   204,
     167,   204,   168,   162,   379,   165,   165,   379,   379,   168,
     191,   165,   433,   165,   165,   165,   236,   476,   162,   355,
     355,   355,   362,   163,   208,   364,   365,   474,   485,   486,
     487,   488,   188,   169,   188,   362,   188,   407,   434,   439,
     229,   323,   162,   169,   188,   368,   369,   368,   368,   196,
     165,   165,   236,   323,   165,   163,   238,   165,   156,   157,
     158,   159,   179,   188,   259,   260,   238,   237,   188,   260,
     165,   170,   236,   164,   236,   237,   258,   188,   500,   165,
     165,   165,   165,   240,   284,   285,   163,   229,   163,   197,
       1,   238,   210,   267,   236,    82,   117,   268,   270,    82,
     433,   398,   168,   165,   191,   191,   168,   168,   376,   500,
     162,   378,   393,   165,   229,   201,   229,   500,   162,   168,
     168,   203,   203,   165,   433,   433,   165,   165,   168,   168,
     229,   188,   486,   487,   488,   323,   485,   169,   188,   433,
     433,   188,   165,   439,   433,   238,   238,    84,    85,   171,
     249,   250,   251,   165,   236,    82,   238,   236,   164,   236,
      82,   188,   164,   236,   237,   258,   323,   345,   164,   236,
     238,   256,   260,   260,   188,   236,   162,   171,   251,   238,
     238,   163,   286,   321,   323,   494,   188,   197,   165,   170,
     165,   169,   170,   165,   238,   163,   238,   238,   238,   404,
     168,   168,   500,   162,   500,   162,   168,   168,   165,   165,
     165,   485,   433,   363,    82,     1,   225,   247,   248,   431,
       1,   170,     1,   190,   238,   249,    82,   188,   165,   238,
      82,   188,   179,   179,   238,   237,   260,   260,   188,    64,
     236,   257,   346,   179,   179,    82,   164,   236,   164,   236,
     237,   188,     1,   190,   286,   188,   283,   163,   208,   430,
     485,   194,   170,   188,   167,   197,   291,   292,   293,   210,
     226,   236,   269,   162,   162,   163,   433,   474,   477,   365,
     238,   144,     1,   169,   170,   162,   296,   297,   303,   238,
      82,   188,   238,   236,   164,   164,   236,   164,   236,   164,
     236,   237,   194,   346,   164,   236,   164,   236,   238,   179,
     179,   179,   179,   162,   296,   283,   224,   165,   323,   170,
     113,   163,   165,   170,   169,   165,   165,    82,   265,   379,
     225,   247,   250,   252,   253,   303,   238,   179,   179,   179,
     179,   164,   164,   236,   164,   236,   164,   236,   252,   165,
     240,   291,   168,   225,   188,   291,   293,   238,    82,   165,
     238,   243,   191,   250,   164,   164,   236,   164,   236,   164,
     236,   191,   240,   170,   197,   165,   165,   170,   238,     1,
     238,   162,   243,   162,   197,   294,   163,   188,   294,   169,
     170,   225,   165,   197,   194,   295,   165,   188,   165,   169,
     188,   194
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   189,   190,   191,   192,   192,   192,   192,   192,   193,
     193,   193,   193,   193,   193,   193,   193,   194,   194,   195,
     195,   196,   196,   196,   197,   198,   198,   199,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   200,   200,   201,   201,   202,   202,   202,   202,
     202,   202,   202,   202,   202,   202,   202,   202,   202,   202,
     202,   202,   202,   202,   202,   202,   202,   202,   202,   202,
     203,   203,   204,   204,   204,   204,   204,   204,   204,   205,
     205,   205,   206,   206,   207,   207,   207,   207,   207,   207,
     207,   207,   207,   207,   207,   207,   207,   207,   207,   207,
     207,   207,   208,   208,   208,   209,   209,   209,   209,   210,
     210,   210,   210,   210,   210,   210,   210,   210,   211,   211,
     211,   211,   212,   212,   213,   213,   214,   214,   214,   214,
     215,   215,   215,   216,   216,   216,   217,   217,   217,   217,
     217,   218,   218,   218,   219,   219,   220,   220,   221,   221,
     222,   222,   223,   223,   224,   224,   224,   225,   226,   226,
     227,   227,   228,   228,   228,   229,   229,   229,   230,   230,
     231,   231,   232,   232,   233,   233,   233,   233,   233,   233,
     233,   233,   233,   233,   233,   234,   234,   235,   235,   235,
     235,   236,   236,   237,   237,   238,   238,   238,   238,   238,
     238,   238,   238,   238,   238,   238,   238,   238,   238,   238,
     238,   239,   239,   240,   240,   241,   241,   242,   242,   242,
     242,   242,   243,   243,   243,   244,   245,   245,   245,   245,
     245,   245,   245,   245,   246,   246,   246,   246,   247,   247,
     247,   248,   248,   249,   249,   249,   249,   249,   250,   250,
     251,   252,   252,   253,   253,   254,   254,   254,   254,   254,
     254,   254,   254,   254,   254,   254,   254,   255,   255,   256,
     256,   256,   256,   256,   256,   256,   256,   256,   256,   256,
     256,   256,   256,   256,   256,   256,   256,   256,   256,   256,
     256,   256,   256,   256,   256,   256,   256,   256,   256,   256,
     256,   256,   256,   256,   256,   256,   256,   256,   256,   256,
     256,   256,   256,   256,   257,   257,   257,   258,   258,   258,
     258,   259,   259,   259,   260,   260,   260,   261,   261,   261,
     261,   261,   261,   261,   261,   261,   261,   261,   261,   261,
     261,   261,   261,   261,   261,   261,   261,   262,   262,   263,
     264,   265,   266,   266,   267,   267,   268,   269,   269,   270,
     270,   271,   271,   271,   271,   271,   271,   272,   273,   273,
     274,   275,   275,   276,   276,   277,   277,   277,   278,   279,
     280,   281,   281,   281,   282,   282,   283,   283,   284,   284,
     284,   284,   285,   286,   286,   286,   286,   286,   287,   288,
     288,   289,   289,   289,   289,   289,   290,   290,   291,   291,
     292,   292,   293,   293,   294,   294,   294,   295,   295,   296,
     296,   297,   297,   298,   298,   299,   299,   300,   300,   301,
     301,   302,   302,   303,   303,   303,   304,   304,   305,   305,
     305,   305,   305,   306,   306,   306,   307,   307,   307,   307,
     307,   308,   308,   308,   308,   308,   309,   309,   309,   309,
     310,   310,   311,   311,   311,   312,   312,   312,   312,   312,
     313,   313,   314,   314,   314,   314,   315,   315,   315,   315,
     315,   316,   316,   317,   317,   317,   317,   318,   318,   318,
     319,   319,   319,   320,   320,   320,   321,   321,   321,   322,
     322,   323,   323,   324,   324,   325,   325,   325,   325,   325,
     326,   327,   327,   327,   328,   328,   329,   329,   329,   329,
     329,   329,   329,   329,   329,   330,   331,   331,   331,   331,
     331,   331,   331,   331,   331,   331,   331,   331,   331,   331,
     331,   331,   331,   331,   331,   331,   331,   331,   331,   331,
     331,   331,   331,   331,   331,   331,   331,   331,   331,   331,
     332,   332,   333,   334,   334,   335,   335,   335,   335,   335,
     336,   336,   337,   337,   337,   337,   338,   338,   338,   338,
     338,   338,   339,   339,   339,   339,   340,   341,   340,   340,
     342,   342,   342,   342,   343,   343,   343,   344,   344,   344,
     344,   345,   345,   345,   346,   346,   346,   346,   346,   346,
     347,   347,   347,   348,   348,   349,   349,   351,   350,   352,
     350,   353,   350,   354,   350,   350,   355,   355,   356,   356,
     357,   357,   358,   358,   358,   359,   359,   359,   359,   359,
     359,   359,   359,   360,   360,   361,   361,   361,   361,   361,
     361,   361,   361,   361,   361,   361,   361,   362,   362,   362,
     363,   363,   363,   363,   364,   364,   364,   365,   366,   366,
     367,   367,   368,   368,   369,   370,   370,   371,   370,   370,
     372,   370,   370,   370,   373,   373,   374,   374,   375,   375,
     376,   376,   376,   376,   376,   377,   377,   378,   378,   378,
     379,   379,   379,   379,   380,   380,   380,   380,   381,   381,
     381,   381,   381,   381,   381,   382,   382,   382,   382,   383,
     383,   384,   384,   385,   385,   386,   386,   386,   386,   386,
     387,   387,   387,   387,   387,   388,   388,   389,   389,   389,
     390,   390,   391,   391,   391,   391,   392,   392,   393,   393,
     393,   393,   393,   394,   394,   395,   395,   396,   396,   396,
     396,   396,   397,   397,   398,   398,   400,   399,   401,   399,
     399,   399,   399,   402,   402,   402,   402,   403,   403,   403,
     403,   404,   404,   405,   405,   406,   406,   407,   407,   407,
     407,   408,   408,   408,   409,   409,   410,   410,   411,   411,
     411,   411,   412,   412,   413,   413,   414,   414,   414,   415,
     415,   415,   416,   416,   417,   417,   418,   418,   419,   420,
     421,   421,   421,   421,   421,   421,   421,   421,   421,   421,
     421,   422,   421,   423,   421,   424,   421,   425,   421,   426,
     421,   421,   427,   427,   427,   428,   428,   429,   429,   429,
     429,   429,   429,   429,   429,   429,   429,   430,   430,   430,
     430,   431,   432,   432,   433,   433,   434,   434,   435,   435,
     435,   436,   436,   437,   437,   437,   438,   438,   438,   439,
     439,   439,   440,   440,   440,   440,   441,   441,   441,   441,
     442,   442,   442,   442,   442,   442,   442,   443,   443,   443,
     443,   444,   444,   444,   445,   445,   445,   445,   445,   446,
     446,   446,   446,   447,   447,   447,   447,   447,   447,   448,
     448,   448,   449,   449,   449,   449,   449,   450,   450,   450,
     450,   451,   451,   451,   451,   451,   451,   452,   452,   453,
     453,   453,   453,   454,   454,   454,   454,   455,   455,   455,
     455,   455,   455,   455,   456,   456,   456,   456,   457,   457,
     457,   458,   458,   458,   458,   458,   459,   459,   459,   459,
     460,   460,   460,   460,   460,   460,   461,   461,   461,   461,
     461,   462,   462,   462,   463,   463,   463,   463,   464,   464,
     464,   465,   465,   465,   465,   465,   466,   466,   467,   467,
     467,   468,   468,   469,   469,   470,   470,   470,   471,   471,
     471,   471,   471,   472,   472,   472,   472,   473,   473,   473,
     474,   474,   474,   474,   474,   475,   475,   475,   475,   475,
     475,   476,   476,   477,   477,   477,   477,   478,   478,   479,
     479,   479,   479,   480,   480,   480,   480,   480,   481,   481,
     481,   481,   482,   482,   482,   483,   483,   483,   484,   484,
     484,   484,   484,   484,   485,   485,   485,   486,   486,   486,
     486,   486,   487,   487,   487,   487,   488,   488,   489,   489,
     489,   490,   490,   491,   491,   491,   491,   491,   491,   492,
     492,   492,   492,   492,   492,   492,   492,   492,   492,   493,
     493,   493,   493,   494,   494,   494,   495,   495,   496,   496,
     496,   496,   496,   496,   497,   497,   497,   497,   497,   497,
     498,   498,   498,   499,   499,   499,   500,   500,   501,   501
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
       3,     2,     2,     3,     3,     2,     2,     0,     1,     4,
       1,     2,     2,     2,     0,     1,     4,     1,     2,     3,
       1,     2,     0,     1,     2,     7,     8,     0,     9,     8,
       0,    11,    10,     1,     2,     3,     0,     1,     3,     3,
       0,     3,     2,     5,     4,     1,     1,     0,     2,     5,
       0,     1,     1,     3,     1,     1,     3,     3,     0,     1,
       1,     1,     3,     3,     3,     1,     3,     3,     5,     1,
       3,     3,     3,     2,     3,     1,     3,     3,     4,     1,
       1,     1,     1,     2,     1,     1,     3,     1,     1,     2,
       1,     1,     0,     2,     2,     4,     1,     4,     0,     1,
       2,     3,     4,     2,     2,     1,     2,     2,     5,     5,
       7,     6,     1,     3,     0,     2,     0,     5,     0,     5,
       3,     1,     8,     0,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     5,     4,     1,     1,     3,
       3,     2,     3,     3,     2,     4,     1,     4,     7,     5,
       8,     6,     1,     2,     2,     2,     1,     1,     3,     2,
       3,     1,     0,     1,     3,     4,     0,     1,     0,     0,
       1,     1,     2,     2,     2,     2,     2,     2,     1,     2,
       5,     0,     6,     0,     8,     0,     7,     0,     7,     0,
       8,     1,     1,     2,     3,     0,     5,     3,     4,     4,
       4,     4,     5,     5,     5,     5,     6,     1,     1,     1,
       1,     3,     0,     5,     0,     1,     1,     2,     6,     4,
       4,     1,     3,     0,     1,     4,     1,     1,     1,     1,
       2,     3,     2,     1,     2,     2,     2,     3,     4,     5,
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
#line 645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 9117 "Parser/parser.cc"
    break;

  case 3:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 9123 "Parser/parser.cc"
    break;

  case 4:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 9129 "Parser/parser.cc"
    break;

  case 5:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9135 "Parser/parser.cc"
    break;

  case 6:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9141 "Parser/parser.cc"
    break;

  case 7:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9147 "Parser/parser.cc"
    break;

  case 8:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 9153 "Parser/parser.cc"
    break;

  case 20:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 9159 "Parser/parser.cc"
    break;

  case 24:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 9165 "Parser/parser.cc"
    break;

  case 25:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 9171 "Parser/parser.cc"
    break;

  case 26:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 9181 "Parser/parser.cc"
    break;

  case 27:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 9187 "Parser/parser.cc"
    break;

  case 28:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 9193 "Parser/parser.cc"
    break;

  case 29:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 9199 "Parser/parser.cc"
    break;

  case 31:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9205 "Parser/parser.cc"
    break;

  case 32:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 9211 "Parser/parser.cc"
    break;

  case 33:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9217 "Parser/parser.cc"
    break;

  case 34:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9223 "Parser/parser.cc"
    break;

  case 35:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 9233 "Parser/parser.cc"
    break;

  case 36:
#line 734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 9239 "Parser/parser.cc"
    break;

  case 37:
#line 736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 9245 "Parser/parser.cc"
    break;

  case 38:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 9251 "Parser/parser.cc"
    break;

  case 39:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9257 "Parser/parser.cc"
    break;

  case 40:
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9263 "Parser/parser.cc"
    break;

  case 41:
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9269 "Parser/parser.cc"
    break;

  case 43:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 9281 "Parser/parser.cc"
    break;

  case 44:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 9290 "Parser/parser.cc"
    break;

  case 45:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 9296 "Parser/parser.cc"
    break;

  case 47:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 9302 "Parser/parser.cc"
    break;

  case 48:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9308 "Parser/parser.cc"
    break;

  case 49:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9314 "Parser/parser.cc"
    break;

  case 50:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9320 "Parser/parser.cc"
    break;

  case 51:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 9330 "Parser/parser.cc"
    break;

  case 52:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9336 "Parser/parser.cc"
    break;

  case 53:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_va_arg( yylloc, (yyvsp[-4].expr), ( (yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl) ) ) ); }
#line 9342 "Parser/parser.cc"
    break;

  case 54:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9348 "Parser/parser.cc"
    break;

  case 55:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9354 "Parser/parser.cc"
    break;

  case 56:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9360 "Parser/parser.cc"
    break;

  case 57:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9366 "Parser/parser.cc"
    break;

  case 58:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9372 "Parser/parser.cc"
    break;

  case 59:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9378 "Parser/parser.cc"
    break;

  case 60:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9384 "Parser/parser.cc"
    break;

  case 61:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 9390 "Parser/parser.cc"
    break;

  case 62:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9396 "Parser/parser.cc"
    break;

  case 63:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9402 "Parser/parser.cc"
    break;

  case 64:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9408 "Parser/parser.cc"
    break;

  case 65:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 9414 "Parser/parser.cc"
    break;

  case 66:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 9420 "Parser/parser.cc"
    break;

  case 67:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 9426 "Parser/parser.cc"
    break;

  case 68:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 9432 "Parser/parser.cc"
    break;

  case 69:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 9442 "Parser/parser.cc"
    break;

  case 71:
#line 855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9448 "Parser/parser.cc"
    break;

  case 73:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9454 "Parser/parser.cc"
    break;

  case 74:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9460 "Parser/parser.cc"
    break;

  case 75:
#line 865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9466 "Parser/parser.cc"
    break;

  case 76:
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9472 "Parser/parser.cc"
    break;

  case 77:
#line 869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9478 "Parser/parser.cc"
    break;

  case 78:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9484 "Parser/parser.cc"
    break;

  case 79:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 9490 "Parser/parser.cc"
    break;

  case 80:
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 9496 "Parser/parser.cc"
    break;

  case 81:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 9504 "Parser/parser.cc"
    break;

  case 82:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9510 "Parser/parser.cc"
    break;

  case 83:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 9519 "Parser/parser.cc"
    break;

  case 86:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 9525 "Parser/parser.cc"
    break;

  case 87:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 9531 "Parser/parser.cc"
    break;

  case 88:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9551 "Parser/parser.cc"
    break;

  case 89:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 9557 "Parser/parser.cc"
    break;

  case 90:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 9563 "Parser/parser.cc"
    break;

  case 91:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 9569 "Parser/parser.cc"
    break;

  case 92:
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 9575 "Parser/parser.cc"
    break;

  case 93:
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9581 "Parser/parser.cc"
    break;

  case 94:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 9587 "Parser/parser.cc"
    break;

  case 95:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9593 "Parser/parser.cc"
    break;

  case 96:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9599 "Parser/parser.cc"
    break;

  case 97:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9605 "Parser/parser.cc"
    break;

  case 98:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 9611 "Parser/parser.cc"
    break;

  case 99:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9620 "Parser/parser.cc"
    break;

  case 100:
#line 953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9626 "Parser/parser.cc"
    break;

  case 101:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9632 "Parser/parser.cc"
    break;

  case 102:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9638 "Parser/parser.cc"
    break;

  case 103:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9644 "Parser/parser.cc"
    break;

  case 104:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9650 "Parser/parser.cc"
    break;

  case 105:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9656 "Parser/parser.cc"
    break;

  case 106:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9662 "Parser/parser.cc"
    break;

  case 107:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9668 "Parser/parser.cc"
    break;

  case 108:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9674 "Parser/parser.cc"
    break;

  case 110:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9680 "Parser/parser.cc"
    break;

  case 111:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9686 "Parser/parser.cc"
    break;

  case 112:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9692 "Parser/parser.cc"
    break;

  case 113:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9698 "Parser/parser.cc"
    break;

  case 114:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9704 "Parser/parser.cc"
    break;

  case 115:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9710 "Parser/parser.cc"
    break;

  case 116:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9716 "Parser/parser.cc"
    break;

  case 117:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9722 "Parser/parser.cc"
    break;

  case 125:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9728 "Parser/parser.cc"
    break;

  case 127:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9734 "Parser/parser.cc"
    break;

  case 128:
#line 1017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9740 "Parser/parser.cc"
    break;

  case 129:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9746 "Parser/parser.cc"
    break;

  case 131:
#line 1025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9752 "Parser/parser.cc"
    break;

  case 132:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9758 "Parser/parser.cc"
    break;

  case 134:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9764 "Parser/parser.cc"
    break;

  case 135:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9770 "Parser/parser.cc"
    break;

  case 137:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9776 "Parser/parser.cc"
    break;

  case 138:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9782 "Parser/parser.cc"
    break;

  case 139:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9788 "Parser/parser.cc"
    break;

  case 140:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9794 "Parser/parser.cc"
    break;

  case 142:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9800 "Parser/parser.cc"
    break;

  case 143:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9806 "Parser/parser.cc"
    break;

  case 145:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9812 "Parser/parser.cc"
    break;

  case 147:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9818 "Parser/parser.cc"
    break;

  case 149:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9824 "Parser/parser.cc"
    break;

  case 151:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9830 "Parser/parser.cc"
    break;

  case 153:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9836 "Parser/parser.cc"
    break;

  case 155:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9842 "Parser/parser.cc"
    break;

  case 156:
#line 1093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9848 "Parser/parser.cc"
    break;

  case 158:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9854 "Parser/parser.cc"
    break;

  case 161:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9860 "Parser/parser.cc"
    break;

  case 162:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 9866 "Parser/parser.cc"
    break;

  case 163:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 9872 "Parser/parser.cc"
    break;

  case 166:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 9884 "Parser/parser.cc"
    break;

  case 167:
#line 1135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9890 "Parser/parser.cc"
    break;

  case 168:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9896 "Parser/parser.cc"
    break;

  case 172:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9902 "Parser/parser.cc"
    break;

  case 173:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9908 "Parser/parser.cc"
    break;

  case 174:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9914 "Parser/parser.cc"
    break;

  case 175:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9920 "Parser/parser.cc"
    break;

  case 176:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9926 "Parser/parser.cc"
    break;

  case 177:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9932 "Parser/parser.cc"
    break;

  case 178:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9938 "Parser/parser.cc"
    break;

  case 179:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9944 "Parser/parser.cc"
    break;

  case 180:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9950 "Parser/parser.cc"
    break;

  case 181:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9956 "Parser/parser.cc"
    break;

  case 182:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9962 "Parser/parser.cc"
    break;

  case 183:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9968 "Parser/parser.cc"
    break;

  case 184:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9974 "Parser/parser.cc"
    break;

  case 185:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9980 "Parser/parser.cc"
    break;

  case 186:
#line 1178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9986 "Parser/parser.cc"
    break;

  case 188:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9992 "Parser/parser.cc"
    break;

  case 189:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9998 "Parser/parser.cc"
    break;

  case 190:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10004 "Parser/parser.cc"
    break;

  case 192:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10010 "Parser/parser.cc"
    break;

  case 193:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10016 "Parser/parser.cc"
    break;

  case 208:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10022 "Parser/parser.cc"
    break;

  case 210:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 10028 "Parser/parser.cc"
    break;

  case 211:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 10034 "Parser/parser.cc"
    break;

  case 212:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 10045 "Parser/parser.cc"
    break;

  case 213:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 10051 "Parser/parser.cc"
    break;

  case 214:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 10057 "Parser/parser.cc"
    break;

  case 216:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10063 "Parser/parser.cc"
    break;

  case 217:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10069 "Parser/parser.cc"
    break;

  case 218:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10075 "Parser/parser.cc"
    break;

  case 219:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10081 "Parser/parser.cc"
    break;

  case 220:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10087 "Parser/parser.cc"
    break;

  case 223:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10093 "Parser/parser.cc"
    break;

  case 224:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 10100 "Parser/parser.cc"
    break;

  case 225:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 10106 "Parser/parser.cc"
    break;

  case 226:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 10112 "Parser/parser.cc"
    break;

  case 227:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10118 "Parser/parser.cc"
    break;

  case 228:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10124 "Parser/parser.cc"
    break;

  case 229:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 10138 "Parser/parser.cc"
    break;

  case 230:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10144 "Parser/parser.cc"
    break;

  case 231:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10150 "Parser/parser.cc"
    break;

  case 232:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 10159 "Parser/parser.cc"
    break;

  case 233:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10165 "Parser/parser.cc"
    break;

  case 234:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( nullptr, (yyvsp[0].expr) ); }
#line 10171 "Parser/parser.cc"
    break;

  case 235:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 10177 "Parser/parser.cc"
    break;

  case 236:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 10183 "Parser/parser.cc"
    break;

  case 237:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 10189 "Parser/parser.cc"
    break;

  case 238:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10195 "Parser/parser.cc"
    break;

  case 239:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10201 "Parser/parser.cc"
    break;

  case 241:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 10207 "Parser/parser.cc"
    break;

  case 242:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 10213 "Parser/parser.cc"
    break;

  case 243:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 10219 "Parser/parser.cc"
    break;

  case 244:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 10225 "Parser/parser.cc"
    break;

  case 245:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 10231 "Parser/parser.cc"
    break;

  case 246:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 10237 "Parser/parser.cc"
    break;

  case 247:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 10243 "Parser/parser.cc"
    break;

  case 249:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 10249 "Parser/parser.cc"
    break;

  case 250:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10255 "Parser/parser.cc"
    break;

  case 251:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 10261 "Parser/parser.cc"
    break;

  case 253:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10267 "Parser/parser.cc"
    break;

  case 254:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 10273 "Parser/parser.cc"
    break;

  case 255:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10279 "Parser/parser.cc"
    break;

  case 256:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10288 "Parser/parser.cc"
    break;

  case 257:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10294 "Parser/parser.cc"
    break;

  case 258:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10300 "Parser/parser.cc"
    break;

  case 259:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 10306 "Parser/parser.cc"
    break;

  case 260:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10315 "Parser/parser.cc"
    break;

  case 261:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 10321 "Parser/parser.cc"
    break;

  case 262:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10327 "Parser/parser.cc"
    break;

  case 263:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10333 "Parser/parser.cc"
    break;

  case 264:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10342 "Parser/parser.cc"
    break;

  case 265:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10348 "Parser/parser.cc"
    break;

  case 266:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10354 "Parser/parser.cc"
    break;

  case 268:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyvsp[-2].forctrl)->init->set_last( (yyvsp[0].forctrl)->init );
			if ( (yyvsp[-2].forctrl)->condition ) {
				if ( (yyvsp[0].forctrl)->condition ) {
					(yyvsp[-2].forctrl)->condition->expr.reset( new ast::LogicalExpr( yylloc, (yyvsp[-2].forctrl)->condition->expr.release(), (yyvsp[0].forctrl)->condition->expr.release(), ast::AndExpr ) );
				} // if
			} else (yyvsp[-2].forctrl)->condition = (yyvsp[0].forctrl)->condition;
			if ( (yyvsp[-2].forctrl)->change ) {
				if ( (yyvsp[0].forctrl)->change ) {
					(yyvsp[-2].forctrl)->change->expr.reset( new ast::CommaExpr( yylloc, (yyvsp[-2].forctrl)->change->expr.release(), (yyvsp[0].forctrl)->change->expr.release() ) );
				} // if
			} else (yyvsp[-2].forctrl)->change = (yyvsp[0].forctrl)->change;
			(yyval.forctrl) = (yyvsp[-2].forctrl);
		}
#line 10373 "Parser/parser.cc"
    break;

  case 269:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10379 "Parser/parser.cc"
    break;

  case 270:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 10387 "Parser/parser.cc"
    break;

  case 271:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10393 "Parser/parser.cc"
    break;

  case 272:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 10399 "Parser/parser.cc"
    break;

  case 273:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10405 "Parser/parser.cc"
    break;

  case 274:
#line 1472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 10411 "Parser/parser.cc"
    break;

  case 275:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10417 "Parser/parser.cc"
    break;

  case 276:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10423 "Parser/parser.cc"
    break;

  case 277:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10432 "Parser/parser.cc"
    break;

  case 278:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 10441 "Parser/parser.cc"
    break;

  case 279:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10447 "Parser/parser.cc"
    break;

  case 280:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10456 "Parser/parser.cc"
    break;

  case 281:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 10465 "Parser/parser.cc"
    break;

  case 282:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 10471 "Parser/parser.cc"
    break;

  case 283:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 10477 "Parser/parser.cc"
    break;

  case 284:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 10483 "Parser/parser.cc"
    break;

  case 285:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 10489 "Parser/parser.cc"
    break;

  case 286:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 10495 "Parser/parser.cc"
    break;

  case 287:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 10501 "Parser/parser.cc"
    break;

  case 288:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10507 "Parser/parser.cc"
    break;

  case 289:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10513 "Parser/parser.cc"
    break;

  case 290:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10522 "Parser/parser.cc"
    break;

  case 291:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10532 "Parser/parser.cc"
    break;

  case 292:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 10538 "Parser/parser.cc"
    break;

  case 293:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10544 "Parser/parser.cc"
    break;

  case 294:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10553 "Parser/parser.cc"
    break;

  case 295:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10563 "Parser/parser.cc"
    break;

  case 296:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10569 "Parser/parser.cc"
    break;

  case 297:
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10578 "Parser/parser.cc"
    break;

  case 298:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10588 "Parser/parser.cc"
    break;

  case 299:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 10594 "Parser/parser.cc"
    break;

  case 300:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 10600 "Parser/parser.cc"
    break;

  case 301:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10606 "Parser/parser.cc"
    break;

  case 302:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10612 "Parser/parser.cc"
    break;

  case 303:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10621 "Parser/parser.cc"
    break;

  case 304:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10631 "Parser/parser.cc"
    break;

  case 305:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10637 "Parser/parser.cc"
    break;

  case 306:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10646 "Parser/parser.cc"
    break;

  case 307:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10656 "Parser/parser.cc"
    break;

  case 308:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10662 "Parser/parser.cc"
    break;

  case 309:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10671 "Parser/parser.cc"
    break;

  case 310:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10681 "Parser/parser.cc"
    break;

  case 311:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 10687 "Parser/parser.cc"
    break;

  case 312:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 10695 "Parser/parser.cc"
    break;

  case 313:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); (yyval.forctrl) = nullptr;
				(yyvsp[-1].oper) = OperKinds::GEThan;
			} // if
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-3].expr), (yyvsp[-1].oper), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 10707 "Parser/parser.cc"
    break;

  case 314:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 10716 "Parser/parser.cc"
    break;

  case 315:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false );
		}
#line 10725 "Parser/parser.cc"
    break;

  case 316:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 10734 "Parser/parser.cc"
    break;

  case 317:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10740 "Parser/parser.cc"
    break;

  case 318:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10746 "Parser/parser.cc"
    break;

  case 319:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10752 "Parser/parser.cc"
    break;

  case 320:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10758 "Parser/parser.cc"
    break;

  case 321:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10764 "Parser/parser.cc"
    break;

  case 322:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10770 "Parser/parser.cc"
    break;

  case 323:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10776 "Parser/parser.cc"
    break;

  case 325:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10782 "Parser/parser.cc"
    break;

  case 326:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10788 "Parser/parser.cc"
    break;

  case 327:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10794 "Parser/parser.cc"
    break;

  case 328:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10800 "Parser/parser.cc"
    break;

  case 329:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10806 "Parser/parser.cc"
    break;

  case 330:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10812 "Parser/parser.cc"
    break;

  case 331:
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 332:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10824 "Parser/parser.cc"
    break;

  case 333:
#line 1695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10830 "Parser/parser.cc"
    break;

  case 334:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10836 "Parser/parser.cc"
    break;

  case 335:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10842 "Parser/parser.cc"
    break;

  case 336:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10848 "Parser/parser.cc"
    break;

  case 337:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10854 "Parser/parser.cc"
    break;

  case 338:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10860 "Parser/parser.cc"
    break;

  case 339:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10866 "Parser/parser.cc"
    break;

  case 340:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10872 "Parser/parser.cc"
    break;

  case 341:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10878 "Parser/parser.cc"
    break;

  case 342:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10884 "Parser/parser.cc"
    break;

  case 343:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 344:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 345:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10902 "Parser/parser.cc"
    break;

  case 346:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10908 "Parser/parser.cc"
    break;

  case 349:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10914 "Parser/parser.cc"
    break;

  case 350:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10923 "Parser/parser.cc"
    break;

  case 351:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10929 "Parser/parser.cc"
    break;

  case 352:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10935 "Parser/parser.cc"
    break;

  case 355:
#line 1759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10941 "Parser/parser.cc"
    break;

  case 356:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10947 "Parser/parser.cc"
    break;

  case 359:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10953 "Parser/parser.cc"
    break;

  case 360:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10959 "Parser/parser.cc"
    break;

  case 361:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10965 "Parser/parser.cc"
    break;

  case 362:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10971 "Parser/parser.cc"
    break;

  case 363:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10977 "Parser/parser.cc"
    break;

  case 364:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10983 "Parser/parser.cc"
    break;

  case 365:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10989 "Parser/parser.cc"
    break;

  case 366:
#line 1791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10995 "Parser/parser.cc"
    break;

  case 367:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 11001 "Parser/parser.cc"
    break;

  case 370:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11007 "Parser/parser.cc"
    break;

  case 371:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11013 "Parser/parser.cc"
    break;

  case 372:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 11019 "Parser/parser.cc"
    break;

  case 373:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11025 "Parser/parser.cc"
    break;

  case 374:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11031 "Parser/parser.cc"
    break;

  case 375:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11037 "Parser/parser.cc"
    break;

  case 376:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11043 "Parser/parser.cc"
    break;

  case 377:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11049 "Parser/parser.cc"
    break;

  case 378:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 11055 "Parser/parser.cc"
    break;

  case 379:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 11061 "Parser/parser.cc"
    break;

  case 380:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11067 "Parser/parser.cc"
    break;

  case 381:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 11073 "Parser/parser.cc"
    break;

  case 382:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 11079 "Parser/parser.cc"
    break;

  case 383:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 11085 "Parser/parser.cc"
    break;

  case 384:
#line 1858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11091 "Parser/parser.cc"
    break;

  case 385:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-6].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 11097 "Parser/parser.cc"
    break;

  case 386:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11103 "Parser/parser.cc"
    break;

  case 387:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 11109 "Parser/parser.cc"
    break;

  case 388:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11115 "Parser/parser.cc"
    break;

  case 389:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11121 "Parser/parser.cc"
    break;

  case 390:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 11127 "Parser/parser.cc"
    break;

  case 391:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 11133 "Parser/parser.cc"
    break;

  case 392:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 11139 "Parser/parser.cc"
    break;

  case 394:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11145 "Parser/parser.cc"
    break;

  case 395:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11151 "Parser/parser.cc"
    break;

  case 396:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11157 "Parser/parser.cc"
    break;

  case 401:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 11163 "Parser/parser.cc"
    break;

  case 402:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11169 "Parser/parser.cc"
    break;

  case 403:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11175 "Parser/parser.cc"
    break;

  case 404:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11181 "Parser/parser.cc"
    break;

  case 405:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 11187 "Parser/parser.cc"
    break;

  case 406:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 11193 "Parser/parser.cc"
    break;

  case 407:
#line 1918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 11199 "Parser/parser.cc"
    break;

  case 408:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11205 "Parser/parser.cc"
    break;

  case 411:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11211 "Parser/parser.cc"
    break;

  case 412:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 11217 "Parser/parser.cc"
    break;

  case 413:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 11226 "Parser/parser.cc"
    break;

  case 414:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11232 "Parser/parser.cc"
    break;

  case 415:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11238 "Parser/parser.cc"
    break;

  case 416:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11244 "Parser/parser.cc"
    break;

  case 417:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 11253 "Parser/parser.cc"
    break;

  case 418:
#line 1959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 11262 "Parser/parser.cc"
    break;

  case 419:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11268 "Parser/parser.cc"
    break;

  case 422:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 11274 "Parser/parser.cc"
    break;

  case 423:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11280 "Parser/parser.cc"
    break;

  case 425:
#line 1987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11286 "Parser/parser.cc"
    break;

  case 426:
#line 1989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 11292 "Parser/parser.cc"
    break;

  case 436:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 11298 "Parser/parser.cc"
    break;

  case 437:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 11304 "Parser/parser.cc"
    break;

  case 441:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11310 "Parser/parser.cc"
    break;

  case 443:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 11316 "Parser/parser.cc"
    break;

  case 444:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11322 "Parser/parser.cc"
    break;

  case 445:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 11328 "Parser/parser.cc"
    break;

  case 446:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11334 "Parser/parser.cc"
    break;

  case 447:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11340 "Parser/parser.cc"
    break;

  case 448:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11346 "Parser/parser.cc"
    break;

  case 449:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11352 "Parser/parser.cc"
    break;

  case 450:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11358 "Parser/parser.cc"
    break;

  case 452:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11364 "Parser/parser.cc"
    break;

  case 453:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11370 "Parser/parser.cc"
    break;

  case 454:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11376 "Parser/parser.cc"
    break;

  case 455:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 11387 "Parser/parser.cc"
    break;

  case 456:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11393 "Parser/parser.cc"
    break;

  case 457:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11399 "Parser/parser.cc"
    break;

  case 458:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11405 "Parser/parser.cc"
    break;

  case 459:
#line 2107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11411 "Parser/parser.cc"
    break;

  case 460:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11417 "Parser/parser.cc"
    break;

  case 461:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 11423 "Parser/parser.cc"
    break;

  case 462:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 11432 "Parser/parser.cc"
    break;

  case 463:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 11441 "Parser/parser.cc"
    break;

  case 464:
#line 2130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 11450 "Parser/parser.cc"
    break;

  case 465:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 11461 "Parser/parser.cc"
    break;

  case 466:
#line 2148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 11470 "Parser/parser.cc"
    break;

  case 467:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11476 "Parser/parser.cc"
    break;

  case 468:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11482 "Parser/parser.cc"
    break;

  case 469:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11488 "Parser/parser.cc"
    break;

  case 470:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 11496 "Parser/parser.cc"
    break;

  case 471:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 11504 "Parser/parser.cc"
    break;

  case 472:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 11510 "Parser/parser.cc"
    break;

  case 475:
#line 2178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11525 "Parser/parser.cc"
    break;

  case 476:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11531 "Parser/parser.cc"
    break;

  case 477:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11537 "Parser/parser.cc"
    break;

  case 478:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 11543 "Parser/parser.cc"
    break;

  case 479:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 11549 "Parser/parser.cc"
    break;

  case 480:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 11555 "Parser/parser.cc"
    break;

  case 486:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 11565 "Parser/parser.cc"
    break;

  case 499:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11571 "Parser/parser.cc"
    break;

  case 502:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11577 "Parser/parser.cc"
    break;

  case 503:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11583 "Parser/parser.cc"
    break;

  case 505:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 11589 "Parser/parser.cc"
    break;

  case 506:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 11595 "Parser/parser.cc"
    break;

  case 507:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 11601 "Parser/parser.cc"
    break;

  case 508:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 11607 "Parser/parser.cc"
    break;

  case 509:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 11613 "Parser/parser.cc"
    break;

  case 510:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11619 "Parser/parser.cc"
    break;

  case 512:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11625 "Parser/parser.cc"
    break;

  case 513:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11631 "Parser/parser.cc"
    break;

  case 515:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11637 "Parser/parser.cc"
    break;

  case 516:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 11643 "Parser/parser.cc"
    break;

  case 517:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 11649 "Parser/parser.cc"
    break;

  case 518:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 11655 "Parser/parser.cc"
    break;

  case 519:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 11661 "Parser/parser.cc"
    break;

  case 520:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 11667 "Parser/parser.cc"
    break;

  case 521:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 11673 "Parser/parser.cc"
    break;

  case 522:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 11679 "Parser/parser.cc"
    break;

  case 523:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 11685 "Parser/parser.cc"
    break;

  case 524:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11691 "Parser/parser.cc"
    break;

  case 525:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11697 "Parser/parser.cc"
    break;

  case 526:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 11703 "Parser/parser.cc"
    break;

  case 527:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11709 "Parser/parser.cc"
    break;

  case 528:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11715 "Parser/parser.cc"
    break;

  case 529:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11721 "Parser/parser.cc"
    break;

  case 530:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11727 "Parser/parser.cc"
    break;

  case 531:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11733 "Parser/parser.cc"
    break;

  case 532:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11739 "Parser/parser.cc"
    break;

  case 533:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11745 "Parser/parser.cc"
    break;

  case 534:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float80 ); }
#line 11751 "Parser/parser.cc"
    break;

  case 535:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11757 "Parser/parser.cc"
    break;

  case 536:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float16 ); }
#line 11763 "Parser/parser.cc"
    break;

  case 537:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32 ); }
#line 11769 "Parser/parser.cc"
    break;

  case 538:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x ); }
#line 11775 "Parser/parser.cc"
    break;

  case 539:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64 ); }
#line 11781 "Parser/parser.cc"
    break;

  case 540:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x ); }
#line 11787 "Parser/parser.cc"
    break;

  case 541:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128 ); }
#line 11793 "Parser/parser.cc"
    break;

  case 542:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128x ); }
#line 11799 "Parser/parser.cc"
    break;

  case 543:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x4 ); }
#line 11805 "Parser/parser.cc"
    break;

  case 544:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x2 ); }
#line 11811 "Parser/parser.cc"
    break;

  case 545:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat32 ); }
#line 11817 "Parser/parser.cc"
    break;

  case 546:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat64 ); }
#line 11823 "Parser/parser.cc"
    break;

  case 547:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svbool ); }
#line 11829 "Parser/parser.cc"
    break;

  case 548:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11835 "Parser/parser.cc"
    break;

  case 549:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11841 "Parser/parser.cc"
    break;

  case 550:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11847 "Parser/parser.cc"
    break;

  case 551:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11853 "Parser/parser.cc"
    break;

  case 552:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11859 "Parser/parser.cc"
    break;

  case 553:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11865 "Parser/parser.cc"
    break;

  case 554:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11871 "Parser/parser.cc"
    break;

  case 555:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11877 "Parser/parser.cc"
    break;

  case 556:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11883 "Parser/parser.cc"
    break;

  case 557:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11889 "Parser/parser.cc"
    break;

  case 558:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11895 "Parser/parser.cc"
    break;

  case 560:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11901 "Parser/parser.cc"
    break;

  case 562:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11907 "Parser/parser.cc"
    break;

  case 563:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11913 "Parser/parser.cc"
    break;

  case 564:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11919 "Parser/parser.cc"
    break;

  case 566:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11925 "Parser/parser.cc"
    break;

  case 567:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11931 "Parser/parser.cc"
    break;

  case 568:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11937 "Parser/parser.cc"
    break;

  case 569:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11943 "Parser/parser.cc"
    break;

  case 571:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11949 "Parser/parser.cc"
    break;

  case 573:
#line 2463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11955 "Parser/parser.cc"
    break;

  case 574:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11961 "Parser/parser.cc"
    break;

  case 575:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11967 "Parser/parser.cc"
    break;

  case 576:
#line 2472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11973 "Parser/parser.cc"
    break;

  case 577:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11979 "Parser/parser.cc"
    break;

  case 578:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11985 "Parser/parser.cc"
    break;

  case 579:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11991 "Parser/parser.cc"
    break;

  case 580:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11997 "Parser/parser.cc"
    break;

  case 581:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 12003 "Parser/parser.cc"
    break;

  case 583:
#line 2488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12009 "Parser/parser.cc"
    break;

  case 584:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12015 "Parser/parser.cc"
    break;

  case 585:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12021 "Parser/parser.cc"
    break;

  case 587:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 12027 "Parser/parser.cc"
    break;

  case 588:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12033 "Parser/parser.cc"
    break;

  case 589:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 12042 "Parser/parser.cc"
    break;

  case 591:
#line 2511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12048 "Parser/parser.cc"
    break;

  case 592:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12054 "Parser/parser.cc"
    break;

  case 593:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12060 "Parser/parser.cc"
    break;

  case 595:
#line 2521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12066 "Parser/parser.cc"
    break;

  case 596:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12072 "Parser/parser.cc"
    break;

  case 598:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12078 "Parser/parser.cc"
    break;

  case 599:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12084 "Parser/parser.cc"
    break;

  case 600:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12090 "Parser/parser.cc"
    break;

  case 601:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12096 "Parser/parser.cc"
    break;

  case 602:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12102 "Parser/parser.cc"
    break;

  case 603:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12108 "Parser/parser.cc"
    break;

  case 604:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 12114 "Parser/parser.cc"
    break;

  case 605:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 12120 "Parser/parser.cc"
    break;

  case 606:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 12126 "Parser/parser.cc"
    break;

  case 608:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 12132 "Parser/parser.cc"
    break;

  case 609:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 12138 "Parser/parser.cc"
    break;

  case 610:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 12144 "Parser/parser.cc"
    break;

  case 611:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 12150 "Parser/parser.cc"
    break;

  case 612:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12156 "Parser/parser.cc"
    break;

  case 617:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 12162 "Parser/parser.cc"
    break;

  case 618:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12168 "Parser/parser.cc"
    break;

  case 619:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 12177 "Parser/parser.cc"
    break;

  case 620:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 12185 "Parser/parser.cc"
    break;

  case 621:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 12194 "Parser/parser.cc"
    break;

  case 622:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 12203 "Parser/parser.cc"
    break;

  case 623:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 12212 "Parser/parser.cc"
    break;

  case 624:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 12221 "Parser/parser.cc"
    break;

  case 626:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12227 "Parser/parser.cc"
    break;

  case 627:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12233 "Parser/parser.cc"
    break;

  case 628:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12243 "Parser/parser.cc"
    break;

  case 629:
#line 2632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12262 "Parser/parser.cc"
    break;

  case 632:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 12268 "Parser/parser.cc"
    break;

  case 633:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 12274 "Parser/parser.cc"
    break;

  case 634:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 12280 "Parser/parser.cc"
    break;

  case 635:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 12286 "Parser/parser.cc"
    break;

  case 636:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 12292 "Parser/parser.cc"
    break;

  case 637:
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 12298 "Parser/parser.cc"
    break;

  case 638:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12307 "Parser/parser.cc"
    break;

  case 639:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 12313 "Parser/parser.cc"
    break;

  case 640:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12322 "Parser/parser.cc"
    break;

  case 641:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 12328 "Parser/parser.cc"
    break;

  case 642:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12337 "Parser/parser.cc"
    break;

  case 643:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12343 "Parser/parser.cc"
    break;

  case 644:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 12349 "Parser/parser.cc"
    break;

  case 645:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 12362 "Parser/parser.cc"
    break;

  case 646:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 12371 "Parser/parser.cc"
    break;

  case 647:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 12377 "Parser/parser.cc"
    break;

  case 648:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12383 "Parser/parser.cc"
    break;

  case 649:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 12396 "Parser/parser.cc"
    break;

  case 650:
#line 2726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12402 "Parser/parser.cc"
    break;

  case 653:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 12408 "Parser/parser.cc"
    break;

  case 654:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12414 "Parser/parser.cc"
    break;

  case 657:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12420 "Parser/parser.cc"
    break;

  case 659:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 12426 "Parser/parser.cc"
    break;

  case 660:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 12432 "Parser/parser.cc"
    break;

  case 661:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12438 "Parser/parser.cc"
    break;

  case 662:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12444 "Parser/parser.cc"
    break;

  case 663:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12450 "Parser/parser.cc"
    break;

  case 664:
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12456 "Parser/parser.cc"
    break;

  case 666:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 12462 "Parser/parser.cc"
    break;

  case 668:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 12468 "Parser/parser.cc"
    break;

  case 669:
#line 2777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12474 "Parser/parser.cc"
    break;

  case 671:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 12480 "Parser/parser.cc"
    break;

  case 672:
#line 2789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12486 "Parser/parser.cc"
    break;

  case 674:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 12492 "Parser/parser.cc"
    break;

  case 675:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 12503 "Parser/parser.cc"
    break;

  case 676:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 12517 "Parser/parser.cc"
    break;

  case 677:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 12523 "Parser/parser.cc"
    break;

  case 678:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 12529 "Parser/parser.cc"
    break;

  case 679:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 12535 "Parser/parser.cc"
    break;

  case 680:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 12546 "Parser/parser.cc"
    break;

  case 681:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 12552 "Parser/parser.cc"
    break;

  case 682:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12558 "Parser/parser.cc"
    break;

  case 684:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12564 "Parser/parser.cc"
    break;

  case 685:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12570 "Parser/parser.cc"
    break;

  case 686:
#line 2852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 12576 "Parser/parser.cc"
    break;

  case 687:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 12582 "Parser/parser.cc"
    break;

  case 688:
#line 2859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12591 "Parser/parser.cc"
    break;

  case 689:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12600 "Parser/parser.cc"
    break;

  case 690:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 12606 "Parser/parser.cc"
    break;

  case 691:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 12612 "Parser/parser.cc"
    break;

  case 692:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 12622 "Parser/parser.cc"
    break;

  case 693:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 12628 "Parser/parser.cc"
    break;

  case 694:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 12634 "Parser/parser.cc"
    break;

  case 696:
#line 2890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 12640 "Parser/parser.cc"
    break;

  case 697:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12646 "Parser/parser.cc"
    break;

  case 698:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12652 "Parser/parser.cc"
    break;

  case 699:
#line 2897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12658 "Parser/parser.cc"
    break;

  case 700:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 12664 "Parser/parser.cc"
    break;

  case 701:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12670 "Parser/parser.cc"
    break;

  case 703:
#line 2911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12676 "Parser/parser.cc"
    break;

  case 706:
#line 2918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12682 "Parser/parser.cc"
    break;

  case 707:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12688 "Parser/parser.cc"
    break;

  case 708:
#line 2925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 12694 "Parser/parser.cc"
    break;

  case 709:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12700 "Parser/parser.cc"
    break;

  case 712:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12706 "Parser/parser.cc"
    break;

  case 713:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12712 "Parser/parser.cc"
    break;

  case 714:
#line 2935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12718 "Parser/parser.cc"
    break;

  case 716:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12724 "Parser/parser.cc"
    break;

  case 717:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12730 "Parser/parser.cc"
    break;

  case 718:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12736 "Parser/parser.cc"
    break;

  case 720:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12742 "Parser/parser.cc"
    break;

  case 721:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12748 "Parser/parser.cc"
    break;

  case 722:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12754 "Parser/parser.cc"
    break;

  case 723:
#line 2969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12760 "Parser/parser.cc"
    break;

  case 724:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12766 "Parser/parser.cc"
    break;

  case 726:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12772 "Parser/parser.cc"
    break;

  case 727:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12778 "Parser/parser.cc"
    break;

  case 728:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12784 "Parser/parser.cc"
    break;

  case 733:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12790 "Parser/parser.cc"
    break;

  case 735:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12796 "Parser/parser.cc"
    break;

  case 736:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12802 "Parser/parser.cc"
    break;

  case 739:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12808 "Parser/parser.cc"
    break;

  case 742:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12814 "Parser/parser.cc"
    break;

  case 743:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12820 "Parser/parser.cc"
    break;

  case 744:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12826 "Parser/parser.cc"
    break;

  case 745:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12832 "Parser/parser.cc"
    break;

  case 746:
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12838 "Parser/parser.cc"
    break;

  case 747:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12844 "Parser/parser.cc"
    break;

  case 748:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12850 "Parser/parser.cc"
    break;

  case 750:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12856 "Parser/parser.cc"
    break;

  case 751:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12862 "Parser/parser.cc"
    break;

  case 752:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12868 "Parser/parser.cc"
    break;

  case 754:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12874 "Parser/parser.cc"
    break;

  case 756:
#line 3060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12880 "Parser/parser.cc"
    break;

  case 757:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12886 "Parser/parser.cc"
    break;

  case 758:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12892 "Parser/parser.cc"
    break;

  case 759:
#line 3071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12898 "Parser/parser.cc"
    break;

  case 760:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12904 "Parser/parser.cc"
    break;

  case 761:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12910 "Parser/parser.cc"
    break;

  case 763:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12916 "Parser/parser.cc"
    break;

  case 764:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12922 "Parser/parser.cc"
    break;

  case 765:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12928 "Parser/parser.cc"
    break;

  case 766:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12939 "Parser/parser.cc"
    break;

  case 767:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12945 "Parser/parser.cc"
    break;

  case 768:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12951 "Parser/parser.cc"
    break;

  case 769:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12957 "Parser/parser.cc"
    break;

  case 770:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12966 "Parser/parser.cc"
    break;

  case 771:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12972 "Parser/parser.cc"
    break;

  case 772:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12982 "Parser/parser.cc"
    break;

  case 773:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12988 "Parser/parser.cc"
    break;

  case 774:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12994 "Parser/parser.cc"
    break;

  case 775:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 13000 "Parser/parser.cc"
    break;

  case 776:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13006 "Parser/parser.cc"
    break;

  case 777:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13012 "Parser/parser.cc"
    break;

  case 778:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13018 "Parser/parser.cc"
    break;

  case 779:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 13024 "Parser/parser.cc"
    break;

  case 780:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13030 "Parser/parser.cc"
    break;

  case 781:
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13036 "Parser/parser.cc"
    break;

  case 784:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13042 "Parser/parser.cc"
    break;

  case 785:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13048 "Parser/parser.cc"
    break;

  case 786:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13054 "Parser/parser.cc"
    break;

  case 787:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13060 "Parser/parser.cc"
    break;

  case 789:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13066 "Parser/parser.cc"
    break;

  case 790:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 13072 "Parser/parser.cc"
    break;

  case 791:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13078 "Parser/parser.cc"
    break;

  case 792:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13084 "Parser/parser.cc"
    break;

  case 793:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 13090 "Parser/parser.cc"
    break;

  case 794:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 13096 "Parser/parser.cc"
    break;

  case 795:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 13102 "Parser/parser.cc"
    break;

  case 796:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 13111 "Parser/parser.cc"
    break;

  case 797:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 13120 "Parser/parser.cc"
    break;

  case 798:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 13129 "Parser/parser.cc"
    break;

  case 799:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 13135 "Parser/parser.cc"
    break;

  case 800:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-6].tok), (yyvsp[-4].decl), (yyvsp[-1].decl) );
		}
#line 13144 "Parser/parser.cc"
    break;

  case 801:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-3].tok), (yyvsp[-5].decl), (yyvsp[-1].decl) ); }
#line 13150 "Parser/parser.cc"
    break;

  case 803:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13156 "Parser/parser.cc"
    break;

  case 808:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13162 "Parser/parser.cc"
    break;

  case 809:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 13168 "Parser/parser.cc"
    break;

  case 810:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 13174 "Parser/parser.cc"
    break;

  case 811:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Possible cause is declaring an aggregate or enumeration type in a trait." ); (yyval.decl) = nullptr; }
#line 13180 "Parser/parser.cc"
    break;

  case 813:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 13186 "Parser/parser.cc"
    break;

  case 814:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13192 "Parser/parser.cc"
    break;

  case 815:
#line 3280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 13198 "Parser/parser.cc"
    break;

  case 816:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13204 "Parser/parser.cc"
    break;

  case 818:
#line 3290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 13210 "Parser/parser.cc"
    break;

  case 819:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 13216 "Parser/parser.cc"
    break;

  case 820:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 13222 "Parser/parser.cc"
    break;

  case 821:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 13238 "Parser/parser.cc"
    break;

  case 822:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 13244 "Parser/parser.cc"
    break;

  case 823:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 13250 "Parser/parser.cc"
    break;

  case 824:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 13256 "Parser/parser.cc"
    break;

  case 825:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13262 "Parser/parser.cc"
    break;

  case 826:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13268 "Parser/parser.cc"
    break;

  case 827:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13274 "Parser/parser.cc"
    break;

  case 829:
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 13283 "Parser/parser.cc"
    break;

  case 830:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 13289 "Parser/parser.cc"
    break;

  case 831:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 13298 "Parser/parser.cc"
    break;

  case 832:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 13308 "Parser/parser.cc"
    break;

  case 833:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 13317 "Parser/parser.cc"
    break;

  case 834:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13327 "Parser/parser.cc"
    break;

  case 835:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 13338 "Parser/parser.cc"
    break;

  case 836:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13348 "Parser/parser.cc"
    break;

  case 837:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 13359 "Parser/parser.cc"
    break;

  case 838:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13369 "Parser/parser.cc"
    break;

  case 839:
#line 3382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 13380 "Parser/parser.cc"
    break;

  case 840:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13390 "Parser/parser.cc"
    break;

  case 841:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13396 "Parser/parser.cc"
    break;

  case 843:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 13402 "Parser/parser.cc"
    break;

  case 844:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 13408 "Parser/parser.cc"
    break;

  case 845:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 13414 "Parser/parser.cc"
    break;

  case 846:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 13426 "Parser/parser.cc"
    break;

  case 847:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13437 "Parser/parser.cc"
    break;

  case 848:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 13446 "Parser/parser.cc"
    break;

  case 849:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 13455 "Parser/parser.cc"
    break;

  case 850:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13461 "Parser/parser.cc"
    break;

  case 851:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13467 "Parser/parser.cc"
    break;

  case 852:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13473 "Parser/parser.cc"
    break;

  case 853:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 13482 "Parser/parser.cc"
    break;

  case 854:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13488 "Parser/parser.cc"
    break;

  case 855:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13494 "Parser/parser.cc"
    break;

  case 856:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 13500 "Parser/parser.cc"
    break;

  case 861:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 13506 "Parser/parser.cc"
    break;

  case 862:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13512 "Parser/parser.cc"
    break;

  case 863:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 13522 "Parser/parser.cc"
    break;

  case 864:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13528 "Parser/parser.cc"
    break;

  case 867:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13534 "Parser/parser.cc"
    break;

  case 868:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 13540 "Parser/parser.cc"
    break;

  case 869:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13546 "Parser/parser.cc"
    break;

  case 870:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13552 "Parser/parser.cc"
    break;

  case 872:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13558 "Parser/parser.cc"
    break;

  case 873:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13564 "Parser/parser.cc"
    break;

  case 874:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 13570 "Parser/parser.cc"
    break;

  case 875:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13576 "Parser/parser.cc"
    break;

  case 877:
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 13582 "Parser/parser.cc"
    break;

  case 878:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 13588 "Parser/parser.cc"
    break;

  case 879:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13594 "Parser/parser.cc"
    break;

  case 880:
#line 3575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13600 "Parser/parser.cc"
    break;

  case 881:
#line 3577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13606 "Parser/parser.cc"
    break;

  case 882:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13612 "Parser/parser.cc"
    break;

  case 884:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13618 "Parser/parser.cc"
    break;

  case 885:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13624 "Parser/parser.cc"
    break;

  case 886:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13630 "Parser/parser.cc"
    break;

  case 887:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13636 "Parser/parser.cc"
    break;

  case 888:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13642 "Parser/parser.cc"
    break;

  case 889:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13648 "Parser/parser.cc"
    break;

  case 890:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13654 "Parser/parser.cc"
    break;

  case 891:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13660 "Parser/parser.cc"
    break;

  case 892:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13666 "Parser/parser.cc"
    break;

  case 893:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13672 "Parser/parser.cc"
    break;

  case 894:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13678 "Parser/parser.cc"
    break;

  case 895:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13684 "Parser/parser.cc"
    break;

  case 896:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13690 "Parser/parser.cc"
    break;

  case 897:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13696 "Parser/parser.cc"
    break;

  case 898:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13702 "Parser/parser.cc"
    break;

  case 899:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13708 "Parser/parser.cc"
    break;

  case 900:
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13714 "Parser/parser.cc"
    break;

  case 901:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13720 "Parser/parser.cc"
    break;

  case 903:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13726 "Parser/parser.cc"
    break;

  case 904:
#line 3643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13732 "Parser/parser.cc"
    break;

  case 905:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13738 "Parser/parser.cc"
    break;

  case 906:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13744 "Parser/parser.cc"
    break;

  case 907:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13750 "Parser/parser.cc"
    break;

  case 908:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13756 "Parser/parser.cc"
    break;

  case 909:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13762 "Parser/parser.cc"
    break;

  case 910:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13768 "Parser/parser.cc"
    break;

  case 911:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13774 "Parser/parser.cc"
    break;

  case 912:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13780 "Parser/parser.cc"
    break;

  case 913:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13786 "Parser/parser.cc"
    break;

  case 914:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13792 "Parser/parser.cc"
    break;

  case 915:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13798 "Parser/parser.cc"
    break;

  case 916:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13804 "Parser/parser.cc"
    break;

  case 917:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13810 "Parser/parser.cc"
    break;

  case 918:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13816 "Parser/parser.cc"
    break;

  case 922:
#line 3695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13822 "Parser/parser.cc"
    break;

  case 923:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13828 "Parser/parser.cc"
    break;

  case 924:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13834 "Parser/parser.cc"
    break;

  case 925:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13840 "Parser/parser.cc"
    break;

  case 926:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13846 "Parser/parser.cc"
    break;

  case 927:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13852 "Parser/parser.cc"
    break;

  case 928:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13858 "Parser/parser.cc"
    break;

  case 929:
#line 3712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13864 "Parser/parser.cc"
    break;

  case 930:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13870 "Parser/parser.cc"
    break;

  case 931:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13876 "Parser/parser.cc"
    break;

  case 932:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13882 "Parser/parser.cc"
    break;

  case 933:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13888 "Parser/parser.cc"
    break;

  case 934:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13894 "Parser/parser.cc"
    break;

  case 935:
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13900 "Parser/parser.cc"
    break;

  case 936:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13906 "Parser/parser.cc"
    break;

  case 937:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13915 "Parser/parser.cc"
    break;

  case 938:
#line 3746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13921 "Parser/parser.cc"
    break;

  case 939:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13927 "Parser/parser.cc"
    break;

  case 941:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13933 "Parser/parser.cc"
    break;

  case 942:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13939 "Parser/parser.cc"
    break;

  case 943:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13945 "Parser/parser.cc"
    break;

  case 944:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13951 "Parser/parser.cc"
    break;

  case 945:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13957 "Parser/parser.cc"
    break;

  case 946:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13963 "Parser/parser.cc"
    break;

  case 947:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13969 "Parser/parser.cc"
    break;

  case 948:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13975 "Parser/parser.cc"
    break;

  case 949:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13981 "Parser/parser.cc"
    break;

  case 950:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13987 "Parser/parser.cc"
    break;

  case 951:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13993 "Parser/parser.cc"
    break;

  case 952:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13999 "Parser/parser.cc"
    break;

  case 953:
#line 3784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14005 "Parser/parser.cc"
    break;

  case 954:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14011 "Parser/parser.cc"
    break;

  case 955:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14017 "Parser/parser.cc"
    break;

  case 956:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14023 "Parser/parser.cc"
    break;

  case 957:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14029 "Parser/parser.cc"
    break;

  case 958:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14035 "Parser/parser.cc"
    break;

  case 960:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14041 "Parser/parser.cc"
    break;

  case 961:
#line 3812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14047 "Parser/parser.cc"
    break;

  case 962:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14053 "Parser/parser.cc"
    break;

  case 963:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14059 "Parser/parser.cc"
    break;

  case 964:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14065 "Parser/parser.cc"
    break;

  case 965:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14071 "Parser/parser.cc"
    break;

  case 966:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14077 "Parser/parser.cc"
    break;

  case 967:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14083 "Parser/parser.cc"
    break;

  case 968:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14089 "Parser/parser.cc"
    break;

  case 969:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14095 "Parser/parser.cc"
    break;

  case 970:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14101 "Parser/parser.cc"
    break;

  case 971:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14107 "Parser/parser.cc"
    break;

  case 972:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14113 "Parser/parser.cc"
    break;

  case 973:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14119 "Parser/parser.cc"
    break;

  case 974:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14125 "Parser/parser.cc"
    break;

  case 975:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14131 "Parser/parser.cc"
    break;

  case 976:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14137 "Parser/parser.cc"
    break;

  case 977:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14144 "Parser/parser.cc"
    break;

  case 979:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14150 "Parser/parser.cc"
    break;

  case 980:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14156 "Parser/parser.cc"
    break;

  case 981:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14162 "Parser/parser.cc"
    break;

  case 982:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14168 "Parser/parser.cc"
    break;

  case 983:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14174 "Parser/parser.cc"
    break;

  case 984:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14180 "Parser/parser.cc"
    break;

  case 985:
#line 3880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14186 "Parser/parser.cc"
    break;

  case 986:
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14192 "Parser/parser.cc"
    break;

  case 987:
#line 3884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14198 "Parser/parser.cc"
    break;

  case 988:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14204 "Parser/parser.cc"
    break;

  case 989:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14210 "Parser/parser.cc"
    break;

  case 990:
#line 3893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14216 "Parser/parser.cc"
    break;

  case 991:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14222 "Parser/parser.cc"
    break;

  case 992:
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14229 "Parser/parser.cc"
    break;

  case 994:
#line 3913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14235 "Parser/parser.cc"
    break;

  case 995:
#line 3915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14241 "Parser/parser.cc"
    break;

  case 996:
#line 3920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14247 "Parser/parser.cc"
    break;

  case 997:
#line 3922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14253 "Parser/parser.cc"
    break;

  case 998:
#line 3927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14259 "Parser/parser.cc"
    break;

  case 999:
#line 3929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14265 "Parser/parser.cc"
    break;

  case 1000:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14271 "Parser/parser.cc"
    break;

  case 1001:
#line 3936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14277 "Parser/parser.cc"
    break;

  case 1002:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14283 "Parser/parser.cc"
    break;

  case 1003:
#line 3943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14289 "Parser/parser.cc"
    break;

  case 1004:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14295 "Parser/parser.cc"
    break;

  case 1006:
#line 3963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14301 "Parser/parser.cc"
    break;

  case 1007:
#line 3965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14307 "Parser/parser.cc"
    break;

  case 1008:
#line 3970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 14313 "Parser/parser.cc"
    break;

  case 1009:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 14319 "Parser/parser.cc"
    break;

  case 1010:
#line 3974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14325 "Parser/parser.cc"
    break;

  case 1011:
#line 3976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14331 "Parser/parser.cc"
    break;

  case 1012:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14337 "Parser/parser.cc"
    break;

  case 1014:
#line 3984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14343 "Parser/parser.cc"
    break;

  case 1015:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14349 "Parser/parser.cc"
    break;

  case 1016:
#line 3988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14355 "Parser/parser.cc"
    break;

  case 1017:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 14361 "Parser/parser.cc"
    break;

  case 1018:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14367 "Parser/parser.cc"
    break;

  case 1019:
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14373 "Parser/parser.cc"
    break;

  case 1020:
#line 4003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 14379 "Parser/parser.cc"
    break;

  case 1021:
#line 4005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 14385 "Parser/parser.cc"
    break;

  case 1022:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 14391 "Parser/parser.cc"
    break;

  case 1023:
#line 4015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 14397 "Parser/parser.cc"
    break;

  case 1025:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 14403 "Parser/parser.cc"
    break;

  case 1026:
#line 4028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 14409 "Parser/parser.cc"
    break;

  case 1028:
#line 4031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 14415 "Parser/parser.cc"
    break;

  case 1029:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 14421 "Parser/parser.cc"
    break;

  case 1031:
#line 4039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 14427 "Parser/parser.cc"
    break;

  case 1032:
#line 4041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 14433 "Parser/parser.cc"
    break;

  case 1033:
#line 4046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 14439 "Parser/parser.cc"
    break;

  case 1034:
#line 4048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 14445 "Parser/parser.cc"
    break;

  case 1035:
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 14451 "Parser/parser.cc"
    break;

  case 1036:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 14457 "Parser/parser.cc"
    break;

  case 1037:
#line 4086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14463 "Parser/parser.cc"
    break;

  case 1040:
#line 4093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 14470 "Parser/parser.cc"
    break;

  case 1041:
#line 4096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14476 "Parser/parser.cc"
    break;

  case 1042:
#line 4098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14482 "Parser/parser.cc"
    break;

  case 1043:
#line 4103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 14488 "Parser/parser.cc"
    break;

  case 1044:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 14494 "Parser/parser.cc"
    break;

  case 1045:
#line 4107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14500 "Parser/parser.cc"
    break;

  case 1046:
#line 4109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14506 "Parser/parser.cc"
    break;

  case 1047:
#line 4111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14512 "Parser/parser.cc"
    break;

  case 1049:
#line 4117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14518 "Parser/parser.cc"
    break;

  case 1050:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14524 "Parser/parser.cc"
    break;

  case 1051:
#line 4121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14530 "Parser/parser.cc"
    break;

  case 1052:
#line 4126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 14536 "Parser/parser.cc"
    break;

  case 1053:
#line 4128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14542 "Parser/parser.cc"
    break;

  case 1054:
#line 4130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14548 "Parser/parser.cc"
    break;

  case 1056:
#line 4137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14554 "Parser/parser.cc"
    break;

  case 1058:
#line 4148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 14560 "Parser/parser.cc"
    break;

  case 1059:
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14566 "Parser/parser.cc"
    break;

  case 1060:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 14572 "Parser/parser.cc"
    break;

  case 1061:
#line 4156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14578 "Parser/parser.cc"
    break;

  case 1062:
#line 4158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14584 "Parser/parser.cc"
    break;

  case 1063:
#line 4160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 14590 "Parser/parser.cc"
    break;

  case 1065:
#line 4175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14596 "Parser/parser.cc"
    break;

  case 1066:
#line 4177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14602 "Parser/parser.cc"
    break;

  case 1067:
#line 4182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 14608 "Parser/parser.cc"
    break;

  case 1068:
#line 4184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 14614 "Parser/parser.cc"
    break;

  case 1069:
#line 4186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14620 "Parser/parser.cc"
    break;

  case 1070:
#line 4188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14626 "Parser/parser.cc"
    break;

  case 1071:
#line 4190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14632 "Parser/parser.cc"
    break;

  case 1073:
#line 4196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14638 "Parser/parser.cc"
    break;

  case 1074:
#line 4198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14644 "Parser/parser.cc"
    break;

  case 1075:
#line 4200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14650 "Parser/parser.cc"
    break;

  case 1076:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14656 "Parser/parser.cc"
    break;

  case 1077:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14662 "Parser/parser.cc"
    break;

  case 1080:
#line 4217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14668 "Parser/parser.cc"
    break;

  case 1083:
#line 4228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14674 "Parser/parser.cc"
    break;

  case 1084:
#line 4230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14680 "Parser/parser.cc"
    break;

  case 1085:
#line 4232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14686 "Parser/parser.cc"
    break;

  case 1086:
#line 4234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14692 "Parser/parser.cc"
    break;

  case 1087:
#line 4236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14698 "Parser/parser.cc"
    break;

  case 1088:
#line 4238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14704 "Parser/parser.cc"
    break;

  case 1089:
#line 4245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14710 "Parser/parser.cc"
    break;

  case 1090:
#line 4247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14716 "Parser/parser.cc"
    break;

  case 1091:
#line 4249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14722 "Parser/parser.cc"
    break;

  case 1092:
#line 4251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14728 "Parser/parser.cc"
    break;

  case 1093:
#line 4253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14734 "Parser/parser.cc"
    break;

  case 1094:
#line 4256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14740 "Parser/parser.cc"
    break;

  case 1095:
#line 4258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14746 "Parser/parser.cc"
    break;

  case 1096:
#line 4260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14752 "Parser/parser.cc"
    break;

  case 1097:
#line 4262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14758 "Parser/parser.cc"
    break;

  case 1098:
#line 4264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14764 "Parser/parser.cc"
    break;

  case 1099:
#line 4269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14770 "Parser/parser.cc"
    break;

  case 1100:
#line 4271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14776 "Parser/parser.cc"
    break;

  case 1101:
#line 4276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14782 "Parser/parser.cc"
    break;

  case 1102:
#line 4278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14788 "Parser/parser.cc"
    break;

  case 1104:
#line 4305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14794 "Parser/parser.cc"
    break;

  case 1108:
#line 4316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14800 "Parser/parser.cc"
    break;

  case 1109:
#line 4318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14806 "Parser/parser.cc"
    break;

  case 1110:
#line 4320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14812 "Parser/parser.cc"
    break;

  case 1111:
#line 4322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14818 "Parser/parser.cc"
    break;

  case 1112:
#line 4324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14824 "Parser/parser.cc"
    break;

  case 1113:
#line 4326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14830 "Parser/parser.cc"
    break;

  case 1114:
#line 4333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14836 "Parser/parser.cc"
    break;

  case 1115:
#line 4335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14842 "Parser/parser.cc"
    break;

  case 1116:
#line 4337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14848 "Parser/parser.cc"
    break;

  case 1117:
#line 4339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14854 "Parser/parser.cc"
    break;

  case 1118:
#line 4341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14860 "Parser/parser.cc"
    break;

  case 1119:
#line 4343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14866 "Parser/parser.cc"
    break;

  case 1120:
#line 4348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14872 "Parser/parser.cc"
    break;

  case 1121:
#line 4350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14878 "Parser/parser.cc"
    break;

  case 1122:
#line 4352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14884 "Parser/parser.cc"
    break;

  case 1123:
#line 4357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14890 "Parser/parser.cc"
    break;

  case 1124:
#line 4359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14896 "Parser/parser.cc"
    break;

  case 1125:
#line 4361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14902 "Parser/parser.cc"
    break;

  case 1128:
#line 4385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14908 "Parser/parser.cc"
    break;

  case 1129:
#line 4387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14914 "Parser/parser.cc"
    break;


#line 14918 "Parser/parser.cc"

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
#line 4390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
