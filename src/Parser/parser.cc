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
#define YYLAST   30873

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  189
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1129
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2164

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

#define YYPACT_NINF (-1836)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1128)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      71, 11778,   182,   233, 23265,    67, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,    88,   843,
     108, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836,    92,   257, -1836, -1836, -1836, -1836,
   -1836, -1836,  5959,  5959,   120, 11778,   227,   271, 27783, -1836,
     299, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836,   332,  2855, -1836,   955, 14114, -1836, -1836,  2095, -1836,
   -1836, -1836, -1836, 16914, -1836,   316,   328,   378,   337,   130,
   -1836,  5731,   381,   407,   420,   441,  5777,   635,   883, 11964,
   -1836, -1836,   568, 16749,  2742, -1836, -1836, -1836, -1836,  3926,
     668, 10337,  8556,   943,  3926,   961,   492, -1836, -1836, -1836,
   -1836,   114, -1836, -1836, -1836, -1836,   548, -1836, -1836, -1836,
   -1836, -1836,   588,   591,   114, -1836,   114, 21235, -1836, -1836,
   -1836, 24620,  5959, -1836, -1836,  5959, -1836, 11778, -1836,   581,
   24783, -1836, -1836,  5874, 26638, -1836, -1836,  1255,  1255,   626,
    2960, -1836, -1836, -1836, -1836,    64, 19282,   114,  3928,   114,
   -1836, -1836, -1836, -1836, -1836, -1836,   660, -1836,   629,   688,
    2437, -1836,   730, 30232, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, 21728,  2887,  4777,  2855,   455,   702,   705,   713,   720,
     723,   738, -1836, -1836, 30309,   763, 30386,   778,   798,   114,
   30232, 30463,   800, 27905, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, 30540, 30540, 21561, 13234, 23428,  4290,   803, -1836, -1836,
   -1836, -1836,   673, -1836,   683,   878, -1836,  1125,  5632, 22229,
   30232, -1836,   823,   651,   718,  1067,   494,  1187,   840,   844,
     849,   903,    11, -1836,   885, -1836, -1836,  3948,  4495,   902,
   19970,  9393,  3926,  3926,   917,  3926,  1506,  3926,  2161,   892,
   -1836, -1836,   114, -1836,  1096,  1198, -1836, -1836, -1836, -1836,
   24946,  5959, -1836, -1836, 25109,  5022, -1836, -1836, 14290,   910,
   -1836, 17244, -1836, -1836, -1836, -1836,   920, -1836, -1836, -1836,
     919, -1836, 27994,  1073, 28148, -1836,   930,  5959,   591,   964,
     970, -1836,  2095,  5874,  2095, -1836, -1836, -1836,  5081,  4317,
     969,  1070,   183,  1070, -1836,   114,   114,    35, 20897,   450,
    1070, -1836,   114,   114,    35,   114, -1836,   114, -1836,  4718,
   -1836, -1836,  1033,  1042,  1255, 27578, 19454, 16914, -1836,  5731,
   -1836,  3926, -1836,  1549,   492,  1017,  1114, 20897,  5959,  5959,
     337, -1836, 12514, -1836,  1255,  1255,  1048,  1114, 20897,  5959,
   -1836,  8412, -1836, -1836, -1836,  1255, -1836, -1836, -1836, -1836,
    1255, -1836,  1258,  5278,  5959, -1836, 22948,  1059, -1836, -1836,
   -1836, 27433,   591, 21066,  1046,  5874, 22888, 27578, 30617, -1836,
   26950, -1836,  1070,    49, -1836, 30232, 26794,  5294,  4718, -1836,
     546, -1836, -1836, -1836, -1836, 20142, 24783,  1070,  5959, -1836,
    1072,  1074, -1836, -1836, -1836, -1836,  5959,  4222,    40,   694,
   -1836,  5959,   629, -1836,  1225, -1836, 14466, 25272,  1051, 19970,
   -1836,  1255,  1255,  1089, -1836,   920,  4058,   521,   637, -1836,
     530,   492,  1104,  1108, -1836,  2960,  1124,   629,  2960, -1836,
   -1836,  2887, -1836,   773, -1836,  1136,  1183, 28225, -1836, 30232,
   -1836,   779,   892, -1836, 13410, 22396, -1836,   553, -1836, -1836,
     786, -1836, -1836,   835,  4777,  1199,  1205,  1215,  1218,  1231,
    1247, -1836, -1836,   624,  1134, -1836,   861,  1134, 21895, -1836,
    4290, 22062, -1836, 25435, 24783,  5498, -1836, 21895, -1836, 30232,
   -1836, -1836, -1836, -1836, -1836, -1836, 22062, -1836, -1836, 23968,
   25435, 25435, 13586,  1624,  1831,   748,  1953, -1836,   876,  1249,
    1228,  1256, -1836, 23102, 28302,  1238, 15170, 22563,  1263, 30694,
    1278, -1836, 24620, -1836, 28379,  1261, -1836, 30232,  2095, 30232,
    2095, -1836, -1836,  2573, -1836, -1836,  9184,  2550, 30232,  9184,
    2095, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836,  1273, 30232, -1836, -1836, 13762, -1836, -1836, 25598,
   -1836,  1255,  1255, -1836, -1836,   920, -1836, -1836, 30232, 30232,
   30232, 30232, 30232, 30232, 30232, 30232, 30232, 30232, 30232, 30232,
   30232, 30232, 30232, 30232, 30232, 30232, 30232, 28456, -1836, -1836,
   12331, 28533,  1411, 30232,  3160,   655,  1270, -1836,   114,   114,
    1270,   674, -1836,   114,   114,  1286,  1270, -1836,   114,   114,
   -1836,  1134, -1836, 28610, 20314, 25272, -1836,  5959, 23908,  1255,
    1255, -1836,  4419,  5498, -1836, 20142, -1836, 20142, -1836, 26482,
   -1836,  1270, -1836, 24946, -1836, -1836,   106, 24131, -1836, -1836,
   -1836, -1836, 22739,  4349, 28225, 27994,  1289,  1295, -1836, -1836,
    1294, 28148,   889, -1836, -1836, -1836, 22396,  1317, -1836,   730,
   -1836, -1836, -1836,  1284,  5081,   817,  1323,  1337,  1343,   846,
    1346,  1352,  1359,  1365,  1369,  1376,  4317, -1836, -1836, -1836,
     114,  1360, 27106, -1836, -1836,  1286,   337, -1836, -1836,   591,
    1114, 23600, -1836, -1836,   337, -1836, -1836,   591, -1836, -1836,
    4718, -1836, 22396, 22396, -1836,  1255,  5874, 27723, 14642,  1663,
   -1836, -1836, -1836, -1836, -1836, -1836,   591,  1114,    49,  1383,
   -1836, -1836,  3926,  1400,  1114, 20897, -1836,   591,  1114, -1836,
    8654, -1836,  1255,  1255, -1836, -1836,  1405,   319,  1425,   492,
    1426, -1836, -1836, -1836, 23908,  1434,  1432, -1836, -1836,   895,
   -1836,  1525, -1836,  1418, -1836, -1836, -1836, 25770,  1440,  1442,
   -1836, -1836, -1836, -1836, -1836,  5294,   936,  4718, 23600,  1070,
   11778, -1836,  5959,  1443, 16515,  1446, -1836, -1836, -1836, -1836,
   -1836,  2960, -1836, -1836,  1538, 24294, 18766,  1606,  1034, -1836,
   -1836,   114,  1454,   114,  1108,   533,  1455,   898, 24783,   913,
     965, -1836,  2887,  9184,  1444,  1459, -1836,   730, 18938,  2368,
   -1836, -1836,   114,   114, -1836, -1836, 22396, -1836, -1836,   685,
    1134, -1836,   981,  1134, 23600, -1836, -1836,  1286, 23600, -1836,
    1286,  1478,  1480,  1461,  1481, 14818,  1479,  1489, -1836,  1490,
    1491,  1492,  1493, 30232,  1494,  1497,  1498, 25830,  1241, 30232,
   -1836, -1836,  2185, -1836, -1836, -1836, 30232, -1836,  1501,  1502,
   28071, 19110, -1836, 24946, -1836, -1836, -1836,  1266, 28302, 14994,
    1503, 22229,  1521,   892,  1523, 15346, -1836, -1836, -1836, -1836,
    9184,  1524, -1836,  1527, -1836, -1836,  4933, -1836,  2095,  1504,
    1529, -1836, -1836, -1836,  4933, -1836, -1836,  1269,  1531, -1836,
   27994, -1836, -1836, -1836,   823,   823,   823,   651,   651,   718,
     718,  1067,  1067,  1067,  1067,   494,   494,  1187,   840,   844,
     849,   903, 30232,  1279, 19110, 12331,  1536,  1005,  1539,  1540,
    1542,  1543,  1545,  1546,  1547, -1836,  1290,  3669, -1836,  3160,
   -1836, -1836, -1836, 23600, -1836, -1836, -1836, -1836, -1836, -1836,
   23600, -1836, -1836, -1836, -1836, -1836, -1836, -1836,  1286, -1836,
    1528, -1836, -1836, -1836,  1270,  1255,  4933, -1836, -1836,  1283,
   -1836, -1836, -1836, -1836, -1836, -1836, 19110, -1836,  5959,  4933,
   -1836,  1287,    44,  1293,  1550,  1294, -1836, 27994,  1551, -1836,
    2743, 30232, -1836, -1836,   980, -1836,  1552, 19110, 30232,   992,
    1554,  1556,  1560,   997,  1565,  1567,  1568,  1570,  1572,  1573,
     686,  1134, -1836, -1836,   691,  1134, -1836, -1836,   693,  1134,
   -1836, -1836, -1836,  5874,  1693,  1134,   480, -1836,   892,  1299,
   -1836, -1836,   591,  1577, -1836, -1836, -1836,  1025,  1580,  1030,
    1584, -1836,  1263,  1589, -1836,   591, -1836, 10641, -1836,   591,
    1114,  1589, -1836,   591,  1585,  1586,  1588, -1836, -1836, 23763,
   -1836,  2095,  5959, 11219,  1678, -1836, -1836, -1836, 19110,  1066,
   -1836,  1589,  1595, -1836, -1836, -1836, -1836,  5874, 25993, 15863,
   -1836,   185,   318, 22396,  1574, -1836,  1574, -1836, -1836,   114,
    1034, -1836,   533,  1108,  1591,    64, -1836, -1836,  1596,  5959,
     533, -1836, -1836,  1599,  1601, -1836,  1602, -1836,  1607,  1608,
    1609,  1612,  1625,  2368, -1836, -1836, -1836, -1836, -1836, 23600,
   -1836, -1836,  1286, 23600, -1836,  1286,  1626,  1627,   645, -1836,
   23908,   645,  2095, -1836,   645, -1836, 24457,   645, -1836, 30232,
   30232, 30232, 20314, -1836, -1836, -1836, -1836, 30232, 30232,  1622,
   27994, -1836, -1836,  1629,  1630,  1631, -1836, -1836, -1836, 28687,
    1305, -1836, -1836, -1836,  1628, 19110, 19110,  1642, -1836, -1836,
   -1836,  2999, -1836, -1836,  1308, -1836,   204,  1618, -1836,  9184,
    1310, -1836, 28302, -1836,  1294, -1836, 30232, -1836,  1641, -1836,
     701,  1134, -1836,   767,   914,  1134, -1836,  1255, 16582,  3788,
   -1836,   114,   114, -1836, -1836, -1836,  1643,  1645, -1836, -1836,
    1314, -1836, 20142, -1836,   337,  1316, 30232, 30232, -1836, -1836,
    1650, -1836, 28148, -1836,  1639,   114, 19110,   114, -1836, -1836,
     922,  1134, -1836,   931,  1134, -1836, -1836,   944,  1134, 23600,
   -1836, -1836,  1286, 23600, -1836, -1836,  1286, 23600, -1836, -1836,
    1286,  1070, -1836,  1286, -1836, 30232, -1836, 30232, -1836, 27266,
   -1836, -1836, -1836, -1836, -1836, -1836,  1651, -1836, -1836, -1836,
   16026,  1589, -1836,   591, -1836, -1836, -1836, -1836, -1836, 17767,
   -1836, -1836, -1836, -1836, -1836,   398,   615,    70, 13058,  1652,
    1653, 20711,  1655,  1661,  3372,  3595,  4268, 28764,  1665, -1836,
   -1836,  1666,  1673, 20711,  1675, -1836, -1836,   591, 30232, 30232,
    1826,  1671,   650, -1836, 21394, 13938,  1672,  1681,  1669, -1836,
   -1836, -1836, 11026, -1836, -1836, -1836, -1836, -1836,  3244, -1836,
   -1836, -1836,  1392,   200, -1836,   330, -1836,   200, -1836, -1836,
   -1836, -1836, -1836,  2095, -1836, -1836, 12148, 17079, -1836,  5959,
   -1836, -1836, -1836,  5959, -1836, -1836, -1836,  5959, -1836,  5874,
   -1836,  1071, 24783,   629,   629,  1108,  1596,  1682,   533,   492,
     281,  1696,  1674,  1596, 16189, -1836, -1836, -1836, -1836,   957,
    1134, -1836, -1836,  1694,  1700, -1836, -1836,   851,  1701,  1697,
    1112, -1836,  1698, -1836, -1836, -1836, -1836, -1836, 27994,  1294,
   -1836, 19626, 19798, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
    1740,  4933, -1836,  1740,  1740, -1836,  4933,  4634,  4856, 30232,
   -1836, -1836,  1324,  1710, -1836,  1709,   114, 23600, -1836, -1836,
    1286, 23600, -1836, -1836, 23600, -1836, -1836,  1286, 30232, 30232,
    1714,  1716, -1836,  1713, -1836, -1836, -1836, -1836, -1836, -1836,
    1721, -1836, -1836,  1722, -1836, -1836, -1836, -1836, -1836, -1836,
    1726, 23600, -1836, -1836,  1286, 23600, -1836, -1836,  1286, 23600,
   -1836, -1836,  1286,  1727,  1735,  1736,   337,  1329, -1836,     6,
   -1836,   892,  1741, -1836, -1836, -1836,  1743, 17933, 18099, 18265,
   26156, 27578, 25435, 25435,  1739,  1718,   446,   481,  2659, 18594,
   -1836,   487,  5959,  5959, -1836,  9184,   107,   188, -1836, -1836,
   -1836, -1836, 13058, 30232,  1744,  1821, 12881, 11406, -1836,  1723,
   -1836,  1728, 30232,  1746, 27994,  1749, 30232, 22396, 30232, -1836,
   11592,  1126, -1836,  1755,    61, -1836,    56,  1810,   418,   114,
   -1836,  1752, -1836,  1756, -1836,  1758,  1764,  1768, 20711, 20711,
   -1836, -1836,  1818, -1836, -1836,    58,    58,   554, 12697,   499,
    1783,  1788,    40, -1836, -1836, -1836, -1836, -1836, -1836,  1781,
    1791,   533,  1596,    64,  5959, -1836, 28841, -1836,  1793, -1836,
   16352, 23600, -1836, -1836,  1286, -1836, -1836,  1792, -1836, -1836,
   30232, -1836, 24457, 30232,  1294,  1794, -1836,  1790, -1836,  1795,
    1797, -1836,  1331, -1836,  4933, -1836,  4933, -1836, -1836, -1836,
   -1836, -1836,  1808,  1809,  1811, -1836, -1836,  1815, -1836,  1816,
   -1836, -1836,  1812,   114,  1814,  1820,  1824, -1836, -1836, -1836,
   -1836, -1836, 30232, -1836,  1830, -1836,  1739,  1739,  1739,  4078,
    1057,  1798,   522, -1836,  4078,   531, 22396, -1836, -1836, -1836,
   -1836,  5415, 30232,  5153,   439, -1836, -1836,   113,  1823,  1823,
    1823,  5959, -1836, -1836, -1836,  1837, -1836, -1836, -1836, -1836,
    1681,  1840, 30232,   328,  1838,   441, 18438, 26156,  1113,  1845,
   20711,  1844, -1836, -1836, -1836, -1836,  1159, 20711, 30232,  1347,
     196, -1836, 30232, 27912, -1836, -1836,   594, -1836,  1294, -1836,
    1115,  1120,  1133,   496, -1836, -1836, -1836, -1836,   591,  1126,
    1848, -1836, -1836, 30232, -1836,  1849,   730, -1836, 10818, -1836,
   -1836, -1836, 30232, 30232, -1836, -1836,   362,    58, -1836,   406,
   -1836, -1836, -1836,   114, -1836,  1574,   533, -1836,  1596,  1851,
     492,  1674, 27994, -1836, -1836, -1836,  1850, -1836, -1836, -1836,
   -1836,  1854, -1836,   114,   114, -1836,  1345,  1368, -1836, -1836,
   -1836,  1852,  1861, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836,   597,  1057,  2421,   602, -1836,
   -1836, -1836, -1836,   114,   114, -1836, -1836, -1836,   604, -1836,
    1143,  5415,   619, -1836,  5153, -1836,   114, -1836, -1836, -1836,
   -1836, -1836, -1836, 20711, 20711,  1681, 20486,    38, 28918,  1935,
   20711, -1836, -1836, -1836, -1836, -1836, 30232, -1836, 28995,  1948,
    1843,  9898, 29072, 20711, 11592,  1681,  1028,  1518,  1857, 30232,
   -1836,  1870,    42, 20711, -1836, 20711, -1836,  1873, -1836, 26319,
    1858,   730,   711, -1836, -1836,  1868,  1374,  1144, 20711,  1875,
   20711, 20711, 20711, -1836,   629,  1596,  1878, -1836, -1836,  1294,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,  1882,
    1884,  1888,  2421, -1836,   114, -1836, -1836, -1836, -1836, -1836,
    1887,  4078, -1836,  1973,  8665,    82, 15525, -1836, 20585, -1836,
      16,  1164, 20711,  1974,   607,  1880,   254, 20711, 30232,  1028,
    1518,  1869, 29154,  1101,  1255,  1881,   305,  1980, -1836, 29231,
   29308, 30232,  1681,  1877, 15700, -1836, -1836, -1836, 26319,  1879,
    5577, 25598,  2095, -1836,  1896,  1883,    51, -1836, 30232,  9184,
   -1836, -1836, 30232,   200, -1836, -1836, -1836,  1907, -1836,  1910,
    1026,  1134, -1836, -1836,  1057, -1836, 20711, -1836,    46, -1836,
      73, -1836, -1836, -1836,  1911, 17428, -1836, -1836, 20711, -1836,
      36, -1836, 20711, 30232,  1912, 29385, -1836, -1836, 29462, 29539,
   30232,  5498,  1681, -1836,   892, 29616, 29693, 20711,  1895,   377,
    1898,   394, -1836, -1836,  1919, 17428,  1879, 30232,  1917,  3912,
    4575, -1836, -1836, -1836,  1914, -1836,  1972,  1931,   726,  1927,
   -1836, -1836,  1933,  1170,   452, -1836, -1836, 23600, -1836, -1836,
    1286, -1836, -1836, 30232, -1836, 30232, -1836, -1836,  1465, 17601,
   -1836, -1836, 20711, -1836, -1836,  1681, -1836, -1836,  1681,  1921,
     421,  1922,   474, -1836, -1836,   492, -1836,  1681, -1836,  1681,
   -1836,  1941, 29770, 29847, 29924, -1836,  1465,  1943, -1836,   591,
    4575,    51,  1942, 30232,  1932,    51,    51, -1836, -1836, 20711,
    2027,  1956, -1836, -1836, 20585, -1836,  1465, -1836, -1836,  1959,
   30001, 30078, 30155, -1836, -1836,  1681, -1836,  1681, -1836,  1681,
   -1836,   591, -1836,  1955,   730,  1961, -1836,   739, -1836, -1836,
   20711, -1836, -1836, 10138,  1965, 20585, -1836, -1836,  1681, -1836,
    1681, -1836,  1681,  1966, -1836,   730,  1968, -1836,  1944,   730,
   -1836, -1836, -1836, -1836, 10409, -1836, -1836,  1382, 30232, -1836,
    1171,   730,  2095,  1969,  1947, -1836, -1836,  1176, -1836, -1836,
    1950,  2095, -1836, -1836
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
     104,     0,     0,    20,     0,   499,   102,   103,     0,   841,
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
       0,     0,   605,   608,     0,     0,     0,     0,     0,   864,
       0,     0,     0,    27,    29,     4,     8,    25,     5,     6,
       7,     0,     0,   499,   499,   499,     0,   102,   105,   106,
     107,   108,    85,    28,    86,    24,    46,    84,   109,   499,
       0,   124,   126,   130,   133,   136,   141,   144,   146,   148,
     150,   152,   154,   165,     0,    30,   729,     0,  1128,     0,
     500,   499,   511,   490,   565,   491,   590,   492,   597,   601,
     594,   615,   864,   616,     0,     0,   725,   730,   715,   719,
     499,   731,  1081,  1082,   499,   732,   734,   880,   499,     0,
    1108,   587,   909,   927,  1112,  1105,  1103,  1110,   435,   434,
       0,   173,   748,   172,     0,   443,     0,     0,     0,     0,
       0,   449,     0,     0,     0,   433,   996,   997,     0,     0,
     472,   862,   864,   862,   883,   864,   864,   482,   499,   864,
     862,   940,   864,   864,   481,   864,   959,   864,   937,     0,
     580,   581,     0,     0,   499,   499,   499,   499,   452,   862,
     502,   512,   573,     0,   602,     0,   845,   499,     0,     0,
     742,   453,   587,   566,   583,   598,     0,   845,   499,     0,
     515,   567,   574,   575,   486,   584,   488,   489,   487,   589,
     599,   603,     0,   617,     0,   814,   499,     2,   843,   901,
     903,   499,     0,   499,     0,     0,   587,   499,     0,  1116,
     587,  1119,   862,   862,     3,     0,   587,     0,     0,   465,
     864,   857,   859,   858,   860,   499,   499,   862,     0,   818,
       0,     0,   777,   779,   778,   780,     0,     0,   773,     0,
     762,     0,   771,   783,     0,   684,   499,   499,  1128,   500,
     565,   590,   597,     0,   731,   732,   686,   604,   610,   687,
     688,   689,     0,   686,   867,     0,   794,   782,     0,   878,
     877,   873,   876,     0,   871,   874,     0,     0,   109,     0,
     157,     0,     0,   611,   499,   499,   788,   738,   740,   787,
       0,   737,   741,     0,     0,     0,     0,     0,     0,     0,
       0,   881,   907,   864,   917,   925,   929,   935,   499,    92,
       0,   499,   100,   499,   499,     0,    87,   499,    94,     0,
      36,    40,    41,    37,    38,    39,   499,    90,    91,   499,
     499,   499,   499,   105,   106,     0,     0,   191,     0,     0,
     631,     0,  1103,   499,     0,     0,   500,   499,   604,     0,
       0,  1114,   499,  1117,     0,     0,  1034,     0,     0,     0,
       0,    26,    59,     0,    65,    66,   158,     0,     0,   158,
       0,   174,   175,   176,   177,   178,   179,   180,   181,   182,
     183,   184,   172,     0,   170,   171,   499,    88,  1083,   500,
     496,   497,   498,  1087,  1078,  1079,  1085,    89,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1033,     2,
     499,     2,   103,     0,  1043,   864,  1128,   978,   864,   864,
    1128,   864,   993,   864,   864,  1057,  1128,  1039,   864,   864,
    1048,  1055,   723,     0,   499,   499,   595,   733,   500,   591,
     592,   596,     0,     0,   460,   499,  1120,   499,  1093,   500,
    1098,  1128,  1090,   499,  1095,     2,  1128,   499,   910,   928,
    1104,     2,    27,     0,     0,   748,    28,     0,   746,   749,
    1126,     0,     0,   755,   744,   743,   499,     0,   847,     0,
       2,   464,   466,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   886,   943,   966,
     864,   478,     0,   882,   890,  1024,   742,   884,   885,     0,
     845,   499,   939,   947,   742,   941,   942,     0,   958,   960,
       0,   468,   499,   499,   571,   500,     0,   587,   499,     0,
    1109,  1113,  1111,   450,   588,   818,     0,   845,   862,     0,
     444,   454,   513,     0,   845,   499,   818,     0,   845,   792,
     568,   569,   585,   600,   606,   609,   604,   610,   628,   629,
       0,   793,   701,   735,   500,     0,   702,   704,   705,     0,
     213,   427,   844,     0,   425,   482,   481,   587,   102,     0,
     446,     2,   447,   815,   470,     0,     0,     0,   499,   862,
     499,   818,     0,     0,     0,     0,   776,   775,   774,   768,
     510,     0,   766,   784,   563,   499,   499,   103,  1043,   733,
     685,   864,     0,   864,   686,   686,     0,     0,   499,     0,
       0,   869,   873,   158,     0,     0,   437,     0,   499,  1008,
     739,  1005,   864,   864,  1013,   612,   499,   870,   908,   864,
     918,   926,   930,   936,   499,   911,   913,   915,   499,   931,
     933,     0,     0,     0,     0,   499,     0,     0,   688,     0,
       0,     0,     0,     0,     0,     0,     0,   499,     0,     0,
     123,   122,     0,   119,   118,    31,     0,    32,     0,     0,
       0,   499,  1089,   499,  1094,   188,   187,     0,     0,   499,
     102,   499,     0,   602,     0,   500,     2,     2,  1115,  1118,
     158,     0,    55,     0,    56,    63,     0,    62,   162,     0,
     159,   160,   164,    58,     0,    57,    61,     0,     0,    54,
     748,   166,  1080,   125,   127,   128,   129,   131,   132,   134,
     135,   139,   140,   137,   138,   142,   143,   145,   147,   149,
     151,   153,     0,     0,   499,   499,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1058,     0,   864,  1129,  1044,
     981,   998,  1045,   499,   976,   984,   721,   979,   980,   722,
     499,   991,  1001,   994,   995,   724,  1041,  1042,  1056,  1121,
       0,  1084,  1088,  1086,  1128,   593,     0,    33,   628,     0,
     717,   716,   720,   726,  1092,  1097,   499,   727,     0,     0,
     757,   157,     0,     0,     0,  1126,   754,  1127,     0,   750,
       0,     0,   753,   756,     0,     2,     0,   499,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     864,   895,   899,   938,   864,   952,   956,   964,   864,   974,
     887,   944,   967,     0,     0,  1020,     0,  1025,  1026,     0,
     476,   848,     0,     0,   477,   849,   469,     0,     0,     0,
       0,   467,     0,     2,   850,     0,   448,     0,   818,     0,
     845,     2,   851,     0,     0,     0,     0,   643,   904,   499,
     922,     0,     0,   499,   428,   426,  1036,  1035,   499,     0,
     819,     2,     0,   770,   811,   806,   807,     0,   500,     0,
     802,     0,     0,   499,   764,   763,   764,   564,   562,   864,
    1044,   680,   686,   686,     0,     0,   696,   695,  1126,     0,
     686,   797,   795,     0,     0,   872,     0,   830,     0,     0,
       0,     0,     0,  1009,  1010,  1006,  1007,   790,   789,   499,
     912,   914,   916,   499,   932,   934,     0,     0,    93,    96,
     499,   101,     0,    99,    95,    97,   499,     0,   113,     0,
       0,     0,   499,   117,   121,   120,   192,     0,     0,     0,
     748,   110,   709,     0,   710,   711,  1091,  1096,   185,     0,
       0,  1099,  1100,  1101,     0,   499,   499,     0,    49,    50,
      82,     0,    82,    82,     0,    70,    72,     0,    52,     0,
       0,    48,     0,    51,  1126,   156,     0,     3,     0,  1052,
     864,   987,   990,   864,   864,  1051,  1054,   499,     3,     0,
    1040,   864,   864,   982,   999,  1046,     0,     0,  1122,   728,
       0,   461,   499,     3,   742,     0,     0,     0,   758,   759,
       0,   751,     0,   745,     0,   864,   499,   864,     3,   471,
     864,   896,   900,   864,   953,   957,   965,   864,   975,   499,
     888,   891,   893,   499,   945,   948,   950,   499,   968,   970,
     972,   862,   479,  1021,  1032,     0,  1031,     0,  1023,     0,
     853,   961,   577,   576,   579,   578,     2,   819,   854,   799,
       0,     2,   852,     0,   819,   855,   643,   643,   643,   499,
     703,   706,   707,   736,   431,     0,     0,     0,   499,     0,
       0,   352,     0,     0,     0,     0,     0,   193,     0,   347,
     348,     0,     0,   352,     0,   400,   399,     0,   168,   168,
     406,   604,   610,   210,   499,   499,     0,   194,     0,   221,
     195,   196,   499,   215,   197,   198,   199,   200,     0,   201,
     202,   353,     0,   367,   203,   373,   375,   378,   204,   205,
     206,   207,   208,     0,   209,   217,   587,   499,   219,     0,
       3,   832,   819,     0,   809,   786,   803,     0,   804,     0,
     805,     0,   499,   781,   781,   686,  1126,     0,   686,   692,
     686,     0,   697,  1126,     0,   868,   875,   436,  1017,   864,
    1016,  1019,  1011,     0,     0,   905,   923,  1037,     0,     0,
       0,    42,     0,   114,   116,   115,   112,   111,   748,  1126,
    1123,   499,   499,   190,   189,   186,  1102,     3,     3,    69,
      79,     0,    73,    80,    81,    64,     0,     0,     0,     0,
     161,    60,     0,     0,   155,     0,   864,   499,   983,   985,
     986,   499,  1000,  1002,   499,  1047,  1049,  1050,     0,     0,
     102,     0,     3,     0,   977,   992,   988,  1003,    34,   718,
       0,   445,   761,     0,   861,   747,   752,   846,     3,   863,
       0,   499,   889,   892,   894,   499,   946,   949,   951,   499,
     969,   971,   973,     0,     0,     0,   742,     0,  1027,     0,
    1028,  1029,     0,   801,   819,   856,     0,   499,   499,   499,
     499,   499,   499,   499,   626,     0,     0,     0,   657,   587,
     644,     0,     0,     0,   429,   158,     0,     0,   338,   339,
     218,   220,   499,     0,     0,     0,   499,   499,   334,     0,
     332,     0,     0,     0,   748,     0,     0,   499,     0,   379,
     499,     0,   169,     0,     0,   407,     0,     0,     0,   864,
     225,     0,   216,     0,   329,     0,     0,     0,   352,   352,
     358,   357,   352,   369,   368,   352,   352,     0,   587,     0,
       0,     0,   773,   808,   810,   785,   765,   769,   767,     0,
       0,   686,  1126,     0,     0,   675,     0,   691,     0,   798,
       0,   499,  1012,  1014,  1015,   906,   924,     0,  1038,    98,
       0,    35,   499,     0,  1126,     0,   713,   712,   714,     0,
       0,    83,     0,    71,     0,    77,     0,    75,   163,    47,
     167,  1125,     0,     0,     0,     3,     3,     0,  1060,     0,
    1124,   760,     0,   864,     0,     0,     0,   897,   954,   962,
     480,  1022,     0,   836,     0,   838,   626,   626,   626,   657,
     664,   631,     0,   670,   657,     0,   499,   618,   656,   655,
     651,     0,     0,     0,     0,   658,   660,   864,   672,   672,
     672,     0,   652,   668,   432,     0,   342,   343,   340,   341,
     234,     0,     0,   236,   440,   235,   587,   499,     0,     0,
     352,     0,   317,   319,   318,   320,     0,   352,   193,   274,
       0,   267,     0,   193,   335,   333,     0,   327,  1126,   336,
       0,     0,     0,     0,   388,   389,   390,   391,     0,   381,
       0,   382,   344,     0,   345,     0,     0,   372,     0,   214,
     331,   330,     0,     0,   361,   371,     0,   352,   374,     0,
     376,   398,   430,   864,   834,   764,   686,   676,  1126,     0,
     694,   697,   748,   698,   679,   800,     0,    53,    45,    43,
      44,     0,    67,   864,   864,    74,     0,     0,   989,  1004,
    1053,     0,     0,  1059,  1061,   455,   459,   898,   955,   963,
    1030,   840,   622,   624,   620,     0,     0,  1067,     0,   665,
    1072,   667,  1064,   864,   864,   650,   671,   654,     0,   653,
       0,     0,     0,   674,     0,   646,   864,   645,   661,   673,
     662,   663,   669,   352,   352,   237,   587,     0,     0,   255,
     352,   322,   325,   323,   326,   321,     0,   324,     0,   263,
       0,   193,     0,   352,   499,   275,     0,   300,     0,     0,
     328,     0,     0,   352,   351,   352,   392,     0,   383,   499,
       0,     0,     0,   212,   211,   354,     0,     0,   352,     0,
     352,   352,   352,   458,   781,  1126,     0,   678,   693,  1126,
    1018,    68,   457,   456,    78,    76,  1062,  1063,   648,     0,
       0,     0,  1068,  1069,   864,   649,  1065,  1066,   647,   627,
       0,     0,   350,   226,     0,     0,     0,   248,   352,   228,
       0,     0,   352,   257,   272,   283,   277,   352,   193,     0,
     287,     0,     0,     0,   312,   278,   276,   265,   268,     0,
       0,   193,   301,     0,     0,   231,   349,   380,   499,   386,
     393,   500,   397,   346,     0,     0,   408,   359,     0,   158,
     370,   363,     0,   364,   362,   377,   772,     0,   682,     0,
     864,  1075,  1077,  1070,     0,   659,   352,   243,   238,   241,
       0,   240,   247,   246,     0,   499,   250,   249,   352,   259,
       0,   256,   352,     0,     0,     0,   264,   269,     0,     0,
     193,     0,   288,   313,   314,     0,     0,   352,     0,   303,
     304,   302,   271,   337,     0,   499,   386,     0,     0,     0,
    1067,   394,   395,   396,     0,   401,     0,     0,     0,   409,
     410,   355,     0,     0,     0,   681,   699,   499,  1071,  1073,
    1074,   666,   227,     0,   245,     0,   244,   230,   251,   499,
     421,   260,   352,   261,   258,   273,   286,   284,   280,   292,
     290,   291,   289,   270,   315,   316,   285,   281,   282,   279,
     266,     0,     0,     0,     0,   233,   251,     0,   387,     0,
    1068,   408,     0,     0,     0,   408,     0,   360,   356,   352,
       0,     0,   239,   242,   352,     3,   252,   422,   262,     0,
       0,     0,     0,   311,   309,   306,   310,   307,   308,   305,
       3,     0,   384,     0,     0,     0,   402,     0,   411,   365,
     352,  1076,   222,     0,     0,   352,   299,   297,   294,   298,
     295,   296,   293,     0,   385,   414,     0,   412,     0,   414,
     366,   224,   223,   229,     0,   232,   415,     0,     0,   403,
       0,     0,     0,     0,     0,   416,   417,     0,   413,   404,
       0,     0,   405,   418
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1836,    14,  -107, -1836,    -1,   834,  2005,  4921,   -44, -1836,
    -145, -1836,   514, -1836,  -910,  -866, -1836,   355,  7762,  1993,
   -1836,    -4, -1836,  1604,   387,   960,   976,   811,   949,  1533,
    1534,  1526,  1537,  1530, -1836,    47,  -105,  -546, -1836,   939,
    8657,   810, -1836,  1906, -1836, -1836,  -803,  7539, -1153,  5233,
   -1836,  1847, -1836,   806,    34, -1836, -1836,   617,   116, -1836,
   -1679, -1826,   302,    90, -1836, -1836,   608,   317, -1836, -1646,
   -1836, -1345, -1836, -1836, -1836, -1836,   136, -1300, -1836, -1836,
   -1337,   416, -1836, -1836, -1836, -1836, -1836,     9, -1279, -1836,
   -1836, -1836, -1836, -1836,   157,   442,   443,   232, -1836, -1836,
   -1836, -1836,  -739, -1836,    98,    48, -1836,   170, -1836,  -263,
   -1836, -1836, -1836,   813,  -286,  -965,  -188, -1836,    62,     3,
     275,  6178,  -769,  -758, -1836,  -131, -1836, -1836,     4, -1836,
    -118,   595,   322,  -353,  4711,  2466,  -479,   382,    19,   870,
     758,  3473, -1836, -1836,  2134, -1836,   194,  5478, -1836,  2068,
   -1836,   118, -1836, -1836,  3020,   329,  5802,  3471,   -45,  1841,
    -229, -1836, -1836, -1836, -1836, -1836,  -514,  4121,   376, -1836,
    -222,    53, -1836,  -704,   282, -1836,   231,   697, -1836,   -98,
    -265, -1836, -1836, -1836, -1836,  -154,  6482, -1084,   821,   453,
    2070, -1836,  -743,  -746,   488,  2733,  1441,  -620,  -166,   852,
    1817,  -420,  -325,  -299,  -622,  1212, -1836,  1553,   374, -1103,
    1420, -1836, -1836,   640, -1836, -1364,  -181,   -29,  -674, -1836,
     208, -1836, -1836, -1055, -1062, -1836, -1836, -1836,  2233,  -967,
    -610, -1145,   -56, -1836, -1836, -1836, -1836, -1836, -1836,  -179,
    -980,  -264, -1835,   324,  7920,   -74,  7691,  -139,  1402, -1836,
    2622,  -100,  -296,  -292,  -288,    24,   -80,   -55,   -54,   -33,
     -64,   -40,   -31,  -276,   -85,  -273,  -231,  -221,   186,  -217,
    -200,  -185,  -568,  -570,  -550,  -548,  -557,   -69,  -524, -1836,
   -1836,  -719,  1399,  1403,  1406,  2705, -1836,   729,  7086, -1836,
    -590,  -527,  -518,  -510,  -582, -1836, -1492, -1758, -1716, -1690,
    -591,  -143,  -212, -1836, -1836,   505,   479,  -132, -1836,  9457,
    2969,   873,  -494
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   310,   242,   243,   185,    92,  1346,   244,   245,
     246,  1420,  1421,   247,  1204,  1205,  1206,  1440,   248,   479,
     250,   251,   535,   536,   252,   253,   254,   255,   256,   257,
     258,   259,   260,   261,   262,   263,  1012,   919,   920,   921,
     537,  1563,   583,   314,   585,   265,   897,  1347,  1348,  1349,
    1350,  1351,  1352,  1353,  2123,  1354,  1355,  1701,  1979,  1980,
    1917,  1918,  1919,  2095,  2096,  1356,  1720,  1721,  2003,  1722,
    1847,  1848,  1357,  1358,  1359,  1360,  1361,  1362,  1876,  1880,
    1586,  1578,  1363,  1364,  1585,  1579,  1365,  1366,  1367,  1368,
    1369,  1370,  1371,  1739,  2018,  1740,  1741,  1949,  1372,  1373,
    1374,  1566,  2028,  2029,  2030,  2147,  2157,  2048,  2049,   402,
     403,  1093,  1094,  1315,    94,    95,    96,    97,    98,  1704,
     266,   299,   102,   103,   104,   105,   330,   331,   405,   384,
     268,   487,   269,   108,   416,   110,   111,   165,   271,   272,
     115,   116,   117,   181,   118,  1118,   273,   166,   121,   354,
     122,   167,   363,   275,   451,   277,   168,   482,   127,   128,
     280,   129,   770,  1086,  1084,  1085,  1677,   281,   282,   132,
     133,  1309,  1530,  1684,  1685,  1808,  1809,  1531,  1672,  1828,
    1686,   134,   824,  1395,   177,  1127,   283,  1128,  1129,  1607,
     956,   776,  1183,   284,   285,   777,   287,   288,   289,   779,
     488,   489,   315,   679,   680,   681,   682,   683,   439,  1393,
     440,  1116,  1114,   809,   441,   466,   442,   443,   490,   136,
     187,   188,   137,  1109,  1110,  1111,  1112,     2,  1296,  1297,
     800,  1381,   138,   429,   430,   365,   376,   753,   139,   318,
     140,   419,  1014,   743,   713,   179,   141,   473,   474,   475,
     142,   421,   334,   335,   336,   422,   144,   145,   146,   147,
     148,   149,   150,   339,   423,   341,   342,   343,   424,   345,
     346,   347,   626,   627,   628,   629,   630,   348,   632,   633,
     634,   840,   841,   842,   843,   714,  1059,  1287,   151,  1617,
     636,   637,   638,   639,   640,   641,  1811,  1812,  1813,  1814,
     593,   291,   292,   293,   294,   491,   305,   153,   154,   155,
     296,  1018,   642
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      90,   734,   206,    90,    99,   106,   333,   467,   204,   191,
     453,   540,  1024,  1394,  1210,   685,   404,   157,   209,   411,
     113,   340,  1300,   928,   462,   143,  1582,   207,   208,  1597,
    1598,   395,   696,   454,   972,  1000,   697,  1001,  1396,   213,
     698,   646,   210,   975,  1567,   750,  1403,  1386,  1899,   982,
     958,   211,   699,  1015,   992,   700,   970,   884,   448,   692,
     303,    90,    90,   493,    90,  1568,   161,   971,    99,   106,
     959,  -812,   960,   338,  2044,   720,   367,  1859,   660,  1981,
     378,    90,   664,  1982,   113,   731,  1240,    90,   481,   143,
    1900,   555,    90,   962,   867,  1190,   961,   701,  1988,  1245,
      90,   414,   963,   553,   747,    90,   594,   702,    90,   463,
     964,   703,    90,  1378,  1304,   758,  1901,   302,  2052,   123,
    1144,   696,  1914,  1915,   495,   697,  1914,  1915,   704,   698,
     337,   316,   976,   366,  1382,  1073,   979,   377,  1536,  1537,
     498,   699,   985,   705,   700,  1184,  1081,  1745,   476,   496,
     497,    90,  1512,   616,    90,  1332,    90,   319,   719,  1516,
      99,   106,    90,  1284,   499,   727,   407,  1003,  1743,    90,
     303,    64,  1007,   500,  1555,   370,   113,    90,   227,    69,
      70,   143,   156,   123,   806,  1286,   701,   746,  1246,    90,
    2043,  1101,    64,    64,   169,   119,   702,   617,   757,   631,
     703,    90,    90,    90,  1989,  1932,  -845,   404,  1184,  1916,
    1981,  1217,   791,  1944,   807,   808,   516,   704,  2026,  1746,
     646,  1757,  2094,  1247,  2053,  1247,   316,   302,   972,   707,
      84,   374,   705,  -813,    90,    81,   404,  1591,  1386,  1987,
     480,   397,  2045,  2046,   708,   587,   597,   404,   206,  1744,
    2094,   170,  1983,   547,   694,   176,    81,    81,  1538,   119,
    1184,  1899,    64,  1243,   209,   123,    90,    90,   669,   791,
    2125,   175,   783,   207,   208,   123,   100,    20,   397,   162,
     712,  1184,  1756,   190,  1258,  1567,  1759,  1136,   210,   962,
      90,  1603,   344,   623,    90,  1696,   303,   211,   963,   317,
      90,  -845,   822,  1900,   594,   333,   964,   793,  1214,   826,
     992,   676,  1068,  1070,  1602,  1903,    90,   646,   707,  1580,
     340,    90,    90,    90,   303,   668,    81,    90,    90,  1901,
     124,   894,   830,   708,  1514,  1442,  1447,   206,  1541,   755,
     100,   119,  1581,   795,  1525,   926,   396,   765,    90,  1610,
     712,   119,  1184,   302,  1387,  1380,    90,   407,    90,   397,
     646,  1853,   207,   208,  1197,   863,  1854,    90,    90,  1664,
    1448,    90,  1378,  1388,  1852,   975,  1698,   131,    90,  1235,
     131,   302,  1019,   112,   358,   958,   407,   823,   371,   454,
     192,  1060,    90,    90,   124,    90,   754,   407,  1030,  1064,
      90,  1233,  1031,  1175,    90,   959,  1032,   960,  1132,  1452,
    1973,   781,  1234,   813,   448,  1987,    90,    90,  1033,   646,
     909,  1034,   407,   886,  1412,   321,  1148,    90,   962,   785,
     646,  1218,   100,  1995,   193,    90,    90,   963,   813,  1391,
      90,   131,   302, -1127,  1878,   964,  1987,   112,  1583,  1184,
    1184,  1005,  1437,  1438,  1126,    90,   123,  1567,  2022,   459,
    1576,   669,   201,  1035,    90,   547,  1066,    90,  1301,   131,
      90,  1584,  1071,  1036,   886,   587,   124,  1037,  1568,  1879,
     152,   587,   201,   152,  2006,   123,   124,  1389,  1882,   131,
    -623,  1002,  1080,    90,  1038,   202,   123,   311,   388,  1030,
    1239,   454,   594,  1031,   308,  1577,  1390,  1032,   312,  1039,
    1184,  1939,  1940,  1488,    90,   709,   309,  1768,   668,  1033,
    1966,   123,  1034,   131,   313,   374,   448,   131,  1903,    64,
    1235,  1632,   119,   131,  2089,   553,   131,  1580,   990,   112,
    1526,  1062,    90,  1125,   152,   646,   886,    -3,  1386,  1332,
     322,  1527,  1525,  1525,  1525,   631,  2072,    90,  1429,    90,
    1581,   119,    90,   886,  1035,  1850,    90,  1533,  1075,    90,
    1858,   588,   119,  2074,  1036,  1079,   323,   131,  1037,  1083,
    1633,  1635,  1637,  1747,  1998,  1999,  1534,   759,  1973,   324,
     886,   786,   152,    81,   933,  1038,   107,   119,   304,   163,
    2100,  1825,   771,   894,   709,   547,  1050,   765,  1826,   131,
    1039,   131,   658,   721,   495,   322,   662,   712,   396,    90,
     501,  1051,  1479,    90,   404,    64,  2034,  1827,   213,   325,
     498,   606,   607,   954,  1679,   966,   152,  1284,   349,   496,
     497,  1463,  1466,   886,   369,  1026,    90,   751,   618,  1285,
     323,    90,    90,  2102,   499,   711,  1691,   716,   392,  1286,
     107,  1865,  1884,   500,   724,   304,  1854,   124,  1533,  1680,
    1124,  -493,    90,   669,   676,  1692,   646,   608,   609,  1006,
     171,  1187,  1885,   172,   173,  1008,   174,  1762,   300,    81,
     480,  1816,  -996,    90,    80,  -690,   124,  1050,  1931,  -996,
    1691,  -677,  -690,    64,  1027,    90,  1126,   124,  -677,   798,
    1817,   459,  1051,   712,   131,   495,   838,   394,  1251,  1819,
     712,  1011,   454,  1091,  1786,   397,  1787,    86,    87,    90,
     668,   131,   124,   131,    64,    90,   790,   792,    90,   131,
     496,   497,   107,   131,  1100,   553,   409,   448,  1526,  1526,
    1526,   396,   107,    64,   131,   300,   454,   547,   594,  1527,
    1527,  1527,   397,   886,    64,    64,  1826,    81,   415,   588,
      64,  1904,    64,  1826,   407,  1997,   886,   131,  1535,   131,
      64,   448,  1860,   131,   501,  1898,   712,   854,  2012,  1281,
    1905,   712,  1908,  1138,    90,  1993,    90,  -833,    81,    90,
     201,    90,   131,    99,   106,  1098,  1624,  1375,  -997,  1179,
      90,  1000,  1001,   201,   437,  -997,  1180,    81,   973,   113,
     -23,   668,   621,   465,   143,   599,   199,  1618,    81,    81,
     551,    90,   600,   601,    81,    91,    81,   980,   159,   827,
     557,   621,   829,  1384,    81,   558,    64,  2063,  1149,  1269,
     559,   468,   712,   712,  1273,   560,  1277,   227,   712,   810,
     712,   131,   741,   811,  1457,   892,  1105,   502,   712,  1168,
     503,   114,   380,   123,  1468,  1173,  1955,   381,   504,   646,
     385,  1956,   390,  1731,   131,   505,  1181,   131,   506,   131,
     131,  2084,  1052,   131,   602,   603,  2085,   587,  1232,    91,
     631,  1303,   131,   507,  2138,   131,   131,   131,   171,  2139,
      81,   172,   173,   879,   174,    90,    91,    90,   123,  1481,
    1195,  1196,   297,    90,   880,   881,   510,    91,   131,   676,
    1461,   741,  1728,   107,   621,   114,   350,   351,   831,   352,
      91,   513,   832,    91,   836,   353,  -494,    91,   837,   119,
     736,   845,   740,  1486,    90,   846,    14,    15,    16,    17,
      18,   514,   107,   519,  -495,  1805,    90,   991,    90,  1397,
    1818,   556,  1596,   107,    14,    15,    16,    17,    18,   361,
     396,   524,   501,  1052,   712,  1004,   934,   935,   936,  1695,
     304,    91,    80,    64,   119,    90,   163,    91,   107,  1215,
     847,    64,  1820,   598,   832,   561,  1002,    90,    90,   721,
      64,  1043,   460,   712,   816,   612,   676,   613,   621,    90,
     878,   740,    64,    64,   858,    86,   817,   114,   712,   454,
     380,   381,  1540,   650,   614,   390,    64,    91,    91,  1256,
      64,   885,  1417,   131,   615,   886,   643,    14,    15,    16,
      17,    18,    90,   618,   448,  1020,  1021,    81,   652,  1022,
    1090,  -496,   131,  1131,  1091,    81,  1375,   811,  1011,   550,
     212,    70,  1076,   619,    81,   100,  1002,  1464,  1133,  1106,
     300,   621,   811,   665,   124,  1491,    81,    81,   671,   712,
      90,    90,   676,   686,  1495,   684,    99,   106,   712,   798,
      81,   501,    91,   712,    81,    64,    90,  1499,   131,   131,
    1455,   712,  1377,    64,    14,    15,    16,    17,    18,   380,
    1611,  1471,   540,  1076,   712,  1594,  1107,   689,    90,   124,
    1134,   131,   878,   690,   832,    91,  1480,   892,   710,  1105,
     361,  1483,  1484,   999,  1153,  1255,    91,   908,   712,   846,
    1889,  1490,  1802,  1803,  1804,   396,   691,    91,   693,   712,
     721,    90,    91,    91,   712,  1423,  1424,  1425,   973,    81,
     501,  1105,   621,  1426,  1427,    80,   131,    81,   319,   676,
      64,  1660,   112,    91,  1841,  1842,  1843,  1844,   745,  2037,
    1292,    91,    80,   712,   886,  1294,   732,   816,    80,   886,
      90,   621,   604,   605,   131,   733,    91,  1845,    86,   817,
     316,   123,   467,   467,   816,  1186,    69,    70,   621,   756,
    1806,   780,   131,   991,   712,    86,   817,   768,    90,   798,
     773,    86,    87,   712,   784,   159,  1595,   802,   623,    91,
     846,  1599,   752,   801,    81,  1571,  1734,  1735,  1736,  1737,
    1738,    91,    91,   131,   820,   562,  1702,   563,   564,   565,
    1702,  1723,   551,  1454,   654,   655,  1002,    84,    14,    15,
      16,    17,    18,  1590,  1723,   825,   361,  1621,  1837,   152,
    1862,  1622,   886,   152,   886,  1863,   459,   119,   566,   846,
     821,   567,   568,   480,   480,  1227,   569,   570,  1864,   833,
    1671,   549,   886,    14,    15,    16,    17,    18,  1909,  1960,
     157,   828,   846,   886,   516,  1841,  1842,  1843,  1844,   361,
     676,    99,   106,    90,    90,    90,   610,   611,    91,  1990,
    1629,  1630,   676,   886,    64,  2088,  2154,  1377,  1845,   886,
    2151,  2160,  2113,   367,   378,  2161,  2117,  1846,   834,   868,
     107,   676,  1106,   878,  1703,    99,   106,    90,  1703,  1517,
    1518,  1519,  1105,  1647,   848,  1649,   656,   657,   100,    64,
     849,  1377,    90,   764,    70,    90,    90,   550,    90,  1185,
     850,  1652,    90,   851,  1106,   696,    90,   878,    90,   697,
     814,   392,   912,   698,   914,   107,   852,   917,    81,  1107,
     366,   377,   888,   889,   929,   699,   618,   898,   700,   656,
    1172,  1705,   853,  2032,   887,  1705,   361,   941,   942,   943,
     944,   890,   124,  1829,  1829,  1829,   906,   676,  1688,   967,
     524,  1107,   910,    81,  1188,  1189,   123,  1211,  1212,   370,
      90,   907,  1185,  1689,   930,    90,    90,    90,   886,  1216,
     701,  1241,  1242,   408,    91,  -165,  -165,   623,    91,   -18,
     702,  1248,   898,  1017,   703,  1016,  1105,  1288,  1289,   131,
     123,  1028,   551,  1435,  1189,   112,  1445,  1446,  1451,  1446,
    1025,   704,  1478,  1446,  1482,  1446,   997,   998,  1040,   131,
    1576,  1577,  1639,  1189,  1185,   374,   705,  1661,   886,  1785,
    1446,  1773,  1041,  1841,  1842,  1843,  1844,    91,  1042,    91,
     380,  1044,   119,  1894,  1446,  1185,   886,  1045,   752,    14,
      15,    16,    17,    18,  1046,  1030,  1845,    90,    91,  1031,
    1047,    90,    90,  1032,  1048,  1851,  1895,  1446,  1791,  1792,
      91,  1049,   131,  1958,  1959,  1033,   119,  1054,  1034,  1914,
    1915,  2151,  2152,   676,  1077,   755,   152,  1443,  1444,   945,
     946,  1707,   937,   938,    91,  1707,  1707,    19,  1723,  1688,
      91,  1078,   152,   550,  1688,  1106,  -621,   676,   676,  1707,
     939,   940,   161,   707,  1689,    64,  1185,    90,   152,  1689,
    1035,  1830,  1831,   100,  1758,  1760,  -619,  1087,   708,  1088,
    1036,  1089,  1092,    90,  1037,  1506,  1095,  1821,  1096,  1113,
    1097,  1103,   754,    54,    55,    56,    57,    58,    59,    60,
      61,  1038,  1107,  1117,  1119,  1122,  1130,   100,   898,    91,
    1160,    91,  1137,    90,    91,    90,  1039,  -123,  -123,  -123,
    -123,  -123,  -123,  1158,   361,  1159,  1161,   124,  1162,    81,
    -497,   358,   371,   480,  1163,  1164,  1165,   752,  1167,  1169,
     123,  1166,  1170,  1171,   123,   123,  1177,  1178,    90,  1208,
     114,  1191,  1105,    90,  1841,  1842,  1843,  1844,   123,  1106,
      90,   124,    90,  1185,  1185,   131,  2021,   886,   107,  1192,
      90,  1193,  1198,  1213,   131,  1199,  1238,  1845,  1209,  2050,
     112,  1219,  1872,   467,  1220,  1221,  -194,  1222,  1223,   676,
    1224,  1225,  1226,  1253,  1690,  1282,   676,  1257,  1249,  1260,
     131,  1261,   646,   696,  1050,  1262,  1107,   697,   131,  2050,
    1263,   698,  1264,  1265,   112,  1266,   119,  1267,  1268,  1051,
     119,   119,  1291,   699,  1185,  1293,   700,   676,  1875,  1295,
      91,  -816,  1207,   131,   119,  1379,  1306,  1307,    91,  1308,
    1383,  1392,  1398,  2097,    91,  1400,  1405,  1406,   131,   370,
    1404,   676,  1407,  1408,  1409,   752,   158,  1410,  1072,   152,
      71,    72,    73,    74,    75,    76,    77,    78,   701,    91,
    1411,  1415,  1416,  1428,  1430,   162,  1436,   152,   702,  1431,
    1432,    91,   703,    91,  1439,  1449,  1456,  1248,  1476,  1978,
    1477,  1688,  1485,  -817,  1532,  1542,  1543,   100,  1546,   704,
      90,   100,   100,    90,  1547,   374,  1689,  1954,  1556,  1557,
      91,   152,   676,   676,   705,   100,  1558,   778,  1560,   676,
    1565,   -22,  1569,    91,  -122,  -122,  -122,  -122,  -122,  -122,
     886,    91,   676,  1601,    91,  1690,   152,  1570,  1605,  1615,
    1690,  1606,   676,   751,   676,  1616,  1619,  1620,  1623,   709,
    1631,   124,  1640,  1707,  1641,   124,   124,   676,   501,   676,
     676,   676,    -3,   152,  1648,  1106,  1650,    91,  1250,   124,
    1651,  1653,  1657,   131,   131,   131,   131,   131,   131,   131,
    1658,  1659,  1676,  1663,  1528,  1665,  1678,  1535,  1709,  1577,
      90,  1724,  2027,   107,  1749,  1332,  1725,   676,   131,  1627,
     707,   676,   131,   131,   112,  1313,   676,  1752,   112,   112,
    1985,  1753,  1107,   131,  1727,   708,   131,  1729,  2092,   206,
    1978,    91,   112,  1742,  1750,   795,  1751,   107,  1763,    90,
    1764,    90,  1766,  1767,  2031,  1774,  1782,  1777,  2015,  1242,
    1783,   480,  1784,   114,   207,   208,    14,    15,    16,    17,
      18,   883,   123,  1788,  1789,   676,  1790,  1795,  2115,  1797,
    1050,   358,   371,  1793,  1794,  1798,  1815,   676,  2124,  1799,
     398,   676,  1801,  1682,    93,  1051,  1418,   160,   131,   152,
      90,  1401,  1833,  2133,  1707,  1834,   676,   317,  1838,  1840,
    1052,  1869,  1871,  1887,    91,  1890,  1891,  1922,    90,    90,
    1896,   152,  1532,  1532,  1532,   152,   152,  1673,  1532,  1897,
    1927,  1928,  1943,  1957,  1707,    91,  1948,  2027,  1962,   152,
    1968,  2027,  2027,  2153,   302,  1941,  1953,  1970,   119,  1971,
     541,   676,   131,  1972,   712,  1976,  1992,  2000,    93,  1994,
    2005,   778,  2007,    91,  2078,  2013,  2024,  2017,  1707,  2035,
    2136,  2025,  2036,  2047,  2071,   203,  2056,  2073,   249,    90,
     539,  2075,  2079,   131,  2081,  2082,    93,  1453,   676,   152,
     480,  2146,   480,   676,  2083,  2146,  2086,  1690,  2087,   329,
    2099,  2101,   357,   123,   668,  2103,    93,  2155,  2111,  2120,
    2114,   751,  1528,  1528,  1528,   163,  1669,  1670,  1674,   676,
    2116,  2121,   676,  2126,   676,  2135,  2137,  2143,  2145,   100,
     480,  2148,  2149,   123,  2158,  2159,  1779,   107,  2162,   882,
     949,   107,   107,   676,   160,   947,   951,   948,  1450,  1564,
      93,    90,   950,   160,   584,   107,   418,   426,  1572,  2144,
      90,  2093,   778,  1711,  1945,   688,  2110,   123,  1733,   447,
    2090,  1938,  1881,  2077,    14,    15,    16,    17,    18,   119,
    2016,  1867,  1868,   124,  2118,  2076,   152,  2150,   114,   182,
     387,  1950,  1589,  1975,   485,   480,   203,   203,    14,    15,
      16,    17,    18,  1174,   744,  2041,   709,   773,   158,   119,
      91,    91,    71,    72,    73,    74,    75,    76,    77,    78,
    1675,  1604,   114,    91,  1888,  1587,   485,   249,   160,  1252,
     131,  1115,  1765,     3,  1135,  1023,   112,  1140,  1662,   778,
      64,  1141,   249,   119,  1142,     0,   361,     0,     0,   782,
       0,     0,     0,     0,     0,     0,     0,   778,     0,     0,
     100,   624,    91,   645,     0,     0,  1052,     0,     0,  1600,
    1950,     0,     0,     0,     0,    91,  1608,     0,     0,   778,
      91,    91,    91,   447,     0,     0,     0,   447,     0,     0,
     100,   249,     0,     0,   357,   778,     0,     0,     0,   778,
       0,     0,  1625,     0,    81,  -498,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,   418,   677,     0,     0,
       0,   329,   329,     0,   100,   861,     0,     0,   864,     0,
     866,     0,     0,   152,   869,     0,     0,     0,     0,     0,
       0,     0,   418,   872,   124,     0,   874,   875,   876,   447,
      93,     0,     0,     0,     0,   539,     0,     0,   539,     0,
       0,   131,    91,     0,   539,   357,     0,   112,     0,     0,
       0,     0,     0,   539,  1952,     0,     0,     0,   124,     0,
       0,    14,    15,    16,    17,    18,     0,     0,    91,     0,
       0,   131,     0,     0,     0,     0,   778,   112,   418,     0,
       0,   539,     0,   426,     0,     0,     0,     0,     0,   426,
     418,   418,   114,     0,   778,     0,   114,   114,   447,   160,
       0,   778,    91,     0,     0,   131,     0,     0,     0,     0,
     114,   112,     0,     0,    14,    15,    16,    17,    18,   249,
     447,   818,   645,     0,     0,     0,     0,    64,     0,   107,
     469,     0,     0,  1952,     0,     0,     0,     0,   752,     0,
       0,     0,     0,     0,   152,     0,   775,     0,    91,     0,
      91,     0,     0,     0,     0,  1769,     0,   249,   485,     0,
     839,     0,     0,     0,     0,     0,     0,   203,     0,     0,
       0,     0,     0,     0,   152,     0,     0,  1781,     0,     0,
      64,   485,     0,    91,   485,     0,   160,   160,    91,    80,
     485,    81,     0,     0,     0,    91,     0,    91,     0,   485,
       0,     0,   160,   160,   160,   249,     0,     0,   152,     0,
    1312,   838,   470,     0,   198,   712,   447,     0,     0,   901,
       0,     0,    86,    87,     0,   160,     0,     0,     0,     0,
     158,     0,   183,   184,    71,    72,    73,    74,    75,    76,
      77,    78,    80,     0,    81,     0,  1061,     0,    19,     0,
       0,     0,     0,     0,  1065,   360,   361,     0,     0,   249,
     107,     0,   645,     0,  1806,     0,     0,   382,   712,   389,
     778,   391,     0,  1074,   778,    86,    87,     0,     0,     0,
     471,  1861,     0,     0,  1082,     0,    91,     0,     0,     0,
     107,     0,     0,   624,     0,     0,     0,   624,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,   389,   391,     0,     0,   447,   447,     0,
       0,  1886,     0,     0,   107,     0,     0,     0,   447,     0,
     447,     0,   645,     0,     0,    91,   447,     0,    91,     0,
     160,     0,   349,   158,     0,   183,   184,    71,    72,    73,
      74,    75,    76,    77,    78,   923,     0,     0,  1010,   485,
     677,     0,     0,     0,     0,     0,   158,   329,     0,   523,
      71,    72,    73,    74,    75,    76,    77,    78,   915,   329,
       0,     0,     0,     0,   205,     0,   752,     0,     0,     0,
     778,     0,     0,     0,   778,     0,     0,   924,   778,     0,
       0,     0,     0,   418,   114,   485,   485,     0,   332,   418,
       0,   249,     0,     0,     0,     0,   360,     0,     0,     0,
     916,     0,   651,     0,   391,    91,   194,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,  1967,     0,
       0,     0,  1969,     0,     0,     0,     0,   360,     0,     0,
       0,     0,   158,     0,   326,   327,    71,    72,    73,    74,
      75,    76,    77,    78,    91,   420,  2023,     0,   418,     0,
     418,  1063,     0,    93,     0,     0,     0,   160,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,   447,   818,
       0,   818,     0,     0,   379,     0,     0,     0,   286,     0,
       0,   160,  1681,    83,     0,     0,     0,     0,     0,  1682,
       0,   839,   839,    86,    87,  2064,     0,     0,   360,   485,
     389,   391,     0,     0,     0,    88,     0,     0,     0,     0,
       0,     0,     0,    91,    91,   114,   158,     0,   249,     0,
      71,    72,    73,    74,    75,    76,    77,    78,   775,     0,
     160,     0,   360,     0,     0,     0,   360,     0,     0,     0,
       0,     0,   360,     0,   447,   114,   447,     0,     0,     0,
     625,     0,   249,     0,   249,     0,     0,     0,   778,     0,
     469,     0,   778,     0,     0,   778,     0,    83,  1139,  1290,
    1009,     0,     0,     0,    91,   360,     0,   651,   391,   114,
       0,  1203,  1298,     0,  1156,     0,  1302,     0,  1157,  1203,
    1305,     0,   778,     0,    64,   677,   778,     0,     0,     0,
     778,     0,     0,     0,     0,   420,     0,   447,   624,     0,
     695,   332,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,   624,     0,     0,     0,     0,   286,   158,     0,
       0,   420,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,   470,  1422,     0,     0,  2156,     0,     0,     0,
       0,     0,     0,     0,     0,  2163,    80,     0,    81,   447,
     158,  1203,   183,   184,    71,    72,    73,    74,    75,    76,
      77,    78,   360,     0,  1203,     0,     0,     0,    82,    83,
     447,     0,   677,     0,   431,  1010,     0,   420,     0,    86,
      87,   286,   432,   433,   434,   435,     0,     0,     0,   796,
     420,    88,     0,  1236,   723,     0,   418,     0,     0,     0,
    1237,     0,   778,     0,     0,   360,     0,   651,   391,     0,
       0,     0,   307,     0,     0,     0,     0,     0,     0,     0,
     160,     0,     0,   158,     0,   183,   184,    71,    72,    73,
      74,    75,    76,    77,    78,     0,   485,     0,     0,   286,
       0,   447,     0,     0,     0,     0,     0,     0,     0,     0,
     418,   426,   160,     0,     0,   276,   485,     0,     0,     0,
       0,     0,   158,   818,   360,   651,    71,    72,    73,    74,
      75,    76,    77,    78,  1200,   360,     0,   436,  1201,   307,
    1202,     0,     0,     0,     0,     0,   839,     0,   523,     0,
       0,     0,     0,     0,     0,   437,     0,     0,     0,     0,
    1515,     0,     0,     0,     0,     0,     0,     0,   286,   160,
       0,   541,     0,    83,  1539,   447,  1441,     0,     0,     0,
     492,     0,   360,    14,    15,    16,    17,    18,     0,   286,
       0,     0,     0,     0,  1561,   677,     0,     0,   447,   447,
       0,   539,   844,     0,     0,     0,   360,     0,     0,     0,
       0,   360,     0,   360,     0,     0,  1203,     0,   856,     0,
       0,   859,     0,     0,     0,     0,     0,   286,   596,  1413,
       0,     0,     0,  1414,     0,     0,   360,     0,   360,   360,
       0,     0,     0,     0,     0,   447,     0,     0,     0,    64,
     360,     0,   957,     0,     0,     0,   625,     0,     0,   447,
       0,     0,     0,   360,   276,     0,     0,     0,     0,     0,
       0,     0,     0,   360,     0,     0,     0,     0,     0,   591,
       0,     0,     0,   158,     0,   326,   327,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,     0,     0,     0,
       0,   649,     0,   160,     0,     0,     0,     0,     0,     0,
       0,    80,   160,    81,     0,     0,     0,     0,     0,     0,
     591,   485,     0,     0,   591,     0,  1029,     0,   276,   286,
       0,     0,     0,   620,    83,     0,   742,   621,   332,  1573,
       0,     0,     0,     0,    86,   622,     0,   485,   249,  1503,
       0,     0,     0,  1504,     0,   485,    88,  1505,     0,     0,
       0,     0,   420,     0,     0,     0,     0,   158,   420,   183,
     184,    71,    72,    73,    74,    75,    76,    77,    78,   357,
      93,   360,     0,     0,     0,     0,   276,     0,     0,     0,
       0,     0,   418,  1697,  1699,   160,     0,     0,   286,     0,
     286,     0,     0,     0,     0,   742,     0,   160,     0,     0,
       0,     0,     0,     0,   723,     0,     0,     0,     0,     0,
     818,     0,     0,     0,     0,     0,   596,  1099,     0,   420,
       0,     0,     0,     0,   447,   447,     0,     0,     0,     0,
       0,     0,  1574,   677,  1761,   360,     0,     0,     0,  1422,
       0,     0,     0,     0,     0,   276,  1203,     0,     0,     0,
       0,  1203,  1203,  1203,   307,     0,     0,     0,     0,     0,
       0,   360,     0,     0,     0,     0,   276,   591,     0,     0,
       0,   286,   126,     0,     0,   126,     0,   862,   778,     0,
       0,     0,     0,   492,     0,   158,   870,   183,   184,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,     0,
       0,     0,     0,     0,   276,     0,     0,     0,     0,     0,
     160,   160,   160,   160,     0,   160,   160,     0,     0,     0,
       0,  1683,   426,     0,     0,     0,     0,  1642,     0,     0,
       0,  1643,     0,     0,  1644,   485,   126,     0,     0,   485,
     485,   200,     0,   844,   844,     0,     0,     0,     0,     0,
     485,     0,   276,   485,  1151,     0,   279,  1154,     0,   677,
    1548,  1654,     0,   591,   126,  1655,     0,   649,     0,  1656,
       0,     0,     0,     0,   360,     0,     0,   957,     0,     0,
     364,   357,   362,     0,   126,  1866,   360,     0,     0,  1231,
       0,   625,     0,     0,   383,   386,     0,     0,     0,     0,
       0,     0,     0,   160,     0,     0,   276,     0,     0,   360,
       0,     0,     0,     0,   993,   160,     0,     0,   126,     0,
       0,     0,   126,     0,   286,     0,     0,     0,   126,     0,
       0,   126,     0,     0,     0,   364,     0,   362,     0,  1203,
       0,  1203,     0,     0,     0,     0,   444,   126,     0,   461,
       0,     0,     0,     0,     0,   492,     0,     0,     0,     0,
       0,     0,  1683,  1807,   276,   591,     0,  1683,     0,   485,
       0,     0,   279,     0,  1683,   420,  1683,     0,     0,     0,
       0,  1776,     0,   591,     0,     0,     0,   286,     0,     0,
       0,     0,     0,     0,   360,     0,   525,     0,     0,   426,
     160,   492,   492,     0,   279,   279,   126,     0,   158,     0,
     183,   184,    71,    72,    73,    74,    75,    76,    77,    78,
     279,     0,     0,     0,     0,     0,     0,     0,     0,   420,
       0,     0,     0,     0,     0,     0,     0,     0,   723,   286,
       0,   364,   126,   362,     0,  1271,     0,     0,    64,  1275,
       0,     0,     0,  1279,     0,     0,     0,     0,   276,     0,
     286,   126,     0,     0,     0,   126,     0,     0,     0,   279,
       0,     0,   364,     0,   362,     0,     0,   677,     0,     0,
       0,     0,   158,  1550,   326,   327,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,   492,     0,  1807,
    1807,     0,     0,     0,     0,     0,     0,     0,     0,   126,
       0,     0,    81,     0,  1683,   492,     0,  1683,     0,     0,
       0,     0,  1311,     0,   591,     0,   126,   126,   126,   426,
       0,   286,  1229,    83,     0,   591,     0,     0,   126,     0,
       0,     0,   360,   364,     0,   362,     0,   485,   844,   126,
       0,  1473,     0,     0,     0,    88,     0,     0,     0,     0,
       0,     0,   160,     0,   769,     0,     0,   126,     0,     0,
     993,     0,   126,     0,   126,     0,     0,   364,   126,   362,
       0,   364,     0,   362,     0,   276,     0,   364,     0,   362,
       0,     0,     0,     0,     0,  1807,   126,   126,     0,     0,
       0,   158,     0,     0,  1683,    71,    72,    73,    74,    75,
      76,    77,    78,   591,     0,     0,     0,   279,   126,   276,
     364,   591,   362,     0,     0,  1459,  2112,     0,   286,   286,
     194,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,   160,     0,  2020,   426,     0,     0,     0,     0,     0,
       0,  1229,    83,     0,     0,   279,   279,     0,  2134,     0,
       0,     0,     0,     0,     0,  1493,     0,  1807,  1497,     0,
       0,     0,  1501,     0,    88,   286,     0,     0,   160,   279,
       0,     0,   279,     0,   126,   126,   461,     0,   279,   286,
       0,    64,     0,     0,     0,   360,     0,   279,     0,     0,
     126,   126,   126,   279,     0,     0,     0,     0,   160,     0,
       0,   420,  2020,  2020,   126,     0,     0,   903,   279,   362,
       0,     0,     0,   126,     0,   158,     0,   326,   327,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,     0,
       0,   158,   160,   457,   458,    71,    72,    73,    74,    75,
      76,    77,    78,    80,   360,    81,     0,   279,     0,     0,
     364,   158,   362,   183,   184,    71,    72,    73,    74,    75,
      76,    77,    78,  2020,     0,  2019,    83,     0,   286,   712,
       0,     0,   492,     0,     0,     0,    86,    87,     0,     0,
       0,   126,     0,     0,    84,     0,     0,   591,    88,     0,
       0,     0,     0,     0,     0,     0,   459,  2091,     0,     0,
       0,   619,     0,     0,  1613,   126,   126,     0,     0,   364,
       0,   362,   130,     0,   769,   130,   126,     0,   126,   591,
     364,     0,   362,     0,   126,     0,     0,     0,   126,     0,
       0,     0,     0,     0,     0,   525,     0,     0,     0,     0,
    1687,     0,     0,     0,     0,     0,     0,   279,     0,     0,
       0,     0,     0,     0,   286,   286,     0,     0,     0,     0,
       0,   158,   360,   326,   327,    71,    72,    73,    74,    75,
      76,    77,    78,  1058,     0,  1057,   130,     0,     0,     0,
       0,   158,   126,   326,   327,    71,    72,    73,    74,    75,
      76,    77,    78,   279,   279,     0,     0,     0,   364,   279,
     362,     0,     0,     0,   130,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,   126,     0,     0,     0,
       0,     0,     0,   362,   130,     0,   459,     0,     0,     0,
       0,  1681,    83,     0,     0,   364,     0,   362,  1682,     0,
       0,     0,    86,    87,     0,     0,     0,     0,   364,     0,
     362,     0,     0,     0,    88,     0,     0,     0,   130,   126,
       0,   126,   130,     0,     0,   126,     0,     0,   130,     0,
       0,   130,     0,     0,     0,     0,   126,   126,     0,     0,
     360,  1687,     0,     0,     0,     0,  1687,     0,     0,   126,
       0,     0,   360,  1822,     0,  1687,     0,     0,     0,   126,
       0,     0,     0,     0,     0,     0,     0,   279,     0,     0,
     591,     0,   130,     0,     0,   126,     0,     0,     0,   126,
      14,    15,    16,    17,    18,   158,   279,   183,   184,    71,
      72,    73,    74,    75,    76,    77,    78,     0,   126,     0,
       0,     0,     0,     0,   130,     0,   130,     0,     0,     0,
       0,   492,   126,     0,   126,   276,     0,     0,   360,     0,
     279,     0,   279,     0,     0,  1810,   903,     0,   362,     0,
       0,   158,     0,   183,   184,    71,    72,    73,    74,    75,
      76,    77,    78,   804,     0,     0,    64,     0,     0,   360,
     391,   222,     0,   223,   224,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,   360,     0,     0,
       0,     0,     0,     0,   591,   126,   126,     0,     0,     0,
     158,     0,   326,   327,    71,    72,    73,    74,    75,    76,
      77,    78,  1552,  1910,   126,     0,  1687,     0,     0,     0,
       0,   126,     0,   554,     0,     0,    84,   477,    80,   130,
      81,     0,   158,     0,   212,    70,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,   130,   126,   130,     0,
     328,    83,     0,     0,   130,     0,     0,     0,   130,     0,
       0,    86,    87,     0,     0,     0,     0,     0,   126,   130,
       0,     0,     0,    88,     0,     0,     0,     0,     0,     0,
       0,  1810,  1810,    83,     0,     0,  1009,     0,     0,     0,
       0,     0,   130,     0,   130,     0,   492,     0,   130,     0,
       0,     0,   158,  1687,   764,    70,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,   360,   130,   126,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     126,     0,     0,     0,   279,     0,     0,     0,     0,   126,
       0,     0,   420,     0,     0,     0,     0,     0,     0,   364,
     126,   362,     0,     0,   279,     0,   996,     0,    14,    15,
      16,    17,    18,     0,     0,     0,  1399,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   130,  1810,   158,     0,
     326,   327,    71,    72,    73,    74,    75,    76,    77,    78,
     126,     0,     0,     0,   126,     0,     0,     0,     0,   130,
     591,   126,   130,     0,   130,   130,    80,   126,   130,     0,
       0,   796,   420,   126,     0,   492,     0,   130,     0,     0,
     130,   130,   130,     0,    64,  1810,     0,     0,   620,    83,
       0,     0,   621,     0,     0,     0,   126,   126,     0,    86,
     622,     0,     0,   130,     0,  2039,     0,     0,     0,  1810,
       0,    88,   623,     0,     0,     0,     0,     0,   158,     0,
     326,   327,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,   420,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,   126,     0,   164,    80,     0,    81,     0,
       0,     0,     0,     0,  1810,  1810,     0,   126,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,  2019,    83,
     126,     0,   712,     0,   126,     0,     0,   158,   126,    86,
      87,    71,    72,    73,    74,    75,    76,    77,    78,  1200,
    1511,    88,  1510,  1201,     0,  1202,     0,     0,     0,     0,
       0,   126,     0,     0,     0,     0,   109,     0,     0,     0,
     126,     0,     0,     0,     0,  1810,     0,     0,   130,   279,
      14,    15,    16,    17,    18,     0,   270,    64,    83,     0,
       0,  1634,     0,     0,   301,     0,     0,   130,     0,     0,
       0,     0,     0,     0,     0,   279,   279,     0,     0,     0,
       0,     0,     0,   279,   372,     0,     0,     0,     0,     0,
       0,   158,     0,   326,   327,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,   364,   126,   362,
       0,     0,     0,   130,   130,     0,    64,     0,   406,    80,
       0,    81,   410,   126,     0,     0,     0,     0,   109,     0,
       0,     0,     0,     0,     0,   126,   130,     0,     0,     0,
       0,   417,    83,     0,     0,     0,     0,   449,     0,   591,
     158,     0,    86,    87,    71,    72,    73,    74,    75,    76,
      77,    78,   126,   126,    88,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   494,     0,     0,     0,    80,     0,
      81,   130,     0,     0,     0,     0,     0,     0,   126,     0,
       0,     0,   126,     0,     0,   126,     0,     0,     0,     0,
      82,    83,     0,     0,     0,   546,   410,     0,     0,   130,
       0,    86,    87,     0,     0,     0,     0,     0,     0,     0,
     589,     0,   126,    88,     0,     0,   126,   130,   591,   158,
     126,     0,     0,    71,    72,    73,    74,    75,    76,    77,
      78,  1200,   648,   186,   189,  1201,     0,  1202,   126,   126,
     126,   126,   126,   126,   126,     0,     0,     0,   130,     0,
     364,   659,   362,     0,     0,   659,     0,     0,     0,   270,
       0,     0,     0,   279,     0,     0,     0,   279,   279,     0,
      83,     0,   320,  1636,     0,     0,     0,     0,   279,     0,
       0,   279,     0,     0,     0,     0,     0,     0,     0,     0,
     706,     0,     0,     0,     0,     0,   158,     0,     0,   406,
      71,    72,    73,    74,    75,    76,    77,    78,  1200,   364,
     730,   362,  1201,     0,  1202,   735,   737,   270,   301,     0,
       0,     0,     0,   412,  1770,     0,   413,     0,   406,     0,
       0,   126,   126,     0,     0,     0,     0,     0,     0,   406,
       0,   438,   760,   126,     0,     0,   762,    83,     0,     0,
       0,   763,     0,     0,     0,     0,     0,   774,     0,     0,
       0,   472,   737,     0,   406,     0,     0,     0,   787,     0,
       0,     0,     0,   472,     0,     0,     0,     0,     0,   797,
       0,     0,     0,     0,     0,   158,   270,   183,   184,    71,
      72,    73,    74,    75,    76,    77,    78,   279,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   546,   589,     0,
      64,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     0,   364,   126,   362,
       0,     0,     0,     0,     0,   665,     0,     0,   320,     0,
       0,     0,     0,     0,   158,   270,   326,   327,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
       0,     0,   661,     0,   130,     0,   666,     0,     0,     0,
       0,     0,    80,     0,    81,     0,     0,     0,     0,     0,
       0,     0,    64,     0,   130,     0,     0,     0,   687,     0,
       0,     0,     0,   449,   328,    83,     0,     0,     0,     0,
       0,     0,     0,     0,   659,    86,    87,     0,   905,     0,
       0,     0,     0,   410,     0,     0,   158,    88,   326,   327,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
     320,     0,     0,     0,     0,     0,     0,   130,     0,   748,
     749,     0,     0,     0,    80,     0,    81,   546,     0,     0,
     186,     0,     0,     0,     0,     0,     0,   364,     0,   362,
       0,     0,     0,     0,     0,   186,  1681,    83,     0,     0,
       0,     0,   279,     0,     0,   279,     0,    86,    87,     0,
       0,   774,     0,     0,     0,   969,     0,     0,     0,    88,
     126,     0,     0,     0,     0,     0,     0,     0,     0,   799,
       0,     0,     0,     0,     0,   449,   589,   803,   805,     0,
       0,   995,   812,     0,     0,     0,   270,     0,   270,     0,
       0,     0,     0,    64,   659,     0,     0,     0,   410,     0,
       0,     0,     0,     0,     0,     0,   438,     0,     0,   438,
       0,   158,   472,   766,   767,    71,    72,    73,    74,    75,
      76,    77,    78,  2004,   364,     0,     0,   158,     0,   326,
     327,    71,    72,    73,    74,    75,    76,    77,    78,   126,
       0,     0,   364,     0,   362,     0,     0,     0,     0,     0,
     130,     0,   774,     0,     0,    80,     0,    81,     0,   130,
       0,     0,     0,     0,    84,     0,     0,     0,     0,   546,
       0,     0,     0,     0,     0,     0,   126,   417,    83,     0,
       0,     0,     0,     0,     0,   130,   406,     0,    86,    87,
       0,     0,  2065,   130,     0,     0,     0,     0,     0,   120,
      88,     0,     0,     0,     0,     0,   126,     0,   925,     0,
       0,     0,     0,     0,    64,     0,     0,     0,   130,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   126,   774,
       0,   109,     0,   130,     0,  1108,     0,     0,     0,     0,
     126,     0,     0,     0,     0,     0,   659,   774,   158,  1120,
     326,   327,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,   120,     0,     0,     0,     0,     0,   774,
    1143,     0,     0,     0,     0,     0,    80,     0,    81,     0,
       0,     0,     0,   274,     0,   774,     0,     0,   994,   774,
       0,     0,     0,     0,     0,     0,   449,     0,  1681,    83,
       0,     0,     0,     0,     0,     0,     0,     0,   410,    86,
      87,   373,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    88,   270,     0,   659,     0,     0,     0,     0,     0,
     449,   158,   589,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,     0,     0,   130,   130,
     130,   130,   130,   130,   130,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   450,     0,     0,     0,     0,     0,
       0,     0,     0,   130,    84,   270,   774,   130,   130,     0,
       0,     0,     0,     0,     0,     0,     0,  1228,   130,     0,
       0,   130,     0,     0,   774,     0,     0,     0,     0,     0,
     158,   774,   326,   327,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   274,     0,     0,     0,     0,   270,    80,     0,
       0,     0,     0,  1102,     0,     0,     0,   590,     0,     0,
       0,     0,   438,     0,     0,     0,     0,     0,   270,     0,
    2019,    83,     0,   130,   712,     0,     0,     0,     0,   373,
       0,    86,    87,   472,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    88,     0,     0,     0,     0,   590,     0,
       0,     0,   590,     0,     0,     0,   274,   571,   572,   573,
     574,   575,   576,   577,   578,   579,   580,   581,  1108,     0,
       0,     0,   311,     0,     0,     0,     0,   130,     0,     0,
     774,     0,     0,   125,  1376,     0,     0,     0,     0,   270,
       0,     0,     0,     0,     0,     0,   120,     0,     0,   582,
    1108,     0,     0,     0,     0,     0,     0,     0,   130,     0,
       0,     0,     0,     0,   274,     0,     0,     0,     0,   319,
       0,     0,     0,     0,   158,   120,   183,   184,    71,    72,
      73,    74,    75,    76,    77,    78,   120,     0,     0,   761,
     774,     0,     0,     0,   774,     0,     0,   125,     0,     0,
       0,   774,     0,     0,   450,     0,     0,     0,     0,     0,
       0,   120,     0,   449,     0,   373,     0,   278,     0,     0,
     158,     0,   326,   327,    71,    72,    73,    74,    75,    76,
      77,    78,     0,   274,     0,     0,   270,   270,     0,     0,
       0,     0,     0,     0,     0,   375,     0,     0,    80,     0,
       0,     0,     0,     0,   274,   590,     0,     0,     0,  1244,
       0,     0,     0,     0,     0,     0,     0,     0,   735,     0,
     328,    83,     0,     0,     0,     0,     0,     0,     0,   125,
       0,    86,    87,   270,     0,     0,     0,     0,     0,   125,
       0,     0,   274,    88,     0,     0,     0,   270,     0,     0,
       0,     0,     0,     0,     0,   130,     0,     0,   452,     0,
     774,     0,     0,     0,   774,     0,     0,   158,   774,   326,
     327,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     274,  1108,     0,  1314,     0,    80,     0,     0,     0,     0,
    1529,   590,     0,     0,     0,   373,     0,     0,     0,  1376,
       0,     0,     0,     0,     0,     0,   278,   417,    83,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    86,    87,
    1402,   592,     0,     0,     0,     0,   270,     0,     0,     0,
      88,     0,     0,  1376,   274,     0,     0,     0,     0,     0,
       0,     0,   158,   375,   183,   184,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,  1588,     0,
       0,     0,   592,     0,     0,     0,   592,     0,   450,     0,
     278,     0,     0,     0,     0,     0,   130,     0,     0,     0,
       0,     0,     0,     0,     0,  1108,     0,     0,     0,     0,
       0,     0,   274,   590,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   450,     0,   450,   130,     0,     0,     0,
     125,   590,   270,   270,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   278,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   774,   125,
     130,     0,   774,     0,     0,   774,     0,     0,     0,   101,
     125,     0,   101,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   452,   450,
       0,     0,   774,     0,     0,   125,   774,     0,     0,   375,
     774,     0,     0,     0,     0,     0,   274,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   278,  1529,  1529,
    1529,   164,   737,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   101,     0,  1549,  1551,  1553,   278,   592,
       0,     0,     0,  1706,     0,     0,     0,  1706,  1706,     0,
       0,     0,     0,   267,     0,     0,     0,     0,     0,     0,
       0,  1706,     0,     0,     0,     0,   450,     0,   120,  1575,
       0,     0,   590,     0,     0,     0,   278,   359,     0,     0,
       0,   101,     0,   590,   450,     0,     0,     0,     0,     0,
    1314,     0,     0,     0,  1592,     0,     0,     0,  1593,     0,
       0,     0,     0,     0,     0,     0,   450,     0,     0,     0,
       0,  1108,   774,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   450,     0,   278,   101,   450,     0,     0,     0,
       0,     0,   427,   274,     0,   592,     0,     0,     0,   375,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   450,
       0,   590,     0,     0,     0,     0,     0,   274,     0,   590,
       0,     0,     0,     0,     0,     0,     0,     0,   278,     0,
       0,     0,     0,     0,  1824,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   267,     0,     0,     0,     0,     0,  1836,     0,
       0,     0,   452,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   450,   450,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   278,   592,     0,     0,
       0,   450,     0,  1693,  1694,     0,     0,   452,   450,   452,
       0,     0,     0,     0,     0,   592,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   267,     0,     0,     0,
       0,     0,     0,   135,   450,     0,   135,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   450,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1902,     0,
       0,     0,     0,   452,     0,  1771,     0,     0,     0,     0,
       0,     0,     0,     0,   267,     0,     0,     0,     0,     0,
     278,     0,     0,     0,     0,     0,     0,   135,     0,     0,
     359,     0,     0,     0,  1545,   590,     0,   125,     0,     0,
       0,     0,  1933,     0,     0,  1706,  1559,   450,     0,     0,
       0,   120,     0,     0,     0,   135,   450,     0,     0,     0,
    1951,     0,     0,     0,     0,     0,     0,   590,     0,     0,
       0,     0,     0,     0,     0,   135,     0,     0,     0,     0,
     452,     0,   125,   267,     0,     0,   592,     0,     0,     0,
       0,     0,  1832,     0,     0,     0,     0,   592,   452,     0,
       0,     0,     0,     0,   267,     0,     0,   450,     0,   135,
       0,   450,     0,   135,     0,     0,     0,     0,   450,   135,
     452,     0,   135,     0,     0,     0,     0,     0,     0,     0,
     450,     0,     0,     0,     0,     0,   452,     0,     0,  1951,
     452,     0,   267,     0,     0,     0,     0,   278,     0,     0,
       0,     0,     0,   450,   450,     0,     0,     0,     0,     0,
       0,     0,     0,   135,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   452,     0,   592,  1706,     0,     0,     0,
       0,   278,     0,   592,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   135,     0,   135,     0,     0,
     450,     0,     0,     0,     0,     0,  1706,     0,     0,     0,
       0,  2080,     0,     0,   450,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   450,   774,     0,
       0,   450,     0,     0,     0,   450,   452,   452,     0,     0,
    1706,     0,     0,     0,   267,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   452,     0,     0,   590,     0,
       0,     0,   452,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   452,     0,
       0,  1754,  1755,     0,     0,     0,     0,     0,     0,     0,
     135,     0,     0,   274,     0,     0,     0,     0,     0,   452,
     120,     0,     0,   267,     0,   267,     0,   135,     0,   135,
       0,     0,     0,     0,     0,   135,     0,     0,     0,   135,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
     135,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   592,
       0,     0,   590,   135,     0,   135,     0,     0,     0,   135,
       0,   452,     0,     0,     0,   125,     0,     0,     0,     0,
     452,     0,     0,     0,     0,     0,     0,     0,   135,   450,
     450,   592,     0,     0,     0,     0,   267,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   450,     0,     0,     0,   450,
       0,     0,   450,  1839,     0,     0,     0,     0,     0,     0,
    1849,   452,     0,     0,     0,   452,     0,     0,     0,     0,
       0,     0,   452,     0,     0,     0,     0,   135,     0,   450,
       0,     0,     0,   450,   452,     0,     0,   450,   101,     0,
       0,  1874,   101,     0,     0,     0,     0,     0,     0,     0,
     135,     0,     0,   135,     0,   135,   135,   452,   452,   135,
       0,     0,     0,     0,     0,     0,     0,     0,   135,     0,
       0,   135,   135,   135,     0,     0,     0,     0,     0,     0,
     120,     0,     0,     0,   120,   120,     0,     0,     0,     0,
       0,     0,     0,     0,   135,     0,     0,     0,   120,     0,
       0,     0,     0,     0,   452,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   452,     0,
       0,     0,     0,     0,     0,     0,  1912,  1913,     0,   267,
       0,   452,     0,  1923,     0,   452,     0,     0,     0,   452,
       0,     0,     0,     0,     0,     0,  1937,     0,   590,   450,
       0,     0,     0,     0,     0,     0,  1946,     0,  1947,     0,
       0,     0,   592,     0,     0,     0,     0,     0,     0,     0,
       0,  1961,     0,  1963,  1964,  1965,     0,     0,     0,     0,
     125,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   267,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   278,     0,   135,
       0,  1986,     0,     0,   125,  1991,     0,     0,     0,     0,
    1996,     0,     0,     0,     0,     0,     0,     0,   135,     0,
       0,   290,     0,     0,     0,     0,     0,     0,     0,   375,
       0,     0,     0,     0,   267,   373,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   267,   592,     0,     0,  2042,
       0,     0,     0,     0,   135,   135,     0,     0,     0,     0,
       0,  2051,     0,     0,     0,  2054,     0,     0,     0,     0,
       0,     0,     0,   452,   452,     0,     0,   135,     0,     0,
    2070,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   101,     0,     0,     0,   452,
       0,     0,   290,   452,     0,     0,   452,     0,     0,     0,
       0,   101,     0,     0,     0,     0,   267,     0,     0,     0,
       0,     0,   135,     0,     0,  2098,   427,   101,     0,     0,
       0,     0,     0,   452,     0,     0,     0,   452,     0,     0,
       0,   452,     0,     0,     0,     0,     0,     0,     0,     0,
     135,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     290,   552,  2119,     0,     0,     0,     0,  2122,   135,     0,
       0,     0,   120,     0,     0,   290,     0,     0,     0,     0,
       0,     0,     0,     0,   125,     0,     0,   590,   125,   125,
       0,     0,     0,  2140,   635,     0,  2142,     0,  2122,   135,
       0,     0,   125,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   267,   267,     0,     0,  2142,     0,     0,
     663,     0,     0,     0,   290,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   592,   452,     0,     0,     0,     0,   715,     0,
     267,     0,     0,     0,     0,   715,   590,     0,     0,     0,
       0,     0,     0,     0,   267,     0,     0,     0,     0,     0,
       0,     0,   290,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,   101,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   715,     0,     0,   375,
       0,   290,     0,     0,     0,   450,     0,     0,     0,     0,
       0,     0,     0,   267,     0,     0,     0,   120,     0,     0,
     101,     0,   290,   290,   635,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   359,   101,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     290,     0,     0,   715,     0,   135,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,     0,     0,   715,
     857,     0,   715,   860,     0,   135,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   267,
     267,     0,     0,     0,     0,     0,     0,     0,   290,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   893,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   135,     0,
       0,     0,     0,  1934,     0,     0,   125,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   592,   290,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,     0,     0,     0,   635,     0,     0,     0,
     635,   635,     0,     0,     0,     0,     0,   635,     0,     0,
     101,     0,     0,     0,   101,   101,     0,   988,     0,     0,
     290,   290,     0,     0,     0,     0,     0,     0,   101,     0,
       0,   290,     0,   290,     0,   180,     0,     0,     0,     0,
     592,     0,     0,   552,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   359,     0,     0,     0,
       0,     0,   538,   180,     0,     0,     0,     0,     0,     0,
       0,   715,     0,     0,     0,   715,     0,   125,   101,     0,
       0,   135,     0,     0,     0,     0,     0,     0,     0,     0,
     135,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   125,     0,     0,
       0,     0,   180,     0,   290,     0,   135,     0,     0,     0,
       0,     0,     0,     0,   135,   180,     0,   180,     0,   452,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   125,     0,     0,     0,     0,     0,     0,     0,   135,
       0,     0,     0,     0,     0,     0,     0,     0,   180,     0,
     464,     0,     0,     0,   135,     0,     0,     0,     0,     0,
       0,     0,   715,     0,   359,   101,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   464,     0,     0,     0,     0,
       0,   893,   635,     0,   635,     0,     0,     0,     0,     0,
     180,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   715,   715,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   715,  1152,     0,   715,  1155,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   290,     0,     0,     0,   478,     0,     0,     0,     0,
       0,     0,     0,   552,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,   178,     0,   509,   290,   512,     0,
       0,     0,   478,   518,     0,   290,     0,   290,     0,     0,
       0,     0,     0,   527,   528,     0,     0,     0,     0,   135,
     135,   135,   135,   135,   135,   135,     0,     0,     0,     0,
       0,   478,   478,     0,   359,     0,     0,     0,     0,   180,
       0,     0,     0,   180,   135,     0,   180,   180,   135,   135,
     180,     0,   101,   180,   180,     0,   180,     0,   180,   135,
     290,   635,   135,   635,     0,     0,     0,   538,     0,     0,
     538,   393,     0,     0,     0,   635,   538,     0,     0,     0,
       0,     0,     0,     0,   399,   538,   400,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   290,   538,     0,     0,     0,   456,     0,     0,
       0,     0,     0,     0,   135,     0,     0,     0,   180,     0,
       0,   180,     0,   290,     0,   715,     0,     0,     0,   715,
       0,     0,     0,     0,     0,     0,   715,  1272,     0,     0,
     715,  1276,     0,     0,   715,  1280,     0,     0,     0,   515,
       0,  1283,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   953,     0,   135,     0,
       0,     0,     0,   101,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   290,   715,     0,     0,     0,   135,
       0,     0,     0,   101,   180,     0,     0,     0,     0,     0,
       0,     0,   653,     0,     0,     0,   635,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   101,     0,   715,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   478,     0,     0,     0,     0,     0,   478,     0,     0,
       0,     0,     0,     0,     0,   717,   718,     0,   290,   722,
       0,     0,   725,   726,     0,   728,     0,   729,     0,     0,
       0,  1067,  1069,     0,     0,     0,     0,     0,     0,     0,
       0,   290,   290,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   715,  1460,     0,   635,
     635,  1467,     0,     0,     0,     0,   180,     0,     0,   180,
     180,     0,   180,     0,   180,   180,     0,     0,   290,   180,
     180,     0,     0,     0,     0,     0,   135,     0,     0,     0,
       0,     0,   290,     0,     0,     0,   715,  1494,     0,   715,
    1498,     0,     0,   715,  1502,     0,     0,     0,     0,     0,
     478,   478,   478,   478,   478,   478,   478,   478,   478,   478,
     478,   478,   478,   478,   478,   478,   478,   478,   478,     0,
       0,     0,     0,     0,     0,   464,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,     0,     0,     0,   478,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   855,     0,    14,    15,    16,    17,    18,
       0,   290,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -499,  -499,   135,  -499,    52,
       0,    53,     0,     0,  -499,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   464,     0,     0,     0,
       0,    64,     0,     0,     0,   715,  1614,   135,     0,     0,
       0,     0,     0,   635,     0,     0,     0,     0,     0,     0,
       0,     0,   180,     0,   180,     0,     0,   290,   290,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   135,     0,   180,   180,     0,     0,     0,     0,     0,
     180,     0,     0,     0,     0,   974,     0,     0,   977,   978,
       0,   981,     0,   983,   984,    81,     0,     0,   986,   987,
     194,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,   350,
     351,     0,   352,    52,     0,    53,     0,     0,   353,     0,
       0,    55,    56,    57,    58,    59,    60,    61,     0,     0,
    1053,     0,     0,     0,     0,   478,     0,     0,     0,     0,
       0,   478,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   478,     0,     0,     0,     0,     0,   180,     0,
       0,     0,     0,   478,     0,     0,  1977,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,  -500,  -500,     0,
    -500,    52,     0,    53,   478,     0,  -500,     0,     0,     0,
     214,  -475,     0,   215,     0,   216,   217,     0,   218,     0,
       0,   180,     0,    64,     0,   180,     0,     0,     0,   180,
       0,  1121,   264,  1123,  -475,   220,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   715,     0,     0,     0,
       0,     0,  1145,  1146,     0,     0,     0,     0,     0,  1150,
       0,     0,     0,   715,     0,   221,   222,     0,   223,   224,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     225,   226,   227,     0,   228,   229,   230,    81,   231,   232,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
     180,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1507,     0,     0,     0,   233,     0,
       0,    84,   477,     0,     0,     0,     0,     0,   236,    86,
      87,   238,   239,   240,   241,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   486,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   538,     0,     0,     0,  1230,     0,     0,
       0,   545,   715,   715,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   715,     0,
       0,   180,     0,     0,   180,   180,     0,     0,     0,     0,
       0,     0,   180,   180,     0,     0,     0,     0,     0,     0,
       0,   478,   478,   478,     0,     0,     0,     0,     0,   478,
     478,     0,     0,     0,     0,     0,   180,     0,   180,     0,
       0,   180,     0,     0,   180,   264,     0,     0,   180,     0,
    1270,     0,     0,     0,  1274,     0,     0,     0,  1278,   678,
       0,   678,     0,     0,     0,     0,     0,     0,   478,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   715,     0,
       0,     0,     0,     0,     0,     0,   715,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   478,   478,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   715,     0,     0,  1230,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   715,  2040,     0,     0,
     715,     0,     0,     0,     0,   789,     0,     0,     0,     0,
       0,     0,   794,     0,     0,     0,     0,     0,     0,     0,
     478,  1700,  1708,     0,     0,  1700,  1719,     0,     0,     0,
       0,  1726,     0,     0,     0,  1730,     0,  1732,     0,  1719,
     180,     0,     0,   264,     0,   715,   715,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   835,     0,     0,     0,     0,     0,
    1458,   545,     0,  1462,  1465,     0,     0,   180,     0,     0,
       0,  1474,  1475,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   715,     0,     0,     0,
       0,     0,     0,     0,     0,  1487,   871,  1489,     0,     0,
    1492,     0,     0,  1496,     0,     0,     0,  1500,     0,   545,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   896,     0,   902,   904,     0,   264,     0,     0,     0,
       0,     0,     0,     0,   911,     0,   913,     0,     0,     0,
       0,     0,     0,   922,     0,   927,   922,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   214,
     931,  1835,   215,   545,   216,   217,     0,   218,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,  1855,  1857,     0,   220,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   264,     0,
     968,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1877,     0,   221,   222,     0,   223,   224,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   225,
     226,   227,     0,   228,   229,   230,     0,   231,   232,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,  1612,
       0,  1013,   678,     0,     0,     0,     0,     0,   678,     0,
       0,     0,     0,   486,   180,     0,     0,   233,     0,     0,
      84,   477,     0,     0,     0,     0,     0,   236,    86,    87,
     238,   239,   240,   241,     0,     0,     0,     0,   478,  1056,
     918,     0,   180,     0,     0,     0,  1462,  1921,   180,     0,
       0,     0,     0,     0,     0,  1924,     0,  1926,     0,     0,
    1930,  1936,     0,  1719,     0,   264,     0,     0,  1942,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,   180,     0,     0,   219,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,  2002,    64,     0,   180,   180,     0,     0,  2009,  2011,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1748,
     922,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2033,     0,  1147,   180,   180,     0,     0,    69,    70,
       0,     0,   464,     0,   478,     0,     0,   180,     0,     0,
       0,     0,   264,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2055,     0,  2058,     0,    81,  2060,  2062,     0,
       0,     0,   295,  1176,  2067,  2069,     0,     0,     0,     0,
     306,     0,     0,     0,     0,   896,   835,     0,     0,    84,
       0,     0,  1194,     0,     0,     0,   368,   922,     0,     0,
       0,     0,     0,  1796,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   678,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2105,  2107,  2109,     0,     0,     0,   306,     0,     0,
       0,   428,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   455,     0,     0,     0,     0,     0,  2128,
    2130,  2132,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   306,     0,
       0,   180,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   678,     0,   478,     0,  1254,     0,
       0,     0,     0,  1883,     0,  1259,     0,     0,     0,     0,
     542,   295,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1892,  1893,     0,   595,     0,     0,     0,
     180,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     478,     0,     0,     0,     0,     0,     0,   647,     0,     0,
       0,     0,     0,  1906,  1907,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1911,     0,     0,     0,
       0,     0,     0,     0,     0,   295,     0,     0,   670,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     486,     0,     0,     0,     0,     0,     0,     0,     0,   478,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   478,     0,   478,     0,     0,
       0,     0,     0,   295,   306,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1974,     0,     0,     0,     0,   368,
       0,     0,     0,     0,     0,     0,     0,   678,     0,     0,
       0,     0,     0,     0,     0,   478,  1434,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   922,     0,     0,   896,
       0,     0,     0,   670,     0,     0,     0,     0,     0,     0,
       0,     0,   295,   306,     0,  1472,     0,     0,     0,     0,
    2038,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   295,   595,     0,   819,     0,     0,   678,
     478,    14,    15,    16,    17,    18,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   295,   306,     0,  1508,     0,  1509,     0,     0,     0,
       0,     0,     0,   214,     0,     0,   215,     0,   216,   217,
       0,   218,     0,     0,     0,   306,     0,     0,   542,     0,
     542,   306,     0,     0,   306,     0,     0,    64,   220,     0,
       0,     0,     0,   542,     0,     0,   542,   542,   542,   455,
       0,     0,     0,     0,     0,  1562,  1562,     0,     0,     0,
       0,     0,   545,   647,     0,     0,     0,     0,   221,   222,
       0,   223,   224,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   225,   226,   227,     0,   228,   229,   230,
       0,   231,   232,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,   295,     0,     0,   932,     0,     0,     0,
       0,     0,     0,     0,  1712,  1713,  1714,  1715,     0,     0,
       0,   233,  1929,     0,    84,   477,     0,     0,     0,     0,
       0,   236,    86,    87,   238,   239,   240,   241,     0,     0,
       0,     0,     0,     0,     0,   678,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   455,   595,     0,     0,     0,  1638,     0,     0,     0,
       0,     0,   295,     0,   295,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1645,  1646,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2141,
       0,     0,     0,   306,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1544,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   306,
     306,     0,   922,   214,     0,   295,   215,     0,   216,   217,
       0,   218,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   678,     0,     0,   486,     0,  1317,     0,   220,  1319,
       0,  1320,  -253,  -253,  1321,  1322,  1323,  1324,  1325,  1326,
    1327,  1328,  1329,  1330,  1331,  1332,  -352,  -352,  1333,  1334,
    1335,  1336,  1337,  1338,  1339,     0,  1340,     0,   221,   222,
       0,   672,   224,  1341,  1342,    71,    72,    73,    74,    75,
      76,    77,    78,   225,   226,   227,  1343,   228,   229,   230,
       0,   231,   232,     0,     0,     0,     0,  1778,     0,    80,
    1780,     0,     0,     0,     0,   306,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -253,  1344,     0,   306,    84,   477,     0,     0,     0,   397,
       0,   236,    86,    87,   238,   239,   240,   241,     0,  1800,
       0,     0,   455,     0,     0,     0,  -193,     0,     0,     0,
       0,     0,     0,   486,     0,     0,     0,     0,     0,  1823,
       0,     0,     0,     0,     0,     0,     0,     0,   295,     0,
      14,    15,    16,    17,    18,     0,   455,    20,   595,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
    1870,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2141,   295,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1544,     0,   678,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   295,   214,     0,     0,   215,     0,   216,
     217,     0,   218,     0,     0,     0,     0,     0,     0,     0,
      81,     0,     0,     0,   295,     0,     0,  1317,     0,   220,
    1319,     0,  1320,  -254,  -254,  1321,  1322,  1323,  1324,  1325,
    1326,  1327,  1328,  1329,  1330,  1331,  1332,  -352,  -352,  1333,
    1334,  1335,  1336,  1337,  1338,  1339,     0,  1340,     0,   221,
     222,     0,   672,   224,  1341,  1342,    71,    72,    73,    74,
      75,    76,    77,    78,   225,   226,   227,  1343,   228,   229,
     230,     0,   231,   232,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,   295,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   428,     0,     0,     0,     0,
     306,  -254,  1344,     0,     0,    84,   477,     0,     0,     0,
     397,     0,   236,    86,    87,   238,   239,   240,   241,     0,
       0,     0,     0,     0,     0,     0,     0,  -193,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   922,     0,     0,     0,
       0,     0,     0,   542,     0,     0,     0,     0,     0,   455,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1104,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   295,   295,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -499,  -499,     0,  -499,    52,   295,
      53,     0,     0,  -499,     0,   219,    55,    56,    57,    58,
      59,    60,    61,   295,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,   542,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   542,   295,  1299,     0,     0,     0,    84,    85,     0,
       0,     0,     0,     0,     0,    86,    87,     0,     0,  1873,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   368,     0,     0,  1544,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   306,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   214,     0,     0,   215,     0,   216,   217,
       0,   218,     0,     0,     0,     0,     0,     0,   295,   295,
       0,     0,     0,     0,     0,     0,  1317,     0,   220,  1319,
       0,  1320,     0,     0,  1321,  1322,  1323,  1324,  1325,  1326,
    1327,  1328,  1329,  1330,  1331,  1332,  -352,  -352,  1333,  1334,
    1335,  1336,  1337,  1338,  1339,     0,  1340,     0,   221,   222,
       0,   672,   224,  1341,  1342,    71,    72,    73,    74,    75,
      76,    77,    78,   225,   226,   227,  1343,   228,   229,   230,
       0,   231,   232,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   542,   542,   542,     0,     0,   542,
     542,  1344,     0,     0,    84,   477,   670,     0,     0,   397,
       0,   236,    86,    87,   238,   239,   240,   241,     0,     0,
       0,     0,     0,     0,     0,     0,  -193,     0,     0,     0,
       0,     0,     0,     0,   306,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     4,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,  1316,   368,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,   542,
       0,   214,     0,    52,   215,    53,   216,   217,     0,   218,
      54,    55,    56,    57,    58,    59,    60,    61,    62,     0,
       0,     0,    63,     0,  1317,    64,  1318,  1319,     0,  1320,
       0,     0,  1321,  1322,  1323,  1324,  1325,  1326,  1327,  1328,
    1329,  1330,  1331,  1332,  -352,  -352,  1333,  1334,  1335,  1336,
    1337,  1338,  1339,   306,  1340,     0,   221,   222,    67,   672,
     224,  1341,  1342,    71,    72,    73,    74,    75,    76,    77,
      78,   225,   226,   227,  1343,   228,   229,   230,     0,   231,
     232,     0,     0,   368,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -3,  1344,
       0,     0,    84,  1345,     0,     0,     0,   397,     0,   236,
      86,    87,   238,   239,   240,   241,     0,     0,     0,     0,
       0,     0,     0,     0,  -193,     0,     0,     0,     0,     0,
       0,     0,     4,   194,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,  1316,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   214,     0,    52,   215,    53,   216,
     217,     0,   218,    54,    55,    56,    57,    58,    59,    60,
      61,    62,     0,   368,     0,    63,     0,  1317,    64,  1318,
    1319,     0,  1320,     0,     0,  1321,  1322,  1323,  1324,  1325,
    1326,  1327,  1328,  1329,  1330,  1331,  1332,  -352,  -352,  1333,
    1334,  1335,  1336,  1337,  1338,  1339,   542,  1340,     0,   221,
     222,    67,   672,   224,  1341,  1342,    71,    72,    73,    74,
      75,    76,    77,    78,   225,   226,   227,  1343,   228,   229,
     230,     0,   231,   232,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1344,     0,     0,    84,  1345,     0,     0,     0,
     397,     0,   236,    86,    87,   238,   239,   240,   241,     0,
       0,     0,     0,     0,     0,   542,     0,  -193,   670,     4,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   214,     0,    52,   215,    53,   216,   217,     0,   218,
      54,    55,    56,    57,    58,    59,    60,    61,    62,     0,
       0,     0,    63,     0,     0,    64,   220,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,   222,    67,   223,
     224,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   225,   226,   227,     0,   228,   229,   230,     0,   231,
     232,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1712,  1713,  1714,  1715,     0,     0,     0,   233,
    1716,  1717,    84,  1345,     0,     0,     0,     0,     0,   236,
      86,    87,   238,   239,   240,   241,     0,     0,     0,     0,
       0,     0,     0,     0,  1718,     4,   194,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   214,     0,    52,
     215,    53,   216,   217,     0,   218,    54,    55,    56,    57,
      58,    59,    60,    61,    62,     0,     0,     0,    63,     0,
       0,    64,   220,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,   222,    67,   223,   224,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   225,   226,   227,
       0,   228,   229,   230,     0,   231,   232,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1712,  1713,
    1714,  1715,     0,     0,     0,   233,  1716,     0,    84,  1345,
       0,     0,     0,     0,     0,   236,    86,    87,   238,   239,
     240,   241,     0,     0,     0,     0,     0,     0,     0,     0,
    1718,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,    54,    55,    56,    57,    58,    59,    60,    61,
      62,     0,     0,     0,    63,     0,     0,    64,    65,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    66,     0,     0,     0,
      67,    68,     0,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,    79,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    82,    83,     0,    84,    85,     0,     0,     0,     0,
       0,     0,    86,    87,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    88,     0,    89,   355,   194,     6,
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
       0,     0,     0,     0,     0,     0,     0,   158,     0,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    82,    83,     0,
      84,   356,     0,     0,     0,  -835,     0,     0,    86,    87,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      88,   355,   194,     6,     7,     8,     9,    10,    11,    12,
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
       0,   158,     0,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    82,    83,     0,    84,   356,     0,     0,     0,     0,
       0,     0,    86,    87,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    88,   194,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,   219,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   158,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,    81,   772,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   955,    83,  -700,    84,   621,     0,
       0,     0,     0,     0,     0,    86,    87,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    88,   194,     6,
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
       0,     0,     0,     0,     0,     0,     0,   158,     0,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    82,    83,     0,
      84,   356,     0,     0,     0,  -839,     0,     0,    86,    87,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      88,   194,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    -500,  -500,     0,  -500,    52,     0,    53,     0,     0,  -500,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     158,     0,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,    83,     0,    84,   356,     0,     0,     0,     0,     0,
       0,    86,    87,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    88,     4,   194,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   214,     0,    52,   215,
      53,   216,   217,     0,   218,    54,    55,    56,    57,    58,
      59,    60,    61,    62,     0,     0,     0,    63,     0,     0,
      64,   220,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   221,   222,    67,   223,   224,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   225,   226,   227,     0,
     228,   229,   230,     0,   231,   232,     0,     0,     0,     0,
       0,     0,    80,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   233,     0,  1710,    84,  1345,     0,
       0,     0,     0,     0,   236,    86,    87,   238,   239,   240,
     241,     4,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   214,     0,    52,   215,    53,   216,   217,
       0,   218,    54,    55,    56,    57,    58,    59,    60,    61,
      62,     0,     0,     0,    63,     0,     0,    64,   220,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,   222,
      67,   223,   224,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   225,   226,   227,     0,   228,   229,   230,
       0,   231,   232,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   233,     0,     0,    84,  1345,     0,     0,     0,     0,
       0,   236,    86,    87,   238,   239,   240,   241,   194,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   214,
       0,    52,   215,    53,   216,   217,     0,   218,   219,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   220,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,   222,     0,   223,   224,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   225,
     226,   227,     0,   228,   229,   230,     0,   231,   232,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   233,     0,     0,
      84,   234,   543,   544,     0,     0,     0,   236,   237,    87,
     238,   239,   240,   241,   194,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   214,     0,    52,   215,    53,
     216,   217,     0,   218,   219,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     220,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     221,   222,     0,   223,   224,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   225,   226,   227,     0,   228,
     229,   230,     0,   231,   232,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   233,     0,     0,    84,   234,   667,   544,
       0,     0,     0,   236,   237,    87,   238,   239,   240,   241,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   214,     0,    52,   215,    53,   216,   217,     0,   218,
     219,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   220,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,   222,     0,   223,
     224,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   225,   226,   227,     0,   228,   229,   230,     0,   231,
     232,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   233,
       0,     0,    84,   586,   877,   544,     0,     0,     0,   236,
     237,    87,   238,   239,   240,   241,   194,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   214,     0,    52,
     215,    53,   216,   217,     0,   218,   219,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   220,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,   222,     0,   223,   224,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   225,   226,   227,
       0,   228,   229,   230,     0,   231,   232,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   233,     0,     0,    84,   234,
     815,   544,     0,     0,     0,   236,   237,    87,   238,   239,
     240,   241,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   214,     0,    52,   215,    53,   216,   217,
       0,   218,   219,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   220,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,   222,
       0,   223,   224,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   225,   226,   227,     0,   228,   229,   230,
       0,   231,   232,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   233,     0,     0,    84,   234,   235,   544,     0,     0,
       0,   236,   237,    87,   238,   239,   240,   241,   194,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   214,
       0,    52,   215,    53,   216,   217,     0,   218,   219,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   220,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,   222,     0,   223,   224,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   225,
     226,   227,     0,   228,   229,   230,     0,   231,   232,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   233,     0,     0,
      84,   234,   235,     0,     0,     0,     0,   236,   237,    87,
     238,   239,   240,   241,   194,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   214,     0,    52,   215,    53,
     216,   217,     0,   218,   219,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     220,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     221,   222,     0,   223,   224,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   225,   226,   227,     0,   228,
     229,   230,     0,   231,   232,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   233,     0,     0,    84,   234,   667,     0,
       0,     0,     0,   236,   237,    87,   238,   239,   240,   241,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   214,     0,    52,   215,    53,   216,   217,     0,   218,
     219,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   220,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,   222,     0,   223,
     224,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   225,   226,   227,     0,   228,   229,   230,     0,   231,
     232,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   233,
       0,     0,    84,   234,   815,     0,     0,     0,     0,   236,
     237,    87,   238,   239,   240,   241,   194,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   214,     0,    52,
     215,    53,   216,   217,     0,   218,   219,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   220,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,   222,     0,   223,   224,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   225,   226,   227,
       0,   228,   229,   230,     0,   231,   232,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   233,     0,     0,    84,   234,
     543,     0,     0,     0,     0,   236,   237,    87,   238,   239,
     240,   241,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   214,     0,    52,   215,    53,   216,   217,
       0,   218,   219,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   220,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,   222,
       0,   223,   224,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   225,   226,   227,     0,   228,   229,   230,
       0,   231,   232,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   233,     0,     0,    84,   586,   877,     0,     0,     0,
       0,   236,   237,    87,   238,   239,   240,   241,   194,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   214,
       0,    52,   215,    53,   216,   217,     0,   218,   219,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   220,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,   222,     0,   223,   224,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   225,
     226,   227,     0,   228,   229,   230,     0,   231,   232,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   233,     0,     0,
      84,   586,     0,   544,     0,     0,     0,   236,    86,    87,
     238,   239,   240,   241,   194,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   214,     0,    52,   215,    53,
     216,   217,     0,   218,   219,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     220,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     221,   222,     0,   223,   224,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   225,   226,   227,     0,   228,
     229,   230,     0,   231,   232,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   233,     0,     0,    84,   899,     0,     0,
       0,     0,     0,   236,   900,    87,   238,   239,   240,   241,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   214,     0,    52,   215,    53,   216,   217,     0,   218,
     219,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   220,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,   222,     0,   223,
     224,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   225,   226,   227,     0,   228,   229,   230,     0,   231,
     232,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   233,
       0,     0,    84,   477,     0,     0,     0,     0,     0,   236,
      86,    87,   238,   239,   240,   241,  1984,     0,    -2,    -2,
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
      -2,  2014,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
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
       0,     0,    -2,     0,  1104,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,  -499,  -499,     0,  -499,
      52,     0,    53,     0,     0,  -499,     0,   219,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1385,     0,  1104,     0,    84,
      85,     0,     0,     0,     0,     0,     0,    86,    87,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,  -499,
    -499,     0,  -499,    52,     0,    53,     0,     0,  -499,     0,
     219,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1513,     0,
    1104,     0,    84,    85,     0,     0,     0,     0,     0,     0,
      86,    87,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,  -499,  -499,     0,  -499,    52,     0,    53,     0,
       0,  -499,     0,   219,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1609,     0,  1104,     0,    84,    85,     0,     0,     0,
       0,     0,     0,    86,    87,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -499,  -499,     0,  -499,    52,
       0,    53,     0,     0,  -499,     0,   219,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1775,     0,  1104,     0,    84,    85,
       0,     0,     0,     0,     0,     0,    86,    87,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -499,  -499,
       0,  -499,    52,     0,    53,     0,     0,  -499,     0,   219,
      55,    56,    57,    58,    59,    60,    61,  1469,     0,     0,
       0,     0,     0,     0,    64,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,   214,     0,     0,
     215,     0,   216,   217,     0,   218,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,    81,     0,
       0,    64,   220,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    84,    85,     0,     0,     0,     0,     0,     0,    86,
      87,     0,   221,   222,     0,   223,   224,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   225,   226,   227,
       0,   228,   229,   230,     0,   231,   232,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   233,     0,     0,    84,   477,
       0,     0,     0,     0,     0,   236,  1470,    87,   238,   239,
     240,   241,    14,    15,    16,    17,    18,    19,     0,    20,
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
      84,   298,     0,     0,     0,     0,     0,     0,    86,    87,
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
      84,   644,     0,     0,     0,     0,     0,     0,    86,    87,
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
    1520,     0,  1521,     0,     0,    86,    87,  1522,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,  1523,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    67,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1524,
       0,     0,     0,    84,   865,     0,  1520,     0,  1521,     0,
       0,    86,    87,  1522,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,  1523,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    67,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1666,     0,     0,     0,    84,
     865,     0,  1520,     0,  1521,     0,     0,    86,    87,  1522,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,  1523,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    67,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1667,     0,     0,     0,    84,   865,     0,  1520,     0,
    1521,     0,     0,    86,    87,  1522,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,  1523,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    67,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1668,     0,     0,
       0,    84,   865,     0,     0,     0,     0,     0,     0,    86,
      87,   355,   194,     6,     7,     8,     9,    10,    11,    12,
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
       0,     0,     0,     0,     0,     0,     0,   355,     0,     0,
       0,     0,     0,     0,    84,   356,     0,    14,    15,    16,
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
      84,   644,     0,     0,     0,     0,     0,     0,    86,    87,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     219,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
     772,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   816,
       0,  -700,    84,   621,     0,     0,     0,     0,     0,     0,
      86,    87,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   219,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,   772,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   838,     0,  -700,    84,   712,     0,     0,     0,     0,
       0,     0,    86,    87,   194,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   219,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,    81,  1182,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -708,    84,   738,     0,     0,
       0,     0,     0,     0,    86,    87,   194,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   219,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   445,    84,   446,
       0,     0,     0,     0,     0,     0,    86,    87,   194,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   219,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      84,   738,   739,     0,     0,     0,     0,     0,    86,    87,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     219,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
    1626,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    84,   738,     0,     0,     0,     0,     0,     0,
      86,    87,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   219,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,  1628,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    84,   738,     0,     0,     0,     0,
       0,     0,    86,    87,   194,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   219,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,   644,     0,     0,
       0,     0,     0,     0,    86,    87,   194,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   219,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    84,   738,
       0,     0,     0,     0,     0,     0,    86,    87,   194,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   219,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      84,   446,     0,     0,     0,     0,     0,     0,    86,    87,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,  1544,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     214,     0,     0,   215,     0,   216,   217,     0,   218,     0,
       0,     0,    84,   356,     0,     0,     0,     0,     0,     0,
      86,    87,     0,  1317,     0,   220,  1319,     0,  1320,  1914,
    1915,  1321,  1322,  1323,  1324,  1325,  1326,  1327,  1328,  1329,
    1330,  1331,  1332,     0,     0,  1333,  1334,  1335,  1336,  1337,
    1338,  1339,     0,  1340,     0,   221,   222,     0,   672,   224,
    1341,  1342,    71,    72,    73,    74,    75,    76,    77,    78,
     225,   226,   227,  1343,   228,   229,   230,     0,   231,   232,
       0,     0,     0,     0,     0,     0,    80,     0,     0,  1544,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1344,     0,
       0,    84,   477,     0,     0,     0,   397,     0,   236,    86,
      87,   238,   239,   240,   241,     0,   214,     0,     0,   215,
       0,   216,   217,  -193,   218,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1317,
       0,   220,  1319,     0,  1320,     0,     0,  1321,  1322,  1323,
    1324,  1325,  1326,  1327,  1328,  1329,  1330,  1331,  1332,     0,
       0,  1333,  1334,  1335,  1336,  1337,  1338,  1339,     0,  1340,
       0,   221,   222,     0,   672,   224,  1341,  1342,    71,    72,
      73,    74,    75,    76,    77,    78,   225,   226,   227,  1343,
     228,   229,   230,     0,   231,   232,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1344,     0,     0,    84,   477,     0,
       0,     0,   397,     0,   236,    86,    87,   238,   239,   240,
     241,     0,     0,     0,     0,     0,     0,     0,     0,  -193,
     401,   194,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -423,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    84,     0,     0,     0,     0,  -423,   401,
     194,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -424,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    84,     0,     0,     0,     0,  -424,   401,   194,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    84,     0,     0,     0,     0,  -423,    14,    15,    16,
      17,    18,    19,   529,    20,   530,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   214,
       0,    52,   215,    53,   216,   217,     0,   218,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   220,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   531,     0,     0,     0,
       0,  1332,     0,  -352,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,   222,     0,   223,   224,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   225,
     226,   227,     0,   228,   229,   230,     0,   231,   232,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1344,     0,     0,
      84,   532,     0,     0,     0,   397,     0,   236,    86,    87,
     533,   534,   240,   241,    14,    15,    16,    17,    18,    19,
     529,    20,   530,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   214,     0,    52,   215,
      53,   216,   217,     0,   218,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,   220,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   531,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   221,   222,     0,   223,   224,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   225,   226,   227,     0,
     228,   229,   230,     0,   231,   232,     0,     0,     0,     0,
       0,     0,    80,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   233,     0,     0,    84,   532,     0,
       0,     0,   397,     0,   236,    86,    87,   533,   534,   240,
     241,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   214,     0,    52,   215,    53,   216,   217,
       0,   218,    54,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   220,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,   222,
       0,   223,   224,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   225,   226,   227,     0,   228,   229,   230,
       0,   231,   232,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   233,     0,   483,    84,   484,     0,     0,     0,     0,
       0,   236,    86,    87,   238,   239,   240,   241,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     214,     0,    52,   215,    53,   216,   217,     0,   218,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,   220,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,   222,     0,   223,   224,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     225,   226,   227,     0,   228,   229,   230,     0,   231,   232,
       0,     0,     0,     0,     0,     0,    80,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   233,     0,
       0,    84,   484,     0,     0,     0,   397,     0,   236,    86,
      87,   238,   239,   240,   241,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   214,     0,    52,
     215,    53,   216,   217,     0,   218,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   220,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,   222,     0,   223,   224,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   225,   226,   227,
       0,   228,   229,   230,     0,   231,   232,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   233,     0,     0,    84,   532,
       0,     0,     0,   397,     0,   236,    86,    87,   238,   239,
     240,   241,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   214,     0,    52,   215,    53,   216,
     217,     0,   218,   219,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   220,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   221,
     222,     0,   223,   224,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   225,   226,   227,     0,   228,   229,
     230,     0,   231,   232,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   233,     0,     0,    84,   586,     0,     0,     0,
       0,     0,   236,    86,    87,   238,   239,   240,   241,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   214,     0,    52,   215,    53,   216,   217,     0,   218,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   220,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,   222,     0,   223,
     224,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   225,   226,   227,     0,   228,   229,   230,     0,   231,
     232,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   233,
       0,     0,    84,   484,     0,     0,     0,     0,     0,   236,
      86,    87,   238,   239,   240,   241,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   214,     0,
      52,   215,    53,   216,   217,     0,   218,   219,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,   220,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   221,   222,     0,   223,   224,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   225,   226,
     227,     0,   228,   229,   230,     0,   231,   232,     0,     0,
       0,     0,     0,     0,    80,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   233,     0,     0,    84,
     477,     0,     0,     0,     0,     0,   236,    86,    87,   238,
     239,   240,   241,   194,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   520,     0,   521,   522,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,   355,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,   -17,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -500,  -500,     0,  -500,    52,     0,    53,     0,     0,
    -500,     0,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    64,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,    69,    70,    52,     0,    53,     0,     0,
       0,     0,   219,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    84,     0,     0,     0,     0,     0,
       0,   158,     0,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,   772,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -700,    84,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   219,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   158,     0,   548,    70,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   891,     0,     0,    84,   549,
       0,     0,     0,     0,     0,     0,    86,    87,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   158,     0,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    84,    85,     0,     0,     0,     0,     0,     0,    86,
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
       0,   158,     0,   548,    70,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    84,   549,     0,     0,     0,     0,
       0,     0,    86,    87,   194,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   219,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,   772,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -700,    84,   194,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,   219,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,  1310,     0,     0,
       0,     0,   194,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,    84,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   219,    55,    56,    57,    58,    59,    60,    61,
       0,    14,    15,    16,    17,    18,    19,    64,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,    69,    70,    52,     0,    53,     0,     0,
       0,     0,    54,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    84,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   873,    84,   865,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   891,     0,     0,    84,   549,     0,
       0,     0,     0,     0,     0,    86,    87,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   219,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   891,     0,     0,
      84,   549,     0,     0,     0,     0,     0,     0,    86,    87,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,  1419,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    84,   865,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,    84,   408,     0,     0,
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
     298,     0,     0,     0,     0,     0,     0,    86,    87,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     219,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    84,   408,     0,     0,     0,     0,     0,     0,
      86,    87,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   219,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    84,   549,     0,     0,     0,
       0,     0,     0,    86,    87,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   219,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    84,   446,
       0,     0,     0,     0,     0,     0,    86,    87,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
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
       0,    84,   865,     0,     0,     0,     0,     0,     0,    86,
      87,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   219,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    84,   644,     0,     0,     0,     0,
       0,     0,    86,    87,   194,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,  -500,  -500,     0,  -500,    52,     0,    53,
       0,     0,  -500,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,    64,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,    69,    70,    52,     0,    53,
       0,     0,     0,     0,    54,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,   549,     0,     0,
       0,     0,     0,     0,    86,    87,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,   219,    55,    56,
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
     425,     0,     0,     0,     0,     0,     0,    86,    87,    14,
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
       0,     0,    84,    85,     0,     0,     0,     0,     0,     0,
      86,    87,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   219,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    84,   865,     0,     0,     0,
       0,     0,     0,    86,    87,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   219,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    84,     0,
       0,    14,    15,    16,    17,    18,    86,    87,    20,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    84,   425,     0,    14,    15,    16,
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
      84,   644,     0,    14,    15,    16,    17,    18,    86,    87,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,  -500,  -500,     0,  -500,    52,     0,    53,
       0,     0,  -500,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,     0,     0,     0,
       0,     0,     0,     0,    86,    87,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   214,     0,    52,   215,    53,   216,   217,     0,   218,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   220,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,   222,     0,   223,
     224,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   225,   226,   227,     0,   228,   229,   230,     0,   231,
     232,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   233,
       0,     0,    84,   477,  1055,     0,     0,     0,     0,   236,
     237,    87,   238,   239,   240,   241,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   214,     0,    52,   215,    53,   216,   217,     0,   218,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   220,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,   222,     0,   223,
     224,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   225,   226,   227,     0,   228,   229,   230,     0,   231,
     232,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   233,
       0,     0,    84,   477,     0,     0,     0,     0,     0,   236,
      86,    87,   238,   239,   240,   241,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   158,     0,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,    84,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,    54,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    84,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,  -500,  -500,     0,  -500,
      52,     0,    53,     0,     0,  -500,     0,   194,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    64,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,    69,    70,
      52,     0,    53,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    84,
       0,     0,     0,     0,     0,     0,   195,     0,   196,   197,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   194,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,    81,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,   214,     0,     0,
     215,     0,   216,   217,     0,   218,     0,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,   220,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   520,     0,
     521,   522,   221,   222,     0,   223,   224,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   225,   226,   227,
       0,   228,   229,   230,     0,   231,   232,     0,    81,   214,
       0,     0,   215,    80,   216,   217,     0,   218,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1712,  1713,
    1714,  1715,     0,     0,   220,   233,  1856,     0,    84,   477,
       0,     0,     0,     0,     0,   236,    86,    87,   238,   239,
     240,   241,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,   222,     0,   672,   224,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   225,
     226,   227,     0,   228,   229,   230,   214,   231,   232,   215,
       0,   216,   217,     0,   218,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   220,     0,     0,     0,     0,     0,   233,    83,     0,
     673,   674,     0,     0,     0,   675,     0,   236,    86,    87,
     238,   239,   240,   241,     0,     0,     0,     0,     0,     0,
       0,   221,   222,     0,   223,   224,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   225,   226,   227,     0,
     228,   229,   230,   214,   231,   232,   215,     0,   216,   217,
       0,   218,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   220,     0,
       0,     0,     0,     0,   233,  1179,     0,    84,   477,     0,
       0,     0,  1180,     0,   236,    86,    87,   238,   239,   240,
     241,     0,     0,     0,     0,     0,     0,     0,   221,   222,
       0,   223,   224,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   225,   226,   227,     0,   228,   229,   230,
     214,   231,   232,   215,     0,   216,   217,     0,   218,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   220,     0,     0,     0,     0,
       0,   233,     0,     0,    84,   477,     0,     0,     0,   675,
       0,   236,    86,    87,   238,   239,   240,   241,     0,     0,
       0,     0,     0,     0,     0,   221,   222,     0,   223,   224,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     225,   226,   227,     0,   228,   229,   230,   214,   231,   232,
     215,     0,   216,   217,     0,   218,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   220,     0,     0,     0,     0,     0,   233,     0,
       0,    84,   477,     0,   544,     0,     0,     0,   236,    86,
      87,   238,   239,   240,   241,     0,     0,     0,     0,     0,
       0,     0,   221,   222,     0,   223,   224,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   225,   226,   227,
       0,   228,   229,   230,   214,   231,   232,   215,     0,   216,
     217,     0,   218,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   220,
       0,     0,     0,     0,     0,   233,   895,     0,    84,   477,
       0,     0,     0,     0,     0,   236,    86,    87,   238,   239,
     240,   241,     0,     0,     0,     0,     0,     0,     0,   221,
     222,     0,   223,   224,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   225,   226,   227,     0,   228,   229,
     230,   214,   231,   232,   215,     0,   216,   217,     0,   218,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   220,     0,     0,     0,
       0,     0,   233,     0,     0,    84,   477,     0,     0,     0,
     397,     0,   236,    86,    87,   238,   239,   240,   241,     0,
       0,     0,     0,     0,     0,     0,   221,   222,     0,   223,
     224,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   225,   226,   227,     0,   228,   229,   230,   214,   231,
     232,   215,     0,   216,   217,     0,   218,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   220,     0,     0,     0,     0,     0,   233,
       0,     0,    84,   477,     0,     0,   952,     0,     0,   236,
      86,    87,   238,   239,   240,   241,     0,     0,     0,     0,
       0,     0,     0,   221,   222,     0,   223,   224,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   225,   226,
     227,     0,   228,   229,   230,   214,   231,   232,   215,     0,
     216,   217,     0,   218,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     220,     0,     0,     0,     0,     0,   233,     0,     0,    84,
     477,   965,     0,     0,     0,     0,   236,   237,    87,   238,
     239,   240,   241,     0,     0,     0,     0,     0,     0,     0,
     221,   222,     0,   223,   224,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   225,   226,   227,     0,   228,
     229,   230,   214,   231,   232,   215,     0,   216,   217,     0,
     218,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   220,     0,     0,
       0,     0,     0,   233,     0,     0,    84,   477,   989,     0,
       0,     0,     0,   236,    86,    87,   238,   239,   240,   241,
       0,     0,     0,     0,     0,     0,     0,   221,   222,     0,
     223,   224,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   225,   226,   227,     0,   228,   229,   230,   214,
     231,   232,   215,     0,   216,   217,     0,   218,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   220,     0,     0,     0,     0,     0,
     233,  1433,     0,    84,   477,     0,     0,     0,     0,     0,
     236,    86,    87,   238,   239,   240,   241,     0,     0,     0,
       0,     0,     0,     0,   221,   222,     0,   223,   224,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   225,
     226,   227,     0,   228,   229,   230,   214,   231,   232,   215,
       0,   216,   217,     0,   218,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   220,     0,     0,     0,     0,     0,   233,     0,     0,
      84,   477,     0,     0,     0,  1554,     0,   236,    86,    87,
     238,   239,   240,   241,     0,     0,     0,     0,     0,     0,
       0,   221,   222,     0,   223,   224,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   225,   226,   227,     0,
     228,   229,   230,   214,   231,   232,   215,     0,   216,   217,
       0,   218,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   220,     0,
       0,     0,     0,     0,   233,     0,     0,    84,   477,     0,
       0,     0,  1772,     0,   236,    86,    87,   238,   239,   240,
     241,     0,     0,     0,     0,     0,     0,     0,   221,   222,
       0,   223,   224,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   225,   226,   227,     0,   228,   229,   230,
     214,   231,   232,   215,     0,   216,   217,     0,   218,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   220,     0,     0,     0,     0,
       0,   233,     0,  1920,    84,   477,     0,     0,     0,     0,
       0,   236,    86,    87,   238,   239,   240,   241,     0,     0,
       0,     0,     0,     0,     0,   221,   222,     0,   223,   224,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     225,   226,   227,     0,   228,   229,   230,   214,   231,   232,
     215,     0,   216,   217,     0,   218,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   220,     0,     0,     0,     0,     0,   233,  1925,
       0,    84,   477,     0,     0,     0,     0,     0,   236,    86,
      87,   238,   239,   240,   241,     0,     0,     0,     0,     0,
       0,     0,   221,   222,     0,   223,   224,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   225,   226,   227,
       0,   228,   229,   230,     0,   231,   232,     0,     0,   214,
       0,     0,   215,    80,   216,   217,     0,   218,  2001,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   220,   233,  1935,     0,    84,   477,
       0,     0,     0,     0,     0,   236,    86,    87,   238,   239,
     240,   241,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,   222,     0,   223,   224,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   225,
     226,   227,     0,   228,   229,   230,   214,   231,   232,   215,
       0,   216,   217,     0,   218,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   220,     0,     0,     0,     0,     0,   233,     0,     0,
      84,   477,     0,     0,     0,     0,     0,   236,    86,    87,
     238,   239,   240,   241,     0,     0,     0,     0,     0,     0,
       0,   221,   222,     0,   223,   224,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   225,   226,   227,     0,
     228,   229,   230,   214,   231,   232,   215,     0,   216,   217,
       0,   218,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   220,     0,
       0,     0,     0,     0,   233,  2008,     0,    84,   477,     0,
       0,     0,     0,     0,   236,    86,    87,   238,   239,   240,
     241,     0,     0,     0,     0,     0,     0,     0,   221,   222,
       0,   223,   224,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   225,   226,   227,     0,   228,   229,   230,
     214,   231,   232,   215,     0,   216,   217,     0,   218,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   220,     0,     0,     0,     0,
       0,   233,  2010,     0,    84,   477,     0,     0,     0,     0,
       0,   236,    86,    87,   238,   239,   240,   241,     0,     0,
       0,     0,     0,     0,     0,   221,   222,     0,   223,   224,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     225,   226,   227,     0,   228,   229,   230,   214,   231,   232,
     215,     0,   216,   217,     0,   218,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   220,     0,     0,     0,     0,     0,   233,  2057,
       0,    84,   477,     0,     0,     0,     0,     0,   236,    86,
      87,   238,   239,   240,   241,     0,     0,     0,     0,     0,
       0,     0,   221,   222,     0,   223,   224,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   225,   226,   227,
       0,   228,   229,   230,   214,   231,   232,   215,     0,   216,
     217,     0,   218,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   220,
       0,     0,     0,     0,     0,   233,  2059,     0,    84,   477,
       0,     0,     0,     0,     0,   236,    86,    87,   238,   239,
     240,   241,     0,     0,     0,     0,     0,     0,     0,   221,
     222,     0,   223,   224,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   225,   226,   227,     0,   228,   229,
     230,   214,   231,   232,   215,     0,   216,   217,     0,   218,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   220,     0,     0,     0,
       0,     0,   233,  2061,     0,    84,   477,     0,     0,     0,
       0,     0,   236,    86,    87,   238,   239,   240,   241,     0,
       0,     0,     0,     0,     0,     0,   221,   222,     0,   223,
     224,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   225,   226,   227,     0,   228,   229,   230,   214,   231,
     232,   215,     0,   216,   217,     0,   218,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   220,     0,     0,     0,     0,     0,   233,
    2066,     0,    84,   477,     0,     0,     0,     0,     0,   236,
      86,    87,   238,   239,   240,   241,     0,     0,     0,     0,
       0,     0,     0,   221,   222,     0,   223,   224,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   225,   226,
     227,     0,   228,   229,   230,   214,   231,   232,   215,     0,
     216,   217,     0,   218,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     220,     0,     0,     0,     0,     0,   233,  2068,     0,    84,
     477,     0,     0,     0,     0,     0,   236,    86,    87,   238,
     239,   240,   241,     0,     0,     0,     0,     0,     0,     0,
     221,   222,     0,   223,   224,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   225,   226,   227,     0,   228,
     229,   230,   214,   231,   232,   215,     0,   216,   217,     0,
     218,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   220,     0,     0,
       0,     0,     0,   233,  2104,     0,    84,   477,     0,     0,
       0,     0,     0,   236,    86,    87,   238,   239,   240,   241,
       0,     0,     0,     0,     0,     0,     0,   221,   222,     0,
     223,   224,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   225,   226,   227,     0,   228,   229,   230,   214,
     231,   232,   215,     0,   216,   217,     0,   218,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   220,     0,     0,     0,     0,     0,
     233,  2106,     0,    84,   477,     0,     0,     0,     0,     0,
     236,    86,    87,   238,   239,   240,   241,     0,     0,     0,
       0,     0,     0,     0,   221,   222,     0,   223,   224,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   225,
     226,   227,     0,   228,   229,   230,   214,   231,   232,   215,
       0,   216,   217,     0,   218,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   220,     0,     0,     0,     0,     0,   233,  2108,     0,
      84,   477,     0,     0,     0,     0,     0,   236,    86,    87,
     238,   239,   240,   241,     0,     0,     0,     0,     0,     0,
       0,   221,   222,     0,   223,   224,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   225,   226,   227,     0,
     228,   229,   230,   214,   231,   232,   215,     0,   216,   217,
       0,   218,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   220,     0,
       0,     0,     0,     0,   233,  2127,     0,    84,   477,     0,
       0,     0,     0,     0,   236,    86,    87,   238,   239,   240,
     241,     0,     0,     0,     0,     0,     0,     0,   221,   222,
       0,   223,   224,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   225,   226,   227,     0,   228,   229,   230,
     214,   231,   232,   215,     0,   216,   217,     0,   218,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   220,     0,     0,     0,     0,
       0,   233,  2129,     0,    84,   477,     0,     0,     0,     0,
       0,   236,    86,    87,   238,   239,   240,   241,     0,     0,
       0,     0,     0,     0,     0,   221,   222,     0,   223,   224,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     225,   226,   227,     0,   228,   229,   230,   214,   231,   232,
     215,     0,   216,   217,     0,   218,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   220,     0,     0,     0,     0,     0,   233,  2131,
       0,    84,   477,     0,     0,     0,     0,     0,   236,    86,
      87,   238,   239,   240,   241,     0,     0,     0,     0,     0,
       0,     0,   221,   222,     0,   223,   224,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   225,   226,   227,
       0,   228,   229,   230,   214,   231,   232,   215,     0,   216,
     217,     0,   218,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   220,
       0,     0,     0,     0,     0,   233,     0,     0,    84,   477,
       0,     0,     0,     0,     0,   236,    86,    87,   238,   239,
     240,   241,     0,     0,     0,     0,     0,     0,     0,   221,
     222,     0,   223,   224,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   225,   226,   227,     0,   228,   229,
     230,   214,   231,   232,   215,     0,   216,   217,     0,   218,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   220,     0,     0,     0,
       0,     0,   508,     0,     0,    84,   477,     0,     0,     0,
       0,     0,   236,    86,    87,   238,   239,   240,   241,     0,
       0,     0,     0,     0,     0,     0,   221,   222,     0,   223,
     224,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   225,   226,   227,     0,   228,   229,   230,   214,   231,
     232,   215,     0,   216,   217,     0,   218,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   220,     0,     0,     0,     0,     0,   511,
       0,     0,    84,   477,     0,     0,     0,     0,     0,   236,
      86,    87,   238,   239,   240,   241,     0,     0,     0,     0,
       0,     0,     0,   221,   222,     0,   223,   224,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   225,   226,
     227,     0,   228,   229,   230,   214,   231,   232,   215,     0,
     216,   217,     0,   218,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     220,     0,     0,     0,     0,     0,   517,     0,     0,    84,
     477,     0,     0,     0,     0,     0,   236,    86,    87,   238,
     239,   240,   241,     0,     0,     0,     0,     0,     0,     0,
     221,   222,     0,   223,   224,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   225,   226,   227,     0,   228,
     229,   230,   214,   231,   232,   215,     0,   216,   217,     0,
     218,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   220,     0,     0,
       0,     0,     0,   526,     0,     0,    84,   477,     0,     0,
       0,     0,     0,   236,    86,    87,   238,   239,   240,   241,
       0,     0,     0,     0,     0,     0,     0,   221,   222,     0,
     223,   224,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   225,   226,   227,     0,   228,   229,   230,   214,
     231,   232,   215,     0,   216,   217,     0,   218,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   220,     0,     0,     0,     0,     0,
     233,     0,     0,    84,   477,     0,     0,     0,     0,     0,
     236,   788,    87,   238,   239,   240,   241,     0,     0,     0,
       0,     0,     0,     0,   221,   222,     0,   223,   224,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   225,
     226,   227,     0,   228,   229,   230,     0,   231,   232,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   233,     0,     0,
      84,   477,     0,     0,     0,     0,     0,   236,   237,    87,
     238,   239,   240,   241
};

static const yytype_int16 yycheck[] =
{
       1,   354,    82,     4,     1,     1,   106,   188,    82,    65,
     176,   233,   686,  1116,   924,   314,   147,     3,    82,   151,
       1,   106,  1077,   569,   178,     1,  1363,    82,    82,  1393,
    1394,   138,   328,   176,   624,   655,   328,   657,  1122,    84,
     328,   270,    82,   625,  1344,   370,  1130,  1109,  1806,   631,
     620,    82,   328,   675,   645,   328,   624,   536,   176,   323,
      93,    62,    63,   202,    65,  1344,     4,   624,    65,    65,
     620,     0,   620,   106,     1,   338,   109,  1723,   290,  1914,
     113,    82,   294,     1,    65,   349,   996,    88,   193,    65,
    1806,   236,    93,   620,   514,   898,   620,   328,    82,  1009,
     101,   157,   620,   235,   367,   106,   249,   328,   109,   178,
     620,   328,   113,  1093,  1081,   378,  1806,    93,    82,     1,
     839,   417,    84,    85,   204,   417,    84,    85,   328,   417,
     106,    96,   626,   109,  1101,   745,   630,   113,    68,    69,
     204,   417,   636,   328,   417,   891,   756,    91,   192,   204,
     204,   152,  1297,   142,   155,    97,   157,   108,   337,  1304,
     157,   157,   163,   157,   204,   344,   147,   661,   107,   170,
     203,    79,   666,   204,  1327,   113,   157,   178,   127,   115,
     116,   157,     0,    65,   144,   179,   417,   366,   144,   190,
     144,   801,    79,    79,   127,     1,   417,   186,   377,   268,
     417,   202,   203,   204,   188,  1851,   171,   338,   954,   171,
    2045,   954,   163,   171,   174,   175,   220,   417,   167,   163,
     449,   163,  2048,   179,   188,   179,    96,   203,   818,   329,
     166,   113,   417,     0,   235,   143,   367,  1382,  1300,  1918,
     193,   171,   169,   170,   329,   249,   250,   378,   328,   188,
    2076,   163,   170,   234,   328,   163,   143,   143,   188,    65,
    1006,  2019,    79,  1006,   328,   147,   267,   268,   301,   163,
    2096,   163,   403,   328,   328,   157,     1,    20,   171,     4,
     167,  1027,  1582,   163,  1027,  1585,  1586,   833,   328,   816,
     291,    10,   106,   187,   295,   188,   329,   328,   816,   169,
     301,   171,   456,  2019,   447,   405,   816,   414,   930,   463,
     901,   312,   732,   733,  1398,  1807,   317,   546,   418,   119,
     405,   322,   323,   324,   357,   301,   143,   328,   329,  2019,
       1,   543,   471,   418,  1301,  1201,   132,   417,  1318,   372,
      65,   147,   142,   417,  1309,   567,   163,   392,   349,  1404,
     167,   157,  1098,   329,   169,  1098,   357,   338,   359,   171,
     589,   165,   417,   417,   910,   510,   170,   368,   369,  1514,
     166,   372,  1352,   188,  1719,   957,   188,     1,   379,   969,
       4,   357,   681,     1,   109,   955,   367,   456,   113,   532,
     163,   716,   393,   394,    65,   396,   372,   378,   694,   724,
     401,   969,   694,   882,   405,   955,   694,   955,   828,  1212,
    1902,   397,   969,   442,   532,  2094,   417,   418,   694,   648,
     552,   694,   403,   169,  1143,   101,   846,   428,   955,   405,
     659,   955,   157,   179,   163,   436,   437,   955,   467,  1113,
     441,    65,   418,   162,    82,   955,  2125,    65,   118,  1195,
    1196,   663,  1195,  1196,   173,   456,   338,  1757,  1950,   178,
      98,   494,   163,   694,   465,   446,   730,   468,  1078,    93,
     471,   141,   736,   694,   169,   479,   147,   694,  1757,   117,
       1,   485,   163,     4,   179,   367,   157,   169,    82,   113,
     171,   657,   755,   494,   694,   163,   378,   160,   122,   795,
     994,   644,   645,   795,   188,    99,   188,   795,   171,   694,
    1256,  1856,  1857,  1256,   515,   329,   188,  1601,   494,   795,
    1884,   403,   795,   147,   187,   407,   644,   151,  2020,    79,
    1120,  1441,   338,   157,    82,   667,   160,   119,   643,   157,
    1309,   720,   543,    10,    65,   774,   169,   169,  1610,    97,
     169,  1309,  1517,  1518,  1519,   624,   179,   558,  1180,   560,
     142,   367,   563,   169,   795,  1718,   567,   169,   747,   570,
    1723,   249,   378,   179,   795,   754,   169,   201,   795,   758,
    1446,  1447,  1448,   165,  1929,  1930,   188,   379,  2080,   169,
     169,   405,   113,   143,   598,   795,     1,   403,    93,     4,
     179,   162,   394,   815,   418,   586,   706,   652,   169,   233,
     795,   235,   290,   163,   694,   169,   294,   167,   163,   620,
     165,   706,  1242,   624,   755,    79,  1963,   188,   673,   188,
     694,   137,   138,   619,   188,   621,   157,   157,     3,   694,
     694,  1223,  1224,   169,    76,   689,   647,   372,   168,   169,
     169,   652,   653,   179,   694,   331,   169,   333,   166,   179,
      65,   165,  1765,   694,   340,   160,   170,   338,   169,   188,
     824,     3,   673,   706,   675,   188,   905,   183,   184,   665,
      65,   893,  1766,    68,    69,   671,    71,   188,    93,   143,
     643,   169,   171,   694,   141,   162,   367,   797,  1851,   178,
     169,   171,   169,    79,   690,   706,   173,   378,   178,   163,
     188,   178,   797,   167,   338,   795,   163,   169,  1017,   188,
     167,   674,   865,   169,  1634,   171,  1636,   174,   175,   730,
     706,   355,   403,   357,    79,   736,   412,   413,   739,   363,
     795,   795,   147,   367,   800,   877,   151,   865,  1517,  1518,
    1519,   163,   157,    79,   378,   160,   899,   738,   901,  1517,
    1518,  1519,   171,   169,    79,    79,   169,   143,   187,   447,
      79,   169,    79,   169,   755,  1928,   169,   401,   163,   403,
      79,   899,   188,   407,   165,   188,   167,   163,  1941,  1053,
     188,   167,   188,   837,   795,   188,   797,   171,   143,   800,
     163,   802,   426,   800,   800,   791,  1428,  1093,   171,   164,
     811,  1431,  1432,   163,   185,   178,   171,   143,   163,   800,
     170,   797,   167,   163,   800,   174,    68,  1417,   143,   143,
     235,   832,   181,   182,   143,     1,   143,   163,     4,   465,
     167,   167,   468,  1107,   143,   172,    79,  2000,   163,   163,
     167,   163,   167,   167,   163,   172,   163,   127,   167,   165,
     167,   485,   357,   169,   163,   543,   804,   165,   167,   873,
     165,     1,   114,   755,  1227,   879,   165,   119,   165,  1108,
     122,   170,   124,  1557,   508,   165,   890,   511,   165,   513,
     514,   165,   706,   517,   176,   177,   170,   901,   967,    65,
     969,  1080,   526,   165,   165,   529,   530,   531,    65,   170,
     143,    68,    69,   165,    71,   916,    82,   918,   800,  1244,
     906,   907,    88,   924,   176,   177,   163,    93,   552,   930,
     163,   426,  1554,   338,   167,    65,    53,    54,   165,    56,
     106,   163,   169,   109,   165,    62,     3,   113,   169,   755,
     355,   165,   357,  1252,   955,   169,    13,    14,    15,    16,
      17,   163,   367,   163,     3,  1669,   967,   645,   969,  1123,
    1674,   168,  1392,   378,    13,    14,    15,    16,    17,   109,
     163,   223,   165,   797,   167,   663,   599,   600,   601,  1535,
     485,   157,   141,    79,   800,   996,   401,   163,   403,   952,
     165,    79,  1676,   180,   169,   127,  1172,  1008,  1009,   163,
      79,   165,   178,   167,   163,   175,  1017,   173,   167,  1020,
     532,   426,    79,    79,   163,   174,   175,   157,   167,  1172,
     272,   273,  1318,   275,   185,   277,    79,   203,   204,  1025,
      79,   165,  1160,   667,   141,   169,   144,    13,    14,    15,
      16,    17,  1053,   168,  1172,   166,   167,   143,   166,   170,
     165,   144,   686,   165,   169,   143,  1352,   169,  1021,   235,
     115,   116,   748,   163,   143,   800,  1242,   163,   165,   804,
     485,   167,   169,   163,   755,   163,   143,   143,   169,   167,
    1091,  1092,  1093,   163,   163,    22,  1093,  1093,   167,   163,
     143,   165,   268,   167,   143,    79,  1107,   163,   732,   733,
    1217,   167,  1093,    79,    13,    14,    15,    16,    17,   361,
     163,  1228,  1344,   799,   167,  1389,   804,   163,  1129,   800,
     165,   755,   644,   163,   169,   301,  1243,   815,   169,  1077,
     270,  1246,  1247,   655,   163,   165,   312,   552,   167,   169,
    1772,  1258,  1666,  1667,  1668,   163,   322,   323,   324,   167,
     163,  1162,   328,   329,   167,  1169,  1170,  1171,   163,   143,
     165,  1109,   167,  1177,  1178,   141,   800,   143,   108,  1180,
      79,  1506,   800,   349,   156,   157,   158,   159,   171,   163,
     165,   357,   141,   167,   169,   165,   163,   163,   141,   169,
    1201,   167,   135,   136,   828,   163,   372,   179,   174,   175,
      96,  1093,  1393,  1394,   163,   893,   115,   116,   167,   171,
     163,   162,   846,   901,   167,   174,   175,   393,  1229,   163,
     396,   174,   175,   167,   188,   401,   165,   163,   187,   405,
     169,  1395,   372,   171,   143,  1352,   120,   121,   122,   123,
     124,   417,   418,   877,   165,   130,  1542,   132,   133,   134,
    1546,  1547,   667,  1216,   168,   169,  1432,   166,    13,    14,
      15,    16,    17,  1380,  1560,   171,   406,   165,   165,   800,
     165,   169,   169,   804,   169,   165,   178,  1093,   163,   169,
     456,   166,   167,  1246,  1247,     5,   171,   172,   165,   163,
    1522,   167,   169,    13,    14,    15,    16,    17,   165,   165,
    1296,   187,   169,   169,  1318,   156,   157,   158,   159,   449,
    1321,  1318,  1318,  1324,  1325,  1326,   139,   140,   494,   165,
    1437,  1438,  1333,   169,    79,   165,   165,  1318,   179,   169,
     169,   165,  2081,  1376,  1377,   169,  2085,   188,   165,   515,
     755,  1352,  1077,   865,  1542,  1352,  1352,  1358,  1546,  1306,
    1307,  1308,  1300,  1470,   165,  1472,   168,   169,  1093,    79,
     165,  1352,  1373,   115,   116,  1376,  1377,   543,  1379,   891,
     165,  1488,  1383,   165,  1109,  1681,  1387,   899,  1389,  1681,
     165,   166,   558,  1681,   560,   800,   165,   563,   143,  1077,
    1376,  1377,   174,   175,   570,  1681,   168,   169,  1681,   168,
     169,  1542,   165,  1959,   165,  1546,   546,   606,   607,   608,
     609,   165,  1093,  1688,  1689,  1690,   163,  1428,  1528,    18,
     672,  1109,   171,   143,   168,   169,  1318,   168,   169,  1377,
    1441,   163,   954,  1528,   171,  1446,  1447,  1448,   169,   170,
    1681,   168,   169,   167,   620,   168,   169,   187,   624,   170,
    1681,   168,   169,   169,  1681,   170,  1404,   168,   169,  1093,
    1352,   187,   877,   168,   169,  1093,   168,   169,   168,   169,
     163,  1681,   168,   169,   168,   169,   652,   653,   165,  1113,
      98,    99,   168,   169,  1006,  1377,  1681,   168,   169,   168,
     169,  1606,   165,   156,   157,   158,   159,   673,   165,   675,
     752,   165,  1318,   168,   169,  1027,   169,   165,   648,    13,
      14,    15,    16,    17,   165,  1821,   179,  1528,   694,  1821,
     165,  1532,  1533,  1821,   165,   188,   168,   169,  1645,  1646,
     706,   165,  1166,   169,   170,  1821,  1352,   187,  1821,    84,
      85,   169,   170,  1554,   171,  1588,  1077,  1202,  1203,   610,
     611,  1542,   602,   603,   730,  1546,  1547,    18,  1854,  1669,
     736,   171,  1093,   739,  1674,  1300,   171,  1578,  1579,  1560,
     604,   605,  1520,  1683,  1669,    79,  1098,  1588,  1109,  1674,
    1821,  1689,  1690,  1318,  1585,  1586,   171,   171,  1683,   165,
    1821,   169,    77,  1604,  1821,  1281,   188,  1681,   168,   163,
     168,   168,  1588,    64,    65,    66,    67,    68,    69,    70,
      71,  1821,  1300,    85,    18,   171,   171,  1352,   169,   795,
     169,   797,   188,  1634,   800,  1636,  1821,    13,    14,    15,
      16,    17,    18,   165,   774,   165,   165,  1318,   169,   143,
     144,  1376,  1377,  1606,   165,   165,   165,   787,   165,   165,
    1542,   169,   165,   165,  1546,  1547,   165,   165,  1669,   165,
     800,   168,  1610,  1674,   156,   157,   158,   159,  1560,  1404,
    1681,  1352,  1683,  1195,  1196,  1309,  1950,   169,  1093,   168,
    1691,   168,   168,   162,  1318,   168,   168,   179,   169,  1985,
    1318,   165,  1746,  1884,   165,   165,   188,   165,   165,  1710,
     165,   165,   165,   162,  1528,    22,  1717,   165,   168,   165,
    1344,   165,  1951,  2019,  1824,   165,  1404,  2019,  1352,  2015,
     165,  2019,   165,   165,  1352,   165,  1542,   165,   165,  1824,
    1546,  1547,   165,  2019,  1256,   165,  2019,  1748,  1752,   165,
     916,   162,   918,  1377,  1560,    77,   171,   171,   924,   171,
     165,   187,   171,  2049,   930,   169,   165,   165,  1392,  1707,
     171,  1772,   165,   165,   165,   905,   113,   165,   115,  1300,
     117,   118,   119,   120,   121,   122,   123,   124,  2019,   955,
     165,   165,   165,   171,   165,  1520,   168,  1318,  2019,   169,
     169,   967,  2019,   969,   162,   187,   165,   168,   165,  1914,
     165,  1911,   162,   162,  1309,   163,   163,  1542,   163,  2019,
    1821,  1546,  1547,  1824,   163,  1707,  1911,  1871,   163,   163,
     996,  1352,  1833,  1834,  2019,  1560,   163,   396,   163,  1840,
      14,   170,   170,  1009,    13,    14,    15,    16,    17,    18,
     169,  1017,  1853,   171,  1020,  1669,  1377,   188,   162,   165,
    1674,   187,  1863,  1588,  1865,   165,   165,   170,   170,  1683,
     130,  1542,   162,  1854,   165,  1546,  1547,  1878,   165,  1880,
    1881,  1882,   168,  1404,   168,  1610,   165,  1053,  1015,  1560,
     168,   165,   165,  1517,  1518,  1519,  1520,  1521,  1522,  1523,
     165,   165,   163,   162,  1309,   162,   188,   163,    87,    99,
    1911,   188,  1956,  1318,   162,    97,   188,  1918,  1542,  1431,
    2020,  1922,  1546,  1547,  1542,  1091,  1927,   163,  1546,  1547,
    1916,   163,  1610,  1557,   188,  2020,  1560,   188,  2043,  2019,
    2045,  1107,  1560,   188,   188,  2019,   188,  1352,   165,  1950,
     162,  1952,   171,   162,  1958,   162,   162,   165,  1944,   169,
     165,  1914,   165,  1093,  2019,  2019,    13,    14,    15,    16,
      17,    18,  1854,   165,   165,  1976,   165,   165,  2083,   165,
    2080,  1706,  1707,   168,   168,   165,   188,  1988,  2095,   165,
     143,  1992,   162,   170,     1,  2080,  1162,     4,  1622,  1520,
    2001,  1128,   165,  2110,  1985,   165,  2007,   169,   163,   165,
    1824,   163,   163,   162,  1180,   165,   162,    82,  2019,  2020,
     168,  1542,  1517,  1518,  1519,  1546,  1547,  1522,  1523,   168,
      82,   188,   162,   165,  2015,  1201,   163,  2081,   163,  1560,
     162,  2085,  2086,  2148,  2020,   188,   188,   165,  1854,   165,
     233,  2052,  1676,   165,   167,    82,    82,   188,    65,   179,
     179,   620,    82,  1229,  2017,   188,   170,   188,  2049,   162,
    2114,   188,   162,   162,   179,    82,   164,   179,    85,  2080,
     233,   162,   165,  1707,   170,   113,    93,  1214,  2089,  1610,
    2043,  2135,  2045,  2094,   163,  2139,   169,  1911,   165,   106,
     179,   179,   109,  1985,  2080,   164,   113,  2151,   165,    82,
     168,  1836,  1517,  1518,  1519,  1520,  1521,  1522,  1523,  2120,
     188,   165,  2123,   164,  2125,   170,   165,   162,   162,  1854,
    2083,   163,   188,  2015,   165,   188,  1622,  1542,   188,   535,
     614,  1546,  1547,  2144,   151,   612,   616,   613,  1209,  1339,
     157,  2152,   615,   160,   248,  1560,   163,   164,  1352,  2125,
    2161,  2045,   721,  1546,  1862,   318,  2076,  2049,  1560,   176,
    2034,  1854,  1756,  2016,    13,    14,    15,    16,    17,  1985,
    1948,  1739,  1739,  1854,  2086,  2015,  1707,  2139,  1318,    55,
     122,  1869,  1379,  1911,   201,  2148,   203,   204,    13,    14,
      15,    16,    17,    18,   363,  1974,  2020,  1373,   113,  2015,
    1376,  1377,   117,   118,   119,   120,   121,   122,   123,   124,
    1523,  1400,  1352,  1389,  1771,  1373,   233,   234,   235,  1017,
    1854,   811,  1592,     0,   832,   682,  1854,   838,  1509,   798,
      79,   838,   249,  2049,   838,    -1,  1376,    -1,    -1,   402,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   816,    -1,    -1,
    1985,   268,  1428,   270,    -1,    -1,  2080,    -1,    -1,  1396,
    1948,    -1,    -1,    -1,    -1,  1441,  1403,    -1,    -1,   838,
    1446,  1447,  1448,   290,    -1,    -1,    -1,   294,    -1,    -1,
    2015,   298,    -1,    -1,   301,   854,    -1,    -1,    -1,   858,
      -1,    -1,  1429,    -1,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1985,    -1,   323,   312,    -1,    -1,
      -1,   328,   329,    -1,  2049,   508,    -1,    -1,   511,    -1,
     513,    -1,    -1,  1854,   517,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   349,   526,  2015,    -1,   529,   530,   531,   356,
     357,    -1,    -1,    -1,    -1,   508,    -1,    -1,   511,    -1,
      -1,  1985,  1528,    -1,   517,   372,    -1,  1985,    -1,    -1,
      -1,    -1,    -1,   526,  1869,    -1,    -1,    -1,  2049,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,  1554,    -1,
      -1,  2015,    -1,    -1,    -1,    -1,   955,  2015,   405,    -1,
      -1,   554,    -1,   410,    -1,    -1,    -1,    -1,    -1,   416,
     417,   418,  1542,    -1,   973,    -1,  1546,  1547,   425,   426,
      -1,   980,  1588,    -1,    -1,  2049,    -1,    -1,    -1,    -1,
    1560,  2049,    -1,    -1,    13,    14,    15,    16,    17,   446,
     447,   448,   449,    -1,    -1,    -1,    -1,    79,    -1,  1854,
      13,    -1,    -1,  1948,    -1,    -1,    -1,    -1,  1588,    -1,
      -1,    -1,    -1,    -1,  1985,    -1,   396,    -1,  1634,    -1,
    1636,    -1,    -1,    -1,    -1,  1602,    -1,   484,   485,    -1,
     487,    -1,    -1,    -1,    -1,    -1,    -1,   494,    -1,    -1,
      -1,    -1,    -1,    -1,  2015,    -1,    -1,  1624,    -1,    -1,
      79,   508,    -1,  1669,   511,    -1,   513,   514,  1674,   141,
     517,   143,    -1,    -1,    -1,  1681,    -1,  1683,    -1,   526,
      -1,    -1,   529,   530,   531,   532,    -1,    -1,  2049,    -1,
    1089,   163,    95,    -1,    68,   167,   543,    -1,    -1,   546,
      -1,    -1,   174,   175,    -1,   552,    -1,    -1,    -1,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   141,    -1,   143,    -1,   719,    -1,    18,    -1,
      -1,    -1,    -1,    -1,   727,   109,  1706,    -1,    -1,   586,
    1985,    -1,   589,    -1,   163,    -1,    -1,   121,   167,   123,
    1149,   125,    -1,   746,  1153,   174,   175,    -1,    -1,    -1,
     163,  1728,    -1,    -1,   757,    -1,  1772,    -1,    -1,    -1,
    2015,    -1,    -1,   620,    -1,    -1,    -1,   624,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,   644,   645,    -1,
      -1,  1768,    -1,    -1,  2049,    -1,    -1,    -1,   655,    -1,
     657,    -1,   659,    -1,    -1,  1821,   663,    -1,  1824,    -1,
     667,    -1,     3,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,    -1,    -1,   673,   686,
     675,    -1,    -1,    -1,    -1,    -1,   113,   694,    -1,   223,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   706,
      -1,    -1,    -1,    -1,    82,    -1,  1836,    -1,    -1,    -1,
    1269,    -1,    -1,    -1,  1273,    -1,    -1,   167,  1277,    -1,
      -1,    -1,    -1,   730,  1854,   732,   733,    -1,   106,   736,
      -1,   738,    -1,    -1,    -1,    -1,   270,    -1,    -1,    -1,
     167,    -1,   276,    -1,   278,  1911,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    -1,  1885,    -1,
      -1,    -1,  1889,    -1,    -1,    -1,    -1,   301,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,  1950,   163,  1952,    -1,   795,    -1,
     797,   721,    -1,   800,    -1,    -1,    -1,   804,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   815,   816,
      -1,   818,    -1,    -1,    72,    -1,    -1,    -1,    85,    -1,
      -1,   828,   163,   164,    -1,    -1,    -1,    -1,    -1,   170,
      -1,   838,   839,   174,   175,  2001,    -1,    -1,   372,   846,
     374,   375,    -1,    -1,    -1,   186,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2019,  2020,  1985,   113,    -1,   865,    -1,
     117,   118,   119,   120,   121,   122,   123,   124,   798,    -1,
     877,    -1,   406,    -1,    -1,    -1,   410,    -1,    -1,    -1,
      -1,    -1,   416,    -1,   891,  2015,   893,    -1,    -1,    -1,
     268,    -1,   899,    -1,   901,    -1,    -1,    -1,  1457,    -1,
      13,    -1,  1461,    -1,    -1,  1464,    -1,   164,   838,  1062,
     167,    -1,    -1,    -1,  2080,   449,    -1,   451,   452,  2049,
      -1,   916,  1075,    -1,   854,    -1,  1079,    -1,   858,   924,
    1083,    -1,  1491,    -1,    79,   930,  1495,    -1,    -1,    -1,
    1499,    -1,    -1,    -1,    -1,   323,    -1,   954,   955,    -1,
     328,   329,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     494,    -1,   969,    -1,    -1,    -1,    -1,   234,   113,    -1,
      -1,   349,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    95,  1166,    -1,    -1,  2152,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2161,   141,    -1,   143,  1006,
     113,   996,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   546,    -1,  1009,    -1,    -1,    -1,   163,   164,
    1027,    -1,  1017,    -1,    64,  1020,    -1,   405,    -1,   174,
     175,   298,    72,    73,    74,    75,    -1,    -1,    -1,   417,
     418,   186,    -1,   973,   339,    -1,  1053,    -1,    -1,    -1,
     980,    -1,  1611,    -1,    -1,   589,    -1,   591,   592,    -1,
      -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1077,    -1,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,  1093,    -1,    -1,   356,
      -1,  1098,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1107,  1108,  1109,    -1,    -1,    85,  1113,    -1,    -1,    -1,
      -1,    -1,   113,  1120,   648,   649,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   659,    -1,   167,   129,   160,
     131,    -1,    -1,    -1,    -1,    -1,  1143,    -1,   672,    -1,
      -1,    -1,    -1,    -1,    -1,   185,    -1,    -1,    -1,    -1,
    1303,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   425,  1166,
      -1,  1344,    -1,   164,  1317,  1172,   167,    -1,    -1,    -1,
     201,    -1,   706,    13,    14,    15,    16,    17,    -1,   446,
      -1,    -1,    -1,    -1,  1337,  1180,    -1,    -1,  1195,  1196,
      -1,  1344,   487,    -1,    -1,    -1,   730,    -1,    -1,    -1,
      -1,   735,    -1,   737,    -1,    -1,  1201,    -1,   503,    -1,
      -1,   506,    -1,    -1,    -1,    -1,    -1,   484,   249,  1149,
      -1,    -1,    -1,  1153,    -1,    -1,   760,    -1,   762,   763,
      -1,    -1,    -1,    -1,    -1,  1242,    -1,    -1,    -1,    79,
     774,    -1,   620,    -1,    -1,    -1,   624,    -1,    -1,  1256,
      -1,    -1,    -1,   787,   234,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   797,    -1,    -1,    -1,    -1,    -1,   249,
      -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,    -1,
      -1,   271,    -1,  1300,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,  1309,   143,    -1,    -1,    -1,    -1,    -1,    -1,
     290,  1318,    -1,    -1,   294,    -1,   694,    -1,   298,   586,
      -1,    -1,    -1,   163,   164,    -1,   357,   167,   706,    85,
      -1,    -1,    -1,    -1,   174,   175,    -1,  1344,  1345,  1269,
      -1,    -1,    -1,  1273,    -1,  1352,   186,  1277,    -1,    -1,
      -1,    -1,   730,    -1,    -1,    -1,    -1,   113,   736,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,  1376,
    1377,   905,    -1,    -1,    -1,    -1,   356,    -1,    -1,    -1,
      -1,    -1,  1389,  1536,  1537,  1392,    -1,    -1,   655,    -1,
     657,    -1,    -1,    -1,    -1,   426,    -1,  1404,    -1,    -1,
      -1,    -1,    -1,    -1,   699,    -1,    -1,    -1,    -1,    -1,
    1417,    -1,    -1,    -1,    -1,    -1,   447,   795,    -1,   797,
      -1,    -1,    -1,    -1,  1431,  1432,    -1,    -1,    -1,    -1,
      -1,    -1,   188,  1428,  1587,   969,    -1,    -1,    -1,  1622,
      -1,    -1,    -1,    -1,    -1,   425,  1441,    -1,    -1,    -1,
      -1,  1446,  1447,  1448,   485,    -1,    -1,    -1,    -1,    -1,
      -1,   995,    -1,    -1,    -1,    -1,   446,   447,    -1,    -1,
      -1,   738,     1,    -1,    -1,     4,    -1,   508,  2037,    -1,
      -1,    -1,    -1,   514,    -1,   113,   517,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,    -1,    -1,
    1517,  1518,  1519,  1520,    -1,  1522,  1523,    -1,    -1,    -1,
      -1,  1528,  1529,    -1,    -1,    -1,    -1,  1457,    -1,    -1,
      -1,  1461,    -1,    -1,  1464,  1542,    65,    -1,    -1,  1546,
    1547,    68,    -1,   838,   839,    -1,    -1,    -1,    -1,    -1,
    1557,    -1,   532,  1560,   849,    -1,    85,   852,    -1,  1554,
     188,  1491,    -1,   543,    93,  1495,    -1,   547,    -1,  1499,
      -1,    -1,    -1,    -1,  1108,    -1,    -1,   955,    -1,    -1,
     109,  1588,   109,    -1,   113,  1738,  1120,    -1,    -1,   967,
      -1,   969,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1610,    -1,    -1,   586,    -1,    -1,  1143,
      -1,    -1,    -1,    -1,   645,  1622,    -1,    -1,   147,    -1,
      -1,    -1,   151,    -1,   891,    -1,    -1,    -1,   157,    -1,
      -1,   160,    -1,    -1,    -1,   164,    -1,   164,    -1,  1634,
      -1,  1636,    -1,    -1,    -1,    -1,   175,   176,    -1,   178,
      -1,    -1,    -1,    -1,    -1,   686,    -1,    -1,    -1,    -1,
      -1,    -1,  1669,  1670,   644,   645,    -1,  1674,    -1,  1676,
      -1,    -1,   201,    -1,  1681,  1053,  1683,    -1,    -1,    -1,
      -1,  1611,    -1,   663,    -1,    -1,    -1,   954,    -1,    -1,
      -1,    -1,    -1,    -1,  1228,    -1,   223,    -1,    -1,  1706,
    1707,   732,   733,    -1,   233,   234,   235,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1033,  1006,
      -1,   270,   271,   270,    -1,  1040,    -1,    -1,    79,  1044,
      -1,    -1,    -1,  1048,    -1,    -1,    -1,    -1,   738,    -1,
    1027,   290,    -1,    -1,    -1,   294,    -1,    -1,    -1,   298,
      -1,    -1,   301,    -1,   301,    -1,    -1,  1772,    -1,    -1,
      -1,    -1,   113,   188,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,   828,    -1,  1806,
    1807,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   338,
      -1,    -1,   143,    -1,  1821,   846,    -1,  1824,    -1,    -1,
      -1,    -1,  1089,    -1,   804,    -1,   355,   356,   357,  1836,
      -1,  1098,   163,   164,    -1,   815,    -1,    -1,   367,    -1,
      -1,    -1,  1376,   372,    -1,   372,    -1,  1854,  1143,   378,
      -1,  1229,    -1,    -1,    -1,   186,    -1,    -1,    -1,    -1,
      -1,    -1,  1869,    -1,   393,    -1,    -1,   396,    -1,    -1,
     901,    -1,   401,    -1,   403,    -1,    -1,   406,   407,   406,
      -1,   410,    -1,   410,    -1,   865,    -1,   416,    -1,   416,
      -1,    -1,    -1,    -1,    -1,  1902,   425,   426,    -1,    -1,
      -1,   113,    -1,    -1,  1911,   117,   118,   119,   120,   121,
     122,   123,   124,   893,    -1,    -1,    -1,   446,   447,   899,
     449,   901,   449,    -1,    -1,  1220,  2079,    -1,  1195,  1196,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,  1948,    -1,  1950,  1951,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,    -1,   484,   485,    -1,  2111,    -1,
      -1,    -1,    -1,    -1,    -1,  1260,    -1,  1974,  1263,    -1,
      -1,    -1,  1267,    -1,   186,  1242,    -1,    -1,  1985,   508,
      -1,    -1,   511,    -1,   513,   514,   515,    -1,   517,  1256,
      -1,    79,    -1,    -1,    -1,  1529,    -1,   526,    -1,    -1,
     529,   530,   531,   532,    -1,    -1,    -1,    -1,  2015,    -1,
      -1,  1389,  2019,  2020,   543,    -1,    -1,   546,   547,   546,
      -1,    -1,    -1,   552,    -1,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,   113,  2049,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   141,  1588,   143,    -1,   586,    -1,    -1,
     589,   113,   589,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,  2080,    -1,   163,   164,    -1,  1345,   167,
      -1,    -1,  1113,    -1,    -1,    -1,   174,   175,    -1,    -1,
      -1,   620,    -1,    -1,   166,    -1,    -1,  1077,   186,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   178,  2037,    -1,    -1,
      -1,   163,    -1,    -1,  1409,   644,   645,    -1,    -1,   648,
      -1,   648,     1,    -1,   653,     4,   655,    -1,   657,  1109,
     659,    -1,   659,    -1,   663,    -1,    -1,    -1,   667,    -1,
      -1,    -1,    -1,    -1,    -1,   672,    -1,    -1,    -1,    -1,
    1528,    -1,    -1,    -1,    -1,    -1,    -1,   686,    -1,    -1,
      -1,    -1,    -1,    -1,  1431,  1432,    -1,    -1,    -1,    -1,
      -1,   113,  1706,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   712,    -1,   712,    65,    -1,    -1,    -1,
      -1,   113,   721,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   732,   733,    -1,    -1,    -1,   737,   738,
     737,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,   755,    -1,    -1,    -1,
      -1,    -1,    -1,   760,   113,    -1,   178,    -1,    -1,    -1,
      -1,   163,   164,    -1,    -1,   774,    -1,   774,   170,    -1,
      -1,    -1,   174,   175,    -1,    -1,    -1,    -1,   787,    -1,
     787,    -1,    -1,    -1,   186,    -1,    -1,    -1,   147,   798,
      -1,   800,   151,    -1,    -1,   804,    -1,    -1,   157,    -1,
      -1,   160,    -1,    -1,    -1,    -1,   815,   816,    -1,    -1,
    1824,  1669,    -1,    -1,    -1,    -1,  1674,    -1,    -1,   828,
      -1,    -1,  1836,  1681,    -1,  1683,    -1,    -1,    -1,   838,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   846,    -1,    -1,
    1300,    -1,   201,    -1,    -1,   854,    -1,    -1,    -1,   858,
      13,    14,    15,    16,    17,   113,   865,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,   877,    -1,
      -1,    -1,    -1,    -1,   233,    -1,   235,    -1,    -1,    -1,
      -1,  1392,   891,    -1,   893,  1345,    -1,    -1,  1902,    -1,
     899,    -1,   901,    -1,    -1,  1670,   905,    -1,   905,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   171,    -1,    -1,    79,    -1,    -1,  1933,
    1934,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,  1951,    -1,    -1,
      -1,    -1,    -1,    -1,  1404,   954,   955,    -1,    -1,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   174,  1821,   973,    -1,  1824,    -1,    -1,    -1,
      -1,   980,    -1,   163,    -1,    -1,   166,   167,   141,   338,
     143,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,   355,  1006,   357,    -1,
     163,   164,    -1,    -1,   363,    -1,    -1,    -1,   367,    -1,
      -1,   174,   175,    -1,    -1,    -1,    -1,    -1,  1027,   378,
      -1,    -1,    -1,   186,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1806,  1807,   164,    -1,    -1,   167,    -1,    -1,    -1,
      -1,    -1,   401,    -1,   403,    -1,  1557,    -1,   407,    -1,
      -1,    -1,   113,  1911,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,  2080,   426,  1077,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1089,    -1,    -1,    -1,  1093,    -1,    -1,    -1,    -1,  1098,
      -1,    -1,  1950,    -1,    -1,    -1,    -1,    -1,    -1,  1108,
    1109,  1108,    -1,    -1,  1113,    -1,   167,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,  1125,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   485,  1902,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
    1149,    -1,    -1,    -1,  1153,    -1,    -1,    -1,    -1,   508,
    1610,  1160,   511,    -1,   513,   514,   141,  1166,   517,    -1,
      -1,  2019,  2020,  1172,    -1,  1676,    -1,   526,    -1,    -1,
     529,   530,   531,    -1,    79,  1950,    -1,    -1,   163,   164,
      -1,    -1,   167,    -1,    -1,    -1,  1195,  1196,    -1,   174,
     175,    -1,    -1,   552,    -1,  1970,    -1,    -1,    -1,  1974,
      -1,   186,   187,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,  2080,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,  1242,    -1,     4,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,  2019,  2020,    -1,  1256,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,   163,   164,
    1269,    -1,   167,    -1,  1273,    -1,    -1,   113,  1277,   174,
     175,   117,   118,   119,   120,   121,   122,   123,   124,   125,
    1289,   186,  1289,   129,    -1,   131,    -1,    -1,    -1,    -1,
      -1,  1300,    -1,    -1,    -1,    -1,    65,    -1,    -1,    -1,
    1309,    -1,    -1,    -1,    -1,  2080,    -1,    -1,   667,  1318,
      13,    14,    15,    16,    17,    -1,    85,    79,   164,    -1,
      -1,   167,    -1,    -1,    93,    -1,    -1,   686,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1344,  1345,    -1,    -1,    -1,
      -1,    -1,    -1,  1352,   113,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,  1376,  1377,  1376,
      -1,    -1,    -1,   732,   733,    -1,    79,    -1,   147,   141,
      -1,   143,   151,  1392,    -1,    -1,    -1,    -1,   157,    -1,
      -1,    -1,    -1,    -1,    -1,  1404,   755,    -1,    -1,    -1,
      -1,   163,   164,    -1,    -1,    -1,    -1,   176,    -1,  1869,
     113,    -1,   174,   175,   117,   118,   119,   120,   121,   122,
     123,   124,  1431,  1432,   186,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   203,    -1,    -1,    -1,   141,    -1,
     143,   800,    -1,    -1,    -1,    -1,    -1,    -1,  1457,    -1,
      -1,    -1,  1461,    -1,    -1,  1464,    -1,    -1,    -1,    -1,
     163,   164,    -1,    -1,    -1,   234,   235,    -1,    -1,   828,
      -1,   174,   175,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     249,    -1,  1491,   186,    -1,    -1,  1495,   846,  1948,   113,
    1499,    -1,    -1,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   271,    62,    63,   129,    -1,   131,  1517,  1518,
    1519,  1520,  1521,  1522,  1523,    -1,    -1,    -1,   877,    -1,
    1529,   290,  1529,    -1,    -1,   294,    -1,    -1,    -1,   298,
      -1,    -1,    -1,  1542,    -1,    -1,    -1,  1546,  1547,    -1,
     164,    -1,   101,   167,    -1,    -1,    -1,    -1,  1557,    -1,
      -1,  1560,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     329,    -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,   338,
     117,   118,   119,   120,   121,   122,   123,   124,   125,  1588,
     349,  1588,   129,    -1,   131,   354,   355,   356,   357,    -1,
      -1,    -1,    -1,   152,  1603,    -1,   155,    -1,   367,    -1,
      -1,  1610,  1611,    -1,    -1,    -1,    -1,    -1,    -1,   378,
      -1,   170,   381,  1622,    -1,    -1,   385,   164,    -1,    -1,
      -1,   390,    -1,    -1,    -1,    -1,    -1,   396,    -1,    -1,
      -1,   190,   401,    -1,   403,    -1,    -1,    -1,   407,    -1,
      -1,    -1,    -1,   202,    -1,    -1,    -1,    -1,    -1,   418,
      -1,    -1,    -1,    -1,    -1,   113,   425,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,  1676,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   446,   447,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,  1706,  1707,  1706,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   267,    -1,
      -1,    -1,    -1,    -1,   113,   484,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,
      -1,    -1,   291,    -1,  1093,    -1,   295,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,  1113,    -1,    -1,    -1,   317,    -1,
      -1,    -1,    -1,   532,   163,   164,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   543,   174,   175,    -1,   547,    -1,
      -1,    -1,    -1,   552,    -1,    -1,   113,   186,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
     359,    -1,    -1,    -1,    -1,    -1,    -1,  1166,    -1,   368,
     369,    -1,    -1,    -1,   141,    -1,   143,   586,    -1,    -1,
     379,    -1,    -1,    -1,    -1,    -1,    -1,  1836,    -1,  1836,
      -1,    -1,    -1,    -1,    -1,   394,   163,   164,    -1,    -1,
      -1,    -1,  1851,    -1,    -1,  1854,    -1,   174,   175,    -1,
      -1,   620,    -1,    -1,    -1,   624,    -1,    -1,    -1,   186,
    1869,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   428,
      -1,    -1,    -1,    -1,    -1,   644,   645,   436,   437,    -1,
      -1,   650,   441,    -1,    -1,    -1,   655,    -1,   657,    -1,
      -1,    -1,    -1,    79,   663,    -1,    -1,    -1,   667,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   465,    -1,    -1,   468,
      -1,   113,   471,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,  1932,  1933,    -1,    -1,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,  1948,
      -1,    -1,  1951,    -1,  1951,    -1,    -1,    -1,    -1,    -1,
    1309,    -1,   721,    -1,    -1,   141,    -1,   143,    -1,  1318,
      -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,   738,
      -1,    -1,    -1,    -1,    -1,    -1,  1985,   163,   164,    -1,
      -1,    -1,    -1,    -1,    -1,  1344,   755,    -1,   174,   175,
      -1,    -1,  2001,  1352,    -1,    -1,    -1,    -1,    -1,     1,
     186,    -1,    -1,    -1,    -1,    -1,  2015,    -1,   567,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,  1377,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2037,   798,
      -1,   800,    -1,  1392,    -1,   804,    -1,    -1,    -1,    -1,
    2049,    -1,    -1,    -1,    -1,    -1,   815,   816,   113,   818,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    65,    -1,    -1,    -1,    -1,    -1,   838,
     839,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    85,    -1,   854,    -1,    -1,   647,   858,
      -1,    -1,    -1,    -1,    -1,    -1,   865,    -1,   163,   164,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   877,   174,
     175,   113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   186,   891,    -1,   893,    -1,    -1,    -1,    -1,    -1,
     899,   113,   901,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,   147,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,  1517,  1518,
    1519,  1520,  1521,  1522,  1523,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   176,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1542,   166,   954,   955,  1546,  1547,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   966,  1557,    -1,
      -1,  1560,    -1,    -1,   973,    -1,    -1,    -1,    -1,    -1,
     113,   980,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   234,    -1,    -1,    -1,    -1,  1006,   141,    -1,
      -1,    -1,    -1,   802,    -1,    -1,    -1,   249,    -1,    -1,
      -1,    -1,   811,    -1,    -1,    -1,    -1,    -1,  1027,    -1,
     163,   164,    -1,  1622,   167,    -1,    -1,    -1,    -1,   271,
      -1,   174,   175,   832,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   186,    -1,    -1,    -1,    -1,   290,    -1,
      -1,    -1,   294,    -1,    -1,    -1,   298,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,  1077,    -1,
      -1,    -1,   160,    -1,    -1,    -1,    -1,  1676,    -1,    -1,
    1089,    -1,    -1,     1,  1093,    -1,    -1,    -1,    -1,  1098,
      -1,    -1,    -1,    -1,    -1,    -1,   338,    -1,    -1,   187,
    1109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1707,    -1,
      -1,    -1,    -1,    -1,   356,    -1,    -1,    -1,    -1,   108,
      -1,    -1,    -1,    -1,   113,   367,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   378,    -1,    -1,   381,
    1149,    -1,    -1,    -1,  1153,    -1,    -1,    65,    -1,    -1,
      -1,  1160,    -1,    -1,   396,    -1,    -1,    -1,    -1,    -1,
      -1,   403,    -1,  1172,    -1,   407,    -1,    85,    -1,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,   425,    -1,    -1,  1195,  1196,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,   446,   447,    -1,    -1,    -1,  1008,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1227,    -1,
     163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   147,
      -1,   174,   175,  1242,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   484,   186,    -1,    -1,    -1,  1256,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1854,    -1,    -1,   176,    -1,
    1269,    -1,    -1,    -1,  1273,    -1,    -1,   113,  1277,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     532,  1300,    -1,  1092,    -1,   141,    -1,    -1,    -1,    -1,
    1309,   543,    -1,    -1,    -1,   547,    -1,    -1,    -1,  1318,
      -1,    -1,    -1,    -1,    -1,    -1,   234,   163,   164,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,
    1129,   249,    -1,    -1,    -1,    -1,  1345,    -1,    -1,    -1,
     186,    -1,    -1,  1352,   586,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,   271,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,  1377,    -1,
      -1,    -1,   290,    -1,    -1,    -1,   294,    -1,   620,    -1,
     298,    -1,    -1,    -1,    -1,    -1,  1985,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1404,    -1,    -1,    -1,    -1,
      -1,    -1,   644,   645,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   655,    -1,   657,  2015,    -1,    -1,    -1,
     338,   663,  1431,  1432,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   356,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1457,   367,
    2049,    -1,  1461,    -1,    -1,  1464,    -1,    -1,    -1,     1,
     378,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   396,   721,
      -1,    -1,  1491,    -1,    -1,   403,  1495,    -1,    -1,   407,
    1499,    -1,    -1,    -1,    -1,    -1,   738,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   425,  1517,  1518,
    1519,  1520,  1521,   755,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    65,    -1,  1324,  1325,  1326,   446,   447,
      -1,    -1,    -1,  1542,    -1,    -1,    -1,  1546,  1547,    -1,
      -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1560,    -1,    -1,    -1,    -1,   798,    -1,   800,  1358,
      -1,    -1,   804,    -1,    -1,    -1,   484,   109,    -1,    -1,
      -1,   113,    -1,   815,   816,    -1,    -1,    -1,    -1,    -1,
    1379,    -1,    -1,    -1,  1383,    -1,    -1,    -1,  1387,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   838,    -1,    -1,    -1,
      -1,  1610,  1611,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   854,    -1,   532,   157,   858,    -1,    -1,    -1,
      -1,    -1,   164,   865,    -1,   543,    -1,    -1,    -1,   547,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   891,
      -1,   893,    -1,    -1,    -1,    -1,    -1,   899,    -1,   901,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   586,    -1,
      -1,    -1,    -1,    -1,  1683,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   234,    -1,    -1,    -1,    -1,    -1,  1707,    -1,
      -1,    -1,   620,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   954,   955,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   644,   645,    -1,    -1,
      -1,   973,    -1,  1532,  1533,    -1,    -1,   655,   980,   657,
      -1,    -1,    -1,    -1,    -1,   663,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   298,    -1,    -1,    -1,
      -1,    -1,    -1,     1,  1006,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1027,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1807,    -1,
      -1,    -1,    -1,   721,    -1,  1604,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   356,    -1,    -1,    -1,    -1,    -1,
     738,    -1,    -1,    -1,    -1,    -1,    -1,    65,    -1,    -1,
     372,    -1,    -1,    -1,  1321,  1077,    -1,   755,    -1,    -1,
      -1,    -1,  1851,    -1,    -1,  1854,  1333,  1089,    -1,    -1,
      -1,  1093,    -1,    -1,    -1,    93,  1098,    -1,    -1,    -1,
    1869,    -1,    -1,    -1,    -1,    -1,    -1,  1109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,    -1,    -1,
     798,    -1,   800,   425,    -1,    -1,   804,    -1,    -1,    -1,
      -1,    -1,  1691,    -1,    -1,    -1,    -1,   815,   816,    -1,
      -1,    -1,    -1,    -1,   446,    -1,    -1,  1149,    -1,   147,
      -1,  1153,    -1,   151,    -1,    -1,    -1,    -1,  1160,   157,
     838,    -1,   160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1172,    -1,    -1,    -1,    -1,    -1,   854,    -1,    -1,  1948,
     858,    -1,   484,    -1,    -1,    -1,    -1,   865,    -1,    -1,
      -1,    -1,    -1,  1195,  1196,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   891,    -1,   893,  1985,    -1,    -1,    -1,
      -1,   899,    -1,   901,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   233,    -1,   235,    -1,    -1,
    1242,    -1,    -1,    -1,    -1,    -1,  2015,    -1,    -1,    -1,
      -1,  2020,    -1,    -1,  1256,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1269,  2037,    -1,
      -1,  1273,    -1,    -1,    -1,  1277,   954,   955,    -1,    -1,
    2049,    -1,    -1,    -1,   586,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   973,    -1,    -1,  1300,    -1,
      -1,    -1,   980,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1318,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1006,    -1,
      -1,  1578,  1579,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     338,    -1,    -1,  1345,    -1,    -1,    -1,    -1,    -1,  1027,
    1352,    -1,    -1,   655,    -1,   657,    -1,   355,    -1,   357,
      -1,    -1,    -1,    -1,    -1,   363,    -1,    -1,    -1,   367,
      -1,    -1,    -1,    -1,    -1,  1377,    -1,    -1,    -1,    -1,
     378,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1077,
      -1,    -1,  1404,   401,    -1,   403,    -1,    -1,    -1,   407,
      -1,  1089,    -1,    -1,    -1,  1093,    -1,    -1,    -1,    -1,
    1098,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   426,  1431,
    1432,  1109,    -1,    -1,    -1,    -1,   738,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1457,    -1,    -1,    -1,  1461,
      -1,    -1,  1464,  1710,    -1,    -1,    -1,    -1,    -1,    -1,
    1717,  1149,    -1,    -1,    -1,  1153,    -1,    -1,    -1,    -1,
      -1,    -1,  1160,    -1,    -1,    -1,    -1,   485,    -1,  1491,
      -1,    -1,    -1,  1495,  1172,    -1,    -1,  1499,   800,    -1,
      -1,  1748,   804,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     508,    -1,    -1,   511,    -1,   513,   514,  1195,  1196,   517,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   526,    -1,
      -1,   529,   530,   531,    -1,    -1,    -1,    -1,    -1,    -1,
    1542,    -1,    -1,    -1,  1546,  1547,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   552,    -1,    -1,    -1,  1560,    -1,
      -1,    -1,    -1,    -1,  1242,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1256,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1833,  1834,    -1,   891,
      -1,  1269,    -1,  1840,    -1,  1273,    -1,    -1,    -1,  1277,
      -1,    -1,    -1,    -1,    -1,    -1,  1853,    -1,  1610,  1611,
      -1,    -1,    -1,    -1,    -1,    -1,  1863,    -1,  1865,    -1,
      -1,    -1,  1300,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1878,    -1,  1880,  1881,  1882,    -1,    -1,    -1,    -1,
    1318,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   954,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1345,    -1,   667,
      -1,  1918,    -1,    -1,  1352,  1922,    -1,    -1,    -1,    -1,
    1927,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   686,    -1,
      -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1377,
      -1,    -1,    -1,    -1,  1006,  1707,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1027,  1404,    -1,    -1,  1976,
      -1,    -1,    -1,    -1,   732,   733,    -1,    -1,    -1,    -1,
      -1,  1988,    -1,    -1,    -1,  1992,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1431,  1432,    -1,    -1,   755,    -1,    -1,
    2007,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1077,    -1,    -1,    -1,  1457,
      -1,    -1,   176,  1461,    -1,    -1,  1464,    -1,    -1,    -1,
      -1,  1093,    -1,    -1,    -1,    -1,  1098,    -1,    -1,    -1,
      -1,    -1,   800,    -1,    -1,  2052,  1108,  1109,    -1,    -1,
      -1,    -1,    -1,  1491,    -1,    -1,    -1,  1495,    -1,    -1,
      -1,  1499,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     828,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     234,   235,  2089,    -1,    -1,    -1,    -1,  2094,   846,    -1,
      -1,    -1,  1854,    -1,    -1,   249,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1542,    -1,    -1,  1869,  1546,  1547,
      -1,    -1,    -1,  2120,   268,    -1,  2123,    -1,  2125,   877,
      -1,    -1,  1560,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1195,  1196,    -1,    -1,  2144,    -1,    -1,
     294,    -1,    -1,    -1,   298,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1610,  1611,    -1,    -1,    -1,    -1,   332,    -1,
    1242,    -1,    -1,    -1,    -1,   339,  1948,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1256,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   356,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1985,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1300,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2015,    -1,    -1,  1318,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   420,    -1,    -1,  1707,
      -1,   425,    -1,    -1,    -1,  2037,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1345,    -1,    -1,    -1,  2049,    -1,    -1,
    1352,    -1,   446,   447,   448,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1376,  1377,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     484,    -1,    -1,   487,    -1,  1093,    -1,    -1,    -1,    -1,
      -1,    -1,  1404,    -1,    -1,    -1,    -1,    -1,    -1,   503,
     504,    -1,   506,   507,    -1,  1113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1431,
    1432,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   532,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   543,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1166,    -1,
      -1,    -1,    -1,  1851,    -1,    -1,  1854,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1869,   586,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1520,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   620,    -1,    -1,    -1,
     624,   625,    -1,    -1,    -1,    -1,    -1,   631,    -1,    -1,
    1542,    -1,    -1,    -1,  1546,  1547,    -1,   641,    -1,    -1,
     644,   645,    -1,    -1,    -1,    -1,    -1,    -1,  1560,    -1,
      -1,   655,    -1,   657,    -1,    54,    -1,    -1,    -1,    -1,
    1948,    -1,    -1,   667,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1588,    -1,    -1,    -1,
      -1,    -1,   233,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   695,    -1,    -1,    -1,   699,    -1,  1985,  1610,    -1,
      -1,  1309,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1318,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2015,    -1,    -1,
      -1,    -1,   131,    -1,   738,    -1,  1344,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1352,   144,    -1,   146,    -1,  2037,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2049,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1377,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,    -1,
     179,    -1,    -1,    -1,  1392,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   796,    -1,  1706,  1707,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   204,    -1,    -1,    -1,    -1,
      -1,   815,   816,    -1,   818,    -1,    -1,    -1,    -1,    -1,
     219,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   838,   839,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   849,   850,    -1,   852,   853,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   865,    -1,    -1,    -1,   193,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   877,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   282,    54,    -1,   214,   891,   216,    -1,
      -1,    -1,   220,   221,    -1,   899,    -1,   901,    -1,    -1,
      -1,    -1,    -1,   231,   232,    -1,    -1,    -1,    -1,  1517,
    1518,  1519,  1520,  1521,  1522,  1523,    -1,    -1,    -1,    -1,
      -1,   249,   250,    -1,  1836,    -1,    -1,    -1,    -1,   328,
      -1,    -1,    -1,   332,  1542,    -1,   335,   336,  1546,  1547,
     339,    -1,  1854,   342,   343,    -1,   345,    -1,   347,  1557,
     954,   955,  1560,   957,    -1,    -1,    -1,   508,    -1,    -1,
     511,   131,    -1,    -1,    -1,   969,   517,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   144,   526,   146,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1006,   554,    -1,    -1,    -1,   177,    -1,    -1,
      -1,    -1,    -1,    -1,  1622,    -1,    -1,    -1,   417,    -1,
      -1,   420,    -1,  1027,    -1,  1029,    -1,    -1,    -1,  1033,
      -1,    -1,    -1,    -1,    -1,    -1,  1040,  1041,    -1,    -1,
    1044,  1045,    -1,    -1,  1048,  1049,    -1,    -1,    -1,   219,
      -1,  1055,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   617,    -1,  1676,    -1,
      -1,    -1,    -1,  1985,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1098,  1099,    -1,    -1,    -1,  1707,
      -1,    -1,    -1,  2015,   503,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   282,    -1,    -1,    -1,  1120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2049,    -1,  1143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   479,    -1,    -1,    -1,    -1,    -1,   485,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   335,   336,    -1,  1172,   339,
      -1,    -1,   342,   343,    -1,   345,    -1,   347,    -1,    -1,
      -1,   732,   733,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1195,  1196,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1220,  1221,    -1,  1223,
    1224,  1225,    -1,    -1,    -1,    -1,   625,    -1,    -1,   628,
     629,    -1,   631,    -1,   633,   634,    -1,    -1,  1242,   638,
     639,    -1,    -1,    -1,    -1,    -1,  1854,    -1,    -1,    -1,
      -1,    -1,  1256,    -1,    -1,    -1,  1260,  1261,    -1,  1263,
    1264,    -1,    -1,  1267,  1268,    -1,    -1,    -1,    -1,    -1,
     598,   599,   600,   601,   602,   603,   604,   605,   606,   607,
     608,   609,   610,   611,   612,   613,   614,   615,   616,    -1,
      -1,    -1,    -1,    -1,    -1,   694,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   710,    -1,    -1,    -1,   643,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   503,    -1,    13,    14,    15,    16,    17,
      -1,  1345,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,  1985,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   795,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,  1409,  1410,  2015,    -1,    -1,
      -1,    -1,    -1,  1417,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   821,    -1,   823,    -1,    -1,  1431,  1432,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2049,    -1,   842,   843,    -1,    -1,    -1,    -1,    -1,
     849,    -1,    -1,    -1,    -1,   625,    -1,    -1,   628,   629,
      -1,   631,    -1,   633,   634,   143,    -1,    -1,   638,   639,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,
      -1,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
     710,    -1,    -1,    -1,    -1,   873,    -1,    -1,    -1,    -1,
      -1,   879,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   890,    -1,    -1,    -1,    -1,    -1,   967,    -1,
      -1,    -1,    -1,   901,    -1,    -1,     1,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,   952,    -1,    62,    -1,    -1,    -1,
      55,   165,    -1,    58,    -1,    60,    61,    -1,    63,    -1,
      -1,  1040,    -1,    79,    -1,  1044,    -1,    -1,    -1,  1048,
      -1,   821,    85,   823,   188,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1670,    -1,    -1,    -1,
      -1,    -1,   842,   843,    -1,    -1,    -1,    -1,    -1,   849,
      -1,    -1,    -1,  1687,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,   143,   133,   134,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
    1119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1285,    -1,    -1,    -1,   163,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   201,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1344,    -1,    -1,    -1,   967,    -1,    -1,
      -1,   234,  1806,  1807,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1822,    -1,
      -1,  1220,    -1,    -1,  1223,  1224,    -1,    -1,    -1,    -1,
      -1,    -1,  1231,  1232,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1169,  1170,  1171,    -1,    -1,    -1,    -1,    -1,  1177,
    1178,    -1,    -1,    -1,    -1,    -1,  1255,    -1,  1257,    -1,
      -1,  1260,    -1,    -1,  1263,   298,    -1,    -1,  1267,    -1,
    1040,    -1,    -1,    -1,  1044,    -1,    -1,    -1,  1048,   312,
      -1,   314,    -1,    -1,    -1,    -1,    -1,    -1,  1216,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1902,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1910,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1246,  1247,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1950,    -1,    -1,  1119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1970,  1971,    -1,    -1,
    1974,    -1,    -1,    -1,    -1,   408,    -1,    -1,    -1,    -1,
      -1,    -1,   415,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1318,  1542,  1543,    -1,    -1,  1546,  1547,    -1,    -1,    -1,
      -1,  1552,    -1,    -1,    -1,  1556,    -1,  1558,    -1,  1560,
    1409,    -1,    -1,   446,    -1,  2019,  2020,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   477,    -1,    -1,    -1,    -1,    -1,
    1220,   484,    -1,  1223,  1224,    -1,    -1,  1456,    -1,    -1,
      -1,  1231,  1232,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2080,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1255,   519,  1257,    -1,    -1,
    1260,    -1,    -1,  1263,    -1,    -1,    -1,  1267,    -1,   532,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   544,    -1,   546,   547,    -1,   549,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   557,    -1,   559,    -1,    -1,    -1,
      -1,    -1,    -1,   566,    -1,   568,   569,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,
     583,  1702,    58,   586,    60,    61,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1569,  1722,  1723,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   621,    -1,
     623,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1753,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,  1409,
      -1,   674,   675,    -1,    -1,    -1,    -1,    -1,   681,    -1,
      -1,    -1,    -1,   686,  1653,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,  1606,   712,
     186,    -1,  1681,    -1,    -1,    -1,  1456,  1838,  1687,    -1,
      -1,    -1,    -1,    -1,    -1,  1846,    -1,  1848,    -1,    -1,
    1851,  1852,    -1,  1854,    -1,   738,    -1,    -1,  1859,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,  1763,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,  1932,    79,    -1,  1783,  1784,    -1,    -1,  1939,  1940,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1569,
     833,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1962,    -1,   846,  1813,  1814,    -1,    -1,   115,   116,
      -1,    -1,  1821,    -1,  1752,    -1,    -1,  1826,    -1,    -1,
      -1,    -1,   865,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1993,    -1,  1995,    -1,   143,  1998,  1999,    -1,
      -1,    -1,    85,   886,  2005,  2006,    -1,    -1,    -1,    -1,
      93,    -1,    -1,    -1,    -1,   898,   899,    -1,    -1,   166,
      -1,    -1,   905,    -1,    -1,    -1,   109,   910,    -1,    -1,
      -1,    -1,    -1,  1653,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   930,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1904,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2072,  2073,  2074,    -1,    -1,    -1,   160,    -1,    -1,
      -1,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   176,    -1,    -1,    -1,    -1,    -1,  2100,
    2101,  2102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   201,    -1,
      -1,  1970,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1017,    -1,  1914,    -1,  1021,    -1,
      -1,    -1,    -1,  1763,    -1,  1028,    -1,    -1,    -1,    -1,
     233,   234,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1783,  1784,    -1,   249,    -1,    -1,    -1,
    2019,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1958,    -1,    -1,    -1,    -1,    -1,    -1,   270,    -1,    -1,
      -1,    -1,    -1,  1813,  1814,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1826,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   298,    -1,    -1,   301,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2017,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2043,    -1,  2045,    -1,    -1,
      -1,    -1,    -1,   356,   357,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1904,    -1,    -1,    -1,    -1,   372,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2083,  1189,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1209,    -1,    -1,  1212,
      -1,    -1,    -1,   416,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   425,   426,    -1,  1228,    -1,    -1,    -1,    -1,
    1970,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   446,   447,    -1,   449,    -1,    -1,  1252,
    2148,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   484,   485,    -1,  1287,    -1,  1289,    -1,    -1,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,
      -1,    63,    -1,    -1,    -1,   508,    -1,    -1,   511,    -1,
     513,   514,    -1,    -1,   517,    -1,    -1,    79,    80,    -1,
      -1,    -1,    -1,   526,    -1,    -1,   529,   530,   531,   532,
      -1,    -1,    -1,    -1,    -1,  1338,  1339,    -1,    -1,    -1,
      -1,    -1,  1345,   546,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,   586,    -1,    -1,   589,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   156,   157,   158,   159,    -1,    -1,
      -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1428,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   644,   645,    -1,    -1,    -1,  1449,    -1,    -1,    -1,
      -1,    -1,   655,    -1,   657,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1468,  1469,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,   686,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   732,
     733,    -1,  1535,    55,    -1,   738,    58,    -1,    60,    61,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1554,    -1,    -1,  1557,    -1,    78,    -1,    80,    81,
      -1,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,    -1,   108,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,  1620,    -1,   141,
    1623,    -1,    -1,    -1,    -1,   828,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     162,   163,    -1,   846,   166,   167,    -1,    -1,    -1,   171,
      -1,   173,   174,   175,   176,   177,   178,   179,    -1,  1662,
      -1,    -1,   865,    -1,    -1,    -1,   188,    -1,    -1,    -1,
      -1,    -1,    -1,  1676,    -1,    -1,    -1,    -1,    -1,  1682,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   891,    -1,
      13,    14,    15,    16,    17,    -1,   899,    20,   901,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
    1743,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,   954,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,  1772,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1006,    55,    -1,    -1,    58,    -1,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     143,    -1,    -1,    -1,  1027,    -1,    -1,    78,    -1,    80,
      81,    -1,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,    -1,   108,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,  1098,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1108,    -1,    -1,    -1,    -1,
    1113,   162,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,
     171,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   188,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1959,    -1,    -1,    -1,
      -1,    -1,    -1,  1166,    -1,    -1,    -1,    -1,    -1,  1172,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1195,  1196,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,  1242,
      59,    -1,    -1,    62,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,  1256,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1309,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1344,  1345,   162,    -1,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,   174,   175,    -1,    -1,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1376,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1392,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,  1431,  1432,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    80,    81,
      -1,    83,    -1,    -1,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,    -1,   108,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1517,  1518,  1519,    -1,    -1,  1522,
    1523,   163,    -1,    -1,   166,   167,  1529,    -1,    -1,   171,
      -1,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   188,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1557,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1588,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,  1622,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    -1,
      -1,    -1,    76,    -1,    78,    79,    80,    81,    -1,    83,
      -1,    -1,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,  1676,   108,    -1,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,    -1,   133,
     134,    -1,    -1,  1706,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,   163,
      -1,    -1,   166,   167,    -1,    -1,    -1,   171,    -1,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   188,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    -1,  1836,    -1,    76,    -1,    78,    79,    80,
      81,    -1,    83,    -1,    -1,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,  1869,   108,    -1,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,
     171,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,  1948,    -1,   188,  1951,     3,
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
      -1,    -1,   156,   157,   158,   159,    -1,    -1,    -1,   163,
     164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   188,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,
      -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,
     158,   159,    -1,    -1,    -1,   163,   164,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     188,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    -1,    -1,    -1,    76,    -1,    -1,    79,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
     112,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,   128,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,   174,   175,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   186,    -1,   188,     3,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,
     166,   167,    -1,    -1,    -1,   171,    -1,    -1,   174,   175,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     186,     3,     4,     5,     6,     7,     8,     9,    10,    11,
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
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,   174,   175,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   186,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,   141,    -1,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,   165,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,   174,   175,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   186,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,
     166,   167,    -1,    -1,    -1,   171,    -1,    -1,   174,   175,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     186,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,   174,   175,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   186,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,
      79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,   165,   166,   167,    -1,
      -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,   178,
     179,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    -1,    -1,    -1,    76,    -1,    -1,    79,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
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
     166,   167,   168,   169,    -1,    -1,    -1,   173,   174,   175,
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
      -1,    -1,    -1,   163,    -1,    -1,   166,   167,   168,   169,
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
      -1,    -1,   166,   167,   168,   169,    -1,    -1,    -1,   173,
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
     168,   169,    -1,    -1,    -1,   173,   174,   175,   176,   177,
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
      -1,   163,    -1,    -1,   166,   167,   168,   169,    -1,    -1,
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
     166,   167,   168,    -1,    -1,    -1,    -1,   173,   174,   175,
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
      -1,    -1,    -1,   163,    -1,    -1,   166,   167,   168,    -1,
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
      -1,    -1,   166,   167,   168,    -1,    -1,    -1,    -1,   173,
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
     168,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
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
      -1,   163,    -1,    -1,   166,   167,   168,    -1,    -1,    -1,
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
     166,   167,    -1,   169,    -1,    -1,    -1,   173,   174,   175,
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
      65,    66,    67,    68,    69,    70,    71,     5,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,
     175,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
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
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,
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
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    -1,    60,    61,    -1,    63,    -1,
      -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
     174,   175,    -1,    78,    -1,    80,    81,    -1,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    -1,    -1,   100,   101,   102,   103,   104,
     105,   106,    -1,   108,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,    -1,   133,   134,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    18,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
      -1,   166,   167,    -1,    -1,    -1,   171,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    55,    -1,    -1,    58,
      -1,    60,    61,   188,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,
      -1,    80,    81,    -1,    83,    -1,    -1,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    -1,
      -1,   100,   101,   102,   103,   104,   105,   106,    -1,   108,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,
      -1,    -1,   171,    -1,   173,   174,   175,   176,   177,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   188,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,   171,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   166,    -1,    -1,    -1,    -1,   171,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   166,    -1,    -1,    -1,    -1,   171,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,
      -1,    97,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,    -1,   171,    -1,   173,   174,   175,
     176,   177,   178,   179,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,
      -1,    -1,   171,    -1,   173,   174,   175,   176,   177,   178,
     179,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
      -1,   163,    -1,   165,   166,   167,    -1,    -1,    -1,    -1,
      -1,   173,   174,   175,   176,   177,   178,   179,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,    -1,   133,   134,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
      -1,   166,   167,    -1,    -1,    -1,   171,    -1,   173,   174,
     175,   176,   177,   178,   179,    13,    14,    15,    16,    17,
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
      -1,    -1,    -1,   171,    -1,   173,   174,   175,   176,   177,
     178,   179,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,   173,   174,   175,   176,   177,   178,   179,    13,
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
     167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,
     177,   178,   179,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,   170,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    79,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,   115,   116,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   165,   166,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   165,   166,     4,     5,     6,
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
      -1,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,   166,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    13,    14,    15,    16,    17,    18,    79,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,   115,   116,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,    -1,    -1,    -1,    -1,
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
      -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,   174,   175,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    79,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,   115,   116,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,
      -1,    13,    14,    15,    16,    17,   174,   175,    20,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     166,   167,    -1,    13,    14,    15,    16,    17,   174,   175,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   174,   175,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,    -1,   166,   167,   168,    -1,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,   166,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,   166,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    79,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,   115,   116,
      57,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,   143,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,    -1,   133,   134,    -1,   143,    55,
      -1,    -1,    58,   141,    60,    61,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,
     158,   159,    -1,    -1,    80,   163,   164,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,    55,   133,   134,    58,
      -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,
     166,   167,    -1,    -1,    -1,   171,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,    -1,
      -1,    -1,   171,    -1,   173,   174,   175,   176,   177,   178,
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
      -1,   166,   167,    -1,   169,    -1,    -1,    -1,   173,   174,
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
     171,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,    55,   133,
     134,    58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,
      -1,    -1,   166,   167,    -1,    -1,   170,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,
     167,   168,    -1,    -1,    -1,    -1,   173,   174,   175,   176,
     177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   163,    -1,    -1,   166,   167,   168,    -1,
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
      -1,    80,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,    -1,   171,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,
      -1,    -1,   171,    -1,   173,   174,   175,   176,   177,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   163,    -1,   165,   166,   167,    -1,    -1,    -1,    -1,
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
      -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,    55,
      -1,    -1,    58,   141,    60,    61,    -1,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,   163,   164,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,
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
     126,   127,    -1,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179
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
     450,   451,   115,   347,    55,    58,    60,    61,    63,    64,
      80,   110,   111,   113,   114,   125,   126,   127,   129,   130,
     131,   133,   134,   163,   167,   168,   173,   174,   176,   177,
     178,   179,   192,   193,   197,   198,   199,   202,   207,   208,
     209,   210,   213,   214,   215,   216,   217,   218,   219,   220,
     221,   222,   223,   224,   229,   234,   309,   310,   319,   321,
     323,   327,   328,   335,   336,   342,   343,   344,   345,   346,
     349,   356,   357,   375,   382,   383,   384,   385,   386,   387,
     477,   490,   491,   492,   493,   498,   499,   194,   167,   310,
     320,   323,   444,   448,   494,   495,   498,   499,   188,   188,
     191,   160,   171,   187,   232,   391,    96,   169,   428,   108,
     196,   432,   169,   169,   169,   188,   115,   116,   163,   208,
     315,   316,   439,   440,   441,   442,   443,   444,   448,   452,
     453,   454,   455,   456,   457,   458,   459,   460,   466,     3,
      53,    54,    56,    62,   338,     3,   167,   208,   309,   310,
     324,   328,   330,   341,   346,   424,   444,   448,   498,    76,
     307,   309,   323,   336,   340,   345,   425,   444,   448,    72,
     329,   329,   324,   330,   318,   329,   330,   338,   357,   324,
     329,   324,   166,   433,   169,   191,   163,   171,   240,   433,
     433,     3,   298,   299,   314,   317,   323,   327,   167,   320,
     323,   496,   196,   196,   421,   187,   323,   163,   208,   430,
     439,   440,   444,   453,   457,   167,   208,   310,   498,   422,
     423,    64,    72,    73,    74,    75,   167,   185,   196,   397,
     399,   403,   405,   406,   346,   165,   167,   208,   319,   323,
     336,   343,   345,   387,   490,   498,   433,   115,   116,   178,
     194,   346,   374,   466,   435,   163,   404,   405,   163,    13,
      95,   163,   196,   436,   437,   438,   197,   167,   207,   208,
     224,   225,   346,   165,   167,   208,   229,   320,   389,   390,
     407,   494,   499,   436,   323,   445,   446,   447,   449,   450,
     451,   165,   165,   165,   165,   165,   165,   165,   163,   207,
     163,   163,   207,   163,   163,   433,   210,   163,   207,   163,
     113,   115,   116,   324,   329,   330,   163,   207,   207,    19,
      21,    92,   167,   176,   177,   211,   212,   229,   236,   240,
     359,   389,   498,   168,   169,   229,   323,   327,   115,   167,
     194,   320,   477,   496,   163,   199,   168,   167,   172,   167,
     172,   127,   130,   132,   133,   134,   163,   166,   167,   171,
     172,   145,   146,   147,   148,   149,   150,   151,   152,   153,
     154,   155,   187,   231,   232,   233,   167,   210,   321,   323,
     336,   343,   345,   489,   490,   498,   499,   210,   180,   174,
     181,   182,   176,   177,   135,   136,   137,   138,   183,   184,
     139,   140,   175,   173,   185,   141,   142,   186,   168,   163,
     163,   167,   175,   187,   208,   439,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   477,   479,   480,   481,   482,
     483,   484,   501,   144,   167,   208,   349,   498,   323,   343,
     329,   324,   166,   433,   168,   169,   168,   169,   321,   323,
     491,   196,   321,   477,   491,   163,   196,   168,   444,   448,
     498,   169,   113,   166,   167,   171,   193,   195,   229,   392,
     393,   394,   395,   396,    22,   392,   163,   196,   240,   163,
     163,   194,   430,   194,   434,   439,   441,   442,   443,   452,
     454,   455,   456,   458,   459,   460,   323,   440,   453,   457,
     169,   432,   167,   433,   474,   477,   432,   433,   433,   428,
     298,   163,   433,   474,   432,   433,   433,   428,   433,   433,
     323,   430,   163,   163,   322,   323,   320,   323,   167,   168,
     320,   494,   499,   432,   348,   171,   428,   298,   196,   196,
     391,   309,   328,   426,   444,   448,   171,   428,   298,   409,
     323,   336,   323,   323,   115,   347,   115,   116,   194,   346,
     351,   409,   144,   194,   323,   379,   380,   384,   385,   388,
     162,   190,   240,   314,   188,   444,   457,   323,   174,   229,
     432,   163,   432,   191,   229,   434,   439,   323,   163,   196,
     419,   171,   163,   196,   171,   196,   144,   174,   175,   402,
     165,   169,   196,   406,   165,   168,   163,   175,   208,   498,
     165,   194,   374,   466,   371,   171,   374,   397,   187,   397,
     436,   165,   169,   163,   165,   229,   165,   169,   163,   208,
     470,   471,   472,   473,   474,   165,   169,   165,   165,   165,
     165,   165,   165,   165,   163,   433,   474,   477,   163,   474,
     477,   389,   499,   199,   389,   167,   389,   390,   194,   389,
     499,   229,   389,   165,   389,   389,   389,   168,   383,   165,
     176,   177,   212,    18,   325,   165,   169,   165,   174,   175,
     165,   163,   321,   477,   491,   164,   229,   235,   169,   167,
     174,   208,   229,   346,   229,   323,   163,   163,   320,   496,
     171,   229,   194,   229,   194,   125,   167,   194,   186,   226,
     227,   228,   229,   125,   167,   196,   359,   229,   226,   194,
     171,   229,   498,   210,   213,   213,   213,   214,   214,   215,
     215,   216,   216,   216,   216,   217,   217,   218,   219,   220,
     221,   222,   170,   236,   190,   163,   379,   439,   462,   463,
     464,   467,   480,   481,   482,   168,   190,    18,   229,   323,
     461,   465,   479,   163,   433,   483,   501,   433,   433,   501,
     163,   433,   483,   433,   433,   501,   433,   433,   477,   168,
     225,   321,   489,   499,   196,   323,   167,   194,   194,   383,
     386,   386,   387,   501,   321,   491,   190,   501,   190,   167,
     195,   224,   225,   229,   431,   393,   170,   169,   500,   392,
     166,   167,   170,   396,   407,   163,   197,   190,   187,   439,
     441,   442,   443,   452,   454,   455,   456,   458,   459,   460,
     165,   165,   165,   165,   165,   165,   165,   165,   165,   165,
     440,   453,   457,   433,   187,   168,   229,   330,   346,   475,
     391,   240,   428,   379,   391,   240,   430,   236,   390,   236,
     390,   430,   115,   419,   240,   428,   432,   171,   171,   428,
     298,   419,   240,   428,   353,   354,   352,   171,   165,   169,
     165,   169,    77,   300,   301,   188,   168,   168,   190,   439,
     421,   419,   196,   168,     1,   307,   309,   321,   323,   412,
     413,   414,   415,   163,   401,   399,   400,    85,   334,    18,
     323,   433,   171,   433,   374,    10,   173,   374,   376,   377,
     171,   165,   390,   165,   165,   437,   226,   188,   197,   379,
     471,   472,   473,   323,   470,   433,   433,   229,   390,   163,
     433,   474,   477,   163,   474,   477,   379,   379,   165,   165,
     169,   165,   169,   165,   165,   165,   169,   165,   210,   165,
     165,   165,   169,   210,    18,   325,   229,   165,   165,   164,
     171,   210,   144,   381,   382,   383,   321,   491,   168,   169,
     235,   168,   168,   168,   229,   190,   190,   226,   168,   168,
     125,   129,   131,   195,   203,   204,   205,   194,   165,   169,
     203,   168,   169,   162,   393,   224,   170,   381,   467,   165,
     165,   165,   165,   165,   165,   165,   165,     5,   323,   163,
     433,   439,   466,   461,   465,   479,   379,   379,   168,   501,
     203,   168,   169,   381,   196,   203,   144,   179,   168,   168,
     500,   392,   394,   162,   229,   165,   190,   165,   381,   229,
     165,   165,   165,   165,   165,   165,   165,   165,   165,   163,
     433,   474,   477,   163,   433,   474,   477,   163,   433,   474,
     477,   430,    22,   477,   157,   169,   179,   476,   168,   169,
     240,   165,   165,   165,   165,   165,   417,   418,   240,   162,
     412,   419,   240,   428,   418,   240,   171,   171,   171,   360,
     144,   384,   385,   194,   196,   302,    18,    78,    80,    81,
      83,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,   100,   101,   102,   103,   104,   105,   106,
     108,   115,   116,   128,   163,   167,   196,   236,   237,   238,
     239,   240,   241,   242,   244,   245,   254,   261,   262,   263,
     264,   265,   266,   271,   272,   275,   276,   277,   278,   279,
     280,   281,   287,   288,   289,   303,   323,   327,   429,    77,
     381,   420,   418,   165,   430,   162,   413,   169,   188,   169,
     188,   407,   187,   398,   398,   372,   376,   374,   171,   346,
     169,   500,   196,   376,   171,   165,   165,   165,   165,   165,
     165,   165,   470,   379,   379,   165,   165,   319,   194,    85,
     200,   201,   389,   210,   210,   210,   210,   210,   171,   393,
     165,   169,   169,   164,   229,   168,   168,   381,   381,   162,
     206,   167,   204,   206,   206,   168,   169,   132,   166,   187,
     228,   168,   235,   500,   224,   191,   165,   163,   433,   474,
     477,   163,   433,   483,   163,   433,   483,   477,   322,     5,
     174,   191,   229,   439,   433,   433,   165,   165,   168,   386,
     191,   391,   168,   225,   225,   162,   392,   433,   381,   433,
     191,   163,   433,   474,   477,   163,   433,   474,   477,   163,
     433,   474,   477,   379,   379,   379,   432,   236,   229,   229,
     330,   346,   420,   162,   418,   240,   420,   360,   360,   360,
       3,     5,    10,    80,   162,   304,   311,   312,   320,   323,
     361,   366,   494,   169,   188,   163,    68,    69,   188,   240,
     303,   429,   163,   163,    18,   238,   163,   163,   188,   196,
     188,   196,   174,   196,   171,   237,   163,   163,   163,   238,
     163,   240,   229,   230,   230,    14,   290,   266,   277,   170,
     188,   191,   242,    85,   188,   196,    98,    99,   270,   274,
     119,   142,   269,   118,   141,   273,   269,   388,   323,   302,
     191,   420,   196,   196,   430,   165,   390,   404,   404,   374,
     500,   171,   376,    10,   377,   162,   187,   378,   500,   162,
     412,   163,   433,   474,   477,   165,   165,   478,   479,   165,
     170,   165,   169,   170,   393,   500,   144,   383,   144,   191,
     191,   130,   203,   204,   167,   204,   167,   204,   229,   168,
     162,   165,   379,   379,   379,   229,   229,   191,   168,   191,
     165,   168,   191,   165,   379,   379,   379,   165,   165,   165,
     391,   168,   476,   162,   420,   162,   162,   162,   162,   320,
     320,   359,   367,   494,   320,   366,   163,   355,   188,   188,
     188,   163,   170,   208,   362,   363,   369,   439,   440,   453,
     457,   169,   188,   196,   196,   226,   188,   240,   188,   240,
     236,   246,   303,   305,   308,   314,   323,   327,   236,    87,
     165,   246,   156,   157,   158,   159,   164,   165,   188,   236,
     255,   256,   258,   303,   188,   188,   236,   188,   393,   188,
     236,   407,   236,   255,   120,   121,   122,   123,   124,   282,
     284,   285,   188,   107,   188,    91,   163,   165,   433,   162,
     188,   188,   163,   163,   238,   238,   266,   163,   276,   266,
     276,   240,   188,   165,   162,   402,   171,   162,   376,   500,
     346,   196,   171,   225,   162,   162,   379,   165,   229,   201,
     229,   500,   162,   165,   165,   168,   203,   203,   165,   165,
     165,   191,   191,   168,   168,   165,   433,   165,   165,   165,
     229,   162,   355,   355,   355,   362,   163,   208,   364,   365,
     474,   485,   486,   487,   488,   188,   169,   188,   362,   188,
     407,   434,   439,   229,   323,   162,   169,   188,   368,   369,
     368,   368,   196,   165,   165,   236,   323,   165,   163,   238,
     165,   156,   157,   158,   159,   179,   188,   259,   260,   238,
     237,   188,   260,   165,   170,   236,   164,   236,   237,   258,
     188,   500,   165,   165,   165,   165,   240,   284,   285,   163,
     229,   163,   197,     1,   238,   210,   267,   236,    82,   117,
     268,   270,    82,   433,   398,   376,   500,   162,   378,   393,
     165,   162,   433,   433,   168,   168,   168,   168,   188,   486,
     487,   488,   323,   485,   169,   188,   433,   433,   188,   165,
     439,   433,   238,   238,    84,    85,   171,   249,   250,   251,
     165,   236,    82,   238,   236,   164,   236,    82,   188,   164,
     236,   237,   258,   323,   345,   164,   236,   238,   256,   260,
     260,   188,   236,   162,   171,   251,   238,   238,   163,   286,
     321,   323,   494,   188,   197,   165,   170,   165,   169,   170,
     165,   238,   163,   238,   238,   238,   404,   500,   162,   500,
     165,   165,   165,   485,   433,   363,    82,     1,   225,   247,
     248,   431,     1,   170,     1,   190,   238,   249,    82,   188,
     165,   238,    82,   188,   179,   179,   238,   237,   260,   260,
     188,    64,   236,   257,   346,   179,   179,    82,   164,   236,
     164,   236,   237,   188,     1,   190,   286,   188,   283,   163,
     208,   430,   485,   194,   170,   188,   167,   197,   291,   292,
     293,   210,   226,   236,   269,   162,   162,   163,   433,   474,
     477,   365,   238,   144,     1,   169,   170,   162,   296,   297,
     303,   238,    82,   188,   238,   236,   164,   164,   236,   164,
     236,   164,   236,   237,   194,   346,   164,   236,   164,   236,
     238,   179,   179,   179,   179,   162,   296,   283,   224,   165,
     323,   170,   113,   163,   165,   170,   169,   165,   165,    82,
     265,   379,   225,   247,   250,   252,   253,   303,   238,   179,
     179,   179,   179,   164,   164,   236,   164,   236,   164,   236,
     252,   165,   240,   291,   168,   225,   188,   291,   293,   238,
      82,   165,   238,   243,   191,   250,   164,   164,   236,   164,
     236,   164,   236,   191,   240,   170,   197,   165,   165,   170,
     238,     1,   238,   162,   243,   162,   197,   294,   163,   188,
     294,   169,   170,   225,   165,   197,   194,   295,   165,   188,
     165,   169,   188,   194
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
       1,     1,     1,     1,     1,     4,     5,     1,     1,     3,
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
       3,     5,     2,     2,     3,     3,     3,     4,     3,     4,
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
       2,     3,     4,     2,     2,     1,     2,     2,     3,     3,
       5,     4,     1,     3,     0,     2,     0,     5,     0,     5,
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
       2,     3,     5,     3,     1,     1,     1,     3,     3,     3,
       5,     1,     1,     3,     3,     4,     4,     0,     1,     1,
       3,     2,     2,     1,     2,     2,     3,     4,     1,     4,
       4,     3,     3,     6,     3,     1,     2,     1,     2,     6,
       5,     6,     7,     7,     1,     2,     2,     1,     2,     2,
       3,     4,     1,     4,     4,     3,     6,     3,     1,     1,
       2,     1,     1,     2,     3,     2,     3,     2,     3,     3,
       2,     4,     3,     2,     3,     2,     4,     3,     2,     4,
       4,     4,     5,     1,     2,     1,     1,     1,     2,     3,
       2,     3,     2,     3,     3,     4,     2,     3,     4,     2,
       3,     4,     5,     5,     6,     6,     0,     1,     0,     2
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
#line 9417 "Parser/parser.cc"
    break;

  case 3:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 9423 "Parser/parser.cc"
    break;

  case 4:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 9429 "Parser/parser.cc"
    break;

  case 5:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9435 "Parser/parser.cc"
    break;

  case 6:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9441 "Parser/parser.cc"
    break;

  case 7:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9447 "Parser/parser.cc"
    break;

  case 8:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 9453 "Parser/parser.cc"
    break;

  case 20:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 9459 "Parser/parser.cc"
    break;

  case 24:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 9465 "Parser/parser.cc"
    break;

  case 25:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 9471 "Parser/parser.cc"
    break;

  case 26:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 9481 "Parser/parser.cc"
    break;

  case 27:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 9487 "Parser/parser.cc"
    break;

  case 28:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 9493 "Parser/parser.cc"
    break;

  case 29:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 9499 "Parser/parser.cc"
    break;

  case 31:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9505 "Parser/parser.cc"
    break;

  case 32:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 9511 "Parser/parser.cc"
    break;

  case 33:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9517 "Parser/parser.cc"
    break;

  case 34:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9523 "Parser/parser.cc"
    break;

  case 35:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 9533 "Parser/parser.cc"
    break;

  case 36:
#line 734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 9539 "Parser/parser.cc"
    break;

  case 37:
#line 736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 9545 "Parser/parser.cc"
    break;

  case 38:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 9551 "Parser/parser.cc"
    break;

  case 39:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9557 "Parser/parser.cc"
    break;

  case 40:
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9563 "Parser/parser.cc"
    break;

  case 41:
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9569 "Parser/parser.cc"
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
#line 9581 "Parser/parser.cc"
    break;

  case 44:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 9590 "Parser/parser.cc"
    break;

  case 45:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 9596 "Parser/parser.cc"
    break;

  case 47:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 9602 "Parser/parser.cc"
    break;

  case 48:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9608 "Parser/parser.cc"
    break;

  case 49:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9614 "Parser/parser.cc"
    break;

  case 50:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9620 "Parser/parser.cc"
    break;

  case 51:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 9630 "Parser/parser.cc"
    break;

  case 52:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9636 "Parser/parser.cc"
    break;

  case 53:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_va_arg( yylloc, (yyvsp[-4].expr), ( (yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl) ) ) ); }
#line 9642 "Parser/parser.cc"
    break;

  case 54:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9648 "Parser/parser.cc"
    break;

  case 55:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9654 "Parser/parser.cc"
    break;

  case 56:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9660 "Parser/parser.cc"
    break;

  case 57:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9666 "Parser/parser.cc"
    break;

  case 58:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9672 "Parser/parser.cc"
    break;

  case 59:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9678 "Parser/parser.cc"
    break;

  case 60:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9684 "Parser/parser.cc"
    break;

  case 61:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 9690 "Parser/parser.cc"
    break;

  case 62:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9696 "Parser/parser.cc"
    break;

  case 63:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9702 "Parser/parser.cc"
    break;

  case 64:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9708 "Parser/parser.cc"
    break;

  case 65:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 9714 "Parser/parser.cc"
    break;

  case 66:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 9720 "Parser/parser.cc"
    break;

  case 67:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 9726 "Parser/parser.cc"
    break;

  case 68:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 9732 "Parser/parser.cc"
    break;

  case 69:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 9742 "Parser/parser.cc"
    break;

  case 71:
#line 855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9748 "Parser/parser.cc"
    break;

  case 73:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9754 "Parser/parser.cc"
    break;

  case 74:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9760 "Parser/parser.cc"
    break;

  case 75:
#line 865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9766 "Parser/parser.cc"
    break;

  case 76:
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9772 "Parser/parser.cc"
    break;

  case 77:
#line 869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9778 "Parser/parser.cc"
    break;

  case 78:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9784 "Parser/parser.cc"
    break;

  case 79:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 9790 "Parser/parser.cc"
    break;

  case 80:
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 9796 "Parser/parser.cc"
    break;

  case 81:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 9804 "Parser/parser.cc"
    break;

  case 82:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9810 "Parser/parser.cc"
    break;

  case 83:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 9819 "Parser/parser.cc"
    break;

  case 86:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 9825 "Parser/parser.cc"
    break;

  case 87:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 9831 "Parser/parser.cc"
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
#line 9851 "Parser/parser.cc"
    break;

  case 89:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 9857 "Parser/parser.cc"
    break;

  case 90:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 9863 "Parser/parser.cc"
    break;

  case 91:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 9869 "Parser/parser.cc"
    break;

  case 92:
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 9875 "Parser/parser.cc"
    break;

  case 93:
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9881 "Parser/parser.cc"
    break;

  case 94:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 9887 "Parser/parser.cc"
    break;

  case 95:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9893 "Parser/parser.cc"
    break;

  case 96:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9899 "Parser/parser.cc"
    break;

  case 97:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9905 "Parser/parser.cc"
    break;

  case 98:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 9911 "Parser/parser.cc"
    break;

  case 99:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9920 "Parser/parser.cc"
    break;

  case 100:
#line 953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9926 "Parser/parser.cc"
    break;

  case 101:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9932 "Parser/parser.cc"
    break;

  case 102:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9938 "Parser/parser.cc"
    break;

  case 103:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9944 "Parser/parser.cc"
    break;

  case 104:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9950 "Parser/parser.cc"
    break;

  case 105:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9956 "Parser/parser.cc"
    break;

  case 106:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9962 "Parser/parser.cc"
    break;

  case 107:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9968 "Parser/parser.cc"
    break;

  case 108:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9974 "Parser/parser.cc"
    break;

  case 110:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9980 "Parser/parser.cc"
    break;

  case 111:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9986 "Parser/parser.cc"
    break;

  case 112:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9992 "Parser/parser.cc"
    break;

  case 113:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9998 "Parser/parser.cc"
    break;

  case 114:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 10004 "Parser/parser.cc"
    break;

  case 115:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 10010 "Parser/parser.cc"
    break;

  case 116:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10016 "Parser/parser.cc"
    break;

  case 117:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10022 "Parser/parser.cc"
    break;

  case 125:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10028 "Parser/parser.cc"
    break;

  case 127:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10034 "Parser/parser.cc"
    break;

  case 128:
#line 1017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10040 "Parser/parser.cc"
    break;

  case 129:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10046 "Parser/parser.cc"
    break;

  case 131:
#line 1025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10052 "Parser/parser.cc"
    break;

  case 132:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10058 "Parser/parser.cc"
    break;

  case 134:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10064 "Parser/parser.cc"
    break;

  case 135:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10070 "Parser/parser.cc"
    break;

  case 137:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10076 "Parser/parser.cc"
    break;

  case 138:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10082 "Parser/parser.cc"
    break;

  case 139:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10088 "Parser/parser.cc"
    break;

  case 140:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10094 "Parser/parser.cc"
    break;

  case 142:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10100 "Parser/parser.cc"
    break;

  case 143:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10106 "Parser/parser.cc"
    break;

  case 145:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10112 "Parser/parser.cc"
    break;

  case 147:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10118 "Parser/parser.cc"
    break;

  case 149:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10124 "Parser/parser.cc"
    break;

  case 151:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 10130 "Parser/parser.cc"
    break;

  case 153:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 10136 "Parser/parser.cc"
    break;

  case 155:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10142 "Parser/parser.cc"
    break;

  case 156:
#line 1093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 10148 "Parser/parser.cc"
    break;

  case 158:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10154 "Parser/parser.cc"
    break;

  case 161:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10160 "Parser/parser.cc"
    break;

  case 162:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 10166 "Parser/parser.cc"
    break;

  case 163:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10172 "Parser/parser.cc"
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
#line 10184 "Parser/parser.cc"
    break;

  case 167:
#line 1135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10190 "Parser/parser.cc"
    break;

  case 168:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10196 "Parser/parser.cc"
    break;

  case 172:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 10202 "Parser/parser.cc"
    break;

  case 173:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 10208 "Parser/parser.cc"
    break;

  case 174:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 10214 "Parser/parser.cc"
    break;

  case 175:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 10220 "Parser/parser.cc"
    break;

  case 176:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 10226 "Parser/parser.cc"
    break;

  case 177:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 10232 "Parser/parser.cc"
    break;

  case 178:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 10238 "Parser/parser.cc"
    break;

  case 179:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 10244 "Parser/parser.cc"
    break;

  case 180:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 10250 "Parser/parser.cc"
    break;

  case 181:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 10256 "Parser/parser.cc"
    break;

  case 182:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 10262 "Parser/parser.cc"
    break;

  case 183:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 10268 "Parser/parser.cc"
    break;

  case 184:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 10274 "Parser/parser.cc"
    break;

  case 185:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 10280 "Parser/parser.cc"
    break;

  case 186:
#line 1178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 10286 "Parser/parser.cc"
    break;

  case 188:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10292 "Parser/parser.cc"
    break;

  case 189:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10298 "Parser/parser.cc"
    break;

  case 190:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10304 "Parser/parser.cc"
    break;

  case 192:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10310 "Parser/parser.cc"
    break;

  case 193:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10316 "Parser/parser.cc"
    break;

  case 208:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10322 "Parser/parser.cc"
    break;

  case 210:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 10328 "Parser/parser.cc"
    break;

  case 211:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 10334 "Parser/parser.cc"
    break;

  case 212:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 10345 "Parser/parser.cc"
    break;

  case 213:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 10351 "Parser/parser.cc"
    break;

  case 214:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 10357 "Parser/parser.cc"
    break;

  case 216:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10363 "Parser/parser.cc"
    break;

  case 217:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10369 "Parser/parser.cc"
    break;

  case 218:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10375 "Parser/parser.cc"
    break;

  case 219:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10381 "Parser/parser.cc"
    break;

  case 220:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10387 "Parser/parser.cc"
    break;

  case 223:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10393 "Parser/parser.cc"
    break;

  case 224:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 10400 "Parser/parser.cc"
    break;

  case 225:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 10406 "Parser/parser.cc"
    break;

  case 226:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 10412 "Parser/parser.cc"
    break;

  case 227:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10418 "Parser/parser.cc"
    break;

  case 228:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10424 "Parser/parser.cc"
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
#line 10438 "Parser/parser.cc"
    break;

  case 230:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10444 "Parser/parser.cc"
    break;

  case 231:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10450 "Parser/parser.cc"
    break;

  case 232:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 10459 "Parser/parser.cc"
    break;

  case 233:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10465 "Parser/parser.cc"
    break;

  case 234:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( nullptr, (yyvsp[0].expr) ); }
#line 10471 "Parser/parser.cc"
    break;

  case 235:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 10477 "Parser/parser.cc"
    break;

  case 236:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 10483 "Parser/parser.cc"
    break;

  case 237:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 10489 "Parser/parser.cc"
    break;

  case 238:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10495 "Parser/parser.cc"
    break;

  case 239:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10501 "Parser/parser.cc"
    break;

  case 241:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 10507 "Parser/parser.cc"
    break;

  case 242:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 10513 "Parser/parser.cc"
    break;

  case 243:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 10519 "Parser/parser.cc"
    break;

  case 244:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 10525 "Parser/parser.cc"
    break;

  case 245:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 10531 "Parser/parser.cc"
    break;

  case 246:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 10537 "Parser/parser.cc"
    break;

  case 247:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 10543 "Parser/parser.cc"
    break;

  case 249:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 10549 "Parser/parser.cc"
    break;

  case 250:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10555 "Parser/parser.cc"
    break;

  case 251:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 10561 "Parser/parser.cc"
    break;

  case 253:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10567 "Parser/parser.cc"
    break;

  case 254:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 10573 "Parser/parser.cc"
    break;

  case 255:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10579 "Parser/parser.cc"
    break;

  case 256:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10588 "Parser/parser.cc"
    break;

  case 257:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10594 "Parser/parser.cc"
    break;

  case 258:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10600 "Parser/parser.cc"
    break;

  case 259:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 10606 "Parser/parser.cc"
    break;

  case 260:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10615 "Parser/parser.cc"
    break;

  case 261:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 10621 "Parser/parser.cc"
    break;

  case 262:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10627 "Parser/parser.cc"
    break;

  case 263:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10633 "Parser/parser.cc"
    break;

  case 264:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10642 "Parser/parser.cc"
    break;

  case 265:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10648 "Parser/parser.cc"
    break;

  case 266:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10654 "Parser/parser.cc"
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
#line 10673 "Parser/parser.cc"
    break;

  case 269:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10679 "Parser/parser.cc"
    break;

  case 270:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 10687 "Parser/parser.cc"
    break;

  case 271:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10693 "Parser/parser.cc"
    break;

  case 272:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 10699 "Parser/parser.cc"
    break;

  case 273:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10705 "Parser/parser.cc"
    break;

  case 274:
#line 1472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 10711 "Parser/parser.cc"
    break;

  case 275:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10717 "Parser/parser.cc"
    break;

  case 276:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10723 "Parser/parser.cc"
    break;

  case 277:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10732 "Parser/parser.cc"
    break;

  case 278:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 10741 "Parser/parser.cc"
    break;

  case 279:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10747 "Parser/parser.cc"
    break;

  case 280:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10756 "Parser/parser.cc"
    break;

  case 281:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 10765 "Parser/parser.cc"
    break;

  case 282:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 10771 "Parser/parser.cc"
    break;

  case 283:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 10777 "Parser/parser.cc"
    break;

  case 284:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 10783 "Parser/parser.cc"
    break;

  case 285:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 10789 "Parser/parser.cc"
    break;

  case 286:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 10795 "Parser/parser.cc"
    break;

  case 287:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 10801 "Parser/parser.cc"
    break;

  case 288:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10807 "Parser/parser.cc"
    break;

  case 289:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10813 "Parser/parser.cc"
    break;

  case 290:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10822 "Parser/parser.cc"
    break;

  case 291:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10832 "Parser/parser.cc"
    break;

  case 292:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 10838 "Parser/parser.cc"
    break;

  case 293:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10844 "Parser/parser.cc"
    break;

  case 294:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10853 "Parser/parser.cc"
    break;

  case 295:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10863 "Parser/parser.cc"
    break;

  case 296:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10869 "Parser/parser.cc"
    break;

  case 297:
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10878 "Parser/parser.cc"
    break;

  case 298:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10888 "Parser/parser.cc"
    break;

  case 299:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 10894 "Parser/parser.cc"
    break;

  case 300:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 10900 "Parser/parser.cc"
    break;

  case 301:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10906 "Parser/parser.cc"
    break;

  case 302:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10912 "Parser/parser.cc"
    break;

  case 303:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10921 "Parser/parser.cc"
    break;

  case 304:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10931 "Parser/parser.cc"
    break;

  case 305:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10937 "Parser/parser.cc"
    break;

  case 306:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10946 "Parser/parser.cc"
    break;

  case 307:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10956 "Parser/parser.cc"
    break;

  case 308:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10962 "Parser/parser.cc"
    break;

  case 309:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10971 "Parser/parser.cc"
    break;

  case 310:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10981 "Parser/parser.cc"
    break;

  case 311:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 10987 "Parser/parser.cc"
    break;

  case 312:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 10995 "Parser/parser.cc"
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
#line 11007 "Parser/parser.cc"
    break;

  case 314:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11016 "Parser/parser.cc"
    break;

  case 315:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false );
		}
#line 11025 "Parser/parser.cc"
    break;

  case 316:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11034 "Parser/parser.cc"
    break;

  case 317:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11040 "Parser/parser.cc"
    break;

  case 318:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11046 "Parser/parser.cc"
    break;

  case 319:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11052 "Parser/parser.cc"
    break;

  case 320:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11058 "Parser/parser.cc"
    break;

  case 321:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11064 "Parser/parser.cc"
    break;

  case 322:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11070 "Parser/parser.cc"
    break;

  case 323:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11076 "Parser/parser.cc"
    break;

  case 325:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11082 "Parser/parser.cc"
    break;

  case 326:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11088 "Parser/parser.cc"
    break;

  case 327:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 11094 "Parser/parser.cc"
    break;

  case 328:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 11100 "Parser/parser.cc"
    break;

  case 329:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 11106 "Parser/parser.cc"
    break;

  case 330:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 11112 "Parser/parser.cc"
    break;

  case 331:
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 11118 "Parser/parser.cc"
    break;

  case 332:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 11124 "Parser/parser.cc"
    break;

  case 333:
#line 1695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 11130 "Parser/parser.cc"
    break;

  case 334:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 11136 "Parser/parser.cc"
    break;

  case 335:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 11142 "Parser/parser.cc"
    break;

  case 336:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 11148 "Parser/parser.cc"
    break;

  case 337:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 11154 "Parser/parser.cc"
    break;

  case 338:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 11160 "Parser/parser.cc"
    break;

  case 339:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 11166 "Parser/parser.cc"
    break;

  case 340:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 11172 "Parser/parser.cc"
    break;

  case 341:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 11178 "Parser/parser.cc"
    break;

  case 342:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 11184 "Parser/parser.cc"
    break;

  case 343:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 11190 "Parser/parser.cc"
    break;

  case 344:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 11196 "Parser/parser.cc"
    break;

  case 345:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 11202 "Parser/parser.cc"
    break;

  case 346:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 11208 "Parser/parser.cc"
    break;

  case 349:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11214 "Parser/parser.cc"
    break;

  case 350:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 11223 "Parser/parser.cc"
    break;

  case 351:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11229 "Parser/parser.cc"
    break;

  case 352:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11235 "Parser/parser.cc"
    break;

  case 355:
#line 1759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11241 "Parser/parser.cc"
    break;

  case 356:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11247 "Parser/parser.cc"
    break;

  case 359:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11253 "Parser/parser.cc"
    break;

  case 360:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 11259 "Parser/parser.cc"
    break;

  case 361:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11265 "Parser/parser.cc"
    break;

  case 362:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11271 "Parser/parser.cc"
    break;

  case 363:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11277 "Parser/parser.cc"
    break;

  case 364:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11283 "Parser/parser.cc"
    break;

  case 365:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 11289 "Parser/parser.cc"
    break;

  case 366:
#line 1791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11295 "Parser/parser.cc"
    break;

  case 367:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 11301 "Parser/parser.cc"
    break;

  case 370:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11307 "Parser/parser.cc"
    break;

  case 371:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11313 "Parser/parser.cc"
    break;

  case 372:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 11319 "Parser/parser.cc"
    break;

  case 373:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11325 "Parser/parser.cc"
    break;

  case 374:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11331 "Parser/parser.cc"
    break;

  case 375:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11337 "Parser/parser.cc"
    break;

  case 376:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11343 "Parser/parser.cc"
    break;

  case 377:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11349 "Parser/parser.cc"
    break;

  case 378:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 11355 "Parser/parser.cc"
    break;

  case 379:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 11361 "Parser/parser.cc"
    break;

  case 380:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11367 "Parser/parser.cc"
    break;

  case 381:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 11373 "Parser/parser.cc"
    break;

  case 382:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 11379 "Parser/parser.cc"
    break;

  case 383:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 11385 "Parser/parser.cc"
    break;

  case 384:
#line 1858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11391 "Parser/parser.cc"
    break;

  case 385:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-6].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 11397 "Parser/parser.cc"
    break;

  case 386:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11403 "Parser/parser.cc"
    break;

  case 387:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 11409 "Parser/parser.cc"
    break;

  case 388:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11415 "Parser/parser.cc"
    break;

  case 389:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11421 "Parser/parser.cc"
    break;

  case 390:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 11427 "Parser/parser.cc"
    break;

  case 391:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 11433 "Parser/parser.cc"
    break;

  case 392:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 11439 "Parser/parser.cc"
    break;

  case 394:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11445 "Parser/parser.cc"
    break;

  case 395:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11451 "Parser/parser.cc"
    break;

  case 396:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11457 "Parser/parser.cc"
    break;

  case 401:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 11463 "Parser/parser.cc"
    break;

  case 402:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11469 "Parser/parser.cc"
    break;

  case 403:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11475 "Parser/parser.cc"
    break;

  case 404:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11481 "Parser/parser.cc"
    break;

  case 405:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 11487 "Parser/parser.cc"
    break;

  case 406:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 11493 "Parser/parser.cc"
    break;

  case 407:
#line 1918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 11499 "Parser/parser.cc"
    break;

  case 408:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11505 "Parser/parser.cc"
    break;

  case 411:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11511 "Parser/parser.cc"
    break;

  case 412:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 11517 "Parser/parser.cc"
    break;

  case 413:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 11526 "Parser/parser.cc"
    break;

  case 414:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11532 "Parser/parser.cc"
    break;

  case 415:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11538 "Parser/parser.cc"
    break;

  case 416:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11544 "Parser/parser.cc"
    break;

  case 417:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 11553 "Parser/parser.cc"
    break;

  case 418:
#line 1959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 11562 "Parser/parser.cc"
    break;

  case 419:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11568 "Parser/parser.cc"
    break;

  case 422:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 11574 "Parser/parser.cc"
    break;

  case 423:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11580 "Parser/parser.cc"
    break;

  case 425:
#line 1987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11586 "Parser/parser.cc"
    break;

  case 426:
#line 1989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 11592 "Parser/parser.cc"
    break;

  case 436:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 11598 "Parser/parser.cc"
    break;

  case 437:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 11604 "Parser/parser.cc"
    break;

  case 441:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11610 "Parser/parser.cc"
    break;

  case 443:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 11616 "Parser/parser.cc"
    break;

  case 444:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11622 "Parser/parser.cc"
    break;

  case 445:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 11628 "Parser/parser.cc"
    break;

  case 446:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11634 "Parser/parser.cc"
    break;

  case 447:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11640 "Parser/parser.cc"
    break;

  case 448:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11646 "Parser/parser.cc"
    break;

  case 449:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11652 "Parser/parser.cc"
    break;

  case 450:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11658 "Parser/parser.cc"
    break;

  case 452:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11664 "Parser/parser.cc"
    break;

  case 453:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11670 "Parser/parser.cc"
    break;

  case 454:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11676 "Parser/parser.cc"
    break;

  case 455:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 11687 "Parser/parser.cc"
    break;

  case 456:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11693 "Parser/parser.cc"
    break;

  case 457:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11699 "Parser/parser.cc"
    break;

  case 458:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11705 "Parser/parser.cc"
    break;

  case 459:
#line 2107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11711 "Parser/parser.cc"
    break;

  case 460:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 11717 "Parser/parser.cc"
    break;

  case 461:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) ); }
#line 11723 "Parser/parser.cc"
    break;

  case 462:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 11732 "Parser/parser.cc"
    break;

  case 463:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 11741 "Parser/parser.cc"
    break;

  case 464:
#line 2130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 11750 "Parser/parser.cc"
    break;

  case 465:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 11761 "Parser/parser.cc"
    break;

  case 466:
#line 2148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 11770 "Parser/parser.cc"
    break;

  case 467:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11776 "Parser/parser.cc"
    break;

  case 468:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11782 "Parser/parser.cc"
    break;

  case 469:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 11788 "Parser/parser.cc"
    break;

  case 470:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 11796 "Parser/parser.cc"
    break;

  case 471:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 11804 "Parser/parser.cc"
    break;

  case 472:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 11810 "Parser/parser.cc"
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
#line 11825 "Parser/parser.cc"
    break;

  case 476:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11831 "Parser/parser.cc"
    break;

  case 477:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11837 "Parser/parser.cc"
    break;

  case 478:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 11843 "Parser/parser.cc"
    break;

  case 479:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 11849 "Parser/parser.cc"
    break;

  case 480:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 11855 "Parser/parser.cc"
    break;

  case 486:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 11865 "Parser/parser.cc"
    break;

  case 499:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11871 "Parser/parser.cc"
    break;

  case 502:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11877 "Parser/parser.cc"
    break;

  case 503:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11883 "Parser/parser.cc"
    break;

  case 505:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 11889 "Parser/parser.cc"
    break;

  case 506:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 11895 "Parser/parser.cc"
    break;

  case 507:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 11901 "Parser/parser.cc"
    break;

  case 508:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 11907 "Parser/parser.cc"
    break;

  case 509:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 11913 "Parser/parser.cc"
    break;

  case 510:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11919 "Parser/parser.cc"
    break;

  case 512:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11925 "Parser/parser.cc"
    break;

  case 513:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11931 "Parser/parser.cc"
    break;

  case 515:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11937 "Parser/parser.cc"
    break;

  case 516:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 11943 "Parser/parser.cc"
    break;

  case 517:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 11949 "Parser/parser.cc"
    break;

  case 518:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 11955 "Parser/parser.cc"
    break;

  case 519:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 11961 "Parser/parser.cc"
    break;

  case 520:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 11967 "Parser/parser.cc"
    break;

  case 521:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 11973 "Parser/parser.cc"
    break;

  case 522:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 11979 "Parser/parser.cc"
    break;

  case 523:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 11985 "Parser/parser.cc"
    break;

  case 524:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11991 "Parser/parser.cc"
    break;

  case 525:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11997 "Parser/parser.cc"
    break;

  case 526:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 12003 "Parser/parser.cc"
    break;

  case 527:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 12009 "Parser/parser.cc"
    break;

  case 528:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 12015 "Parser/parser.cc"
    break;

  case 529:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 12021 "Parser/parser.cc"
    break;

  case 530:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 12027 "Parser/parser.cc"
    break;

  case 531:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 12033 "Parser/parser.cc"
    break;

  case 532:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 12039 "Parser/parser.cc"
    break;

  case 533:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 12045 "Parser/parser.cc"
    break;

  case 534:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float80 ); }
#line 12051 "Parser/parser.cc"
    break;

  case 535:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 12057 "Parser/parser.cc"
    break;

  case 536:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float16 ); }
#line 12063 "Parser/parser.cc"
    break;

  case 537:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32 ); }
#line 12069 "Parser/parser.cc"
    break;

  case 538:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x ); }
#line 12075 "Parser/parser.cc"
    break;

  case 539:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64 ); }
#line 12081 "Parser/parser.cc"
    break;

  case 540:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x ); }
#line 12087 "Parser/parser.cc"
    break;

  case 541:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128 ); }
#line 12093 "Parser/parser.cc"
    break;

  case 542:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128x ); }
#line 12099 "Parser/parser.cc"
    break;

  case 543:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x4 ); }
#line 12105 "Parser/parser.cc"
    break;

  case 544:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x2 ); }
#line 12111 "Parser/parser.cc"
    break;

  case 545:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat32 ); }
#line 12117 "Parser/parser.cc"
    break;

  case 546:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat64 ); }
#line 12123 "Parser/parser.cc"
    break;

  case 547:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svbool ); }
#line 12129 "Parser/parser.cc"
    break;

  case 548:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12135 "Parser/parser.cc"
    break;

  case 549:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12141 "Parser/parser.cc"
    break;

  case 550:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12147 "Parser/parser.cc"
    break;

  case 551:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 12153 "Parser/parser.cc"
    break;

  case 552:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 12159 "Parser/parser.cc"
    break;

  case 553:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 12165 "Parser/parser.cc"
    break;

  case 554:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 12171 "Parser/parser.cc"
    break;

  case 555:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 12177 "Parser/parser.cc"
    break;

  case 556:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 12183 "Parser/parser.cc"
    break;

  case 557:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 12189 "Parser/parser.cc"
    break;

  case 558:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 12195 "Parser/parser.cc"
    break;

  case 560:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12201 "Parser/parser.cc"
    break;

  case 562:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 12207 "Parser/parser.cc"
    break;

  case 563:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12213 "Parser/parser.cc"
    break;

  case 564:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12219 "Parser/parser.cc"
    break;

  case 566:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12225 "Parser/parser.cc"
    break;

  case 567:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12231 "Parser/parser.cc"
    break;

  case 568:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12237 "Parser/parser.cc"
    break;

  case 569:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 12243 "Parser/parser.cc"
    break;

  case 571:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12249 "Parser/parser.cc"
    break;

  case 573:
#line 2463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12255 "Parser/parser.cc"
    break;

  case 574:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12261 "Parser/parser.cc"
    break;

  case 575:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 12267 "Parser/parser.cc"
    break;

  case 576:
#line 2472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12273 "Parser/parser.cc"
    break;

  case 577:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 12279 "Parser/parser.cc"
    break;

  case 578:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 12285 "Parser/parser.cc"
    break;

  case 579:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 12291 "Parser/parser.cc"
    break;

  case 580:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 12297 "Parser/parser.cc"
    break;

  case 581:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 12303 "Parser/parser.cc"
    break;

  case 583:
#line 2488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12309 "Parser/parser.cc"
    break;

  case 584:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12315 "Parser/parser.cc"
    break;

  case 585:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12321 "Parser/parser.cc"
    break;

  case 587:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 12327 "Parser/parser.cc"
    break;

  case 588:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12333 "Parser/parser.cc"
    break;

  case 589:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 12342 "Parser/parser.cc"
    break;

  case 591:
#line 2511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12348 "Parser/parser.cc"
    break;

  case 592:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12354 "Parser/parser.cc"
    break;

  case 593:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12360 "Parser/parser.cc"
    break;

  case 595:
#line 2521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12366 "Parser/parser.cc"
    break;

  case 596:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12372 "Parser/parser.cc"
    break;

  case 598:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12378 "Parser/parser.cc"
    break;

  case 599:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12384 "Parser/parser.cc"
    break;

  case 600:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12390 "Parser/parser.cc"
    break;

  case 601:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12396 "Parser/parser.cc"
    break;

  case 602:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12402 "Parser/parser.cc"
    break;

  case 603:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12408 "Parser/parser.cc"
    break;

  case 604:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 12414 "Parser/parser.cc"
    break;

  case 605:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 12420 "Parser/parser.cc"
    break;

  case 606:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 12426 "Parser/parser.cc"
    break;

  case 608:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 12432 "Parser/parser.cc"
    break;

  case 609:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 12438 "Parser/parser.cc"
    break;

  case 610:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 12444 "Parser/parser.cc"
    break;

  case 611:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 12450 "Parser/parser.cc"
    break;

  case 612:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12456 "Parser/parser.cc"
    break;

  case 617:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 12462 "Parser/parser.cc"
    break;

  case 618:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12468 "Parser/parser.cc"
    break;

  case 619:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 12477 "Parser/parser.cc"
    break;

  case 620:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 12485 "Parser/parser.cc"
    break;

  case 621:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 12494 "Parser/parser.cc"
    break;

  case 622:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 12503 "Parser/parser.cc"
    break;

  case 623:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 12512 "Parser/parser.cc"
    break;

  case 624:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 12521 "Parser/parser.cc"
    break;

  case 626:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12527 "Parser/parser.cc"
    break;

  case 627:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12533 "Parser/parser.cc"
    break;

  case 628:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12543 "Parser/parser.cc"
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
#line 12562 "Parser/parser.cc"
    break;

  case 632:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 12568 "Parser/parser.cc"
    break;

  case 633:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 12574 "Parser/parser.cc"
    break;

  case 634:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 12580 "Parser/parser.cc"
    break;

  case 635:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 12586 "Parser/parser.cc"
    break;

  case 636:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 12592 "Parser/parser.cc"
    break;

  case 637:
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 12598 "Parser/parser.cc"
    break;

  case 638:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12607 "Parser/parser.cc"
    break;

  case 639:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 12613 "Parser/parser.cc"
    break;

  case 640:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12622 "Parser/parser.cc"
    break;

  case 641:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 12628 "Parser/parser.cc"
    break;

  case 642:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12637 "Parser/parser.cc"
    break;

  case 643:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12643 "Parser/parser.cc"
    break;

  case 644:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 12649 "Parser/parser.cc"
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
#line 12662 "Parser/parser.cc"
    break;

  case 646:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 12671 "Parser/parser.cc"
    break;

  case 647:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 12677 "Parser/parser.cc"
    break;

  case 648:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12683 "Parser/parser.cc"
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
#line 12696 "Parser/parser.cc"
    break;

  case 650:
#line 2726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12702 "Parser/parser.cc"
    break;

  case 653:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 12708 "Parser/parser.cc"
    break;

  case 654:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12714 "Parser/parser.cc"
    break;

  case 657:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12720 "Parser/parser.cc"
    break;

  case 659:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 12726 "Parser/parser.cc"
    break;

  case 660:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 12732 "Parser/parser.cc"
    break;

  case 661:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12738 "Parser/parser.cc"
    break;

  case 662:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12744 "Parser/parser.cc"
    break;

  case 663:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12750 "Parser/parser.cc"
    break;

  case 664:
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12756 "Parser/parser.cc"
    break;

  case 666:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 12762 "Parser/parser.cc"
    break;

  case 668:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 12768 "Parser/parser.cc"
    break;

  case 669:
#line 2777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12774 "Parser/parser.cc"
    break;

  case 671:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 12780 "Parser/parser.cc"
    break;

  case 672:
#line 2789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12786 "Parser/parser.cc"
    break;

  case 674:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 12792 "Parser/parser.cc"
    break;

  case 675:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 12803 "Parser/parser.cc"
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
#line 12817 "Parser/parser.cc"
    break;

  case 677:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 12823 "Parser/parser.cc"
    break;

  case 678:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 12829 "Parser/parser.cc"
    break;

  case 679:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 12835 "Parser/parser.cc"
    break;

  case 680:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 12846 "Parser/parser.cc"
    break;

  case 681:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 12852 "Parser/parser.cc"
    break;

  case 682:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12858 "Parser/parser.cc"
    break;

  case 684:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12864 "Parser/parser.cc"
    break;

  case 685:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12870 "Parser/parser.cc"
    break;

  case 686:
#line 2852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 12876 "Parser/parser.cc"
    break;

  case 687:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 12882 "Parser/parser.cc"
    break;

  case 688:
#line 2859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12891 "Parser/parser.cc"
    break;

  case 689:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12900 "Parser/parser.cc"
    break;

  case 690:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 12906 "Parser/parser.cc"
    break;

  case 691:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 12912 "Parser/parser.cc"
    break;

  case 692:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 12922 "Parser/parser.cc"
    break;

  case 693:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 12928 "Parser/parser.cc"
    break;

  case 694:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 12934 "Parser/parser.cc"
    break;

  case 696:
#line 2890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 12940 "Parser/parser.cc"
    break;

  case 697:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12946 "Parser/parser.cc"
    break;

  case 698:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12952 "Parser/parser.cc"
    break;

  case 699:
#line 2897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12958 "Parser/parser.cc"
    break;

  case 700:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 12964 "Parser/parser.cc"
    break;

  case 701:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12970 "Parser/parser.cc"
    break;

  case 703:
#line 2911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12976 "Parser/parser.cc"
    break;

  case 706:
#line 2918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12982 "Parser/parser.cc"
    break;

  case 707:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12988 "Parser/parser.cc"
    break;

  case 708:
#line 2925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 12994 "Parser/parser.cc"
    break;

  case 709:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13000 "Parser/parser.cc"
    break;

  case 712:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13006 "Parser/parser.cc"
    break;

  case 713:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13012 "Parser/parser.cc"
    break;

  case 714:
#line 2935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13018 "Parser/parser.cc"
    break;

  case 716:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13024 "Parser/parser.cc"
    break;

  case 717:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13030 "Parser/parser.cc"
    break;

  case 718:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 13036 "Parser/parser.cc"
    break;

  case 720:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13042 "Parser/parser.cc"
    break;

  case 721:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13048 "Parser/parser.cc"
    break;

  case 722:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13054 "Parser/parser.cc"
    break;

  case 723:
#line 2969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13060 "Parser/parser.cc"
    break;

  case 724:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13066 "Parser/parser.cc"
    break;

  case 726:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13072 "Parser/parser.cc"
    break;

  case 727:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13078 "Parser/parser.cc"
    break;

  case 728:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13084 "Parser/parser.cc"
    break;

  case 733:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13090 "Parser/parser.cc"
    break;

  case 735:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13096 "Parser/parser.cc"
    break;

  case 736:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 13102 "Parser/parser.cc"
    break;

  case 739:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 13108 "Parser/parser.cc"
    break;

  case 742:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13114 "Parser/parser.cc"
    break;

  case 743:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 13120 "Parser/parser.cc"
    break;

  case 744:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 13126 "Parser/parser.cc"
    break;

  case 745:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13132 "Parser/parser.cc"
    break;

  case 746:
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13138 "Parser/parser.cc"
    break;

  case 747:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13144 "Parser/parser.cc"
    break;

  case 748:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13150 "Parser/parser.cc"
    break;

  case 750:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 13156 "Parser/parser.cc"
    break;

  case 751:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 13162 "Parser/parser.cc"
    break;

  case 752:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 13168 "Parser/parser.cc"
    break;

  case 754:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 13174 "Parser/parser.cc"
    break;

  case 756:
#line 3060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 13180 "Parser/parser.cc"
    break;

  case 757:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 13186 "Parser/parser.cc"
    break;

  case 758:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13192 "Parser/parser.cc"
    break;

  case 759:
#line 3071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13198 "Parser/parser.cc"
    break;

  case 760:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 13204 "Parser/parser.cc"
    break;

  case 761:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13210 "Parser/parser.cc"
    break;

  case 763:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13216 "Parser/parser.cc"
    break;

  case 764:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13222 "Parser/parser.cc"
    break;

  case 765:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13228 "Parser/parser.cc"
    break;

  case 766:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 13239 "Parser/parser.cc"
    break;

  case 767:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13245 "Parser/parser.cc"
    break;

  case 768:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 13251 "Parser/parser.cc"
    break;

  case 769:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13257 "Parser/parser.cc"
    break;

  case 770:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 13266 "Parser/parser.cc"
    break;

  case 771:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 13272 "Parser/parser.cc"
    break;

  case 772:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 13282 "Parser/parser.cc"
    break;

  case 773:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13288 "Parser/parser.cc"
    break;

  case 774:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13294 "Parser/parser.cc"
    break;

  case 775:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 13300 "Parser/parser.cc"
    break;

  case 776:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13306 "Parser/parser.cc"
    break;

  case 777:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13312 "Parser/parser.cc"
    break;

  case 778:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13318 "Parser/parser.cc"
    break;

  case 779:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 13324 "Parser/parser.cc"
    break;

  case 780:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13330 "Parser/parser.cc"
    break;

  case 781:
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13336 "Parser/parser.cc"
    break;

  case 784:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13342 "Parser/parser.cc"
    break;

  case 785:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13348 "Parser/parser.cc"
    break;

  case 786:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13354 "Parser/parser.cc"
    break;

  case 787:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13360 "Parser/parser.cc"
    break;

  case 789:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13366 "Parser/parser.cc"
    break;

  case 790:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 13372 "Parser/parser.cc"
    break;

  case 791:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13378 "Parser/parser.cc"
    break;

  case 792:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13384 "Parser/parser.cc"
    break;

  case 793:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 13390 "Parser/parser.cc"
    break;

  case 794:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 13396 "Parser/parser.cc"
    break;

  case 795:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 13402 "Parser/parser.cc"
    break;

  case 796:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 13411 "Parser/parser.cc"
    break;

  case 797:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 13420 "Parser/parser.cc"
    break;

  case 798:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 13429 "Parser/parser.cc"
    break;

  case 799:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 13435 "Parser/parser.cc"
    break;

  case 800:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-6].tok), (yyvsp[-4].decl), (yyvsp[-1].decl) );
		}
#line 13444 "Parser/parser.cc"
    break;

  case 801:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-3].tok), (yyvsp[-5].decl), (yyvsp[-1].decl) ); }
#line 13450 "Parser/parser.cc"
    break;

  case 803:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13456 "Parser/parser.cc"
    break;

  case 808:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13462 "Parser/parser.cc"
    break;

  case 809:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 13468 "Parser/parser.cc"
    break;

  case 810:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 13474 "Parser/parser.cc"
    break;

  case 811:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Possible cause is declaring an aggregate or enumeration type in a trait." ); (yyval.decl) = nullptr; }
#line 13480 "Parser/parser.cc"
    break;

  case 813:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 13486 "Parser/parser.cc"
    break;

  case 814:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13492 "Parser/parser.cc"
    break;

  case 815:
#line 3280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 13498 "Parser/parser.cc"
    break;

  case 816:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13504 "Parser/parser.cc"
    break;

  case 818:
#line 3290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 13510 "Parser/parser.cc"
    break;

  case 819:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 13516 "Parser/parser.cc"
    break;

  case 820:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 13522 "Parser/parser.cc"
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
#line 13538 "Parser/parser.cc"
    break;

  case 822:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 13544 "Parser/parser.cc"
    break;

  case 823:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 13550 "Parser/parser.cc"
    break;

  case 824:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 13556 "Parser/parser.cc"
    break;

  case 825:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13562 "Parser/parser.cc"
    break;

  case 826:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13568 "Parser/parser.cc"
    break;

  case 827:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13574 "Parser/parser.cc"
    break;

  case 829:
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 13583 "Parser/parser.cc"
    break;

  case 830:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 13589 "Parser/parser.cc"
    break;

  case 831:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 13598 "Parser/parser.cc"
    break;

  case 832:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 13608 "Parser/parser.cc"
    break;

  case 833:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 13617 "Parser/parser.cc"
    break;

  case 834:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13627 "Parser/parser.cc"
    break;

  case 835:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 13638 "Parser/parser.cc"
    break;

  case 836:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13648 "Parser/parser.cc"
    break;

  case 837:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 13659 "Parser/parser.cc"
    break;

  case 838:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13669 "Parser/parser.cc"
    break;

  case 839:
#line 3382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 13680 "Parser/parser.cc"
    break;

  case 840:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13690 "Parser/parser.cc"
    break;

  case 841:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13696 "Parser/parser.cc"
    break;

  case 843:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 13702 "Parser/parser.cc"
    break;

  case 844:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 13708 "Parser/parser.cc"
    break;

  case 845:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 13714 "Parser/parser.cc"
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
#line 13726 "Parser/parser.cc"
    break;

  case 847:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13737 "Parser/parser.cc"
    break;

  case 848:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 13746 "Parser/parser.cc"
    break;

  case 849:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 13755 "Parser/parser.cc"
    break;

  case 850:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13761 "Parser/parser.cc"
    break;

  case 851:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13767 "Parser/parser.cc"
    break;

  case 852:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13773 "Parser/parser.cc"
    break;

  case 853:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 13782 "Parser/parser.cc"
    break;

  case 854:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13788 "Parser/parser.cc"
    break;

  case 855:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 13794 "Parser/parser.cc"
    break;

  case 856:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 13800 "Parser/parser.cc"
    break;

  case 861:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 13806 "Parser/parser.cc"
    break;

  case 862:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13812 "Parser/parser.cc"
    break;

  case 863:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 13822 "Parser/parser.cc"
    break;

  case 864:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13828 "Parser/parser.cc"
    break;

  case 867:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13834 "Parser/parser.cc"
    break;

  case 868:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 13840 "Parser/parser.cc"
    break;

  case 869:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13846 "Parser/parser.cc"
    break;

  case 870:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13852 "Parser/parser.cc"
    break;

  case 872:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13858 "Parser/parser.cc"
    break;

  case 873:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13864 "Parser/parser.cc"
    break;

  case 874:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 13870 "Parser/parser.cc"
    break;

  case 875:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13876 "Parser/parser.cc"
    break;

  case 877:
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 13882 "Parser/parser.cc"
    break;

  case 878:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 13888 "Parser/parser.cc"
    break;

  case 879:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13894 "Parser/parser.cc"
    break;

  case 880:
#line 3575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13900 "Parser/parser.cc"
    break;

  case 881:
#line 3577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13906 "Parser/parser.cc"
    break;

  case 882:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13912 "Parser/parser.cc"
    break;

  case 884:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13918 "Parser/parser.cc"
    break;

  case 885:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13924 "Parser/parser.cc"
    break;

  case 886:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13930 "Parser/parser.cc"
    break;

  case 887:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13936 "Parser/parser.cc"
    break;

  case 888:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13942 "Parser/parser.cc"
    break;

  case 889:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13948 "Parser/parser.cc"
    break;

  case 890:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13954 "Parser/parser.cc"
    break;

  case 891:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13960 "Parser/parser.cc"
    break;

  case 892:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13966 "Parser/parser.cc"
    break;

  case 893:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13972 "Parser/parser.cc"
    break;

  case 894:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13978 "Parser/parser.cc"
    break;

  case 895:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13984 "Parser/parser.cc"
    break;

  case 896:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13990 "Parser/parser.cc"
    break;

  case 897:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13996 "Parser/parser.cc"
    break;

  case 898:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14002 "Parser/parser.cc"
    break;

  case 899:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14008 "Parser/parser.cc"
    break;

  case 900:
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14014 "Parser/parser.cc"
    break;

  case 901:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14020 "Parser/parser.cc"
    break;

  case 903:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14026 "Parser/parser.cc"
    break;

  case 904:
#line 3643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14032 "Parser/parser.cc"
    break;

  case 905:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14038 "Parser/parser.cc"
    break;

  case 906:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14044 "Parser/parser.cc"
    break;

  case 907:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14050 "Parser/parser.cc"
    break;

  case 908:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14056 "Parser/parser.cc"
    break;

  case 909:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14062 "Parser/parser.cc"
    break;

  case 910:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14068 "Parser/parser.cc"
    break;

  case 911:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14074 "Parser/parser.cc"
    break;

  case 912:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14080 "Parser/parser.cc"
    break;

  case 913:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14086 "Parser/parser.cc"
    break;

  case 914:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14092 "Parser/parser.cc"
    break;

  case 915:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14098 "Parser/parser.cc"
    break;

  case 916:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14104 "Parser/parser.cc"
    break;

  case 917:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14110 "Parser/parser.cc"
    break;

  case 918:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14116 "Parser/parser.cc"
    break;

  case 922:
#line 3695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 14122 "Parser/parser.cc"
    break;

  case 923:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14128 "Parser/parser.cc"
    break;

  case 924:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14134 "Parser/parser.cc"
    break;

  case 925:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14140 "Parser/parser.cc"
    break;

  case 926:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14146 "Parser/parser.cc"
    break;

  case 927:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14152 "Parser/parser.cc"
    break;

  case 928:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14158 "Parser/parser.cc"
    break;

  case 929:
#line 3712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14164 "Parser/parser.cc"
    break;

  case 930:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14170 "Parser/parser.cc"
    break;

  case 931:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14176 "Parser/parser.cc"
    break;

  case 932:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14182 "Parser/parser.cc"
    break;

  case 933:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14188 "Parser/parser.cc"
    break;

  case 934:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14194 "Parser/parser.cc"
    break;

  case 935:
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14200 "Parser/parser.cc"
    break;

  case 936:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14206 "Parser/parser.cc"
    break;

  case 937:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 14215 "Parser/parser.cc"
    break;

  case 938:
#line 3746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14221 "Parser/parser.cc"
    break;

  case 939:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14227 "Parser/parser.cc"
    break;

  case 941:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14233 "Parser/parser.cc"
    break;

  case 942:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14239 "Parser/parser.cc"
    break;

  case 943:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14245 "Parser/parser.cc"
    break;

  case 944:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14251 "Parser/parser.cc"
    break;

  case 945:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14257 "Parser/parser.cc"
    break;

  case 946:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14263 "Parser/parser.cc"
    break;

  case 947:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14269 "Parser/parser.cc"
    break;

  case 948:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14275 "Parser/parser.cc"
    break;

  case 949:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14281 "Parser/parser.cc"
    break;

  case 950:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14287 "Parser/parser.cc"
    break;

  case 951:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14293 "Parser/parser.cc"
    break;

  case 952:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14299 "Parser/parser.cc"
    break;

  case 953:
#line 3784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14305 "Parser/parser.cc"
    break;

  case 954:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14311 "Parser/parser.cc"
    break;

  case 955:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14317 "Parser/parser.cc"
    break;

  case 956:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14323 "Parser/parser.cc"
    break;

  case 957:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14329 "Parser/parser.cc"
    break;

  case 958:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14335 "Parser/parser.cc"
    break;

  case 960:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14341 "Parser/parser.cc"
    break;

  case 961:
#line 3812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14347 "Parser/parser.cc"
    break;

  case 962:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14353 "Parser/parser.cc"
    break;

  case 963:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14359 "Parser/parser.cc"
    break;

  case 964:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14365 "Parser/parser.cc"
    break;

  case 965:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14371 "Parser/parser.cc"
    break;

  case 966:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14377 "Parser/parser.cc"
    break;

  case 967:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14383 "Parser/parser.cc"
    break;

  case 968:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14389 "Parser/parser.cc"
    break;

  case 969:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14395 "Parser/parser.cc"
    break;

  case 970:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14401 "Parser/parser.cc"
    break;

  case 971:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14407 "Parser/parser.cc"
    break;

  case 972:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14413 "Parser/parser.cc"
    break;

  case 973:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14419 "Parser/parser.cc"
    break;

  case 974:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14425 "Parser/parser.cc"
    break;

  case 975:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14431 "Parser/parser.cc"
    break;

  case 976:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14437 "Parser/parser.cc"
    break;

  case 977:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14444 "Parser/parser.cc"
    break;

  case 979:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14450 "Parser/parser.cc"
    break;

  case 980:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14456 "Parser/parser.cc"
    break;

  case 981:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14462 "Parser/parser.cc"
    break;

  case 982:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14468 "Parser/parser.cc"
    break;

  case 983:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14474 "Parser/parser.cc"
    break;

  case 984:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14480 "Parser/parser.cc"
    break;

  case 985:
#line 3880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14486 "Parser/parser.cc"
    break;

  case 986:
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14492 "Parser/parser.cc"
    break;

  case 987:
#line 3884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14498 "Parser/parser.cc"
    break;

  case 988:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14504 "Parser/parser.cc"
    break;

  case 989:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14510 "Parser/parser.cc"
    break;

  case 990:
#line 3893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14516 "Parser/parser.cc"
    break;

  case 991:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14522 "Parser/parser.cc"
    break;

  case 992:
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14529 "Parser/parser.cc"
    break;

  case 994:
#line 3913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14535 "Parser/parser.cc"
    break;

  case 995:
#line 3915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14541 "Parser/parser.cc"
    break;

  case 996:
#line 3920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14547 "Parser/parser.cc"
    break;

  case 997:
#line 3922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14553 "Parser/parser.cc"
    break;

  case 998:
#line 3927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14559 "Parser/parser.cc"
    break;

  case 999:
#line 3929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14565 "Parser/parser.cc"
    break;

  case 1000:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14571 "Parser/parser.cc"
    break;

  case 1001:
#line 3936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14577 "Parser/parser.cc"
    break;

  case 1002:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14583 "Parser/parser.cc"
    break;

  case 1003:
#line 3943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14589 "Parser/parser.cc"
    break;

  case 1004:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14595 "Parser/parser.cc"
    break;

  case 1006:
#line 3963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14601 "Parser/parser.cc"
    break;

  case 1007:
#line 3965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14607 "Parser/parser.cc"
    break;

  case 1008:
#line 3970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 14613 "Parser/parser.cc"
    break;

  case 1009:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 14619 "Parser/parser.cc"
    break;

  case 1010:
#line 3974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14625 "Parser/parser.cc"
    break;

  case 1011:
#line 3976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14631 "Parser/parser.cc"
    break;

  case 1012:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14637 "Parser/parser.cc"
    break;

  case 1014:
#line 3984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14643 "Parser/parser.cc"
    break;

  case 1015:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14649 "Parser/parser.cc"
    break;

  case 1016:
#line 3988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14655 "Parser/parser.cc"
    break;

  case 1017:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 14661 "Parser/parser.cc"
    break;

  case 1018:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14667 "Parser/parser.cc"
    break;

  case 1019:
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14673 "Parser/parser.cc"
    break;

  case 1020:
#line 4003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 14679 "Parser/parser.cc"
    break;

  case 1021:
#line 4005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 14685 "Parser/parser.cc"
    break;

  case 1022:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-3].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 14691 "Parser/parser.cc"
    break;

  case 1023:
#line 4015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 14697 "Parser/parser.cc"
    break;

  case 1025:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 14703 "Parser/parser.cc"
    break;

  case 1026:
#line 4028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 14709 "Parser/parser.cc"
    break;

  case 1028:
#line 4031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 14715 "Parser/parser.cc"
    break;

  case 1029:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 14721 "Parser/parser.cc"
    break;

  case 1031:
#line 4039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 14727 "Parser/parser.cc"
    break;

  case 1032:
#line 4041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 14733 "Parser/parser.cc"
    break;

  case 1033:
#line 4046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 14739 "Parser/parser.cc"
    break;

  case 1034:
#line 4048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 14745 "Parser/parser.cc"
    break;

  case 1035:
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 14751 "Parser/parser.cc"
    break;

  case 1036:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 14757 "Parser/parser.cc"
    break;

  case 1037:
#line 4086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14763 "Parser/parser.cc"
    break;

  case 1040:
#line 4093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 14770 "Parser/parser.cc"
    break;

  case 1041:
#line 4096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14776 "Parser/parser.cc"
    break;

  case 1042:
#line 4098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14782 "Parser/parser.cc"
    break;

  case 1043:
#line 4103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 14788 "Parser/parser.cc"
    break;

  case 1044:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 14794 "Parser/parser.cc"
    break;

  case 1045:
#line 4107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14800 "Parser/parser.cc"
    break;

  case 1046:
#line 4109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14806 "Parser/parser.cc"
    break;

  case 1047:
#line 4111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14812 "Parser/parser.cc"
    break;

  case 1049:
#line 4117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14818 "Parser/parser.cc"
    break;

  case 1050:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14824 "Parser/parser.cc"
    break;

  case 1051:
#line 4121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14830 "Parser/parser.cc"
    break;

  case 1052:
#line 4126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 14836 "Parser/parser.cc"
    break;

  case 1053:
#line 4128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14842 "Parser/parser.cc"
    break;

  case 1054:
#line 4130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14848 "Parser/parser.cc"
    break;

  case 1056:
#line 4137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14854 "Parser/parser.cc"
    break;

  case 1058:
#line 4148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 14860 "Parser/parser.cc"
    break;

  case 1059:
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14866 "Parser/parser.cc"
    break;

  case 1060:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 14872 "Parser/parser.cc"
    break;

  case 1061:
#line 4156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14878 "Parser/parser.cc"
    break;

  case 1062:
#line 4158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14884 "Parser/parser.cc"
    break;

  case 1063:
#line 4160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 14890 "Parser/parser.cc"
    break;

  case 1065:
#line 4175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14896 "Parser/parser.cc"
    break;

  case 1066:
#line 4177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14902 "Parser/parser.cc"
    break;

  case 1067:
#line 4182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 14908 "Parser/parser.cc"
    break;

  case 1068:
#line 4184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 14914 "Parser/parser.cc"
    break;

  case 1069:
#line 4186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14920 "Parser/parser.cc"
    break;

  case 1070:
#line 4188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14926 "Parser/parser.cc"
    break;

  case 1071:
#line 4190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14932 "Parser/parser.cc"
    break;

  case 1073:
#line 4196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14938 "Parser/parser.cc"
    break;

  case 1074:
#line 4198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14944 "Parser/parser.cc"
    break;

  case 1075:
#line 4200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14950 "Parser/parser.cc"
    break;

  case 1076:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14956 "Parser/parser.cc"
    break;

  case 1077:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14962 "Parser/parser.cc"
    break;

  case 1080:
#line 4217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14968 "Parser/parser.cc"
    break;

  case 1083:
#line 4228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14974 "Parser/parser.cc"
    break;

  case 1084:
#line 4230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14980 "Parser/parser.cc"
    break;

  case 1085:
#line 4232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14986 "Parser/parser.cc"
    break;

  case 1086:
#line 4234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14992 "Parser/parser.cc"
    break;

  case 1087:
#line 4236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14998 "Parser/parser.cc"
    break;

  case 1088:
#line 4238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15004 "Parser/parser.cc"
    break;

  case 1089:
#line 4245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15010 "Parser/parser.cc"
    break;

  case 1090:
#line 4247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15016 "Parser/parser.cc"
    break;

  case 1091:
#line 4249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15022 "Parser/parser.cc"
    break;

  case 1092:
#line 4251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15028 "Parser/parser.cc"
    break;

  case 1093:
#line 4253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15034 "Parser/parser.cc"
    break;

  case 1094:
#line 4256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15040 "Parser/parser.cc"
    break;

  case 1095:
#line 4258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15046 "Parser/parser.cc"
    break;

  case 1096:
#line 4260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15052 "Parser/parser.cc"
    break;

  case 1097:
#line 4262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15058 "Parser/parser.cc"
    break;

  case 1098:
#line 4264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15064 "Parser/parser.cc"
    break;

  case 1099:
#line 4269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-2].decl) ); }
#line 15070 "Parser/parser.cc"
    break;

  case 1100:
#line 4271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), false ); }
#line 15076 "Parser/parser.cc"
    break;

  case 1101:
#line 4276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), true ); }
#line 15082 "Parser/parser.cc"
    break;

  case 1102:
#line 4278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) ), true ); }
#line 15088 "Parser/parser.cc"
    break;

  case 1104:
#line 4305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15094 "Parser/parser.cc"
    break;

  case 1108:
#line 4316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15100 "Parser/parser.cc"
    break;

  case 1109:
#line 4318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15106 "Parser/parser.cc"
    break;

  case 1110:
#line 4320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15112 "Parser/parser.cc"
    break;

  case 1111:
#line 4322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15118 "Parser/parser.cc"
    break;

  case 1112:
#line 4324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15124 "Parser/parser.cc"
    break;

  case 1113:
#line 4326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15130 "Parser/parser.cc"
    break;

  case 1114:
#line 4333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15136 "Parser/parser.cc"
    break;

  case 1115:
#line 4335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15142 "Parser/parser.cc"
    break;

  case 1116:
#line 4337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15148 "Parser/parser.cc"
    break;

  case 1117:
#line 4339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15154 "Parser/parser.cc"
    break;

  case 1118:
#line 4341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15160 "Parser/parser.cc"
    break;

  case 1119:
#line 4343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15166 "Parser/parser.cc"
    break;

  case 1120:
#line 4348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 15172 "Parser/parser.cc"
    break;

  case 1121:
#line 4350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15178 "Parser/parser.cc"
    break;

  case 1122:
#line 4352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15184 "Parser/parser.cc"
    break;

  case 1123:
#line 4357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 15190 "Parser/parser.cc"
    break;

  case 1124:
#line 4359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15196 "Parser/parser.cc"
    break;

  case 1125:
#line 4361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15202 "Parser/parser.cc"
    break;

  case 1128:
#line 4385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 15208 "Parser/parser.cc"
    break;

  case 1129:
#line 4387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 15214 "Parser/parser.cc"
    break;


#line 15218 "Parser/parser.cc"

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
