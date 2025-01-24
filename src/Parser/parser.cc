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
#line 34 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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
	if ( fieldList == nullptr ) {
		if ( !( typeSpec->type && typeSpec->type->kind == TypeData::Aggregate ) ) { // int; no fieldList
			// printf( "fieldDecl1 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
			SemanticWarning( yylloc, Warning::SuperfluousDecl );
			return nullptr;
		} // if
		// printf( "fieldDecl2 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
		fieldList = DeclarationNode::newName( nullptr ); // struct S { ... } no fieldList
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

#line 348 "Parser/parser.cc"

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
    FALLTHROUGH = 349,
    WITH = 350,
    WHEN = 351,
    WAITFOR = 352,
    WAITUNTIL = 353,
    CORUN = 354,
    COFOR = 355,
    DISABLE = 356,
    ENABLE = 357,
    TRY = 358,
    THROW = 359,
    THROWRESUME = 360,
    AT = 361,
    ASM = 362,
    ALIGNAS = 363,
    ALIGNOF = 364,
    GENERIC = 365,
    STATICASSERT = 366,
    IDENTIFIER = 367,
    TYPEDIMname = 368,
    TYPEDEFname = 369,
    TYPEGENname = 370,
    TIMEOUT = 371,
    WAND = 372,
    WOR = 373,
    CATCH = 374,
    RECOVER = 375,
    CATCHRESUME = 376,
    FIXUP = 377,
    FINALLY = 378,
    INTEGERconstant = 379,
    CHARACTERconstant = 380,
    STRINGliteral = 381,
    DIRECTIVE = 382,
    C23_ATTRIBUTE = 383,
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
#define FALLTHROUGH 349
#define WITH 350
#define WHEN 351
#define WAITFOR 352
#define WAITUNTIL 353
#define CORUN 354
#define COFOR 355
#define DISABLE 356
#define ENABLE 357
#define TRY 358
#define THROW 359
#define THROWRESUME 360
#define AT 361
#define ASM 362
#define ALIGNAS 363
#define ALIGNOF 364
#define GENERIC 365
#define STATICASSERT 366
#define IDENTIFIER 367
#define TYPEDIMname 368
#define TYPEDEFname 369
#define TYPEGENname 370
#define TIMEOUT 371
#define WAND 372
#define WOR 373
#define CATCH 374
#define RECOVER 375
#define CATCHRESUME 376
#define FIXUP 377
#define FINALLY 378
#define INTEGERconstant 379
#define CHARACTERconstant 380
#define STRINGliteral 381
#define DIRECTIVE 382
#define C23_ATTRIBUTE 383
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
#line 316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 755 "Parser/parser.cc"

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
#define YYFINAL  157
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   33338

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  189
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1139
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2187

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
       0,   640,   640,   644,   651,   652,   653,   654,   655,   659,
     660,   661,   662,   663,   664,   665,   666,   670,   671,   675,
     676,   681,   682,   683,   687,   691,   692,   703,   705,   707,
     709,   710,   712,   714,   716,   718,   728,   730,   732,   734,
     736,   738,   743,   744,   755,   760,   765,   766,   771,   777,
     779,   781,   787,   789,   791,   793,   795,   815,   818,   820,
     822,   824,   826,   828,   830,   832,   834,   836,   838,   840,
     849,   850,   854,   855,   857,   859,   861,   863,   865,   870,
     872,   874,   882,   883,   891,   894,   895,   897,   902,   918,
     920,   922,   924,   926,   928,   930,   935,   937,   940,   942,
     947,   949,   954,   955,   957,   961,   962,   963,   964,   968,
     969,   971,   973,   975,   977,   979,   981,   983,   990,   991,
     992,   993,   997,   998,  1002,  1003,  1008,  1009,  1011,  1013,
    1018,  1019,  1021,  1026,  1027,  1029,  1034,  1035,  1037,  1039,
    1041,  1046,  1047,  1049,  1054,  1055,  1060,  1061,  1066,  1067,
    1072,  1073,  1078,  1079,  1084,  1085,  1087,  1092,  1097,  1098,
    1102,  1104,  1109,  1112,  1115,  1120,  1121,  1129,  1135,  1136,
    1140,  1141,  1145,  1146,  1150,  1151,  1152,  1153,  1154,  1155,
    1156,  1157,  1158,  1159,  1160,  1166,  1169,  1171,  1173,  1175,
    1180,  1181,  1183,  1185,  1190,  1191,  1197,  1198,  1204,  1205,
    1206,  1207,  1208,  1209,  1210,  1211,  1212,  1213,  1214,  1215,
    1216,  1217,  1219,  1220,  1228,  1230,  1240,  1242,  1250,  1251,
    1256,  1258,  1260,  1262,  1264,  1268,  1269,  1271,  1277,  1306,
    1309,  1311,  1313,  1323,  1325,  1327,  1332,  1337,  1339,  1341,
    1343,  1351,  1352,  1354,  1358,  1360,  1364,  1366,  1367,  1369,
    1371,  1376,  1377,  1381,  1386,  1387,  1391,  1393,  1398,  1400,
    1405,  1407,  1409,  1411,  1416,  1418,  1420,  1422,  1427,  1429,
    1434,  1435,  1457,  1459,  1463,  1466,  1468,  1471,  1473,  1476,
    1478,  1483,  1488,  1490,  1495,  1500,  1502,  1504,  1506,  1508,
    1513,  1515,  1518,  1520,  1525,  1531,  1534,  1536,  1541,  1547,
    1549,  1554,  1560,  1564,  1566,  1569,  1571,  1576,  1583,  1585,
    1590,  1596,  1598,  1603,  1609,  1612,  1616,  1627,  1632,  1637,
    1648,  1650,  1652,  1654,  1659,  1661,  1663,  1668,  1669,  1671,
    1676,  1678,  1683,  1685,  1687,  1689,  1692,  1696,  1699,  1703,
    1705,  1707,  1709,  1711,  1713,  1715,  1717,  1719,  1721,  1723,
    1728,  1734,  1742,  1747,  1748,  1752,  1753,  1758,  1762,  1763,
    1766,  1768,  1773,  1776,  1778,  1780,  1783,  1785,  1790,  1795,
    1796,  1800,  1805,  1807,  1812,  1814,  1819,  1821,  1823,  1828,
    1833,  1838,  1843,  1845,  1847,  1852,  1854,  1860,  1861,  1865,
    1866,  1867,  1868,  1872,  1877,  1878,  1880,  1882,  1884,  1888,
    1892,  1893,  1897,  1899,  1901,  1903,  1905,  1911,  1912,  1918,
    1919,  1923,  1924,  1929,  1931,  1940,  1941,  1943,  1948,  1953,
    1964,  1965,  1969,  1970,  1976,  1977,  1981,  1983,  1987,  1989,
    1993,  1994,  1998,  1999,  2003,  2004,  2005,  2009,  2011,  2026,
    2027,  2028,  2029,  2031,  2035,  2037,  2041,  2048,  2050,  2052,
    2054,  2062,  2064,  2069,  2070,  2072,  2074,  2076,  2086,  2088,
    2100,  2103,  2108,  2110,  2116,  2121,  2126,  2137,  2144,  2149,
    2151,  2153,  2159,  2163,  2170,  2172,  2173,  2174,  2190,  2192,
    2195,  2197,  2200,  2205,  2206,  2210,  2211,  2212,  2213,  2222,
    2223,  2224,  2233,  2234,  2235,  2239,  2240,  2241,  2250,  2251,
    2252,  2257,  2258,  2267,  2268,  2273,  2275,  2279,  2281,  2283,
    2285,  2292,  2297,  2302,  2303,  2305,  2315,  2316,  2321,  2323,
    2325,  2327,  2329,  2331,  2334,  2336,  2338,  2343,  2349,  2351,
    2353,  2355,  2357,  2359,  2361,  2363,  2365,  2367,  2369,  2371,
    2373,  2375,  2377,  2379,  2382,  2384,  2386,  2388,  2390,  2392,
    2394,  2396,  2398,  2400,  2402,  2404,  2406,  2408,  2410,  2412,
    2414,  2416,  2421,  2422,  2426,  2432,  2433,  2439,  2440,  2442,
    2444,  2446,  2451,  2453,  2458,  2459,  2461,  2463,  2468,  2470,
    2472,  2474,  2476,  2478,  2483,  2484,  2486,  2488,  2493,  2495,
    2494,  2498,  2506,  2507,  2509,  2511,  2516,  2517,  2519,  2524,
    2525,  2527,  2529,  2534,  2536,  2538,  2543,  2545,  2547,  2549,
    2550,  2552,  2557,  2559,  2561,  2566,  2567,  2571,  2572,  2579,
    2578,  2583,  2582,  2592,  2591,  2602,  2601,  2611,  2616,  2617,
    2622,  2628,  2646,  2647,  2651,  2653,  2655,  2660,  2662,  2664,
    2666,  2671,  2673,  2678,  2680,  2689,  2690,  2695,  2699,  2704,
    2706,  2708,  2717,  2719,  2720,  2721,  2723,  2725,  2726,  2731,
    2732,  2736,  2737,  2742,  2744,  2747,  2750,  2757,  2758,  2759,
    2765,  2770,  2772,  2778,  2779,  2785,  2786,  2790,  2798,  2805,
    2818,  2817,  2821,  2824,  2823,  2832,  2836,  2840,  2842,  2848,
    2849,  2854,  2859,  2868,  2869,  2871,  2877,  2879,  2884,  2885,
    2891,  2892,  2893,  2902,  2903,  2905,  2906,  2911,  2912,  2913,
    2915,  2921,  2922,  2924,  2925,  2926,  2928,  2930,  2937,  2938,
    2940,  2942,  2947,  2948,  2957,  2959,  2964,  2966,  2971,  2972,
    2974,  2977,  2979,  2983,  2984,  2985,  2987,  2989,  2997,  2999,
    3004,  3005,  3006,  3011,  3012,  3017,  3018,  3019,  3020,  3024,
    3025,  3030,  3031,  3032,  3033,  3034,  3048,  3049,  3054,  3055,
    3060,  3062,  3064,  3066,  3068,  3091,  3092,  3098,  3099,  3105,
    3104,  3114,  3113,  3117,  3123,  3125,  3135,  3136,  3138,  3142,
    3147,  3149,  3151,  3153,  3159,  3160,  3164,  3165,  3170,  3172,
    3179,  3181,  3182,  3184,  3189,  3191,  3193,  3198,  3200,  3205,
    3210,  3218,  3223,  3225,  3230,  3235,  3236,  3241,  3242,  3246,
    3247,  3248,  3254,  3256,  3258,  3264,  3266,  3271,  3273,  3279,
    3280,  3284,  3288,  3292,  3294,  3306,  3308,  3310,  3312,  3314,
    3316,  3318,  3319,  3324,  3327,  3326,  3338,  3337,  3350,  3349,
    3363,  3362,  3376,  3375,  3388,  3393,  3399,  3401,  3407,  3408,
    3419,  3426,  3431,  3437,  3440,  3443,  3447,  3453,  3456,  3459,
    3464,  3465,  3466,  3467,  3471,  3479,  3480,  3492,  3493,  3497,
    3498,  3503,  3505,  3507,  3509,  3514,  3515,  3521,  3522,  3524,
    3529,  3530,  3532,  3567,  3569,  3572,  3577,  3579,  3580,  3582,
    3587,  3589,  3591,  3593,  3598,  3600,  3602,  3604,  3606,  3608,
    3610,  3615,  3617,  3619,  3621,  3630,  3632,  3633,  3638,  3640,
    3642,  3644,  3646,  3651,  3653,  3655,  3657,  3662,  3664,  3666,
    3668,  3670,  3672,  3684,  3685,  3686,  3690,  3692,  3694,  3696,
    3698,  3703,  3705,  3707,  3709,  3714,  3716,  3718,  3720,  3722,
    3724,  3736,  3741,  3746,  3748,  3749,  3751,  3756,  3758,  3760,
    3762,  3767,  3769,  3771,  3773,  3775,  3777,  3779,  3784,  3786,
    3788,  3790,  3799,  3801,  3802,  3807,  3809,  3811,  3813,  3815,
    3820,  3822,  3824,  3826,  3831,  3833,  3835,  3837,  3839,  3841,
    3851,  3853,  3856,  3857,  3859,  3864,  3866,  3868,  3873,  3875,
    3877,  3879,  3884,  3886,  3888,  3902,  3904,  3907,  3908,  3910,
    3915,  3917,  3922,  3924,  3926,  3931,  3933,  3938,  3940,  3957,
    3958,  3960,  3965,  3967,  3969,  3971,  3973,  3978,  3979,  3981,
    3983,  3988,  3990,  3992,  3998,  4000,  4003,  4010,  4012,  4021,
    4023,  4025,  4026,  4028,  4030,  4034,  4036,  4041,  4043,  4045,
    4047,  4082,  4083,  4087,  4088,  4091,  4093,  4098,  4100,  4102,
    4104,  4106,  4111,  4112,  4114,  4116,  4121,  4123,  4125,  4131,
    4132,  4134,  4143,  4146,  4148,  4151,  4153,  4155,  4169,  4170,
    4172,  4177,  4179,  4181,  4183,  4185,  4190,  4191,  4193,  4195,
    4200,  4202,  4210,  4211,  4212,  4217,  4218,  4219,  4225,  4227,
    4229,  4231,  4233,  4235,  4242,  4244,  4246,  4248,  4250,  4252,
    4254,  4256,  4258,  4260,  4263,  4265,  4267,  4269,  4271,  4276,
    4278,  4280,  4285,  4311,  4312,  4314,  4318,  4319,  4323,  4325,
    4327,  4329,  4331,  4333,  4340,  4342,  4344,  4346,  4348,  4350,
    4355,  4357,  4359,  4364,  4366,  4368,  4386,  4388,  4393,  4394
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
  "GOTO", "RETURN", "CHOOSE", "FALLTHROUGH", "WITH", "WHEN", "WAITFOR",
  "WAITUNTIL", "CORUN", "COFOR", "DISABLE", "ENABLE", "TRY", "THROW",
  "THROWRESUME", "AT", "ASM", "ALIGNAS", "ALIGNOF", "GENERIC",
  "STATICASSERT", "IDENTIFIER", "TYPEDIMname", "TYPEDEFname",
  "TYPEGENname", "TIMEOUT", "WAND", "WOR", "CATCH", "RECOVER",
  "CATCHRESUME", "FIXUP", "FINALLY", "INTEGERconstant",
  "CHARACTERconstant", "STRINGliteral", "DIRECTIVE", "C23_ATTRIBUTE",
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
  "updowneq", "jump_statement", "with_statement", "mutex_statement",
  "when_clause", "when_clause_opt", "cast_expression_list", "timeout",
  "wor", "waitfor", "wor_waitfor_clause", "waitfor_statement", "wand",
  "waituntil", "waituntil_clause", "wand_waituntil_clause",
  "wor_waituntil_clause", "waituntil_statement", "corun_statement",
  "cofor_statement", "exception_statement", "handler_clause",
  "handler_predicate_opt", "handler_key", "finally_clause",
  "exception_declaration", "enable_disable_statement",
  "enable_disable_key", "asm_statement", "asm_volatile_opt",
  "asm_operands_opt", "asm_operands_list", "asm_operand",
  "asm_clobbers_list_opt", "label_list", "declaration_list_opt",
  "declaration_list", "KR_parameter_list_opt", "KR_parameter_list",
  "local_label_declaration_opt", "local_label_declaration_list",
  "local_label_list", "declaration", "static_assert", "cfa_declaration",
  "cfa_variable_declaration", "cfa_variable_specifier",
  "cfa_function_declaration", "cfa_function_specifier",
  "cfa_function_return", "cfa_typedef_declaration", "typedef_declaration",
  "typedef_expression", "c_declaration", "declaring_list",
  "general_function_declarator", "declaration_specifier", "invalid_types",
  "declaration_specifier_nobody", "type_specifier",
  "type_specifier_nobody", "type_qualifier_list_opt",
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

#define YYPACT_NINF (-1843)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1138)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const int yypact[] =
{
      91, 12766,   117,   169, 25054,   159, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,   130,   870,
     156, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843, -1843,   476,   342, -1843, -1843, -1843, -1843,
   -1843, -1843,  5451,  5451,   232, 12766,   301,   323, 30050, -1843,
     336, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,
   -1843, -1843,  2336,  5060, -1843,   507, 15278, -1843, -1843,  2844,
   -1843, -1843, -1843, -1843, 18522, -1843,   244,   314,   289,    78,
     393, -1843,  5657,   339,   389,   410,   403,  5861,   635,   895,
   12952, -1843, -1843,   598, 18357,  2240, -1843, -1843, -1843, -1843,
    2792,   745, 30422, 11294,   982,  2792,  1254,   591, -1843, -1843,
   -1843, -1843,    49, -1843, -1843, -1843, -1843,   608, -1843, -1843,
   -1843, -1843, -1843,   597,   659,    49, -1843,    49, 22842, -1843,
   -1843, -1843, 26494,  5451, -1843, -1843,  5451, -1843, 12766, -1843,
     649, 26657, -1843, -1843,  6241, 28760, -1843, -1843,   911,   911,
     688,  3301, -1843, -1843, -1843, -1843,   587, 20890,    49,  2960,
      49, -1843, -1843, -1843, -1843, -1843, -1843,   716, -1843,   676,
     726,  1084, -1843,   784, 32697, -1843, -1843, -1843, -1843, -1843,
   -1843, -1843, 23335, -1843, -1843, -1843,   703, -1843,   771,  5716,
    5060,   633,   791,   799,   805,   827,   829,   844, -1843, -1843,
   32774,   821, 32851,   840,   868,    49, 32697, 32928,   881, 30175,
   -1843, -1843, -1843, -1843, -1843, -1843, -1843, 33005, 33005, 23168,
   14222, 25217,  3564,   847, -1843, -1843, -1843, -1843,   356, -1843,
     601,   940, -1843,  1951,  5802, 23836, 32697, -1843,   899,   534,
     536,   772,   509,   792,   913,   923,   914,   963,   -11, -1843,
     950, -1843, -1843,  5179,  4430,   979, 21578, 29700,  2792,  2792,
    1003,  2792,  1281,  2792,  1705,   984, -1843, -1843,    49, -1843,
     852,   904, -1843, -1843, -1843, -1843, 26820,  5451, -1843, -1843,
   26983,  5199, -1843, -1843, 15454,   972, -1843, 18852, -1843, -1843,
   27146, -1843, -1843,   999, -1843, -1843, -1843,   995, -1843, 11114,
    1147,  9431, -1843,  1032,  5451,   659,  1045,  1047, -1843,  2844,
    6241,  2844, -1843, -1843, -1843,  3694,  4291,  1044,  1126,   561,
    1126, -1843,    49,    49,    39, 22504,   602,  1126, -1843,    49,
      49,    39,    49, -1843,    49, -1843,  5315, -1843, -1843,  1080,
    1089,   911, 29845, 21062, 18522, -1843,  5657, -1843,  2792, -1843,
    3173,   591,  1054,  1165, 22504,  5451,  5451,    78, -1843, 13502,
   -1843,   911,   911,  1095,  1165, 22504,  5451, -1843, 30297, -1843,
   -1843, -1843,   911, -1843, -1843, -1843, -1843,   911, -1843,  1006,
    4390,  5451, -1843, 24737,  1128, -1843, -1843, -1843, 29555,   659,
   22673,  1113,  6241, 24592, 29845, 16334, -1843, 28916, -1843,  5451,
    1126,   127, -1843, 32697, 28916,  5584,  5315, -1843,   603, -1843,
   -1843, -1843, -1843, 21750, 26657,  1126, -1843,  1135,  1119, -1843,
   -1843, -1843, -1843,  5451,  3626,   432,   455, -1843,  5451,   676,
   -1843,   964, -1843, 15630, 27309,   520, 21578, -1843,   911,   911,
    1134, -1843,   999,  2526,   478,   600, -1843,   621,   591,  1161,
    1139, -1843,  3301,  1149,   676,  3301,  2336,   511,  1169, 30536,
   -1843, 32697, -1843,   730,   984, -1843, 14398, 24003, -1843,   436,
   -1843, -1843,   744, -1843, -1843, -1843,  2336,  8524,  5716,  1173,
    1181,  1191,  1197,  1203,  1207, -1843, -1843,   739,  1159, -1843,
     779,  1159, 23502, -1843,  3564, 23669, -1843, 27472, 26657,  4585,
   -1843, 23502, -1843, 32697, -1843, -1843, -1843, -1843, -1843, -1843,
   23669, -1843, -1843, 25842, 27472, 27472, 14574,  2122,  2145,   544,
    2268, -1843,   790,  1209,   967,  1211, -1843,  1210, 24891,  1213,
    1008, 16510, 24170,  1221, 33082,  1235, -1843, 29072, 27146, -1843,
   30767,  1230, -1843, 32697,  2844, 32697,  2844, -1843, -1843,  3439,
   -1843, -1843,  8524,  3140, 32697,  8524,  2844, -1843, -1843, -1843,
   -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,  1234, 32697,
   -1843, -1843, 14750, -1843, -1843, 27635, -1843,   911,   911, -1843,
   -1843,   999, -1843, -1843, 32697, 32697, 32697, 32697, 32697, 32697,
   32697, 32697, 32697, 32697, 32697, 32697, 32697, 32697, 32697, 32697,
   32697, 32697, 32697, 30844, -1843, -1843, 13319, 30921,  1389, 32697,
    3170,   787,  1227, -1843,    49,    49,  1227,   838, -1843,    49,
      49,  1244,  1227, -1843,    49,    49, -1843,  1159, -1843, 30998,
   15806, 27309, -1843, -1843,  5451, 25697,   911,   911, -1843,  3908,
    4585, -1843, 21750, -1843, 21750, -1843, 10986, -1843, -1843,  1227,
   16686, -1843, 26820, -1843, -1843, -1843,   150, 26005, 21922, -1843,
   -1843, -1843, 33159, -1843, 24346,  4097, 30536, 11114,  1246,  1251,
   -1843, -1843,  1257,  9431,   -13, -1843, -1843, -1843, 24003,  1266,
   -1843,   784, -1843, -1843, -1843,  1258,  3694,   839,  1283,  1285,
    1289,   915,  1312,  1314,  1318,  1322,  1327,  1337,  4291, -1843,
   -1843, -1843,    49,  1320, 29228, -1843, -1843,  1244,    78, -1843,
   -1843,   659,  1165, 25389, -1843, -1843,    78, -1843, -1843,   659,
   -1843, -1843,  5315, -1843, 24003, 24003, -1843,   911,  6241, 29990,
   15982,  3682, -1843, -1843, -1843, -1843, -1843, -1843,   659,  1165,
    1346,  1342, -1843, -1843,  2792,  1345,  1165, 22504, -1843,   659,
    1165, -1843, 30355, -1843,   911,   911, -1843, -1843,  1364,   -20,
    1367,   591,  1369, -1843, -1843, -1843, 25697,  1361,  1376, -1843,
   -1843,   822, -1843,  1477, -1843,  1368, -1843, -1843, -1843, 27807,
    1410,  1416,  1016,  5451,  1126, -1843, -1843, -1843, -1843, -1843,
    5584,   922,  5315, 25389,  1417, 12766, -1843,  5451,  1422, 18194,
    1433, -1843, -1843, -1843, -1843, -1843,  3301, -1843, -1843,  1500,
   26168, 20374,  1582,  2509, -1843, -1843,    49,  1431,    49,  1139,
     344,  1432,   858, 26657,   869,   872, -1843,  1419,  1436, -1843,
     784, 20546,  2652, -1843, -1843,    49,    49, -1843, -1843, 24003,
   -1843,  2844,  1444,  1443, -1843, -1843, -1843,   879,  1159, -1843,
     902,  1159, 25389, -1843, -1843,  1244, 25389, -1843,  1244,  1448,
    1450,  1449,  1452, 16158,  1454,  1463, -1843,  1464,  1468,  1469,
    1472, 32697,  1474,  1475,  1476, 27952, 32697, -1843, -1843,  2316,
   -1843, -1843, -1843, 32697, -1843,  1479,  1482, 30613, 31075,  1484,
   20718, -1843, 26820, -1843, -1843, -1843, 30690, 14926,  1485, 23836,
    1486,   984,  1488, 16862, -1843, -1843, -1843, -1843,  8524,  1491,
   -1843,  1492, -1843, -1843,  4866, -1843,  1497, -1843,  4866, -1843,
   -1843,  1051,  1502, -1843, 11114, -1843, -1843, -1843,   899,   899,
     899,   534,   534,   536,   536,   772,   772,   772,   772,   509,
     509,   792,   913,   923,   914,   963, 32697,  1062, 20718, 13319,
    1501,   994,  1507,  1508,  1509,  1511,  1512,  1514,  1515, -1843,
    1877,  4060, -1843,  3170, -1843, -1843, -1843, 25389, -1843, -1843,
   -1843, -1843, -1843, -1843, 25389, -1843, -1843, -1843, -1843, -1843,
   -1843, -1843,  1244, -1843,  1516, 26983, 16510, -1843, -1843, -1843,
    1227,   911,  4866, -1843, -1843,  1082, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843, 20718, -1843,  5451,  4866, -1843,  1522,   215,
    1521,  1257, -1843, 11114,  1530, -1843,  3754, 32697, -1843, -1843,
     925, -1843,  1528, 20718, 32697,   969,  1529,  1531,  1534,   986,
    1535,  1536,  1540,  1542,  1544,  1549,   900,  1159, -1843, -1843,
     991,  1159, -1843, -1843,  1182,  1159, -1843, -1843, -1843,  6241,
    1676,  1159,   531, -1843,   984,  1172, -1843, -1843,   659,  1550,
   -1843, -1843, -1843,   987,  1552,  1005,  1559, -1843,  1221,  1566,
   -1843,   659, 17379, -1843,   659,  1165,  1566, -1843,   659,  1558,
    1562,  1563, -1843, -1843, 25552, -1843,  2844,  5451, 12208,  1658,
   -1843, -1843, -1843, 21922,  1126, -1843, 20718,  1019, -1843,  1566,
    1572, -1843, -1843, -1843, -1843,  6241, 28115, 17542, -1843,    44,
      59, 24003,  1551, -1843,  1551, -1843, -1843,    49,  2509, -1843,
     344,  1139,  1569,   587, -1843, -1843,  1573,  5451,   344, -1843,
   -1843,  1574,  1581, -1843,  1585,  1589,  1593,  1594,  1597,  2652,
   -1843, -1843, -1843, -1843, -1843,  1577, -1843,  8524, 25389, -1843,
   -1843,  1244, 25389, -1843,  1244,  1606,  1608,   622, -1843, 25697,
     622,  2844, -1843,   622, -1843, 26331,   622, -1843, 32697, 32697,
   32697, -1843, -1843, -1843, -1843, 32697, 32697,  1603, 11114, -1843,
   -1843, -1843,  1607, -1843, -1843,  1615,  1612,  1613, -1843, -1843,
   -1843, -1843,  1607, -1843, -1843, -1843,  1617, 20718, 20718,  1621,
   -1843, -1843, -1843,  4038, -1843, -1843,  1185, -1843,   -21, -1843,
    1190, -1843, 31075, -1843,  1257, -1843, 32697, -1843,  1622, -1843,
    1223,  1159, -1843,  1224,  1284,  1159, -1843,   911,  9832,  2935,
   -1843,    49,    49, -1843, -1843, -1843,  1623,  1624, -1843, -1843,
    1268, -1843, 21750, -1843,    78,  1274, 32697, -1843, 32697, -1843,
    1629, -1843,  9431, -1843,    49, 20718,    49, -1843, -1843,  1424,
    1159, -1843,  1425,  1159, -1843, -1843,  1518,  1159, 25389, -1843,
   -1843,  1244, 25389, -1843, -1843,  1244, 25389, -1843, -1843,  1244,
    1126, -1843,  1244, -1843, 32697, -1843, 32697, -1843, 29388, -1843,
   -1843, -1843, -1843, -1843, -1843,  1633, -1843, -1843, -1843, 17705,
    1566, -1843,   659, -1843, -1843, -1843, -1843, -1843, 19375, -1843,
   -1843, -1843, -1843, -1843,   234,   131,   421, 14046,  1635,  1637,
   22318,  1638,  1642,  2510,  2630,  3467, 31152,  1644,  2424,  1648,
    1649, 22318,  1651, -1843, -1843,   659, 32697, 32697,  1782,  1647,
     665, -1843, 23001, 15102,  1659,  1661,  1653, -1843, -1843, -1843,
   11989, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,  1355,
     295, -1843,   352, -1843,   295, -1843, -1843, -1843, -1843, -1843,
    2844, -1843, -1843, 13136, 18687, -1843,  5451, -1843, -1843, -1843,
   -1843,  5451, -1843, -1843, -1843,  5451, -1843,  6241, -1843,  1046,
   26657,   676,   676,  1139,  1573,  1668,   344,   591,   407,  1670,
    1657,  1573, 17868, -1843, -1843, -1843,  1520,  1159, -1843, -1843,
   32697, -1843,  1685,  1688, -1843, -1843,   737,  1689,  1687,  1061,
   -1843,  1690, -1843, -1843, -1843, -1843, -1843, 11114,  1257, 31229,
    1691, -1843, 21234, 21406,  1696, -1843, -1843, -1843, -1843,  1728,
    4866, -1843,  1728,  1728, -1843,  4866,  4304,  4836, -1843,  1294,
    1703, -1843,  1701,    49, 25389, -1843, -1843,  1244, 25389, -1843,
   -1843, 25389, -1843, -1843,  1244, 32697, 32697,  1699,  1702, -1843,
    1706, -1843, -1843, -1843, -1843, -1843, -1843,  1709, -1843, -1843,
    1708, -1843, -1843, -1843, -1843, -1843, -1843,  1714, 25389, -1843,
   -1843,  1244, 25389, -1843, -1843,  1244, 25389, -1843, -1843,  1244,
    1716,  1719,  1723,    78,  1302, -1843,    85, -1843,   984,  1727,
   -1843, -1843, -1843,  1734, 19541, 19707, 19873, 28278, 29845, 27472,
   27472,  1736,  1713,   343,   365,  2973, 20202, -1843,   413,  5451,
    5451, -1843,  8524,    32,   395, -1843, -1843, -1843, -1843, 14046,
   32697,  1742,  1819, 13869, 12394, -1843,  1725, -1843,  1730, 32697,
    1732, 11114,  1733, 32697,  1740, -1843,  1743, 24003, 32697, -1843,
   12580,  1972, -1843,  1744,    -4, -1843,    34,  1809,   378,    49,
   -1843,  1749, -1843,  1753,  1761, 22318, 22318, -1843, -1843,  1839,
   -1843, -1843,    22,    22,   637, 13685,   417,  1772,  1776,   432,
   -1843, -1843, -1843, -1843, -1843, -1843,  1768,  1779,   344,  1573,
     587,  5451, -1843, 31306, -1843,  1780, -1843, 18031, 25389, -1843,
   -1843,  1244, -1843, -1843, -1843,  1778, -1843, -1843, 32697, -1843,
   26331, 32697,  1257,  1783, -1843, -1843, -1843, -1843,  1781, -1843,
   -1843,  1787,  1788, -1843,  1304, -1843,  4866, -1843,  4866, -1843,
   -1843, 31229, -1843, -1843,  1789,  1793,  1795, -1843, -1843,  1794,
   -1843,  1796, -1843, -1843,  1798,    49,  1800,  1802,  1804, -1843,
   -1843, -1843, -1843, -1843, 32697, -1843,  1785, -1843,  1736,  1736,
    1736,  6140,   873,  1790,   425, -1843,  6140,   428, 24003, -1843,
   -1843, -1843, -1843,  5987, 32697,  5436,   246, -1843, -1843, -1843,
      63,  1801,  1801,  1801,  5451, -1843, -1843, -1843,  1805, -1843,
   -1843, -1843, -1843,  1661,  1808, 32697,   314,  1806,   403, 20046,
   28278,  1093,  1816, 22318,  1817, -1843, -1843, -1843, -1843,  1035,
   22318, 32697,  1261,   661, -1843, 32697, 30454, -1843, -1843,   463,
   -1843,  1257, -1843,  1094, -1843, -1843,  1110,  1111,   684, -1843,
   -1843, -1843, -1843,   659,  1972,  1820, -1843, -1843, 32697, -1843,
    1821,   784, -1843, 11797, -1843, 32697, 32697, -1843, -1843,   496,
      22, -1843,   274, -1843, -1843, -1843,    49, -1843,  1551,   344,
   -1843,  1573,  1825,   591,  1657, 11114, -1843, -1843, -1843,  1826,
   -1843, -1843, -1843, -1843,  1834, -1843,    49,    49, -1843,  1307,
    1329, -1843, -1843, -1843,  1832,  1835, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,   502,   873,
    2846,   554, -1843, -1843, -1843, -1843,    49,    49, -1843, -1843,
   -1843,   567, -1843,  1118,  5987,   655, -1843,  5436, -1843,    49,
   -1843, -1843, -1843, -1843, -1843, -1843, 22318, 22318,  1661, 22094,
      54, 31383,  1906, 22318, -1843, -1843, -1843, -1843, -1843, 32697,
   -1843, 31460,  1922,  1827, 24415, 31537, 22318, 12580,  1661,   685,
    1878,  1828, 32697, -1843,  1847,    62, 22318, -1843, 22318, -1843,
    1848, -1843, 28441,  1831,   784,   692, -1843, -1843,  1856,  1330,
    1120, 22318,  1850, 22318, 22318, 22318, -1843,   676,  1573,  1860,
   -1843, -1843,  1257, -1843, -1843, -1843, -1843, -1843, -1843, -1843,
   -1843, -1843,  1863,  1865,  1866,  2846, -1843,    49, -1843, -1843,
   -1843, -1843, -1843,  1858,  6140, -1843,  1957,  8685,    46, 17041,
   -1843, 22192, -1843,    47,  1144, 22318,  1958,   574,  1854,   101,
   22318, 32697,   685,  1878,  1857, 31619,  2192,   911,  1867,   269,
    1962, -1843, 31696, 31773, 32697,  1661,  1861, 17216, -1843, -1843,
   -1843, 28441,  1862,  6168, 28604,  2844, -1843,  1883,  1871,   115,
   -1843, 32697,  8524, -1843, -1843, 32697,   295, -1843, -1843, -1843,
    1886, -1843,  1892,  1588,  1159, -1843, -1843,   873, -1843, 22318,
   -1843,   -29, -1843,    89, -1843, -1843, -1843,  1893, 19036, -1843,
   -1843, 22318, -1843,    48, -1843, 22318, 32697,  1897, 31850, -1843,
   -1843, 31927, 32004, 32697,  4585,  1661, -1843,   984, 32081, 32158,
   22318,  1884,   377,  1890,   457, -1843, -1843,  1908, 19036,  1862,
   32697,  1910,  3532,  4114, -1843, -1843, -1843,  1903, -1843,  1964,
    1914,   698,  1911, -1843, -1843,  1917,  1146,   444, -1843, -1843,
   25389, -1843, -1843,  1244, -1843, -1843, 32697, -1843, 32697, -1843,
   -1843,  1435, 19209, -1843, -1843, 22318, -1843, -1843,  1661, -1843,
   -1843,  1661,  1918,   585,  1919,   615, -1843, -1843,   591, -1843,
    1661, -1843,  1661, -1843,  1923, 32235, 32312, 32389, -1843,  1435,
    1924, -1843,   659,  4114,   115,  1931, 32697,  1913,   115,   115,
   -1843, -1843, 22318,  2021,  1940, -1843, -1843, 22192, -1843,  1435,
   -1843, -1843,  1942, 32466, 32543, 32620, -1843, -1843,  1661, -1843,
    1661, -1843,  1661, -1843,   659, -1843,  1937,   784,  1948, -1843,
     718, -1843, -1843, 22318, -1843, -1843, 11429,  1959, 22192, -1843,
   -1843,  1661, -1843,  1661, -1843,  1661,  1963, -1843,   784,  1961,
   -1843,  1932,   784, -1843, -1843, -1843, -1843, 11617, -1843, -1843,
    1359, 32697, -1843,  1153,   784,  2844,  1965,  1939, -1843, -1843,
    1155, -1843, -1843,  1941,  2844, -1843, -1843
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   501,     0,     2,   501,   518,   519,   520,   521,   522,
     523,   524,   525,   526,   507,   509,   508,   510,     0,     0,
       0,   528,   530,   557,   531,   558,   534,   535,   555,   556,
     529,   553,   554,   532,   533,   536,   537,   538,   539,   540,
     541,   542,   543,   544,   545,   546,   547,   548,   549,   550,
     551,   552,   559,   560,   867,   562,   635,   636,   639,   641,
     637,   643,     0,     0,     0,   501,     0,     0,    17,   606,
     612,     9,    10,    11,    12,    13,    14,    15,    16,   823,
     874,   104,   877,     0,    20,     0,   501,   102,   103,     0,
     844,    18,    19,   883,   501,   824,     0,     0,   439,   745,
     441,   453,   865,   440,   475,   476,     0,     0,     0,     0,
     589,   503,   505,   511,   501,   513,   516,   574,   527,   561,
     485,   567,   572,   487,   584,   486,   599,   603,   609,   588,
     615,   627,   867,   632,   633,   616,   686,   442,   443,     3,
     831,   845,   506,     0,     0,   867,   906,   867,   501,   923,
     924,   925,   501,     0,  1116,  1117,     0,     1,   501,    17,
       0,   501,   464,   465,     0,   589,   511,   495,   496,   497,
     834,     0,   638,   640,   642,   644,     0,   501,   867,   689,
     868,   869,   634,   563,    22,    23,    21,   799,   794,   784,
       0,   877,   832,     0,     0,   518,   825,   829,   830,   826,
     827,   828,   501,   882,   881,   880,     0,   875,   878,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   607,   610,
       0,     0,     0,     0,     0,   867,     0,     0,     0,    27,
      29,     4,     8,    25,     5,     6,     7,     0,     0,   501,
     501,   501,     0,   102,   105,   106,   107,   108,    85,    28,
      86,    24,    46,    84,   109,   501,     0,   124,   126,   130,
     133,   136,   141,   144,   146,   148,   150,   152,   154,   165,
       0,    30,   732,     0,  1138,     0,   502,   501,   513,   492,
     567,   493,   592,   494,   599,   603,   596,   617,   867,   618,
       0,     0,   728,   733,   718,   722,   501,   734,  1085,  1086,
     501,   735,   737,   884,   501,     0,  1118,   589,   913,   931,
     501,  1122,  1115,  1113,  1120,   436,   435,     0,   173,   751,
     172,     0,   444,     0,     0,     0,     0,     0,   451,     0,
       0,     0,   434,  1000,  1001,     0,     0,   474,   865,   867,
     865,   887,   867,   867,   484,   501,   867,   865,   944,   867,
     867,   483,   867,   963,   867,   941,     0,   582,   583,     0,
       0,   501,   501,   501,   501,   454,   865,   504,   514,   575,
       0,   604,     0,   848,   501,     0,     0,   745,   455,   589,
     568,   585,   600,     0,   848,   501,     0,   517,   569,   576,
     577,   488,   586,   490,   491,   489,   591,   601,   605,     0,
     619,     0,   817,   501,     2,   846,   905,   907,   501,     0,
     501,     0,     0,   589,   501,   501,  1126,   589,  1129,     0,
     865,   865,     3,     0,   589,     0,     0,   467,   867,   860,
     862,   861,   863,   501,   501,   865,   821,     0,     0,   780,
     782,   781,   783,     0,     0,   776,     0,   765,     0,   774,
     786,     0,   687,   501,   501,  1138,   502,   567,   592,   599,
       0,   734,   735,   689,   606,   612,   690,   691,   692,     0,
     689,   870,     0,   797,   785,     0,   877,     0,     0,     0,
     109,     0,   157,     0,     0,   613,   501,   501,   791,   741,
     743,   790,     0,   740,   744,   873,   877,   158,     0,     0,
       0,     0,     0,     0,     0,   885,   911,   867,   921,   929,
     933,   939,   501,    92,     0,   501,   100,   501,   501,     0,
      87,   501,    94,     0,    36,    40,    41,    37,    38,    39,
     501,    90,    91,   501,   501,   501,   501,   105,   106,     0,
       0,   194,     0,     0,   633,     0,  1113,  1136,   501,     0,
       0,   502,   501,   606,     0,     0,  1124,   589,   501,  1127,
       0,     0,  1038,     0,     0,     0,     0,    26,    59,     0,
      65,    66,   158,     0,     0,   158,     0,   174,   175,   176,
     177,   178,   179,   180,   181,   182,   183,   184,   172,     0,
     170,   171,   501,    88,  1088,   502,   498,   499,   500,  1092,
    1082,  1083,  1090,    89,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1037,     2,   501,     2,   103,     0,
    1047,   867,  1138,   982,   867,   867,  1138,   867,   997,   867,
     867,  1061,  1138,  1043,   867,   867,  1052,  1059,   726,     0,
     501,   501,   597,  1087,   736,   502,   593,   594,   598,     0,
       0,   462,   501,  1130,   501,  1102,   502,  1108,  1103,  1138,
     501,  1096,   501,  1105,  1097,     2,  1138,   501,   501,   914,
     932,  1114,     0,     2,    27,     0,     0,   751,    28,     0,
     749,   752,  1136,     0,     0,   758,   747,   746,   501,     0,
     850,     0,     2,   466,   468,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   890,
     947,   970,   867,   480,     0,   886,   894,  1028,   745,   888,
     889,     0,   848,   501,   943,   951,   745,   945,   946,     0,
     962,   964,     0,   470,   501,   501,   573,   502,     0,   589,
     501,     0,  1119,  1123,  1121,   452,   590,   821,     0,   848,
       0,     0,   445,   456,   515,     0,   848,   501,   821,     0,
     848,   795,   570,   571,   587,   602,   608,   611,   606,   612,
     630,   631,     0,   796,   704,   738,   502,     0,   705,   707,
     708,     0,   216,   428,   847,     0,   426,   484,   483,   589,
     102,     0,     0,     0,   865,   447,     2,   448,   818,   472,
       0,     0,     0,   501,     0,   501,   821,     0,     0,     0,
       0,   779,   778,   777,   771,   512,     0,   769,   787,   565,
     501,   501,   103,  1047,   736,   688,   867,     0,   867,   689,
     689,     0,     0,   501,     0,     0,   872,     0,     0,   438,
       0,   501,  1012,   742,  1009,   867,   867,  1017,   614,   501,
     876,   162,     0,   159,   160,   164,   912,   867,   922,   930,
     934,   940,   501,   915,   917,   919,   501,   935,   937,     0,
       0,     0,     0,   501,     0,     0,   691,     0,     0,     0,
       0,     0,     0,     0,     0,   501,     0,   123,   122,     0,
     119,   118,    31,     0,    32,     0,     0,     0,  1137,     0,
     501,  1094,   501,  1104,  1095,   185,     0,   501,   102,   501,
       0,   604,     0,   502,     2,     2,  1125,  1128,   158,     0,
      55,     0,    56,    63,     0,    62,     0,    58,     0,    57,
      61,     0,     0,    54,   751,   166,  1084,   125,   127,   128,
     129,   131,   132,   134,   135,   139,   140,   137,   138,   142,
     143,   145,   147,   149,   151,   153,     0,     0,   501,   501,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1062,
       0,   867,  1139,  1048,   985,  1002,  1049,   501,   980,   988,
     724,   983,   984,   725,   501,   995,  1005,   998,   999,   727,
    1045,  1046,  1060,  1131,     0,   501,   502,  1089,  1093,  1091,
    1138,   595,     0,    33,   630,     0,   720,   719,   723,   729,
    1100,  1107,  1101,   501,   730,     0,     0,   760,   157,     0,
       0,  1136,   757,  1137,     0,   753,     0,     0,   756,   759,
       0,     2,     0,   501,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   867,   899,   903,   942,
     867,   956,   960,   968,   867,   978,   891,   948,   971,     0,
       0,  1024,     0,  1029,  1030,     0,   478,   851,     0,     0,
     479,   852,   471,     0,     0,     0,     0,   469,     0,     2,
     853,     0,     0,   821,     0,   848,     2,   854,     0,     0,
       0,     0,   645,   908,   501,   926,     0,     0,   501,   429,
     427,  1040,  1039,   501,   865,   449,   501,     0,   822,     2,
       0,   773,   814,   809,   810,     0,   502,     0,   805,     0,
       0,   501,   767,   766,   767,   566,   564,   867,  1048,   683,
     689,   689,     0,     0,   699,   698,  1136,     0,   689,   800,
     798,     0,     0,   833,     0,     0,     0,     0,     0,  1013,
    1014,  1010,  1011,   793,   792,     0,   879,     0,   501,   916,
     918,   920,   501,   936,   938,     0,     0,    93,    96,   501,
     101,     0,    99,    95,    97,   501,     0,   113,     0,     0,
       0,   117,   121,   120,   195,     0,     0,     0,   751,   110,
     191,   190,  1136,   187,   712,     0,   713,   714,  1098,  1106,
    1099,   186,  1136,  1109,  1110,  1111,     0,   501,   501,     0,
      49,    50,    82,     0,    82,    82,     0,    70,    72,    52,
       0,    48,     0,    51,  1136,   156,     0,     3,     0,  1056,
     867,   991,   994,   867,   867,  1055,  1058,   501,     3,     0,
    1044,   867,   867,   986,  1003,  1050,     0,     0,  1132,   731,
       0,   463,   501,     3,   745,     0,     0,   761,     0,   762,
       0,   754,     0,   748,   867,   501,   867,     3,   473,   867,
     900,   904,   867,   957,   961,   969,   867,   979,   501,   892,
     895,   897,   501,   949,   952,   954,   501,   972,   974,   976,
     865,   481,  1025,  1036,     0,  1035,     0,  1027,     0,   856,
     965,   579,   578,   581,   580,     2,   822,   857,   802,     0,
       2,   855,     0,   822,   858,   645,   645,   645,   501,   706,
     709,   710,   739,   432,     0,     0,     0,   501,     0,     0,
     353,     0,     0,     0,     0,     0,   196,     0,     0,     0,
       0,   353,     0,   401,   400,     0,   168,   168,   407,   606,
     612,   213,   501,   501,     0,   197,     0,   224,   198,   199,
     501,   218,   200,   201,   202,   203,   204,   205,   354,     0,
     368,   206,   374,   376,   379,   207,   208,   209,   210,   211,
       0,   212,   220,   589,   501,   222,     0,   450,     3,   835,
     822,     0,   812,   789,   806,     0,   807,     0,   808,     0,
     501,   784,   784,   689,  1136,     0,   689,   695,   689,     0,
     700,  1136,     0,   871,   437,  1021,   867,  1020,  1023,  1015,
       0,   161,     0,     0,   909,   927,  1041,     0,     0,     0,
      42,     0,   114,   116,   115,   112,   111,   751,  1136,  1137,
       0,  1133,   501,   501,     0,  1112,     3,     3,    69,    79,
       0,    73,    80,    81,    64,     0,     0,     0,    60,     0,
       0,   155,     0,   867,   501,   987,   989,   990,   501,  1004,
    1006,   501,  1051,  1053,  1054,     0,     0,   102,     0,     3,
       0,   981,   996,   992,  1007,    34,   721,     0,   446,   764,
       0,   864,   750,   755,   849,     3,   866,     0,   501,   893,
     896,   898,   501,   950,   953,   955,   501,   973,   975,   977,
       0,     0,     0,   745,     0,  1031,     0,  1032,  1033,     0,
     804,   822,   859,     0,   501,   501,   501,   501,   501,   501,
     501,   628,     0,     0,     0,   659,   589,   646,     0,     0,
       0,   430,   158,     0,     0,   341,   342,   221,   223,   501,
       0,     0,     0,   501,   501,   337,     0,   335,     0,     0,
       0,   751,     0,     0,     0,   332,     0,   501,     0,   380,
     501,     0,   169,     0,     0,   408,     0,     0,     0,   867,
     228,     0,   219,     0,     0,   353,   353,   359,   358,   353,
     370,   369,   353,   353,     0,   589,     0,     0,     0,   776,
     811,   813,   788,   768,   772,   770,     0,     0,   689,  1136,
       0,     0,   678,     0,   694,     0,   801,     0,   501,  1016,
    1018,  1019,   163,   910,   928,     0,  1042,    98,     0,    35,
     501,     0,  1136,     0,   193,   192,   189,   716,   715,   717,
     188,     0,     0,    83,     0,    71,     0,    77,     0,    75,
      47,     0,   167,  1135,     0,     0,     0,     3,     3,     0,
    1064,     0,  1134,   763,     0,   867,     0,     0,     0,   901,
     958,   966,   482,  1026,     0,   839,     0,   841,   628,   628,
     628,   659,   667,   633,     0,   673,   659,     0,   501,   620,
     658,   657,   653,     0,     0,     0,     0,   660,   661,   663,
     867,   675,   675,   675,     0,   654,   671,   433,     0,   345,
     346,   343,   344,   237,     0,     0,   239,   441,   238,   589,
     501,     0,     0,   353,     0,   320,   322,   321,   323,     0,
     353,   196,   277,     0,   270,     0,   196,   338,   336,     0,
     330,  1136,   339,     0,   334,   333,     0,     0,     0,   389,
     390,   391,   392,     0,   382,     0,   383,   347,     0,   348,
       0,     0,   373,     0,   217,     0,     0,   362,   372,     0,
     353,   375,     0,   377,   399,   431,   867,   837,   767,   689,
     679,  1136,     0,   697,   700,   751,   701,   682,   803,     0,
      53,    45,    43,    44,     0,    67,   867,   867,    74,     0,
       0,   993,  1008,  1057,     0,     0,  1063,  1065,   457,   461,
     902,   959,   967,  1034,   843,   624,   626,   622,     0,     0,
    1071,     0,   668,  1076,   670,  1068,   867,   867,   652,   674,
     656,     0,   655,     0,     0,     0,   677,     0,   648,   867,
     647,   664,   676,   665,   666,   672,   353,   353,   240,   589,
       0,     0,   258,   353,   325,   328,   326,   329,   324,     0,
     327,     0,   266,     0,   196,     0,   353,   501,   278,     0,
     303,     0,     0,   331,     0,     0,   353,   352,   353,   393,
       0,   384,   501,     0,     0,     0,   215,   214,   355,     0,
       0,   353,     0,   353,   353,   353,   460,   784,  1136,     0,
     681,   696,  1136,  1022,    68,   459,   458,    78,    76,  1066,
    1067,   650,     0,     0,     0,  1072,  1073,   867,   651,  1069,
    1070,   649,   629,     0,     0,   351,   229,     0,     0,     0,
     251,   353,   231,     0,     0,   353,   260,   275,   286,   280,
     353,   196,     0,   290,     0,     0,     0,   315,   281,   279,
     268,   271,     0,     0,   196,   304,     0,     0,   234,   350,
     381,   501,   387,   394,   502,   398,   349,     0,     0,   409,
     360,     0,   158,   371,   364,     0,   365,   363,   378,   775,
       0,   685,     0,   867,  1079,  1081,  1074,     0,   662,   353,
     246,   241,   244,     0,   243,   250,   249,     0,   501,   253,
     252,   353,   262,     0,   259,   353,     0,     0,     0,   267,
     272,     0,     0,   196,     0,   291,   316,   317,     0,     0,
     353,     0,   306,   307,   305,   274,   340,     0,   501,   387,
       0,     0,     0,  1071,   395,   396,   397,     0,   402,     0,
       0,     0,   410,   411,   356,     0,     0,     0,   684,   702,
     501,  1075,  1077,  1078,   669,   230,     0,   248,     0,   247,
     233,   254,   501,   422,   263,   353,   264,   261,   276,   289,
     287,   283,   295,   293,   294,   292,   273,   318,   319,   288,
     284,   285,   282,   269,     0,     0,     0,     0,   236,   254,
       0,   388,     0,  1072,   409,     0,     0,     0,   409,     0,
     361,   357,   353,     0,     0,   242,   245,   353,     3,   255,
     423,   265,     0,     0,     0,     0,   314,   312,   309,   313,
     310,   311,   308,     3,     0,   385,     0,     0,     0,   403,
       0,   412,   366,   353,  1080,   225,     0,     0,   353,   302,
     300,   297,   301,   298,   299,   296,     0,   386,   415,     0,
     413,     0,   415,   367,   227,   226,   232,     0,   235,   416,
       0,     0,   404,     0,     0,     0,     0,     0,   417,   418,
       0,   414,   405,     0,     0,   406,   419
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1843,    50,     1, -1843,    -1,   809,  2000,  5422,  -182, -1843,
    -203, -1843,   501, -1843,  -900,  -999, -1843,   307,  4454,  2063,
   -1843,   614, -1843,  1605,   371,   934,   951,   819,   949,  1532,
    1537,  1555,  1527,  1547, -1843,    20,  -100,  -559, -1843,  1004,
    9611,   794, -1843,  1899, -1843, -1843,  -871,  2791, -1286,  3514,
   -1843,   214, -1843,   802,     6, -1843, -1843,   613,   110, -1843,
   -1841, -1832,   294,    81, -1843, -1843,   604,   304, -1843, -1687,
   -1843, -1401, -1843, -1843, -1843,   125, -1061, -1843, -1843, -1368,
     408, -1843, -1843, -1843, -1843, -1843,   -31, -1329, -1843, -1843,
   -1843, -1843, -1843,   149,   426,   430,   221, -1843, -1843, -1843,
   -1843,  -781, -1843,    80,    33, -1843,   158, -1843,  -180, -1843,
   -1843, -1843,   801,  -547, -1291,  -228, -1843,   189,    40,   256,
    7845, -1106, -1051, -1843,   -70, -1843, -1843,    43, -1843,   -36,
     288,   867,  -346,  5782,  2722,  -513,    18,   147,   438,   961,
     505, -1843, -1843,  2143, -1843,    71,  6188, -1843,  2077, -1843,
     366, -1843, -1843,  3845,   185,  7044,  4493,   -42,  1840,  -205,
   -1843, -1843, -1843, -1843, -1843,  -637,  7719,  7660, -1843,  -225,
    -267, -1843,  -806, -1843,   267, -1843,   207,   671, -1843,  -139,
    -600, -1843, -1843, -1843, -1843,  -172,  8201, -1117,   795,   418,
    1533, -1843,  -879,  -103,  2001,  2862,  1823,  -577,  -153,   824,
     270,  -466,  -368,  -301,  -608,  1184, -1843,  1524,  -125, -1128,
    1394, -1843, -1843,   616, -1843, -1382,  -184,  -132,  -672, -1843,
    -264, -1843, -1843, -1071, -1092, -1843, -1843, -1843,  2222, -1023,
    -636, -1219,   -48, -1843, -1843, -1843, -1843, -1843, -1843,   121,
    -855,  -232, -1842,   -16,  9158,   -81,  8803,  -124,  1735, -1843,
    2147,    61,  -265,  -243,  -236,     9,   -61,   -49,   -15,   738,
     -51,   -27,    -3,  -208,    65,  -202,  -179,  -173,   218,  -169,
    -162,  -156,  -612,  -571,  -557,  -549,  -594,  -154,  -542, -1843,
   -1843,  -810,  1378,  1381,  1382,  2725, -1843,   709,  3202, -1843,
    -602,  -563,  -560,  -550,  -580, -1843, -1659, -1781, -1780, -1754,
    -593,   595,  -246,  -268, -1843,   -54,     0,  -106, -1843, 10236,
    2205,   866,  -421
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   317,   248,   249,   186,    93,  1364,   250,   251,
     252,  1439,  1440,   253,  1226,  1227,  1228,  1459,   254,   481,
     256,   257,   539,   540,   258,   259,   260,   261,   262,   263,
     264,   265,   266,   267,   268,   269,  1029,   862,   863,   864,
     541,  1583,   589,   321,   591,   271,  1202,  1365,  1366,  1367,
    1368,  1369,  1370,  1371,  2146,  1372,  1373,  1724,  2002,  2003,
    1940,  1941,  1942,  2118,  2119,  1374,  1743,  1744,  2026,  1745,
    1870,  1871,  1375,  1376,  1377,  1378,  1379,  1899,  1903,  1603,
    1595,  1380,  1381,  1602,  1596,  1382,  1383,  1384,  1385,  1386,
    1387,  1388,  1764,  2041,  1765,  1766,  1972,  1389,  1390,  1391,
    1586,  2051,  2052,  2053,  2170,  2180,  2071,  2072,   409,   410,
    1108,  1109,  1334,    95,    96,    97,    98,    99,  1727,   272,
     305,   103,   104,   105,   106,   337,   338,   412,   391,   274,
     489,   275,   109,   424,   111,   112,   166,   277,   278,   116,
     117,   118,   182,   119,  1136,   279,   167,   122,   361,   123,
     168,   370,   281,   458,   283,   169,   484,   128,   129,   286,
     130,   782,  1101,  1099,  1100,  1699,   287,   288,   133,   134,
    1328,  1547,  1706,  1707,  1708,  1831,  1832,  1548,  1694,  1851,
    1709,   135,   839,  1413,   178,  1145,   289,  1146,  1147,  1624,
     970,   788,  1205,   290,   291,   789,   293,   294,   295,   791,
     490,   491,   322,   691,   692,   693,   694,   695,   446,  1411,
     447,  1134,  1132,   824,   448,   473,   449,   450,   492,   137,
     188,   189,   138,  1127,  1128,  1129,  1130,     2,  1315,  1316,
     815,  1399,   139,   436,   437,   372,   383,   765,   140,   325,
     141,   427,  1030,   755,   725,   180,   142,   206,   207,   208,
     143,   429,   341,   342,   343,   430,   145,   146,   147,   148,
     149,   150,   151,   346,   431,   348,   349,   350,   432,   352,
     353,   354,   632,   633,   634,   635,   636,   355,   638,   639,
     640,   853,   854,   855,   856,   726,  1075,  1306,   296,  1635,
     642,   643,   644,   645,   646,   647,  1834,  1835,  1836,  1837,
     599,   297,   298,   299,   300,   493,   312,   154,   155,   156,
     302,   909,   648
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      91,   153,   210,    91,   153,   474,  1412,   469,   653,   762,
     144,   478,  1599,   936,   544,   746,   942,   192,   984,   113,
     697,  1319,   212,  1414,   460,   470,  1040,   901,   986,  1614,
    1615,  1421,   215,  1588,   213,  1404,   985,  1542,  1230,   561,
     311,   100,  1160,   219,   107,  1212,   418,  2005,  1922,  1923,
     667,   989,   885,   158,   673,   972,   216,   996,  1008,  1882,
    1572,    91,    91,   976,    91,   153,   977,   477,   214,   973,
     708,   652,   120,  1323,   144,  1924,   978,   974,   411,  1031,
     217,    91,    91,   113,   975,  1016,   328,  1017,    91,  1237,
    2067,  -815,   709,    91,   483,  2004,  1400,  1529,   704,   710,
    2010,    91,  1768,   308,  1533,   100,    91,   311,   107,    91,
     422,  1466,  1260,    91,   153,  2066,   344,   157,  1350,   373,
     637,  1089,   771,   384,   743,  1770,  1265,   711,    64,  2011,
    2075,   622,  1096,   712,   323,   559,   120,   783,  1937,  1938,
     402,   455,    64,   202,  1263,  1467,  1937,  1938,   114,   499,
    1268,  -625,    91,  1036,  1037,    91,   713,    91,   153,   502,
     708,   500,   714,    91,  1277,   732,   715,   144,   340,  -816,
      91,  1926,   347,   716,  1038,   623,   113,    80,    91,   717,
    1119,  1608,   709,   503,  1769,  1780,   125,  1955,   653,   710,
      91,    80,    82,   162,   759,   501,   172,  1771,   100,   173,
     174,   107,   175,   404,   418,   770,    82,   504,    91,    91,
    -848,   990,   114,  1405,   482,   993,  2006,   711,   308,   120,
    1719,   999,  1543,   712,  1461,  1939,  2004,  1404,  1407,   120,
     724,   986,  1406,  1967,   326,  2012,  2076,  1398,   318,  2117,
      91,   233,  1303,  1542,  1542,  1542,   713,  1408,  1019,   319,
     125,   652,   714,  1395,   706,  1024,   715,   101,  2068,  2069,
     163,  1922,  1923,   716,  1305,   320,  1996,  2117,   976,   717,
     903,   977,    91,    91,   212,   411,  2010,  1544,  1084,  1086,
    2018,   978,  2049,   653,   215,   170,   213,  2148,  1924,   108,
     806,   837,   164,   171,  1552,   414,    91,  1531,   841,  1619,
      91,  1587,   913,   377,   411,   114,    91,  2010,   216,   838,
     753,   881,  1686,   806,  2045,   411,   679,   828,   688,   176,
     214,   101,   723,    91,   728,   351,  1008,   653,    91,    91,
      91,   736,   217,   125,    91,    91,  1234,   629,  1456,  1457,
     795,  1875,   828,   125,   810,   308,   652,   842,   940,  1429,
     844,  1627,   845,   108,  1143,    91,  1905,   777,   405,  1266,
    1076,  1469,    20,    91,   212,    91,   365,   124,  1080,  1219,
     378,  1253,  1594,   308,    91,    91,   213,  1150,    91,   455,
     753,  1255,   306,  1267,  1926,    91,  1193,   552,   766,  1254,
     652,   989,  1035,  1164,  1268,   191,  1505,   719,   972,    91,
      91,   720,    91,  1550,   805,   807,   976,    91,  1848,   977,
     214,    91,   973,  1597,   101,  1849,   120,  1620,    91,   978,
     974,   797,  1551,   808,    91,    91,  1021,  1238,  1543,  1543,
    1543,   124,   315,   311,  1850,   308,   108,  1598,   903,   115,
     416,  1046,    91,    91,  1996,   120,   108,    91,  2029,   306,
     652,  1588,   927,   543,   793,  1873,   120,  1320,    -3,  1409,
    1881,   652,    91,  1047,   193,   731,  1655,  1657,  1659,  1600,
    1048,    91,   739,   340,    91,    91,   637,   347,  1962,  1963,
     381,   120,  1558,  1544,  1544,  1544,   194,   719,   323,  1553,
    1554,   720,   414,  1601,   758,    91,  1597,    91,  1049,   202,
     455,  1791,   316,   115,  1050,   769,  -693,   679,   329,   545,
    1082,  1018,   329,  -693,   124,  1395,  1087,  1144,    91,  1042,
    1598,   414,   466,   563,   124,  1989,  2112,  1051,   564,   556,
     125,  1701,   414,  1052,   330,  1404,  1255,  1053,  1779,   700,
    1350,  1587,  1782,  1772,  1054,  1046,   903,    91,   368,  1004,
    1055,  2021,  2022,  1702,   721,    64,  2095,   414,   330,   125,
    1654,  1392,   324,    91,  -848,    91,   404,  1047,    91, -1137,
     125,   559,    91,   201,  1048,    91,   821,    81,  1901,   331,
    1144,   652,  1714,  1721,   913,   466,  1550,  1095,  1954,  1259,
    1448,   332,   404,  1593,  1839,   125,   115,  1714,   416,   851,
     552,  1715,  1049,   724,    80,  1785,   822,   823,  1050,  1555,
      87,    88,  1902,  1840,   455,   369,  1842,   777,  2057,    82,
     825,   218,    70,   794,   826,    91,   903,   390,   393,    91,
     798,  1051,   903,   108,   455,   763,  2097,  1052,   356,   177,
      64,  1053,   455,   219,   721,   499,   612,   613,  1054, -1000,
     748,  1883,   752,    91,  1055,   502, -1000,   500,    91,    91,
    1907,    81,   108,  1480,  1483,  2020,  1209,  1142,  1154,   482,
     369,  1849,  1908,   108,   376,   968,   846,   980,  2035,   503,
     496,    64,    64,   831,    91,  1496,   688,   627,  1303,    80,
    1921,   501,   614,   615,    87,   832,   164,   411,   108,   624,
    1304,    69,    70,   504,    82,    91,  1028,   629,   605,   896,
    1305,   124,   608,   609,   368,   606,   607,    91,   652,  1587,
     897,   898,   752,  1927,   403,  1023,   543,   679,   724,   543,
      80,    80,  1271,  1025,   529,   543,  1849,  2086,   653,   552,
     124,    91,  1928,   903,   543,    82,    82,    91,  -495,   499,
      91,   124,  1043,    85,   903,  1931,  1809,   399,  1810,   913,
     403,   500,  2016,   202,  2123,   733,   813,  1118,   565,   724,
     724, -1001,   461,   566,   543,   306,   124,   401, -1001,  1066,
     381,   369,   879,  1067,   903,   882,  1197,   884,  1115,   559,
    1557,   887,  -680,  1198,  2125,   501,   403,   552,   505,  -680,
     890,   652,    91,   892,   893,   894,  1106,  1206,   404,    91,
      92,    91,   369,   160,    91,   153,    91,   764,    64,   153,
     505,   679,   724,  1392,   144,    91,  1876,  1252,   202,   637,
     404,  1877,   309,   113,  1636,   -23,   423,  1300,   120,  1642,
     520,  1864,  1865,  1866,  1867,   345,   926,   455,   374,  1888,
     600,   368,   385,  1078,  1877,   100,  1116,  1978,   107,  -836,
      91,   444,  1979,  2107,  1868,  1206,    64,    80,  2108,   593,
     603,   495,   496,  1066,    92,  1016,  1017,  1067,    81,   472,
    1091,   455,    82,  2161,   369,  1828,   120,  1094,  2162,   475,
    1841,  1098,    92,  1402,   368,   849,  1498,   552,   303,   850,
     831,  1485,   872,    92,   627,  1756,   724,   610,   611,   858,
     233,    87,   832,   859,   414,    80,    92,    64,   369,    92,
    1206,   652,   369,    92,    14,    15,    16,    17,    18,   369,
      82,   616,   617,    91,   497,   172,  1068,    91,   173,   174,
    1206,   175,   876,   688,  1613,  1077,   724,   309,   357,   358,
     987,   359,   125,  1081,   627,   902,   506,   360,    64,   903,
    1018,   369,   114,  1751,   507,   556,    80,    92,    91,  1415,
     508,  1503,  1090,    92,  1217,  1218,   948,   949,   950,    64,
      91,    82,    91,  1097,   514,  -496,  1235,  1105,   467,   368,
      64,  1106,   509,  1718,   510,    14,    15,    16,    17,    18,
     125,   994,   403,   517,   505,   627,   724,    80,  1123,   511,
     461,    91,  1725,  1206,    81,   562,  1725,  1746,    92,    92,
     661,   662,    82,  1149,    91,    91,  1843,   826,    80,   200,
    1068,   518,   688,  1746,  1151,    91,  1829,  1152,   826,    80,
     724,   496,  1168,    82,   523,   680,   724,    87,    88,   600,
     555,  1825,  1826,  1827,    82,   108,   369,   482,  1534,  1535,
    1536,    64,   369,  1288,   552,  1172,   567,   724,    91,   724,
      64,   101,   663,   664,   309,  1124,   387,   455,   733,   604,
    1059,   388,   724,    92,   392,   813,   397,   505,   618,   724,
    1274,  1275,   153,   764,   859,   593,   619,   203,  1397,   620,
     369,   593,   309,   108,   621,    91,    91,   688,   153,  1018,
      80,  1852,  1852,  1852,  1206,  1206,    92,   767,   624,    80,
     776,    70,   594,   649,    91,    82,   113,   153,    92,   829,
     399,   461,   403,   124,    82,   625,   724,   544,   703,    92,
     705,   905,   906,  1436,    92,    92,    91,  -498,   100,   733,
     659,   107,  1311,   724,  1292,  1682,   903,   987,   724,   505,
     369,   627,   675,   665,   683,    92,  1500,   671,  1501,   696,
    1313,   369,  1206,    92,   903,  1611,   624,   916,   204,   120,
      91,   124,   813,   556,   663,  1113,   724,  1912,    92,   529,
     528,  1864,  1865,  1866,  1867,   698,   159,   688,   184,   185,
      71,    72,    73,    74,    75,    76,    77,    78,   701,   780,
     702,  1612,   785,   722,  1868,   859,  1322,   160,   947,  1231,
    1232,    92,    91,  1869,   368,   757,  1639,   474,   474,  1073,
    1640,   903,  1236,   326,    92,    92,   680,   764,  1472,   387,
     388,  1616,   657,   744,   397,   461,   600,   476,    91,  1488,
    1261,  1262,   745,   115,   369,  1394,  1471,  -497,  1860,  1885,
     323,    64,   903,   903,  1497,   461,   768,    14,    15,    16,
      17,    18,   836,   461,  1549,  1886,  1887,   369,  1507,   859,
     903,  1123,   817,  1932,  1523,  1983,   482,   859,   482,   903,
     792,   369,  1309,   125,    14,    15,    16,    17,    18,   835,
    1018,   796,    64,    64,   369,  1317,   816,    92,  1321,  2013,
      80,  2111,  1324,   903,  1693,   903,  1123,   466,  2177,   153,
    2183,   594,  2174,  2136,  2184,    82,   554,  2140,   886,   387,
    1746,  1726,   840,    64,   847,  1726,   843,   153,   866,   688,
    1307,  1308,    91,    91,    91,  1296,   867,    91,  1124,   724,
     688,    80,    80,  1464,  1465,   113,   868,   555,  1468,  1465,
      64,   764,   869,    64,   101,   158,    82,    82,   870,   688,
     153,  1591,   871,   930,   904,   932,   907,   100,   935,   908,
     107,   915,    80,  1124,   924,   943,  1474,  1478,   113,    91,
     724,   627,    91,    91,   153,    91,   108,    82,   925,  1607,
      91,   928,   373,   384,    91,   944,    91,   981,   120,    80,
     100,   682,    80,   107,   629,   911,   -18,  1864,  1865,  1866,
    1867,  1032,   153,  2055,    82,  -499,  1033,    82,   369,  1041,
     903,   955,   956,   957,   958,    92,  1495,  1465,   708,    92,
    1868,   120,  1499,  1465,   368,  1044,   688,  1481,  1056,  1874,
    1057,   627,  1593,  1594,  1058,  1441,   680,  1651,  1652,    91,
     709,  2073,  1660,  1661,    91,    91,    91,   710,  1013,  1014,
    1683,   903,  1808,  1465,   124,  1917,  1465,  1060,   461,  1061,
    1549,  1549,  1549,  1062,  1394,  1695,  1549,  1063,  1669,  1728,
    1671,  2073,  1064,  1728,    92,   711,    92,  1918,  1465,  1981,
    1982,   712,  1065,    64,    64,  1187,  1674,  1070,  1123,   806,
    1191,   369,   461,  1092,   600,    92,  1093,  1394,  1007,  1937,
    1938,  1199,   125,  1796,   713,  2120,  1103,    92,  2174,  2175,
     714,  1462,  1463,   593,   715,  -623,  1532,   153,  -621,  1020,
    1102,   716,   951,   952,    91,  1104,   115,   717,    91,    91,
    1556,    92,    80,    80,  1107,   125,  1110,    92,  1034,   153,
     555,   953,   954,   153,   153,   959,   960,    82,    82,  1581,
     688,  1781,  1783,  1853,  1854,  1124,   543,   113,  1111,  1046,
     153,   113,   113,   377,  1112,  1135,   664,  1508,  1512,  1895,
    1121,   724,   724,   101,   688,   688,  1131,    64,   113,    64,
    1137,  1047,  1140,  1148,    91,   916,  1711,  1153,  1048,  1166,
    1712,  1123,  1167,  1177,   766,  1178,  1545,  1180,  1179,    92,
      91,    92,  1844,  1181,    92,   108,   101,   153,  1182,  1183,
     120,   369,   545,  1184,   120,   120,  1049,  1186,  1185,  1188,
    1189,  1190,  1050,   482,  1195,   528,    80,  1196,    80,   365,
     378,   120,  1203,  1213,  1214,    91,  1215,    91,   108,  1220,
    1221,    82,  1229,    82,  1233,  1051,  1239,    64,  1814,  1815,
    1165,  1052,  1240,  1241,  1242,  1053,  1243,  1244,  1124,  1245,
    1246,  1516,  1054,  1628,  1258,   724,  1125,   724,  1055,  1269,
      91,  -165,  1273,  1276,  1279,    91,  1280,   911,  1301,  1281,
    1282,  1283,    91,   124,    91,  1284,  1730,  1285,   461,  1286,
    1730,  1730,  1977,    91,  1287,  1310,    80,  1312,    14,    15,
      16,    17,    18,   474,  1314,   387,   162,  1730,  -819,  1325,
     153,    82,   688,  1326,  1327,  1396,   124,  1401,  1410,   688,
    1416,  2044,  1418,    92,   125,  1422,  1423,    92,   125,   125,
    1424,  2060,  1711,    92,  1425,   724,  1712,  1711,  1426,  1427,
     381,  1712,  1428,  1713,  1430,   125,   719,  1720,  1722,   652,
     720,  1434,   688,  1435,  1447,   115,  1449,   708,    92,  1208,
    1451,  1452,  1453,  1458,    64,  1455,  1007,  1473,  1493,  1494,
      92,  1502,    92,   163,   688,  -820,  1585,  2050,  1559,   709,
    1560,  1563,  1442,  1443,  1444,  1564,   710,  1573,   115,  1445,
    1446,  1577,  1578,  1527,  1580,   101,  1123,   -22,  1784,   101,
     101,    92,  1545,  1545,  1545,   164,  1691,  1692,  1696,  1589,
     903,   368,  1622,    80,   711,    92,   101,  2001,  1975,  1618,
     712,  1590,    92,    91,  1623,    92,    91,   108,    82,  -500,
    1633,   108,   108,  1634,  1637,   688,   688,  1638,  1653,  1646,
    1641,   763,   688,   713,  1650,  1662,  1663,    -3,   108,   714,
    1670,   505,   911,   715,  1672,   688,  1673,   153,    92,  1675,
     716,  1679,  1247,  1124,  1680,   688,   717,   688,  1681,  1685,
      14,    15,    16,    17,    18,   113,  1687,  1270,   369,  1698,
     688,  1700,   688,   688,   688,  1552,  1732,  1594,  1066,  1713,
    1441,  1774,  1067,  1747,  1713,  1332,  1775,  1975,  1748,   377,
    1750,  1752,  2050,   721,  1776,   124,  2050,  2050,  1754,   124,
     124,  1755,  1767,    91,    92,  1350,   787,  1786,  1787,  1789,
     688,  1790,  1797,  1800,   688,  1805,   124,  1824,   120,   688,
    1262,   520,  1806,  1807,  1811,  2159,    64,   482,  1812,  1125,
    1813,   810,  1816,  1818,  1817,  1820,  2115,  1821,  2001,  1822,
    1856,  1704,    91,  1857,    91,   324,  2169,  1889,  1838,  1861,
    2169,   212,  1863,  1892,  1894,   365,   378,  1910,  1945,  2008,
    1437,  1913,  2178,   213,  1125,  1711,  1914,   115,   688,  1712,
    1919,   115,   115,  1920,  1950,    80,  2138,    92,   153,  1966,
     688,  1971,  1419,  1985,   688,  1951,  1964,  2038,   115,  1976,
      82,  1980,  1991,    91,  1730,   724,   113,   214,  1993,   688,
    1994,  1995,    92,  2017,  1864,  1865,  1866,  1867,   153,  1999,
    2015,    91,    91,   764,  2030,  2023,  2028,   903,  2058,  2036,
    2040,   369,   308,  2047,  2059,  2070,   113,  1868,    92,  2048,
    2101,  2079,   125,  2094,    94,  1068,  -197,   161,  1450,  2096,
    2098,  2176,   153,  2104,   688,  2102,  2105,  2106,  1454,   120,
    2109,   568,  2110,   569,   570,   571,   482,  2126,   482,  2134,
     113,  1759,  1760,  1761,  1762,  1763,   381,  2122,  2124,  2137,
    1470,  2139,    91,  2143,   719,  2144,  2149,  2158,   720,   120,
     369,   688,   679,  2160,   572,   763,   688,   573,   574,  2147,
    2172,  2166,   575,   576,  2171,  2168,   482,  2182,    94,  2185,
    2181,   374,   385,   101,  2156,  -123,  -123,  -123,  -123,  -123,
    -123,  1802,   688,   120,   899,   688,   209,   688,   964,   255,
     961,  1584,  1713,   590,  2167,  1730,   962,    94,  -122,  -122,
    -122,  -122,  -122,  -122,  1066,   108,   688,   368,  1067,   965,
     336,  1431,  1592,   364,    91,   963,  1734,    94,  2116,  1968,
    2133,  1961,  2113,    91,  1758,  1730,  1125,  1904,  2100,  2141,
    1890,   482,  2039,   125,  1891,  2173,  2099,  1606,   183,   785,
     394,  1998,    92,    92,  2064,    14,    15,    16,    17,    18,
     756,  1697,  1911,  1621,  1604,   161,    92,  1272,  1039,  1730,
    1133,    94,     3,   125,   161,  1788,   790,   426,   434,  1156,
     211,   860,  1157,  1158,   369,  1684,     0,     0,     0,     0,
     454,     0,     0,   124,   195,     6,     7,     8,     9,    10,
      11,    12,    13,     0,   339,     0,    92,   125,     0,     0,
       0,   721,     0,     0,   101,   487,  1079,     0,     0,    92,
       0,    64,   209,   209,    92,    92,    92,     0,     0,     0,
    1617,    14,    15,    16,    17,    18,   900,  1625,     0,  1125,
       0,     0,     0,     0,   101,     0,   108,   764,     0,   314,
       0,     0,   487,   255,   161,     0,    69,    70,     0,     0,
       0,   428,   386,     0,  1643,   115,  2135,     0,   255,   689,
      80,  1068,     0,     0,     0,     0,   108,     0,   101,    14,
      15,    16,    17,    18,  1192,    82,     0,   630,     0,   651,
       0,     0,     0,   767,     0,     0,   787,     0,  2157,   203,
       0,     0,     0,     0,    92,     0,     0,     0,    85,   454,
     108,     0,     0,   454,   369,     0,   314,   255,     0,     0,
     364,     0,     0,   161,   124,     0,     0,     0,     0,     0,
      92,     0,     0,     0,  1155,     0,     0,     0,     0,  1898,
       0,     0,     0,   426,     0,     0,     0,     0,   336,   336,
       0,     0,     0,     0,   124,  1175,     0,   494,     0,  1176,
       0,     0,     0,     0,    92,     0,   802,     0,     0,   426,
       0,   631,     0,     0,     0,     0,   454,    94,     0,     0,
     204,     0,     0,     0,   814,     0,     0,     0,   124,     0,
       0,     0,   364,     0,     0,     0,   115,     0,   159,   790,
     184,   185,    71,    72,    73,    74,    75,    76,    77,    78,
     602,     0,     0,     0,     0,    92,     0,    92,     0,     0,
       0,     0,     0,     0,     0,   426,   115,   428,   255,   369,
     434,     0,   707,   339,     0,  1792,     0,   434,   426,   426,
       0,     0,     0,     0,  1125,     0,   454,   161,     0,     0,
      92,     0,     0,   428,     0,    92,     0,     0,  1804,  1574,
     115,     0,    92,     0,    92,     0,   255,   454,   833,   651,
    1256,     0,    14,    15,    16,    17,    18,  1257,     0,     0,
       0,     0,     0,     0,     0,     0,   159,   802,   184,   185,
      71,    72,    73,    74,    75,    76,    77,    78,     0,   255,
     487,     0,   852,     0,     0,     0,   790,     0,     0,   428,
       0,   209,     0,     0,     0,     0,     0,     0,     0,   754,
       0,     0,   811,   428,     0,   487,     0,     0,   487,     0,
     161,   161,     0,     0,   487,     0,     0,     0,    64,     0,
       0,     0,     0,   487,     0,  2054,   161,   161,   161,   255,
       0,     0,     0,     0,    92,     0,     0,     0,     0,     0,
       0,   454,  1575,     0,   919,     0,     0,  1884,     0,     0,
     434,   161,   159,     0,   184,   185,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,   790,    80,   159,   754,
     333,   334,    71,    72,    73,    74,    75,    76,    77,    78,
      81,   802,    82,    92,   790,   255,    92,  1909,   651,   602,
       0,     0,     0,  1015,     0,    14,    15,    16,    17,    18,
       0,   802,   831,     0,   790,     0,   627,     0,     0,   802,
       0,     0,     0,    87,   832,  1027,     0,   689,     0,   630,
       0,     0,   314,   630,     0,   790,     0,     0,  1565,   790,
       0,  1432,     0,     0,   466,  1433,     0,     0,     0,     0,
       0,     0,     0,   255,   454,     0,     0,   880,     0,     0,
       0,     0,     0,   494,     0,   454,   888,   454,     0,   651,
       0,    64,     0,   255,     0,   454,     0,     0,     0,     0,
     161,   454,   159,    92,   184,   185,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,     0,  1973,
       0,   487,     0,     0,     0,     0,     0,     0,     0,   336,
       0,     0,     0,   971,  1990,     0,     0,   631,  1992,     0,
      80,   336,    92,     0,  2046,     0,     0,     0,     0,     0,
     199,     0,   790,    81,     0,    82,   195,     6,     7,     8,
       9,    10,    11,    12,    13,   426,     0,   487,   487,     0,
     790,   426,     0,   255,     0,   851,     0,   790,  1567,   724,
       0,  1520,     0,     0,     0,  1521,    87,    88,     0,  1522,
       0,     0,   367,  2087,     0,     0,     0,     0,  1973,     0,
       0,     0,     0,     0,   389,     0,   396,     0,   398,     0,
       0,    92,    92,  1045,     0,     0,  1009,     0,     0,    14,
      15,    16,    17,    18,     0,   339,     0,     0,     0,     0,
       0,     0,     0,   426,     0,   426,     0,     0,    94,     0,
       0,     0,   161,     0,   802,     0,     0,   367,     0,   428,
     396,   398,     0,   454,   833,   428,   833,     0,     0,     0,
       0,     0,     0,   494,     0,     0,   161,     0,     0,     0,
       0,  1207,    92,     0,   852,   852,     0,     0,   802,     0,
       0,     0,   487,     0,     0,    64,     0,  1331,     0,     0,
       0,     0,     0,     0,  1225,     0,     0,     0,  1225,     0,
       0,     0,     0,     0,   689,     0,   255,     0,   292,   494,
     494,   527,     0,     0,     0,     0,   159,  1117,   161,   428,
      71,    72,    73,    74,    75,    76,    77,    78,     0,  1207,
       0,     0,     0,   454,    80,   454,   356,     0,     0,     0,
     255,     0,   255,     0,  2179,     0,     0,    81,     0,    82,
       0,   790,     0,  2186,     0,   790,     0,     0,   367,     0,
       0,     0,     0,     0,   658,     0,   398,  1664,     0,  1829,
       0,  1665,  1225,   724,  1666,     0,     0,     0,     0,     0,
      87,    88,     0,     0,  1207,     0,  1225,     0,     0,   367,
     542,   454,   630,   689,     0,     0,  1027,     0,     0,     0,
       0,  1676,     0,     0,  1207,  1677,   630,   159,   494,  1678,
       0,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,     0,   494,     0,     0,     0,   454,   919,
       0,   735,   159,     0,   464,   465,    71,    72,    73,    74,
      75,    76,    77,    78,     0,   159,   454,   333,   334,    71,
      72,    73,    74,    75,    76,    77,    78,     0,  1249,    84,
       0,   367,   292,   396,   398,     0,   454,     0,     0,     0,
       0,   790,     0,     0,    81,   790,   971,  1207,     0,   790,
       0,    89,     0,     0,  1009,     0,    85,     0,  1251,     0,
     631,     0,   426,     0,     0,   367,  1703,    84,   466,   367,
       0,     0,     0,  1704,     0,     0,   367,    87,    88,     0,
       0,     0,     0,     0,     0,   161,     0,     0,    19,    89,
       0,  1799,     0,     0,     0,     0,   292,     0,     0,     0,
       0,   487,     0,     0,     0,     0,   454,     0,   367,   454,
     658,   398,     0,    14,    15,    16,    17,    18,   426,   434,
     161,    19,     0,     0,   487,     0,     0,     0,   689,     0,
       0,   833,     0,   152,     0,     0,   152,     0,    58,    59,
      60,    61,     0,     0,   857,     0,   428,     0,  1207,  1207,
     367,     0,   852,  1225,     0,   292,     0,     0,     0,     0,
       0,     0,   874,     0,     0,   877,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,   161,    64,
       0,     0,   159,     0,   184,   185,    71,    72,    73,    74,
      75,    76,    77,    78,   937,     0,     0,   152,     0,     0,
       0,     0,   428,   367,     0,     0,  1207,     0,     0,   367,
     454,   454,   159,     0,   333,   334,    71,    72,    73,    74,
      75,    76,    77,    78,     0,   292,   310,   790,    80,     0,
       0,   790,     0,   542,   790,     0,   542,   938,     0,     0,
       0,    81,   542,    82,     0,   292,   152,   367,     0,   658,
     398,   542,     0,     0,     0,   454,     0,     0,     0,     0,
       0,   790,     0,   626,    84,   790,   494,   627,   454,   790,
       0,     0,     0,     0,    87,   628,     0,     0,   292,     0,
       0,   542,     0,     0,     0,     0,    89,     0,     0,     0,
     152,     0,     0,   310,     0,   438,     0,     0,     0,     0,
       0,     0,     0,   439,   440,   441,   442,   367,   658,     0,
       0,     0,   161,     0,     0,     0,     0,     0,   367,     0,
       0,   161,     0,     0,     0,     0,  1490,     0,     0,     0,
     487,     0,     0,     0,   310,     0,   527,     0,     0,     0,
       0,     0,     0,   159,   967,   184,   185,    71,    72,    73,
      74,    75,    76,    77,    78,   487,   255,     0,     0,     0,
       0,     0,     0,   487,     0,     0,   735,     0,     0,     0,
     367,   310,     0,   558,     0,     0,     0,   689,     0,     0,
       0,   790,     0,  1648,   292,     0,   364,    94,     0,     0,
    1225,     0,     0,     0,   367,  1225,  1225,  1225,   443,   367,
     426,   367,     0,   161,     0,     0,   641,     0,     0,     0,
       0,     0,     0,     0,     0,   161,   444,     0,     0,     0,
       0,     0,     0,     0,   367,     0,   367,   367,     0,   833,
       0,     0,   672,     0,     0,     0,     0,     0,   367,     0,
       0,     0,     0,     0,     0,   454,   454,     0,     0,     0,
       0,   367,     0,     0,   292,     0,   292,     0,     0,     0,
       0,     0,     0,     0,   367,  1083,  1085,     0,     0,     0,
       0,   727,     0,     0,     0,     0,     0,     0,   727,     0,
       0,   159,     0,     0,   428,    71,    72,    73,    74,    75,
      76,    77,    78,   933,     0,     0,   310,     0,     0,     0,
       0,   689,     0,     0,     0,     0,   857,   857,     0,   159,
       0,   184,   185,    71,    72,    73,    74,    75,    76,    77,
      78,     0,  1170,  2114,     0,  1173,     0,   161,   161,   161,
     161,     0,   161,   161,     0,     0,   934,     0,  1705,   434,
       0,    64,   292,     0,     0,   494,     0,     0,     0,     0,
       0,     0,   487,     0,     0,     0,   487,   487,     0,     0,
     727,     0,     0,     0,     0,     0,   310,     0,     0,     0,
     487,  1569,     0,   487,   159,   367,   333,   334,    71,    72,
      73,    74,    75,    76,    77,    78,  1225,   641,  1225,     0,
      80,     0,     0,     0,     0,     0,     0,     0,   364,     0,
       0,     0,     0,    81,   228,    82,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,     0,   310,
     161,   727,  1710,     0,     0,  2042,    84,     0,     0,   724,
       0,     0,     0,   161,     0,   367,    87,    88,     0,   727,
     875,     0,   727,   878,   310,     0,     0,   310,    89,   310,
     310,     0,     0,   310,     0,     0,     0,   560,   367,     0,
      85,   479,   310,   367,     0,   310,   310,   310,   159,     0,
     184,   185,    71,    72,    73,    74,    75,    76,    77,    78,
     912,     0,     0,     0,  1705,  1830,     0,     0,     0,  1705,
       0,   487,     0,     0,     0,     0,  1705,     0,  1705,     0,
       0,     0,   292,    64,   735,     0,     0,     0,     0,     0,
       0,  1290,   494,     0,     0,  1294,     0,     0,     0,  1298,
       0,     0,   434,   161,   159,   689,  1088,   819,    71,    72,
      73,    74,    75,    76,    77,    78,   159,     0,   333,   334,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,   641,     0,
     292,     0,   641,   641,     0,    81,     0,    82,  1710,   641,
       0,     0,     0,  1710,     0,     0,     0,     0,   367,  1002,
    1845,     0,  1710,     0,     0,     0,     0,   335,    84,     0,
     367,     0,     0,     0,     0,     0,   159,     0,    87,    88,
      71,    72,    73,    74,    75,    76,    77,    78,     0,   558,
      89,   367,     0,   790,   857,   292,     0,     0,     0,     0,
       0,     0,  1830,  1830,     0,     0,     0,     0,     0,     0,
     310,     0,     0,   494,     0,   292,     0,  1705,     0,   727,
    1705,     0,     0,   727,     0,     0,     0,     0,    84,     0,
       0,  1026,   434,     0,     0,     0,     0,     0,     0,     0,
       0,   282,     0,     0,     0,     0,     0,     0,     0,     0,
     487,     0,     0,     0,     0,     0,   310,   310,     0,     0,
       0,     0,     0,     0,     0,   161,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1476,  1330,     0,     0,     0,
     367,     0,     0,     0,     0,     0,     0,     0,   292,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1830,     0,
       0,  1933,     0,     0,  1710,     0,     0,  1705,     0,     0,
       0,     0,     0,     0,  1510,     0,     0,  1514,     0,     0,
       0,  1518,     0,   727,     0,     0,     0,   152,     0,     0,
     159,   152,   776,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   912,   641,   161,   641,  2043,   434,     0,     0,
       0,     0,     0,     0,     0,   310,     0,     0,     0,     0,
       0,     0,     0,   727,   727,     0,     0,     0,     0,     0,
    1830,   310,     0,     0,     0,     0,     0,     0,     0,   727,
    1171,   161,   727,  1174,     0,  1012,     0,     0,     0,   292,
     292,  1710,     0,     0,     0,   282,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1524,     0,   558,     0,     0,
     597,   161,     0,     0,     0,  2043,  2043,     0,     0,     0,
       0,     0,     0,     0,     0,   367,     0,     0,     0,     0,
     428,     0,   656,     0,   292,     0,     0,    14,    15,    16,
      17,    18,     0,     0,     0,   161,     0,   292,     0,    64,
       0,   597,     0,     0,     0,   597,     0,     0,     0,   282,
     159,  1630,     0,   542,    71,    72,    73,    74,    75,    76,
      77,    78,  1222,     0,     0,     0,  2043,  1223,     0,  1224,
       0,   641,   159,   641,   333,   334,    71,    72,    73,    74,
      75,    76,    77,    78,     0,   641,     0,     0,    80,   811,
     428,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,    84,    82,     0,  1460,     0,   912,   282,   159,
       0,   218,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,  1249,    84,   292,   159,     0,   333,   334,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
       0,     0,    80,     0,     0,     0,    89,   727,     0,     0,
     428,   727,     0,     0,     0,    81,     0,    82,   727,  1291,
     282,    84,   727,  1295,  1026,     0,   727,  1299,   367,     0,
       0,     0,     0,  1302,     0,     0,     0,  2042,    84,     0,
       0,   724,     0,     0,     0,     0,     0,     0,    87,    88,
       0,     0,     0,     0,   152,     0,     0,     0,   282,   597,
      89,     0,     0,     0,    14,    15,    16,    17,    18,     0,
     152,     0,     0,     0,   292,   292,     0,     0,     0,   727,
       0,     0,     0,     0,     0,     0,     0,   367,     0,   152,
       0,   282,     0,   310,     0,     0,     0,     0,     0,     0,
     641,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1723,  1731,     0,     0,  1723,  1742,     0,     0,     0,     0,
    1749,   727,     0,     0,  1753,     0,     0,     0,     0,  1757,
      64,  1742,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   282,     0,     0,     0,     0,     0,   310,     0,     0,
       0,     0,     0,   597,     0,     0,     0,   656,     0,     0,
       0,     0,     0,   159,     0,   333,   334,    71,    72,    73,
      74,    75,    76,    77,    78,     0,   159,  1833,     0,    80,
      71,    72,    73,    74,    75,    76,    77,    78,  1222,     0,
       0,     0,    81,  1223,    82,  1224,     0,   282,     0,     0,
       0,     0,   727,  1477,     0,   641,   641,  1484,     0,     0,
       0,   367,     0,     0,   335,    84,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    87,    88,     0,    84,     0,
       0,  1656,     0,     0,     0,     0,     0,    89,     0,     0,
       0,   727,  1511,     0,   727,  1515,     0,     0,   727,  1519,
       0,     0,     0,     0,   127,   282,   597,   127,     0,     0,
       0,     0,   159,     0,   778,   779,    71,    72,    73,    74,
      75,    76,    77,    78,     0,   282,  1858,   597,     0,     0,
       0,   152,     0,   282,     0,     0,     0,     0,     0,     0,
     310,     0,     0,     0,     0,     0,  1878,  1880,     0,   152,
       0,     0,   159,     0,   333,   334,    71,    72,    73,    74,
      75,    76,    77,    78,  1833,  1833,    85,     0,   127,     0,
       0,     0,     0,     0,   310,     0,     0,  1900,     0,   367,
       0,    81,   152,     0,     0,     0,     0,     0,     0,   285,
       0,   367,     0,     0,     0,     0,     0,   127,     0,     0,
       0,     0,     0,   626,    84,   282,   152,   627,     0,     0,
       0,     0,     0,   371,    87,   628,     0,   127,     0,     0,
       0,     0,   310,     0,     0,     0,    89,   629,     0,     0,
       0,     0,     0,     0,   152,     0,     0,     0,   727,  1631,
       0,     0,     0,     0,     0,     0,     0,     0,   641,     0,
       0,   127,     0,     0,     0,   127,     0,   367,   480,     0,
    1833,   127,  1944,     0,   127,     0,     0,     0,   371,     0,
    1947,     0,  1949,     0,   597,  1953,  1959,     0,  1742,   451,
     127,     0,   468,  1965,   513,   597,   516,     0,   367,   398,
     480,   522,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   531,   532,     0,     0,   285,   367,   159,  1833,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   480,
     480,     0,     0,     0,     0,     0,     0,     0,  2062,     0,
       0,     0,  1833,     0,     0,     0,     0,     0,   282,     0,
       0,     0,   285,   285,   127,     0,   310,   310,   310,   152,
       0,   310,   310,     0,     0,     0,  2025,     0,   285,     0,
       0,    85,     0,  2032,  2034,     0,     0,   597,     0,     0,
       0,   152,   282,     0,   597,   152,   152,  1833,  1833,   371,
     127,     0,     0,     0,     0,     0,  2056,     0,     0,   310,
       0,     0,   152,     0,     0,     0,     0,     0,     0,   127,
       0,     0,     0,   127,     0,     0,     0,   285,     0,     0,
     371,     0,     0,   127,     0,     0,     0,  2078,     0,  2081,
       0,     0,  2083,  2085,     0,     0,     0,     0,     0,  2090,
    2092,     0,     0,     0,     0,   367,     0,     0,  1833,   152,
       0,     0,     0,     0,     0,     0,     0,     0,   127,     0,
       0,     0,   310,     0,     0,     0,     0,     0,     0,     0,
     597,     0,     0,     0,  1562,   127,   127,   127,     0,     0,
       0,     0,     0,     0,     0,  1579,     0,   127,     0,     0,
       0,     0,   371,     0,     0,     0,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,  2128,  2130,  2132,     0,
       0,     0,     0,   781,   727,     0,   127,     0,     0,     0,
     310,   127,     0,   127,     0,     0,   371,   127,   285,     0,
     371,     0,   727,     0,  2151,  2153,  2155,   371,     0,     0,
       0,     0,     0,     0,     0,     0,   127,   127,     0,     0,
       0,     0,   152,     0,     0,   480,     0,   597,     0,     0,
       0,   480,     0,     0,     0,     0,   285,   127,   159,   371,
       0,     0,    71,    72,    73,    74,    75,    76,    77,    78,
    1222,     0,     0,     0,     0,  1223,     0,  1224,     0,     0,
       0,     0,   597,     0,     0,     0,     0,     0,   159,   285,
     285,     0,    71,    72,    73,    74,    75,    76,    77,    78,
    1222,     0,     0,     0,     0,  1223,     0,  1224,     0,     0,
      84,     0,     0,  1658,     0,   285,     0,     0,   285,     0,
     127,   127,   468,     0,   285,     0,     0,     0,     0,     0,
       0,     0,     0,   285,     0,     0,   127,   127,   127,   285,
      84,   727,   727,     0,     0,     0,     0,     0,     0,     0,
       0,   127,     0,     0,   921,   285,     0,   727,     0,     0,
     371,   127,     0,     0,     0,     0,     0,     0,   480,   480,
     480,   480,   480,   480,   480,   480,   480,   480,   480,   480,
     480,   480,   480,   480,   480,   480,   480,     0,     0,   152,
       0,     0,     0,     0,     0,   285,     0,     0,   371,     0,
       0,     0,     0,     0,   310,     0,     0,     0,     0,     0,
       0,     0,     0,   480,     0,     0,     0,     0,     0,  1777,
    1778,     0,     0,     0,     0,     0,     0,     0,     0,   127,
       0,     0,     0,     0,     0,     0,     0,   727,     0,     0,
       0,     0,     0,     0,     0,   727,     0,     0,     0,    64,
       0,     0,     0,   285,   127,     0,     0,     0,   371,     0,
       0,     0,     0,   781,     0,   127,     0,   127,     0,   371,
       0,     0,     0,   285,   597,   127,     0,     0,     0,     0,
     127,   127,   159,   310,     0,   727,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,    80,     0,
       0,   285,     0,     0,     0,   727,  2063,     0,     0,   727,
       0,    81,     0,    82,     0,     0,     0,     0,   282,     0,
     152,     0,     0,     0,     0,     0,     0,  1074,     0,     0,
       0,     0,     0,    83,    84,     0,   127,     0,     0,     0,
       0,     0,     0,     0,    87,    88,     0,   285,   285,     0,
     152,     0,   371,   285,   727,   727,    89,  1862,     0,     0,
       0,     0,     0,     0,  1872,     0,     0,     0,     0,     0,
     127,     0,     0,     0,     0,     0,     0,   597,     0,     0,
       0,     0,     0,     0,   152,     0,     0,     0,     0,   371,
       0,     0,     0,     0,     0,     0,     0,  1897,     0,     0,
       0,   159,   371,   184,   185,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,   727,   127,     0,   127,     0,
       0,   159,   127,   184,   185,    71,    72,    73,    74,    75,
      76,    77,    78,   127,   127,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,     0,   127,     0,     0,     0,
       0,     0,   625,     0,   127,   480,     0,     0,     0,     0,
     480,     0,   285,     0,     0,     0,     0,     0,     0,     0,
       0,   480,   675,     0,     0,   127,     0,     0,     0,   127,
    1935,  1936,     0,   480,     0,     0,   285,  1946,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,     0,
    1960,     0,     0,     0,    64,     0,     0,     0,     0,     0,
    1969,     0,  1970,   127,     0,   127,     0,     0,     0,     0,
     285,     0,   285,     0,     0,  1984,   921,  1986,  1987,  1988,
     480,     0,     0,     0,     0,     0,     0,   159,     0,   333,
     334,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,  2009,    81,     0,    82,  2014,
       0,   127,   127,     0,  2019,     0,     0,     0,     0,     0,
       0,     0,   597,     0,     0,     0,     0,     0,   425,    84,
     127,     0,     0,     0,   187,   190,     0,   127,     0,    87,
      88,   480,     0,     0,     0,     0,     0,     0,   127,   921,
       0,    89,     0,     0,   205,     0,     0,     0,     0,     0,
       0,     0,     0,  2065,     0,    64,   127,     0,     0,     0,
       0,     0,     0,     0,   327,  2074,     0,     0,     0,  2077,
       0,     0,     0,     0,     0,     0,   127,     0,     0,     0,
       0,     0,     0,     0,  2093,     0,     0,     0,   159,     0,
     333,   334,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,   159,    80,   184,   185,    71,    72,    73,
      74,    75,    76,    77,    78,   420,     0,    81,   421,    82,
       0,     0,     0,     0,     0,   127,     0,     0,     0,  2121,
       0,     0,     0,   445,     0,     0,     0,   127,     0,  1703,
      84,   285,     0,     0,     0,     0,   127,     0,     0,   127,
      87,    88,     0,   205,     0,     0,     0,     0,     0,   371,
     127,     0,    89,     0,   285,     0,  2142,     0,     0,     0,
       0,  2145,     0,     0,     0,     0,  1417,     0,     0,     0,
       0,     0,   480,   480,   480,     0,     0,     0,     0,   480,
     480,     0,     0,     0,     0,     0,     0,  2163,     0,     0,
    2165,   127,  2145,    64,     0,   127,     0,     0,     0,     0,
       0,     0,   127,     0,     0,     0,     0,     0,   127,     0,
       0,  2165,     0,     0,     0,     0,     0,     0,     0,     0,
     480,     0,     0,     0,     0,   327,   159,     0,   333,   334,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
     127,   127,    80,     0,     0,     0,     0,     0,     0,   669,
     480,     0,   480,   676,     0,    81,     0,    82,     0,    14,
      15,    16,    17,    18,     0,     0,     0,   597,     0,     0,
       0,     0,     0,     0,     0,     0,   699,   425,    84,     0,
       0,     0,     0,     0,     0,   127,     0,     0,    87,    88,
       0,     0,     0,     0,   326,     0,     0,     0,   127,   159,
      89,   184,   185,    71,    72,    73,    74,    75,    76,    77,
      78,   127,     0,   110,     0,   127,   165,     0,   327,   127,
       0,   480,     0,     0,     0,    64,     0,   760,   761,     0,
       0,  1528,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,   127,     0,     0,     0,   597,     0,     0,     0,
       0,   127,     0,   187,     0,     0,     0,     0,   159,     0,
     285,     0,    71,    72,    73,    74,    75,    76,    77,    78,
       0,   804,     0,     0,    80,     0,     0,   110,     0,     0,
       0,     0,     0,     0,     0,   285,   285,    81,     0,    82,
       0,     0,     0,   285,     0,   818,   820,     0,   276,     0,
     827,     0,     0,     0,     0,     0,   307,     0,     0,    83,
      84,     0,     0,     0,     0,     0,   371,   127,     0,     0,
      87,    88,     0,     0,   445,     0,   379,   445,   205,     0,
       0,     0,    89,   127,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   127,     0,     0,   205,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     413,     0,     0,     0,   417,     0,     0,     0,     0,     0,
     110,     0,     0,     0,     0,   127,   127,   577,   578,   579,
     580,   581,   582,   583,   584,   585,   586,   587,     0,   456,
       0,     0,   318,     0,     0,     0,     0,   127,     0,     0,
       0,   127,     0,   159,   127,   333,   334,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,     0,     0,   588,
       0,   498,     0,     0,     0,   939,     0,     0,     0,     0,
       0,   127,    81,     0,     0,   127,     0,     0,     0,   127,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   551,   557,   335,    84,     0,   127,   127,   127,
     127,   127,   127,   127,     0,    87,    88,   595,     0,   371,
       0,     0,     0,     0,     0,     0,     0,    89,     0,     0,
       0,     0,   285,     0,     0,     0,   285,   285,     0,   655,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
     285,     0,     0,   285,     0,     0,  1010,   480,   666,     0,
       0,     0,   666,     0,     0,     0,   276,     0,     0,     0,
       0,     0,   557,     0,     0,     0,     0,     0,   371,   159,
       0,   333,   334,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,  1793,     0,    80,     0,     0,   718,     0,
     127,   127,     0,     0,     0,     0,     0,   413,    81,     0,
      82,     0,     0,   127,     0,     0,     0,     0,   742,     0,
       0,     0,     0,   747,   749,   276,   307,     0,     0,     0,
    1703,    84,     0,     0,     0,     0,   413,     0,     0,     0,
       0,    87,    88,     0,     0,     0,     0,   413,     0,     0,
     772,     0,     0,    89,   774,     0,     0,     0,     0,   775,
       0,     0,     0,     0,     0,   786,     0,     0,     0,   121,
     749,   285,   413,     0,     0,     0,   799,   456,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   812,     0,
       0,     0,     0,     0,     0,   276,     0,     0,     0,     0,
       0,     0,   371,   127,     0,  1114,     0,     0,     0,   480,
       0,     0,     0,     0,     0,   551,   595,     0,     0,  1120,
       0,     0,     0,     0,     0,     0,     0,     0,   445,     0,
       0,     0,   159,   121,   333,   334,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,   276,     0,
       0,     0,     0,     0,   280,     0,     0,     0,     0,     0,
     159,    81,   333,   334,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   380,  1703,    84,     0,     0,     0,     0,    81,
    1704,     0,     0,     0,    87,    88,     0,     0,   456,     0,
       0,     0,     0,     0,     0,     0,    89,     0,     0,     0,
     666,  2042,    84,     0,   923,   724,   121,     0,     0,     0,
     557,     0,    87,    88,     0,     0,   121,     0,     0,     0,
       0,     0,   371,   159,    89,   333,   334,    71,    72,    73,
      74,    75,    76,    77,    78,   457,     0,   285,     0,     0,
     285,     0,     0,     0,   551,     0,     0,     0,     0,     0,
       0,     0,    81,     0,     0,   127,     0,     0,     0,     0,
       0,   480,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   425,    84,     0,     0,   786,     0,
       0,     0,   983,     0,     0,    87,    88,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    89,   280,     0,
       0,     0,  1006,   595,     0,   480,     0,     0,     0,  1011,
       0,     0,     0,   596,   276,     0,   276,  1264,  2027,   371,
       0,     0,   456,     0,   666,     0,     0,     0,     0,   557,
     456,     0,     0,     0,   127,   380,     0,   371,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   596,     0,     0,     0,   596,     0,
       0,     0,   280,     0,   480,     0,     0,     0,     0,     0,
       0,   127,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   786,     0,  2088,     0,     0,
     480,     0,   480,     0,     0,     0,     0,     0,     0,  1333,
       0,   127,   551,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,   280,     0,   127,     0,     0,     0,     0,     0,     0,
     480,     0,   121,     0,     0,   127,     0,     0,     0,  1420,
       0,     0,     0,   121,     0,     0,   773,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   457,     0,     0,     0,   786,     0,   110,   121,     0,
       0,  1126,   380,   280,     0,     0,     0,     0,     0,     0,
       0,     0,   666,   786,     0,  1138,     0,     0,     0,     0,
       0,   457,     0,     0,     0,   480,     0,     0,     0,     0,
       0,     0,     0,   786,  1159,     0,     0,     0,     0,     0,
       0,   280,   596,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   786,     0,     0,     0,   786,     0,
       0,     0,     0,     0,     0,   456,     0,     0,     0,     0,
       0,     0,     0,     0,   280,     0,     0,   557,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   276,     0,   666,     0,     0,     0,     0,  1006,
       0,   595,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   280,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   596,     0,     0,     0,
     380,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     276,   786,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1248,     0,     0,  1566,  1568,  1570,     0,   786,
    1576,     0,     0,     0,     0,     0,   786,     0,     0,     0,
     280,     0,     0,     0,     0,     0,     0,   666,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   276,     0,     0,     0,     0,
       0,     0,     0,     0,   457,     0,     0,     0,  1333,     0,
       0,     0,     0,  1609,     0,   276,     0,  1610,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   280,   596,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     457,     0,   457,     0,     0,     0,     0,     0,   280,     0,
     596,     0,     0,     0,     0,     0,   280,     0,     0,     0,
       0,     0,     0,     0,  1126,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   786,     0,     0,     0,
    1393,     0,     0,     0,     0,   456,     0,     0,   276,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1126,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   457,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   280,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     786,     0,     0,     0,   786,   121,     0,     0,     0,     0,
       0,   786,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1716,  1717,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   276,
     276,   457,     0,   121,     0,     0,     0,   596,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   596,   457,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   747,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   457,
       0,     0,     0,  1794,   276,   126,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   276,     0,     0,
     457,     0,     0,     0,   457,     0,     0,     0,     0,     0,
     786,   280,     0,     0,   786,     0,     0,     0,   786,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   457,     0,
     596,  1126,     0,     0,     0,   280,     0,   596,     0,   126,
    1546,     0,     0,     0,     0,     0,     0,     0,     0,  1393,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     284,     0,     0,     0,     0,     0,  1855,     0,     0,     0,
       0,     0,     0,     0,     0,   276,     0,     0,     0,     0,
       0,     0,  1393,     0,     0,     0,   457,   457,   382,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   457,  1605,     0,     0,     0,
       0,     0,   457,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   126,   596,     0,     0,     0,     0,     0,     0,
       0,     0,   126,     0,  1126,     0,     0,     0,     0,     0,
       0,   457,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   459,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   457,     0,     0,   276,   276,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   786,     0,     0,     0,
     786,     0,     0,   786,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     596,     0,     0,     0,   284,     0,     0,     0,     0,     0,
     786,     0,   457,     0,   786,     0,   121,     0,   786,   598,
       0,   457,     0,     0,   457,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   596,  1546,  1546,  1546,   165,
     749,   382,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     598,  1729,     0,     0,   598,  1729,  1729,     0,   284,     0,
       0,     0,     0,     0,     0,     0,   457,     0,     0,     0,
     457,     0,  1729,     0,     0,     0,     0,   457,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   126,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   457,   457,   284,     0,  1126,
     786,     0,     0,     0,     0,     0,     0,     0,   126,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   126,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   459,     0,     0,
     457,     0,     0,     0,   126,     0,     0,     0,   382,   284,
       0,     0,     0,   457,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   457,   459,     0,     0,
     457,     0,     0,     0,   457,     0,     0,  1847,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   284,   598,     0,
       0,     0,     0,     0,     0,     0,     0,   596,     0,     0,
       0,     0,  1859,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   121,     0,     0,     0,     0,
     284,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   280,     0,     0,     0,     0,     0,     0,   121,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     284,     0,   380,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   598,     0,     0,     0,   382,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     596,     0,  1925,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   284,     0,     0,     0,
     457,   457,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1956,     0,     0,  1729,
       0,   132,   457,     0,   132,     0,   457,     0,     0,   457,
     459,     0,     0,     0,  1974,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   284,   598,   457,     0,     0,     0,
     457,     0,     0,     0,   457,     0,   459,     0,   459,     0,
       0,     0,     0,     0,   284,     0,   598,     0,     0,     0,
     131,     0,   284,   131,     0,   132,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   121,     0,     0,
       0,   121,   121,  1974,   132,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
       0,     0,     0,     0,   132,     0,     0,   459,     0,     0,
       0,     0,     0,   395,   131,     0,     0,     0,     0,     0,
    1729,     0,     0,     0,   284,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   132,     0,
       0,   126,   132,   131,     0,   596,   457,     0,   132,     0,
    1729,   132,     0,     0,     0,  2103,     0,     0,     0,     0,
       0,     0,     0,   131,     0,     0,     0,     0,     0,     0,
       0,     0,   786,     0,     0,     0,   102,     0,     0,   102,
       0,     0,     0,     0,  1729,     0,     0,   459,     0,   126,
       0,     0,   132,   598,     0,     0,     0,   131,     0,     0,
       0,   131,     0,     0,   598,   459,     0,   131,     0,     0,
     131,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   459,     0,     0,     0,   132,
       0,   132,     0,     0,     0,     0,     0,     0,     0,     0,
     102,     0,     0,     0,     0,     0,   459,     0,   380,     0,
     459,   131,     0,     0,     0,     0,     0,   284,     0,     0,
       0,   273,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   459,   366,   598,     0,   131,   102,
     131,   284,     0,   598,     0,     0,     0,     0,     0,     0,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   102,     0,   132,     0,     0,     0,     0,
     435,     0,   459,   459,     0,     0,     0,     0,     0,     0,
       0,     0,   132,     0,   132,     0,     0,     0,     0,   131,
     132,   459,     0,     0,   132,     0,     0,     0,   459,     0,
       0,     0,     0,     0,     0,   132,     0,     0,     0,   598,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   131,   121,     0,   459,   132,     0,
     132,     0,     0,     0,   132,     0,     0,     0,     0,     0,
     596,   131,     0,   131,     0,   273,     0,   459,     0,   131,
       0,     0,     0,   131,   132,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   131,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   131,     0,   131,
       0,     0,     0,   131,     0,     0,   598,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   132,   459,   273,
       0,     0,   126,   131,     0,     0,     0,   459,     0,   596,
     459,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   598,   132,     0,     0,   132,     0,   132,   132,     0,
       0,   132,     0,     0,     0,     0,     0,     0,     0,     0,
     132,     0,     0,   132,   132,   132,   121,     0,     0,     0,
       0,     0,   136,     0,     0,   136,   131,     0,   273,     0,
       0,     0,   459,     0,     0,     0,   459,     0,   132,     0,
       0,     0,     0,   459,   366,     0,   121,     0,     0,     0,
       0,   131,     0,     0,   131,     0,   131,   131,     0,     0,
     131,     0,     0,     0,     0,     0,     0,     0,   457,   131,
       0,     0,   131,   131,   131,     0,     0,     0,     0,     0,
     121,   459,   459,     0,     0,     0,   136,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   131,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   136,     0,     0,   273,     0,
       0,     0,     0,     0,     0,     0,   459,     0,     0,     0,
       0,     0,     0,     0,     0,   136,     0,     0,     0,   459,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   273,   459,     0,     0,     0,   459,   132,     0,     0,
     459,     0,     0,     0,     0,     0,     0,     0,     0,   136,
       0,     0,     0,   136,     0,     0,     0,     0,   132,   136,
       0,     0,   136,   598,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   126,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   131,     0,     0,     0,
       0,     0,     0,   136,   132,   132,     0,   284,     0,     0,
       0,     0,     0,     0,   126,     0,     0,   131,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   132,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   273,   382,     0,
     136,     0,   136,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   131,   131,     0,   598,     0,     0,     0,
       0,     0,     0,     0,     0,   132,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   131,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   459,   459,     0,     0,
       0,     0,     0,   132,     0,     0,     0,   273,     0,   273,
       0,   136,     0,     0,     0,     0,     0,     0,   459,   132,
       0,     0,   459,     0,     0,   459,     0,     0,     0,     0,
       0,     0,     0,     0,   131,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   136,     0,     0,     0,
       0,     0,   459,     0,     0,   132,   459,     0,     0,     0,
     459,     0,   131,   136,     0,   136,     0,     0,     0,     0,
       0,   136,     0,     0,     0,   136,     0,     0,   131,   220,
       0,     0,   221,     0,   222,   223,   136,   224,     0,     0,
       0,     0,     0,     0,     0,   273,     0,     0,     0,     0,
       0,     0,     0,   126,   226,     0,     0,   126,   126,   136,
       0,   136,     0,     0,   131,   136,     0,     0,     0,     0,
       0,     0,     0,     0,   126,     0,     0,     0,     0,     0,
       0,     0,     0,   227,   228,   136,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,     0,   234,   235,   236,     0,   237,   238,     0,
     102,     0,     0,     0,   102,    81,     0,     0,     0,     0,
       0,   598,   459,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2000,   239,   136,     0,
      85,   479,     0,     0,     0,     0,     0,   242,    87,    88,
     244,   245,   246,   247,     0,     0,     0,     0,     0,     0,
     861,     0,     0,   136,     0,     0,   136,     0,   136,   136,
       0,     0,   136,     0,     0,     0,     0,     0,     0,     0,
       0,   136,     0,     0,   136,   136,   136,     0,     0,     0,
     220,     0,     0,   221,     0,   222,   223,     0,   224,     0,
       0,     0,     0,     0,     0,   273,     0,     0,     0,   136,
       0,     0,     0,     0,     0,   226,     0,     0,   132,     0,
       0,     0,     0,     0,   382,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   132,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,   273,   234,   235,   236,     0,   237,   238,
       0,     0,     0,     0,     0,     0,    81,   131,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   132,     0,     0,   239,     0,
     131,    85,   479,     0,     0,     0,     0,   181,   242,    87,
      88,   244,   245,   246,   247,     0,     0,     0,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   136,     0,
       0,     0,     0,     0,     0,     0,   181,     0,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   136,
       0,     0,     0,     0,   131,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1957,     0,
       0,   126,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,   598,   102,     0,     0,
       0,     0,     0,     0,     0,   136,   136,     0,   181,     0,
     181,     0,     0,   102,     0,     0,     0,     0,     0,     0,
       0,   273,     0,     0,     0,     0,     0,     0,   136,     0,
       0,   435,   102,     0,     0,     0,     0,     0,     0,     0,
       0,   181,     0,   471,     0,     0,     0,     0,   132,     0,
       0,     0,     0,     0,     0,     0,     0,   132,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   471,     0,   598,   136,     0,     0,     0,
       0,     0,   132,     0,     0,     0,     0,     0,   181,     0,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   136,     0,     0,   131,     0,     0,
       0,     0,   126,     0,   132,     0,   131,     0,     0,     0,
     136,     0,   273,   273,     0,     0,     0,     0,     0,     0,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   131,   126,     0,     0,     0,     0,     0,     0,   131,
       0,   181,     0,     0,     0,     0,   136,     0,     0,     0,
       0,     0,     0,     0,   459,     0,     0,   273,     0,     0,
       0,     0,     0,   131,     0,     0,   126,     0,     0,     0,
     273,     0,     0,     0,     0,     0,     0,     0,     0,   131,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
       0,     0,   181,     0,     0,   181,   181,     0,     0,   181,
       0,     0,   181,   181,     0,   181,     0,   181,     0,     0,
       0,     0,     0,     0,   102,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   102,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   132,   132,   132,   132,   132,   132,
     132,     0,     0,     0,     0,     0,     0,     0,   273,     0,
       0,     0,   179,     0,     0,   102,     0,     0,     0,   132,
       0,     0,     0,   132,   132,     0,     0,     0,   181,     0,
       0,   181,     0,     0,     0,     0,     0,   132,   366,   102,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   131,   131,   131,   131,   131,   131,   131,
       0,     0,     0,     0,     0,     0,     0,   102,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   131,     0,
       0,     0,   131,   131,     0,     0,     0,     0,     0,     0,
     400,     0,     0,     0,     0,     0,   131,   273,   273,   131,
     132,     0,     0,   406,     0,   407,     0,     0,     0,   136,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   136,     0,     0,     0,   463,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   132,   131,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   102,   519,     0,     0,   136,     0,     0,     0,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   102,     0,     0,     0,   102,   102,
       0,     0,     0,     0,     0,     0,     0,   131,     0,     0,
       0,     0,     0,     0,     0,   102,     0,     0,     0,     0,
       0,     0,     0,     0,   181,     0,     0,   181,   181,     0,
     181,     0,   181,   181,     0,     0,   660,   181,   181,   131,
     366,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   102,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   220,     0,     0,   221,
       0,   222,   223,     0,   224,     0,     0,     0,     0,     0,
     729,   730,     0,     0,   734,     0,     0,   737,   738,   471,
     740,   226,   741,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,     0,     0,     0,   136,
       0,     0,     0,     0,     0,     0,     0,   132,   136,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,     0,
     234,   235,   236,   136,   237,   238,     0,     0,     0,     0,
       0,   136,    81,     0,   366,   102,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   239,   136,   131,    85,   479,     0,
       0,     0,   687,     0,   242,    87,    88,   244,   245,   246,
     247,   136,     0,   471,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
       0,   181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,   181,
       0,     0,     0,     0,     0,   873,     0,     0,   132,     0,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   270,   132,     0,
       0,     0,     0,     0,   366,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   102,     0,     0,     0,     0,   131,     0,     0,
       0,     0,   132,     0,     0,   136,   136,   136,   136,   136,
     136,   136,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   131,     0,     0,
     136,     0,     0,     0,   136,   136,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   136,     0,
       0,   136,     0,     0,   181,     0,     0,     0,     0,   988,
       0,   131,   991,   992,     0,   995,     0,   997,   998,     0,
       0,     0,  1000,  1001,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   488,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1486,     0,     0,
       0,   136,     0,     0,     0,    14,    15,    16,    17,    18,
       0,   550,     0,   102,     0,     0,     0,     0,     0,   181,
       0,     0,     0,   181,     0,     0,     0,   181,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1069,     0,     0,   102,     0,     0,     0,   220,     0,     0,
     221,     0,   222,   223,     0,   224,     0,     0,     0,   136,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,   226,     0,     0,   270,     0,   102,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     690,   136,   690,     0,     0,     0,     0,     0,     0,     0,
     181,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
      80,   234,   235,   236,     0,   237,   238,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1139,   239,  1141,     0,    85,   479,
       0,     0,     0,     0,     0,   242,  1487,    88,   244,   245,
     246,   247,     0,  1161,  1162,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1169,   801,     0,     0,     0,
       0,     0,     0,     0,   809,     0,     0,     0,     0,     0,
       0,     0,     0,   181,     0,     0,   181,   181,     0,     0,
       0,     0,     0,     0,   181,   181,     0,     0,     0,     0,
       0,     0,     0,     0,   270,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   181,   136,   181,
       0,     0,   181,     0,     0,   181,     0,     0,     0,   181,
     848,     0,     0,     0,     0,     0,     0,   550,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   865,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   889,     0,     0,     0,     0,  1250,
       0,     0,     0,     0,     0,     0,     0,   550,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   920,   922,     0,   270,     0,     0,     0,     0,
       0,     0,     0,     0,   929,     0,   931,     0,     0,     0,
       0,     0,     0,   865,     0,   941,   865,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     945,     0,     0,   550,     0,     0,     0,     0,     0,   136,
       0,     0,     0,     0,  1289,     0,     0,     0,  1293,     0,
       0,     0,  1297,     0,     0,     0,     0,     0,     0,   181,
       0,     0,     0,     0,     0,     0,     0,     0,   270,   136,
     982,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   270,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   136,     0,     0,   181,     0,     0,     0,
       0,   270,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   801,     0,  1250,     0,   848,   690,     0,
       0,     0,     0,     0,   690,     0,     0,     0,     0,   488,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   301,     0,     0,     0,     0,     0,     0,     0,
     313,     0,     0,     0,     0,  1072,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   270,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,     0,   181,     0,     0,     0,     0,   313,  1475,     0,
       0,  1479,  1482,     0,     0,     0,     0,     0,     0,  1491,
    1492,     0,     0,   462,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1504,     0,  1506,     0,     0,  1509,   313,     0,
    1513,     0,     0,     0,  1517,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1163,     0,     0,     0,     0,   546,   301,     0,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   601,     0,     0,   270,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   181,     0,     0,     0,
       0,     0,   654,   181,  1194,     0,     0,     0,     0,  1201,
       0,     0,     0,     0,     0,     0,     0,  1201,   550,     0,
       0,     0,   668,     0,  1216,     0,   674,     0,     0,   865,
     301,     0,     0,   681,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   690,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1629,     0,     0,     0,     0,   181,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   301,
     313,     0,     0,     0,     0,     0,     0,     0,     0,   181,
     181,     0,     0,     0,     0,   375,     0,   920,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1479,     0,     0,     0,     0,     0,     0,     0,   181,
     181,     0,     0,     0,   690,     0,     0,   471,     0,     0,
       0,   462,   181,   803,     0,  1278,     0,     0,     0,     0,
     681,     0,     0,     0,     0,     0,     0,     0,     0,   301,
     313,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   301,
     601,     0,   834,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   301,   313,     0,     0,     0,     0,     0,     0,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   488,     0,     0,     0,     0,  1773,   313,     0,
       0,   546,     0,   546,   313,     0,     0,   313,     0,     0,
       0,     0,     0,     0,     0,     0,   546,     0,     0,   546,
     546,   546,   462,     0,     0,     0,     0,     0,   865,     0,
       0,     0,     0,     0,   914,     0,     0,   654,     0,     0,
       0,     0,     0,     0,     0,     0,   181,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   690,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   301,     0,
       0,   946,     0,  1819,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1201,     0,   181,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1489,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   690,     0,     0,   462,   601,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   301,     0,
     301,     0,     0,     0,     0,     0,   462,     0,  1022,     0,
       0,     0,     0,     0,   462,     0,     0,  1525,     0,  1526,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   313,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1906,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1915,  1916,     0,  1582,  1582,     0,
       0,     0,     0,     0,   550,     0,     0,     0,     0,     0,
     313,   313,     0,     0,     0,     0,   301,     0,     0,     0,
       0,     0,     0,     0,  1929,  1930,     0,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,  1934,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,  1632,     0,    52,     0,    53,     0,     0,     0,     0,
     225,    55,    56,    57,    58,    59,    60,    61,   690,     0,
    1645,     0,     0,     0,     0,    64,   914,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   313,
       0,     0,     0,     0,     0,  1997,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   313,  1667,  1668,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,   462,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   301,     0,  1210,     0,
       0,  2061,    85,   462,     0,   601,     0,     0,     0,     0,
      87,    88,     0,   865,     0,     0,     0,     0,     0,   220,
       0,     0,   221,     0,   222,   223,     0,   224,     0,     0,
       0,     0,   690,     0,     0,     0,     0,     0,   488,     0,
       0,     0,     0,     0,   226,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   301,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   684,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,   914,   834,   234,   235,   236,     0,   237,   238,  1801,
       0,     0,  1803,     0,     0,    81,     0,     0,     0,   301,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1645,     0,     0,     0,     0,   239,    84,   301,
     685,   686,     0,     0,     0,   687,     0,   242,    87,    88,
     244,   245,   246,   247,     0,  1823,     0,     0,   195,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,   488,
       0,     0,    19,     0,    20,  1846,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,   357,   358,   462,
     359,    52,   301,    53,     0,     0,   360,     0,     0,    55,
      56,    57,    58,    59,    60,    61,     0,   313,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1893,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   690,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   546,     0,     0,     0,     0,     0,     0,     0,     0,
    2164,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1561,     0,     0,
       0,     0,     0,   301,   301,     0,     0,     0,     0,  -477,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -477,     0,   220,     0,     0,   221,     0,   222,
     223,     0,   224,     0,     0,     0,     0,     0,   301,     0,
       0,     0,     0,     0,     0,     0,     0,  1336,     0,   226,
    1338,   301,  1339,  -256,  -256,  1340,  1341,  1342,  1343,  1344,
    1345,  1346,  1347,  1348,  1349,  1350,  -353,  -353,  1351,  1352,
    1353,  1354,  1355,  1356,  1357,     0,  1358,     0,   227,   228,
       0,   684,   230,  1359,  1360,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,  1361,     0,   234,   235,
     236,     0,   237,   238,   546,     0,     0,     0,     0,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -256,  1362,   865,     0,    85,   479,     0,   546,   301,
     404,     0,   242,    87,    88,   244,   245,   246,   247,     0,
       0,     0,     0,     0,     0,     0,     0,  -196,  2164,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   375,
       0,     0,     0,     0,     0,  1561,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   313,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   220,     0,     0,   221,     0,   222,   223,     0,
     224,     0,     0,     0,     0,     0,     0,     0,   301,   301,
       0,     0,     0,     0,     0,  1336,     0,   226,  1338,     0,
    1339,  -257,  -257,  1340,  1341,  1342,  1343,  1344,  1345,  1346,
    1347,  1348,  1349,  1350,  -353,  -353,  1351,  1352,  1353,  1354,
    1355,  1356,  1357,     0,  1358,     0,   227,   228,     0,   684,
     230,  1359,  1360,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,  1361,     0,   234,   235,   236,     0,
     237,   238,     0,     0,     0,     0,     0,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     546,   546,   546,     0,     0,   546,   546,     0,     0,  -257,
    1362,     0,   681,    85,   479,     0,     0,     0,   404,     0,
     242,    87,    88,   244,   245,   246,   247,     0,  1896,     0,
       0,     0,     0,     0,     0,  -196,     0,     0,     0,     0,
       0,     0,     0,   313,     0,  1561,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   220,     0,     0,   221,     0,   222,   223,     0,
     224,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1336,   546,   226,  1338,     0,
    1339,     0,     0,  1340,  1341,  1342,  1343,  1344,  1345,  1346,
    1347,  1348,  1349,  1350,  -353,  -353,  1351,  1352,  1353,  1354,
    1355,  1356,  1357,     0,  1358,     0,   227,   228,     0,   684,
     230,  1359,  1360,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,  1361,     0,   234,   235,   236,     0,
     237,   238,     0,     0,   313,     0,     0,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1362,     0,     0,    85,   479,   375,     0,     0,   404,     0,
     242,    87,    88,   244,   245,   246,   247,     0,     0,     0,
       0,     0,     0,     0,     0,  -196,     0,     0,     0,     0,
       0,     0,     4,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,  1335,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   220,     0,    52,   221,    53,   222,
     223,     0,   224,    54,    55,    56,    57,    58,    59,    60,
      61,    62,     0,     0,     0,    63,     0,  1336,    64,  1337,
    1338,     0,  1339,     0,     0,  1340,  1341,  1342,  1343,  1344,
    1345,  1346,  1347,  1348,  1349,  1350,  -353,  -353,  1351,  1352,
    1353,  1354,  1355,  1356,  1357,   375,  1358,     0,   227,   228,
      67,   684,   230,  1359,  1360,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,  1361,    80,   234,   235,
     236,     0,   237,   238,     0,     0,     0,     0,   546,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -3,  1362,     0,     0,    85,  1363,     0,     0,     0,
     404,     0,   242,    87,    88,   244,   245,   246,   247,     0,
       0,     0,     0,     0,     0,     0,     0,  -196,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   546,     0,     0,
     681,     4,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,  1335,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   220,     0,    52,   221,    53,   222,   223,
       0,   224,    54,    55,    56,    57,    58,    59,    60,    61,
      62,     0,     0,     0,    63,     0,  1336,    64,  1337,  1338,
       0,  1339,     0,     0,  1340,  1341,  1342,  1343,  1344,  1345,
    1346,  1347,  1348,  1349,  1350,  -353,  -353,  1351,  1352,  1353,
    1354,  1355,  1356,  1357,     0,  1358,     0,   227,   228,    67,
     684,   230,  1359,  1360,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,  1361,    80,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1362,     0,     0,    85,  1363,     0,     0,     0,   404,
       0,   242,    87,    88,   244,   245,   246,   247,     0,     0,
       0,     0,     0,     0,     0,     0,  -196,     4,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   220,
       0,    52,   221,    53,   222,   223,     0,   224,    54,    55,
      56,    57,    58,    59,    60,    61,    62,     0,     0,     0,
      63,     0,     0,    64,   226,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   227,   228,    67,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,    80,   234,   235,   236,     0,   237,   238,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1735,  1736,  1737,  1738,     0,     0,     0,   239,  1739,  1740,
      85,  1363,     0,     0,     0,     0,     0,   242,    87,    88,
     244,   245,   246,   247,     0,     0,     0,     0,     0,     0,
       0,     0,  1741,     4,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   220,     0,    52,   221,    53,
     222,   223,     0,   224,    54,    55,    56,    57,    58,    59,
      60,    61,    62,     0,     0,     0,    63,     0,     0,    64,
     226,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   227,
     228,    67,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,    80,   234,
     235,   236,     0,   237,   238,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1735,  1736,  1737,  1738,
       0,     0,     0,   239,  1739,     0,    85,  1363,     0,     0,
       0,     0,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,     0,     0,  1741,     4,
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
       0,     0,     0,    66,     0,     0,     0,    67,    68,     0,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,    79,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    83,
      84,     0,    85,    86,     0,     0,     0,     0,     0,     0,
      87,    88,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    89,     0,    90,   362,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -502,  -502,     0,  -502,    52,
       0,    53,     0,     0,  -502,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   159,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,     0,    85,   363,
       0,     0,     0,  -838,     0,     0,    87,    88,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    89,   362,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,  -502,
    -502,     0,  -502,    52,     0,    53,     0,     0,  -502,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   159,     0,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    83,
      84,     0,    85,   363,     0,     0,     0,     0,     0,     0,
      87,    88,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    89,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   225,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   159,     0,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,   784,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   969,    84,  -703,    85,   627,     0,     0,     0,
       0,     0,     0,    87,    88,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    89,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -502,  -502,     0,  -502,    52,
       0,    53,     0,     0,  -502,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   159,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,     0,    85,   363,
       0,     0,     0,  -842,     0,     0,    87,    88,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    89,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -502,  -502,
       0,  -502,    52,     0,    53,     0,     0,  -502,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   159,     0,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    83,    84,
       0,    85,   363,     0,     0,     0,     0,     0,     0,    87,
      88,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    89,     4,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   220,     0,    52,   221,    53,   222,
     223,     0,   224,    54,    55,    56,    57,    58,    59,    60,
      61,    62,     0,     0,     0,    63,     0,     0,    64,   226,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   227,   228,
      67,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,    80,   234,   235,
     236,     0,   237,   238,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   239,     0,  1733,    85,  1363,     0,     0,     0,
       0,     0,   242,    87,    88,   244,   245,   246,   247,     4,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   220,     0,    52,   221,    53,   222,   223,     0,   224,
      54,    55,    56,    57,    58,    59,    60,    61,    62,     0,
       0,     0,    63,     0,     0,    64,   226,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   227,   228,    67,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,    80,   234,   235,   236,     0,   237,
     238,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   239,
       0,     0,    85,  1363,     0,     0,     0,     0,     0,   242,
      87,    88,   244,   245,   246,   247,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   220,     0,    52,
     221,    53,   222,   223,     0,   224,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   226,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
      80,   234,   235,   236,     0,   237,   238,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   239,   547,     0,    85,   240,
     548,   549,     0,     0,     0,   242,   243,    88,   244,   245,
     246,   247,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   220,     0,    52,   221,    53,   222,   223,
       0,   224,   225,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   226,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,    80,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   239,   547,     0,    85,   240,   677,   549,     0,     0,
       0,   242,   243,    88,   244,   245,   246,   247,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   220,
       0,    52,   221,    53,   222,   223,     0,   224,   225,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   226,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,    80,   234,   235,   236,     0,   237,   238,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   239,   547,     0,
      85,   592,   895,   549,     0,     0,     0,   242,   243,    88,
     244,   245,   246,   247,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   220,     0,    52,   221,    53,
     222,   223,     0,   224,   225,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     226,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,    80,   234,
     235,   236,     0,   237,   238,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   239,   547,     0,    85,   240,   830,   549,
       0,     0,     0,   242,   243,    88,   244,   245,   246,   247,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   220,     0,    52,   221,    53,   222,   223,     0,   224,
     225,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   226,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,    80,   234,   235,   236,     0,   237,
     238,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   239,
     547,     0,    85,   592,  1005,   549,     0,     0,     0,   242,
     243,    88,   244,   245,   246,   247,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   220,     0,    52,
     221,    53,   222,   223,     0,   224,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   226,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
      80,   234,   235,   236,     0,   237,   238,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   239,   547,     0,    85,   240,
     241,   549,     0,     0,     0,   242,   243,    88,   244,   245,
     246,   247,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   220,     0,    52,   221,    53,   222,   223,
       0,   224,   225,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   226,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,    80,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   239,     0,     0,    85,   240,   241,     0,     0,     0,
       0,   242,   243,    88,   244,   245,   246,   247,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   220,
       0,    52,   221,    53,   222,   223,     0,   224,   225,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   226,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,    80,   234,   235,   236,     0,   237,   238,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   239,     0,     0,
      85,   240,   677,     0,     0,     0,     0,   242,   243,    88,
     244,   245,   246,   247,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   220,     0,    52,   221,    53,
     222,   223,     0,   224,   225,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     226,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,    80,   234,
     235,   236,     0,   237,   238,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   239,     0,     0,    85,   240,   830,     0,
       0,     0,     0,   242,   243,    88,   244,   245,   246,   247,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   220,     0,    52,   221,    53,   222,   223,     0,   224,
     225,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   226,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,    80,   234,   235,   236,     0,   237,
     238,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   239,
       0,     0,    85,   592,  1005,     0,     0,     0,     0,   242,
     243,    88,   244,   245,   246,   247,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   220,     0,    52,
     221,    53,   222,   223,     0,   224,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   226,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
      80,   234,   235,   236,     0,   237,   238,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   239,     0,     0,    85,   240,
     548,     0,     0,     0,     0,   242,   243,    88,   244,   245,
     246,   247,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   220,     0,    52,   221,    53,   222,   223,
       0,   224,   225,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   226,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,    80,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   239,     0,     0,    85,   592,   895,     0,     0,     0,
       0,   242,   243,    88,   244,   245,   246,   247,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   220,
       0,    52,   221,    53,   222,   223,     0,   224,   225,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   226,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,    80,   234,   235,   236,     0,   237,   238,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   239,     0,     0,
      85,   592,     0,     0,     0,     0,     0,   242,   800,    88,
     244,   245,   246,   247,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   220,     0,    52,   221,    53,
     222,   223,     0,   224,   225,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     226,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,    80,   234,
     235,   236,     0,   237,   238,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   239,     0,     0,    85,   917,     0,     0,
       0,     0,     0,   242,   918,    88,   244,   245,   246,   247,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   220,     0,    52,   221,    53,   222,   223,     0,   224,
     225,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   226,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,    80,   234,   235,   236,     0,   237,
     238,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   239,
       0,     0,    85,   592,     0,     0,     0,     0,     0,   242,
     243,    88,   244,   245,   246,   247,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   220,     0,    52,
     221,    53,   222,   223,     0,   224,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   226,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
      80,   234,   235,   236,     0,   237,   238,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   239,     0,     0,    85,   479,
       0,     0,     0,     0,     0,   242,    87,    88,   244,   245,
     246,   247,  2007,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,
      -2,     0,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,
      -2,     0,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,    -2,    -2,  2037,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,    -2,    -2,     0,    -2,     0,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
       0,     0,    -2,     0,     0,    -2,     0,     0,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
    1122,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
      -2,    -2,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,  -501,  -501,     0,  -501,    52,     0,    53,     0,
       0,  -501,     0,   225,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1318,     0,  1122,     0,    85,    86,     0,     0,     0,
       0,     0,     0,    87,    88,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -501,  -501,     0,  -501,    52,
       0,    53,     0,     0,  -501,     0,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1403,     0,  1122,     0,    85,    86,
       0,     0,     0,     0,     0,     0,    87,    88,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -501,  -501,
       0,  -501,    52,     0,    53,     0,     0,  -501,     0,   225,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1530,     0,  1122,
       0,    85,    86,     0,     0,     0,     0,     0,     0,    87,
      88,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -501,  -501,     0,  -501,    52,     0,    53,     0,     0,
    -501,     0,   225,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1626,     0,  1122,     0,    85,    86,     0,     0,     0,     0,
       0,     0,    87,    88,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -501,  -501,     0,  -501,    52,     0,
      53,     0,     0,  -501,     0,   225,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1798,     0,  1122,     0,    85,    86,     0,
       0,     0,     0,     0,     0,    87,    88,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,  -501,  -501,     0,
    -501,    52,     0,    53,     0,     0,  -501,     0,   225,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,    86,     0,     0,     0,     0,     0,     0,    87,    88,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   159,
       0,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      83,    84,     0,    85,    86,     0,     0,     0,  -840,     0,
       0,    87,    88,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,    89,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   159,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,     0,    85,   304,
       0,     0,     0,     0,     0,     0,    87,    88,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,    89,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   159,
       0,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      83,    84,     0,    85,    86,     0,     0,     0,     0,     0,
       0,    87,    88,     0,     0,    14,    15,    16,    17,    18,
       0,     0,    20,    89,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -502,  -502,     0,  -502,    52,
       0,    53,     0,     0,  -502,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   159,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,     0,    85,   678,
       0,     0,     0,     0,     0,     0,    87,    88,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    89,     4,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,    62,     0,
       0,     0,    63,     0,     0,    64,     0,     0,     0,     0,
    -420,  -420,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    67,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -420,     0,
       0,     0,    85,    86,     0,     0,     0,     0,     0,     0,
      87,    88,     4,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,    54,    55,    56,    57,    58,    59,    60,
      61,    62,     0,     0,     0,    63,     0,     0,    64,     0,
       0,     0,     0,  -421,  -421,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -421,     0,     0,     0,    85,    86,     0,  1537,     0,
    1538,     0,     0,    87,    88,  1539,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,  1540,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    67,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1541,     0,     0,
       0,    85,   883,     0,  1537,     0,  1538,     0,     0,    87,
      88,  1539,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,  1540,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    67,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1688,     0,     0,     0,    85,   883,     0,
    1537,     0,  1538,     0,     0,    87,    88,  1539,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,  1540,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    67,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1689,
       0,     0,     0,    85,   883,     0,  1537,     0,  1538,     0,
       0,    87,    88,  1539,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,  1540,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    67,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1690,     0,     0,     0,    85,
     883,     0,     0,     0,     0,     0,     0,    87,    88,   362,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,  -502,
    -502,     0,  -502,    52,     0,    53,     0,     0,  -502,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,     0,    85,   363,     0,    14,    15,    16,    17,    18,
      87,    88,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -502,  -502,     0,  -502,    52,
       0,    53,     0,     0,  -502,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   678,
       0,     0,     0,     0,     0,     0,    87,    88,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   225,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,   784,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   831,     0,  -703,
      85,   627,     0,     0,     0,     0,     0,     0,    87,    88,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     225,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
     784,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   851,
       0,  -703,    85,   724,     0,     0,     0,     0,     0,     0,
      87,    88,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   225,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,  1204,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -711,    85,   750,     0,     0,     0,     0,
       0,     0,    87,    88,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   225,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   452,    85,   453,     0,     0,
       0,     0,     0,     0,    87,    88,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   750,
     751,     0,     0,     0,     0,     0,    87,    88,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   225,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,  1647,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,   750,     0,     0,     0,     0,     0,     0,    87,    88,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     225,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
    1649,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,   750,     0,     0,     0,     0,     0,     0,
      87,    88,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   225,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    85,   650,     0,     0,     0,     0,
       0,     0,    87,    88,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   225,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,   750,     0,     0,
       0,     0,     0,     0,    87,    88,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   453,
       0,     0,     0,     0,     0,     0,    87,    88,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,  -502,  -502,     0,
    -502,    52,     0,    53,     0,     0,  -502,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
    1561,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   220,     0,     0,
     221,     0,   222,   223,     0,   224,     0,     0,     0,     0,
      85,   363,     0,     0,     0,     0,     0,     0,    87,    88,
    1336,     0,   226,  1338,     0,  1339,  1937,  1938,  1340,  1341,
    1342,  1343,  1344,  1345,  1346,  1347,  1348,  1349,  1350,     0,
       0,  1351,  1352,  1353,  1354,  1355,  1356,  1357,     0,  1358,
       0,   227,   228,     0,   684,   230,  1359,  1360,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,  1361,
       0,   234,   235,   236,     0,   237,   238,     0,     0,     0,
       0,     0,     0,    81,     0,     0,  1561,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1362,     0,     0,    85,   479,
       0,     0,     0,   404,     0,   242,    87,    88,   244,   245,
     246,   247,     0,   220,     0,     0,   221,     0,   222,   223,
    -196,   224,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1336,     0,   226,  1338,
       0,  1339,     0,     0,  1340,  1341,  1342,  1343,  1344,  1345,
    1346,  1347,  1348,  1349,  1350,     0,     0,  1351,  1352,  1353,
    1354,  1355,  1356,  1357,     0,  1358,     0,   227,   228,     0,
     684,   230,  1359,  1360,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,  1361,     0,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1362,     0,     0,    85,   479,     0,     0,     0,   404,
       0,   242,    87,    88,   244,   245,   246,   247,     0,     0,
       0,     0,     0,     0,     0,     0,  -196,   408,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -424,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,     0,     0,     0,     0,  -424,   408,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -425,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    85,
       0,     0,     0,     0,  -425,   408,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,     0,
       0,     0,     0,  -424,    14,    15,    16,    17,    18,    19,
     533,    20,   534,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   220,     0,    52,   221,
      53,   222,   223,     0,   224,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,   226,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   535,     0,     0,     0,  1350,     0,  -353,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,    80,
     234,   235,   236,     0,   237,   238,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1362,     0,     0,    85,   536,     0,
       0,     0,   404,     0,   242,    87,    88,   537,   538,   246,
     247,    14,    15,    16,    17,    18,    19,   533,    20,   534,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   220,     0,    52,   221,    53,   222,   223,
       0,   224,    54,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   226,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     535,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,    80,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   239,     0,     0,    85,   536,     0,     0,     0,   404,
       0,   242,    87,    88,   537,   538,   246,   247,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     220,     0,    52,   221,    53,   222,   223,     0,   224,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,   226,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,    80,   234,   235,   236,     0,   237,   238,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   239,     0,
     485,    85,   486,     0,     0,     0,     0,     0,   242,    87,
      88,   244,   245,   246,   247,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   220,     0,    52,
     221,    53,   222,   223,     0,   224,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   226,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
      80,   234,   235,   236,     0,   237,   238,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   239,     0,     0,    85,   486,
       0,     0,     0,   404,     0,   242,    87,    88,   244,   245,
     246,   247,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   220,     0,    52,   221,    53,   222,
     223,     0,   224,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   226,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   227,   228,
       0,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,    80,   234,   235,
     236,     0,   237,   238,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   239,     0,     0,    85,   536,     0,     0,     0,
     404,     0,   242,    87,    88,   244,   245,   246,   247,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   220,     0,    52,   221,    53,   222,   223,     0,   224,
     225,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   226,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,    80,   234,   235,   236,     0,   237,
     238,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   239,
       0,     0,    85,   592,     0,     0,     0,     0,     0,   242,
      87,    88,   244,   245,   246,   247,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   220,     0,
      52,   221,    53,   222,   223,     0,   224,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,   226,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,    80,   234,   235,   236,     0,   237,   238,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   239,     0,     0,    85,
     486,     0,     0,     0,     0,     0,   242,    87,    88,   244,
     245,   246,   247,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   220,     0,    52,   221,    53,
     222,   223,     0,   224,   225,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     226,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,    80,   234,
     235,   236,     0,   237,   238,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   239,     0,     0,    85,   479,     0,     0,
       0,     0,     0,   242,    87,    88,   244,   245,   246,   247,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,    14,    15,
      16,    17,    18,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   524,     0,
     525,   526,     0,     0,     0,     0,     0,     0,     0,     0,
     220,     0,     0,   221,    80,   222,   223,     0,   224,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    82,
       0,     0,     0,     0,    64,   226,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   -17,     0,     0,     0,
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,    80,   234,   235,   236,     0,   237,   238,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1735,  1736,  1737,  1738,     0,     0,     0,   239,  1952,
       0,    85,   479,     0,     0,     0,     0,     0,   242,    87,
      88,   244,   245,   246,   247,   362,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -502,  -502,     0,  -502,    52,
       0,    53,     0,     0,  -502,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,     0,     0,     0,     0,
       0,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,    85,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,   225,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   159,
       0,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,   784,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -703,    85,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,   225,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   159,     0,   553,    70,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   910,     0,     0,    85,   670,     0,
       0,     0,     0,     0,     0,    87,    88,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   159,     0,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,    86,     0,     0,     0,     0,     0,     0,    87,    88,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   159,
       0,   553,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,   554,     0,     0,     0,     0,     0,
       0,    87,    88,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   225,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    82,   784,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -703,    85,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,  1329,     0,     0,     0,
       0,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,    85,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,   225,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,    85,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   891,    85,   883,
       0,     0,     0,     0,     0,     0,    87,    88,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   910,     0,
       0,    85,   554,     0,     0,     0,     0,     0,     0,    87,
      88,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   225,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   910,     0,     0,    85,   670,     0,     0,     0,     0,
       0,     0,    87,    88,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,  1438,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,   883,     0,
       0,     0,     0,     0,     0,    87,    88,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,   415,     0,     0,     0,     0,     0,     0,    87,    88,
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
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,   304,     0,     0,     0,     0,     0,
       0,    87,    88,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   225,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,   415,     0,     0,
       0,     0,     0,     0,    87,    88,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,   225,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    85,
     670,     0,     0,     0,     0,     0,     0,    87,    88,    14,
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
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,   682,     0,     0,     0,     0,     0,     0,
      87,    88,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   225,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,   453,     0,     0,     0,
       0,     0,     0,    87,    88,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   883,
       0,     0,     0,     0,     0,     0,    87,    88,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   225,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    85,   650,     0,     0,     0,     0,     0,     0,    87,
      88,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    -502,  -502,     0,  -502,    52,     0,    53,     0,     0,  -502,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,    85,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   554,
       0,     0,     0,     0,     0,     0,    87,    88,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   225,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    85,   433,     0,     0,     0,     0,     0,     0,    87,
      88,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,    54,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    85,    86,     0,     0,     0,     0,
       0,     0,    87,    88,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,   225,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,   883,     0,
       0,     0,     0,     0,     0,    87,    88,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   225,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,   678,     0,    14,    15,    16,    17,    18,    87,    88,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,  -502,  -502,     0,  -502,    52,     0,    53,
       0,     0,  -502,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,   433,     0,    14,
      15,    16,    17,    18,    87,    88,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,  -502,
    -502,     0,  -502,    52,     0,    53,     0,     0,  -502,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,   678,     0,    14,    15,    16,    17,    18,
      87,    88,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -502,  -502,     0,  -502,    52,
       0,    53,     0,     0,  -502,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,     0,
       0,     0,     0,     0,     0,     0,    87,    88,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   220,     0,    52,   221,    53,   222,   223,
       0,   224,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   226,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,     0,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   239,     0,     0,    85,   479,  1071,     0,     0,     0,
       0,   242,   243,    88,   244,   245,   246,   247,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   220,     0,    52,   221,    53,   222,   223,
       0,   224,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   226,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,     0,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   239,     0,     0,    85,   479,     0,     0,     0,     0,
       0,   242,    87,    88,   244,   245,   246,   247,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   159,     0,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,    85,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   225,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,    85,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
      20,    85,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,  -502,  -502,     0,  -502,    52,     0,    53,
       0,     0,  -502,     0,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    64,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,    69,    70,     0,    52,     0,    53,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,     0,     0,     0,
       0,     0,   196,     0,   197,   198,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    82,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   524,     0,   525,
     526,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,    20,    82,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    -501,  -501,     0,  -501,    52,     0,    53,     0,     0,  -501,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,    20,    64,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -502,  -502,
       0,  -502,    52,     0,    53,     0,     0,  -502,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,    64,    14,    15,    16,    17,    18,
      82,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    82,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,   220,
       0,     0,   221,     0,   222,   223,     0,   224,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   226,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   227,   228,    82,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,     0,   234,   235,   236,     0,   237,   238,     0,
       0,   220,     0,     0,   221,    81,   222,   223,     0,   224,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1735,  1736,  1737,  1738,     0,     0,   226,   239,  1879,     0,
      85,   479,     0,     0,     0,     0,     0,   242,    87,    88,
     244,   245,   246,   247,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,     0,   234,   235,   236,   220,   237,
     238,   221,     0,   222,   223,     0,   224,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   226,     0,     0,     0,     0,     0,   239,
     547,     0,    85,   479,     0,   549,     0,     0,     0,   242,
      87,    88,   244,   245,   246,   247,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,     0,   234,   235,   236,   220,   237,   238,   221,     0,
     222,   223,     0,   224,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     226,     0,     0,     0,     0,     0,   239,  1197,     0,    85,
     479,     0,     0,     0,  1198,     0,   242,    87,    88,   244,
     245,   246,   247,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,     0,   234,
     235,   236,   220,   237,   238,   221,     0,   222,   223,     0,
     224,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   226,     0,     0,
       0,     0,     0,   239,  1200,     0,    85,   479,  1211,     0,
       0,     0,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,     0,   234,   235,   236,   220,
     237,   238,   221,     0,   222,   223,     0,   224,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   226,     0,     0,     0,     0,     0,
     239,     0,     0,    85,   479,     0,     0,     0,   404,     0,
     242,    87,    88,   244,   245,   246,   247,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,     0,   234,   235,   236,   220,   237,   238,   221,
       0,   222,   223,     0,   224,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   226,     0,     0,     0,     0,     0,   239,     0,     0,
      85,   479,     0,     0,   966,     0,     0,   242,    87,    88,
     244,   245,   246,   247,     0,     0,     0,     0,     0,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,     0,
     234,   235,   236,   220,   237,   238,   221,     0,   222,   223,
       0,   224,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   226,     0,
       0,     0,     0,     0,   239,     0,     0,    85,   479,   979,
       0,     0,     0,     0,   242,   243,    88,   244,   245,   246,
     247,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,     0,   234,   235,   236,
     220,   237,   238,   221,     0,   222,   223,     0,   224,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   226,     0,     0,     0,     0,
       0,   239,     0,     0,    85,   479,  1003,     0,     0,     0,
       0,   242,    87,    88,   244,   245,   246,   247,     0,     0,
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,     0,   234,   235,   236,   220,   237,   238,
     221,     0,   222,   223,     0,   224,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   226,     0,     0,     0,     0,     0,   239,  1200,
       0,    85,   479,     0,     0,     0,     0,     0,   242,    87,
      88,   244,   245,   246,   247,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
       0,   234,   235,   236,   220,   237,   238,   221,     0,   222,
     223,     0,   224,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   226,
       0,     0,     0,     0,     0,   239,     0,     0,    85,   479,
       0,     0,     0,  1571,     0,   242,    87,    88,   244,   245,
     246,   247,     0,     0,     0,     0,     0,     0,   227,   228,
       0,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,     0,   234,   235,
     236,   220,   237,   238,   221,     0,   222,   223,     0,   224,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   226,     0,     0,     0,
       0,     0,   239,  1644,     0,    85,   479,     0,     0,     0,
       0,     0,   242,    87,    88,   244,   245,   246,   247,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,     0,   234,   235,   236,   220,   237,
     238,   221,     0,   222,   223,     0,   224,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   226,     0,     0,     0,     0,     0,   239,
       0,     0,    85,   479,     0,     0,     0,  1795,     0,   242,
      87,    88,   244,   245,   246,   247,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,     0,   234,   235,   236,   220,   237,   238,   221,     0,
     222,   223,     0,   224,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     226,     0,     0,     0,     0,     0,   239,     0,  1943,    85,
     479,     0,     0,     0,     0,     0,   242,    87,    88,   244,
     245,   246,   247,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,     0,   234,
     235,   236,   220,   237,   238,   221,     0,   222,   223,     0,
     224,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   226,     0,     0,
       0,     0,     0,   239,  1948,     0,    85,   479,     0,     0,
       0,     0,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,     0,   234,   235,   236,     0,
     237,   238,     0,     0,   220,     0,     0,   221,    81,   222,
     223,     0,   224,  2024,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   226,
     239,  1958,     0,    85,   479,     0,     0,     0,     0,     0,
     242,    87,    88,   244,   245,   246,   247,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   227,   228,
       0,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,     0,   234,   235,
     236,   220,   237,   238,   221,     0,   222,   223,     0,   224,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   226,     0,     0,     0,
       0,     0,   239,     0,     0,    85,   479,     0,     0,     0,
       0,     0,   242,    87,    88,   244,   245,   246,   247,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,     0,   234,   235,   236,   220,   237,
     238,   221,     0,   222,   223,     0,   224,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   226,     0,     0,     0,     0,     0,   239,
    2031,     0,    85,   479,     0,     0,     0,     0,     0,   242,
      87,    88,   244,   245,   246,   247,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,     0,   234,   235,   236,   220,   237,   238,   221,     0,
     222,   223,     0,   224,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     226,     0,     0,     0,     0,     0,   239,  2033,     0,    85,
     479,     0,     0,     0,     0,     0,   242,    87,    88,   244,
     245,   246,   247,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,     0,   234,
     235,   236,   220,   237,   238,   221,     0,   222,   223,     0,
     224,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   226,     0,     0,
       0,     0,     0,   239,  2080,     0,    85,   479,     0,     0,
       0,     0,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,     0,   234,   235,   236,   220,
     237,   238,   221,     0,   222,   223,     0,   224,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   226,     0,     0,     0,     0,     0,
     239,  2082,     0,    85,   479,     0,     0,     0,     0,     0,
     242,    87,    88,   244,   245,   246,   247,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,     0,   234,   235,   236,   220,   237,   238,   221,
       0,   222,   223,     0,   224,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   226,     0,     0,     0,     0,     0,   239,  2084,     0,
      85,   479,     0,     0,     0,     0,     0,   242,    87,    88,
     244,   245,   246,   247,     0,     0,     0,     0,     0,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,     0,
     234,   235,   236,   220,   237,   238,   221,     0,   222,   223,
       0,   224,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   226,     0,
       0,     0,     0,     0,   239,  2089,     0,    85,   479,     0,
       0,     0,     0,     0,   242,    87,    88,   244,   245,   246,
     247,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,     0,   234,   235,   236,
     220,   237,   238,   221,     0,   222,   223,     0,   224,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   226,     0,     0,     0,     0,
       0,   239,  2091,     0,    85,   479,     0,     0,     0,     0,
       0,   242,    87,    88,   244,   245,   246,   247,     0,     0,
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,     0,   234,   235,   236,   220,   237,   238,
     221,     0,   222,   223,     0,   224,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   226,     0,     0,     0,     0,     0,   239,  2127,
       0,    85,   479,     0,     0,     0,     0,     0,   242,    87,
      88,   244,   245,   246,   247,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
       0,   234,   235,   236,   220,   237,   238,   221,     0,   222,
     223,     0,   224,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   226,
       0,     0,     0,     0,     0,   239,  2129,     0,    85,   479,
       0,     0,     0,     0,     0,   242,    87,    88,   244,   245,
     246,   247,     0,     0,     0,     0,     0,     0,   227,   228,
       0,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,     0,   234,   235,
     236,   220,   237,   238,   221,     0,   222,   223,     0,   224,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   226,     0,     0,     0,
       0,     0,   239,  2131,     0,    85,   479,     0,     0,     0,
       0,     0,   242,    87,    88,   244,   245,   246,   247,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,     0,   234,   235,   236,   220,   237,
     238,   221,     0,   222,   223,     0,   224,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   226,     0,     0,     0,     0,     0,   239,
    2150,     0,    85,   479,     0,     0,     0,     0,     0,   242,
      87,    88,   244,   245,   246,   247,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,     0,   234,   235,   236,   220,   237,   238,   221,     0,
     222,   223,     0,   224,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     226,     0,     0,     0,     0,     0,   239,  2152,     0,    85,
     479,     0,     0,     0,     0,     0,   242,    87,    88,   244,
     245,   246,   247,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,     0,   234,
     235,   236,   220,   237,   238,   221,     0,   222,   223,     0,
     224,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   226,     0,     0,
       0,     0,     0,   239,  2154,     0,    85,   479,     0,     0,
       0,     0,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,     0,   234,   235,   236,   220,
     237,   238,   221,     0,   222,   223,     0,   224,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   226,     0,     0,     0,     0,     0,
     239,     0,     0,    85,   479,     0,     0,     0,     0,     0,
     242,    87,    88,   244,   245,   246,   247,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,     0,   234,   235,   236,   220,   237,   238,   221,
       0,   222,   223,     0,   224,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   226,     0,     0,     0,     0,     0,   512,     0,     0,
      85,   479,     0,     0,     0,     0,     0,   242,    87,    88,
     244,   245,   246,   247,     0,     0,     0,     0,     0,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,     0,
     234,   235,   236,   220,   237,   238,   221,     0,   222,   223,
       0,   224,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   226,     0,
       0,     0,     0,     0,   515,     0,     0,    85,   479,     0,
       0,     0,     0,     0,   242,    87,    88,   244,   245,   246,
     247,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,     0,   234,   235,   236,
     220,   237,   238,   221,     0,   222,   223,     0,   224,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   226,     0,     0,     0,     0,
       0,   521,     0,     0,    85,   479,     0,     0,     0,     0,
       0,   242,    87,    88,   244,   245,   246,   247,     0,     0,
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,     0,   234,   235,   236,   220,   237,   238,
     221,     0,   222,   223,     0,   224,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   226,     0,     0,     0,     0,     0,   530,     0,
       0,    85,   479,     0,     0,     0,     0,     0,   242,    87,
      88,   244,   245,   246,   247,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
       0,   234,   235,   236,   220,   237,   238,   221,     0,   222,
     223,     0,   224,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   226,
       0,     0,     0,     0,     0,   239,     0,     0,    85,   479,
       0,     0,     0,     0,     0,   242,   243,    88,   244,   245,
     246,   247,     0,     0,     0,     0,     0,     0,   227,   228,
       0,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,     0,   234,   235,
     236,     0,   237,   238,     0,     0,     0,     0,     0,     0,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   239,     0,     0,    85,   479,     0,     0,     0,
       0,     0,   242,   800,    88,   244,   245,   246,   247
};

static const yytype_int16 yycheck[] =
{
       1,     1,    83,     4,     4,   189,  1134,   179,   276,   377,
       1,   193,  1380,   572,   239,   361,   575,    65,   630,     1,
     321,  1092,    83,  1140,   177,   179,   698,   540,   630,  1411,
    1412,  1148,    83,  1362,    83,  1127,   630,  1328,   938,   242,
      94,     1,   852,    85,     1,   916,   152,     1,  1829,  1829,
     296,   631,   518,     3,   300,   626,    83,   637,   651,  1746,
    1346,    62,    63,   626,    65,    65,   626,   191,    83,   626,
     335,   276,     1,  1096,    65,  1829,   626,   626,   148,   687,
      83,    82,    83,    65,   626,   662,   102,   664,    89,   968,
       1,     0,   335,    94,   194,  1937,  1119,  1316,   330,   335,
    1941,   102,   106,    94,  1323,    65,   107,   161,    65,   110,
     158,   132,  1012,   114,   114,   144,   107,     0,    96,   110,
     274,   757,   386,   114,   356,    91,  1026,   335,    79,    82,
      82,   142,   768,   335,    95,   241,    65,   401,    84,    85,
     139,   177,    79,   163,  1023,   166,    84,    85,     1,   210,
     179,   171,   153,   166,   167,   156,   335,   158,   158,   210,
     425,   210,   335,   164,  1043,   345,   335,   158,   107,     0,
     171,  1830,   107,   335,   187,   186,   158,   128,   179,   335,
     816,  1400,   425,   210,   188,   163,     1,  1874,   456,   425,
     191,   128,   143,     4,   374,   210,    65,   163,   158,    68,
      69,   158,    71,   171,   310,   385,   143,   210,   209,   210,
     171,   632,    65,   169,   194,   636,   170,   425,   209,   148,
     188,   642,  1328,   425,  1223,   171,  2068,  1319,   169,   158,
     167,   833,   188,   171,   107,   188,   188,  1116,   160,  2071,
     241,   126,   157,  1534,  1535,  1536,   425,   188,   669,   171,
      65,   456,   425,  1108,   335,   676,   425,     1,   169,   170,
       4,  2042,  2042,   425,   179,   187,  1925,  2099,   831,   425,
     169,   831,   273,   274,   335,   345,  2117,  1328,   744,   745,
     179,   831,   167,   551,   335,   126,   335,  2119,  2042,     1,
     163,   463,     4,   163,   163,   148,   297,  1320,   470,  1416,
     301,  1362,   548,   114,   374,   158,   307,  2148,   335,   463,
     364,   514,  1531,   163,  1973,   385,   307,   449,   319,   163,
     335,    65,   338,   324,   340,   107,   919,   595,   329,   330,
     331,   347,   335,   148,   335,   336,   944,   187,  1217,  1218,
     410,  1742,   474,   158,   425,   336,   551,   472,   573,  1159,
     475,  1422,   476,    65,    10,   356,    82,   399,   144,   144,
     728,  1232,    20,   364,   425,   366,   110,     1,   736,   928,
     114,   983,    98,   364,   375,   376,   425,   843,   379,   415,
     434,   983,    94,   168,  2043,   386,   899,   240,   379,   983,
     595,   971,   693,   859,   179,   163,  1275,   336,   969,   400,
     401,   336,   403,   169,   420,   421,   969,   408,   162,   969,
     425,   412,   969,   118,   158,   169,   345,    10,   419,   969,
     969,   412,   188,   422,   425,   426,   672,   969,  1534,  1535,
    1536,    65,   188,   487,   188,   426,   148,   142,   169,     1,
     152,   706,   443,   444,  2103,   374,   158,   448,   179,   161,
     655,  1780,   558,   239,   404,  1741,   385,  1093,   169,  1131,
    1746,   666,   463,   706,   163,   344,  1465,  1466,  1467,   117,
     706,   472,   351,   412,   475,   476,   630,   412,  1879,  1880,
     114,   410,  1337,  1534,  1535,  1536,   163,   426,    95,    68,
      69,   426,   345,   141,   373,   496,   118,   498,   706,   163,
     536,  1618,   188,    65,   706,   384,   162,   498,   169,   239,
     742,   664,   169,   169,   148,  1370,   748,   173,   519,   701,
     142,   374,   178,   167,   158,  1907,    82,   706,   172,   241,
     345,   188,   385,   706,   169,  1627,  1138,   706,  1599,   325,
      96,  1602,  1603,   165,   706,   810,   169,   548,   110,   649,
     706,  1952,  1953,   188,   336,    79,   179,   410,   169,   374,
    1460,  1108,   169,   564,   171,   566,   171,   810,   569,   162,
     385,   677,   573,    68,   810,   576,   144,   141,    82,   169,
     173,   786,   169,   188,   830,   178,   169,   767,  1874,  1010,
    1198,   188,   171,    97,   169,   410,   158,   169,   310,   163,
     453,   188,   810,   167,   128,   188,   174,   175,   810,   188,
     174,   175,   116,   188,   650,   110,   188,   659,  1986,   143,
     165,   114,   115,   409,   169,   626,   169,   122,   123,   630,
     412,   810,   169,   345,   670,   379,   179,   810,     3,   163,
      79,   810,   678,   685,   426,   706,   137,   138,   810,   171,
     362,   188,   364,   654,   810,   706,   178,   706,   659,   660,
    1788,   141,   374,  1243,  1244,  1951,   912,   839,   850,   649,
     165,   169,  1789,   385,    76,   625,   165,   627,  1964,   706,
     169,    79,    79,   163,   685,  1262,   687,   167,   157,   128,
     188,   706,   183,   184,   174,   175,   408,   767,   410,   168,
     169,   114,   115,   706,   143,   706,   686,   187,   174,   165,
     179,   345,   176,   177,   276,   181,   182,   718,   923,  1780,
     176,   177,   434,   169,   163,   675,   512,   718,   167,   515,
     128,   128,  1033,   683,   229,   521,   169,  2023,  1006,   592,
     374,   742,   188,   169,   530,   143,   143,   748,     3,   810,
     751,   385,   702,   166,   169,   188,  1656,   166,  1658,  1005,
     163,   810,   188,   163,   179,   163,   163,   815,   167,   167,
     167,   171,   177,   172,   560,   487,   410,   169,   178,   718,
     414,   276,   512,   718,   169,   515,   164,   517,   804,   895,
    1337,   521,   171,   171,   179,   810,   163,   650,   165,   178,
     530,  1006,   803,   533,   534,   535,   169,   910,   171,   810,
       1,   812,   307,     4,   815,   815,   817,   379,    79,   819,
     165,   812,   167,  1370,   815,   826,   165,   981,   163,   983,
     171,   170,    94,   815,  1436,   170,   187,  1069,   767,  1447,
     226,   156,   157,   158,   159,   107,   558,   883,   110,   165,
     255,   413,   114,   732,   170,   815,   806,   165,   815,   171,
     861,   185,   170,   165,   179,   968,    79,   128,   170,   255,
     256,   168,   169,   812,    65,  1452,  1453,   812,   141,   163,
     759,   917,   143,   165,   379,  1691,   815,   766,   170,   163,
    1696,   770,    83,  1125,   456,   165,  1264,   750,    89,   169,
     163,  1247,   163,    94,   167,  1577,   167,   135,   136,   165,
     126,   174,   175,   169,   767,   128,   107,    79,   413,   110,
    1023,  1126,   417,   114,    13,    14,    15,    16,    17,   424,
     143,   139,   140,   934,   163,    65,   718,   938,    68,    69,
    1043,    71,   163,   944,  1410,   731,   167,   209,    53,    54,
     163,    56,   767,   739,   167,   165,   165,    62,    79,   169,
    1113,   456,   815,  1571,   165,   677,   128,   158,   969,  1141,
     165,  1272,   758,   164,   924,   925,   605,   606,   607,    79,
     981,   143,   983,   769,   163,     3,   966,   165,   179,   551,
      79,   169,   165,  1552,   165,    13,    14,    15,    16,    17,
     815,   163,   163,   163,   165,   167,   167,   128,   819,   165,
     415,  1012,  1559,  1116,   141,   168,  1563,  1564,   209,   210,
     168,   169,   143,   165,  1025,  1026,  1698,   169,   128,    68,
     812,   163,  1033,  1580,   165,  1036,   163,   165,   169,   128,
     167,   169,   163,   143,   163,   307,   167,   174,   175,   454,
     241,  1688,  1689,  1690,   143,   767,   551,  1037,  1325,  1326,
    1327,    79,   557,   163,   917,   163,   126,   167,  1069,   167,
      79,   815,   168,   169,   336,   819,   115,  1113,   163,   180,
     165,   120,   167,   274,   123,   163,   125,   165,   175,   167,
     165,  1041,  1092,   655,   169,   481,   173,    13,  1114,   185,
     595,   487,   364,   815,   141,  1106,  1107,  1108,  1108,  1262,
     128,  1711,  1712,  1713,  1217,  1218,   307,   379,   168,   128,
     114,   115,   255,   144,  1125,   143,  1108,  1127,   319,   165,
     166,   536,   163,   767,   143,   163,   167,  1362,   329,   330,
     331,   174,   175,  1179,   335,   336,  1147,   144,  1108,   163,
     166,  1108,   165,   167,   163,  1523,   169,   163,   167,   165,
     655,   167,   163,   296,   169,   356,  1266,   300,  1268,    22,
     165,   666,  1275,   364,   169,  1407,   168,   169,    94,  1108,
    1181,   815,   163,   895,   168,   169,   167,  1795,   379,   684,
     229,   156,   157,   158,   159,   163,   112,  1198,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   163,   400,
     163,   165,   403,   169,   179,   169,  1095,   408,   604,   168,
     169,   412,  1223,   188,   786,   171,   165,  1411,  1412,   724,
     169,   169,   170,   107,   425,   426,   498,   799,  1237,   278,
     279,  1413,   281,   163,   283,   650,   651,   163,  1249,  1248,
     168,   169,   163,   815,   749,  1108,  1236,     3,   165,   165,
      95,    79,   169,   169,  1263,   670,   171,    13,    14,    15,
      16,    17,   463,   678,  1328,   165,   165,   772,  1277,   169,
     169,  1092,   163,   165,  1300,   165,  1266,   169,  1268,   169,
     162,   786,  1078,  1108,    13,    14,    15,    16,    17,   165,
    1453,   188,    79,    79,   799,  1091,   171,   498,  1094,   165,
     128,   165,  1098,   169,  1539,   169,  1127,   178,   165,  1319,
     165,   454,   169,  2104,   169,   143,   167,  2108,   519,   368,
    1877,  1559,   171,    79,   165,  1563,   187,  1337,   165,  1340,
     168,   169,  1343,  1344,  1345,   163,   165,  1348,  1092,   167,
    1351,   128,   128,   168,   169,  1337,   165,   548,   168,   169,
      79,   923,   165,    79,  1108,  1315,   143,   143,   165,  1370,
    1370,  1370,   165,   564,   165,   566,   165,  1337,   569,   169,
    1337,   168,   128,  1127,   163,   576,   163,   163,  1370,  1390,
     167,   167,  1393,  1394,  1394,  1396,  1108,   143,   163,  1398,
    1401,   171,  1393,  1394,  1405,   171,  1407,    18,  1337,   128,
    1370,   167,   128,  1370,   187,   548,   170,   156,   157,   158,
     159,   170,  1422,  1982,   143,   144,   169,   143,   923,   163,
     169,   612,   613,   614,   615,   626,   168,   169,  1703,   630,
     179,  1370,   168,   169,  1006,   187,  1447,   163,   165,   188,
     165,   167,    97,    98,   165,  1185,   718,  1456,  1457,  1460,
    1703,  2008,   168,   169,  1465,  1466,  1467,  1703,   659,   660,
     168,   169,   168,   169,  1108,   168,   169,   165,   883,   165,
    1534,  1535,  1536,   165,  1337,  1539,  1540,   165,  1487,  1559,
    1489,  2038,   165,  1563,   685,  1703,   687,   168,   169,   169,
     170,  1703,   165,    79,    79,   891,  1505,   187,  1319,   163,
     896,  1006,   917,   171,   919,   706,   171,  1370,   651,    84,
      85,   907,  1337,  1623,  1703,  2072,   165,   718,   169,   170,
    1703,  1224,  1225,   919,  1703,   171,  1322,  1537,   171,   672,
     171,  1703,   608,   609,  1545,   169,  1108,  1703,  1549,  1550,
    1336,   742,   128,   128,    77,  1370,   188,   748,   692,  1559,
     751,   610,   611,  1563,  1564,   616,   617,   143,   143,  1355,
    1571,  1602,  1603,  1712,  1713,  1319,  1362,  1559,   168,  1844,
    1580,  1563,  1564,  1394,   168,    85,   169,   163,   163,  1771,
     168,   167,   167,  1337,  1595,  1596,   163,    79,  1580,    79,
      18,  1844,   171,   171,  1605,   169,  1545,   188,  1844,   165,
    1545,  1422,   169,   165,  1605,   165,  1328,   165,   169,   810,
    1621,   812,  1703,   169,   815,  1337,  1370,  1627,   165,   165,
    1559,  1126,  1362,   165,  1563,  1564,  1844,   165,   169,   165,
     165,   165,  1844,  1623,   165,   684,   128,   165,   128,  1393,
    1394,  1580,   168,   168,   168,  1656,   168,  1658,  1370,   168,
     168,   143,   165,   143,   162,  1844,   165,    79,  1667,  1668,
     861,  1844,   165,   165,   165,  1844,   165,   165,  1422,   165,
     165,   163,  1844,   163,   168,   167,   819,   167,  1844,   168,
    1691,   169,   162,   165,   165,  1696,   165,   830,    22,   165,
     165,   165,  1703,  1337,  1705,   165,  1559,   165,  1113,   165,
    1563,  1564,  1894,  1714,   165,   165,   128,   165,    13,    14,
      15,    16,    17,  1907,   165,   764,  1537,  1580,   162,   171,
    1730,   143,  1733,   171,   171,    77,  1370,   165,   187,  1740,
     171,  1973,   169,   934,  1559,   171,   165,   938,  1563,  1564,
     165,   163,  1691,   944,   165,   167,  1691,  1696,   165,   165,
    1394,  1696,   165,  1545,   187,  1580,  1705,  1553,  1554,  1974,
    1705,   165,  1773,   165,   171,  1337,   169,  2042,   969,   912,
     165,   169,   169,   162,    79,   168,   919,   165,   165,   165,
     981,   162,   983,  1537,  1795,   162,    14,  1979,   163,  2042,
     163,   163,  1188,  1189,  1190,   163,  2042,   163,  1370,  1195,
    1196,   163,   163,  1308,   163,  1559,  1627,   170,  1604,  1563,
    1564,  1012,  1534,  1535,  1536,  1537,  1538,  1539,  1540,   170,
     169,  1393,   162,   128,  2042,  1026,  1580,  1937,  1892,   171,
    2042,   188,  1033,  1844,   187,  1036,  1847,  1559,   143,   144,
     165,  1563,  1564,   165,   165,  1856,  1857,   170,   130,   168,
     170,  1605,  1863,  2042,   168,   162,   165,   168,  1580,  2042,
     168,   165,  1005,  2042,   165,  1876,   168,  1877,  1069,   165,
    2042,   165,     5,  1627,   165,  1886,  2042,  1888,   165,   162,
      13,    14,    15,    16,    17,  1877,   162,  1031,  1393,   163,
    1901,   188,  1903,  1904,  1905,   163,    87,    98,  1847,  1691,
    1640,   162,  1847,   188,  1696,  1106,   163,  1971,   188,  1730,
     188,   188,  2104,  1705,   163,  1559,  2108,  2109,   188,  1563,
    1564,   188,   188,  1934,  1125,    96,   403,   165,   162,   171,
    1941,   162,   162,   165,  1945,   162,  1580,   162,  1877,  1950,
     169,  1337,   165,   165,   165,  2137,    79,  1937,   165,  1092,
     165,  2042,   168,   165,   168,   165,  2066,   165,  2068,   165,
     165,   170,  1973,   165,  1975,   169,  2158,  1763,   188,   163,
    2162,  2042,   165,   163,   163,  1729,  1730,   162,    82,  1939,
    1181,   165,  2174,  2042,  1127,  1934,   162,  1559,  1999,  1934,
     168,  1563,  1564,   168,    82,   128,  2106,  1198,  2008,   162,
    2011,   163,  1146,   163,  2015,   188,   188,  1967,  1580,   188,
     143,   165,   162,  2024,  1877,   167,  2008,  2042,   165,  2030,
     165,   165,  1223,   179,   156,   157,   158,   159,  2038,    82,
      82,  2042,  2043,  1605,    82,   188,   179,   169,   162,   188,
     188,  1546,  2043,   170,   162,   162,  2038,   179,  1249,   188,
    2040,   164,  1877,   179,     1,  1847,   188,     4,  1202,   179,
     162,  2171,  2072,   170,  2075,   165,   112,   163,  1212,  2008,
     169,   130,   165,   132,   133,   134,  2066,   164,  2068,   165,
    2072,   119,   120,   121,   122,   123,  1730,   179,   179,   168,
    1234,   188,  2103,    82,  2043,   165,   164,   170,  2043,  2038,
    1605,  2112,  2103,   165,   163,  1859,  2117,   166,   167,  2118,
     188,   162,   171,   172,   163,   162,  2106,   188,    65,   188,
     165,  1393,  1394,  1877,  2133,    13,    14,    15,    16,    17,
      18,  1640,  2143,  2072,   539,  2146,    83,  2148,   621,    86,
     618,  1357,  1934,   254,  2148,  2008,   619,    94,    13,    14,
      15,    16,    17,    18,  2103,  1877,  2167,  1729,  2103,   622,
     107,  1167,  1370,   110,  2175,   620,  1563,   114,  2068,  1885,
    2099,  1877,  2057,  2184,  1580,  2038,  1319,  1779,  2039,  2109,
    1764,  2171,  1971,  2008,  1764,  2162,  2038,  1396,    55,  1390,
     123,  1934,  1393,  1394,  1997,    13,    14,    15,    16,    17,
     370,  1540,  1794,  1418,  1390,   152,  1407,  1033,   694,  2072,
     826,   158,     0,  2038,   161,  1609,   403,   164,   165,   851,
      83,   496,   851,   851,  1729,  1526,    -1,    -1,    -1,    -1,
     177,    -1,    -1,  1877,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,   107,    -1,  1447,  2072,    -1,    -1,
      -1,  2043,    -1,    -1,  2008,   202,   733,    -1,    -1,  1460,
      -1,    79,   209,   210,  1465,  1466,  1467,    -1,    -1,    -1,
    1414,    13,    14,    15,    16,    17,    18,  1421,    -1,  1422,
      -1,    -1,    -1,    -1,  2038,    -1,  2008,  1859,    -1,    94,
      -1,    -1,   239,   240,   241,    -1,   114,   115,    -1,    -1,
      -1,   164,    72,    -1,  1448,  1877,  2102,    -1,   255,   319,
     128,  2103,    -1,    -1,    -1,    -1,  2038,    -1,  2072,    13,
      14,    15,    16,    17,    18,   143,    -1,   274,    -1,   276,
      -1,    -1,    -1,  1605,    -1,    -1,   813,    -1,  2134,    13,
      -1,    -1,    -1,    -1,  1545,    -1,    -1,    -1,   166,   296,
    2072,    -1,    -1,   300,  1859,    -1,   161,   304,    -1,    -1,
     307,    -1,    -1,   310,  2008,    -1,    -1,    -1,    -1,    -1,
    1571,    -1,    -1,    -1,   851,    -1,    -1,    -1,    -1,  1775,
      -1,    -1,    -1,   330,    -1,    -1,    -1,    -1,   335,   336,
      -1,    -1,    -1,    -1,  2038,   872,    -1,   202,    -1,   876,
      -1,    -1,    -1,    -1,  1605,    -1,   415,    -1,    -1,   356,
      -1,   274,    -1,    -1,    -1,    -1,   363,   364,    -1,    -1,
      94,    -1,    -1,    -1,   433,    -1,    -1,    -1,  2072,    -1,
      -1,    -1,   379,    -1,    -1,    -1,  2008,    -1,   112,   626,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     255,    -1,    -1,    -1,    -1,  1656,    -1,  1658,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   412,  2038,   330,   415,  1974,
     417,    -1,   335,   336,    -1,  1619,    -1,   424,   425,   426,
      -1,    -1,    -1,    -1,  1627,    -1,   433,   434,    -1,    -1,
    1691,    -1,    -1,   356,    -1,  1696,    -1,    -1,  1642,    85,
    2072,    -1,  1703,    -1,  1705,    -1,   453,   454,   455,   456,
     987,    -1,    13,    14,    15,    16,    17,   994,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   112,   536,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,   486,
     487,    -1,   489,    -1,    -1,    -1,   733,    -1,    -1,   412,
      -1,   498,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   364,
      -1,    -1,   425,   426,    -1,   512,    -1,    -1,   515,    -1,
     517,   518,    -1,    -1,   521,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,   530,    -1,  1981,   533,   534,   535,   536,
      -1,    -1,    -1,    -1,  1795,    -1,    -1,    -1,    -1,    -1,
      -1,   548,   188,    -1,   551,    -1,    -1,  1751,    -1,    -1,
     557,   558,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,   813,   128,   112,   434,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     141,   650,   143,  1844,   831,   592,  1847,  1791,   595,   454,
      -1,    -1,    -1,   662,    -1,    13,    14,    15,    16,    17,
      -1,   670,   163,    -1,   851,    -1,   167,    -1,    -1,   678,
      -1,    -1,    -1,   174,   175,   685,    -1,   687,    -1,   626,
      -1,    -1,   487,   630,    -1,   872,    -1,    -1,   188,   876,
      -1,  1168,    -1,    -1,   178,  1172,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   650,   651,    -1,    -1,   512,    -1,    -1,
      -1,    -1,    -1,   518,    -1,   662,   521,   664,    -1,   666,
      -1,    79,    -1,   670,    -1,   672,    -1,    -1,    -1,    -1,
     677,   678,   112,  1934,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,  1892,
      -1,   698,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   706,
      -1,    -1,    -1,   626,  1908,    -1,    -1,   630,  1912,    -1,
     128,   718,  1973,    -1,  1975,    -1,    -1,    -1,    -1,    -1,
      68,    -1,   969,   141,    -1,   143,     4,     5,     6,     7,
       8,     9,    10,    11,    12,   742,    -1,   744,   745,    -1,
     987,   748,    -1,   750,    -1,   163,    -1,   994,   188,   167,
      -1,  1288,    -1,    -1,    -1,  1292,   174,   175,    -1,  1296,
      -1,    -1,   110,  2024,    -1,    -1,    -1,    -1,  1971,    -1,
      -1,    -1,    -1,    -1,   122,    -1,   124,    -1,   126,    -1,
      -1,  2042,  2043,   706,    -1,    -1,   651,    -1,    -1,    13,
      14,    15,    16,    17,    -1,   718,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   810,    -1,   812,    -1,    -1,   815,    -1,
      -1,    -1,   819,    -1,   883,    -1,    -1,   165,    -1,   742,
     168,   169,    -1,   830,   831,   748,   833,    -1,    -1,    -1,
      -1,    -1,    -1,   698,    -1,    -1,   843,    -1,    -1,    -1,
      -1,   910,  2103,    -1,   851,   852,    -1,    -1,   917,    -1,
      -1,    -1,   859,    -1,    -1,    79,    -1,  1104,    -1,    -1,
      -1,    -1,    -1,    -1,   934,    -1,    -1,    -1,   938,    -1,
      -1,    -1,    -1,    -1,   944,    -1,   883,    -1,    86,   744,
     745,   229,    -1,    -1,    -1,    -1,   112,   810,   895,   812,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,   968,
      -1,    -1,    -1,   910,   128,   912,     3,    -1,    -1,    -1,
     917,    -1,   919,    -1,  2175,    -1,    -1,   141,    -1,   143,
      -1,  1168,    -1,  2184,    -1,  1172,    -1,    -1,   276,    -1,
      -1,    -1,    -1,    -1,   282,    -1,   284,  1474,    -1,   163,
      -1,  1478,  1012,   167,  1481,    -1,    -1,    -1,    -1,    -1,
     174,   175,    -1,    -1,  1023,    -1,  1026,    -1,    -1,   307,
     239,   968,   969,  1033,    -1,    -1,  1036,    -1,    -1,    -1,
      -1,  1508,    -1,    -1,  1043,  1512,   983,   112,   843,  1516,
      -1,   116,   117,   118,   119,   120,   121,   122,   123,    -1,
      -1,    -1,    -1,    -1,   859,    -1,    -1,    -1,  1005,  1006,
      -1,   346,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,   112,  1023,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,    -1,   163,   164,
      -1,   379,   240,   381,   382,    -1,  1043,    -1,    -1,    -1,
      -1,  1288,    -1,    -1,   141,  1292,   969,  1116,    -1,  1296,
      -1,   186,    -1,    -1,   919,    -1,   166,    -1,   981,    -1,
     983,    -1,  1069,    -1,    -1,   413,   163,   164,   178,   417,
      -1,    -1,    -1,   170,    -1,    -1,   424,   174,   175,    -1,
      -1,    -1,    -1,    -1,    -1,  1092,    -1,    -1,    18,   186,
      -1,  1628,    -1,    -1,    -1,    -1,   304,    -1,    -1,    -1,
      -1,  1108,    -1,    -1,    -1,    -1,  1113,    -1,   456,  1116,
     458,   459,    -1,    13,    14,    15,    16,    17,  1125,  1126,
    1127,    18,    -1,    -1,  1131,    -1,    -1,    -1,  1198,    -1,
      -1,  1138,    -1,     1,    -1,    -1,     4,    -1,    68,    69,
      70,    71,    -1,    -1,   489,    -1,  1069,    -1,  1217,  1218,
     498,    -1,  1159,  1223,    -1,   363,    -1,    -1,    -1,    -1,
      -1,    -1,   507,    -1,    -1,   510,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,  1185,    79,
      -1,    -1,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,    65,    -1,    -1,
      -1,    -1,  1125,   551,    -1,    -1,  1275,    -1,    -1,   557,
    1217,  1218,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,   433,    94,  1474,   128,    -1,
      -1,  1478,    -1,   512,  1481,    -1,   515,   167,    -1,    -1,
      -1,   141,   521,   143,    -1,   453,   114,   595,    -1,   597,
     598,   530,    -1,    -1,    -1,  1262,    -1,    -1,    -1,    -1,
      -1,  1508,    -1,   163,   164,  1512,  1131,   167,  1275,  1516,
      -1,    -1,    -1,    -1,   174,   175,    -1,    -1,   486,    -1,
      -1,   560,    -1,    -1,    -1,    -1,   186,    -1,    -1,    -1,
     158,    -1,    -1,   161,    -1,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    74,    75,   655,   656,    -1,
      -1,    -1,  1319,    -1,    -1,    -1,    -1,    -1,   666,    -1,
      -1,  1328,    -1,    -1,    -1,    -1,  1249,    -1,    -1,    -1,
    1337,    -1,    -1,    -1,   202,    -1,   684,    -1,    -1,    -1,
      -1,    -1,    -1,   112,   623,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,  1362,  1363,    -1,    -1,    -1,
      -1,    -1,    -1,  1370,    -1,    -1,   711,    -1,    -1,    -1,
     718,   239,    -1,   241,    -1,    -1,    -1,  1447,    -1,    -1,
      -1,  1628,    -1,  1452,   592,    -1,  1393,  1394,    -1,    -1,
    1460,    -1,    -1,    -1,   742,  1465,  1466,  1467,   167,   747,
    1407,   749,    -1,  1410,    -1,    -1,   274,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1422,   185,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   772,    -1,   774,   775,    -1,  1436,
      -1,    -1,   300,    -1,    -1,    -1,    -1,    -1,   786,    -1,
      -1,    -1,    -1,    -1,    -1,  1452,  1453,    -1,    -1,    -1,
      -1,   799,    -1,    -1,   662,    -1,   664,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   812,   744,   745,    -1,    -1,    -1,
      -1,   339,    -1,    -1,    -1,    -1,    -1,    -1,   346,    -1,
      -1,   112,    -1,    -1,  1407,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,   364,    -1,    -1,    -1,
      -1,  1571,    -1,    -1,    -1,    -1,   851,   852,    -1,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,   867,  2060,    -1,   870,    -1,  1534,  1535,  1536,
    1537,    -1,  1539,  1540,    -1,    -1,   167,    -1,  1545,  1546,
      -1,    79,   750,    -1,    -1,  1410,    -1,    -1,    -1,    -1,
      -1,    -1,  1559,    -1,    -1,    -1,  1563,  1564,    -1,    -1,
     428,    -1,    -1,    -1,    -1,    -1,   434,    -1,    -1,    -1,
    1577,   174,    -1,  1580,   112,   923,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,  1656,   455,  1658,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1605,    -1,
      -1,    -1,    -1,   141,   110,   143,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,   487,
    1627,   489,  1545,    -1,    -1,   163,   164,    -1,    -1,   167,
      -1,    -1,    -1,  1640,    -1,   983,   174,   175,    -1,   507,
     508,    -1,   510,   511,   512,    -1,    -1,   515,   186,   517,
     518,    -1,    -1,   521,    -1,    -1,    -1,   163,  1006,    -1,
     166,   167,   530,  1011,    -1,   533,   534,   535,   112,    -1,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     548,    -1,    -1,    -1,  1691,  1692,    -1,    -1,    -1,  1696,
      -1,  1698,    -1,    -1,    -1,    -1,  1703,    -1,  1705,    -1,
      -1,    -1,   910,    79,  1049,    -1,    -1,    -1,    -1,    -1,
      -1,  1056,  1577,    -1,    -1,  1060,    -1,    -1,    -1,  1064,
      -1,    -1,  1729,  1730,   112,  1795,   114,   171,   116,   117,
     118,   119,   120,   121,   122,   123,   112,    -1,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,   626,    -1,
     968,    -1,   630,   631,    -1,   141,    -1,   143,  1691,   637,
      -1,    -1,    -1,  1696,    -1,    -1,    -1,    -1,  1126,   647,
    1703,    -1,  1705,    -1,    -1,    -1,    -1,   163,   164,    -1,
    1138,    -1,    -1,    -1,    -1,    -1,   112,    -1,   174,   175,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,   677,
     186,  1159,    -1,  2060,  1159,  1023,    -1,    -1,    -1,    -1,
      -1,    -1,  1829,  1830,    -1,    -1,    -1,    -1,    -1,    -1,
     698,    -1,    -1,  1698,    -1,  1043,    -1,  1844,    -1,   707,
    1847,    -1,    -1,   711,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,  1859,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1877,    -1,    -1,    -1,    -1,    -1,   744,   745,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1892,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1240,  1104,    -1,    -1,    -1,
    1248,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1925,    -1,
      -1,  1844,    -1,    -1,  1847,    -1,    -1,  1934,    -1,    -1,
      -1,    -1,    -1,    -1,  1279,    -1,    -1,  1282,    -1,    -1,
      -1,  1286,    -1,   811,    -1,    -1,    -1,   815,    -1,    -1,
     112,   819,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   830,   831,  1971,   833,  1973,  1974,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   843,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   851,   852,    -1,    -1,    -1,    -1,    -1,
    1997,   859,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   867,
     868,  2008,   870,   871,    -1,   167,    -1,    -1,    -1,  1217,
    1218,  1934,    -1,    -1,    -1,   240,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1304,    -1,   895,    -1,    -1,
     255,  2038,    -1,    -1,    -1,  2042,  2043,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1393,    -1,    -1,    -1,    -1,
    1973,    -1,   277,    -1,  1262,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    -1,  2072,    -1,  1275,    -1,    79,
      -1,   296,    -1,    -1,    -1,   300,    -1,    -1,    -1,   304,
     112,  1426,    -1,  1362,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,  2103,   129,    -1,   131,
      -1,   969,   112,   971,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,   983,    -1,    -1,   128,  2042,
    2043,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   164,   143,    -1,   167,    -1,  1005,   363,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,   163,   164,  1363,   112,    -1,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,   186,  1045,    -1,    -1,
    2103,  1049,    -1,    -1,    -1,   141,    -1,   143,  1056,  1057,
     415,   164,  1060,  1061,   167,    -1,  1064,  1065,  1546,    -1,
      -1,    -1,    -1,  1071,    -1,    -1,    -1,   163,   164,    -1,
      -1,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,
      -1,    -1,    -1,    -1,  1092,    -1,    -1,    -1,   453,   454,
     186,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
    1108,    -1,    -1,    -1,  1452,  1453,    -1,    -1,    -1,  1117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1605,    -1,  1127,
      -1,   486,    -1,  1131,    -1,    -1,    -1,    -1,    -1,    -1,
    1138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1559,  1560,    -1,    -1,  1563,  1564,    -1,    -1,    -1,    -1,
    1569,  1159,    -1,    -1,  1573,    -1,    -1,    -1,    -1,  1578,
      79,  1580,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   536,    -1,    -1,    -1,    -1,    -1,  1185,    -1,    -1,
      -1,    -1,    -1,   548,    -1,    -1,    -1,   552,    -1,    -1,
      -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,   112,  1692,    -1,   128,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,   141,   129,   143,   131,    -1,   592,    -1,    -1,
      -1,    -1,  1240,  1241,    -1,  1243,  1244,  1245,    -1,    -1,
      -1,  1729,    -1,    -1,   163,   164,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,   175,    -1,   164,    -1,
      -1,   167,    -1,    -1,    -1,    -1,    -1,   186,    -1,    -1,
      -1,  1279,  1280,    -1,  1282,  1283,    -1,    -1,  1286,  1287,
      -1,    -1,    -1,    -1,     1,   650,   651,     4,    -1,    -1,
      -1,    -1,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,   670,  1725,   672,    -1,    -1,
      -1,  1319,    -1,   678,    -1,    -1,    -1,    -1,    -1,    -1,
    1328,    -1,    -1,    -1,    -1,    -1,  1745,  1746,    -1,  1337,
      -1,    -1,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,  1829,  1830,   166,    -1,    65,    -1,
      -1,    -1,    -1,    -1,  1362,    -1,    -1,  1776,    -1,  1847,
      -1,   141,  1370,    -1,    -1,    -1,    -1,    -1,    -1,    86,
      -1,  1859,    -1,    -1,    -1,    -1,    -1,    94,    -1,    -1,
      -1,    -1,    -1,   163,   164,   750,  1394,   167,    -1,    -1,
      -1,    -1,    -1,   110,   174,   175,    -1,   114,    -1,    -1,
      -1,    -1,  1410,    -1,    -1,    -1,   186,   187,    -1,    -1,
      -1,    -1,    -1,    -1,  1422,    -1,    -1,    -1,  1426,  1427,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1436,    -1,
      -1,   148,    -1,    -1,    -1,   152,    -1,  1925,   194,    -1,
    1925,   158,  1861,    -1,   161,    -1,    -1,    -1,   165,    -1,
    1869,    -1,  1871,    -1,   819,  1874,  1875,    -1,  1877,   176,
     177,    -1,   179,  1882,   220,   830,   222,    -1,  1956,  1957,
     226,   227,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   237,   238,    -1,    -1,   202,  1974,   112,  1973,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   255,
     256,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1993,    -1,
      -1,    -1,  1997,    -1,    -1,    -1,    -1,    -1,   883,    -1,
      -1,    -1,   239,   240,   241,    -1,  1534,  1535,  1536,  1537,
      -1,  1539,  1540,    -1,    -1,    -1,  1955,    -1,   255,    -1,
      -1,   166,    -1,  1962,  1963,    -1,    -1,   912,    -1,    -1,
      -1,  1559,   917,    -1,   919,  1563,  1564,  2042,  2043,   276,
     277,    -1,    -1,    -1,    -1,    -1,  1985,    -1,    -1,  1577,
      -1,    -1,  1580,    -1,    -1,    -1,    -1,    -1,    -1,   296,
      -1,    -1,    -1,   300,    -1,    -1,    -1,   304,    -1,    -1,
     307,    -1,    -1,   310,    -1,    -1,    -1,  2016,    -1,  2018,
      -1,    -1,  2021,  2022,    -1,    -1,    -1,    -1,    -1,  2028,
    2029,    -1,    -1,    -1,    -1,  2103,    -1,    -1,  2103,  1627,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   345,    -1,
      -1,    -1,  1640,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1005,    -1,    -1,    -1,  1340,   362,   363,   364,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1351,    -1,   374,    -1,    -1,
      -1,    -1,   379,    -1,    -1,    -1,    -1,    -1,   385,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2095,  2096,  2097,    -1,
      -1,    -1,    -1,   400,  1692,    -1,   403,    -1,    -1,    -1,
    1698,   408,    -1,   410,    -1,    -1,   413,   414,   415,    -1,
     417,    -1,  1710,    -1,  2123,  2124,  2125,   424,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   433,   434,    -1,    -1,
      -1,    -1,  1730,    -1,    -1,   481,    -1,  1092,    -1,    -1,
      -1,   487,    -1,    -1,    -1,    -1,   453,   454,   112,   456,
      -1,    -1,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,    -1,   129,    -1,   131,    -1,    -1,
      -1,    -1,  1127,    -1,    -1,    -1,    -1,    -1,   112,   486,
     487,    -1,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,    -1,   129,    -1,   131,    -1,    -1,
     164,    -1,    -1,   167,    -1,   512,    -1,    -1,   515,    -1,
     517,   518,   519,    -1,   521,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   530,    -1,    -1,   533,   534,   535,   536,
     164,  1829,  1830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   548,    -1,    -1,   551,   552,    -1,  1845,    -1,    -1,
     557,   558,    -1,    -1,    -1,    -1,    -1,    -1,   604,   605,
     606,   607,   608,   609,   610,   611,   612,   613,   614,   615,
     616,   617,   618,   619,   620,   621,   622,    -1,    -1,  1877,
      -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,   595,    -1,
      -1,    -1,    -1,    -1,  1892,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   649,    -1,    -1,    -1,    -1,    -1,  1595,
    1596,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   626,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1925,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1933,    -1,    -1,    -1,    79,
      -1,    -1,    -1,   650,   651,    -1,    -1,    -1,   655,    -1,
      -1,    -1,    -1,   660,    -1,   662,    -1,   664,    -1,   666,
      -1,    -1,    -1,   670,  1319,   672,    -1,    -1,    -1,    -1,
     677,   678,   112,  1971,    -1,  1973,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,    -1,    -1,   128,    -1,
      -1,   698,    -1,    -1,    -1,  1993,  1994,    -1,    -1,  1997,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,  1363,    -1,
    2008,    -1,    -1,    -1,    -1,    -1,    -1,   724,    -1,    -1,
      -1,    -1,    -1,   163,   164,    -1,   733,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   174,   175,    -1,   744,   745,    -1,
    2038,    -1,   749,   750,  2042,  2043,   186,  1733,    -1,    -1,
      -1,    -1,    -1,    -1,  1740,    -1,    -1,    -1,    -1,    -1,
     767,    -1,    -1,    -1,    -1,    -1,    -1,  1422,    -1,    -1,
      -1,    -1,    -1,    -1,  2072,    -1,    -1,    -1,    -1,   786,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1773,    -1,    -1,
      -1,   112,   799,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,    -1,  2103,   813,    -1,   815,    -1,
      -1,   112,   819,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   830,   831,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,   843,    -1,    -1,    -1,
      -1,    -1,   163,    -1,   851,   891,    -1,    -1,    -1,    -1,
     896,    -1,   859,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   907,   163,    -1,    -1,   872,    -1,    -1,    -1,   876,
    1856,  1857,    -1,   919,    -1,    -1,   883,  1863,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   895,    -1,
    1876,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
    1886,    -1,  1888,   910,    -1,   912,    -1,    -1,    -1,    -1,
     917,    -1,   919,    -1,    -1,  1901,   923,  1903,  1904,  1905,
     966,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,  1941,   141,    -1,   143,  1945,
      -1,   968,   969,    -1,  1950,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1627,    -1,    -1,    -1,    -1,    -1,   163,   164,
     987,    -1,    -1,    -1,    62,    63,    -1,   994,    -1,   174,
     175,  1037,    -1,    -1,    -1,    -1,    -1,    -1,  1005,  1006,
      -1,   186,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1999,    -1,    79,  1023,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,  2011,    -1,    -1,    -1,  2015,
      -1,    -1,    -1,    -1,    -1,    -1,  1043,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2030,    -1,    -1,    -1,   112,    -1,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,    -1,   112,   128,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   153,    -1,   141,   156,   143,
      -1,    -1,    -1,    -1,    -1,  1092,    -1,    -1,    -1,  2075,
      -1,    -1,    -1,   171,    -1,    -1,    -1,  1104,    -1,   163,
     164,  1108,    -1,    -1,    -1,    -1,  1113,    -1,    -1,  1116,
     174,   175,    -1,   191,    -1,    -1,    -1,    -1,    -1,  1126,
    1127,    -1,   186,    -1,  1131,    -1,  2112,    -1,    -1,    -1,
      -1,  2117,    -1,    -1,    -1,    -1,  1143,    -1,    -1,    -1,
      -1,    -1,  1188,  1189,  1190,    -1,    -1,    -1,    -1,  1195,
    1196,    -1,    -1,    -1,    -1,    -1,    -1,  2143,    -1,    -1,
    2146,  1168,  2148,    79,    -1,  1172,    -1,    -1,    -1,    -1,
      -1,    -1,  1179,    -1,    -1,    -1,    -1,    -1,  1185,    -1,
      -1,  2167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1236,    -1,    -1,    -1,    -1,   273,   112,    -1,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,    -1,
    1217,  1218,   128,    -1,    -1,    -1,    -1,    -1,    -1,   297,
    1266,    -1,  1268,   301,    -1,   141,    -1,   143,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,  1892,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   324,   163,   164,    -1,
      -1,    -1,    -1,    -1,    -1,  1262,    -1,    -1,   174,   175,
      -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,  1275,   112,
     186,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,  1288,    -1,     1,    -1,  1292,     4,    -1,   366,  1296,
      -1,  1337,    -1,    -1,    -1,    79,    -1,   375,   376,    -1,
      -1,  1308,    -1,    -1,    -1,    -1,    -1,    -1,   386,    -1,
      -1,    -1,  1319,    -1,    -1,    -1,  1971,    -1,    -1,    -1,
      -1,  1328,    -1,   401,    -1,    -1,    -1,    -1,   112,    -1,
    1337,    -1,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,   419,    -1,    -1,   128,    -1,    -1,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1362,  1363,   141,    -1,   143,
      -1,    -1,    -1,  1370,    -1,   443,   444,    -1,    86,    -1,
     448,    -1,    -1,    -1,    -1,    -1,    94,    -1,    -1,   163,
     164,    -1,    -1,    -1,    -1,    -1,  1393,  1394,    -1,    -1,
     174,   175,    -1,    -1,   472,    -1,   114,   475,   476,    -1,
      -1,    -1,   186,  1410,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1422,    -1,    -1,   496,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     148,    -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,    -1,
     158,    -1,    -1,    -1,    -1,  1452,  1453,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,    -1,   177,
      -1,    -1,   160,    -1,    -1,    -1,    -1,  1474,    -1,    -1,
      -1,  1478,    -1,   112,  1481,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,   187,
      -1,   209,    -1,    -1,    -1,   573,    -1,    -1,    -1,    -1,
      -1,  1508,   141,    -1,    -1,  1512,    -1,    -1,    -1,  1516,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   240,   241,   163,   164,    -1,  1534,  1535,  1536,
    1537,  1538,  1539,  1540,    -1,   174,   175,   255,    -1,  1546,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   186,    -1,    -1,
      -1,    -1,  1559,    -1,    -1,    -1,  1563,  1564,    -1,   277,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
    1577,    -1,    -1,  1580,    -1,    -1,   654,  1623,   296,    -1,
      -1,    -1,   300,    -1,    -1,    -1,   304,    -1,    -1,    -1,
      -1,    -1,   310,    -1,    -1,    -1,    -1,    -1,  1605,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,  1620,    -1,   128,    -1,    -1,   336,    -1,
    1627,  1628,    -1,    -1,    -1,    -1,    -1,   345,   141,    -1,
     143,    -1,    -1,  1640,    -1,    -1,    -1,    -1,   356,    -1,
      -1,    -1,    -1,   361,   362,   363,   364,    -1,    -1,    -1,
     163,   164,    -1,    -1,    -1,    -1,   374,    -1,    -1,    -1,
      -1,   174,   175,    -1,    -1,    -1,    -1,   385,    -1,    -1,
     388,    -1,    -1,   186,   392,    -1,    -1,    -1,    -1,   397,
      -1,    -1,    -1,    -1,    -1,   403,    -1,    -1,    -1,     1,
     408,  1698,   410,    -1,    -1,    -1,   414,   415,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   426,    -1,
      -1,    -1,    -1,    -1,    -1,   433,    -1,    -1,    -1,    -1,
      -1,    -1,  1729,  1730,    -1,   803,    -1,    -1,    -1,  1775,
      -1,    -1,    -1,    -1,    -1,   453,   454,    -1,    -1,   817,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   826,    -1,
      -1,    -1,   112,    65,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,    -1,    -1,   486,    -1,
      -1,    -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,
     112,   141,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   114,   163,   164,    -1,    -1,    -1,    -1,   141,
     170,    -1,    -1,    -1,   174,   175,    -1,    -1,   536,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   186,    -1,    -1,    -1,
     548,   163,   164,    -1,   552,   167,   148,    -1,    -1,    -1,
     558,    -1,   174,   175,    -1,    -1,   158,    -1,    -1,    -1,
      -1,    -1,  1859,   112,   186,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   177,    -1,  1874,    -1,    -1,
    1877,    -1,    -1,    -1,   592,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,  1892,    -1,    -1,    -1,    -1,
      -1,  1937,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    -1,    -1,   626,    -1,
      -1,    -1,   630,    -1,    -1,   174,   175,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   186,   240,    -1,
      -1,    -1,   650,   651,    -1,  1981,    -1,    -1,    -1,   657,
      -1,    -1,    -1,   255,   662,    -1,   664,  1025,  1955,  1956,
      -1,    -1,   670,    -1,   672,    -1,    -1,    -1,    -1,   677,
     678,    -1,    -1,    -1,  1971,   277,    -1,  1974,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   296,    -1,    -1,    -1,   300,    -1,
      -1,    -1,   304,    -1,  2040,    -1,    -1,    -1,    -1,    -1,
      -1,  2008,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   733,    -1,  2024,    -1,    -1,
    2066,    -1,  2068,    -1,    -1,    -1,    -1,    -1,    -1,  1107,
      -1,  2038,   750,   345,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   767,
      -1,   363,    -1,  2060,    -1,    -1,    -1,    -1,    -1,    -1,
    2106,    -1,   374,    -1,    -1,  2072,    -1,    -1,    -1,  1147,
      -1,    -1,    -1,   385,    -1,    -1,   388,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   403,    -1,    -1,    -1,   813,    -1,   815,   410,    -1,
      -1,   819,   414,   415,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   830,   831,    -1,   833,    -1,    -1,    -1,    -1,
      -1,   433,    -1,    -1,    -1,  2171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   851,   852,    -1,    -1,    -1,    -1,    -1,
      -1,   453,   454,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   872,    -1,    -1,    -1,   876,    -1,
      -1,    -1,    -1,    -1,    -1,   883,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   486,    -1,    -1,   895,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   910,    -1,   912,    -1,    -1,    -1,    -1,   917,
      -1,   919,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   536,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   548,    -1,    -1,    -1,
     552,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     968,   969,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   980,    -1,    -1,  1343,  1344,  1345,    -1,   987,
    1348,    -1,    -1,    -1,    -1,    -1,   994,    -1,    -1,    -1,
     592,    -1,    -1,    -1,    -1,    -1,    -1,  1005,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1023,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   626,    -1,    -1,    -1,  1396,    -1,
      -1,    -1,    -1,  1401,    -1,  1043,    -1,  1405,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   650,   651,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     662,    -1,   664,    -1,    -1,    -1,    -1,    -1,   670,    -1,
     672,    -1,    -1,    -1,    -1,    -1,   678,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1092,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1104,    -1,    -1,    -1,
    1108,    -1,    -1,    -1,    -1,  1113,    -1,    -1,  1116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1127,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   733,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   750,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1168,    -1,    -1,    -1,  1172,   767,    -1,    -1,    -1,    -1,
      -1,  1179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1549,  1550,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1217,
    1218,   813,    -1,   815,    -1,    -1,    -1,   819,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,   831,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1247,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   851,
      -1,    -1,    -1,  1621,  1262,     1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1275,    -1,    -1,
     872,    -1,    -1,    -1,   876,    -1,    -1,    -1,    -1,    -1,
    1288,   883,    -1,    -1,  1292,    -1,    -1,    -1,  1296,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   910,    -1,
     912,  1319,    -1,    -1,    -1,   917,    -1,   919,    -1,    65,
    1328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1337,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      86,    -1,    -1,    -1,    -1,    -1,  1714,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1363,    -1,    -1,    -1,    -1,
      -1,    -1,  1370,    -1,    -1,    -1,   968,   969,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   987,  1394,    -1,    -1,    -1,
      -1,    -1,   994,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   148,  1005,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,    -1,  1422,    -1,    -1,    -1,    -1,    -1,
      -1,  1023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1043,    -1,    -1,  1452,  1453,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1474,    -1,    -1,    -1,
    1478,    -1,    -1,  1481,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1092,    -1,    -1,    -1,   240,    -1,    -1,    -1,    -1,    -1,
    1508,    -1,  1104,    -1,  1512,    -1,  1108,    -1,  1516,   255,
      -1,  1113,    -1,    -1,  1116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1127,  1534,  1535,  1536,  1537,
    1538,   277,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     296,  1559,    -1,    -1,   300,  1563,  1564,    -1,   304,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1168,    -1,    -1,    -1,
    1172,    -1,  1580,    -1,    -1,    -1,    -1,  1179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   345,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1217,  1218,   363,    -1,  1627,
    1628,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   374,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   385,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   403,    -1,    -1,
    1262,    -1,    -1,    -1,   410,    -1,    -1,    -1,   414,   415,
      -1,    -1,    -1,  1275,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1288,   433,    -1,    -1,
    1292,    -1,    -1,    -1,  1296,    -1,    -1,  1705,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   453,   454,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1319,    -1,    -1,
      -1,    -1,  1730,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1337,    -1,    -1,    -1,    -1,
     486,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1363,    -1,    -1,    -1,    -1,    -1,    -1,  1370,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     536,    -1,  1394,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   548,    -1,    -1,    -1,   552,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1422,    -1,  1830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,    -1,
    1452,  1453,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1874,    -1,    -1,  1877,
      -1,     1,  1474,    -1,     4,    -1,  1478,    -1,    -1,  1481,
     626,    -1,    -1,    -1,  1892,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   650,   651,  1508,    -1,    -1,    -1,
    1512,    -1,    -1,    -1,  1516,    -1,   662,    -1,   664,    -1,
      -1,    -1,    -1,    -1,   670,    -1,   672,    -1,    -1,    -1,
       1,    -1,   678,     4,    -1,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1559,    -1,    -1,
      -1,  1563,  1564,  1971,    94,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1580,    -1,
      -1,    -1,    -1,    -1,   114,    -1,    -1,   733,    -1,    -1,
      -1,    -1,    -1,   123,    65,    -1,    -1,    -1,    -1,    -1,
    2008,    -1,    -1,    -1,   750,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,    -1,
      -1,   767,   152,    94,    -1,  1627,  1628,    -1,   158,    -1,
    2038,   161,    -1,    -1,    -1,  2043,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2060,    -1,    -1,    -1,     1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,  2072,    -1,    -1,   813,    -1,   815,
      -1,    -1,   202,   819,    -1,    -1,    -1,   148,    -1,    -1,
      -1,   152,    -1,    -1,   830,   831,    -1,   158,    -1,    -1,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   851,    -1,    -1,    -1,   239,
      -1,   241,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      65,    -1,    -1,    -1,    -1,    -1,   872,    -1,  1730,    -1,
     876,   202,    -1,    -1,    -1,    -1,    -1,   883,    -1,    -1,
      -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   910,   110,   912,    -1,   239,   114,
     241,   917,    -1,   919,    -1,    -1,    -1,    -1,    -1,    -1,
     310,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,    -1,   345,    -1,    -1,    -1,    -1,
     165,    -1,   968,   969,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   362,    -1,   364,    -1,    -1,    -1,    -1,   310,
     370,   987,    -1,    -1,   374,    -1,    -1,    -1,   994,    -1,
      -1,    -1,    -1,    -1,    -1,   385,    -1,    -1,    -1,  1005,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   345,  1877,    -1,  1023,   408,    -1,
     410,    -1,    -1,    -1,   414,    -1,    -1,    -1,    -1,    -1,
    1892,   362,    -1,   364,    -1,   240,    -1,  1043,    -1,   370,
      -1,    -1,    -1,   374,   434,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   385,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   408,    -1,   410,
      -1,    -1,    -1,   414,    -1,    -1,  1092,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   487,  1104,   304,
      -1,    -1,  1108,   434,    -1,    -1,    -1,  1113,    -1,  1971,
    1116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1127,   512,    -1,    -1,   515,    -1,   517,   518,    -1,
      -1,   521,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     530,    -1,    -1,   533,   534,   535,  2008,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,   487,    -1,   363,    -1,
      -1,    -1,  1168,    -1,    -1,    -1,  1172,    -1,   558,    -1,
      -1,    -1,    -1,  1179,   379,    -1,  2038,    -1,    -1,    -1,
      -1,   512,    -1,    -1,   515,    -1,   517,   518,    -1,    -1,
     521,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2060,   530,
      -1,    -1,   533,   534,   535,    -1,    -1,    -1,    -1,    -1,
    2072,  1217,  1218,    -1,    -1,    -1,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   558,   433,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    94,    -1,    -1,   453,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1262,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   114,    -1,    -1,    -1,  1275,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   486,  1288,    -1,    -1,    -1,  1292,   677,    -1,    -1,
    1296,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
      -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,   698,   158,
      -1,    -1,   161,  1319,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1337,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   677,    -1,    -1,    -1,
      -1,    -1,    -1,   202,   744,   745,    -1,  1363,    -1,    -1,
      -1,    -1,    -1,    -1,  1370,    -1,    -1,   698,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   767,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,  1394,    -1,
     239,    -1,   241,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   744,   745,    -1,  1422,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   815,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   767,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1452,  1453,    -1,    -1,
      -1,    -1,    -1,   843,    -1,    -1,    -1,   662,    -1,   664,
      -1,   310,    -1,    -1,    -1,    -1,    -1,    -1,  1474,   859,
      -1,    -1,  1478,    -1,    -1,  1481,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   815,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   345,    -1,    -1,    -1,
      -1,    -1,  1508,    -1,    -1,   895,  1512,    -1,    -1,    -1,
    1516,    -1,   843,   362,    -1,   364,    -1,    -1,    -1,    -1,
      -1,   370,    -1,    -1,    -1,   374,    -1,    -1,   859,    55,
      -1,    -1,    58,    -1,    60,    61,   385,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   750,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1559,    80,    -1,    -1,  1563,  1564,   408,
      -1,   410,    -1,    -1,   895,   414,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1580,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,   434,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,    -1,   133,   134,    -1,
     815,    -1,    -1,    -1,   819,   141,    -1,    -1,    -1,    -1,
      -1,  1627,  1628,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,   163,   487,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
     186,    -1,    -1,   512,    -1,    -1,   515,    -1,   517,   518,
      -1,    -1,   521,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   530,    -1,    -1,   533,   534,   535,    -1,    -1,    -1,
      55,    -1,    -1,    58,    -1,    60,    61,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    -1,   910,    -1,    -1,    -1,   558,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,  1108,    -1,
      -1,    -1,    -1,    -1,  1730,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1131,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,   968,   129,   130,   131,    -1,   133,   134,
      -1,    -1,    -1,    -1,    -1,    -1,   141,  1108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1185,    -1,    -1,   163,    -1,
    1131,   166,   167,    -1,    -1,    -1,    -1,    54,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,  1023,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   677,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,  1043,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   698,
      -1,    -1,    -1,    -1,  1185,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1874,    -1,
      -1,  1877,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,  1892,  1092,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   744,   745,    -1,   145,    -1,
     147,    -1,    -1,  1108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1116,    -1,    -1,    -1,    -1,    -1,    -1,   767,    -1,
      -1,  1126,  1127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   178,    -1,   180,    -1,    -1,    -1,    -1,  1328,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1337,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   210,    -1,  1971,   815,    -1,    -1,    -1,
      -1,    -1,  1362,    -1,    -1,    -1,    -1,    -1,   225,    -1,
    1370,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   843,    -1,    -1,  1328,    -1,    -1,
      -1,    -1,  2008,    -1,  1394,    -1,  1337,    -1,    -1,    -1,
     859,    -1,  1217,  1218,    -1,    -1,    -1,    -1,    -1,    -1,
    1410,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1362,  2038,    -1,    -1,    -1,    -1,    -1,    -1,  1370,
      -1,   288,    -1,    -1,    -1,    -1,   895,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2060,    -1,    -1,  1262,    -1,    -1,
      -1,    -1,    -1,  1394,    -1,    -1,  2072,    -1,    -1,    -1,
    1275,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1410,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   335,    -1,
      -1,    -1,   339,    -1,    -1,   342,   343,    -1,    -1,   346,
      -1,    -1,   349,   350,    -1,   352,    -1,   354,    -1,    -1,
      -1,    -1,    -1,    -1,  1319,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1337,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1534,  1535,  1536,  1537,  1538,  1539,
    1540,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1363,    -1,
      -1,    -1,    54,    -1,    -1,  1370,    -1,    -1,    -1,  1559,
      -1,    -1,    -1,  1563,  1564,    -1,    -1,    -1,   425,    -1,
      -1,   428,    -1,    -1,    -1,    -1,    -1,  1577,  1393,  1394,
    1580,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1534,  1535,  1536,  1537,  1538,  1539,  1540,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1422,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1559,    -1,
      -1,    -1,  1563,  1564,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,  1577,  1452,  1453,  1580,
    1640,    -1,    -1,   145,    -1,   147,    -1,    -1,    -1,  1108,
     507,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1131,    -1,    -1,    -1,   178,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1698,  1640,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1537,   225,    -1,    -1,  1185,    -1,    -1,    -1,
    1730,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1559,    -1,    -1,    -1,  1563,  1564,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1698,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1580,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   631,    -1,    -1,   634,   635,    -1,
     637,    -1,   639,   640,    -1,    -1,   288,   644,   645,  1730,
    1605,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1627,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,
      -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,    -1,
     342,   343,    -1,    -1,   346,    -1,    -1,   349,   350,   706,
     352,    80,   354,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   722,    -1,    -1,    -1,  1328,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1877,  1337,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,  1362,   133,   134,    -1,    -1,    -1,    -1,
      -1,  1370,   141,    -1,  1729,  1730,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,  1394,  1877,   166,   167,    -1,
      -1,    -1,   171,    -1,   173,   174,   175,   176,   177,   178,
     179,  1410,    -1,   810,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   836,
      -1,   838,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   855,   856,
      -1,    -1,    -1,    -1,    -1,   507,    -1,    -1,  2008,    -1,
     867,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,  2038,    -1,
      -1,    -1,    -1,    -1,  1859,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1877,    -1,    -1,    -1,    -1,  2008,    -1,    -1,
      -1,    -1,  2072,    -1,    -1,  1534,  1535,  1536,  1537,  1538,
    1539,  1540,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2038,    -1,    -1,
    1559,    -1,    -1,    -1,  1563,  1564,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1577,    -1,
      -1,  1580,    -1,    -1,   981,    -1,    -1,    -1,    -1,   631,
      -1,  2072,   634,   635,    -1,   637,    -1,   639,   640,    -1,
      -1,    -1,   644,   645,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   202,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,
      -1,  1640,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,   240,    -1,  2008,    -1,    -1,    -1,    -1,    -1,  1056,
      -1,    -1,    -1,  1060,    -1,    -1,    -1,  1064,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     722,    -1,    -1,  2038,    -1,    -1,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,  1698,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    80,    -1,    -1,   304,    -1,  2072,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     319,  1730,   321,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1137,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
     128,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   836,   163,   838,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,   855,   856,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   867,   415,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   423,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1240,    -1,    -1,  1243,  1244,    -1,    -1,
      -1,    -1,    -1,    -1,  1251,  1252,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   453,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1274,  1877,  1276,
      -1,    -1,  1279,    -1,    -1,  1282,    -1,    -1,    -1,  1286,
     479,    -1,    -1,    -1,    -1,    -1,    -1,   486,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   497,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   523,    -1,    -1,    -1,    -1,   981,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   536,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   551,   552,    -1,   554,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   563,    -1,   565,    -1,    -1,    -1,
      -1,    -1,    -1,   572,    -1,   574,   575,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     589,    -1,    -1,   592,    -1,    -1,    -1,    -1,    -1,  2008,
      -1,    -1,    -1,    -1,  1056,    -1,    -1,    -1,  1060,    -1,
      -1,    -1,  1064,    -1,    -1,    -1,    -1,    -1,    -1,  1426,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   627,  2038,
     629,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   650,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2072,    -1,    -1,  1473,    -1,    -1,    -1,
      -1,   670,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   682,    -1,  1137,    -1,   686,   687,    -1,
      -1,    -1,    -1,    -1,   693,    -1,    -1,    -1,    -1,   698,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      94,    -1,    -1,    -1,    -1,   724,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   750,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,    -1,  1589,    -1,    -1,    -1,    -1,   161,  1240,    -1,
      -1,  1243,  1244,    -1,    -1,    -1,    -1,    -1,    -1,  1251,
    1252,    -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1274,    -1,  1276,    -1,    -1,  1279,   202,    -1,
    1282,    -1,    -1,    -1,  1286,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     859,    -1,    -1,    -1,    -1,   239,   240,    -1,  1675,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   255,    -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1703,    -1,    -1,    -1,
      -1,    -1,   276,  1710,   903,    -1,    -1,    -1,    -1,   908,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   916,   917,    -1,
      -1,    -1,   296,    -1,   923,    -1,   300,    -1,    -1,   928,
     304,    -1,    -1,   307,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   944,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1426,    -1,    -1,    -1,    -1,  1786,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   363,
     364,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1806,
    1807,    -1,    -1,    -1,    -1,   379,    -1,  1006,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1473,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1836,
    1837,    -1,    -1,    -1,  1033,    -1,    -1,  1844,    -1,    -1,
      -1,   415,  1849,   417,    -1,  1044,    -1,    -1,    -1,    -1,
     424,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   433,
     434,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   453,
     454,    -1,   456,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   486,   487,    -1,    -1,    -1,    -1,    -1,    -1,
    1927,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1131,    -1,    -1,    -1,    -1,  1589,   512,    -1,
      -1,   515,    -1,   517,   518,    -1,    -1,   521,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   530,    -1,    -1,   533,
     534,   535,   536,    -1,    -1,    -1,    -1,    -1,  1167,    -1,
      -1,    -1,    -1,    -1,   548,    -1,    -1,   551,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1993,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1198,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,    -1,
      -1,   595,    -1,  1675,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1232,    -1,  2042,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1248,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1272,    -1,    -1,   650,   651,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   662,    -1,
     664,    -1,    -1,    -1,    -1,    -1,   670,    -1,   672,    -1,
      -1,    -1,    -1,    -1,   678,    -1,    -1,  1306,    -1,  1308,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   698,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1786,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1806,  1807,    -1,  1356,  1357,    -1,
      -1,    -1,    -1,    -1,  1363,    -1,    -1,    -1,    -1,    -1,
     744,   745,    -1,    -1,    -1,    -1,   750,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1836,  1837,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,  1849,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,  1430,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,  1447,    -1,
    1449,    -1,    -1,    -1,    -1,    79,   830,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   843,
      -1,    -1,    -1,    -1,    -1,  1927,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   859,  1485,  1486,    -1,    -1,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,   883,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   910,    -1,   912,    -1,
      -1,  1993,   166,   917,    -1,   919,    -1,    -1,    -1,    -1,
     174,   175,    -1,  1552,    -1,    -1,    -1,    -1,    -1,    55,
      -1,    -1,    58,    -1,    60,    61,    -1,    63,    -1,    -1,
      -1,    -1,  1571,    -1,    -1,    -1,    -1,    -1,  1577,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   968,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,  1005,  1006,   129,   130,   131,    -1,   133,   134,  1638,
      -1,    -1,  1641,    -1,    -1,   141,    -1,    -1,    -1,  1023,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1661,    -1,    -1,    -1,    -1,   163,   164,  1043,
     166,   167,    -1,    -1,    -1,   171,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,  1684,    -1,    -1,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,  1698,
      -1,    -1,    18,    -1,    20,  1704,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,  1113,
      56,    57,  1116,    59,    -1,    -1,    62,    -1,    -1,    65,
      66,    67,    68,    69,    70,    71,    -1,  1131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1768,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1795,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1185,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,  1217,  1218,    -1,    -1,    -1,    -1,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   188,    -1,    55,    -1,    -1,    58,    -1,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,  1262,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    80,
      81,  1275,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,    -1,   107,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,    -1,   133,   134,  1328,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   162,   163,  1982,    -1,   166,   167,    -1,  1362,  1363,
     171,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   188,     1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1393,
      -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1410,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,    -1,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1452,  1453,
      -1,    -1,    -1,    -1,    -1,    78,    -1,    80,    81,    -1,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,    -1,   107,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,    -1,
     133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1534,  1535,  1536,    -1,    -1,  1539,  1540,    -1,    -1,   162,
     163,    -1,  1546,   166,   167,    -1,    -1,    -1,   171,    -1,
     173,   174,   175,   176,   177,   178,   179,    -1,     1,    -1,
      -1,    -1,    -1,    -1,    -1,   188,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1577,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1605,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,    -1,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    78,  1640,    80,    81,    -1,
      83,    -1,    -1,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,    -1,   107,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,    -1,
     133,   134,    -1,    -1,  1698,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,    -1,   166,   167,  1729,    -1,    -1,   171,    -1,
     173,   174,   175,   176,   177,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   188,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    -1,    -1,    -1,    76,    -1,    78,    79,    80,
      81,    -1,    83,    -1,    -1,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,  1859,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,  1892,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   162,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,
     171,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   188,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1971,    -1,    -1,
    1974,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    -1,    -1,    -1,    76,    -1,    78,    79,    80,    81,
      -1,    83,    -1,    -1,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,    -1,   107,    -1,   109,   110,   111,
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
      -1,    -1,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,   128,   129,   130,   131,    -1,   133,   134,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,   128,   129,
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
      -1,    -1,    -1,   107,    -1,    -1,    -1,   111,   112,    -1,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
     174,   175,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   186,    -1,   188,     3,     4,     5,     6,     7,
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
      -1,    -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
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
      -1,   112,    -1,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,    -1,    -1,    -1,   128,    -1,    -1,
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
      -1,    -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,   128,   129,   130,
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
      -1,    -1,    -1,    -1,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,   128,   129,   130,   131,    -1,   133,
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
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
     128,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,   128,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,   166,   167,   168,   169,    -1,    -1,
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
      -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,   128,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,   128,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    -1,   166,   167,   168,   169,
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
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,   128,   129,   130,   131,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,   166,   167,   168,   169,    -1,    -1,    -1,   173,
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
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
     128,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,   128,   129,   130,   131,
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
      -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,   128,   129,   130,   131,    -1,   133,   134,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,   128,   129,
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
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,   128,   129,   130,   131,    -1,   133,
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
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
     128,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,   128,   129,   130,   131,
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
      -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,   128,   129,   130,   131,    -1,   133,   134,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,   128,   129,
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
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,   128,   129,   130,   131,    -1,   133,
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
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
     128,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   111,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   162,    -1,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,   174,   175,     1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    -1,
      -1,    -1,    76,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     162,    -1,     1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,   174,   175,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   162,    -1,     1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,   174,   175,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,    -1,    -1,    62,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,    -1,   166,   167,    -1,    -1,    -1,   171,    -1,
      -1,   174,   175,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,   186,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,   174,   175,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,   186,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,   174,   175,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    20,   186,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,   174,   175,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   186,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    -1,
      -1,    -1,    76,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,    -1,
      -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
     174,   175,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    -1,    -1,    -1,    76,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     111,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,
      -1,   166,   167,    -1,     3,    -1,     5,    -1,    -1,   174,
     175,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   111,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,
      -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   111,    -1,    -1,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,     3,
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
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    -1,    13,    14,    15,    16,    17,
     174,   175,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,
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
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,   174,   175,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,   174,   175,
      78,    -1,    80,    81,    -1,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    -1,
      -1,    99,   100,   101,   102,   103,   104,   105,    -1,   107,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,
      -1,    -1,    -1,   171,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    55,    -1,    -1,    58,    -1,    60,    61,
     188,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    80,    81,
      -1,    83,    -1,    -1,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    -1,    -1,    99,   100,   101,
     102,   103,   104,   105,    -1,   107,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,   171,
      -1,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   188,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    95,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,
      -1,    -1,    -1,    -1,   171,     3,     4,     5,     6,     7,
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
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,
      -1,    -1,    -1,   171,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    -1,    96,    -1,    98,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,   128,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,
      -1,    -1,   171,    -1,   173,   174,   175,   176,   177,   178,
     179,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,   128,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,   171,
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
      -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,   128,   129,   130,   131,    -1,   133,   134,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
     165,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,
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
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
     128,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,   128,   129,   130,
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
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,   128,   129,   130,   131,    -1,   133,
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
      -1,    -1,   109,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,   128,   129,   130,   131,    -1,   133,   134,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,
     177,   178,   179,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,   128,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,   173,   174,   175,   176,   177,   178,   179,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,   128,    60,    61,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,
      -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,   128,   129,   130,   131,    -1,   133,   134,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   156,   157,   158,   159,    -1,    -1,    -1,   163,   164,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,
     175,   176,   177,   178,   179,     3,     4,     5,     6,     7,
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
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,   166,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   165,   166,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,   128,
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
      -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,   174,   175,     4,     5,     6,     7,     8,     9,    10,
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
      -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   165,   166,     4,     5,     6,     7,
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
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   143,   144,    -1,    -1,    -1,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,   166,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,   166,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,   174,   175,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,   166,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    13,
      14,    15,    16,    17,   174,   175,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    -1,    13,    14,    15,    16,    17,
     174,   175,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   174,   175,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,   166,   167,   168,    -1,    -1,    -1,
      -1,   173,   174,   175,   176,   177,   178,   179,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,   173,   174,   175,   176,   177,   178,   179,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,   166,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,   166,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      20,   166,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    79,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,   114,   115,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,
      -1,    -1,   112,    -1,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,   143,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,   143,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    20,    79,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    13,    14,    15,    16,    17,
     143,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,
      -1,    -1,    58,    -1,    60,    61,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,   143,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    55,    -1,    -1,    58,   141,    60,    61,    -1,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,   157,   158,   159,    -1,    -1,    80,   163,   164,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,    -1,   129,   130,   131,    55,   133,
     134,    58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,   166,   167,    -1,   169,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,
     167,    -1,    -1,    -1,   171,    -1,   173,   174,   175,   176,
     177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   163,   164,    -1,   166,   167,   168,    -1,
      -1,    -1,    -1,   173,   174,   175,   176,   177,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    55,
     133,   134,    58,    -1,    60,    61,    -1,    63,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     163,    -1,    -1,   166,   167,    -1,    -1,    -1,   171,    -1,
     173,   174,   175,   176,   177,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,    55,   133,   134,    58,
      -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,   170,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,   168,
      -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,   166,   167,   168,    -1,    -1,    -1,
      -1,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,    -1,   129,   130,   131,    55,   133,   134,
      58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,   164,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
      -1,   129,   130,   131,    55,   133,   134,    58,    -1,    60,
      61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,
      -1,    -1,    -1,   171,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    55,   133,   134,    58,    -1,    60,    61,    -1,    63,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,    -1,   129,   130,   131,    55,   133,
     134,    58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,
      -1,    -1,   166,   167,    -1,    -1,    -1,   171,    -1,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,   166,
     167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,
     177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   163,   164,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,   173,   174,   175,   176,   177,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    -1,
     133,   134,    -1,    -1,    55,    -1,    -1,    58,   141,    60,
      61,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
     163,   164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
     173,   174,   175,   176,   177,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    55,   133,   134,    58,    -1,    60,    61,    -1,    63,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,    -1,   129,   130,   131,    55,   133,
     134,    58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,
     177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   163,   164,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,   173,   174,   175,   176,   177,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    55,
     133,   134,    58,    -1,    60,    61,    -1,    63,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     163,   164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
     173,   174,   175,   176,   177,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,    55,   133,   134,    58,
      -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,    -1,   129,   130,   131,    55,   133,   134,
      58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,   164,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
      -1,   129,   130,   131,    55,   133,   134,    58,    -1,    60,
      61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    55,   133,   134,    58,    -1,    60,    61,    -1,    63,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   163,   164,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,   173,   174,   175,   176,   177,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,    -1,   129,   130,   131,    55,   133,
     134,    58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,
     174,   175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,
     177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   163,   164,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,   173,   174,   175,   176,   177,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    55,
     133,   134,    58,    -1,    60,    61,    -1,    63,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     163,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
     173,   174,   175,   176,   177,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,    55,   133,   134,    58,
      -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,   175,
     176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,   173,   174,   175,   176,   177,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,    -1,   129,   130,   131,    55,   133,   134,
      58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   163,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,   173,   174,
     175,   176,   177,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
      -1,   129,   130,   131,    55,   133,   134,    58,    -1,    60,
      61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,   173,   174,   175,   176,   177,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,   173,   174,   175,   176,   177,   178,   179
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
      70,    71,    72,    76,    79,    80,   107,   111,   112,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   127,
     128,   141,   143,   163,   164,   166,   167,   174,   175,   186,
     188,   193,   194,   195,   208,   302,   303,   304,   305,   306,
     307,   308,   309,   310,   311,   312,   313,   316,   319,   321,
     322,   323,   324,   325,   326,   327,   328,   329,   330,   332,
     334,   335,   336,   338,   339,   343,   344,   345,   346,   347,
     349,   355,   356,   357,   358,   370,   375,   408,   411,   421,
     427,   429,   435,   439,   444,   445,   446,   447,   448,   449,
     450,   451,   477,   495,   496,   497,   498,     0,   190,   112,
     194,   208,   306,   308,   319,   322,   325,   335,   339,   344,
     126,   163,    65,    68,    69,    71,   163,   163,   373,   433,
     434,   435,   331,   332,   114,   115,   194,   196,   409,   410,
     196,   163,   421,   163,   163,     4,   112,   114,   115,   323,
     328,   329,   163,    13,    94,   196,   436,   437,   438,   208,
     434,   439,   445,   446,   447,   449,   450,   451,   114,   346,
      55,    58,    60,    61,    63,    64,    80,   109,   110,   112,
     113,   124,   125,   126,   129,   130,   131,   133,   134,   163,
     167,   168,   173,   174,   176,   177,   178,   179,   192,   193,
     197,   198,   199,   202,   207,   208,   209,   210,   213,   214,
     215,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     229,   234,   308,   309,   318,   320,   322,   326,   327,   334,
     335,   341,   342,   343,   344,   345,   348,   355,   356,   375,
     382,   383,   384,   385,   386,   387,   477,   490,   491,   492,
     493,   498,   499,   194,   167,   309,   319,   322,   444,   448,
     477,   494,   495,   498,   499,   188,   188,   191,   160,   171,
     187,   232,   391,    95,   169,   428,   107,   196,   432,   169,
     169,   169,   188,   114,   115,   163,   208,   314,   315,   439,
     440,   441,   442,   443,   444,   448,   452,   453,   454,   455,
     456,   457,   458,   459,   460,   466,     3,    53,    54,    56,
      62,   337,     3,   167,   208,   308,   309,   323,   327,   329,
     340,   345,   424,   444,   448,   498,    76,   306,   308,   322,
     335,   339,   344,   425,   444,   448,    72,   328,   328,   323,
     329,   317,   328,   329,   337,   356,   323,   328,   323,   166,
     433,   169,   191,   163,   171,   240,   433,   433,     3,   297,
     298,   313,   316,   322,   326,   167,   319,   322,   496,   498,
     196,   196,   421,   187,   322,   163,   208,   430,   439,   440,
     444,   453,   457,   167,   208,   309,   422,   423,    64,    72,
      73,    74,    75,   167,   185,   196,   397,   399,   403,   405,
     406,   345,   165,   167,   208,   318,   322,   335,   342,   344,
     387,   490,   498,   433,   114,   115,   178,   194,   345,   374,
     466,   435,   163,   404,   405,   163,   163,   436,   197,   167,
     207,   208,   224,   225,   345,   165,   167,   208,   229,   319,
     389,   390,   407,   494,   499,   168,   169,   163,   322,   445,
     446,   447,   449,   450,   451,   165,   165,   165,   165,   165,
     165,   165,   163,   207,   163,   163,   207,   163,   163,   433,
     210,   163,   207,   163,   112,   114,   115,   323,   328,   329,
     163,   207,   207,    19,    21,    92,   167,   176,   177,   211,
     212,   229,   236,   240,   358,   389,   498,   164,   168,   169,
     229,   322,   326,   114,   167,   194,   319,   322,   477,   496,
     163,   199,   168,   167,   172,   167,   172,   126,   130,   132,
     133,   134,   163,   166,   167,   171,   172,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   187,   231,
     232,   233,   167,   210,   320,   322,   335,   342,   344,   489,
     490,   498,   499,   210,   180,   174,   181,   182,   176,   177,
     135,   136,   137,   138,   183,   184,   139,   140,   175,   173,
     185,   141,   142,   186,   168,   163,   163,   167,   175,   187,
     208,   439,   461,   462,   463,   464,   465,   466,   467,   468,
     469,   477,   479,   480,   481,   482,   483,   484,   501,   144,
     167,   208,   348,   492,   498,   322,   342,   328,   323,   166,
     433,   168,   169,   168,   169,   320,   322,   491,   498,   196,
     167,   320,   477,   491,   498,   163,   196,   168,   167,   444,
     448,   498,   167,   169,   112,   166,   167,   171,   193,   195,
     229,   392,   393,   394,   395,   396,    22,   392,   163,   196,
     240,   163,   163,   194,   430,   194,   434,   439,   441,   442,
     443,   452,   454,   455,   456,   458,   459,   460,   322,   440,
     453,   457,   169,   432,   167,   433,   474,   477,   432,   433,
     433,   428,   297,   163,   433,   474,   432,   433,   433,   428,
     433,   433,   322,   430,   163,   163,   321,   322,   319,   322,
     167,   168,   319,   494,   499,   432,   347,   171,   428,   297,
     196,   196,   391,   308,   327,   426,   444,   448,   171,   428,
     297,   409,   322,   335,   322,   322,   114,   346,   114,   115,
     194,   345,   350,   409,   144,   194,   322,   379,   380,   384,
     385,   388,   162,   190,   240,   313,   188,   444,   457,   322,
     174,   229,   383,   498,   196,   432,   163,   432,   191,   229,
     434,   439,   322,   163,   383,   419,   171,   163,   196,   171,
     196,   144,   174,   175,   402,   165,   169,   196,   406,   165,
     168,   163,   175,   208,   498,   165,   194,   374,   466,   371,
     171,   374,   397,   187,   397,   436,   165,   165,   229,   165,
     169,   163,   208,   470,   471,   472,   473,   474,   165,   169,
     437,   186,   226,   227,   228,   229,   165,   165,   165,   165,
     165,   165,   163,   433,   474,   477,   163,   474,   477,   389,
     499,   199,   389,   167,   389,   390,   194,   389,   499,   229,
     389,   165,   389,   389,   389,   168,   165,   176,   177,   212,
      18,   324,   165,   169,   165,   174,   175,   165,   169,   500,
     163,   320,   477,   491,   498,   168,   169,   167,   174,   208,
     229,   345,   229,   322,   163,   163,   319,   496,   171,   229,
     194,   229,   194,   124,   167,   194,   226,   124,   167,   196,
     358,   229,   226,   194,   171,   229,   498,   210,   213,   213,
     213,   214,   214,   215,   215,   216,   216,   216,   216,   217,
     217,   218,   219,   220,   221,   222,   170,   236,   190,   163,
     379,   439,   462,   463,   464,   467,   480,   481,   482,   168,
     190,    18,   229,   322,   461,   465,   479,   163,   433,   483,
     501,   433,   433,   501,   163,   433,   483,   433,   433,   501,
     433,   433,   477,   168,   225,   168,   322,   320,   489,   499,
     196,   322,   167,   194,   194,   383,   386,   386,   387,   501,
     320,   491,   498,   190,   501,   190,   167,   195,   224,   225,
     431,   393,   170,   169,   500,   392,   166,   167,   187,   396,
     407,   163,   197,   190,   187,   439,   441,   442,   443,   452,
     454,   455,   456,   458,   459,   460,   165,   165,   165,   165,
     165,   165,   165,   165,   165,   165,   440,   453,   457,   433,
     187,   168,   229,   329,   345,   475,   391,   240,   428,   379,
     391,   240,   430,   236,   390,   236,   390,   430,   114,   419,
     240,   428,   171,   171,   428,   297,   419,   240,   428,   352,
     353,   351,   171,   165,   169,   165,   169,    77,   299,   300,
     188,   168,   168,   169,   196,   432,   190,   439,   421,   419,
     196,   168,     1,   306,   308,   320,   322,   412,   413,   414,
     415,   163,   401,   399,   400,    85,   333,    18,   322,   433,
     171,   433,   374,    10,   173,   374,   376,   377,   171,   165,
     390,   165,   165,   188,   197,   379,   471,   472,   473,   322,
     470,   433,   433,   229,   390,   194,   165,   169,   163,   433,
     474,   477,   163,   474,   477,   379,   379,   165,   165,   169,
     165,   169,   165,   165,   165,   169,   165,   210,   165,   165,
     165,   210,    18,   324,   229,   165,   165,   164,   171,   210,
     164,   229,   235,   168,   144,   381,   382,   383,   320,   491,
     498,   168,   235,   168,   168,   168,   229,   190,   190,   226,
     168,   168,   124,   129,   131,   195,   203,   204,   205,   165,
     203,   168,   169,   162,   393,   224,   170,   381,   467,   165,
     165,   165,   165,   165,   165,   165,   165,     5,   322,   163,
     433,   439,   466,   461,   465,   479,   379,   379,   168,   501,
     203,   168,   169,   381,   196,   203,   144,   168,   179,   168,
     500,   392,   394,   162,   165,   190,   165,   381,   229,   165,
     165,   165,   165,   165,   165,   165,   165,   165,   163,   433,
     474,   477,   163,   433,   474,   477,   163,   433,   474,   477,
     430,    22,   477,   157,   169,   179,   476,   168,   169,   240,
     165,   165,   165,   165,   165,   417,   418,   240,   162,   412,
     419,   240,   428,   418,   240,   171,   171,   171,   359,   144,
     384,   385,   194,   196,   301,    18,    78,    80,    81,    83,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    99,   100,   101,   102,   103,   104,   105,   107,   114,
     115,   127,   163,   167,   196,   236,   237,   238,   239,   240,
     241,   242,   244,   245,   254,   261,   262,   263,   264,   265,
     270,   271,   274,   275,   276,   277,   278,   279,   280,   286,
     287,   288,   302,   322,   326,   429,    77,   432,   381,   420,
     418,   165,   430,   162,   413,   169,   188,   169,   188,   407,
     187,   398,   398,   372,   376,   374,   171,   345,   169,   500,
     196,   376,   171,   165,   165,   165,   165,   165,   165,   470,
     187,   228,   379,   379,   165,   165,   318,   194,    85,   200,
     201,   389,   210,   210,   210,   210,   210,   171,   393,   169,
     500,   165,   169,   169,   500,   168,   381,   381,   162,   206,
     167,   204,   206,   206,   168,   169,   132,   166,   168,   235,
     500,   224,   191,   165,   163,   433,   474,   477,   163,   433,
     483,   163,   433,   483,   477,   321,     5,   174,   191,   229,
     439,   433,   433,   165,   165,   168,   386,   191,   391,   168,
     225,   225,   162,   392,   433,   381,   433,   191,   163,   433,
     474,   477,   163,   433,   474,   477,   163,   433,   474,   477,
     379,   379,   379,   432,   236,   229,   229,   329,   345,   420,
     162,   418,   240,   420,   359,   359,   359,     3,     5,    10,
      80,   162,   303,   310,   311,   319,   322,   360,   366,   494,
     169,   188,   163,    68,    69,   188,   240,   302,   429,   163,
     163,    18,   238,   163,   163,   188,   196,   188,   196,   174,
     196,   171,   237,   163,    85,   188,   196,   163,   163,   238,
     163,   240,   229,   230,   230,    14,   289,   265,   276,   170,
     188,   191,   242,    97,    98,   269,   273,   118,   142,   268,
     117,   141,   272,   268,   388,   322,   301,   191,   420,   196,
     196,   430,   165,   390,   404,   404,   374,   500,   171,   376,
      10,   377,   162,   187,   378,   500,   162,   412,   163,   433,
     474,   477,   229,   165,   165,   478,   479,   165,   170,   165,
     169,   170,   393,   500,   164,   229,   168,   144,   383,   144,
     168,   191,   191,   130,   203,   204,   167,   204,   167,   204,
     168,   169,   162,   165,   379,   379,   379,   229,   229,   191,
     168,   191,   165,   168,   191,   165,   379,   379,   379,   165,
     165,   165,   391,   168,   476,   162,   420,   162,   162,   162,
     162,   319,   319,   358,   367,   494,   319,   366,   163,   354,
     188,   188,   188,   163,   170,   208,   361,   362,   363,   369,
     439,   440,   453,   457,   169,   188,   196,   196,   226,   188,
     240,   188,   240,   236,   246,   302,   304,   307,   313,   322,
     326,   236,    87,   165,   246,   156,   157,   158,   159,   164,
     165,   188,   236,   255,   256,   258,   302,   188,   188,   236,
     188,   393,   188,   236,   188,   188,   407,   236,   255,   119,
     120,   121,   122,   123,   281,   283,   284,   188,   106,   188,
      91,   163,   165,   433,   162,   163,   163,   238,   238,   265,
     163,   275,   265,   275,   240,   188,   165,   162,   402,   171,
     162,   376,   500,   345,   196,   171,   225,   162,   162,   379,
     165,   229,   201,   229,   500,   162,   165,   165,   168,   203,
     203,   165,   165,   165,   191,   191,   168,   168,   165,   433,
     165,   165,   165,   229,   162,   354,   354,   354,   361,   163,
     208,   364,   365,   474,   485,   486,   487,   488,   188,   169,
     188,   361,   188,   407,   434,   439,   229,   322,   162,   169,
     188,   368,   369,   368,   368,   196,   165,   165,   236,   322,
     165,   163,   238,   165,   156,   157,   158,   159,   179,   188,
     259,   260,   238,   237,   188,   260,   165,   170,   236,   164,
     236,   237,   258,   188,   500,   165,   165,   165,   165,   240,
     283,   284,   163,   229,   163,   197,     1,   238,   210,   266,
     236,    82,   116,   267,   269,    82,   433,   398,   376,   500,
     162,   378,   393,   165,   162,   433,   433,   168,   168,   168,
     168,   188,   486,   487,   488,   322,   485,   169,   188,   433,
     433,   188,   165,   439,   433,   238,   238,    84,    85,   171,
     249,   250,   251,   165,   236,    82,   238,   236,   164,   236,
      82,   188,   164,   236,   237,   258,   322,   344,   164,   236,
     238,   256,   260,   260,   188,   236,   162,   171,   251,   238,
     238,   163,   285,   320,   322,   494,   188,   197,   165,   170,
     165,   169,   170,   165,   238,   163,   238,   238,   238,   404,
     500,   162,   500,   165,   165,   165,   485,   433,   363,    82,
       1,   225,   247,   248,   431,     1,   170,     1,   190,   238,
     249,    82,   188,   165,   238,    82,   188,   179,   179,   238,
     237,   260,   260,   188,    64,   236,   257,   345,   179,   179,
      82,   164,   236,   164,   236,   237,   188,     1,   190,   285,
     188,   282,   163,   208,   430,   485,   194,   170,   188,   167,
     197,   290,   291,   292,   210,   226,   236,   268,   162,   162,
     163,   433,   474,   477,   365,   238,   144,     1,   169,   170,
     162,   295,   296,   302,   238,    82,   188,   238,   236,   164,
     164,   236,   164,   236,   164,   236,   237,   194,   345,   164,
     236,   164,   236,   238,   179,   179,   179,   179,   162,   295,
     282,   224,   165,   322,   170,   112,   163,   165,   170,   169,
     165,   165,    82,   264,   379,   225,   247,   250,   252,   253,
     302,   238,   179,   179,   179,   179,   164,   164,   236,   164,
     236,   164,   236,   252,   165,   240,   290,   168,   225,   188,
     290,   292,   238,    82,   165,   238,   243,   191,   250,   164,
     164,   236,   164,   236,   164,   236,   191,   240,   170,   197,
     165,   165,   170,   238,     1,   238,   162,   243,   162,   197,
     293,   163,   188,   293,   169,   170,   225,   165,   197,   194,
     294,   165,   188,   165,   169,   188,   194
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
     233,   233,   233,   233,   233,   234,   234,   234,   234,   234,
     235,   235,   235,   235,   236,   236,   237,   237,   238,   238,
     238,   238,   238,   238,   238,   238,   238,   238,   238,   238,
     238,   238,   238,   238,   239,   239,   240,   240,   241,   241,
     242,   242,   242,   242,   242,   243,   243,   243,   244,   245,
     245,   245,   245,   245,   245,   245,   245,   246,   246,   246,
     246,   247,   247,   247,   248,   248,   249,   249,   249,   249,
     249,   250,   250,   251,   252,   252,   253,   253,   254,   254,
     254,   254,   254,   254,   254,   254,   254,   254,   254,   254,
     255,   255,   256,   256,   256,   256,   256,   256,   256,   256,
     256,   256,   256,   256,   256,   256,   256,   256,   256,   256,
     256,   256,   256,   256,   256,   256,   256,   256,   256,   256,
     256,   256,   256,   256,   256,   256,   256,   256,   256,   256,
     256,   256,   256,   256,   256,   256,   256,   257,   257,   257,
     258,   258,   258,   258,   259,   259,   259,   260,   260,   260,
     261,   261,   261,   261,   261,   261,   261,   261,   261,   261,
     261,   261,   261,   261,   261,   261,   261,   261,   261,   261,
     262,   263,   264,   265,   265,   266,   266,   267,   268,   268,
     269,   269,   270,   270,   270,   270,   270,   270,   271,   272,
     272,   273,   274,   274,   275,   275,   276,   276,   276,   277,
     278,   279,   280,   280,   280,   281,   281,   282,   282,   283,
     283,   283,   283,   284,   285,   285,   285,   285,   285,   286,
     287,   287,   288,   288,   288,   288,   288,   289,   289,   290,
     290,   291,   291,   292,   292,   293,   293,   293,   294,   294,
     295,   295,   296,   296,   297,   297,   298,   298,   299,   299,
     300,   300,   301,   301,   302,   302,   302,   303,   303,   304,
     304,   304,   304,   304,   305,   305,   305,   306,   306,   306,
     306,   306,   306,   307,   307,   307,   307,   307,   308,   308,
     308,   308,   309,   309,   310,   310,   310,   311,   311,   311,
     311,   311,   312,   312,   313,   313,   313,   313,   314,   314,
     314,   314,   314,   315,   315,   316,   316,   316,   316,   317,
     317,   317,   318,   318,   318,   319,   319,   319,   320,   320,
     320,   321,   321,   322,   322,   323,   323,   324,   324,   324,
     324,   324,   325,   326,   326,   326,   327,   327,   328,   328,
     328,   328,   328,   328,   328,   328,   328,   329,   330,   330,
     330,   330,   330,   330,   330,   330,   330,   330,   330,   330,
     330,   330,   330,   330,   330,   330,   330,   330,   330,   330,
     330,   330,   330,   330,   330,   330,   330,   330,   330,   330,
     330,   330,   331,   331,   332,   333,   333,   334,   334,   334,
     334,   334,   335,   335,   336,   336,   336,   336,   337,   337,
     337,   337,   337,   337,   338,   338,   338,   338,   339,   340,
     339,   339,   341,   341,   341,   341,   342,   342,   342,   343,
     343,   343,   343,   344,   344,   344,   345,   345,   345,   345,
     345,   345,   346,   346,   346,   347,   347,   348,   348,   350,
     349,   351,   349,   352,   349,   353,   349,   349,   354,   354,
     355,   355,   356,   356,   357,   357,   357,   358,   358,   358,
     358,   358,   358,   358,   358,   359,   359,   360,   360,   360,
     360,   360,   360,   360,   360,   360,   360,   360,   360,   361,
     361,   362,   362,   363,   363,   363,   363,   364,   364,   364,
     365,   366,   366,   367,   367,   368,   368,   369,   370,   370,
     371,   370,   370,   372,   370,   370,   370,   373,   373,   374,
     374,   375,   375,   376,   376,   376,   376,   376,   377,   377,
     378,   378,   378,   379,   379,   379,   379,   380,   380,   380,
     380,   381,   381,   381,   381,   381,   381,   381,   382,   382,
     382,   382,   383,   383,   384,   384,   385,   385,   386,   386,
     386,   386,   386,   387,   387,   387,   387,   387,   388,   388,
     389,   389,   389,   390,   390,   391,   391,   391,   391,   392,
     392,   393,   393,   393,   393,   393,   394,   394,   395,   395,
     396,   396,   396,   396,   396,   397,   397,   398,   398,   400,
     399,   401,   399,   399,   399,   399,   402,   402,   402,   402,
     403,   403,   403,   403,   404,   404,   405,   405,   406,   406,
     407,   407,   407,   407,   408,   408,   408,   409,   409,   410,
     410,   411,   411,   411,   411,   412,   412,   413,   413,   414,
     414,   414,   415,   415,   415,   416,   416,   417,   417,   418,
     418,   419,   420,   421,   421,   421,   421,   421,   421,   421,
     421,   421,   421,   421,   422,   421,   423,   421,   424,   421,
     425,   421,   426,   421,   421,   427,   427,   427,   428,   428,
     429,   429,   429,   429,   429,   429,   429,   429,   429,   429,
     430,   430,   430,   430,   431,   432,   432,   433,   433,   434,
     434,   435,   435,   435,   435,   436,   436,   437,   437,   437,
     438,   438,   438,   439,   439,   439,   440,   440,   440,   440,
     441,   441,   441,   441,   442,   442,   442,   442,   442,   442,
     442,   443,   443,   443,   443,   444,   444,   444,   445,   445,
     445,   445,   445,   446,   446,   446,   446,   447,   447,   447,
     447,   447,   447,   448,   448,   448,   449,   449,   449,   449,
     449,   450,   450,   450,   450,   451,   451,   451,   451,   451,
     451,   452,   452,   453,   453,   453,   453,   454,   454,   454,
     454,   455,   455,   455,   455,   455,   455,   455,   456,   456,
     456,   456,   457,   457,   457,   458,   458,   458,   458,   458,
     459,   459,   459,   459,   460,   460,   460,   460,   460,   460,
     461,   461,   461,   461,   461,   462,   462,   462,   463,   463,
     463,   463,   464,   464,   464,   465,   465,   465,   465,   465,
     466,   466,   467,   467,   467,   468,   468,   469,   469,   470,
     470,   470,   471,   471,   471,   471,   471,   472,   472,   472,
     472,   473,   473,   473,   474,   474,   474,   474,   474,   475,
     475,   475,   475,   475,   475,   476,   476,   477,   477,   477,
     477,   478,   478,   479,   479,   479,   479,   480,   480,   480,
     480,   480,   481,   481,   481,   481,   482,   482,   482,   483,
     483,   483,   484,   484,   484,   484,   484,   484,   485,   485,
     485,   486,   486,   486,   486,   486,   487,   487,   487,   487,
     488,   488,   489,   489,   489,   490,   490,   490,   491,   491,
     491,   491,   491,   491,   492,   492,   492,   492,   492,   492,
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
       1,     1,     1,     1,     1,     3,     4,     4,     6,     6,
       1,     1,     3,     3,     1,     3,     0,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     4,     2,     6,     1,     2,
       1,     2,     1,     2,     1,     1,     2,     2,     2,     5,
       7,     5,    10,     7,     5,    10,     7,     1,     1,     1,
       2,     1,     3,     1,     1,     3,     2,     3,     3,     2,
       2,     1,     2,     2,     0,     1,     2,     3,     4,     6,
       5,     7,     6,     7,     7,     8,     4,     6,     5,     7,
       1,     3,     4,     5,     4,     3,     5,     1,     2,     3,
       3,     3,     5,     5,     5,     5,     3,     5,     5,     5,
       3,     4,     5,     5,     5,     5,     7,     7,     7,     7,
       7,     7,     7,     2,     3,     4,     4,     4,     6,     6,
       6,     6,     6,     6,     6,     3,     4,     1,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     4,     2,     3,     3,     2,     3,     2,     3,     3,
       6,     2,     2,     3,     3,     3,     3,     3,     3,     5,
       5,     5,     4,     0,     1,     1,     3,     4,     1,     1,
       4,     6,     3,     5,     5,     5,     8,     9,     1,     1,
       1,     4,     3,     3,     1,     3,     1,     3,     5,     1,
       2,     5,     3,     3,     4,     6,     7,     0,     2,     1,
       1,     1,     1,     2,     1,     2,     2,     2,     1,     3,
       1,     1,     6,     8,    10,    12,    14,     0,     1,     0,
       1,     1,     3,     4,     7,     0,     1,     3,     1,     3,
       0,     1,     1,     2,     0,     1,     2,     3,     0,     1,
       3,     4,     1,     3,     2,     2,     2,     6,     4,     1,
       1,     1,     1,     1,     2,     3,     6,     3,     3,     4,
       5,     2,     3,     1,     2,     2,     3,     8,     9,     9,
       8,     8,     3,     5,     2,     2,     3,     3,     3,     4,
       3,     4,     4,     5,     2,     1,     1,     1,     3,     3,
       2,     4,     6,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     4,     1,     2,     3,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     5,     0,     1,     1,     2,     2,
       3,     3,     1,     3,     1,     2,     2,     2,     4,     4,
       4,     4,     1,     1,     1,     2,     2,     3,     1,     0,
       3,     2,     1,     2,     2,     3,     1,     2,     2,     1,
       2,     2,     3,     1,     2,     2,     1,     2,     3,     1,
       2,     3,     1,     3,     4,     1,     1,     1,     1,     0,
       7,     0,     8,     0,     8,     0,     8,     1,     0,     3,
       3,     3,     1,     1,     2,     1,     1,     1,     2,     1,
       2,     1,     2,     1,     2,     0,     2,     3,     3,     4,
       4,     4,     3,     2,     2,     3,     3,     2,     2,     0,
       1,     1,     4,     1,     2,     2,     2,     0,     1,     4,
       1,     2,     3,     1,     2,     0,     1,     2,     7,     8,
       0,     9,     8,     0,    11,    10,     1,     2,     3,     0,
       1,     3,     3,     0,     3,     2,     5,     4,     1,     1,
       0,     2,     5,     0,     1,     1,     3,     1,     1,     3,
       3,     0,     1,     1,     1,     3,     3,     3,     1,     3,
       3,     5,     1,     3,     3,     3,     2,     3,     1,     3,
       3,     4,     1,     1,     1,     1,     2,     1,     1,     3,
       1,     1,     2,     1,     1,     0,     2,     2,     4,     1,
       4,     0,     1,     2,     3,     4,     2,     2,     1,     2,
       2,     3,     3,     5,     4,     1,     3,     0,     2,     0,
       5,     0,     5,     3,     1,     8,     0,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     1,     2,     5,     4,
       1,     1,     3,     3,     2,     3,     3,     2,     4,     1,
       4,     7,     5,     8,     6,     1,     2,     2,     2,     1,
       1,     3,     2,     3,     1,     0,     1,     3,     4,     0,
       1,     0,     0,     1,     1,     2,     2,     2,     2,     2,
       2,     1,     2,     5,     0,     6,     0,     8,     0,     7,
       0,     7,     0,     8,     1,     1,     2,     3,     0,     5,
       3,     4,     4,     4,     4,     5,     5,     5,     5,     6,
       1,     1,     1,     1,     3,     0,     5,     0,     1,     1,
       2,     6,     4,     3,     1,     1,     3,     0,     1,     4,
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
       3,     3,     6,     3,     2,     3,     5,     3,     1,     1,
       1,     3,     3,     3,     5,     1,     1,     3,     3,     4,
       4,     0,     1,     1,     3,     2,     2,     1,     2,     2,
       3,     4,     1,     4,     4,     3,     3,     6,     3,     1,
       2,     1,     2,     6,     5,     6,     7,     7,     1,     2,
       2,     1,     2,     2,     3,     4,     1,     4,     4,     3,
       6,     3,     1,     1,     2,     1,     1,     2,     2,     3,
       2,     3,     2,     3,     3,     3,     2,     2,     4,     4,
       3,     3,     2,     2,     3,     2,     4,     3,     2,     4,
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
#line 640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 9918 "Parser/parser.cc"
    break;

  case 3:
#line 644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 9924 "Parser/parser.cc"
    break;

  case 4:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 9930 "Parser/parser.cc"
    break;

  case 5:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9936 "Parser/parser.cc"
    break;

  case 6:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9942 "Parser/parser.cc"
    break;

  case 7:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9948 "Parser/parser.cc"
    break;

  case 8:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 9954 "Parser/parser.cc"
    break;

  case 20:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 9960 "Parser/parser.cc"
    break;

  case 24:
#line 687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 9966 "Parser/parser.cc"
    break;

  case 25:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 9972 "Parser/parser.cc"
    break;

  case 26:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 9982 "Parser/parser.cc"
    break;

  case 27:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 9988 "Parser/parser.cc"
    break;

  case 28:
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 9994 "Parser/parser.cc"
    break;

  case 29:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 10000 "Parser/parser.cc"
    break;

  case 31:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10006 "Parser/parser.cc"
    break;

  case 32:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 10012 "Parser/parser.cc"
    break;

  case 33:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10018 "Parser/parser.cc"
    break;

  case 34:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10024 "Parser/parser.cc"
    break;

  case 35:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 10034 "Parser/parser.cc"
    break;

  case 36:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 10040 "Parser/parser.cc"
    break;

  case 37:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 10046 "Parser/parser.cc"
    break;

  case 38:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 10052 "Parser/parser.cc"
    break;

  case 39:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10058 "Parser/parser.cc"
    break;

  case 40:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10064 "Parser/parser.cc"
    break;

  case 41:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10070 "Parser/parser.cc"
    break;

  case 43:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 10082 "Parser/parser.cc"
    break;

  case 44:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 10091 "Parser/parser.cc"
    break;

  case 45:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 10097 "Parser/parser.cc"
    break;

  case 47:
#line 770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 10103 "Parser/parser.cc"
    break;

  case 48:
#line 776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10109 "Parser/parser.cc"
    break;

  case 49:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10115 "Parser/parser.cc"
    break;

  case 50:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10121 "Parser/parser.cc"
    break;

  case 51:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 10131 "Parser/parser.cc"
    break;

  case 52:
#line 788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10137 "Parser/parser.cc"
    break;

  case 53:
#line 790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_va_arg( yylloc, (yyvsp[-4].expr), ( (yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl) ) ) ); }
#line 10143 "Parser/parser.cc"
    break;

  case 54:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10149 "Parser/parser.cc"
    break;

  case 55:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10155 "Parser/parser.cc"
    break;

  case 56:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10161 "Parser/parser.cc"
    break;

  case 57:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10167 "Parser/parser.cc"
    break;

  case 58:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10173 "Parser/parser.cc"
    break;

  case 59:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 60:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10185 "Parser/parser.cc"
    break;

  case 61:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 10191 "Parser/parser.cc"
    break;

  case 62:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10197 "Parser/parser.cc"
    break;

  case 63:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10203 "Parser/parser.cc"
    break;

  case 64:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10209 "Parser/parser.cc"
    break;

  case 65:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 10215 "Parser/parser.cc"
    break;

  case 66:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 10221 "Parser/parser.cc"
    break;

  case 67:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 10227 "Parser/parser.cc"
    break;

  case 68:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 10233 "Parser/parser.cc"
    break;

  case 69:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 10243 "Parser/parser.cc"
    break;

  case 71:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10249 "Parser/parser.cc"
    break;

  case 73:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10255 "Parser/parser.cc"
    break;

  case 74:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10261 "Parser/parser.cc"
    break;

  case 75:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10267 "Parser/parser.cc"
    break;

  case 76:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10273 "Parser/parser.cc"
    break;

  case 77:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10279 "Parser/parser.cc"
    break;

  case 78:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10285 "Parser/parser.cc"
    break;

  case 79:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10291 "Parser/parser.cc"
    break;

  case 80:
#line 873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10297 "Parser/parser.cc"
    break;

  case 81:
#line 875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 10305 "Parser/parser.cc"
    break;

  case 82:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10311 "Parser/parser.cc"
    break;

  case 83:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 10320 "Parser/parser.cc"
    break;

  case 86:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10326 "Parser/parser.cc"
    break;

  case 87:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 10332 "Parser/parser.cc"
    break;

  case 88:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10352 "Parser/parser.cc"
    break;

  case 89:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 10358 "Parser/parser.cc"
    break;

  case 90:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 10364 "Parser/parser.cc"
    break;

  case 91:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 10370 "Parser/parser.cc"
    break;

  case 92:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10376 "Parser/parser.cc"
    break;

  case 93:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10382 "Parser/parser.cc"
    break;

  case 94:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10388 "Parser/parser.cc"
    break;

  case 95:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10394 "Parser/parser.cc"
    break;

  case 96:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10400 "Parser/parser.cc"
    break;

  case 97:
#line 938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10406 "Parser/parser.cc"
    break;

  case 98:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 10412 "Parser/parser.cc"
    break;

  case 99:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 10421 "Parser/parser.cc"
    break;

  case 100:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10427 "Parser/parser.cc"
    break;

  case 101:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10433 "Parser/parser.cc"
    break;

  case 102:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 10439 "Parser/parser.cc"
    break;

  case 103:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 10445 "Parser/parser.cc"
    break;

  case 104:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 10451 "Parser/parser.cc"
    break;

  case 105:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 10457 "Parser/parser.cc"
    break;

  case 106:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 10463 "Parser/parser.cc"
    break;

  case 107:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 10469 "Parser/parser.cc"
    break;

  case 108:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 10475 "Parser/parser.cc"
    break;

  case 110:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 10481 "Parser/parser.cc"
    break;

  case 111:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10487 "Parser/parser.cc"
    break;

  case 112:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10493 "Parser/parser.cc"
    break;

  case 113:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 10499 "Parser/parser.cc"
    break;

  case 114:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 10505 "Parser/parser.cc"
    break;

  case 115:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::ReturnCast ) ); }
#line 10511 "Parser/parser.cc"
    break;

  case 116:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10517 "Parser/parser.cc"
    break;

  case 117:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10523 "Parser/parser.cc"
    break;

  case 125:
#line 1004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10529 "Parser/parser.cc"
    break;

  case 127:
#line 1010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10535 "Parser/parser.cc"
    break;

  case 128:
#line 1012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10541 "Parser/parser.cc"
    break;

  case 129:
#line 1014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10547 "Parser/parser.cc"
    break;

  case 131:
#line 1020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10553 "Parser/parser.cc"
    break;

  case 132:
#line 1022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10559 "Parser/parser.cc"
    break;

  case 134:
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10565 "Parser/parser.cc"
    break;

  case 135:
#line 1030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10571 "Parser/parser.cc"
    break;

  case 137:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10577 "Parser/parser.cc"
    break;

  case 138:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10583 "Parser/parser.cc"
    break;

  case 139:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10589 "Parser/parser.cc"
    break;

  case 140:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10595 "Parser/parser.cc"
    break;

  case 142:
#line 1048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10601 "Parser/parser.cc"
    break;

  case 143:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10607 "Parser/parser.cc"
    break;

  case 145:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10613 "Parser/parser.cc"
    break;

  case 147:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10619 "Parser/parser.cc"
    break;

  case 149:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10625 "Parser/parser.cc"
    break;

  case 151:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 10631 "Parser/parser.cc"
    break;

  case 153:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 10637 "Parser/parser.cc"
    break;

  case 155:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10643 "Parser/parser.cc"
    break;

  case 156:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 10649 "Parser/parser.cc"
    break;

  case 158:
#line 1097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10655 "Parser/parser.cc"
    break;

  case 161:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10661 "Parser/parser.cc"
    break;

  case 162:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 10667 "Parser/parser.cc"
    break;

  case 163:
#line 1114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10673 "Parser/parser.cc"
    break;

  case 166:
#line 1122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 10685 "Parser/parser.cc"
    break;

  case 167:
#line 1130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10691 "Parser/parser.cc"
    break;

  case 168:
#line 1135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10697 "Parser/parser.cc"
    break;

  case 172:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 10703 "Parser/parser.cc"
    break;

  case 173:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 10709 "Parser/parser.cc"
    break;

  case 174:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 10715 "Parser/parser.cc"
    break;

  case 175:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 10721 "Parser/parser.cc"
    break;

  case 176:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 10727 "Parser/parser.cc"
    break;

  case 177:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 10733 "Parser/parser.cc"
    break;

  case 178:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 10739 "Parser/parser.cc"
    break;

  case 179:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 10745 "Parser/parser.cc"
    break;

  case 180:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 10751 "Parser/parser.cc"
    break;

  case 181:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 10757 "Parser/parser.cc"
    break;

  case 182:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 10763 "Parser/parser.cc"
    break;

  case 183:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 10769 "Parser/parser.cc"
    break;

  case 184:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 10775 "Parser/parser.cc"
    break;

  case 185:
#line 1168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Empty tuple is meaningless." ); (yyval.expr) = nullptr; }
#line 10781 "Parser/parser.cc"
    break;

  case 186:
#line 1170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-2].expr) ) ); }
#line 10787 "Parser/parser.cc"
    break;

  case 187:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10793 "Parser/parser.cc"
    break;

  case 188:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-2].expr) ) ) ); }
#line 10799 "Parser/parser.cc"
    break;

  case 189:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10805 "Parser/parser.cc"
    break;

  case 191:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10811 "Parser/parser.cc"
    break;

  case 192:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10817 "Parser/parser.cc"
    break;

  case 193:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10823 "Parser/parser.cc"
    break;

  case 195:
#line 1192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10829 "Parser/parser.cc"
    break;

  case 196:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10835 "Parser/parser.cc"
    break;

  case 211:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10841 "Parser/parser.cc"
    break;

  case 213:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 10847 "Parser/parser.cc"
    break;

  case 214:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 10853 "Parser/parser.cc"
    break;

  case 215:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 10864 "Parser/parser.cc"
    break;

  case 216:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 10870 "Parser/parser.cc"
    break;

  case 217:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 10876 "Parser/parser.cc"
    break;

  case 219:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10882 "Parser/parser.cc"
    break;

  case 220:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10888 "Parser/parser.cc"
    break;

  case 221:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10894 "Parser/parser.cc"
    break;

  case 222:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10900 "Parser/parser.cc"
    break;

  case 223:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10906 "Parser/parser.cc"
    break;

  case 226:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10912 "Parser/parser.cc"
    break;

  case 227:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 10919 "Parser/parser.cc"
    break;

  case 228:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 10925 "Parser/parser.cc"
    break;

  case 229:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 10931 "Parser/parser.cc"
    break;

  case 230:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10937 "Parser/parser.cc"
    break;

  case 231:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10943 "Parser/parser.cc"
    break;

  case 232:
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
#line 10957 "Parser/parser.cc"
    break;

  case 233:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10963 "Parser/parser.cc"
    break;

  case 234:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10969 "Parser/parser.cc"
    break;

  case 235:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 10978 "Parser/parser.cc"
    break;

  case 236:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10984 "Parser/parser.cc"
    break;

  case 237:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( nullptr, (yyvsp[0].expr) ); }
#line 10990 "Parser/parser.cc"
    break;

  case 238:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 10996 "Parser/parser.cc"
    break;

  case 239:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 11002 "Parser/parser.cc"
    break;

  case 240:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 11008 "Parser/parser.cc"
    break;

  case 241:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 11014 "Parser/parser.cc"
    break;

  case 242:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 11020 "Parser/parser.cc"
    break;

  case 244:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 11026 "Parser/parser.cc"
    break;

  case 245:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 11032 "Parser/parser.cc"
    break;

  case 246:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 11038 "Parser/parser.cc"
    break;

  case 247:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 11044 "Parser/parser.cc"
    break;

  case 248:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 11050 "Parser/parser.cc"
    break;

  case 249:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 11056 "Parser/parser.cc"
    break;

  case 250:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 11062 "Parser/parser.cc"
    break;

  case 252:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 11068 "Parser/parser.cc"
    break;

  case 253:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11074 "Parser/parser.cc"
    break;

  case 254:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 11080 "Parser/parser.cc"
    break;

  case 256:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11086 "Parser/parser.cc"
    break;

  case 257:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 11092 "Parser/parser.cc"
    break;

  case 258:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11098 "Parser/parser.cc"
    break;

  case 259:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11107 "Parser/parser.cc"
    break;

  case 260:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11113 "Parser/parser.cc"
    break;

  case 261:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11119 "Parser/parser.cc"
    break;

  case 262:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 11125 "Parser/parser.cc"
    break;

  case 263:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11134 "Parser/parser.cc"
    break;

  case 264:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 11140 "Parser/parser.cc"
    break;

  case 265:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11146 "Parser/parser.cc"
    break;

  case 266:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11152 "Parser/parser.cc"
    break;

  case 267:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11161 "Parser/parser.cc"
    break;

  case 268:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11167 "Parser/parser.cc"
    break;

  case 269:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11173 "Parser/parser.cc"
    break;

  case 271:
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
#line 11192 "Parser/parser.cc"
    break;

  case 272:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11198 "Parser/parser.cc"
    break;

  case 273:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 11206 "Parser/parser.cc"
    break;

  case 274:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11212 "Parser/parser.cc"
    break;

  case 275:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 11218 "Parser/parser.cc"
    break;

  case 276:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11224 "Parser/parser.cc"
    break;

  case 277:
#line 1472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11230 "Parser/parser.cc"
    break;

  case 278:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11236 "Parser/parser.cc"
    break;

  case 279:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11242 "Parser/parser.cc"
    break;

  case 280:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11251 "Parser/parser.cc"
    break;

  case 281:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11260 "Parser/parser.cc"
    break;

  case 282:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11266 "Parser/parser.cc"
    break;

  case 283:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11275 "Parser/parser.cc"
    break;

  case 284:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11284 "Parser/parser.cc"
    break;

  case 285:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11290 "Parser/parser.cc"
    break;

  case 286:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11296 "Parser/parser.cc"
    break;

  case 287:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11302 "Parser/parser.cc"
    break;

  case 288:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11308 "Parser/parser.cc"
    break;

  case 289:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11314 "Parser/parser.cc"
    break;

  case 290:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11320 "Parser/parser.cc"
    break;

  case 291:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11326 "Parser/parser.cc"
    break;

  case 292:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11332 "Parser/parser.cc"
    break;

  case 293:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11341 "Parser/parser.cc"
    break;

  case 294:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11351 "Parser/parser.cc"
    break;

  case 295:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11357 "Parser/parser.cc"
    break;

  case 296:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11363 "Parser/parser.cc"
    break;

  case 297:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11372 "Parser/parser.cc"
    break;

  case 298:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11382 "Parser/parser.cc"
    break;

  case 299:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11388 "Parser/parser.cc"
    break;

  case 300:
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11397 "Parser/parser.cc"
    break;

  case 301:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11407 "Parser/parser.cc"
    break;

  case 302:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11413 "Parser/parser.cc"
    break;

  case 303:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 11419 "Parser/parser.cc"
    break;

  case 304:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11425 "Parser/parser.cc"
    break;

  case 305:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11431 "Parser/parser.cc"
    break;

  case 306:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11440 "Parser/parser.cc"
    break;

  case 307:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11450 "Parser/parser.cc"
    break;

  case 308:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11456 "Parser/parser.cc"
    break;

  case 309:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11465 "Parser/parser.cc"
    break;

  case 310:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11475 "Parser/parser.cc"
    break;

  case 311:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11481 "Parser/parser.cc"
    break;

  case 312:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11490 "Parser/parser.cc"
    break;

  case 313:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11500 "Parser/parser.cc"
    break;

  case 314:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11506 "Parser/parser.cc"
    break;

  case 315:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 11514 "Parser/parser.cc"
    break;

  case 316:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); (yyval.forctrl) = nullptr;
				(yyvsp[-1].oper) = OperKinds::GEThan;
			} // if
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-3].expr), (yyvsp[-1].oper), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 11526 "Parser/parser.cc"
    break;

  case 317:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11535 "Parser/parser.cc"
    break;

  case 318:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false );
		}
#line 11544 "Parser/parser.cc"
    break;

  case 319:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11553 "Parser/parser.cc"
    break;

  case 320:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11559 "Parser/parser.cc"
    break;

  case 321:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11565 "Parser/parser.cc"
    break;

  case 322:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11571 "Parser/parser.cc"
    break;

  case 323:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11577 "Parser/parser.cc"
    break;

  case 324:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11583 "Parser/parser.cc"
    break;

  case 325:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11589 "Parser/parser.cc"
    break;

  case 326:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11595 "Parser/parser.cc"
    break;

  case 328:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11601 "Parser/parser.cc"
    break;

  case 329:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11607 "Parser/parser.cc"
    break;

  case 330:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 11613 "Parser/parser.cc"
    break;

  case 331:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 11619 "Parser/parser.cc"
    break;

  case 332:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 11625 "Parser/parser.cc"
    break;

  case 333:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 11631 "Parser/parser.cc"
    break;

  case 334:
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 11637 "Parser/parser.cc"
    break;

  case 335:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 11643 "Parser/parser.cc"
    break;

  case 336:
#line 1695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 11649 "Parser/parser.cc"
    break;

  case 337:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 11655 "Parser/parser.cc"
    break;

  case 338:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 11661 "Parser/parser.cc"
    break;

  case 339:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 11667 "Parser/parser.cc"
    break;

  case 340:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 11673 "Parser/parser.cc"
    break;

  case 341:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 11679 "Parser/parser.cc"
    break;

  case 342:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 11685 "Parser/parser.cc"
    break;

  case 343:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 11691 "Parser/parser.cc"
    break;

  case 344:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 11697 "Parser/parser.cc"
    break;

  case 345:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 11703 "Parser/parser.cc"
    break;

  case 346:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 11709 "Parser/parser.cc"
    break;

  case 347:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 11715 "Parser/parser.cc"
    break;

  case 348:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 11721 "Parser/parser.cc"
    break;

  case 349:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 11727 "Parser/parser.cc"
    break;

  case 350:
#line 1729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11733 "Parser/parser.cc"
    break;

  case 351:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 11742 "Parser/parser.cc"
    break;

  case 352:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11748 "Parser/parser.cc"
    break;

  case 353:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11754 "Parser/parser.cc"
    break;

  case 356:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11760 "Parser/parser.cc"
    break;

  case 357:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11766 "Parser/parser.cc"
    break;

  case 360:
#line 1767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11772 "Parser/parser.cc"
    break;

  case 361:
#line 1769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 11778 "Parser/parser.cc"
    break;

  case 362:
#line 1775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11784 "Parser/parser.cc"
    break;

  case 363:
#line 1777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11790 "Parser/parser.cc"
    break;

  case 364:
#line 1779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11796 "Parser/parser.cc"
    break;

  case 365:
#line 1781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11802 "Parser/parser.cc"
    break;

  case 366:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 11808 "Parser/parser.cc"
    break;

  case 367:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11814 "Parser/parser.cc"
    break;

  case 368:
#line 1791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 11820 "Parser/parser.cc"
    break;

  case 371:
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11826 "Parser/parser.cc"
    break;

  case 372:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11832 "Parser/parser.cc"
    break;

  case 373:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 11838 "Parser/parser.cc"
    break;

  case 374:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11844 "Parser/parser.cc"
    break;

  case 375:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11850 "Parser/parser.cc"
    break;

  case 376:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11856 "Parser/parser.cc"
    break;

  case 377:
#line 1822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11862 "Parser/parser.cc"
    break;

  case 378:
#line 1824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11868 "Parser/parser.cc"
    break;

  case 379:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 11874 "Parser/parser.cc"
    break;

  case 380:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 11880 "Parser/parser.cc"
    break;

  case 381:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11886 "Parser/parser.cc"
    break;

  case 382:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 11892 "Parser/parser.cc"
    break;

  case 383:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 11898 "Parser/parser.cc"
    break;

  case 384:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 11904 "Parser/parser.cc"
    break;

  case 385:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11910 "Parser/parser.cc"
    break;

  case 386:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-6].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 11916 "Parser/parser.cc"
    break;

  case 387:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11922 "Parser/parser.cc"
    break;

  case 388:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 11928 "Parser/parser.cc"
    break;

  case 389:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11934 "Parser/parser.cc"
    break;

  case 390:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11940 "Parser/parser.cc"
    break;

  case 391:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 11946 "Parser/parser.cc"
    break;

  case 392:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 11952 "Parser/parser.cc"
    break;

  case 393:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 11958 "Parser/parser.cc"
    break;

  case 395:
#line 1879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11964 "Parser/parser.cc"
    break;

  case 396:
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11970 "Parser/parser.cc"
    break;

  case 397:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11976 "Parser/parser.cc"
    break;

  case 402:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 11982 "Parser/parser.cc"
    break;

  case 403:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11988 "Parser/parser.cc"
    break;

  case 404:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11994 "Parser/parser.cc"
    break;

  case 405:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12000 "Parser/parser.cc"
    break;

  case 406:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 12006 "Parser/parser.cc"
    break;

  case 407:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 12012 "Parser/parser.cc"
    break;

  case 408:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 12018 "Parser/parser.cc"
    break;

  case 409:
#line 1918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12024 "Parser/parser.cc"
    break;

  case 412:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12030 "Parser/parser.cc"
    break;

  case 413:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 12036 "Parser/parser.cc"
    break;

  case 414:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 12045 "Parser/parser.cc"
    break;

  case 415:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12051 "Parser/parser.cc"
    break;

  case 416:
#line 1942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 12057 "Parser/parser.cc"
    break;

  case 417:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12063 "Parser/parser.cc"
    break;

  case 418:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 12072 "Parser/parser.cc"
    break;

  case 419:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 12081 "Parser/parser.cc"
    break;

  case 420:
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12087 "Parser/parser.cc"
    break;

  case 423:
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12093 "Parser/parser.cc"
    break;

  case 424:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12099 "Parser/parser.cc"
    break;

  case 426:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12105 "Parser/parser.cc"
    break;

  case 427:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 12111 "Parser/parser.cc"
    break;

  case 437:
#line 2010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 12117 "Parser/parser.cc"
    break;

  case 438:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 12123 "Parser/parser.cc"
    break;

  case 442:
#line 2030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12129 "Parser/parser.cc"
    break;

  case 444:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 12135 "Parser/parser.cc"
    break;

  case 445:
#line 2040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12141 "Parser/parser.cc"
    break;

  case 446:
#line 2042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12147 "Parser/parser.cc"
    break;

  case 447:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12153 "Parser/parser.cc"
    break;

  case 448:
#line 2051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12159 "Parser/parser.cc"
    break;

  case 449:
#line 2053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12165 "Parser/parser.cc"
    break;

  case 450:
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12171 "Parser/parser.cc"
    break;

  case 451:
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12177 "Parser/parser.cc"
    break;

  case 452:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12183 "Parser/parser.cc"
    break;

  case 454:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12189 "Parser/parser.cc"
    break;

  case 455:
#line 2073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12195 "Parser/parser.cc"
    break;

  case 456:
#line 2075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12201 "Parser/parser.cc"
    break;

  case 457:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 12212 "Parser/parser.cc"
    break;

  case 458:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12218 "Parser/parser.cc"
    break;

  case 459:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12224 "Parser/parser.cc"
    break;

  case 460:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12230 "Parser/parser.cc"
    break;

  case 461:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12236 "Parser/parser.cc"
    break;

  case 462:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 12242 "Parser/parser.cc"
    break;

  case 463:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) ); }
#line 12248 "Parser/parser.cc"
    break;

  case 464:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 12257 "Parser/parser.cc"
    break;

  case 465:
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 12266 "Parser/parser.cc"
    break;

  case 466:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 12275 "Parser/parser.cc"
    break;

  case 467:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 12286 "Parser/parser.cc"
    break;

  case 468:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 12295 "Parser/parser.cc"
    break;

  case 469:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12301 "Parser/parser.cc"
    break;

  case 470:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12307 "Parser/parser.cc"
    break;

  case 471:
#line 2154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12313 "Parser/parser.cc"
    break;

  case 472:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 12321 "Parser/parser.cc"
    break;

  case 473:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 12329 "Parser/parser.cc"
    break;

  case 474:
#line 2171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 12335 "Parser/parser.cc"
    break;

  case 477:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12350 "Parser/parser.cc"
    break;

  case 478:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12356 "Parser/parser.cc"
    break;

  case 479:
#line 2193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12362 "Parser/parser.cc"
    break;

  case 480:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 12368 "Parser/parser.cc"
    break;

  case 481:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 12374 "Parser/parser.cc"
    break;

  case 482:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12380 "Parser/parser.cc"
    break;

  case 488:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 12390 "Parser/parser.cc"
    break;

  case 501:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12396 "Parser/parser.cc"
    break;

  case 504:
#line 2269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12402 "Parser/parser.cc"
    break;

  case 505:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12408 "Parser/parser.cc"
    break;

  case 507:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 12414 "Parser/parser.cc"
    break;

  case 508:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 12420 "Parser/parser.cc"
    break;

  case 509:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 12426 "Parser/parser.cc"
    break;

  case 510:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 12432 "Parser/parser.cc"
    break;

  case 511:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 12438 "Parser/parser.cc"
    break;

  case 512:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12444 "Parser/parser.cc"
    break;

  case 514:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12450 "Parser/parser.cc"
    break;

  case 515:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12456 "Parser/parser.cc"
    break;

  case 517:
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12462 "Parser/parser.cc"
    break;

  case 518:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 12468 "Parser/parser.cc"
    break;

  case 519:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 12474 "Parser/parser.cc"
    break;

  case 520:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 12480 "Parser/parser.cc"
    break;

  case 521:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 12486 "Parser/parser.cc"
    break;

  case 522:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 12492 "Parser/parser.cc"
    break;

  case 523:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 12498 "Parser/parser.cc"
    break;

  case 524:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 12504 "Parser/parser.cc"
    break;

  case 525:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 12510 "Parser/parser.cc"
    break;

  case 526:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 12516 "Parser/parser.cc"
    break;

  case 527:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12522 "Parser/parser.cc"
    break;

  case 528:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 12528 "Parser/parser.cc"
    break;

  case 529:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 12534 "Parser/parser.cc"
    break;

  case 530:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 12540 "Parser/parser.cc"
    break;

  case 531:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 12546 "Parser/parser.cc"
    break;

  case 532:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 12552 "Parser/parser.cc"
    break;

  case 533:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 12558 "Parser/parser.cc"
    break;

  case 534:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 12564 "Parser/parser.cc"
    break;

  case 535:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 12570 "Parser/parser.cc"
    break;

  case 536:
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float80 ); }
#line 12576 "Parser/parser.cc"
    break;

  case 537:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 12582 "Parser/parser.cc"
    break;

  case 538:
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float16 ); }
#line 12588 "Parser/parser.cc"
    break;

  case 539:
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32 ); }
#line 12594 "Parser/parser.cc"
    break;

  case 540:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x ); }
#line 12600 "Parser/parser.cc"
    break;

  case 541:
#line 2376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64 ); }
#line 12606 "Parser/parser.cc"
    break;

  case 542:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x ); }
#line 12612 "Parser/parser.cc"
    break;

  case 543:
#line 2380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128 ); }
#line 12618 "Parser/parser.cc"
    break;

  case 544:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128x ); }
#line 12624 "Parser/parser.cc"
    break;

  case 545:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x4 ); }
#line 12630 "Parser/parser.cc"
    break;

  case 546:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x2 ); }
#line 12636 "Parser/parser.cc"
    break;

  case 547:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat32 ); }
#line 12642 "Parser/parser.cc"
    break;

  case 548:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat64 ); }
#line 12648 "Parser/parser.cc"
    break;

  case 549:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svbool ); }
#line 12654 "Parser/parser.cc"
    break;

  case 550:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12660 "Parser/parser.cc"
    break;

  case 551:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12666 "Parser/parser.cc"
    break;

  case 552:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12672 "Parser/parser.cc"
    break;

  case 553:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 12678 "Parser/parser.cc"
    break;

  case 554:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 12684 "Parser/parser.cc"
    break;

  case 555:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 12690 "Parser/parser.cc"
    break;

  case 556:
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 12696 "Parser/parser.cc"
    break;

  case 557:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 12702 "Parser/parser.cc"
    break;

  case 558:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 12708 "Parser/parser.cc"
    break;

  case 559:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 12714 "Parser/parser.cc"
    break;

  case 560:
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 12720 "Parser/parser.cc"
    break;

  case 562:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12726 "Parser/parser.cc"
    break;

  case 564:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 12732 "Parser/parser.cc"
    break;

  case 565:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12738 "Parser/parser.cc"
    break;

  case 566:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12744 "Parser/parser.cc"
    break;

  case 568:
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12750 "Parser/parser.cc"
    break;

  case 569:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12756 "Parser/parser.cc"
    break;

  case 570:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12762 "Parser/parser.cc"
    break;

  case 571:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 12768 "Parser/parser.cc"
    break;

  case 573:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12774 "Parser/parser.cc"
    break;

  case 575:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12780 "Parser/parser.cc"
    break;

  case 576:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12786 "Parser/parser.cc"
    break;

  case 577:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 12792 "Parser/parser.cc"
    break;

  case 578:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12798 "Parser/parser.cc"
    break;

  case 579:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 12804 "Parser/parser.cc"
    break;

  case 580:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 12810 "Parser/parser.cc"
    break;

  case 581:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 12816 "Parser/parser.cc"
    break;

  case 582:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 12822 "Parser/parser.cc"
    break;

  case 583:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 12828 "Parser/parser.cc"
    break;

  case 585:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12834 "Parser/parser.cc"
    break;

  case 586:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12840 "Parser/parser.cc"
    break;

  case 587:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12846 "Parser/parser.cc"
    break;

  case 589:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 12852 "Parser/parser.cc"
    break;

  case 590:
#line 2497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12858 "Parser/parser.cc"
    break;

  case 591:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 12867 "Parser/parser.cc"
    break;

  case 593:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12873 "Parser/parser.cc"
    break;

  case 594:
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12879 "Parser/parser.cc"
    break;

  case 595:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12885 "Parser/parser.cc"
    break;

  case 597:
#line 2518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12891 "Parser/parser.cc"
    break;

  case 598:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12897 "Parser/parser.cc"
    break;

  case 600:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12903 "Parser/parser.cc"
    break;

  case 601:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12909 "Parser/parser.cc"
    break;

  case 602:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12915 "Parser/parser.cc"
    break;

  case 603:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12921 "Parser/parser.cc"
    break;

  case 604:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12927 "Parser/parser.cc"
    break;

  case 605:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12933 "Parser/parser.cc"
    break;

  case 606:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 12939 "Parser/parser.cc"
    break;

  case 607:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 12945 "Parser/parser.cc"
    break;

  case 608:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 12951 "Parser/parser.cc"
    break;

  case 610:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 12957 "Parser/parser.cc"
    break;

  case 611:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 12963 "Parser/parser.cc"
    break;

  case 612:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 12969 "Parser/parser.cc"
    break;

  case 613:
#line 2560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 12975 "Parser/parser.cc"
    break;

  case 614:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12981 "Parser/parser.cc"
    break;

  case 619:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 12987 "Parser/parser.cc"
    break;

  case 620:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12993 "Parser/parser.cc"
    break;

  case 621:
#line 2583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 13002 "Parser/parser.cc"
    break;

  case 622:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 13010 "Parser/parser.cc"
    break;

  case 623:
#line 2592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 13019 "Parser/parser.cc"
    break;

  case 624:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 13028 "Parser/parser.cc"
    break;

  case 625:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 13037 "Parser/parser.cc"
    break;

  case 626:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 13046 "Parser/parser.cc"
    break;

  case 628:
#line 2616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13052 "Parser/parser.cc"
    break;

  case 629:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13058 "Parser/parser.cc"
    break;

  case 630:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13068 "Parser/parser.cc"
    break;

  case 631:
#line 2629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 13087 "Parser/parser.cc"
    break;

  case 634:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 13093 "Parser/parser.cc"
    break;

  case 635:
#line 2654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 13099 "Parser/parser.cc"
    break;

  case 636:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 13105 "Parser/parser.cc"
    break;

  case 637:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 13111 "Parser/parser.cc"
    break;

  case 638:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 13117 "Parser/parser.cc"
    break;

  case 639:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 13123 "Parser/parser.cc"
    break;

  case 640:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13132 "Parser/parser.cc"
    break;

  case 641:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 13138 "Parser/parser.cc"
    break;

  case 642:
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13147 "Parser/parser.cc"
    break;

  case 643:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 13153 "Parser/parser.cc"
    break;

  case 644:
#line 2681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13162 "Parser/parser.cc"
    break;

  case 645:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13168 "Parser/parser.cc"
    break;

  case 646:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 13174 "Parser/parser.cc"
    break;

  case 647:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
		}
#line 13182 "Parser/parser.cc"
    break;

  case 648:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 13191 "Parser/parser.cc"
    break;

  case 649:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 13197 "Parser/parser.cc"
    break;

  case 650:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13203 "Parser/parser.cc"
    break;

  case 651:
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 13216 "Parser/parser.cc"
    break;

  case 652:
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13222 "Parser/parser.cc"
    break;

  case 655:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 13228 "Parser/parser.cc"
    break;

  case 656:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13234 "Parser/parser.cc"
    break;

  case 659:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13240 "Parser/parser.cc"
    break;

  case 662:
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13246 "Parser/parser.cc"
    break;

  case 663:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 13252 "Parser/parser.cc"
    break;

  case 664:
#line 2746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13258 "Parser/parser.cc"
    break;

  case 665:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13264 "Parser/parser.cc"
    break;

  case 666:
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13270 "Parser/parser.cc"
    break;

  case 667:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13276 "Parser/parser.cc"
    break;

  case 669:
#line 2760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13282 "Parser/parser.cc"
    break;

  case 671:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 13288 "Parser/parser.cc"
    break;

  case 672:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13294 "Parser/parser.cc"
    break;

  case 674:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 13300 "Parser/parser.cc"
    break;

  case 675:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13306 "Parser/parser.cc"
    break;

  case 677:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13312 "Parser/parser.cc"
    break;

  case 678:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 13323 "Parser/parser.cc"
    break;

  case 679:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 13337 "Parser/parser.cc"
    break;

  case 680:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 13343 "Parser/parser.cc"
    break;

  case 681:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 13349 "Parser/parser.cc"
    break;

  case 682:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 13355 "Parser/parser.cc"
    break;

  case 683:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 13366 "Parser/parser.cc"
    break;

  case 684:
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 13372 "Parser/parser.cc"
    break;

  case 685:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 13378 "Parser/parser.cc"
    break;

  case 687:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13384 "Parser/parser.cc"
    break;

  case 688:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13390 "Parser/parser.cc"
    break;

  case 689:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13396 "Parser/parser.cc"
    break;

  case 690:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 13402 "Parser/parser.cc"
    break;

  case 691:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13411 "Parser/parser.cc"
    break;

  case 692:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13420 "Parser/parser.cc"
    break;

  case 693:
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 13426 "Parser/parser.cc"
    break;

  case 694:
#line 2870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 13432 "Parser/parser.cc"
    break;

  case 695:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 13442 "Parser/parser.cc"
    break;

  case 696:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 13448 "Parser/parser.cc"
    break;

  case 697:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 13454 "Parser/parser.cc"
    break;

  case 699:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13460 "Parser/parser.cc"
    break;

  case 700:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13466 "Parser/parser.cc"
    break;

  case 701:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13472 "Parser/parser.cc"
    break;

  case 702:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13478 "Parser/parser.cc"
    break;

  case 703:
#line 2902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13484 "Parser/parser.cc"
    break;

  case 704:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13490 "Parser/parser.cc"
    break;

  case 706:
#line 2907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13496 "Parser/parser.cc"
    break;

  case 709:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13502 "Parser/parser.cc"
    break;

  case 710:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13508 "Parser/parser.cc"
    break;

  case 711:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13514 "Parser/parser.cc"
    break;

  case 712:
#line 2923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13520 "Parser/parser.cc"
    break;

  case 715:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13526 "Parser/parser.cc"
    break;

  case 716:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13532 "Parser/parser.cc"
    break;

  case 717:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13538 "Parser/parser.cc"
    break;

  case 719:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13544 "Parser/parser.cc"
    break;

  case 720:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13550 "Parser/parser.cc"
    break;

  case 721:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 13556 "Parser/parser.cc"
    break;

  case 723:
#line 2949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13562 "Parser/parser.cc"
    break;

  case 724:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13568 "Parser/parser.cc"
    break;

  case 725:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13574 "Parser/parser.cc"
    break;

  case 726:
#line 2965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13580 "Parser/parser.cc"
    break;

  case 727:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13586 "Parser/parser.cc"
    break;

  case 729:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13592 "Parser/parser.cc"
    break;

  case 730:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13598 "Parser/parser.cc"
    break;

  case 731:
#line 2978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13604 "Parser/parser.cc"
    break;

  case 736:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13610 "Parser/parser.cc"
    break;

  case 738:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13616 "Parser/parser.cc"
    break;

  case 739:
#line 3000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 13622 "Parser/parser.cc"
    break;

  case 742:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 13628 "Parser/parser.cc"
    break;

  case 745:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13634 "Parser/parser.cc"
    break;

  case 746:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 13640 "Parser/parser.cc"
    break;

  case 747:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 13646 "Parser/parser.cc"
    break;

  case 748:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13652 "Parser/parser.cc"
    break;

  case 749:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13658 "Parser/parser.cc"
    break;

  case 750:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13664 "Parser/parser.cc"
    break;

  case 751:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13670 "Parser/parser.cc"
    break;

  case 753:
#line 3032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 13676 "Parser/parser.cc"
    break;

  case 754:
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 13682 "Parser/parser.cc"
    break;

  case 755:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 13688 "Parser/parser.cc"
    break;

  case 757:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 13694 "Parser/parser.cc"
    break;

  case 759:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 13700 "Parser/parser.cc"
    break;

  case 760:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 13706 "Parser/parser.cc"
    break;

  case 761:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13712 "Parser/parser.cc"
    break;

  case 762:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13718 "Parser/parser.cc"
    break;

  case 763:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 13724 "Parser/parser.cc"
    break;

  case 764:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13730 "Parser/parser.cc"
    break;

  case 766:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13736 "Parser/parser.cc"
    break;

  case 767:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13742 "Parser/parser.cc"
    break;

  case 768:
#line 3100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13748 "Parser/parser.cc"
    break;

  case 769:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 13759 "Parser/parser.cc"
    break;

  case 770:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13765 "Parser/parser.cc"
    break;

  case 771:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 13771 "Parser/parser.cc"
    break;

  case 772:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13777 "Parser/parser.cc"
    break;

  case 773:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 13786 "Parser/parser.cc"
    break;

  case 774:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 13792 "Parser/parser.cc"
    break;

  case 775:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 13802 "Parser/parser.cc"
    break;

  case 776:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13808 "Parser/parser.cc"
    break;

  case 777:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13814 "Parser/parser.cc"
    break;

  case 778:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 13820 "Parser/parser.cc"
    break;

  case 779:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13826 "Parser/parser.cc"
    break;

  case 780:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13832 "Parser/parser.cc"
    break;

  case 781:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13838 "Parser/parser.cc"
    break;

  case 782:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 13844 "Parser/parser.cc"
    break;

  case 783:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13850 "Parser/parser.cc"
    break;

  case 784:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13856 "Parser/parser.cc"
    break;

  case 787:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13862 "Parser/parser.cc"
    break;

  case 788:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13868 "Parser/parser.cc"
    break;

  case 789:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13874 "Parser/parser.cc"
    break;

  case 790:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13880 "Parser/parser.cc"
    break;

  case 792:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13886 "Parser/parser.cc"
    break;

  case 793:
#line 3185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 13892 "Parser/parser.cc"
    break;

  case 794:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13898 "Parser/parser.cc"
    break;

  case 795:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13904 "Parser/parser.cc"
    break;

  case 796:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 13910 "Parser/parser.cc"
    break;

  case 797:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 13916 "Parser/parser.cc"
    break;

  case 798:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 13922 "Parser/parser.cc"
    break;

  case 799:
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 13931 "Parser/parser.cc"
    break;

  case 800:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 13940 "Parser/parser.cc"
    break;

  case 801:
#line 3219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 13949 "Parser/parser.cc"
    break;

  case 802:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 13955 "Parser/parser.cc"
    break;

  case 803:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-6].tok), (yyvsp[-4].decl), (yyvsp[-1].decl) );
		}
#line 13964 "Parser/parser.cc"
    break;

  case 804:
#line 3231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-3].tok), (yyvsp[-5].decl), (yyvsp[-1].decl) ); }
#line 13970 "Parser/parser.cc"
    break;

  case 806:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13976 "Parser/parser.cc"
    break;

  case 811:
#line 3249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13982 "Parser/parser.cc"
    break;

  case 812:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 13988 "Parser/parser.cc"
    break;

  case 813:
#line 3257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 13994 "Parser/parser.cc"
    break;

  case 814:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Possible cause is declaring an aggregate or enumeration type in a trait." ); (yyval.decl) = nullptr; }
#line 14000 "Parser/parser.cc"
    break;

  case 816:
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 14006 "Parser/parser.cc"
    break;

  case 817:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14012 "Parser/parser.cc"
    break;

  case 818:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 14018 "Parser/parser.cc"
    break;

  case 819:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14024 "Parser/parser.cc"
    break;

  case 821:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 14030 "Parser/parser.cc"
    break;

  case 822:
#line 3288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 14036 "Parser/parser.cc"
    break;

  case 823:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 14042 "Parser/parser.cc"
    break;

  case 824:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 14058 "Parser/parser.cc"
    break;

  case 825:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 14064 "Parser/parser.cc"
    break;

  case 826:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 14070 "Parser/parser.cc"
    break;

  case 827:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 14076 "Parser/parser.cc"
    break;

  case 828:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14082 "Parser/parser.cc"
    break;

  case 829:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14088 "Parser/parser.cc"
    break;

  case 830:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14094 "Parser/parser.cc"
    break;

  case 832:
#line 3320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 14103 "Parser/parser.cc"
    break;

  case 833:
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 14109 "Parser/parser.cc"
    break;

  case 834:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 14118 "Parser/parser.cc"
    break;

  case 835:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 14128 "Parser/parser.cc"
    break;

  case 836:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 14137 "Parser/parser.cc"
    break;

  case 837:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14147 "Parser/parser.cc"
    break;

  case 838:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 14158 "Parser/parser.cc"
    break;

  case 839:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14168 "Parser/parser.cc"
    break;

  case 840:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 14179 "Parser/parser.cc"
    break;

  case 841:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14189 "Parser/parser.cc"
    break;

  case 842:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 14200 "Parser/parser.cc"
    break;

  case 843:
#line 3383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14210 "Parser/parser.cc"
    break;

  case 844:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14216 "Parser/parser.cc"
    break;

  case 846:
#line 3400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14222 "Parser/parser.cc"
    break;

  case 847:
#line 3402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14228 "Parser/parser.cc"
    break;

  case 848:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 14234 "Parser/parser.cc"
    break;

  case 849:
#line 3409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 14246 "Parser/parser.cc"
    break;

  case 850:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14257 "Parser/parser.cc"
    break;

  case 851:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14266 "Parser/parser.cc"
    break;

  case 852:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14275 "Parser/parser.cc"
    break;

  case 853:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14281 "Parser/parser.cc"
    break;

  case 854:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14287 "Parser/parser.cc"
    break;

  case 855:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14293 "Parser/parser.cc"
    break;

  case 856:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 14302 "Parser/parser.cc"
    break;

  case 857:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14308 "Parser/parser.cc"
    break;

  case 858:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14314 "Parser/parser.cc"
    break;

  case 859:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 14320 "Parser/parser.cc"
    break;

  case 864:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 14326 "Parser/parser.cc"
    break;

  case 865:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14332 "Parser/parser.cc"
    break;

  case 866:
#line 3481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 14342 "Parser/parser.cc"
    break;

  case 867:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14348 "Parser/parser.cc"
    break;

  case 870:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14354 "Parser/parser.cc"
    break;

  case 871:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 14360 "Parser/parser.cc"
    break;

  case 872:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14366 "Parser/parser.cc"
    break;

  case 873:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14372 "Parser/parser.cc"
    break;

  case 874:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14378 "Parser/parser.cc"
    break;

  case 876:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14384 "Parser/parser.cc"
    break;

  case 877:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14390 "Parser/parser.cc"
    break;

  case 878:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14396 "Parser/parser.cc"
    break;

  case 879:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 14402 "Parser/parser.cc"
    break;

  case 881:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 14408 "Parser/parser.cc"
    break;

  case 882:
#line 3533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 14414 "Parser/parser.cc"
    break;

  case 883:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14420 "Parser/parser.cc"
    break;

  case 884:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14426 "Parser/parser.cc"
    break;

  case 885:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14432 "Parser/parser.cc"
    break;

  case 886:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14438 "Parser/parser.cc"
    break;

  case 888:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14444 "Parser/parser.cc"
    break;

  case 889:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14450 "Parser/parser.cc"
    break;

  case 890:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14456 "Parser/parser.cc"
    break;

  case 891:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14462 "Parser/parser.cc"
    break;

  case 892:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14468 "Parser/parser.cc"
    break;

  case 893:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14474 "Parser/parser.cc"
    break;

  case 894:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14480 "Parser/parser.cc"
    break;

  case 895:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14486 "Parser/parser.cc"
    break;

  case 896:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14492 "Parser/parser.cc"
    break;

  case 897:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14498 "Parser/parser.cc"
    break;

  case 898:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14504 "Parser/parser.cc"
    break;

  case 899:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14510 "Parser/parser.cc"
    break;

  case 900:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14516 "Parser/parser.cc"
    break;

  case 901:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14522 "Parser/parser.cc"
    break;

  case 902:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14528 "Parser/parser.cc"
    break;

  case 903:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14534 "Parser/parser.cc"
    break;

  case 904:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14540 "Parser/parser.cc"
    break;

  case 905:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14546 "Parser/parser.cc"
    break;

  case 907:
#line 3634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14552 "Parser/parser.cc"
    break;

  case 908:
#line 3639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14558 "Parser/parser.cc"
    break;

  case 909:
#line 3641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14564 "Parser/parser.cc"
    break;

  case 910:
#line 3643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14570 "Parser/parser.cc"
    break;

  case 911:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14576 "Parser/parser.cc"
    break;

  case 912:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14582 "Parser/parser.cc"
    break;

  case 913:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14588 "Parser/parser.cc"
    break;

  case 914:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14594 "Parser/parser.cc"
    break;

  case 915:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14600 "Parser/parser.cc"
    break;

  case 916:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14606 "Parser/parser.cc"
    break;

  case 917:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14612 "Parser/parser.cc"
    break;

  case 918:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14618 "Parser/parser.cc"
    break;

  case 919:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14624 "Parser/parser.cc"
    break;

  case 920:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14630 "Parser/parser.cc"
    break;

  case 921:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14636 "Parser/parser.cc"
    break;

  case 922:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14642 "Parser/parser.cc"
    break;

  case 926:
#line 3691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 14648 "Parser/parser.cc"
    break;

  case 927:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14654 "Parser/parser.cc"
    break;

  case 928:
#line 3695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14660 "Parser/parser.cc"
    break;

  case 929:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14666 "Parser/parser.cc"
    break;

  case 930:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14672 "Parser/parser.cc"
    break;

  case 931:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14678 "Parser/parser.cc"
    break;

  case 932:
#line 3706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14684 "Parser/parser.cc"
    break;

  case 933:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14690 "Parser/parser.cc"
    break;

  case 934:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14696 "Parser/parser.cc"
    break;

  case 935:
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14702 "Parser/parser.cc"
    break;

  case 936:
#line 3717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14708 "Parser/parser.cc"
    break;

  case 937:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14714 "Parser/parser.cc"
    break;

  case 938:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14720 "Parser/parser.cc"
    break;

  case 939:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14726 "Parser/parser.cc"
    break;

  case 940:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14732 "Parser/parser.cc"
    break;

  case 941:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 14741 "Parser/parser.cc"
    break;

  case 942:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14747 "Parser/parser.cc"
    break;

  case 943:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14753 "Parser/parser.cc"
    break;

  case 945:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14759 "Parser/parser.cc"
    break;

  case 946:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14765 "Parser/parser.cc"
    break;

  case 947:
#line 3757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14771 "Parser/parser.cc"
    break;

  case 948:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14777 "Parser/parser.cc"
    break;

  case 949:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14783 "Parser/parser.cc"
    break;

  case 950:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14789 "Parser/parser.cc"
    break;

  case 951:
#line 3768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14795 "Parser/parser.cc"
    break;

  case 952:
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14801 "Parser/parser.cc"
    break;

  case 953:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14807 "Parser/parser.cc"
    break;

  case 954:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14813 "Parser/parser.cc"
    break;

  case 955:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14819 "Parser/parser.cc"
    break;

  case 956:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14825 "Parser/parser.cc"
    break;

  case 957:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14831 "Parser/parser.cc"
    break;

  case 958:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14837 "Parser/parser.cc"
    break;

  case 959:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14843 "Parser/parser.cc"
    break;

  case 960:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14849 "Parser/parser.cc"
    break;

  case 961:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14855 "Parser/parser.cc"
    break;

  case 962:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14861 "Parser/parser.cc"
    break;

  case 964:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14867 "Parser/parser.cc"
    break;

  case 965:
#line 3808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14873 "Parser/parser.cc"
    break;

  case 966:
#line 3810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14879 "Parser/parser.cc"
    break;

  case 967:
#line 3812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14885 "Parser/parser.cc"
    break;

  case 968:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14891 "Parser/parser.cc"
    break;

  case 969:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14897 "Parser/parser.cc"
    break;

  case 970:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14903 "Parser/parser.cc"
    break;

  case 971:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14909 "Parser/parser.cc"
    break;

  case 972:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14915 "Parser/parser.cc"
    break;

  case 973:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14921 "Parser/parser.cc"
    break;

  case 974:
#line 3832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14927 "Parser/parser.cc"
    break;

  case 975:
#line 3834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14933 "Parser/parser.cc"
    break;

  case 976:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14939 "Parser/parser.cc"
    break;

  case 977:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14945 "Parser/parser.cc"
    break;

  case 978:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14951 "Parser/parser.cc"
    break;

  case 979:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14957 "Parser/parser.cc"
    break;

  case 980:
#line 3852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14963 "Parser/parser.cc"
    break;

  case 981:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14970 "Parser/parser.cc"
    break;

  case 983:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14976 "Parser/parser.cc"
    break;

  case 984:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14982 "Parser/parser.cc"
    break;

  case 985:
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14988 "Parser/parser.cc"
    break;

  case 986:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14994 "Parser/parser.cc"
    break;

  case 987:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15000 "Parser/parser.cc"
    break;

  case 988:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15006 "Parser/parser.cc"
    break;

  case 989:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15012 "Parser/parser.cc"
    break;

  case 990:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15018 "Parser/parser.cc"
    break;

  case 991:
#line 3880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15024 "Parser/parser.cc"
    break;

  case 992:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15030 "Parser/parser.cc"
    break;

  case 993:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15036 "Parser/parser.cc"
    break;

  case 994:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15042 "Parser/parser.cc"
    break;

  case 995:
#line 3903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15048 "Parser/parser.cc"
    break;

  case 996:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15055 "Parser/parser.cc"
    break;

  case 998:
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15061 "Parser/parser.cc"
    break;

  case 999:
#line 3911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15067 "Parser/parser.cc"
    break;

  case 1000:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 15073 "Parser/parser.cc"
    break;

  case 1001:
#line 3918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 15079 "Parser/parser.cc"
    break;

  case 1002:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15085 "Parser/parser.cc"
    break;

  case 1003:
#line 3925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15091 "Parser/parser.cc"
    break;

  case 1004:
#line 3927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15097 "Parser/parser.cc"
    break;

  case 1005:
#line 3932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15103 "Parser/parser.cc"
    break;

  case 1006:
#line 3934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15109 "Parser/parser.cc"
    break;

  case 1007:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15115 "Parser/parser.cc"
    break;

  case 1008:
#line 3941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15121 "Parser/parser.cc"
    break;

  case 1010:
#line 3959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15127 "Parser/parser.cc"
    break;

  case 1011:
#line 3961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15133 "Parser/parser.cc"
    break;

  case 1012:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 15139 "Parser/parser.cc"
    break;

  case 1013:
#line 3968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15145 "Parser/parser.cc"
    break;

  case 1014:
#line 3970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15151 "Parser/parser.cc"
    break;

  case 1015:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15157 "Parser/parser.cc"
    break;

  case 1016:
#line 3974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15163 "Parser/parser.cc"
    break;

  case 1018:
#line 3980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15169 "Parser/parser.cc"
    break;

  case 1019:
#line 3982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15175 "Parser/parser.cc"
    break;

  case 1020:
#line 3984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15181 "Parser/parser.cc"
    break;

  case 1021:
#line 3989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15187 "Parser/parser.cc"
    break;

  case 1022:
#line 3991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15193 "Parser/parser.cc"
    break;

  case 1023:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15199 "Parser/parser.cc"
    break;

  case 1024:
#line 3999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15205 "Parser/parser.cc"
    break;

  case 1025:
#line 4001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 15211 "Parser/parser.cc"
    break;

  case 1026:
#line 4004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-3].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 15217 "Parser/parser.cc"
    break;

  case 1027:
#line 4011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15223 "Parser/parser.cc"
    break;

  case 1029:
#line 4022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 15229 "Parser/parser.cc"
    break;

  case 1030:
#line 4024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 15235 "Parser/parser.cc"
    break;

  case 1032:
#line 4027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 15241 "Parser/parser.cc"
    break;

  case 1033:
#line 4029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 15247 "Parser/parser.cc"
    break;

  case 1035:
#line 4035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 15253 "Parser/parser.cc"
    break;

  case 1036:
#line 4037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 15259 "Parser/parser.cc"
    break;

  case 1037:
#line 4042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15265 "Parser/parser.cc"
    break;

  case 1038:
#line 4044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 15271 "Parser/parser.cc"
    break;

  case 1039:
#line 4046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 15277 "Parser/parser.cc"
    break;

  case 1040:
#line 4048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 15283 "Parser/parser.cc"
    break;

  case 1041:
#line 4082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 15289 "Parser/parser.cc"
    break;

  case 1044:
#line 4089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 15296 "Parser/parser.cc"
    break;

  case 1045:
#line 4092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15302 "Parser/parser.cc"
    break;

  case 1046:
#line 4094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15308 "Parser/parser.cc"
    break;

  case 1047:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 15314 "Parser/parser.cc"
    break;

  case 1048:
#line 4101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15320 "Parser/parser.cc"
    break;

  case 1049:
#line 4103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15326 "Parser/parser.cc"
    break;

  case 1050:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15332 "Parser/parser.cc"
    break;

  case 1051:
#line 4107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15338 "Parser/parser.cc"
    break;

  case 1053:
#line 4113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15344 "Parser/parser.cc"
    break;

  case 1054:
#line 4115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15350 "Parser/parser.cc"
    break;

  case 1055:
#line 4117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15356 "Parser/parser.cc"
    break;

  case 1056:
#line 4122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15362 "Parser/parser.cc"
    break;

  case 1057:
#line 4124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15368 "Parser/parser.cc"
    break;

  case 1058:
#line 4126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15374 "Parser/parser.cc"
    break;

  case 1060:
#line 4133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15380 "Parser/parser.cc"
    break;

  case 1062:
#line 4144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15386 "Parser/parser.cc"
    break;

  case 1063:
#line 4147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 15392 "Parser/parser.cc"
    break;

  case 1064:
#line 4149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 15398 "Parser/parser.cc"
    break;

  case 1065:
#line 4152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 15404 "Parser/parser.cc"
    break;

  case 1066:
#line 4154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 15410 "Parser/parser.cc"
    break;

  case 1067:
#line 4156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 15416 "Parser/parser.cc"
    break;

  case 1069:
#line 4171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15422 "Parser/parser.cc"
    break;

  case 1070:
#line 4173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15428 "Parser/parser.cc"
    break;

  case 1071:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 15434 "Parser/parser.cc"
    break;

  case 1072:
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15440 "Parser/parser.cc"
    break;

  case 1073:
#line 4182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15446 "Parser/parser.cc"
    break;

  case 1074:
#line 4184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15452 "Parser/parser.cc"
    break;

  case 1075:
#line 4186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15458 "Parser/parser.cc"
    break;

  case 1077:
#line 4192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15464 "Parser/parser.cc"
    break;

  case 1078:
#line 4194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15470 "Parser/parser.cc"
    break;

  case 1079:
#line 4196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15476 "Parser/parser.cc"
    break;

  case 1080:
#line 4201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15482 "Parser/parser.cc"
    break;

  case 1081:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15488 "Parser/parser.cc"
    break;

  case 1084:
#line 4213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15494 "Parser/parser.cc"
    break;

  case 1087:
#line 4220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15500 "Parser/parser.cc"
    break;

  case 1088:
#line 4226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15506 "Parser/parser.cc"
    break;

  case 1089:
#line 4228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15512 "Parser/parser.cc"
    break;

  case 1090:
#line 4230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15518 "Parser/parser.cc"
    break;

  case 1091:
#line 4232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15524 "Parser/parser.cc"
    break;

  case 1092:
#line 4234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15530 "Parser/parser.cc"
    break;

  case 1093:
#line 4236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15536 "Parser/parser.cc"
    break;

  case 1094:
#line 4243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15542 "Parser/parser.cc"
    break;

  case 1095:
#line 4245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15548 "Parser/parser.cc"
    break;

  case 1096:
#line 4247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15554 "Parser/parser.cc"
    break;

  case 1097:
#line 4249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15560 "Parser/parser.cc"
    break;

  case 1098:
#line 4251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15566 "Parser/parser.cc"
    break;

  case 1099:
#line 4253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15572 "Parser/parser.cc"
    break;

  case 1100:
#line 4255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15578 "Parser/parser.cc"
    break;

  case 1101:
#line 4257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15584 "Parser/parser.cc"
    break;

  case 1102:
#line 4259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15590 "Parser/parser.cc"
    break;

  case 1103:
#line 4261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15596 "Parser/parser.cc"
    break;

  case 1104:
#line 4264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15602 "Parser/parser.cc"
    break;

  case 1105:
#line 4266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15608 "Parser/parser.cc"
    break;

  case 1106:
#line 4268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15614 "Parser/parser.cc"
    break;

  case 1107:
#line 4270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15620 "Parser/parser.cc"
    break;

  case 1108:
#line 4272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15626 "Parser/parser.cc"
    break;

  case 1109:
#line 4277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-2].decl) ); }
#line 15632 "Parser/parser.cc"
    break;

  case 1110:
#line 4279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), false ); }
#line 15638 "Parser/parser.cc"
    break;

  case 1111:
#line 4284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), true ); }
#line 15644 "Parser/parser.cc"
    break;

  case 1112:
#line 4286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) ), true ); }
#line 15650 "Parser/parser.cc"
    break;

  case 1114:
#line 4313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15656 "Parser/parser.cc"
    break;

  case 1118:
#line 4324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15662 "Parser/parser.cc"
    break;

  case 1119:
#line 4326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15668 "Parser/parser.cc"
    break;

  case 1120:
#line 4328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15674 "Parser/parser.cc"
    break;

  case 1121:
#line 4330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15680 "Parser/parser.cc"
    break;

  case 1122:
#line 4332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15686 "Parser/parser.cc"
    break;

  case 1123:
#line 4334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15692 "Parser/parser.cc"
    break;

  case 1124:
#line 4341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15698 "Parser/parser.cc"
    break;

  case 1125:
#line 4343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15704 "Parser/parser.cc"
    break;

  case 1126:
#line 4345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15710 "Parser/parser.cc"
    break;

  case 1127:
#line 4347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15716 "Parser/parser.cc"
    break;

  case 1128:
#line 4349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15722 "Parser/parser.cc"
    break;

  case 1129:
#line 4351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15728 "Parser/parser.cc"
    break;

  case 1130:
#line 4356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 15734 "Parser/parser.cc"
    break;

  case 1131:
#line 4358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15740 "Parser/parser.cc"
    break;

  case 1132:
#line 4360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15746 "Parser/parser.cc"
    break;

  case 1133:
#line 4365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 15752 "Parser/parser.cc"
    break;

  case 1134:
#line 4367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15758 "Parser/parser.cc"
    break;

  case 1135:
#line 4369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15764 "Parser/parser.cc"
    break;

  case 1138:
#line 4393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 15770 "Parser/parser.cc"
    break;

  case 1139:
#line 4395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 15776 "Parser/parser.cc"
    break;


#line 15780 "Parser/parser.cc"

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
#line 4398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
