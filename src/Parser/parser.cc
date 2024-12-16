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
    C23_ATTRIBUTE = 384,
    FLOATING_DECIMALconstant = 385,
    FLOATING_FRACTIONconstant = 386,
    FLOATINGconstant = 387,
    ARROW = 388,
    ICR = 389,
    DECR = 390,
    LS = 391,
    RS = 392,
    LE = 393,
    GE = 394,
    EQ = 395,
    NE = 396,
    ANDAND = 397,
    OROR = 398,
    ATTR = 399,
    ELLIPSIS = 400,
    EXPassign = 401,
    MULTassign = 402,
    DIVassign = 403,
    MODassign = 404,
    PLUSassign = 405,
    MINUSassign = 406,
    LSassign = 407,
    RSassign = 408,
    ANDassign = 409,
    ERassign = 410,
    ORassign = 411,
    ErangeUp = 412,
    ErangeUpEq = 413,
    ErangeDown = 414,
    ErangeDownEq = 415,
    ATassign = 416,
    THEN = 417
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
#define C23_ATTRIBUTE 384
#define FLOATING_DECIMALconstant 385
#define FLOATING_FRACTIONconstant 386
#define FLOATINGconstant 387
#define ARROW 388
#define ICR 389
#define DECR 390
#define LS 391
#define RS 392
#define LE 393
#define GE 394
#define EQ 395
#define NE 396
#define ANDAND 397
#define OROR 398
#define ATTR 399
#define ELLIPSIS 400
#define EXPassign 401
#define MULTassign 402
#define DIVassign 403
#define MODassign 404
#define PLUSassign 405
#define MINUSassign 406
#define LSassign 407
#define RSassign 408
#define ANDassign 409
#define ERassign 410
#define ORassign 411
#define ErangeUp 412
#define ErangeUpEq 413
#define ErangeDown 414
#define ErangeDownEq 415
#define ATassign 416
#define THEN 417

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

#line 757 "Parser/parser.cc"

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
#define YYLAST   33752

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  190
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  314
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1141
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2189

#define YYUNDEFTOK  2
#define YYMAXUTOK   417


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
       2,     2,     2,   179,     2,     2,     2,   183,   176,     2,
     164,   166,   175,   177,   170,   178,   167,   182,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   171,   189,
     184,   188,   185,   187,   165,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   168,   181,   169,   174,     2,   173,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   172,   186,   163,   180,     2,     2,     2,
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
     155,   156,   157,   158,   159,   160,   161,   162
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
    1728,  1729,  1733,  1739,  1747,  1752,  1753,  1757,  1758,  1763,
    1767,  1768,  1771,  1773,  1778,  1781,  1783,  1785,  1788,  1790,
    1795,  1800,  1801,  1805,  1810,  1812,  1817,  1819,  1824,  1826,
    1828,  1833,  1838,  1843,  1848,  1850,  1852,  1857,  1859,  1865,
    1866,  1870,  1871,  1872,  1873,  1877,  1882,  1883,  1885,  1887,
    1889,  1893,  1897,  1898,  1902,  1904,  1906,  1908,  1910,  1916,
    1917,  1923,  1924,  1928,  1929,  1934,  1936,  1945,  1946,  1948,
    1953,  1958,  1969,  1970,  1974,  1975,  1981,  1982,  1986,  1988,
    1992,  1994,  1998,  1999,  2003,  2004,  2008,  2009,  2010,  2014,
    2016,  2031,  2032,  2033,  2034,  2036,  2040,  2042,  2046,  2053,
    2055,  2057,  2059,  2067,  2069,  2074,  2075,  2077,  2079,  2081,
    2091,  2093,  2105,  2108,  2113,  2115,  2121,  2126,  2131,  2142,
    2149,  2154,  2156,  2158,  2164,  2168,  2175,  2177,  2178,  2179,
    2195,  2197,  2200,  2202,  2205,  2210,  2211,  2215,  2216,  2217,
    2218,  2227,  2228,  2229,  2238,  2239,  2240,  2244,  2245,  2246,
    2255,  2256,  2257,  2262,  2263,  2272,  2273,  2278,  2280,  2284,
    2286,  2288,  2290,  2297,  2302,  2307,  2308,  2310,  2320,  2321,
    2326,  2328,  2330,  2332,  2334,  2336,  2339,  2341,  2343,  2348,
    2354,  2356,  2358,  2360,  2362,  2364,  2366,  2368,  2370,  2372,
    2374,  2376,  2378,  2380,  2382,  2384,  2387,  2389,  2391,  2393,
    2395,  2397,  2399,  2401,  2403,  2405,  2407,  2409,  2411,  2413,
    2415,  2417,  2419,  2421,  2426,  2427,  2431,  2437,  2438,  2444,
    2445,  2447,  2449,  2451,  2456,  2458,  2463,  2464,  2466,  2468,
    2473,  2475,  2477,  2479,  2481,  2483,  2488,  2489,  2491,  2493,
    2498,  2500,  2499,  2503,  2511,  2512,  2514,  2516,  2521,  2522,
    2524,  2529,  2530,  2532,  2534,  2539,  2541,  2543,  2548,  2550,
    2552,  2554,  2555,  2557,  2562,  2564,  2566,  2571,  2572,  2576,
    2577,  2584,  2583,  2588,  2587,  2597,  2596,  2607,  2606,  2616,
    2621,  2622,  2627,  2633,  2651,  2652,  2656,  2658,  2660,  2665,
    2667,  2669,  2671,  2676,  2678,  2683,  2685,  2694,  2695,  2700,
    2704,  2709,  2711,  2713,  2722,  2724,  2725,  2726,  2728,  2730,
    2731,  2736,  2737,  2741,  2742,  2747,  2749,  2752,  2755,  2762,
    2763,  2764,  2770,  2775,  2777,  2783,  2784,  2790,  2791,  2795,
    2803,  2810,  2823,  2822,  2826,  2829,  2828,  2837,  2841,  2845,
    2847,  2853,  2854,  2859,  2864,  2873,  2874,  2876,  2882,  2884,
    2889,  2890,  2896,  2897,  2898,  2907,  2908,  2910,  2911,  2916,
    2917,  2918,  2920,  2926,  2927,  2929,  2930,  2931,  2933,  2935,
    2942,  2943,  2945,  2947,  2952,  2953,  2962,  2964,  2969,  2971,
    2976,  2977,  2979,  2982,  2984,  2988,  2989,  2990,  2992,  2994,
    3002,  3004,  3009,  3010,  3011,  3016,  3017,  3022,  3023,  3024,
    3025,  3029,  3030,  3035,  3036,  3037,  3038,  3039,  3053,  3054,
    3059,  3060,  3065,  3067,  3069,  3071,  3073,  3096,  3097,  3103,
    3104,  3110,  3109,  3119,  3118,  3122,  3128,  3130,  3140,  3141,
    3143,  3147,  3152,  3154,  3156,  3158,  3164,  3165,  3169,  3170,
    3175,  3177,  3184,  3186,  3187,  3189,  3194,  3196,  3198,  3203,
    3205,  3210,  3215,  3223,  3228,  3230,  3235,  3240,  3241,  3246,
    3247,  3251,  3252,  3253,  3259,  3261,  3263,  3269,  3271,  3276,
    3278,  3284,  3285,  3289,  3293,  3297,  3299,  3311,  3313,  3315,
    3317,  3319,  3321,  3323,  3324,  3329,  3332,  3331,  3343,  3342,
    3355,  3354,  3368,  3367,  3381,  3380,  3393,  3398,  3404,  3406,
    3412,  3413,  3424,  3431,  3436,  3442,  3445,  3448,  3452,  3458,
    3461,  3464,  3469,  3470,  3471,  3472,  3476,  3484,  3485,  3497,
    3498,  3502,  3503,  3508,  3510,  3512,  3514,  3519,  3520,  3526,
    3527,  3529,  3534,  3535,  3537,  3572,  3574,  3577,  3582,  3584,
    3585,  3587,  3592,  3594,  3596,  3598,  3603,  3605,  3607,  3609,
    3611,  3613,  3615,  3620,  3622,  3624,  3626,  3635,  3637,  3638,
    3643,  3645,  3647,  3649,  3651,  3656,  3658,  3660,  3662,  3667,
    3669,  3671,  3673,  3675,  3677,  3689,  3690,  3691,  3695,  3697,
    3699,  3701,  3703,  3708,  3710,  3712,  3714,  3719,  3721,  3723,
    3725,  3727,  3729,  3741,  3746,  3751,  3753,  3754,  3756,  3761,
    3763,  3765,  3767,  3772,  3774,  3776,  3778,  3780,  3782,  3784,
    3789,  3791,  3793,  3795,  3804,  3806,  3807,  3812,  3814,  3816,
    3818,  3820,  3825,  3827,  3829,  3831,  3836,  3838,  3840,  3842,
    3844,  3846,  3856,  3858,  3861,  3862,  3864,  3869,  3871,  3873,
    3878,  3880,  3882,  3884,  3889,  3891,  3893,  3907,  3909,  3912,
    3913,  3915,  3920,  3922,  3927,  3929,  3931,  3936,  3938,  3943,
    3945,  3962,  3963,  3965,  3970,  3972,  3974,  3976,  3978,  3983,
    3984,  3986,  3988,  3993,  3995,  3997,  4003,  4005,  4008,  4015,
    4017,  4026,  4028,  4030,  4031,  4033,  4035,  4039,  4041,  4046,
    4048,  4050,  4052,  4087,  4088,  4092,  4093,  4096,  4098,  4103,
    4105,  4107,  4109,  4111,  4116,  4117,  4119,  4121,  4126,  4128,
    4130,  4136,  4137,  4139,  4148,  4151,  4153,  4156,  4158,  4160,
    4174,  4175,  4177,  4182,  4184,  4186,  4188,  4190,  4195,  4196,
    4198,  4200,  4205,  4207,  4215,  4216,  4217,  4222,  4223,  4224,
    4230,  4232,  4234,  4236,  4238,  4240,  4247,  4249,  4251,  4253,
    4255,  4257,  4259,  4261,  4263,  4265,  4268,  4270,  4272,  4274,
    4276,  4281,  4283,  4285,  4290,  4316,  4317,  4319,  4323,  4324,
    4328,  4330,  4332,  4334,  4336,  4338,  4345,  4347,  4349,  4351,
    4353,  4355,  4360,  4362,  4364,  4369,  4371,  4373,  4391,  4393,
    4398,  4399
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
     415,   416,   417,   125,    40,    64,    41,    46,    91,    93,
      44,    58,   123,    96,    94,    42,    38,    43,    45,    33,
     126,    92,    47,    37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1896)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1140)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const int yypact[] =
{
     191, 13018,   194,   252, 25211,   139, -1896, -1896, -1896, -1896,
   -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896,   115,   898,
     133, -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896,
   -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896,
   -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896,
   -1896, -1896, -1896, -1896,   455,   373, -1896, -1896, -1896, -1896,
   -1896, -1896,  5259,  5259,   180, 13018,   249,   263, 11926, -1896,
     269, -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896,
   -1896, -1896,  2229,  4524, -1896,   762, 15544, -1896, -1896,  3024,
   -1896, -1896, -1896, -1896, 18807, -1896,   292,   325,   352,   200,
     411, -1896,  5681,   358,   378,   406,   400,  4857,   593,   854,
   13205, -1896, -1896,   533, 18641,  2462, -1896, -1896, -1896, -1896,
    3313,   615, 30727,  5417,  1620,  3313,  1796,   457, -1896, -1896,
   -1896, -1896,    69, -1896, -1896, -1896, -1896,   470, -1896, -1896,
   -1896, -1896, -1896,   532,   603,    69, -1896,    69, 22996, -1896,
   -1896, -1896, 26660,  5259, -1896, -1896,  5259, -1896, 13018, -1896,
     598, 26824, -1896, -1896,  5503, 29104, -1896, -1896,  1186,  1186,
     628,  2822, -1896, -1896, -1896, -1896,    81, 21032,    69,  3127,
      69, -1896, -1896, -1896, -1896, -1896, -1896,   644, -1896,   673,
     698,  1159, -1896,   758, 33104, -1896, -1896, -1896, -1896, -1896,
   -1896, -1896, 23492, -1896, -1896, -1896,   820, -1896,   734,  5158,
    4524,   543,   769,   788,   794,   805,   810,   812, -1896, -1896,
   33182,   817, 33260,   828,   831,    69, 33104, 33338,   857, 30402,
   -1896, -1896, -1896, -1896, -1896, -1896, -1896, 33416, 33416, 23324,
   14482, 25375,  5247,   815, -1896, -1896, -1896, -1896,   196, -1896,
     459,   887, -1896,  1934,  5746, 23996, 33104, -1896,   859,   691,
     983,  1045,   592,  1100,   858,   871,   864,   906,   -21, -1896,
     884, -1896, -1896,  5711,  5697,   922, 21724, 30050,  3313,  3313,
     929,  3313,  1809,  3313,  2006,   888, -1896, -1896,    69, -1896,
    1041,  1094, -1896, -1896, -1896, -1896, 26988,  5259, -1896, -1896,
   27152,  5886, -1896, -1896, 15721,   905, -1896, 19139, -1896, -1896,
   27316, -1896, -1896,   913, -1896, -1896, -1896,   927, -1896, 30759,
    1072, 31071, -1896,   939,  5259,   603,   948,   962, -1896,  3024,
    5503,  3024, -1896, -1896, -1896,  5128,  3090,   971,  1024,   811,
    1024, -1896,    69,    69,    66, 22656,   862,  1024, -1896,    69,
      69,    66,    69, -1896,    69, -1896,  4668, -1896, -1896,   994,
     999,  1186, 30196, 21205, 18807, -1896,  5681, -1896,  3313, -1896,
    2180,   457,   997,  1080, 22656,  5259,  5259,   200, -1896, 13758,
   -1896,  1186,  1186,  1012,  1080, 22656,  5259, -1896, 30602, -1896,
   -1896, -1896,  1186, -1896, -1896, -1896, -1896,  1186, -1896,  1194,
    5233,  5259, -1896, 24892,  1027, -1896, -1896, -1896, 29904,   603,
   22826,  1006,  5503, 24746, 30196, 16606, -1896, 29261, -1896,  5259,
    1024,    65, -1896, 33104, 29261,  5398,  4668, -1896,   874, -1896,
   -1896, -1896, -1896, 21897, 26824,  1024, -1896,  1026,  1044, -1896,
   -1896, -1896, -1896,  5259,  4786,   446,   622, -1896,  5259,   673,
   -1896,  1146, -1896, 15898, 27480,   895, 21724, -1896,  1186,  1186,
    1047, -1896,   913,  3248,   -53,   610, -1896,   169,   457,  1046,
    1042, -1896,  2822,  1035,   673,  2822,  2229,   669,  1062, 30837,
   -1896, 33104, -1896,   705,   888, -1896, 14659, 24164, -1896,   623,
   -1896, -1896,   735, -1896, -1896, -1896,  2229, 30434,  5158,  1081,
    1105,  1119,  1124,  1130,  1141, -1896, -1896,  1088,  1098, -1896,
     751,  1098, 23660, -1896,  5247, 23828, -1896, 27644, 26824,  5379,
   -1896, 23660, -1896, 33104, -1896, -1896, -1896, -1896, -1896, -1896,
   23828, -1896, -1896, 26004, 27644, 27644, 14836,  1467,  2066,   627,
    2273, -1896,   779,  1151,  1153,  1175, -1896,  1168, 25047,  1176,
    1165, 16783, 24332,  1221, 33494,  1237, -1896, 29418, 27316, -1896,
   31149,  1178, -1896, 33104,  3024, 33104,  3024, -1896, -1896,  3067,
   -1896, -1896, 30434,  2241, 33104, 30434,  3024, -1896, -1896, -1896,
   -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896,  1238, 33104,
   -1896, -1896, 15013, -1896, -1896, 27808, -1896,  1186,  1186, -1896,
   -1896,   913, -1896, -1896, 33104, 33104, 33104, 33104, 33104, 33104,
   33104, 33104, 33104, 33104, 33104, 33104, 33104, 33104, 33104, 33104,
   33104, 33104, 33104, 31227, -1896, -1896, 13574, 31305,  1340, 33104,
    4057,  1140,  1219, -1896,    69,    69,  1219,  1189, -1896,    69,
      69,  1247,  1219, -1896,    69,    69, -1896,  1098, -1896, 31383,
   16075, 27480, -1896, -1896,  5259, 25858,  1186,  1186, -1896,  5145,
    5379, -1896, 21897, -1896, 21897, -1896, 28947, -1896, -1896,  1219,
   16960, -1896, 26988, -1896, -1896, -1896,    71, 26168, 22070, -1896,
   -1896, -1896, 33572, -1896, 24509,  4221, 30837, 30759,  1260,  1276,
   -1896, -1896,  1249, 31071,   585, -1896, -1896, -1896, 24164,  1286,
   -1896,   758, -1896, -1896, -1896,  1266,  5128,   393,  1290,  1292,
    1304,   766,  1307,  1320,  1344,  1354,  1382,  1388,  3090, -1896,
   -1896, -1896,    69,  1317, 29575, -1896, -1896,  1247,   200, -1896,
   -1896,   603,  1080, 25548, -1896, -1896,   200, -1896, -1896,   603,
   -1896, -1896,  4668, -1896, 24164, 24164, -1896,  1186,  5503, 30342,
   16252,  2799, -1896, -1896, -1896, -1896, -1896, -1896,   603,  1080,
    1392,  1386, -1896, -1896,  3313,  1393,  1080, 22656, -1896,   603,
    1080, -1896, 30660, -1896,  1186,  1186, -1896, -1896,  1398,   324,
    1401,   457,  1406, -1896, -1896, -1896, 25858,  1419,  1425, -1896,
   -1896,   807, -1896,  1521, -1896,  1410, -1896, -1896, -1896, 27981,
    1435,  1439,  1191,  5259,  1024, -1896, -1896, -1896, -1896, -1896,
    5398,   832,  4668, 25548,  1441, 13018, -1896,  5259,  1440, 18477,
    1448, -1896, -1896, -1896, -1896, -1896,  2822, -1896, -1896,  1530,
   26332, 20513,  1598,   753, -1896, -1896,    69,  1445,    69,  1042,
     275,  1449,   847, 26824,   850,   853, -1896,  1437,  1469, -1896,
     758, 20686,  1085, -1896, -1896,    69,    69, -1896, -1896, 24164,
   -1896,  3024,  1475,  1477, -1896, -1896, -1896,  1248,  1098, -1896,
     863,  1098, 25548, -1896, -1896,  1247, 25548, -1896,  1247,  1480,
    1482,  1481,  1487, 16429,  1484,  1489, -1896,  1490,  1492,  1491,
    1502, 33104,  1504,  1505,  1509, 28127, 33104, -1896, -1896,  2300,
   -1896, -1896, -1896, 33104, -1896,  1510,  1514, 30915, 31461,  1516,
   20859, -1896, 26988, -1896, -1896, -1896, 30993, 15190,  1518, 23996,
    1522,   888,  1524, 17137, -1896, -1896, -1896, -1896, 30434,  1536,
   -1896,  1538, -1896, -1896,  5584, -1896,  1517, -1896,  5584, -1896,
   -1896,  1197,  1511, -1896, 30759, -1896, -1896, -1896,   859,   859,
     859,   691,   691,   983,   983,  1045,  1045,  1045,  1045,   592,
     592,  1100,   858,   871,   864,   906, 33104,  1185, 20859, 13574,
    1542,   844,  1546,  1547,  1553,  1557,  1561,  1562,  1566, -1896,
     841,  3754, -1896,  4057, -1896, -1896, -1896, 25548, -1896, -1896,
   -1896, -1896, -1896, -1896, 25548, -1896, -1896, -1896, -1896, -1896,
   -1896, -1896,  1247, -1896,  1567, 27152, 16783, -1896, -1896, -1896,
    1219,  1186,  5584, -1896, -1896,  1202, -1896, -1896, -1896, -1896,
   -1896, -1896, -1896, 20859, -1896,  5259,  5584, -1896,  1570,   387,
    1576,  1249, -1896, 30759,  1568, -1896,  3833, 33104, -1896, -1896,
     877, -1896,  1569, 20859, 33104,   865,  1580,  1581,  1585,   892,
    1588,  1589,  1590,  1595,  1596,  1604,  1372,  1098, -1896, -1896,
    1373,  1098, -1896, -1896,  1416,  1098, -1896, -1896, -1896,  5503,
    1751,  1098,   480, -1896,   888,  1209, -1896, -1896,   603,  1605,
   -1896, -1896, -1896,   910,  1610,   938,  1612, -1896,  1221,  1616,
   -1896,   603, 17657, -1896,   603,  1080,  1616, -1896,   603,  1608,
    1613,  1614, -1896, -1896, 25712, -1896,  3024,  5259, 12644,  1712,
   -1896, -1896, -1896, 22070,  1024, -1896, 20859,   894, -1896,  1616,
    1625, -1896, -1896, -1896, -1896,  5503, 28291, 17821, -1896,   295,
     351, 24164,  1609, -1896,  1609, -1896, -1896,    69,   753, -1896,
     275,  1042,  1626,    81, -1896, -1896,  1622,  5259,   275, -1896,
   -1896,  1630,  1638, -1896,  1651,  1653,  1654,  1655,  1661,  1085,
   -1896, -1896, -1896, -1896, -1896,  1642, -1896, 30434, 25548, -1896,
   -1896,  1247, 25548, -1896,  1247,  1666,  1667,   489, -1896, 25858,
     489,  3024, -1896,   489, -1896, 26496,   489, -1896, 33104, 33104,
   33104, -1896, -1896, -1896, -1896, 33104, 33104,  1662, 30759, -1896,
   -1896, -1896,  1670, -1896, -1896,  1676,  1677,  1679, -1896, -1896,
   -1896, -1896,  1670, -1896, -1896, -1896,  1668, 20859, 20859,  1683,
   -1896, -1896, -1896,  2536, -1896, -1896,  1212, -1896,   -33, -1896,
    1235, -1896, 31461, -1896,  1249, -1896, 33104, -1896,  1685, -1896,
    1474,  1098, -1896,  1565,  1619,  1098, -1896,  1186, 11729,  2108,
   -1896,    69,    69, -1896, -1896, -1896,  1686,  1687, -1896, -1896,
    1254, -1896, 21897, -1896,   200,  1264, 33104, -1896, 33104, -1896,
    1696, -1896, 31071, -1896,    69, 20859,    69, -1896, -1896,  1637,
    1098, -1896,  1671,  1098, -1896, -1896,  1727,  1098, 25548, -1896,
   -1896,  1247, 25548, -1896, -1896,  1247, 25548, -1896, -1896,  1247,
    1024, -1896,  1247, -1896, 33104, -1896, 33104, -1896, 29736, -1896,
   -1896, -1896, -1896, -1896, -1896,  1697, -1896, -1896, -1896, 17985,
    1616, -1896,   603, -1896, -1896, -1896, -1896, -1896, 19665, -1896,
   -1896, -1896, -1896, -1896,   390,   576,    29, 14305,  1698,  1699,
   22469,  1701,  1702,  2861,  2908,  3570, 31539,  1705, -1896, -1896,
    1706,  1708, 22469,  1710, -1896, -1896,   603, 33104, 33104,  1847,
    1714,   658, -1896, 23156, 15367,  1715,  1711,  1703, -1896, -1896,
   -1896, 12457, -1896, -1896, -1896, -1896, -1896,  2786, -1896, -1896,
   -1896,  1341,   213, -1896,   215, -1896,   213, -1896, -1896, -1896,
   -1896, -1896,  3024, -1896, -1896, 13390, 18973, -1896,  5259, -1896,
   -1896, -1896, -1896,  5259, -1896, -1896, -1896,  5259, -1896,  5503,
   -1896,   947, 26824,   673,   673,  1042,  1622,  1704,   275,   457,
     575,  1735,  1719,  1622, 18149, -1896, -1896, -1896,  1749,  1098,
   -1896, -1896, 33104, -1896,  1724,  1733, -1896, -1896,   897,  1734,
    1732,   949, -1896,  1740, -1896, -1896, -1896, -1896, -1896, 30759,
    1249, 31617,  1746, -1896, 21378, 21551,  1752, -1896, -1896, -1896,
   -1896,  1788,  5584, -1896,  1788,  1788, -1896,  5584,  4959,  5618,
   -1896,  1274,  1760, -1896,  1761,    69, 25548, -1896, -1896,  1247,
   25548, -1896, -1896, 25548, -1896, -1896,  1247, 33104, 33104,  1757,
    1762, -1896,  1763, -1896, -1896, -1896, -1896, -1896, -1896,  1764,
   -1896, -1896,  1765, -1896, -1896, -1896, -1896, -1896, -1896,  1770,
   25548, -1896, -1896,  1247, 25548, -1896, -1896,  1247, 25548, -1896,
   -1896,  1247,  1773,  1775,  1777,   200,  1293, -1896,   276, -1896,
     888,  1782, -1896, -1896, -1896,  1784, 19832, 19999, 20166, 28455,
   30196, 27644, 27644,  1785,  1759,   416,   424,   969, 11296, -1896,
     493,  5259,  5259, -1896, 30434,   138,   425, -1896, -1896, -1896,
   -1896, 14305, 33104,  1794,  1865, 14127, 11639, -1896,  1786, -1896,
    1790, 33104,  1791, 30759,  1792, 33104, 24164, 33104, -1896, 12831,
    1508, -1896,  1798,    47, -1896,     8,  1860,   156,    69, -1896,
    1810, -1896,  1800, -1896,  1801,  1819,  1821, 22469, 22469, -1896,
   -1896,  1894, -1896, -1896,    -3,    -3,   588, 13942,   505,  1826,
    1831,   446, -1896, -1896, -1896, -1896, -1896, -1896,  1825,  1838,
     275,  1622,    81,  5259, -1896, 31695, -1896,  1841, -1896, 18313,
   25548, -1896, -1896,  1247, -1896, -1896, -1896,  1839, -1896, -1896,
   33104, -1896, 26496, 33104,  1249,  1844, -1896, -1896, -1896, -1896,
    1843, -1896, -1896,  1845,  1851, -1896,  1295, -1896,  5584, -1896,
    5584, -1896, -1896, 31617, -1896, -1896,  1852,  1862,  1864, -1896,
   -1896,  1840, -1896,  1857, -1896, -1896,  1868,    69,  1871,  1873,
    1874, -1896, -1896, -1896, -1896, -1896, 33104, -1896,  1870, -1896,
    1785,  1785,  1785,  3594,  1070,  1853,   513, -1896,  3594,   529,
   24164, -1896, -1896, -1896, -1896,  5473, 33104,  5036,   231, -1896,
   -1896, -1896,   527,  1875,  1875,  1875,  5259, -1896, -1896, -1896,
    1882, -1896, -1896, -1896, -1896,  1711,  1883, 33104,   325,  1884,
     400, 20340, 28455,   957,  1877, 22469,  1885, -1896, -1896, -1896,
   -1896,  1318, 22469, 33104,  2236,   660, -1896, 33104, 12273, -1896,
   -1896,   538, -1896,  1249, -1896,   959,  1008,  1013,   670, -1896,
   -1896, -1896, -1896,   603,  1508,  1888, -1896, -1896, 33104, -1896,
    1889,   758, -1896, 11452, -1896, -1896, -1896, 33104, 33104, -1896,
   -1896,   171,    -3, -1896,   521, -1896, -1896, -1896,    69, -1896,
    1609,   275, -1896,  1622,  1893,   457,  1719, 30759, -1896, -1896,
   -1896,  1898, -1896, -1896, -1896, -1896,  1896, -1896,    69,    69,
   -1896,  1334,  1339, -1896, -1896, -1896,  1891,  1897, -1896, -1896,
   -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896, -1896,
     555,  1070,  1172,   572, -1896, -1896, -1896, -1896,    69,    69,
   -1896, -1896, -1896,   594, -1896,  1050,  5473,   791, -1896,  5036,
   -1896,    69, -1896, -1896, -1896, -1896, -1896, -1896, 22469, 22469,
    1711, 22243,    62, 31773,  1988, 22469, -1896, -1896, -1896, -1896,
   -1896, 33104, -1896, 31851,  1990,  1899, 24568, 31929, 22469, 12831,
    1711,   690,  2242,  1900, 33104, -1896,  1910,   131, 22469, -1896,
   22469, -1896,  1913, -1896, 28619,  1902,   758,   715, -1896, -1896,
    1920,  1342,  1067, 22469,  1923, 22469, 22469, 22469, -1896,   673,
    1622,  1929, -1896, -1896,  1249, -1896, -1896, -1896, -1896, -1896,
   -1896, -1896, -1896, -1896,  1927,  1931,  1933,  1172, -1896,    69,
   -1896, -1896, -1896, -1896, -1896,  1932,  3594, -1896,  2021,  9877,
      94, 17317, -1896, 22342, -1896,    23,  1089, 22469,  2028,   601,
    1935,   -62, 22469, 33104,   690,  2242,  1925, 32012,  1476,  1186,
    1937,    25,  2037, -1896, 32090, 32168, 33104,  1711,  1938, 17493,
   -1896, -1896, -1896, 28619,  1941,  4304, 28783,  3024, -1896,  1949,
    1942,   -17, -1896, 33104, 30434, -1896, -1896, 33104,   213, -1896,
   -1896, -1896,  1958, -1896,  1969,  1750,  1098, -1896, -1896,  1070,
   -1896, 22469, -1896,    75, -1896,   143, -1896, -1896, -1896,  1971,
   19324, -1896, -1896, 22469, -1896,    34, -1896, 22469, 33104,  1972,
   32246, -1896, -1896, 32324, 32402, 33104,  5379,  1711, -1896,   888,
   32480, 32558, 22469,  1956,   256,  1959,   456, -1896, -1896,  1975,
   19324,  1941, 33104,  1974,  3973,  4418, -1896, -1896, -1896,  1981,
   -1896,  2029,  1979,   721,  1983, -1896, -1896,  1991,  1092,   144,
   -1896, -1896, 25548, -1896, -1896,  1247, -1896, -1896, 33104, -1896,
   33104, -1896, -1896,  1438, 19498, -1896, -1896, 22469, -1896, -1896,
    1711, -1896, -1896,  1711,  1980,   498,  1982,   541, -1896, -1896,
     457, -1896,  1711, -1896,  1711, -1896,  1989, 32636, 32714, 32792,
   -1896,  1438,  1993, -1896,   603,  4418,   -17,  1992, 33104,  1978,
     -17,   -17, -1896, -1896, 22469,  2082,  2004, -1896, -1896, 22342,
   -1896,  1438, -1896, -1896,  2012, 32870, 32948, 33026, -1896, -1896,
    1711, -1896,  1711, -1896,  1711, -1896,   603, -1896,  2000,   758,
    2013, -1896,   723, -1896, -1896, 22469, -1896, -1896, 11999,  2015,
   22342, -1896, -1896,  1711, -1896,  1711, -1896,  1711,  2017, -1896,
     758,  2018, -1896,  1995,   758, -1896, -1896, -1896, -1896, 12188,
   -1896, -1896,  1355, 33104, -1896,  1121,   758,  3024,  2023,  1998,
   -1896, -1896,  1127, -1896, -1896,  2002,  3024, -1896, -1896
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   503,     0,     2,   503,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   509,   511,   510,   512,     0,     0,
       0,   530,   532,   559,   533,   560,   536,   537,   557,   558,
     531,   555,   556,   534,   535,   538,   539,   540,   541,   542,
     543,   544,   545,   546,   547,   548,   549,   550,   551,   552,
     553,   554,   561,   562,   869,   564,   637,   638,   641,   643,
     639,   645,     0,     0,     0,   503,     0,     0,    17,   608,
     614,     9,    10,    11,    12,    13,    14,    15,    16,   825,
     876,   104,   879,     0,    20,     0,   503,   102,   103,     0,
     846,    18,    19,   885,   503,   826,     0,     0,   441,   747,
     443,   455,   867,   442,   477,   478,     0,     0,     0,     0,
     591,   505,   507,   513,   503,   515,   518,   576,   529,   563,
     487,   569,   574,   489,   586,   488,   601,   605,   611,   590,
     617,   629,   869,   634,   635,   618,   688,   444,   445,     3,
     833,   847,   508,     0,     0,   869,   908,   869,   503,   925,
     926,   927,   503,     0,  1118,  1119,     0,     1,   503,    17,
       0,   503,   466,   467,     0,   591,   513,   497,   498,   499,
     836,     0,   640,   642,   644,   646,     0,   503,   869,   691,
     870,   871,   636,   565,    22,    23,    21,   801,   796,   786,
       0,   879,   834,     0,     0,   520,   827,   831,   832,   828,
     829,   830,   503,   884,   883,   882,     0,   877,   880,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   609,   612,
       0,     0,     0,     0,     0,   869,     0,     0,     0,    27,
      29,     4,     8,    25,     5,     6,     7,     0,     0,   503,
     503,   503,     0,   102,   105,   106,   107,   108,    85,    28,
      86,    24,    46,    84,   109,   503,     0,   124,   126,   130,
     133,   136,   141,   144,   146,   148,   150,   152,   154,   165,
       0,    30,   734,     0,  1140,     0,   504,   503,   515,   494,
     569,   495,   594,   496,   601,   605,   598,   619,   869,   620,
       0,     0,   730,   735,   720,   724,   503,   736,  1087,  1088,
     503,   737,   739,   886,   503,     0,  1120,   591,   915,   933,
     503,  1124,  1117,  1115,  1122,   438,   437,     0,   173,   753,
     172,     0,   446,     0,     0,     0,     0,     0,   453,     0,
       0,     0,   436,  1002,  1003,     0,     0,   476,   867,   869,
     867,   889,   869,   869,   486,   503,   869,   867,   946,   869,
     869,   485,   869,   965,   869,   943,     0,   584,   585,     0,
       0,   503,   503,   503,   503,   456,   867,   506,   516,   577,
       0,   606,     0,   850,   503,     0,     0,   747,   457,   591,
     570,   587,   602,     0,   850,   503,     0,   519,   571,   578,
     579,   490,   588,   492,   493,   491,   593,   603,   607,     0,
     621,     0,   819,   503,     2,   848,   907,   909,   503,     0,
     503,     0,     0,   591,   503,   503,  1128,   591,  1131,     0,
     867,   867,     3,     0,   591,     0,     0,   469,   869,   862,
     864,   863,   865,   503,   503,   867,   823,     0,     0,   782,
     784,   783,   785,     0,     0,   778,     0,   767,     0,   776,
     788,     0,   689,   503,   503,  1140,   504,   569,   594,   601,
       0,   736,   737,   691,   608,   614,   692,   693,   694,     0,
     691,   872,     0,   799,   787,     0,   879,     0,     0,     0,
     109,     0,   157,     0,     0,   615,   503,   503,   793,   743,
     745,   792,     0,   742,   746,   875,   879,   158,     0,     0,
       0,     0,     0,     0,     0,   887,   913,   869,   923,   931,
     935,   941,   503,    92,     0,   503,   100,   503,   503,     0,
      87,   503,    94,     0,    36,    40,    41,    37,    38,    39,
     503,    90,    91,   503,   503,   503,   503,   105,   106,     0,
       0,   194,     0,     0,   635,     0,  1115,  1138,   503,     0,
       0,   504,   503,   608,     0,     0,  1126,   591,   503,  1129,
       0,     0,  1040,     0,     0,     0,     0,    26,    59,     0,
      65,    66,   158,     0,     0,   158,     0,   174,   175,   176,
     177,   178,   179,   180,   181,   182,   183,   184,   172,     0,
     170,   171,   503,    88,  1090,   504,   500,   501,   502,  1094,
    1084,  1085,  1092,    89,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1039,     2,   503,     2,   103,     0,
    1049,   869,  1140,   984,   869,   869,  1140,   869,   999,   869,
     869,  1063,  1140,  1045,   869,   869,  1054,  1061,   728,     0,
     503,   503,   599,  1089,   738,   504,   595,   596,   600,     0,
       0,   464,   503,  1132,   503,  1104,   504,  1110,  1105,  1140,
     503,  1098,   503,  1107,  1099,     2,  1140,   503,   503,   916,
     934,  1116,     0,     2,    27,     0,     0,   753,    28,     0,
     751,   754,  1138,     0,     0,   760,   749,   748,   503,     0,
     852,     0,     2,   468,   470,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   892,
     949,   972,   869,   482,     0,   888,   896,  1030,   747,   890,
     891,     0,   850,   503,   945,   953,   747,   947,   948,     0,
     964,   966,     0,   472,   503,   503,   575,   504,     0,   591,
     503,     0,  1121,  1125,  1123,   454,   592,   823,     0,   850,
       0,     0,   447,   458,   517,     0,   850,   503,   823,     0,
     850,   797,   572,   573,   589,   604,   610,   613,   608,   614,
     632,   633,     0,   798,   706,   740,   504,     0,   707,   709,
     710,     0,   216,   430,   849,     0,   428,   486,   485,   591,
     102,     0,     0,     0,   867,   449,     2,   450,   820,   474,
       0,     0,     0,   503,     0,   503,   823,     0,     0,     0,
       0,   781,   780,   779,   773,   514,     0,   771,   789,   567,
     503,   503,   103,  1049,   738,   690,   869,     0,   869,   691,
     691,     0,     0,   503,     0,     0,   874,     0,     0,   440,
       0,   503,  1014,   744,  1011,   869,   869,  1019,   616,   503,
     878,   162,     0,   159,   160,   164,   914,   869,   924,   932,
     936,   942,   503,   917,   919,   921,   503,   937,   939,     0,
       0,     0,     0,   503,     0,     0,   693,     0,     0,     0,
       0,     0,     0,     0,     0,   503,     0,   123,   122,     0,
     119,   118,    31,     0,    32,     0,     0,     0,  1139,     0,
     503,  1096,   503,  1106,  1097,   185,     0,   503,   102,   503,
       0,   606,     0,   504,     2,     2,  1127,  1130,   158,     0,
      55,     0,    56,    63,     0,    62,     0,    58,     0,    57,
      61,     0,     0,    54,   753,   166,  1086,   125,   127,   128,
     129,   131,   132,   134,   135,   139,   140,   137,   138,   142,
     143,   145,   147,   149,   151,   153,     0,     0,   503,   503,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1064,
       0,   869,  1141,  1050,   987,  1004,  1051,   503,   982,   990,
     726,   985,   986,   727,   503,   997,  1007,  1000,  1001,   729,
    1047,  1048,  1062,  1133,     0,   503,   504,  1091,  1095,  1093,
    1140,   597,     0,    33,   632,     0,   722,   721,   725,   731,
    1102,  1109,  1103,   503,   732,     0,     0,   762,   157,     0,
       0,  1138,   759,  1139,     0,   755,     0,     0,   758,   761,
       0,     2,     0,   503,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   869,   901,   905,   944,
     869,   958,   962,   970,   869,   980,   893,   950,   973,     0,
       0,  1026,     0,  1031,  1032,     0,   480,   853,     0,     0,
     481,   854,   473,     0,     0,     0,     0,   471,     0,     2,
     855,     0,     0,   823,     0,   850,     2,   856,     0,     0,
       0,     0,   647,   910,   503,   928,     0,     0,   503,   431,
     429,  1042,  1041,   503,   867,   451,   503,     0,   824,     2,
       0,   775,   816,   811,   812,     0,   504,     0,   807,     0,
       0,   503,   769,   768,   769,   568,   566,   869,  1050,   685,
     691,   691,     0,     0,   701,   700,  1138,     0,   691,   802,
     800,     0,     0,   835,     0,     0,     0,     0,     0,  1015,
    1016,  1012,  1013,   795,   794,     0,   881,     0,   503,   918,
     920,   922,   503,   938,   940,     0,     0,    93,    96,   503,
     101,     0,    99,    95,    97,   503,     0,   113,     0,     0,
       0,   117,   121,   120,   195,     0,     0,     0,   753,   110,
     191,   190,  1138,   187,   714,     0,   715,   716,  1100,  1108,
    1101,   186,  1138,  1111,  1112,  1113,     0,   503,   503,     0,
      49,    50,    82,     0,    82,    82,     0,    70,    72,    52,
       0,    48,     0,    51,  1138,   156,     0,     3,     0,  1058,
     869,   993,   996,   869,   869,  1057,  1060,   503,     3,     0,
    1046,   869,   869,   988,  1005,  1052,     0,     0,  1134,   733,
       0,   465,   503,     3,   747,     0,     0,   763,     0,   764,
       0,   756,     0,   750,   869,   503,   869,     3,   475,   869,
     902,   906,   869,   959,   963,   971,   869,   981,   503,   894,
     897,   899,   503,   951,   954,   956,   503,   974,   976,   978,
     867,   483,  1027,  1038,     0,  1037,     0,  1029,     0,   858,
     967,   581,   580,   583,   582,     2,   824,   859,   804,     0,
       2,   857,     0,   824,   860,   647,   647,   647,   503,   708,
     711,   712,   741,   434,     0,     0,     0,   503,     0,     0,
     355,     0,     0,     0,     0,     0,   196,     0,   350,   351,
       0,     0,   355,     0,   403,   402,     0,   168,   168,   409,
     608,   614,   213,   503,   503,     0,   197,     0,   224,   198,
     199,   503,   218,   200,   201,   202,   203,     0,   204,   205,
     356,     0,   370,   206,   376,   378,   381,   207,   208,   209,
     210,   211,     0,   212,   220,   591,   503,   222,     0,   452,
       3,   837,   824,     0,   814,   791,   808,     0,   809,     0,
     810,     0,   503,   786,   786,   691,  1138,     0,   691,   697,
     691,     0,   702,  1138,     0,   873,   439,  1023,   869,  1022,
    1025,  1017,     0,   161,     0,     0,   911,   929,  1043,     0,
       0,     0,    42,     0,   114,   116,   115,   112,   111,   753,
    1138,  1139,     0,  1135,   503,   503,     0,  1114,     3,     3,
      69,    79,     0,    73,    80,    81,    64,     0,     0,     0,
      60,     0,     0,   155,     0,   869,   503,   989,   991,   992,
     503,  1006,  1008,   503,  1053,  1055,  1056,     0,     0,   102,
       0,     3,     0,   983,   998,   994,  1009,    34,   723,     0,
     448,   766,     0,   866,   752,   757,   851,     3,   868,     0,
     503,   895,   898,   900,   503,   952,   955,   957,   503,   975,
     977,   979,     0,     0,     0,   747,     0,  1033,     0,  1034,
    1035,     0,   806,   824,   861,     0,   503,   503,   503,   503,
     503,   503,   503,   630,     0,     0,     0,   661,   591,   648,
       0,     0,     0,   432,   158,     0,     0,   341,   342,   221,
     223,   503,     0,     0,     0,   503,   503,   337,     0,   335,
       0,     0,     0,   753,     0,     0,   503,     0,   382,   503,
       0,   169,     0,     0,   410,     0,     0,     0,   869,   228,
       0,   219,     0,   332,     0,     0,     0,   355,   355,   361,
     360,   355,   372,   371,   355,   355,     0,   591,     0,     0,
       0,   778,   813,   815,   790,   770,   774,   772,     0,     0,
     691,  1138,     0,     0,   680,     0,   696,     0,   803,     0,
     503,  1018,  1020,  1021,   163,   912,   930,     0,  1044,    98,
       0,    35,   503,     0,  1138,     0,   193,   192,   189,   718,
     717,   719,   188,     0,     0,    83,     0,    71,     0,    77,
       0,    75,    47,     0,   167,  1137,     0,     0,     0,     3,
       3,     0,  1066,     0,  1136,   765,     0,   869,     0,     0,
       0,   903,   960,   968,   484,  1028,     0,   841,     0,   843,
     630,   630,   630,   661,   669,   635,     0,   675,   661,     0,
     503,   622,   660,   659,   655,     0,     0,     0,     0,   662,
     663,   665,   869,   677,   677,   677,     0,   656,   673,   435,
       0,   345,   346,   343,   344,   237,     0,     0,   239,   443,
     238,   591,   503,     0,     0,   355,     0,   320,   322,   321,
     323,     0,   355,   196,   277,     0,   270,     0,   196,   338,
     336,     0,   330,  1138,   339,     0,     0,     0,     0,   391,
     392,   393,   394,     0,   384,     0,   385,   347,     0,   348,
       0,     0,   375,     0,   217,   334,   333,     0,     0,   364,
     374,     0,   355,   377,     0,   379,   401,   433,   869,   839,
     769,   691,   681,  1138,     0,   699,   702,   753,   703,   684,
     805,     0,    53,    45,    43,    44,     0,    67,   869,   869,
      74,     0,     0,   995,  1010,  1059,     0,     0,  1065,  1067,
     459,   463,   904,   961,   969,  1036,   845,   626,   628,   624,
       0,     0,  1073,     0,   670,  1078,   672,  1070,   869,   869,
     654,   676,   658,     0,   657,     0,     0,     0,   679,     0,
     650,   869,   649,   666,   678,   667,   668,   674,   355,   355,
     240,   591,     0,     0,   258,   355,   325,   328,   326,   329,
     324,     0,   327,     0,   266,     0,   196,     0,   355,   503,
     278,     0,   303,     0,     0,   331,     0,     0,   355,   354,
     355,   395,     0,   386,   503,     0,     0,     0,   215,   214,
     357,     0,     0,   355,     0,   355,   355,   355,   462,   786,
    1138,     0,   683,   698,  1138,  1024,    68,   461,   460,    78,
      76,  1068,  1069,   652,     0,     0,     0,  1074,  1075,   869,
     653,  1071,  1072,   651,   631,     0,     0,   353,   229,     0,
       0,     0,   251,   355,   231,     0,     0,   355,   260,   275,
     286,   280,   355,   196,     0,   290,     0,     0,     0,   315,
     281,   279,   268,   271,     0,     0,   196,   304,     0,     0,
     234,   352,   383,   503,   389,   396,   504,   400,   349,     0,
       0,   411,   362,     0,   158,   373,   366,     0,   367,   365,
     380,   777,     0,   687,     0,   869,  1081,  1083,  1076,     0,
     664,   355,   246,   241,   244,     0,   243,   250,   249,     0,
     503,   253,   252,   355,   262,     0,   259,   355,     0,     0,
       0,   267,   272,     0,     0,   196,     0,   291,   316,   317,
       0,     0,   355,     0,   306,   307,   305,   274,   340,     0,
     503,   389,     0,     0,     0,  1073,   397,   398,   399,     0,
     404,     0,     0,     0,   412,   413,   358,     0,     0,     0,
     686,   704,   503,  1077,  1079,  1080,   671,   230,     0,   248,
       0,   247,   233,   254,   503,   424,   263,   355,   264,   261,
     276,   289,   287,   283,   295,   293,   294,   292,   273,   318,
     319,   288,   284,   285,   282,   269,     0,     0,     0,     0,
     236,   254,     0,   390,     0,  1074,   411,     0,     0,     0,
     411,     0,   363,   359,   355,     0,     0,   242,   245,   355,
       3,   255,   425,   265,     0,     0,     0,     0,   314,   312,
     309,   313,   310,   311,   308,     3,     0,   387,     0,     0,
       0,   405,     0,   414,   368,   355,  1082,   225,     0,     0,
     355,   302,   300,   297,   301,   298,   299,   296,     0,   388,
     417,     0,   415,     0,   417,   369,   227,   226,   232,     0,
     235,   418,     0,     0,   406,     0,     0,     0,     0,     0,
     419,   420,     0,   416,   407,     0,     0,   408,   421
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1896,    37,  -127, -1896,    -1,   488,  2453,  4744,  -178, -1896,
    -177, -1896,   550, -1896,  -749, -1001, -1896,   304,  8842,  1964,
   -1896,   477, -1896,  1657,   719,   924,   928,   707,   918,  1582,
    1583,  1586,  1587,  1579, -1896,   -98,  -146,  -566, -1896,  1040,
    9729,   852, -1896,  1960, -1896, -1896,  -865,  4480, -1266,  2240,
   -1896,  2195, -1896,   842,    72, -1896, -1896,   647,   145, -1896,
   -1829, -1582,   336,   134, -1896, -1896,   645,   355, -1896, -1674,
   -1896, -1289, -1896, -1896, -1896, -1896,   177, -1334, -1896, -1896,
   -1351,   460, -1896, -1896, -1896, -1896, -1896,   -29, -1325, -1896,
   -1896, -1896, -1896, -1896,   202,   491,   492,   280, -1896, -1896,
   -1896, -1896,  -971, -1896,   146,    98, -1896,   218, -1896,  -282,
   -1896, -1896, -1896,   867,  -931, -1157,  -266, -1896,    50,    46,
     123,  8160,  -845,  -822, -1896,  -124, -1896, -1896,    88, -1896,
    -104,  1001,  1364,  -341,  5567,  8589,  -501,    22,    84,   229,
     926,  2336, -1896, -1896,  2208, -1896,   116,  6408, -1896,  2143,
   -1896,   135, -1896, -1896,  3030,   159,  6880,  4216,    -8,  1905,
    -250, -1896, -1896, -1896, -1896, -1896,  -292,  7670,  7343, -1896,
    -235,    63, -1896,  -775, -1896,   331, -1896,   278,   736, -1896,
    -133,  -286, -1896, -1896, -1896, -1896,  -158,  8214, -1103,   861,
     486,  1564, -1896,  -353,  -765,  1846,  2566,  1867,  -634,  -166,
     900,   189,  -500,  -360,  -307,  -646,  1251, -1896,  1602,   370,
   -1121,  1472, -1896, -1896,   693, -1896, -1379,  -188,  -281,  -696,
   -1896,    77, -1896, -1896, -1040, -1102, -1896, -1896, -1896,  2301,
    -989,  -529,  -785,   -43, -1896, -1896, -1896, -1896, -1896, -1896,
    -159, -1018,  -272, -1895,     2,  8784,   -78,  9259,  -105,  1806,
   -1896,  1881,    60,  -302,  -285,  -275,    18,   -75,   -73,   -67,
     448,   -51,   -27,   -26,  -269,    67,  -267,  -260,  -256,   175,
    -206,  -194,  -136,  -588,  -580,  -557,  -554,  -575,  -143,  -550,
   -1896, -1896,  -793,  1454,  1457,  1468,  2569, -1896,   793,  2923,
   -1896,  -623,  -577,  -555,  -505,  -498, -1896, -1789, -1764, -1753,
   -1740,  -549,   274,   -49,  -249, -1896,    26,   178,   -99, -1896,
   10617,  2317,  -622,  -259
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   317,   248,   249,   186,    93,  1365,   250,   251,
     252,  1441,  1442,   253,  1226,  1227,  1228,  1461,   254,   481,
     256,   257,   539,   540,   258,   259,   260,   261,   262,   263,
     264,   265,   266,   267,   268,   269,  1029,   862,   863,   864,
     541,  1582,   589,   321,   591,   271,  1202,  1366,  1367,  1368,
    1369,  1370,  1371,  1372,  2148,  1373,  1374,  1726,  2004,  2005,
    1942,  1943,  1944,  2120,  2121,  1375,  1745,  1746,  2028,  1747,
    1872,  1873,  1376,  1377,  1378,  1379,  1380,  1381,  1901,  1905,
    1605,  1597,  1382,  1383,  1604,  1598,  1384,  1385,  1386,  1387,
    1388,  1389,  1390,  1764,  2043,  1765,  1766,  1974,  1391,  1392,
    1393,  1585,  2053,  2054,  2055,  2172,  2182,  2073,  2074,   409,
     410,  1108,  1109,  1334,    95,    96,    97,    98,    99,  1729,
     272,   305,   103,   104,   105,   106,   337,   338,   412,   391,
     274,   489,   275,   109,   424,   111,   112,   166,   277,   278,
     116,   117,   118,   182,   119,  1136,   279,   167,   122,   361,
     123,   168,   370,   281,   458,   283,   169,   484,   128,   129,
     286,   130,   782,  1101,  1099,  1100,  1701,   287,   288,   133,
     134,  1328,  1549,  1708,  1709,  1710,  1833,  1834,  1550,  1696,
    1853,  1711,   135,   839,  1415,   178,  1145,   289,  1146,  1147,
    1626,   970,   788,  1205,   290,   291,   789,   293,   294,   295,
     791,   490,   491,   322,   691,   692,   693,   694,   695,   446,
    1413,   447,  1134,  1132,   824,   448,   473,   449,   450,   492,
     137,   188,   189,   138,  1127,  1128,  1129,  1130,     2,  1315,
    1316,   815,  1401,   139,   436,   437,   372,   383,   765,   140,
     325,   141,   427,  1030,   755,   725,   180,   142,   206,   207,
     208,   143,   429,   341,   342,   343,   430,   145,   146,   147,
     148,   149,   150,   151,   346,   431,   348,   349,   350,   432,
     352,   353,   354,   632,   633,   634,   635,   636,   355,   638,
     639,   640,   853,   854,   855,   856,   726,  1075,  1306,   296,
    1637,   642,   643,   644,   645,   646,   647,  1836,  1837,  1838,
    1839,   599,   297,   298,   299,   300,   493,   312,   154,   155,
     156,   302,   909,   648
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      91,   474,  1040,    91,   544,   210,   936,   986,   212,   942,
     213,   460,   402,  1414,   697,   478,   214,   762,   885,   144,
     746,   469,   192,   113,   411,  1406,   652,   653,  1016,  1586,
    1017,  1601,   215,   708,  1616,  1617,   470,  1416,  1587,   901,
     158,  1031,   984,  1928,  2006,  1423,   972,   100,   483,   976,
     709,  1212,  1319,   418,   162,   985,   216,   217,   704,  1160,
     710,    91,    91,   732,    91,   561,   711,  1924,   712,   973,
    1034,   977,   974,   455,  1884,   713,   975,   219,  1925,   714,
    1574,    91,    91,   144,   743,   114,   477,   113,    91,   107,
    1397,  1926,   759,    91,  1351,  2007,   482,  1555,  1556,  1770,
    1468,    91,  1008,   770,   328,  2013,    91,  1323,   903,    91,
     233,   100,   308,    91,  2012,   422,  2077,   120,  2020, -1002,
     311,   978,   622,   708,   101,   344, -1002,   163,   373,   715,
    1402,   637,   384,   989,  1469,   499,   124,   500,  1998,   996,
     709,   716,   559,   501,  2069,  1206,  1939,  1940,    64,   114,
     710,  2051,    91,   107,  1768,    91,   711,    91,   712,   502,
     125,  1782,   323,    91,   377,   713,   623,   340,   828,   714,
      91,  1544,  1771,   326,   347,  2006,   144,  1394,    91,   153,
     113,   120,   153,   503,   504,   731,  2047,   311,   101,  1230,
      91,  -817,   739,   828,   157,   903,    69,    70,    80,   717,
     124,   404,  1957,  1206,   100,  2031,   652,   653,    91,    91,
     986,   418,  2014,    82,   758,  1939,  1940,  1406,  1557,   715,
    2068,   411,  1463,  2078,   125,   769,  2114,   308,  1089,   806,
     115,   716,   414,   365,  1941,   806,  1769,   378,  -850,  1096,
      91,  1351,   114,   153,  1084,  1086,   107,   667,    85,   381,
     411,   673,  -818,  1903,   976,  1268,  1928,   706,  1206,   629,
     212,   411,   213,  1260,   120,  2008,   170,  1781,   214,  1595,
    1586,  1784,    91,    91,   120,  1599,   977,  1265,  1206,   171,
    1924,   101,   351,   124,   215,  1143,   795,  1119,  1904,   717,
    2012,  1925,   153,   124,   115,   808,    91,   176,  1234,  1600,
      91,   652,   653,  1969,  1926,   837,    91,   125,   216,   217,
     404,   455,   841,  2070,  2071,  1621,  1998,   125,   688,  1560,
     838,  2012,  1772,    91,   552,   679,   978,  1721,    91,    91,
      91,  1533,  1599,  1602,    91,    91,   153,   881,   940,   368,
     723,  -682,   728,  1150,   191,   652,   653,   810,  -682,   736,
     212,  1206,   213,  1397,   308,    91,  1600,  1603,   214,  1164,
    1255,   318,  1219,    91,   563,    91,  1431,  1471,  1076,   564,
    1008,   845,   319,   990,    91,    91,  1080,   993,    91,  1544,
    1544,  1544,   308,   999,  1629,    91,  1035,   115,   320,   972,
     753,   777,   976,    20,  1850,  1253,   719,   766,  1193,    91,
      91,  1851,    91,   720,  1046,   652,  1559,    91,  1254,  1270,
    1019,    91,   973,   193,   977,   974,   652,  1024,    91,  1238,
    1852,  1047,   805,   807,    91,    91,   903,   194,   545,   414,
     797,  1048,   455,   202,  1303,  1411,  2097,  1049,  -695,  1050,
    1394,   793,    91,    91,   308,  -695,  1051,    91,  1586,  1144,
    1052,   461,  1206,  1206,   466,  1877,  1305,  1587,   414,   927,
     753,   120,    91,   771,   978,  1407,  1657,  1659,  1661,   414,
    1082,    91,   340,   989,    91,    91,  1087,  1875,   783,   347,
     124,   315,  1883,  1545,  1408,  1095,   719,   637,   202,    92,
     120,  2119,   160,   720,   414,    91,  -627,    91,  1018,   913,
    1053,   120,   763,  1004,   125,   368,  1546,   323,  1046,   124,
    1206,   721,  1054,   311,   316,  1255,   679,  1793,    91,  2119,
     124,  1409,    -3,  1042,  1421,  1047,   120,  1406,   329,   600,
    1991,  1531,  1266,   125,    64,  1048,   652,   552,  1535,  2150,
    1410,  1049,   309,  1050,   125,   124,   455,    91,   330,   381,
    1051,   482,  1450,    92,  1052,   345,  1267,   403,   374,   505,
    1552,   724,   385,    91,  1320,    91,   455,  1268,    91,   125,
    1055,    92,    91,  1078,   455,    91,   331,   303,   559,  1553,
    1452,   324,    92,  -850,    80,  1622,   329,   798,  1028,   332,
    1456,   821,  1964,  1965,   330,    92,   356,   404,    92,    82,
    1091,   721,    92,  1907,  1053,  1703,    64,  1094,   764,   376,
    1956,  1098,  1472,  1704,  1723,  1237,  1054,  1610,  -497,   177,
    1596,   822,   823,  1021,   399,    91,   903,   565,  1498,    91,
    1727,   499,   566,   500,  1727,  1748,  2099,  2059,  1303,   501,
     401,   172,   368,   411,   173,   174,    92,   175,  1748,   624,
    1304,   777,    92,    91,  1197,   502,    80,   309,    91,    91,
    1305,  1198,   968,  1716,   980,  2023,  2024,   467,   903,  1909,
    1263,    82,  1154,   652,  1055,  1552,   552,   219,  2125,   503,
     504,  1142,  1717,  1841,    91,   368,   688,  2022,  1910,   461,
    1277,  1545,  1545,  1545,  1787,   724,   403,    92,    92,  1716,
    2037,   879,  1842,   520,   882,    91,   884,   403,   903,   505,
     887,   903,  1023,  1656,  1546,  1546,  1546,    91,  1844,   890,
    1025,  2127,   892,   893,   894,  1851,  1271,  1885,   600,   555,
     612,   613,   593,   603,   552,   499,   679,   500, -1139,  1043,
    1554,    91,  1929,   501,  1923,  1482,  1485,    91,  1688,  1144,
      91,  1259,  1036,  1037,   466,   680,   652,   653,  1106,  2088,
     404,  1930,    92,  1400,  1851,    81,    14,    15,    16,    17,
      18,   903,  1118,  1038,   202,   404,   614,   615,  1066,   455,
     368,   913, -1003,  1933,   309,  1067,   423,   851,   825, -1003,
    2018,   724,   826,   896,  1619,    92,   559,  1300,    87,    88,
    -838,  1627,    91,  1644,   897,   898,  1115,    92,   472,    91,
     461,    91,   309,   455,    91,  1638,    91,   703,    92,   705,
    1016,  1017,   202,    92,    92,    91,  1878,   767,  1645,   -23,
     679,  1879,    64,   144,   552,   846,  1890,   113,  1252,   496,
     637,  1879,   842,  1116,    92,   844,  1247,  1866,  1867,  1868,
    1869,   414,    92,  1404,    14,    15,    16,    17,    18,   444,
      91,   100,   475,  1209,  1458,  1459,   605,    92,  1235,  1123,
    1870,   849,  1066,   606,   607,   850,   652,   218,    70,  1067,
    1756,  1980,    80,   120,   764,   233,  1981,  2109,   780,  2163,
      64,   785,  2110,  1068,  2164,    81,   160,    82,   497,   114,
      92,   858,   124,   107,  1500,   859,  1487,   357,   358,  1811,
     359,  1812,  1615,    92,    92,   876,   360,   831,  1830,   724,
      64,   627,  1507,  1843,   461,   600,   125,  1753,    87,   832,
     733,   120,  1059,    91,   724,   506,  1322,    91,   101,   482,
      80,    64,  1124,   688,   461,   902,   680,  1018,  1748,   903,
     124,   836,   461,    64,   507,    82,   913,   505,   593,   724,
     508,  1217,  1218,   172,   593,  1505,   173,   174,    91,   175,
      80,   509,   356,  1105,   125,   403,   510,  1106,   511,   724,
      91,   514,    91,  1417,   562,    82,    92,  1068,  1720,   495,
     496,    80,   517,   153,   200,   518,   813,   153,   505,  1794,
     724,   552,   108,    80,  1845,   164,    82,   886,   987,   455,
     505,    91,   627,  1149,   567,   368,  1151,   826,    82,  1152,
     826,   523,  1806,   496,    91,    91,   733,  1172,   764,   403,
     724,   724,   688,   724,   618,    91,   555,    81,   813,    81,
     604,   387,   724,  1274,   115,   619,   388,   859,   621,   392,
     620,   397,   930,   624,   932,   659,   733,   935,   813,   831,
     724,   831,   724,   627,   943,   627,   108,   649,    91,   625,
      87,   832,    87,   832,  -500,  1438,  1311,   675,  1275,  2075,
     903,   947,   159,   629,   333,   334,    71,    72,    73,    74,
      75,    76,    77,    78,   696,   306,  1018,   683,    14,    15,
      16,    17,    18,   698,  1313,    91,    91,   688,   903,  2075,
    1474,    81,   701,  1614,    92,  1641,  1399,   859,    92,  1642,
    1502,  1490,  1503,  1862,    91,  1887,   702,   903,   544,   903,
     113,  1886,   326,  1705,    84,  2138,  1499,  1613,  1473,  2142,
    1706,   722,  1123,  2122,    87,    88,    91,  1013,  1014,   108,
    1509,  1914,   764,   416,   100,   528,    89,   461,   744,   108,
     608,   609,   306,   745,    64,  1684,   680,    64,   482,   757,
     482,  1911,   203,    92,  1888,    92,   323,  1123,   859,  1889,
      91,   610,   611,   903,   768,    14,    15,    16,    17,    18,
     792,   461,  1396,   600,    92,   796,   107,   688,   816,    14,
      15,    16,    17,    18,   387,   388,    92,   657,   817,   397,
     661,   662,    81,   835,    80,  1124,  1934,    80,   840,    64,
     859,   466,    91,   843,   120,   474,   474,    81,   847,    82,
      92,   101,    82,  1985,  1831,   368,    92,   903,   724,   555,
     616,   617,   556,   124,  1590,    87,    88,   866,    91,   851,
    1124,    64,   872,   724,   204,  2015,   724,  1618,  2113,   903,
      87,    88,   903,   663,   664,    64,   554,   125,    64,    80,
     153,   867,   159,  1609,   184,   185,    71,    72,    73,    74,
      75,    76,    77,    78,    82,   868,   153,  2179,  1992,  1018,
     869,  2176,  1994,  2185,   387,  1728,   870,  2186,    92,  1728,
      92,    80,  1525,    92,   987,   153,  1695,   871,   627,   776,
      70,   416,   829,   399,    81,    80,    82,   904,    80,   955,
     956,   957,   958,   476,   948,   949,   950,    64,   905,   906,
      82,  1653,  1654,    82,   624,   916,  1831,   115,   908,   688,
     724,   907,    91,    91,    91,   915,   108,    87,    88,  1165,
     928,   688,   158,   994,  1551,   903,  1236,   627,   981,   113,
     663,  1113,  1671,   748,  1673,   752,  1231,  1232,  1187,  1123,
     688,  1261,  1262,  1191,  1443,   108,    91,    80,  1307,  1308,
    1676,  1466,  1467,   100,  1199,   924,   108,   461,  1536,  1537,
    1538,    91,    82,   113,    91,    91,   593,    91,  1827,  1828,
    1829,   925,    91,   708,  1470,  1467,    91,   629,    91,   164,
     944,   108,  1168,   373,   384,   682,   724,   100,  2057,  1033,
     709,  1396,    92,  1497,  1467,   107,    92,  1854,  1854,  1854,
     710,   -18,    92,  1501,  1467,   752,   711,  1730,   712,  1595,
    1596,  1730,  1124,  1662,  1663,   713,   377,  1032,   688,   714,
    1041,    64,    64,   120,  1044,  1396,  1056,    92,  1057,   107,
     101,    91,  1685,   903,  1810,  1467,    91,    91,    91,    92,
    1058,    92,   124,  1060,  1123,  1866,  1867,  1868,  1869,  1798,
    -123,  -123,  -123,  -123,  -123,  -123,  1061,   120,   306,    14,
      15,    16,    17,    18,   101,    64,   125,   153,  1870,   715,
      92,    80,    80,  1919,  1467,  1070,   124,  1871,  1920,  1467,
    1062,   716,  1983,  1984,    92,   153,    82,    82,   365,   378,
    1063,    92,  1939,  1940,    92,  2176,  2177,   482,  1464,  1465,
     125,   381,   951,   952,   959,   960,  1288,  1292,   953,   954,
     724,   724,  1816,  1817,  1046,    80,    91,  1124,  1064,   153,
      91,    91,   545,    64,  1065,    64,   806,    92,  1092,   926,
      82,  1047,  1551,  1551,  1551,  1093,   115,  1697,  1551,   717,
    -625,  1048,   688,  -623,   153,  1783,  1785,  1049,  1102,  1050,
    1296,  1855,  1856,   113,   724,  1103,  1051,   113,   113,   162,
    1052,    69,    70,  1897,  1332,  1104,   688,   688,  1107,  1110,
     115,   113,   153,    80,  1111,    80,    91,  1713,  1112,  1121,
     528,   664,  1131,    92,  1714,  1135,  1137,  1140,    82,   594,
      82,  1148,    91,  -498,   368,   766,  1153,  1846,  1759,  1760,
    1761,  1762,  1763,    14,    15,    16,    17,    18,  1476,   916,
    1053,  1166,   724,    85,    64,  1732,  1177,  1167,  1178,  1732,
    1732,  1179,  1054,  1180,  1181,  1182,  1183,    91,  1184,    91,
     665,  1185,   163,  1732,   671,  1444,  1445,  1446,  1186,  1439,
    1188,  1189,  1447,  1448,  1233,  1190,  1195,   120,   556,  1123,
    1196,   120,   120,  1229,   101,  1203,    92,  1213,   101,   101,
     387,  1214,    91,  1215,    80,   120,   124,    91,    64,    64,
     124,   124,   101,  2046,    91,  1220,    91,  1221,  1239,    82,
    1055,    92,  1240,  1241,   124,    91,    64,   153,  1979,  1242,
     125,   474,  1715,  1243,   125,   125,   652,  1244,  1245,  1480,
     763,  1273,  1246,   627,   688,  1276,  1258,    92,   125,   153,
    -165,   688,   708,   153,   153,  1269,  1279,  1280,    80,    80,
      64,  1281,  1124,  1713,  1282,  1283,  1284,   153,  1713,   709,
    1714,  1285,  1286,    82,    82,  1714,    80,   719,   108,   710,
    1287,  1310,   688,  1301,   720,   711,  1312,   712,  1314,  -821,
    1325,    82,   377,  1483,   713,  1326,  1327,   627,   714,  1398,
     115,  1403,  1420,  2003,   115,   115,   688,  1412,  1418,  -499,
      80,  1510,  1424,  2052,  1425,   724,    64,   153,   115,    14,
      15,    16,    17,    18,   520,    82,   108,  1426,   594,  1427,
    1428,  1429,    14,    15,    16,    17,    18,  1430,    64,    64,
    1432,  1443,  1436,  1437,  1449,  1514,   764,  1457,   715,   724,
    1451,   482,  1453,   374,   385,    91,  1460,  1454,    91,  1455,
     716,  1475,  1495,  1496,   365,   378,    80,   688,   688,  1504,
    -822,  1584,  1561,  1562,   688,  1565,  1566,   381,  1715,  1575,
    1576,    82,  1577,  1715,  1579,    64,  1620,   688,    80,    80,
     785,   903,   721,    92,    92,   -22,  1588,   688,    64,   688,
    1635,  1518,  1589,    82,    82,   724,   556,    92,  1624,  1636,
    1639,   113,   688,  1640,   688,   688,   688,  1625,   717,  1066,
     153,  1643,   911,  1630,  2062,  1648,  1067,   724,   724,  1655,
    1977,  1652,  2117,  1664,  2003,    80,    -3,  1665,  2052,   505,
    1674,  1672,  2052,  2052,  1675,    91,  1677,    92,    80,  1681,
      82,  1682,   688,  1683,  2103,  1687,   688,  1689,  1702,  1700,
      92,   688,  1734,    82,  -501,    92,    92,    92,  1554,  1596,
     368,  2161,  2140,  1732,   211,    94,   810,   787,   161,   212,
     482,   213,   482,  1774,    91,  1749,    91,   214,  2010,  1750,
    1752,  1754,  2171,  1777,   763,  1778,  2171,  1767,   339,  1775,
    1776,  1351,  1788,  2149,  1789,   120,  1713,  1791,  2180,  1977,
     688,  1792,   101,  1714,  1799,  1802,  2040,  1807,  2158,  1818,
     482,  1808,   688,  1262,   124,  1007,   688,  1809,  1813,    14,
      15,    16,    17,    18,  1068,    91,  1819,  2178,  1814,    94,
    1815,   688,   113,  1826,  1820,    92,  1020,  1822,   125,  1823,
    1824,  1863,  1840,    91,    91,   428,  1706,   209,  1858,  1859,
     255,  1865,  1894,  1896,   324,   767,  1912,   153,    94,  1916,
    1921,    92,   113,   308,  1915,   568,  1922,   569,   570,   571,
    1947,   336,  1952,  1968,   364,   482,   688,  1973,    94,  -122,
    -122,  -122,  -122,  -122,  -122,    64,  1982,  1987,  1953,  1966,
     764,  1978,  1993,  1995,  1732,    92,   113,  1996,   572,  1997,
     724,   573,   574,  2001,    91,   719,   575,   576,   115,   108,
    2017,  1715,   720,   688,  2025,  2019,   161,  2030,   688,  2032,
    2049,  2060,    94,   679,  1732,   161,   120,  2038,   426,   434,
    2042,  2050,  2061,   101,  2072,    80,  2096,  2081,  2100,  2098,
    2104,   454,  2107,  2108,   688,   124,    92,   688,    92,   688,
      82,  -502,  2106,  2111,  2128,   631,   120,  2112,  1732,  2136,
    2124,  2139,  2126,   101,  2145,  1066,   487,  2141,   688,   125,
    2146,  2160,  1067,   209,   209,   124,    91,  2151,  2168,  2162,
    2170,    92,  2173,  1125,  2174,    91,    92,  2184,   153,  2183,
     120,  2187,  1804,    92,   911,    92,   899,   101,    19,   125,
     961,   965,   962,   487,   255,   161,   963,  1433,   964,   124,
    1583,   428,  1736,  1591,   590,  2118,   707,   339,   153,   255,
     721,   159,  2169,  1970,  1758,    71,    72,    73,    74,    75,
      76,    77,    78,   125,  1963,  2135,  2115,   428,   630,   115,
     651,  1906,   203,  2102,    54,    55,    56,    57,    58,    59,
      60,    61,   153,  2041,  1900,  1892,  1893,  2143,  2101,    19,
     454,   802,  2175,   183,   454,  1608,   394,  2000,   255,   115,
     790,   364,  1249,    84,   161,   756,  1208,  2066,  1699,   814,
    1068,  1623,  1913,  1007,  1272,    92,    14,    15,    16,    17,
      18,   900,  1606,   428,   426,    89,  1039,  1079,  1133,   336,
     336,     3,   860,   115,  1790,  1156,   811,   428,  1157,    58,
      59,    60,    61,    14,    15,    16,    17,    18,  1192,  1158,
     426,  1686,     0,     0,   204,     0,     0,   454,    94,  1547,
       0,     0,     0,     0,    92,     0,     0,    92,   108,   405,
       0,     0,   159,   364,   184,   185,    71,    72,    73,    74,
      75,    76,    77,    78,   159,     0,   184,   185,    71,    72,
      73,    74,    75,    76,    77,    78,   937,     0,     0,   911,
       0,     0,   108,     0,     0,     0,   426,   787,     0,   255,
       0,   434,   802,     0,     0,     0,     0,     0,   434,   426,
     426,     0,     0,  1866,  1867,  1868,  1869,   454,   161,  1866,
    1867,  1868,  1869,     0,   201,     0,   903,     0,     0,   938,
       0,   314,   903,     0,     0,  1155,  1870,   255,   454,   833,
     651,     0,  1870,     0,    92,  1876,     0,     0,     0,     0,
       0,  -197,     0,     0,   543,     0,  1175,     0,     0,     0,
    1176,     0,     0,     0,     0,     0,   369,     0,     0,     0,
     255,   487,     0,   852,     0,     0,  1125,     0,   390,   393,
    2056,     0,   209,    92,     0,  2048,   195,     6,     7,     8,
       9,    10,    11,    12,    13,     0,   487,     0,   314,   487,
       0,   161,   161,     0,     0,   487,     0,     0,     0,     0,
       0,  1125,     0,   790,   487,     0,   802,   161,   161,   161,
     255,   369,     0,     0,     0,     0,     0,   971,  1015,     0,
       0,   631,   454,     0,  2089,   919,   802,     0,     0,   494,
     700,   434,   161,     0,   802,     0,     0,     0,     0,     0,
       0,     0,    92,    92,   386,     0,     0,  1547,  1547,  1547,
     164,  1693,  1694,  1698,     0,     0,     0,     0,     0,     0,
       0,  1256,     0,     0,     0,     0,   255,     0,  1257,   651,
       0,     0,   108,     0,     0,   529,   108,   108,     0,     0,
       0,     0,   602,     0,     0,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,     0,  1045,     0,     0,
     630,     0,     0,    92,   630,     0,     0,     0,     0,   339,
     790,     0,     0,     0,   794,     0,     0,     0,     0,     0,
       0,     0,   369,     0,   255,   454,     0,     0,     0,     0,
       0,     0,     0,   428,     0,     0,   454,     0,   454,   428,
     651,     0,     0,     0,   255,     0,   454,     0,     0,     0,
       0,   161,   454,   369,     0,     0,     0,     0,     0,   159,
       0,     0,   292,    71,    72,    73,    74,    75,    76,    77,
      78,  1222,   487,     0,     0,  2181,  1223,     0,  1224,     0,
     336,     0,     0,     0,  2188,     0,     0,     0,     0,     0,
     790,   754,   336,  1125,     0,     0,     0,     0,     0,     0,
       0,  1117,     0,   428,     0,     0,     0,     0,   790,     0,
       0,    84,     0,     0,  1462,     0,   426,   543,   487,   487,
     543,     0,   426,     0,   255,   369,   543,     0,   790,     0,
       0,     0,     0,     0,     0,   543,     0,     0,     0,   802,
       0,     0,  1434,     0,     0,     0,  1435,     0,     0,   790,
       0,     0,     0,   790,     0,     0,     0,     0,     0,   369,
       0,   754,     0,   369,     0,   543,  1207,     0,     0,     0,
     369,     0,     0,   802,     0,     0,     0,     0,     0,     0,
       0,   602,   689,     0,   426,     0,   426,     0,     0,    94,
       0,     0,     0,   161,     0,     0,     0,     0,  1125,     0,
       0,     0,   369,     0,   454,   833,     0,   833,     0,     0,
       0,     0,     0,     0,   314,     0,   292,   161,     0,     0,
       0,     0,     0,     0,  1207,   852,   852,     0,     0,     0,
       0,     0,     0,   487,     0,     0,     0,     0,     0,   880,
       0,     0,     0,     0,     0,   494,   790,     0,   888,     0,
       0,     0,     0,     0,     0,     0,     0,   255,     0,     0,
     971,     0,  1522,     0,   790,     0,  1523,     0,     0,   161,
    1524,   790,  1251,     0,   631,     0,     0,     0,     0,  1207,
     292,  1592,     0,     0,   454,     0,   454,     0,     0,     0,
     108,   255,     0,   255,     0,     0,   438,   369,     0,  1207,
       0,     0,     0,   369,   439,   440,   441,   442,     0,   159,
       0,   184,   185,    71,    72,    73,    74,    75,    76,    77,
      78,     0,   159,     0,  1088,   735,    71,    72,    73,    74,
      75,    76,    77,    78,   152,     0,  1077,   152,     0,   292,
       0,   369,   454,   630,  1081,   159,     0,   184,   185,    71,
      72,    73,    74,    75,    76,    77,    78,   630,     0,     0,
     428,     0,     0,  1090,     0,     0,     0,     0,     0,     0,
       0,     0,  1207,     0,  1097,     0,     0,     0,  1009,   454,
     919,  1331,     0,     0,   159,  1593,   184,   185,    71,    72,
      73,    74,    75,    76,    77,    78,     0,   454,   152,     0,
     443,   369,     0,  1125,     0,     0,     0,     0,     0,   292,
       0,     0,   369,     0,     0,     0,   428,   454,   444,     0,
       0,   108,     0,     0,     0,   494,     0,   310,     0,   292,
     529,   159,     0,   184,   185,    71,    72,    73,    74,    75,
      76,    77,    78,   426,     0,   790,     0,   152,     0,   790,
    1666,   108,     0,     0,  1667,     0,     0,  1668,     0,     0,
    1567,     0,   292,     0,     0,     0,   161,     0,   857,     0,
    1073,   494,   494,  1207,  1207,     0,     0,     0,     0,     0,
       0,     0,   487,     0,  1678,   108,   874,   454,  1679,   877,
     454,   152,  1680,     0,   310,   369,     0,     0,     0,   426,
     434,   161,     0,     0,     0,   487,     0,  1569,     0,     0,
       0,     0,   833,    14,    15,    16,    17,    18,   369,     0,
       0,     0,     0,     0,     0,     0,   282,     0,     0,     0,
       0,  1207,   369,   852,     0,   310,     0,     0,     0,     0,
    1492,     0,     0,     0,     0,   369,     0,   159,  1027,     0,
     689,    71,    72,    73,    74,    75,    76,    77,    78,   161,
       0,     0,     0,     0,     0,   790,     0,     0,   292,   790,
     494,     0,   310,   790,   558,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,   494,     0,     0,     0,
     159,   454,   454,     0,    71,    72,    73,    74,    75,    76,
      77,    78,   933,     0,  1801,     0,     0,   641,     0,     0,
       0,     0,     0,   159,     0,   333,   334,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,     0,     0,    80,
       0,     0,     0,   672,     0,     0,   454,     0,   292,     0,
     292,     0,    81,     0,    82,   934,  1009,     0,     0,   454,
     159,     0,   464,   465,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,   335,    84,     0,     0,  1975,   369,
       0,     0,   727,     0,     0,    87,    88,     0,     0,   727,
     282,     0,     0,  1309,     0,     0,     0,    89,     0,     0,
     735,     0,     0,   161,     0,   597,  1317,   310,     0,  1321,
     428,     0,   161,  1324,    85,     0,     0,     0,     0,     0,
    1650,   487,     0,     0,     0,     0,   466,   656,     0,     0,
       0,     0,     0,     0,     0,     0,   292,   195,     6,     7,
       8,     9,    10,    11,    12,    13,   597,   487,   255,     0,
     597,     0,     0,     0,   282,   487,     0,  1975,     0,     0,
       0,     0,   369,   790,     0,     0,     0,   790,     0,     0,
     790,   727,     0,     0,     0,     0,     0,   310,     0,   364,
      94,   159,     0,   333,   334,    71,    72,    73,    74,    75,
      76,    77,    78,   426,     0,     0,   161,   790,   641,     0,
       0,   790,     0,     0,     0,   790,     0,  1225,   161,     0,
       0,  1225,     0,   282,     0,     0,     0,   689,     0,     0,
       0,     0,   833,     0,     0,     0,     0,     0,     0,     0,
     310,     0,   727,     0,     0,     0,     0,     0,   454,   454,
     857,   857,     0,     0,     0,     0,     0,   466,  1712,     0,
     727,   875,     0,   727,   878,   310,  1170,     0,   310,  1173,
     310,   310,     0,     0,   310,   282,     0,     0,   494,     0,
       0,     0,     0,   310,     0,     0,   310,   310,   310,     0,
       0,     0,   369,     0,     0,  1225,     0,     0,     0,     0,
       0,   912,     0,     0,     0,     0,   292,     0,     0,  1225,
       0,     0,     0,   282,   597,     0,   689,     0,     0,  1027,
       0,     0,     0,     0,     0,     0,     0,   790,     0,     0,
     161,   161,   161,   161,     0,   161,   161,     0,     0,     0,
       0,  1707,   434,     0,     0,     0,   282,  1534,     0,     0,
       0,     0,     0,     0,     0,   487,     0,     0,     0,   487,
     487,  1558,     0,     0,   292,     0,     0,     0,     0,     0,
     487,     0,     0,   487,     0,     0,     0,     0,     0,   641,
       0,  1580,     0,   641,   641,     0,     0,     0,   543,     0,
     641,     0,     0,     0,     0,     0,   282,     0,     0,     0,
    1002,   364,     0,     0,  1712,     0,     0,     0,   597,  1712,
    1564,     0,   656,     0,     0,     0,  1847,     0,  1712,   292,
       0,     0,  1578,   161,     0,     0,     0,     0,     0,     0,
     558,     0,     0,     0,     0,     0,   161,     0,     0,   292,
       0,     0,     0,     0,     0,     0,     0,     0,   735,     0,
       0,   310,   282,     0,     0,  1290,  2116,     0,     0,  1294,
     727,     0,     0,  1298,   727,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1529,     0,     0,     0,     0,     0,
       0,   689,     0,     0,     0,     0,     0,  1707,  1832,     0,
       0,     0,  1707,     0,   487,     0,     0,   310,   310,  1707,
    1330,  1707,     0,     0,     0,     0,  1225,     0,     0,     0,
     282,   597,   292,   159,     0,   184,   185,    71,    72,    73,
      74,    75,    76,    77,    78,   434,   161,     0,     0,     0,
     282,     0,   597,     0,     0,     0,     0,   159,   282,   333,
     334,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,  1935,   857,   494,
    1712,   369,     0,     0,   727,     0,    81,     0,   152,     0,
       0,     0,   152,     0,     0,  1571,     0,     0,     0,     0,
    1722,  1724,     0,   912,   641,     0,   641,     0,  1705,    84,
       0,     0,     0,     0,     0,  1706,   310,     0,     0,    87,
      88,     0,     0,     0,   727,   727,     0,     0,     0,     0,
     282,    89,   310,   292,   292,     0,     0,     0,     0,     0,
     727,  1171,     0,   727,  1174,  1832,  1832,     0,     0,     0,
       0,  1786,     0,     0,     0,     0,     0,     0,     0,  1478,
    1707,     0,     0,  1707,     0,     0,     0,  1712,   558,     0,
       0,     0,     0,     0,     0,   434,     0,     0,   292,     0,
       0,     0,     0,    64,     0,     0,     0,  1779,  1780,     0,
       0,   292,     0,   487,     0,     0,     0,     0,  1512,   597,
       0,  1516,     0,     0,     0,  1520,   428,     0,   161,     0,
     597,     0,     0,     0,     0,     0,     0,   159,     0,   333,
     334,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,    80,   369,     0,     0,     0,     0,     0,
       0,  1832,   641,   494,   641,     0,     0,     0,    82,     0,
    1707,     0,   689,     0,     0,     0,   641,     0,     0,     0,
       0,     0,     0,   282,     0,  1225,     0,     0,  1249,    84,
    1225,  1225,  1225,     0,     0,   811,   428,     0,   912,   790,
     292,     0,     0,     0,     0,     0,     0,   161,     0,  2045,
     434,    89,   597,   369,     0,     0,   159,   282,     0,   597,
      71,    72,    73,    74,    75,    76,    77,    78,  1891,     0,
       0,     0,     0,  1832,     0,     0,     0,     0,   727,     0,
       0,     0,   727,     0,   161,  1864,     0,     0,     0,   727,
    1291,     0,  1874,   727,  1295,     0,   428,   727,  1299,     0,
       0,     0,     0,     0,  1302,     0,     0,  1632,    84,     0,
       0,  1026,     0,     0,   161,     0,     0,     0,  2045,  2045,
       0,     0,     0,  1899,     0,   152,     0,   494,     0,     0,
     292,   292,     0,     0,     0,     0,   689,     0,     0,     0,
       0,   152,     0,     0,     0,   597,     0,     0,   161,     0,
     727,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     152,     0,    64,     0,   310,     0,     0,     0,     0,     0,
       0,   641,     0,     0,     0,     0,     0,   369,     0,  2045,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
       0,     0,   727,     0,     0,     0,   159,     0,   333,   334,
      71,    72,    73,    74,    75,    76,    77,    78,  1937,  1938,
       0,     0,    80,     0,     0,  1948,     0,     0,   310,     0,
       0,  1225,     0,  1225,     0,    81,     0,    82,  1962,     0,
       0,     0,   597,     0,     0,     0,     0,     0,  1971,     0,
    1972,     0,     0,     0,     0,     0,    64,  2044,    84,     0,
       0,   724,     0,  1986,     0,  1988,  1989,  1990,    87,    88,
       0,     0,     0,     0,     0,     0,     0,   597,     0,     0,
      89,     0,     0,   727,  1479,     0,   641,   641,  1486,     0,
     159,     0,   333,   334,    71,    72,    73,    74,    75,    76,
      77,    78,     0,  2011,     0,     0,    80,  2016,     0,     0,
       0,     0,  2021,     0,     0,     0,     0,   369,     0,    81,
       0,    82,   727,  1513,     0,   727,  1517,     0,     0,   727,
    1521,     0,     0,     0,     0,     0,     0,   127,     0,     0,
     127,   626,    84,     0,     0,   627,     0,     0,     0,     0,
       0,     0,    87,   628,     0,     0,     0,     0,     0,     0,
       0,  2067,   152,     0,    89,     0,     0,     0,     0,     0,
     689,   310,     0,  2076,     0,     0,     0,  2079,     0,     0,
     152,     0,     0,  1835,     0,     0,     0,     0,     0,     0,
       0,     0,  2095,     0,     0,     0,     0,     0,     0,     0,
       0,   127,     0,     0,     0,     0,   310,     0,     0,     0,
       0,     0,     0,     0,   152,     0,     0,     0,     0,  2137,
       0,     0,   285,     0,     0,     0,     0,     0,     0,     0,
     127,     0,   369,     0,     0,     0,     0,  2123,     0,   152,
       0,     0,     0,     0,     0,     0,   371,     0,     0,     0,
     127,  2159,     0,     0,   159,   310,   218,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,   152,     0,   597,
       0,   727,  1633,     0,  2144,     0,     0,     0,     0,  2147,
       0,   641,     0,     0,   127,     0,     0,     0,   127,     0,
       0,     0,     0,     0,   127,     0,     0,   127,     0,     0,
       0,   371,     0,     0,     0,  2165,    84,     0,  2167,  1026,
    2147,     0,   451,   127,   282,   468,     0,     0,     0,     0,
    1835,  1835,     0,     0,     0,     0,     0,     0,     0,  2167,
       0,     0,     0,     0,     0,     0,     0,   159,   285,   333,
     334,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,    14,    15,    16,    17,    18,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,     0,     0,
       0,     0,     0,     0,   597,   285,   285,   127,     0,   310,
     310,   310,   152,     0,   310,   310,     0,     0,  2044,    84,
       0,   285,   724,     0,     0,     0,     0,     0,     0,    87,
      88,     0,     0,     0,   152,     0,     0,     0,   152,   152,
       0,    89,   371,   127,     0,     0,  1835,    64,     0,   310,
       0,     0,   152,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   127,     0,     0,     0,   127,     0,     0,     0,
     285,     0,     0,   371,     0,     0,   127,     0,     0,     0,
       0,   159,     0,   333,   334,    71,    72,    73,    74,    75,
      76,    77,    78,     0,  1835,     0,     0,    80,     0,     0,
       0,     0,   152,     0,     0,     0,     0,     0,     0,     0,
      81,   127,    82,     0,  2064,   310,     0,     0,  1835,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,   127,
     127,     0,  2044,    84,     0,     0,   724,     0,     0,     0,
     127,     0,     0,    87,    88,   371,     0,     0,     0,     0,
       0,   127,     0,    64,     0,    89,     0,     0,     0,     0,
       0,     0,     0,  1835,  1835,     0,   781,   727,     0,   127,
       0,     0,     0,   310,   127,     0,   127,     0,     0,   371,
     127,   285,     0,   371,     0,   727,     0,   159,     0,     0,
     371,    71,    72,    73,    74,    75,    76,    77,    78,   127,
     127,     0,     0,    80,     0,   152,     0,     0,     0,   597,
       0,     0,     0,     0,     0,     0,    81,     0,    82,   285,
     127,     0,   371,     0,  1835,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,    83,    84,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    87,
      88,     0,   285,   285,     0,     0,     0,     0,     0,     0,
       0,    89,     0,     0,     0,     0,     0,     0,     0,   542,
       0,     0,     0,     0,     0,     0,     0,     0,   285,     0,
       0,   285,     0,   127,   127,   468,     0,   285,     0,     0,
       0,     0,     0,     0,     0,     0,   285,    64,     0,   127,
     127,   127,   285,     0,   727,   727,     0,     0,     0,     0,
       0,     0,     0,     0,   127,     0,     0,   921,   285,     0,
     727,     0,     0,   371,   127,     0,     0,     0,     0,     0,
       0,   159,     0,   333,   334,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,    80,     0,     0,
       0,     0,   152,     0,     0,     0,   187,   190,   285,     0,
      81,   371,    82,     0,     0,     0,     0,   310,     0,     0,
       0,     0,     0,     0,     0,     0,   205,     0,     0,     0,
       0,     0,   425,    84,     0,     0,     0,     0,     0,     0,
       0,     0,   127,    87,    88,     0,   327,     0,     0,     0,
     727,     0,     0,     0,     0,    89,     0,     0,   727,     0,
       0,     0,     0,     0,     0,     0,   285,   127,     0,     0,
       0,   371,     0,     0,     0,     0,   781,     0,   127,     0,
     127,     0,   371,     0,     0,     0,   285,     0,   127,     0,
       0,     0,     0,   127,   127,     0,   310,   420,   727,   159,
     421,   184,   185,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,   285,   445,     0,     0,   727,  2065,
       0,     0,   727,     0,   597,     0,     0,     0,     0,     0,
       0,     0,     0,   152,     0,   205,     0,     0,     0,     0,
    1074,     0,     0,     0,     0,     0,     0,     0,     0,   127,
       0,     0,     0,     0,     0,     0,     0,     0,   819,     0,
     285,   285,     0,   152,     0,   371,   285,   727,   727,     0,
     159,     0,   333,   334,    71,    72,    73,    74,    75,    76,
      77,    78,     0,   127,     0,     0,     0,     0,     0,     0,
       0,     0,   542,     0,     0,   542,     0,   152,     0,    81,
       0,   542,   371,   597,     0,     0,     0,     0,     0,     0,
     542,     0,     0,     0,     0,   371,     0,   327,     0,     0,
       0,   335,    84,     0,     0,     0,     0,     0,   727,   127,
       0,   127,    87,    88,     0,   127,     0,     0,     0,     0,
     542,   669,     0,     0,    89,   676,   127,   127,     0,    14,
      15,    16,    17,    18,     0,     0,     0,     0,     0,   127,
       0,     0,     0,     0,     0,     0,     0,   127,   699,     0,
       0,     0,   159,     0,     0,   285,    71,    72,    73,    74,
      75,    76,    77,    78,  1222,     0,     0,     0,   127,  1223,
       0,  1224,   127,     0,     0,     0,     0,     0,     0,   285,
       0,     0,     0,   967,     0,     0,     0,     0,     0,     0,
     327,   127,     0,     0,     0,    64,     0,     0,     0,   760,
     761,     0,     0,     0,    84,     0,   127,  1658,   127,     0,
     187,     0,     0,   285,     0,   285,     0,     0,     0,   921,
       0,     0,     0,     0,     0,   187,     0,     0,     0,   159,
       0,   333,   334,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,   804,     0,    80,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,    81,     0,
      82,     0,     0,     0,   127,   127,     0,   818,   820,     0,
       0,     0,   827,     0,     0,     0,     0,     0,     0,     0,
    1705,    84,     0,   127,     0,     0,     0,    64,     0,     0,
     127,    87,    88,     0,     0,     0,   445,     0,     0,   445,
     205,   127,   921,    89,  1083,  1085,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,   127,
     205,   159,     0,   333,   334,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,    80,   159,   127,
     776,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      81,   159,    82,     0,     0,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,    80,     0,     0,
       0,     0,   335,    84,     0,     0,     0,     0,     0,     0,
      81,     0,    82,    87,    88,     0,     0,     0,   127,     0,
       0,     0,     0,  1012,     0,    89,     0,   939,     0,     0,
     127,     0,    83,    84,   285,     0,     0,     0,     0,   127,
       0,     0,   127,    87,    88,     0,     0,     0,     0,     0,
       0,     0,   371,   127,     0,    89,   159,   285,   778,   779,
      71,    72,    73,    74,    75,    76,    77,    78,   228,  1419,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   159,     0,   184,   185,    71,    72,    73,    74,
      75,    76,    77,    78,   127,     0,     0,     0,   127,     0,
       0,     0,     0,     0,     0,   127,     0,     0,  1010,     0,
      85,   127,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   560,     0,     0,    85,   479,     0,     0,     0,     0,
       0,   195,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,   127,   127,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
     357,   358,     0,   359,    52,     0,    53,    64,   127,   360,
       0,     0,    55,    56,    57,    58,    59,    60,    61,     0,
       0,   127,   159,     0,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   127,     0,     0,     0,   127,     0,
       0,   159,   127,   333,   334,    71,    72,    73,    74,    75,
      76,    77,    78,     0,  1530,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,   127,     0,     0,     0,     0,
      81,     0,    82,     0,   127,     0,    85,  1114,     0,     0,
       0,     0,    64,   285,     0,     0,     0,     0,     0,     0,
       0,  1120,   425,    84,     0,     0,     0,     0,   110,     0,
     445,   165,     0,    87,    88,     0,     0,     0,     0,   285,
     285,     0,     0,  -479,     0,    89,   159,   285,   333,   334,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
       0,     0,    80,     0,     0,     0,  -479,     0,     0,     0,
       0,   371,   127,     0,     0,    81,   159,    82,   333,   334,
      71,    72,    73,    74,    75,    76,    77,    78,   127,     0,
       0,     0,   110,     0,     0,     0,     0,  1705,    84,     0,
     127,     0,     0,     0,     0,    81,     0,     0,    87,    88,
       0,     0,     0,   276,     0,     0,     0,     0,     0,     0,
      89,   307,     0,     0,     0,     0,     0,   425,    84,     0,
     127,   127,     0,     0,     0,     0,     0,     0,    87,    88,
       0,   379,     0,     0,     0,     0,     0,     0,     0,     0,
      89,     0,   127,     0,     0,     0,   127,   159,     0,   127,
       0,    71,    72,    73,    74,    75,    76,    77,    78,  1222,
       0,     0,     0,     0,  1223,   413,  1224,     0,     0,   417,
       0,     0,     0,     0,     0,   110,   127,     0,     0,     0,
     127,   159,     0,     0,   127,    71,    72,    73,    74,    75,
      76,    77,    78,  1222,   456,     0,     0,     0,  1223,    84,
    1224,     0,   127,   127,   127,   127,   127,   127,   127,     0,
       0,     0,     0,     0,   371,     0,     0,     0,     0,  1264,
       0,     0,     0,     0,     0,     0,   498,   285,     0,     0,
       0,   285,   285,    84,  1526,     0,  1660,     0,     0,   326,
       0,     0,   285,     0,   159,   285,   184,   185,    71,    72,
      73,    74,    75,    76,    77,    78,     0,   551,   557,     0,
     159,     0,   333,   334,    71,    72,    73,    74,    75,    76,
      77,    78,   595,   371,   159,     0,   184,   185,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,  1795,    81,
       0,     0,     0,   542,   655,   127,   127,     0,     0,     0,
       0,  1333,     0,     0,     0,     0,     0,     0,   127,     0,
       0,   626,    84,   666,     0,   627,     0,   666,     0,     0,
       0,   276,    87,   628,     0,   625,     0,   557,     0,     0,
       0,     0,     0,     0,    89,   629,     0,     0,     0,     0,
       0,  1422,   577,   578,   579,   580,   581,   582,   583,   584,
     585,   586,   587,   718,     0,     0,     0,   318,     0,     0,
       0,     0,   413,     0,     0,     0,   285,     0,     0,     0,
       0,     0,     0,   742,     0,     0,     0,     0,   747,   749,
     276,   307,     0,     0,   588,     0,     0,     0,     0,     0,
       0,   413,     0,     0,     0,     0,     0,   371,   127,     0,
       0,     0,   413,     0,     0,   772,     0,     0,     0,   774,
       0,     0,     0,     0,   775,     0,     0,     0,     0,     0,
     786,     0,     0,     0,     0,   749,     0,   413,     0,     0,
       0,   799,   456,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   812,     0,     0,     0,     0,     0,   159,
     276,   184,   185,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     551,   595,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1725,  1733,     0,     0,  1725,  1744,     0,     0,     0,
     675,  1751,     0,   276,     0,  1755,     0,  1757,     0,  1744,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   371,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1568,  1570,  1572,
       0,     0,   285,     0,     0,   285,     0,     0,     0,     0,
       0,     0,     0,   456,     0,     0,     0,     0,     0,     0,
     127,     0,     0,     0,     0,   666,     0,     0,     0,   923,
       0,  1594,     0,     0,     0,   557,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1333,     0,     0,     0,     0,  1611,     0,     0,
       0,  1612,     0,     0,     0,     0,     0,     0,     0,   551,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2029,   371,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   127,
       0,     0,   371,   786,     0,     0,     0,   983,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1860,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1006,   595,     0,
       0,     0,     0,     0,  1011,     0,   127,  1880,  1882,   276,
       0,   276,     0,     0,     0,     0,     0,   456,     0,   666,
       0,     0,  2090,     0,   557,   456,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   127,     0,  1902,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     127,     0,     0,     0,     0,  1718,  1719,     0,     0,     0,
     786,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   551,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
       0,     0,     0,  1946,     0,     0,     0,     0,     0,     0,
       0,  1949,     0,  1951,     0,     0,  1955,  1961,     0,  1744,
       0,     0,     0,     0,  1967,     0,     0,  1796,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     786,     0,   110,     0,     0,     0,  1126,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   666,   786,     0,
    1138,     0,     0,     0,     0,     0,     0,     0,     0,   121,
       0,     0,     0,     0,     0,     0,     0,     0,   786,  1159,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2027,     0,   786,
       0,     0,     0,   786,  2034,  2036,     0,     0,     0,     0,
     456,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1857,     0,   557,     0,     0,     0,     0,  2058,     0,     0,
       0,     0,     0,   121,     0,     0,     0,   276,     0,   666,
       0,     0,     0,     0,  1006,     0,   595,     0,     0,     0,
       0,     0,     0,     0,   280,     0,     0,     0,  2080,     0,
    2083,     0,     0,  2085,  2087,     0,     0,     0,     0,     0,
    2092,  2094,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   380,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   276,   786,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1248,     0,     0,
       0,     0,     0,     0,   786,     0,   121,     0,     0,     0,
       0,   786,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,   666,     0,     0,     0,     0,  2130,  2132,  2134,
       0,     0,     0,     0,     0,   457,     0,     0,     0,     0,
     276,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2153,  2155,  2157,     0,     0,
     276,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   280,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1126,
       0,     0,     0,   596,     0,     0,     0,     0,     0,     0,
       0,   786,     0,     0,     0,  1395,     0,     0,     0,     0,
     456,     0,     0,   276,     0,   380,     0,     0,     0,     0,
       0,     0,     0,     0,  1126,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   596,     0,     0,     0,   596,     0,
       0,     0,   280,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   786,     0,     0,     0,   786,
       0,     0,     0,     0,     0,     0,   786,     0,     0,     0,
       0,     0,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   280,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   121,     0,   276,   276,     0,     0,     0,     0,
       0,     0,     0,   121,     0,     0,   773,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   457,     0,     0,   747,     0,     0,     0,   121,     0,
       0,     0,   380,   280,     0,     0,     0,     0,     0,   276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   457,   276,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   786,     0,     0,     0,   786,
       0,   280,   596,   786,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   126,     0,     0,     0,     0,  1126,     0,     0,     0,
       0,     0,     0,     0,   280,  1548,     0,     0,     0,     0,
       0,     0,     0,     0,  1395,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   276,     0,     0,     0,     0,     0,     0,  1395,     0,
       0,     0,     0,     0,   280,   126,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   596,     0,     0,     0,
     380,     0,     0,  1607,     0,     0,   284,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1126,     0,     0,   382,     0,     0,     0,     0,     0,
     280,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   276,   276,     0,     0,     0,     0,     0,   126,     0,
       0,     0,     0,     0,   457,     0,     0,     0,   126,     0,
       0,     0,     0,   786,     0,     0,     0,   786,     0,     0,
     786,     0,     0,     0,     0,     0,     0,   459,   280,   596,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     457,     0,   457,     0,     0,     0,     0,   786,   280,     0,
     596,   786,     0,     0,     0,   786,   280,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1548,  1548,  1548,   165,   749,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     284,     0,     0,     0,     0,     0,     0,     0,  1731,     0,
       0,     0,  1731,  1731,     0,   598,     0,     0,     0,     0,
       0,   457,     0,     0,     0,     0,  1731,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   382,   280,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   121,   598,     0,     0,     0,
     598,     0,     0,     0,   284,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1126,   786,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   457,     0,   121,     0,   126,     0,   596,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   596,   457,
       0,     0,     0,   284,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   126,     0,     0,     0,     0,   457,
       0,     0,     0,     0,     0,   126,     0,     0,     0,     0,
       0,     0,     0,     0,  1849,     0,     0,     0,     0,     0,
     457,     0,     0,   459,   457,     0,     0,     0,     0,     0,
     126,   280,     0,     0,   382,   284,     0,     0,     0,  1861,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   459,     0,     0,     0,     0,   457,     0,
     596,     0,     0,     0,     0,   280,     0,   596,     0,     0,
       0,     0,     0,   284,   598,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   132,     0,     0,   132,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   284,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   457,   457,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   457,     0,     0,     0,  1927,
       0,     0,   457,     0,     0,     0,     0,     0,   132,     0,
       0,     0,     0,   596,     0,     0,   284,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   598,     0,
       0,   457,   382,     0,     0,     0,     0,   132,     0,     0,
       0,     0,     0,  1958,     0,     0,  1731,     0,     0,     0,
       0,   457,     0,     0,     0,     0,     0,   132,     0,     0,
       0,  1976,     0,     0,     0,     0,   395,     0,     0,     0,
       0,     0,   284,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   132,     0,     0,     0,   132,     0,     0,     0,     0,
     596,   132,     0,     0,   132,     0,   459,     0,     0,     0,
       0,     0,   457,     0,     0,     0,   121,     0,     0,     0,
       0,   457,     0,     0,   457,     0,     0,     0,     0,     0,
     284,   598,     0,     0,     0,   596,     0,     0,     0,     0,
    1976,     0,   459,     0,   459,   132,     0,     0,     0,     0,
     284,     0,   598,     0,     0,     0,     0,     0,   284,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   457,  1731,     0,     0,
     457,     0,   132,     0,   132,     0,     0,   457,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1731,     0,     0,
       0,     0,  2105,   459,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   457,   457,     0,     0,   786,
     284,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1731,     0,     0,     0,     0,     0,   126,     0,     0,
       0,     0,     0,   132,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     457,   131,     0,     0,   131,     0,     0,     0,     0,     0,
       0,     0,     0,   457,     0,     0,     0,     0,   132,     0,
       0,     0,     0,   459,     0,   126,   457,     0,     0,   598,
     457,     0,     0,     0,   457,   132,     0,   132,     0,     0,
     598,   459,     0,   132,     0,     0,     0,   132,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   596,   132,     0,
       0,   459,     0,     0,     0,   131,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   121,     0,     0,     0,     0,
       0,   132,   459,   132,     0,     0,   459,   132,     0,     0,
       0,     0,     0,   284,   131,     0,     0,     0,     0,     0,
       0,     0,   280,     0,     0,     0,     0,   132,     0,   121,
       0,     0,     0,     0,   131,     0,     0,     0,     0,     0,
     459,     0,   598,     0,     0,     0,     0,   284,     0,   598,
       0,     0,     0,     0,   380,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   131,     0,
       0,     0,   131,     0,     0,     0,     0,     0,   131,     0,
     132,   131,   596,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   459,   459,
       0,     0,     0,     0,     0,   132,     0,     0,   132,     0,
     132,   132,   457,   457,   132,     0,     0,   459,     0,     0,
       0,     0,   131,   132,   459,     0,   132,   132,   132,     0,
       0,     0,     0,     0,   457,   598,     0,     0,   457,     0,
       0,   457,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   132,     0,   459,     0,     0,     0,     0,     0,   131,
       0,   131,     0,     0,     0,     0,     0,     0,   457,     0,
       0,     0,   457,   459,     0,     0,   457,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   121,
       0,     0,   598,   121,   121,     0,     0,     0,     0,     0,
     131,     0,     0,     0,   459,     0,     0,   121,   126,     0,
       0,     0,     0,   459,     0,     0,   459,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   598,     0,     0,
       0,     0,     0,     0,     0,   131,     0,     0,     0,     0,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   131,     0,   131,     0,     0,   596,   457,     0,
     131,   132,     0,     0,   131,     0,     0,     0,   459,     0,
       0,     0,   459,     0,     0,   131,     0,     0,     0,   459,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   131,     0,
     131,     0,     0,     0,   131,     0,     0,   132,   132,     0,
       0,     0,     0,     0,     0,     0,     0,   459,   459,     0,
       0,     0,     0,     0,   131,     0,     0,     0,     0,     0,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     380,     0,   459,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   459,     0,   131,   132,     0,
       0,   102,     0,     0,   102,     0,     0,     0,   459,     0,
       0,     0,   459,     0,     0,     0,   459,     0,     0,     0,
       0,     0,   131,     0,     0,   131,   132,   131,   131,     0,
       0,   131,     0,     0,     0,     0,     0,     0,     0,   598,
     131,     0,   132,   131,   131,   131,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   136,     0,   126,   136,     0,
       0,     0,     0,     0,     0,   102,     0,     0,   131,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   132,     0,
       0,     0,     0,     0,   284,     0,   273,     0,     0,     0,
       0,   126,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     366,     0,     0,     0,   102,     0,   382,     0,     0,   136,
       0,     0,     0,     0,     0,     0,     0,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   596,     0,   598,     0,     0,     0,   136,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   102,     0,
       0,     0,     0,     0,     0,   435,     0,     0,   136,     0,
       0,     0,     0,     0,   459,   459,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   131,     0,     0,
       0,     0,     0,     0,     0,     0,   459,     0,     0,     0,
     459,     0,   136,   459,     0,     0,   136,     0,   131,     0,
       0,     0,   136,     0,     0,   136,     0,     0,     0,     0,
       0,   596,     0,     0,     0,     0,     0,     0,     0,     0,
     459,     0,     0,     0,   459,     0,     0,     0,   459,     0,
     273,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   131,   131,   136,     0,   121,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   131,     0,     0,
       0,   126,     0,     0,     0,   126,   126,     0,   121,     0,
       0,   132,     0,   136,     0,   136,     0,     0,     0,   126,
       0,     0,     0,     0,   273,     0,     0,     0,     0,     0,
     457,     0,     0,     0,   132,     0,     0,     0,     0,     0,
       0,     0,   121,     0,     0,   131,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   598,
     459,     0,     0,   131,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   273,   136,     0,     0,     0,   132,   131,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   366,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   136,
       0,     0,     0,     0,     0,   131,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   136,     0,   136,     0,
       0,     0,     0,     0,   136,     0,     0,     0,   136,     0,
       0,     0,     0,   273,     0,     0,     0,     0,     0,   136,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   382,   273,     0,     0,     0,     0,     0,     0,
       0,     0,   136,     0,   136,     0,     0,     0,   136,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   273,     0,   136,     0,
       0,     0,     0,     0,     0,     0,     0,   199,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   132,     0,     0,     0,     0,     0,     0,     0,     0,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   367,
       0,   136,     0,     0,     0,     0,   132,     0,     0,     0,
       0,   389,     0,   396,   132,   398,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   136,     0,     0,   136,
       0,   136,   136,     0,     0,   136,     0,     0,     0,   132,
       0,     0,     0,     0,   136,     0,     0,   136,   136,   136,
       0,     0,   273,     0,   367,   132,  1959,   396,   398,   126,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   136,     0,   598,     0,     0,     0,   131,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   131,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   527,     0,
       0,     0,   273,     0,   273,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   598,     0,   131,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   367,     0,     0,     0,     0,
       0,   658,     0,   398,     0,     0,     0,     0,     0,   132,
     132,   132,   132,   132,   132,   132,     0,     0,     0,     0,
     126,   136,     0,     0,     0,     0,   367,     0,     0,     0,
       0,     0,     0,     0,   132,     0,     0,     0,   132,   132,
     273,     0,   136,     0,     0,     0,   400,     0,     0,   132,
     126,     0,   132,     0,     0,     0,     0,     0,     0,   406,
       0,   407,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   459,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   126,     0,     0,     0,   136,   136,
       0,     0,   463,     0,     0,     0,     0,     0,   367,     0,
     396,   398,     0,     0,     0,   102,     0,     0,     0,   102,
       0,   136,     0,     0,     0,   132,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   131,     0,
       0,     0,   367,     0,     0,     0,   367,   131,     0,   519,
       0,     0,     0,   367,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   136,
       0,     0,     0,   131,     0,     0,   480,     0,     0,     0,
       0,   131,     0,   132,     0,   367,     0,   658,   398,     0,
       0,     0,     0,     0,     0,     0,     0,   136,     0,     0,
       0,     0,   513,     0,   516,     0,   131,     0,   480,   522,
     273,     0,   660,   136,     0,   132,     0,     0,     0,   531,
     532,     0,   131,     0,     0,     0,     0,   367,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   480,   480,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   136,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   729,   730,   273,     0,
     734,     0,     0,   737,   738,     0,   740,     0,   741,     0,
     367,     0,     0,     0,     0,     0,   367,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   273,   367,     0,   658,   398,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   273,     0,     0,   131,   131,   131,   131,
     131,   131,   131,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   132,     0,     0,     0,     0,     0,     0,     0,
       0,   131,     0,     0,     0,   131,   131,     0,     0,     0,
       0,     0,     0,     0,   367,   658,   131,     0,     0,   131,
       0,     0,   102,     0,     0,   367,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   102,     0,
       0,     0,     0,   527,     0,     0,   273,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   435,   102,     0,     0,
       0,   873,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   367,     0,     0,
       0,     0,   131,   181,     0,     0,     0,     0,     0,     0,
       0,     0,   136,   480,     0,     0,     0,     0,     0,   480,
       0,   367,     0,     0,     0,     0,   367,     0,   367,     0,
       0,     0,   181,     0,     0,   136,     0,     0,     0,     0,
       0,     0,     0,   132,     0,     0,     0,     0,     0,     0,
       0,   367,     0,   367,   367,     0,     0,     0,     0,     0,
     131,     0,     0,     0,     0,   367,     0,   273,   273,     0,
       0,     0,     0,   132,     0,     0,     0,     0,   367,     0,
       0,   181,     0,     0,     0,     0,     0,     0,     0,   136,
       0,   367,   131,     0,   181,     0,   181,     0,     0,     0,
       0,     0,     0,     0,     0,   988,     0,   132,   991,   992,
       0,   995,   273,   997,   998,     0,     0,     0,  1000,  1001,
       0,     0,     0,     0,     0,   273,     0,   181,     0,   471,
       0,     0,     0,     0,     0,     0,   480,   480,   480,   480,
     480,   480,   480,   480,   480,   480,   480,   480,   480,   480,
     480,   480,   480,   480,   480,     0,     0,     0,     0,   471,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   102,
       0,     0,     0,     0,   181,     0,     0,     0,     0,     0,
       0,   480,     0,     0,     0,     0,     0,   102,     0,     0,
       0,     0,     0,     0,     0,     0,  1069,     0,     0,     0,
       0,     0,   367,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   273,     0,     0,     0,     0,     0,
       0,   102,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   136,     0,     0,     0,     0,   181,     0,   131,
       0,   136,     0,     0,     0,   366,   102,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   367,     0,     0,     0,     0,   136,     0,     0,
       0,     0,     0,     0,   102,   136,     0,     0,     0,     0,
       0,     0,     0,     0,   181,   367,     0,     0,   181,     0,
     367,   181,   181,     0,     0,   181,     0,     0,   181,   181,
     136,   181,     0,   181,   273,   273,     0,     0,     0,     0,
    1139,     0,  1141,     0,     0,     0,   136,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1161,
    1162,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1169,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     131,     0,     0,     0,   181,     0,     0,   181,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   102,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     131,     0,     0,     0,     0,   367,     0,     0,     0,     0,
       0,   102,     0,     0,     0,   102,   102,   367,     0,     0,
       0,     0,     0,   480,     0,     0,     0,     0,   480,   102,
       0,     0,     0,     0,   131,     0,     0,     0,   367,   480,
     136,   136,   136,   136,   136,   136,   136,     0,     0,     0,
       0,   480,     0,     0,     0,  1250,   181,   366,     0,     0,
       0,     0,     0,     0,     0,   136,     0,     0,     0,   136,
     136,     0,     0,     0,     0,     0,     0,     0,     0,   102,
     136,     0,     0,   136,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   480,     0,
       0,     0,     0,     0,     0,   270,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   367,     0,     0,
    1289,     0,     0,     0,  1293,     0,     0,     0,  1297,     0,
       0,     0,     0,     0,     0,     0,   136,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2002,   480,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     181,   366,   102,   181,   181,     0,   181,     0,   181,   181,
       0,     0,     0,   181,   181,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   136,     0,     0,     0,     0,     0,
       0,  1250,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   488,   220,     0,     0,   221,     0,   222,   223,     0,
     224,     0,     0,     0,     0,     0,   136,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   226,     0,     0,
       0,     0,     0,     0,     0,   471,     0,     0,     0,   550,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,     0,     0,   367,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,     0,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,   366,     0,     0,  1477,     0,     0,  1481,  1484,     0,
     480,   480,   480,   270,     0,  1493,  1494,   480,   480,   102,
       0,   239,     0,     0,    85,   479,     0,     0,   690,     0,
     690,   242,    87,    88,   244,   245,   246,   247,  1506,     0,
    1508,     0,     0,  1511,     0,     0,  1515,     0,     0,   471,
    1519,     0,     0,     0,     0,     0,     0,     0,   480,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   136,     0,   181,     0,   181,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   480,     0,
     480,     0,     0,     0,   181,   181,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   181,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   367,     0,     0,
       0,     0,     0,     0,   801,     0,     0,     0,     0,     0,
       0,     0,   809,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     102,     0,     0,     0,     0,     0,     0,     0,     0,   480,
       0,     0,   270,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   367,     0,     0,     0,
     102,     0,     0,     0,     0,     0,     0,     0,   848,     0,
       0,     0,  1631,     0,     0,   550,     0,     0,     0,     0,
       0,     0,     0,     0,   136,     0,   865,     0,     0,     0,
       0,     0,     0,     0,   102,     0,     0,     0,     0,     0,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   889,     0,   136,     0,     0,     0,     0,  1481,
       0,     0,     0,     0,     0,   550,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     920,   922,     0,   270,     0,     0,     0,     0,   136,     0,
       0,     0,   929,     0,   931,     0,     0,     0,     0,     0,
       0,   865,     0,   941,   865,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,     0,     0,   945,   181,
     367,   550,     0,   181,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   270,     0,   982,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1773,     0,     0,     0,     0,     0,     0,   270,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   181,     0,     0,   270,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   801,     0,     0,     0,   848,   690,     0,     0,     0,
       0,     0,   690,     0,     0,     0,     0,   488,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   367,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     367,     0,     0,  1072,     0,     0,     0,     0,     0,     0,
       0,  1821,     0,     0,     0,     0,     0,   480,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   270,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
       0,     0,   181,   181,     0,     0,     0,     0,     0,     0,
     181,   181,     0,     0,     0,     0,   367,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   181,     0,   181,     0,     0,   181,     0,
       0,   181,     0,     0,     0,   181,     0,   367,   398,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   367,     0,     0,     0,     0,
       0,     0,  1908,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1163,     0,
       0,     0,  1917,  1918,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   270,     0,     0,     0,     0,     0,     0,   480,
       0,     0,  1931,  1932,     0,     0,     0,     0,     0,     0,
       0,     0,  1194,     0,     0,  1936,     0,  1201,     0,     0,
       0,     0,     0,     0,     0,  1201,   550,     0,     0,     0,
       0,     0,  1216,     0,     0,     0,     0,   865,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   690,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   181,     0,     0,
       0,     0,     0,     0,   367,     0,     0,     0,     0,     0,
       0,     0,     0,   301,     0,     0,     0,     0,     0,     0,
       0,   313,     0,  1999,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
       0,     0,     0,     0,   181,   920,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   690,     0,     0,     0,     0,     0,     0,   419,
       0,     0,     0,  1278,     0,     0,     0,     0,   313,  2063,
       0,   480,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   462,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   313,
       0,     0,     0,     0,     0,   480,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   181,     0,     0,
       0,     0,     0,     0,     0,     0,   546,   301,     0,     0,
     488,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   601,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   480,     0,     0,     0,     0,     0,
       0,     0,     0,   654,     0,     0,   865,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     480,     0,   480,   668,     0,     0,     0,   674,     0,     0,
       0,   301,     0,     0,   681,     0,     0,   690,     0,     0,
       0,     0,     0,     0,     0,     0,   181,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     480,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1201,     0,     0,   181,     0,     0,     0,     0,     0,
       0,   181,     0,     0,     0,     0,     0,  1491,     0,     0,
     301,   313,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,     0,
       0,   690,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   480,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   462,     0,   803,  1527,     0,  1528,     0,     0,
       0,   681,     0,     0,     0,     0,     0,   181,     0,     0,
     301,   313,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   181,   181,     0,
     301,   601,     0,   834,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1581,  1581,     0,     0,
       0,     0,     0,   550,     0,     0,     0,   181,   181,     0,
       0,     0,     0,   301,   313,   471,     0,     0,     0,     0,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   313,
       0,     0,   546,     0,   546,   313,     0,     0,   313,     0,
       0,     0,     0,     0,     0,     0,     0,   546,     0,     0,
     546,   546,   546,   462,     0,     0,     0,     0,     0,     0,
       0,  1634,     0,     0,     0,   914,     0,     0,   654,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   690,     0,
    1647,     0,     0,     0,     0,     0,     0,     0,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   301,
       0,     0,   946,     0,     0,     0,  1669,  1670,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   462,   601,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   301,
       0,   301,     0,   865,     0,     0,     0,   462,     0,  1022,
       0,     0,     0,     0,     0,   462,     0,     0,     0,   362,
       0,     0,   690,   181,     0,   488,     0,     0,     0,    14,
      15,    16,    17,    18,     0,   313,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,  -504,
    -504,     0,  -504,    52,     0,    53,     0,     0,  -504,     0,
       0,   313,   313,     0,     0,     0,     0,   301,     0,  1803,
       0,     0,  1805,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1647,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,  1825,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,   488,
       0,     0,     0,     0,     0,  1848,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,   914,     0,     0,
       0,     0,     0,  1898,     0,     0,     0,     0,     0,     0,
     313,     0,     0,    85,   678,     0,     0,     0,     0,     0,
    1563,    87,    88,     0,     0,     0,   313,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1895,     0,     0,
     462,     0,     0,     0,     0,     0,     0,   220,     0,     0,
     221,     0,   222,   223,     0,   224,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   690,   301,     0,  1210,
    1336,     0,   226,  1338,   462,  1339,   601,     0,  1340,  1341,
    1342,  1343,  1344,  1345,  1346,  1347,  1348,  1349,  1350,  1351,
    -355,  -355,  1352,  1353,  1354,  1355,  1356,  1357,  1358,     0,
    1359,     0,   227,   228,     0,   684,   230,  1360,  1361,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
    1362,     0,   234,   235,   236,   301,   237,   238,     0,     0,
       0,     0,     0,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1363,     0,     0,    85,
     479,     0,   914,   834,   404,     0,   242,    87,    88,   244,
     245,   246,   247,     0,     0,     0,     0,     0,     0,     0,
     301,  -196,     4,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
     301,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   220,     0,    52,   221,    53,   222,
     223,     0,   224,    54,    55,    56,    57,    58,    59,    60,
      61,    62,     0,   865,     0,    63,     0,     0,    64,   226,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     462,     0,     0,   301,  1488,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,   313,   227,
     228,    67,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,    80,   234,
     235,   236,     0,   237,   238,     0,     0,     0,     0,     0,
       0,    81,     0,    82,   220,     0,     0,   221,     0,   222,
     223,     0,   224,     0,     0,     0,  1737,  1738,  1739,  1740,
       0,     0,   546,   239,  1741,  1742,    85,  1364,    64,   226,
       0,     0,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,     0,     0,  1743,     0,
       0,     0,     0,     0,   301,   301,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,    80,   234,
     235,   236,     0,   237,   238,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,   301,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   301,   239,     0,     0,    85,   479,     0,     0,
       0,     0,     0,   242,  1489,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,   546,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
     546,   301,     0,    52,     0,    53,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2166,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,   375,     0,     0,     0,     0,  1563,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   313,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   196,
       0,   197,   198,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   220,    80,     0,   221,     0,   222,
     223,     0,   224,     0,     0,     0,     0,     0,     0,     0,
      82,   301,   301,     0,     0,     0,     0,  1336,     0,   226,
    1338,     0,  1339,  -256,  -256,  1340,  1341,  1342,  1343,  1344,
    1345,  1346,  1347,  1348,  1349,  1350,  1351,  -355,  -355,  1352,
    1353,  1354,  1355,  1356,  1357,  1358,     0,  1359,     0,   227,
     228,     0,   684,   230,  1360,  1361,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,  1362,     0,   234,
     235,   236,     0,   237,   238,     0,     0,     0,     0,     0,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   546,   546,   546,     0,     0,   546,   546,
       0,     0,  -256,  1363,     0,   681,    85,   479,     0,     0,
       0,   404,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,     0,     0,  -196,  2166,
       0,     0,     0,   313,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1563,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   220,     0,     0,   221,     0,   222,   223,
       0,   224,     0,     0,     0,     0,     0,     0,     0,   546,
       0,     0,     0,     0,     0,     0,  1336,     0,   226,  1338,
       0,  1339,  -257,  -257,  1340,  1341,  1342,  1343,  1344,  1345,
    1346,  1347,  1348,  1349,  1350,  1351,  -355,  -355,  1352,  1353,
    1354,  1355,  1356,  1357,  1358,     0,  1359,     0,   227,   228,
       0,   684,   230,  1360,  1361,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,  1362,   313,   234,   235,
     236,     0,   237,   238,     0,     0,     0,     0,   220,     0,
      81,   221,     0,   222,   223,     0,   224,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,  -257,  1363,   226,     0,    85,   479,     0,     0,     0,
     404,     0,   242,    87,    88,   244,   245,   246,   247,     0,
       0,     0,     0,     0,     0,     0,     0,  -196,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,     0,   234,   235,   236,     0,   237,   238,     0,
       0,     0,     0,     0,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1737,  1738,  1739,  1740,     0,     0,     0,   239,  1881,     0,
      85,   479,     0,     0,     0,     0,     0,   242,    87,    88,
     244,   245,   246,   247,     0,     0,     0,     0,     0,     0,
       4,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1335,     0,    20,   375,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,   546,   220,     0,    52,   221,    53,   222,   223,     0,
     224,    54,    55,    56,    57,    58,    59,    60,    61,    62,
       0,     0,     0,    63,     0,  1336,    64,  1337,  1338,     0,
    1339,     0,     0,  1340,  1341,  1342,  1343,  1344,  1345,  1346,
    1347,  1348,  1349,  1350,  1351,  -355,  -355,  1352,  1353,  1354,
    1355,  1356,  1357,  1358,     0,  1359,     0,   227,   228,    67,
     684,   230,  1360,  1361,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,  1362,    80,   234,   235,   236,
     546,   237,   238,   681,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -3,  1363,     0,     0,    85,  1364,     0,     0,     0,   404,
       0,   242,    87,    88,   244,   245,   246,   247,     0,     0,
       0,     0,     0,     0,     0,     0,  -196,     4,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,  1335,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   220,
       0,    52,   221,    53,   222,   223,     0,   224,    54,    55,
      56,    57,    58,    59,    60,    61,    62,     0,     0,     0,
      63,     0,  1336,    64,  1337,  1338,     0,  1339,     0,     0,
    1340,  1341,  1342,  1343,  1344,  1345,  1346,  1347,  1348,  1349,
    1350,  1351,  -355,  -355,  1352,  1353,  1354,  1355,  1356,  1357,
    1358,     0,  1359,     0,   227,   228,    67,   684,   230,  1360,
    1361,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,  1362,    80,   234,   235,   236,     0,   237,   238,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1363,     0,
       0,    85,  1364,     0,     0,     0,   404,     0,   242,    87,
      88,   244,   245,   246,   247,     0,     0,     0,     0,     0,
       0,     0,     0,  -196,     4,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   220,     0,    52,   221,
      53,   222,   223,     0,   224,    54,    55,    56,    57,    58,
      59,    60,    61,    62,     0,     0,     0,    63,     0,     0,
      64,   226,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   227,   228,    67,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
      80,   234,   235,   236,     0,   237,   238,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1737,  1738,
    1739,  1740,     0,     0,     0,   239,  1741,     0,    85,  1364,
       0,     0,     0,     0,     0,   242,    87,    88,   244,   245,
     246,   247,     0,     0,     0,     0,     0,     0,     0,     0,
    1743,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      76,    77,    78,     0,     0,     0,    79,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    83,    84,     0,    85,    86,     0,     0,     0,
       0,     0,     0,    87,    88,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    89,     0,    90,   362,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -504,  -504,
       0,  -504,    52,     0,    53,     0,     0,  -504,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   159,     0,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    83,
      84,     0,    85,   363,     0,     0,     0,  -840,     0,     0,
      87,    88,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    89,   362,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,  -504,  -504,     0,  -504,    52,     0,    53,
       0,     0,  -504,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   159,     0,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    83,    84,     0,    85,   363,     0,
       0,     0,     0,     0,     0,    87,    88,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    89,   195,     6,
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
       0,     0,     0,     0,     0,     0,     0,   159,     0,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,   784,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   969,    84,
    -705,    85,   627,     0,     0,     0,     0,     0,     0,    87,
      88,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    89,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -504,  -504,     0,  -504,    52,     0,    53,     0,     0,
    -504,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   159,     0,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    83,    84,     0,    85,   363,     0,     0,     0,
    -844,     0,     0,    87,    88,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    89,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -504,  -504,     0,  -504,    52,
       0,    53,     0,     0,  -504,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   159,     0,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    83,    84,     0,    85,
     363,     0,     0,     0,     0,     0,     0,    87,    88,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    89,
       4,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   220,     0,    52,   221,    53,   222,   223,     0,
     224,    54,    55,    56,    57,    58,    59,    60,    61,    62,
       0,     0,     0,    63,     0,     0,    64,   226,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   227,   228,    67,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,    80,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   239,     0,  1735,    85,  1364,     0,     0,     0,     0,
       0,   242,    87,    88,   244,   245,   246,   247,     4,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     220,     0,    52,   221,    53,   222,   223,     0,   224,    54,
      55,    56,    57,    58,    59,    60,    61,    62,     0,     0,
       0,    63,     0,     0,    64,   226,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   227,   228,    67,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,    80,   234,   235,   236,     0,   237,
     238,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   239,
       0,     0,    85,  1364,     0,     0,     0,     0,     0,   242,
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
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,    80,   234,   235,   236,     0,   237,   238,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   239,   547,     0,    85,
     240,   548,   549,     0,     0,     0,   242,   243,    88,   244,
     245,   246,   247,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   220,     0,    52,   221,    53,   222,
     223,     0,   224,   225,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   226,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,    80,   234,
     235,   236,     0,   237,   238,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   239,   547,     0,    85,   240,   677,   549,
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
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,    80,   234,   235,   236,     0,
     237,   238,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     239,   547,     0,    85,   592,   895,   549,     0,     0,     0,
     242,   243,    88,   244,   245,   246,   247,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   220,     0,
      52,   221,    53,   222,   223,     0,   224,   225,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,   226,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,    80,   234,   235,   236,     0,   237,   238,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   239,   547,     0,
      85,   240,   830,   549,     0,     0,     0,   242,   243,    88,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,    80,
     234,   235,   236,     0,   237,   238,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   239,   547,     0,    85,   592,  1005,
     549,     0,     0,     0,   242,   243,    88,   244,   245,   246,
     247,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   220,     0,    52,   221,    53,   222,   223,     0,
     224,   225,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,   226,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,    80,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   239,   547,     0,    85,   240,   241,   549,     0,     0,
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
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,    80,   234,   235,   236,     0,   237,   238,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   239,     0,
       0,    85,   240,   241,     0,     0,     0,     0,   242,   243,
      88,   244,   245,   246,   247,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   220,     0,    52,   221,
      53,   222,   223,     0,   224,   225,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,   226,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
      80,   234,   235,   236,     0,   237,   238,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   239,     0,     0,    85,   240,
     677,     0,     0,     0,     0,   242,   243,    88,   244,   245,
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
       0,     0,     0,     0,     0,     0,     0,     0,   227,   228,
       0,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,    80,   234,   235,
     236,     0,   237,   238,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   239,     0,     0,    85,   240,   830,     0,     0,
       0,     0,   242,   243,    88,   244,   245,   246,   247,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     220,     0,    52,   221,    53,   222,   223,     0,   224,   225,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,   226,     0,     0,     0,     0,
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
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,    80,   234,   235,   236,     0,   237,   238,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   239,     0,     0,    85,
     240,   548,     0,     0,     0,     0,   242,   243,    88,   244,
     245,   246,   247,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   220,     0,    52,   221,    53,   222,
     223,     0,   224,   225,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   226,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,    80,   234,
     235,   236,     0,   237,   238,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   239,     0,     0,    85,   592,   895,     0,
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
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,    80,   234,   235,   236,     0,
     237,   238,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     239,     0,     0,    85,   592,     0,     0,     0,     0,     0,
     242,   800,    88,   244,   245,   246,   247,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   220,     0,
      52,   221,    53,   222,   223,     0,   224,   225,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,   226,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,    80,   234,   235,   236,     0,   237,   238,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   239,     0,     0,
      85,   917,     0,     0,     0,     0,     0,   242,   918,    88,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,    80,
     234,   235,   236,     0,   237,   238,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   239,     0,     0,    85,   592,     0,
       0,     0,     0,     0,   242,   243,    88,   244,   245,   246,
     247,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   220,     0,    52,   221,    53,   222,   223,     0,
     224,   225,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,   226,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,    80,   234,   235,   236,
       0,   237,   238,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   239,     0,     0,    85,   479,     0,     0,     0,     0,
       0,   242,    87,    88,   244,   245,   246,   247,  2009,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,    -2,     0,    -2,     0,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,     0,     0,    -2,     0,     0,    -2,     0,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,    -2,    -2,  2039,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
      -2,     0,    -2,     0,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,
       0,     0,    -2,     0,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,  1122,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,    -2,    -2,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    -503,  -503,     0,  -503,    52,     0,    53,     0,     0,  -503,
       0,   225,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1318,     0,  1122,     0,    85,    86,     0,     0,     0,     0,
       0,     0,    87,    88,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -503,  -503,     0,  -503,    52,     0,
      53,     0,     0,  -503,     0,   225,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1405,     0,  1122,     0,    85,    86,
       0,     0,     0,     0,     0,     0,    87,    88,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -503,  -503,
       0,  -503,    52,     0,    53,     0,     0,  -503,     0,   225,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1532,     0,
    1122,     0,    85,    86,     0,     0,     0,     0,     0,     0,
      87,    88,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,  -503,  -503,     0,  -503,    52,     0,    53,     0,
       0,  -503,     0,   225,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1628,     0,  1122,     0,    85,    86,     0,     0,
       0,     0,     0,     0,    87,    88,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,  -503,  -503,     0,  -503,
      52,     0,    53,     0,     0,  -503,     0,   225,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1800,     0,  1122,     0,
      85,    86,     0,     0,     0,     0,     0,     0,    87,    88,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    -503,  -503,     0,  -503,    52,     0,    53,     0,     0,  -503,
       0,   225,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
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
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   159,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,     0,    85,    86,
       0,     0,     0,  -842,     0,     0,    87,    88,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,    89,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     159,     0,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    83,    84,     0,    85,   304,     0,     0,     0,     0,
       0,     0,    87,    88,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,    89,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   159,     0,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    83,    84,     0,
      85,    86,     0,     0,     0,     0,     0,     0,    87,    88,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      89,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,  -504,  -504,     0,  -504,    52,     0,    53,     0,
       0,  -504,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   159,     0,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    83,    84,     0,    85,   678,     0,     0,
       0,     0,     0,     0,    87,    88,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    89,     4,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,    62,     0,     0,     0,
      63,     0,     0,    64,     0,     0,     0,     0,  -422,  -422,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    67,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -422,     0,     0,
       0,    85,    86,     0,     0,     0,     0,     0,     0,    87,
      88,     4,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,    54,    55,    56,    57,    58,    59,    60,    61,
      62,     0,     0,     0,    63,     0,     0,    64,     0,     0,
       0,     0,  -423,  -423,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -423,     0,     0,     0,    85,    86,     0,  1539,     0,
    1540,     0,     0,    87,    88,  1541,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,  1542,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    67,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1543,     0,
       0,     0,    85,   883,     0,  1539,     0,  1540,     0,     0,
      87,    88,  1541,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,  1542,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    67,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1690,     0,     0,     0,    85,
     883,     0,  1539,     0,  1540,     0,     0,    87,    88,  1541,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,  1542,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    67,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1691,     0,     0,     0,    85,   883,     0,  1539,
       0,  1540,     0,     0,    87,    88,  1541,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,  1542,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    67,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1692,
       0,     0,     0,    85,   883,     0,     0,     0,     0,     0,
       0,    87,    88,   362,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,  -504,  -504,     0,  -504,    52,     0,    53,
       0,     0,  -504,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,   363,     0,
       0,     0,     0,     0,     0,    87,    88,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,   225,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,   784,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   831,     0,  -705,
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
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,   784,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     851,     0,  -705,    85,   724,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,  1204,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -713,    85,   750,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   452,    85,
     453,     0,     0,     0,     0,     0,     0,    87,    88,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   225,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,   750,   751,     0,     0,     0,     0,     0,
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
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,  1649,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,   750,     0,     0,     0,
       0,     0,     0,    87,    88,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,   225,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,  1651,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   750,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    85,   650,     0,     0,     0,     0,     0,     0,    87,
      88,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,   225,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    85,   750,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,   453,     0,
       0,     0,     0,     0,     0,    87,    88,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,  -504,  -504,     0,  -504,
      52,     0,    53,     0,     0,  -504,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
    1563,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   220,     0,     0,
     221,     0,   222,   223,     0,   224,     0,     0,     0,     0,
      85,   363,     0,     0,     0,     0,     0,     0,    87,    88,
    1336,     0,   226,  1338,     0,  1339,  1939,  1940,  1340,  1341,
    1342,  1343,  1344,  1345,  1346,  1347,  1348,  1349,  1350,  1351,
       0,     0,  1352,  1353,  1354,  1355,  1356,  1357,  1358,     0,
    1359,     0,   227,   228,     0,   684,   230,  1360,  1361,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
    1362,     0,   234,   235,   236,     0,   237,   238,     0,     0,
       0,     0,     0,     0,    81,     0,     0,  1563,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1363,     0,     0,    85,
     479,     0,     0,     0,   404,     0,   242,    87,    88,   244,
     245,   246,   247,     0,   220,     0,     0,   221,     0,   222,
     223,  -196,   224,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1336,     0,   226,
    1338,     0,  1339,     0,     0,  1340,  1341,  1342,  1343,  1344,
    1345,  1346,  1347,  1348,  1349,  1350,  1351,     0,     0,  1352,
    1353,  1354,  1355,  1356,  1357,  1358,     0,  1359,     0,   227,
     228,     0,   684,   230,  1360,  1361,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,  1362,     0,   234,
     235,   236,     0,   237,   238,     0,     0,     0,     0,     0,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1363,     0,     0,    85,   479,     0,     0,
       0,   404,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,     0,     0,  -196,   408,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -426,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,     0,     0,     0,     0,  -426,   408,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -427,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,     0,     0,     0,     0,  -427,   408,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,     0,     0,     0,     0,  -426,    14,
      15,    16,    17,    18,    19,   533,    20,   534,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   220,     0,    52,   221,    53,   222,   223,     0,   224,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   226,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   535,     0,
       0,     0,     0,  1351,     0,  -355,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,    80,   234,   235,   236,     0,
     237,   238,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1363,     0,     0,    85,   536,     0,     0,     0,   404,     0,
     242,    87,    88,   537,   538,   246,   247,    14,    15,    16,
      17,    18,    19,   533,    20,   534,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   220,
       0,    52,   221,    53,   222,   223,     0,   224,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   226,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   535,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,    80,   234,   235,   236,     0,   237,   238,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   239,     0,
       0,    85,   536,     0,     0,     0,   404,     0,   242,    87,
      88,   537,   538,   246,   247,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   220,     0,    52,
     221,    53,   222,   223,     0,   224,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   226,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,    80,   234,   235,   236,     0,   237,   238,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   239,     0,   485,    85,
     486,     0,     0,     0,     0,     0,   242,    87,    88,   244,
     245,   246,   247,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   220,     0,    52,   221,    53,
     222,   223,     0,   224,    54,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     226,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,    80,
     234,   235,   236,     0,   237,   238,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   239,     0,     0,    85,   486,     0,
       0,     0,   404,     0,   242,    87,    88,   244,   245,   246,
     247,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   220,     0,    52,   221,    53,   222,   223,
       0,   224,    54,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   226,     0,
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
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,    80,   234,   235,   236,     0,
     237,   238,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     239,     0,     0,    85,   592,     0,     0,     0,     0,     0,
     242,    87,    88,   244,   245,   246,   247,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   220,
       0,    52,   221,    53,   222,   223,     0,   224,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   226,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,    80,   234,   235,   236,     0,   237,   238,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   239,     0,
       0,    85,   486,     0,     0,     0,     0,     0,   242,    87,
      88,   244,   245,   246,   247,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   220,     0,    52,
     221,    53,   222,   223,     0,   224,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   226,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,    80,   234,   235,   236,     0,   237,   238,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   239,     0,     0,    85,
     479,     0,     0,     0,     0,     0,   242,    87,    88,   244,
     245,   246,   247,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   524,   220,   525,   526,   221,     0,   222,   223,
       0,   224,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,    64,   226,     0,
       0,     0,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   227,   228,
     -17,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,    80,   234,   235,
     236,     0,   237,   238,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1737,  1738,  1739,  1740,     0,
       0,     0,   239,  1954,     0,    85,   479,     0,     0,     0,
       0,     0,   242,    87,    88,   244,   245,   246,   247,   362,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,  -504,
    -504,     0,  -504,    52,     0,    53,     0,     0,  -504,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,     0,     0,     0,     0,     0,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,    85,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   159,     0,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    82,   784,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -705,    85,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,   225,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     159,     0,   553,    70,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,     0,     0,    80,     0,     0,     0,
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
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   159,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,    86,
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
       0,     0,     0,     0,     0,     0,     0,     0,   159,     0,
     553,    70,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,   554,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    82,   784,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -705,    85,   195,     6,     7,     8,
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
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    82,  1329,     0,     0,
       0,     0,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,    85,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   225,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,    85,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     891,    85,   883,     0,     0,     0,     0,     0,     0,    87,
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
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   910,     0,     0,    85,   554,     0,     0,     0,
       0,     0,     0,    87,    88,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   910,     0,     0,    85,
     670,     0,     0,     0,     0,     0,     0,    87,    88,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,  1440,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,   883,     0,     0,     0,     0,     0,
       0,    87,    88,    14,    15,    16,    17,    18,    19,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,   415,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    85,   304,     0,     0,     0,     0,     0,     0,    87,
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
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,   415,     0,     0,     0,
       0,     0,     0,    87,    88,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   225,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,   682,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,   453,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    85,   883,     0,     0,     0,     0,     0,     0,    87,
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
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,   650,     0,     0,     0,
       0,     0,     0,    87,    88,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -504,  -504,     0,  -504,    52,     0,
      53,     0,     0,  -504,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,    85,    21,
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
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    85,   554,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   433,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,    86,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,   883,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,   678,     0,     0,     0,     0,     0,     0,    87,    88,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,   225,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    85,     0,     0,    14,    15,    16,
      17,    18,    87,    88,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,  -504,  -504,     0,
    -504,    52,     0,    53,     0,     0,  -504,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    85,   433,     0,    14,    15,    16,    17,    18,    87,
      88,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -504,  -504,     0,  -504,    52,     0,
      53,     0,     0,  -504,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   678,
       0,    14,    15,    16,    17,    18,    87,    88,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -504,  -504,     0,  -504,    52,     0,    53,     0,     0,
    -504,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,     0,     0,     0,     0,
       0,     0,     0,    87,    88,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     220,     0,    52,   221,    53,   222,   223,     0,   224,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   226,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,     0,   234,   235,   236,     0,   237,
     238,     0,     0,     0,     0,     0,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   239,
       0,     0,    85,   479,  1071,     0,     0,     0,     0,   242,
     243,    88,   244,   245,   246,   247,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   220,     0,    52,   221,    53,   222,   223,     0,   224,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   226,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,     0,   234,   235,   236,     0,
     237,   238,     0,     0,     0,     0,     0,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     239,     0,     0,    85,   479,     0,     0,     0,     0,     0,
     242,    87,    88,   244,   245,   246,   247,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,    85,    21,    22,
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
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,    20,    85,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -504,  -504,     0,  -504,    52,
       0,    53,     0,     0,  -504,     0,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    64,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,    69,    70,    52,
       0,    53,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,    82,     0,     0,   220,
       0,     0,   221,     0,   222,   223,     0,   224,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    85,
       0,     0,     0,     0,   226,   524,     0,   525,   526,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   227,   228,    82,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,     0,   234,   235,   236,     0,   237,   238,
       0,     0,     0,     0,     0,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   239,     0,
       0,    85,   479,     0,     0,     0,     0,     0,   242,    87,
      88,   244,   245,   246,   247,    14,    15,    16,    17,    18,
       0,   861,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -503,  -503,     0,  -503,    52,
       0,    53,     0,     0,  -503,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
      20,    64,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,  -504,  -504,     0,  -504,    52,     0,    53,
       0,     0,  -504,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,    64,
      14,    15,    16,    17,    18,     0,    82,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    82,     0,    64,     0,     0,     0,
       0,     0,     0,     0,   220,     0,     0,   221,     0,   222,
     223,     0,   224,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   226,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   227,
     228,    82,   684,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,     0,   234,
     235,   236,   220,   237,   238,   221,     0,   222,   223,     0,
     224,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   226,     0,     0,
       0,     0,     0,   239,    84,     0,   685,   686,     0,     0,
       0,   687,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,     0,   234,   235,   236,
     220,   237,   238,   221,     0,   222,   223,     0,   224,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   226,     0,     0,     0,     0,
       0,   239,   547,     0,    85,   479,     0,   549,     0,     0,
       0,   242,    87,    88,   244,   245,   246,   247,     0,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,     0,   234,   235,   236,   220,   237,
     238,   221,     0,   222,   223,     0,   224,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   226,     0,     0,     0,     0,     0,   239,
    1197,     0,    85,   479,     0,     0,     0,  1198,     0,   242,
      87,    88,   244,   245,   246,   247,     0,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,     0,   234,   235,   236,   220,   237,   238,   221,
       0,   222,   223,     0,   224,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   226,     0,     0,     0,     0,     0,   239,  1200,     0,
      85,   479,  1211,     0,     0,     0,     0,   242,    87,    88,
     244,   245,   246,   247,     0,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
       0,   234,   235,   236,   220,   237,   238,   221,     0,   222,
     223,     0,   224,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   226,
       0,     0,     0,     0,     0,   239,     0,     0,    85,   479,
       0,     0,     0,   687,     0,   242,    87,    88,   244,   245,
     246,   247,     0,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,     0,   234,
     235,   236,   220,   237,   238,   221,     0,   222,   223,     0,
     224,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   226,     0,     0,
       0,     0,     0,   239,     0,     0,    85,   479,     0,     0,
       0,   404,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,     0,   234,   235,   236,
     220,   237,   238,   221,     0,   222,   223,     0,   224,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   226,     0,     0,     0,     0,
       0,   239,     0,     0,    85,   479,     0,     0,   966,     0,
       0,   242,    87,    88,   244,   245,   246,   247,     0,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,     0,   234,   235,   236,   220,   237,
     238,   221,     0,   222,   223,     0,   224,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   226,     0,     0,     0,     0,     0,   239,
       0,     0,    85,   479,   979,     0,     0,     0,     0,   242,
     243,    88,   244,   245,   246,   247,     0,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,     0,   234,   235,   236,   220,   237,   238,   221,
       0,   222,   223,     0,   224,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   226,     0,     0,     0,     0,     0,   239,     0,     0,
      85,   479,  1003,     0,     0,     0,     0,   242,    87,    88,
     244,   245,   246,   247,     0,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
       0,   234,   235,   236,   220,   237,   238,   221,     0,   222,
     223,     0,   224,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   226,
       0,     0,     0,     0,     0,   239,  1200,     0,    85,   479,
       0,     0,     0,     0,     0,   242,    87,    88,   244,   245,
     246,   247,     0,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,     0,   234,
     235,   236,   220,   237,   238,   221,     0,   222,   223,     0,
     224,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   226,     0,     0,
       0,     0,     0,   239,     0,     0,    85,   479,     0,     0,
       0,  1573,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,     0,   227,   228,     0,
     229,   230,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   231,   232,   233,     0,     0,   234,   235,   236,
     220,   237,   238,   221,     0,   222,   223,     0,   224,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   226,     0,     0,     0,     0,
       0,   239,  1646,     0,    85,   479,     0,     0,     0,     0,
       0,   242,    87,    88,   244,   245,   246,   247,     0,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   229,   230,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     231,   232,   233,     0,     0,   234,   235,   236,   220,   237,
     238,   221,     0,   222,   223,     0,   224,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   226,     0,     0,     0,     0,     0,   239,
       0,     0,    85,   479,     0,     0,     0,  1797,     0,   242,
      87,    88,   244,   245,   246,   247,     0,     0,     0,     0,
       0,     0,     0,   227,   228,     0,   229,   230,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   231,   232,
     233,     0,     0,   234,   235,   236,   220,   237,   238,   221,
       0,   222,   223,     0,   224,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   226,     0,     0,     0,     0,     0,   239,     0,  1945,
      85,   479,     0,     0,     0,     0,     0,   242,    87,    88,
     244,   245,   246,   247,     0,     0,     0,     0,     0,     0,
       0,   227,   228,     0,   229,   230,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   231,   232,   233,     0,
       0,   234,   235,   236,   220,   237,   238,   221,     0,   222,
     223,     0,   224,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   226,
       0,     0,     0,     0,     0,   239,  1950,     0,    85,   479,
       0,     0,     0,     0,     0,   242,    87,    88,   244,   245,
     246,   247,     0,     0,     0,     0,     0,     0,     0,   227,
     228,     0,   229,   230,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   231,   232,   233,     0,     0,   234,
     235,   236,     0,   237,   238,     0,     0,   220,     0,     0,
     221,    81,   222,   223,     0,   224,  2026,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   226,   239,  1960,     0,    85,   479,     0,     0,
       0,     0,     0,   242,    87,    88,   244,   245,   246,   247,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,     0,   234,   235,   236,   220,   237,   238,   221,     0,
     222,   223,     0,   224,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     226,     0,     0,     0,     0,     0,   239,     0,     0,    85,
     479,     0,     0,     0,     0,     0,   242,    87,    88,   244,
     245,   246,   247,     0,     0,     0,     0,     0,     0,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,     0,
     234,   235,   236,   220,   237,   238,   221,     0,   222,   223,
       0,   224,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   226,     0,
       0,     0,     0,     0,   239,  2033,     0,    85,   479,     0,
       0,     0,     0,     0,   242,    87,    88,   244,   245,   246,
     247,     0,     0,     0,     0,     0,     0,     0,   227,   228,
       0,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,     0,   234,   235,
     236,   220,   237,   238,   221,     0,   222,   223,     0,   224,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   226,     0,     0,     0,
       0,     0,   239,  2035,     0,    85,   479,     0,     0,     0,
       0,     0,   242,    87,    88,   244,   245,   246,   247,     0,
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,     0,   234,   235,   236,   220,
     237,   238,   221,     0,   222,   223,     0,   224,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   226,     0,     0,     0,     0,     0,
     239,  2082,     0,    85,   479,     0,     0,     0,     0,     0,
     242,    87,    88,   244,   245,   246,   247,     0,     0,     0,
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,     0,   234,   235,   236,   220,   237,   238,
     221,     0,   222,   223,     0,   224,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   226,     0,     0,     0,     0,     0,   239,  2084,
       0,    85,   479,     0,     0,     0,     0,     0,   242,    87,
      88,   244,   245,   246,   247,     0,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,     0,   234,   235,   236,   220,   237,   238,   221,     0,
     222,   223,     0,   224,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     226,     0,     0,     0,     0,     0,   239,  2086,     0,    85,
     479,     0,     0,     0,     0,     0,   242,    87,    88,   244,
     245,   246,   247,     0,     0,     0,     0,     0,     0,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,     0,
     234,   235,   236,   220,   237,   238,   221,     0,   222,   223,
       0,   224,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   226,     0,
       0,     0,     0,     0,   239,  2091,     0,    85,   479,     0,
       0,     0,     0,     0,   242,    87,    88,   244,   245,   246,
     247,     0,     0,     0,     0,     0,     0,     0,   227,   228,
       0,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,     0,   234,   235,
     236,   220,   237,   238,   221,     0,   222,   223,     0,   224,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   226,     0,     0,     0,
       0,     0,   239,  2093,     0,    85,   479,     0,     0,     0,
       0,     0,   242,    87,    88,   244,   245,   246,   247,     0,
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,     0,   234,   235,   236,   220,
     237,   238,   221,     0,   222,   223,     0,   224,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   226,     0,     0,     0,     0,     0,
     239,  2129,     0,    85,   479,     0,     0,     0,     0,     0,
     242,    87,    88,   244,   245,   246,   247,     0,     0,     0,
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,     0,   234,   235,   236,   220,   237,   238,
     221,     0,   222,   223,     0,   224,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   226,     0,     0,     0,     0,     0,   239,  2131,
       0,    85,   479,     0,     0,     0,     0,     0,   242,    87,
      88,   244,   245,   246,   247,     0,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,     0,   234,   235,   236,   220,   237,   238,   221,     0,
     222,   223,     0,   224,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     226,     0,     0,     0,     0,     0,   239,  2133,     0,    85,
     479,     0,     0,     0,     0,     0,   242,    87,    88,   244,
     245,   246,   247,     0,     0,     0,     0,     0,     0,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,     0,
     234,   235,   236,   220,   237,   238,   221,     0,   222,   223,
       0,   224,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   226,     0,
       0,     0,     0,     0,   239,  2152,     0,    85,   479,     0,
       0,     0,     0,     0,   242,    87,    88,   244,   245,   246,
     247,     0,     0,     0,     0,     0,     0,     0,   227,   228,
       0,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,     0,   234,   235,
     236,   220,   237,   238,   221,     0,   222,   223,     0,   224,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   226,     0,     0,     0,
       0,     0,   239,  2154,     0,    85,   479,     0,     0,     0,
       0,     0,   242,    87,    88,   244,   245,   246,   247,     0,
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,     0,   234,   235,   236,   220,
     237,   238,   221,     0,   222,   223,     0,   224,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   226,     0,     0,     0,     0,     0,
     239,  2156,     0,    85,   479,     0,     0,     0,     0,     0,
     242,    87,    88,   244,   245,   246,   247,     0,     0,     0,
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,     0,   234,   235,   236,   220,   237,   238,
     221,     0,   222,   223,     0,   224,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   226,     0,     0,     0,     0,     0,   239,     0,
       0,    85,   479,     0,     0,     0,     0,     0,   242,    87,
      88,   244,   245,   246,   247,     0,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,     0,   234,   235,   236,   220,   237,   238,   221,     0,
     222,   223,     0,   224,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     226,     0,     0,     0,     0,     0,   512,     0,     0,    85,
     479,     0,     0,     0,     0,     0,   242,    87,    88,   244,
     245,   246,   247,     0,     0,     0,     0,     0,     0,     0,
     227,   228,     0,   229,   230,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   231,   232,   233,     0,     0,
     234,   235,   236,   220,   237,   238,   221,     0,   222,   223,
       0,   224,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   226,     0,
       0,     0,     0,     0,   515,     0,     0,    85,   479,     0,
       0,     0,     0,     0,   242,    87,    88,   244,   245,   246,
     247,     0,     0,     0,     0,     0,     0,     0,   227,   228,
       0,   229,   230,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   231,   232,   233,     0,     0,   234,   235,
     236,   220,   237,   238,   221,     0,   222,   223,     0,   224,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   226,     0,     0,     0,
       0,     0,   521,     0,     0,    85,   479,     0,     0,     0,
       0,     0,   242,    87,    88,   244,   245,   246,   247,     0,
       0,     0,     0,     0,     0,     0,   227,   228,     0,   229,
     230,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   231,   232,   233,     0,     0,   234,   235,   236,   220,
     237,   238,   221,     0,   222,   223,     0,   224,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   226,     0,     0,     0,     0,     0,
     530,     0,     0,    85,   479,     0,     0,     0,     0,     0,
     242,    87,    88,   244,   245,   246,   247,     0,     0,     0,
       0,     0,     0,     0,   227,   228,     0,   229,   230,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   231,
     232,   233,     0,     0,   234,   235,   236,   220,   237,   238,
     221,     0,   222,   223,     0,   224,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   226,     0,     0,     0,     0,     0,   239,     0,
       0,    85,   479,     0,     0,     0,     0,     0,   242,   243,
      88,   244,   245,   246,   247,     0,     0,     0,     0,     0,
       0,     0,   227,   228,     0,   229,   230,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   231,   232,   233,
       0,     0,   234,   235,   236,     0,   237,   238,     0,     0,
       0,     0,     0,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   239,     0,     0,    85,
     479,     0,     0,     0,     0,     0,   242,   800,    88,   244,
     245,   246,   247
};

static const yytype_int16 yycheck[] =
{
       1,   189,   698,     4,   239,    83,   572,   630,    83,   575,
      83,   177,   139,  1134,   321,   193,    83,   377,   518,     1,
     361,   179,    65,     1,   148,  1127,   276,   276,   662,  1363,
     664,  1382,    83,   335,  1413,  1414,   179,  1140,  1363,   540,
       3,   687,   630,  1832,  1939,  1148,   626,     1,   194,   626,
     335,   916,  1092,   152,     4,   630,    83,    83,   330,   852,
     335,    62,    63,   345,    65,   242,   335,  1831,   335,   626,
     692,   626,   626,   177,  1748,   335,   626,    85,  1831,   335,
    1346,    82,    83,    65,   356,     1,   191,    65,    89,     1,
    1108,  1831,   374,    94,    97,     1,   194,    68,    69,    91,
     133,   102,   651,   385,   102,    82,   107,  1096,   170,   110,
     127,    65,    94,   114,  1943,   158,    82,     1,   180,   172,
      94,   626,   143,   425,     1,   107,   179,     4,   110,   335,
    1119,   274,   114,   631,   167,   210,     1,   210,  1927,   637,
     425,   335,   241,   210,     1,   910,    84,    85,    79,    65,
     425,   168,   153,    65,   107,   156,   425,   158,   425,   210,
       1,   164,    96,   164,   114,   425,   187,   107,   449,   425,
     171,  1328,   164,   108,   107,  2070,   158,  1108,   179,     1,
     158,    65,     4,   210,   210,   344,  1975,   161,    65,   938,
     191,     0,   351,   474,     0,   170,   115,   116,   129,   335,
      65,   172,  1876,   968,   158,   180,   456,   456,   209,   210,
     833,   310,   189,   144,   373,    84,    85,  1319,   189,   425,
     145,   345,  1223,   189,    65,   384,    82,   209,   757,   164,
       1,   425,   148,   110,   172,   164,   189,   114,   172,   768,
     241,    97,   158,    65,   744,   745,   158,   296,   167,   114,
     374,   300,     0,    82,   831,   180,  2045,   335,  1023,   188,
     335,   385,   335,  1012,   148,   171,   127,  1601,   335,    98,
    1604,  1605,   273,   274,   158,   119,   831,  1026,  1043,   164,
    2044,   158,   107,   148,   335,    10,   410,   816,   117,   425,
    2119,  2044,   114,   158,    65,   422,   297,   164,   944,   143,
     301,   551,   551,   172,  2044,   463,   307,   148,   335,   335,
     172,   415,   470,   170,   171,  1418,  2105,   158,   319,  1337,
     463,  2150,   166,   324,   240,   307,   831,   189,   329,   330,
     331,  1320,   119,   118,   335,   336,   158,   514,   573,   110,
     338,   172,   340,   843,   164,   595,   595,   425,   179,   347,
     425,  1116,   425,  1371,   336,   356,   143,   142,   425,   859,
     983,   161,   928,   364,   168,   366,  1159,  1232,   728,   173,
     919,   476,   172,   632,   375,   376,   736,   636,   379,  1536,
    1537,  1538,   364,   642,  1424,   386,   693,   158,   188,   969,
     364,   399,   969,    20,   163,   983,   336,   379,   899,   400,
     401,   170,   403,   336,   706,   655,  1337,   408,   983,  1031,
     669,   412,   969,   164,   969,   969,   666,   676,   419,   969,
     189,   706,   420,   421,   425,   426,   170,   164,   239,   345,
     412,   706,   536,   164,   158,  1131,   180,   706,   163,   706,
    1371,   404,   443,   444,   426,   170,   706,   448,  1782,   174,
     706,   177,  1217,  1218,   179,  1744,   180,  1782,   374,   558,
     434,   345,   463,   386,   969,   170,  1467,  1468,  1469,   385,
     742,   472,   412,   971,   475,   476,   748,  1743,   401,   412,
     345,   189,  1748,  1328,   189,   767,   426,   630,   164,     1,
     374,  2073,     4,   426,   410,   496,   172,   498,   664,   548,
     706,   385,   379,   649,   345,   276,  1328,    96,   810,   374,
    1275,   336,   706,   487,   189,  1138,   498,  1620,   519,  2101,
     385,   170,   170,   701,  1146,   810,   410,  1629,   170,   255,
    1909,  1316,   145,   374,    79,   810,   786,   453,  1323,  2121,
     189,   810,    94,   810,   385,   410,   650,   548,   170,   414,
     810,   649,  1198,    65,   810,   107,   169,   164,   110,   166,
     170,   168,   114,   564,  1093,   566,   670,   180,   569,   410,
     706,    83,   573,   732,   678,   576,   170,    89,   677,   189,
    1202,   170,    94,   172,   129,    10,   170,   412,   686,   189,
    1212,   145,  1881,  1882,   170,   107,     3,   172,   110,   144,
     759,   426,   114,    82,   810,   189,    79,   766,   379,    76,
    1876,   770,  1234,   189,   189,   968,   810,  1402,     3,   164,
      99,   175,   176,   672,   167,   626,   170,   168,  1262,   630,
    1561,   706,   173,   706,  1565,  1566,   180,  1988,   158,   706,
     170,    65,   413,   767,    68,    69,   158,    71,  1579,   169,
     170,   659,   164,   654,   165,   706,   129,   209,   659,   660,
     180,   172,   625,   170,   627,  1954,  1955,   179,   170,  1790,
    1023,   144,   850,   923,   810,   170,   592,   685,   180,   706,
     706,   839,   189,   170,   685,   456,   687,  1953,  1791,   415,
    1043,  1536,  1537,  1538,   189,   168,   164,   209,   210,   170,
    1966,   512,   189,   226,   515,   706,   517,   164,   170,   166,
     521,   170,   675,  1462,  1536,  1537,  1538,   718,   189,   530,
     683,   180,   533,   534,   535,   170,  1033,   189,   454,   241,
     138,   139,   255,   256,   650,   810,   718,   810,   163,   702,
     164,   742,   170,   810,   189,  1243,  1244,   748,  1533,   174,
     751,  1010,   167,   168,   179,   307,  1006,  1006,   170,  2025,
     172,   189,   274,  1116,   170,   142,    13,    14,    15,    16,
      17,   170,   815,   188,   164,   172,   184,   185,   718,   883,
     551,   830,   172,   189,   336,   718,   188,   164,   166,   179,
     189,   168,   170,   166,  1416,   307,   895,  1069,   175,   176,
     172,  1423,   803,  1449,   177,   178,   804,   319,   164,   810,
     536,   812,   364,   917,   815,  1438,   817,   329,   330,   331,
    1454,  1455,   164,   335,   336,   826,   166,   379,  1450,   171,
     812,   171,    79,   815,   750,   166,   166,   815,   981,   170,
     983,   171,   472,   806,   356,   475,     5,   157,   158,   159,
     160,   767,   364,  1125,    13,    14,    15,    16,    17,   186,
     861,   815,   164,   912,  1217,  1218,   175,   379,   966,   819,
     180,   166,   812,   182,   183,   170,  1126,   115,   116,   812,
    1576,   166,   129,   767,   655,   127,   171,   166,   400,   166,
      79,   403,   171,   718,   171,   142,   408,   144,   164,   815,
     412,   166,   767,   815,  1264,   170,  1247,    53,    54,  1658,
      56,  1660,  1412,   425,   426,   164,    62,   164,  1693,   168,
      79,   168,  1275,  1698,   650,   651,   767,  1573,   175,   176,
     164,   815,   166,   934,   168,   166,  1095,   938,   815,  1037,
     129,    79,   819,   944,   670,   166,   498,  1113,  1879,   170,
     815,   463,   678,    79,   166,   144,  1005,   166,   481,   168,
     166,   924,   925,    65,   487,  1272,    68,    69,   969,    71,
     129,   166,     3,   166,   815,   164,   166,   170,   166,   168,
     981,   164,   983,  1141,   169,   144,   498,   812,  1554,   169,
     170,   129,   164,   815,    68,   164,   164,   819,   166,  1621,
     168,   917,     1,   129,  1700,     4,   144,   519,   164,  1113,
     166,  1012,   168,   166,   127,   786,   166,   170,   144,   166,
     170,   164,  1644,   170,  1025,  1026,   164,   164,   799,   164,
     168,   168,  1033,   168,   176,  1036,   548,   142,   164,   142,
     181,   115,   168,   166,   815,   174,   120,   170,   142,   123,
     186,   125,   564,   169,   566,   167,   164,   569,   164,   164,
     168,   164,   168,   168,   576,   168,    65,   145,  1069,   164,
     175,   176,   175,   176,   145,  1179,   166,   164,  1041,  2010,
     170,   604,   113,   188,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    22,    94,  1262,   170,    13,    14,
      15,    16,    17,   164,   166,  1106,  1107,  1108,   170,  2040,
    1237,   142,   164,   166,   626,   166,  1114,   170,   630,   170,
    1266,  1248,  1268,   166,  1125,   166,   164,   170,  1363,   170,
    1108,  1753,   108,   164,   165,  2106,  1263,  1409,  1236,  2110,
     171,   170,  1092,  2074,   175,   176,  1147,   659,   660,   148,
    1277,  1797,   923,   152,  1108,   229,   187,   883,   164,   158,
     177,   178,   161,   164,    79,  1525,   718,    79,  1266,   172,
    1268,  1793,    13,   685,   166,   687,    96,  1127,   170,   166,
    1181,   136,   137,   170,   172,    13,    14,    15,    16,    17,
     163,   917,  1108,   919,   706,   189,  1108,  1198,   172,    13,
      14,    15,    16,    17,   278,   279,   718,   281,   164,   283,
     169,   170,   142,   166,   129,  1092,   166,   129,   172,    79,
     170,   179,  1223,   188,  1108,  1413,  1414,   142,   166,   144,
     742,  1108,   144,   166,   164,  1006,   748,   170,   168,   751,
     140,   141,   241,  1108,  1371,   175,   176,   166,  1249,   164,
    1127,    79,   164,   168,    95,   166,   168,  1415,   166,   170,
     175,   176,   170,   169,   170,    79,   168,  1108,    79,   129,
    1092,   166,   113,  1400,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   144,   166,  1108,   166,  1910,  1455,
     166,   170,  1914,   166,   368,  1561,   166,   170,   810,  1565,
     812,   129,  1300,   815,   164,  1127,  1541,   166,   168,   115,
     116,   310,   166,   167,   142,   129,   144,   166,   129,   612,
     613,   614,   615,   164,   605,   606,   607,    79,   175,   176,
     144,  1458,  1459,   144,   169,   170,   164,  1108,   170,  1340,
     168,   166,  1343,  1344,  1345,   169,   345,   175,   176,   861,
     172,  1352,  1315,   164,  1328,   170,   171,   168,    18,  1337,
     169,   170,  1489,   362,  1491,   364,   169,   170,   891,  1319,
    1371,   169,   170,   896,  1185,   374,  1377,   129,   169,   170,
    1507,   169,   170,  1337,   907,   164,   385,  1113,  1325,  1326,
    1327,  1392,   144,  1371,  1395,  1396,   919,  1398,  1690,  1691,
    1692,   164,  1403,  1705,   169,   170,  1407,   188,  1409,   408,
     172,   410,   164,  1395,  1396,   168,   168,  1371,  1984,   170,
    1705,  1337,   934,   169,   170,  1337,   938,  1713,  1714,  1715,
    1705,   171,   944,   169,   170,   434,  1705,  1561,  1705,    98,
      99,  1565,  1319,   169,   170,  1705,  1396,   171,  1449,  1705,
     164,    79,    79,  1337,   188,  1371,   166,   969,   166,  1371,
    1337,  1462,   169,   170,   169,   170,  1467,  1468,  1469,   981,
     166,   983,  1337,   166,  1424,   157,   158,   159,   160,  1625,
      13,    14,    15,    16,    17,    18,   166,  1371,   487,    13,
      14,    15,    16,    17,  1371,    79,  1337,  1319,   180,  1705,
    1012,   129,   129,   169,   170,   188,  1371,   189,   169,   170,
     166,  1705,   170,   171,  1026,  1337,   144,   144,  1395,  1396,
     166,  1033,    84,    85,  1036,   170,   171,  1625,  1224,  1225,
    1371,  1396,   608,   609,   616,   617,   164,   164,   610,   611,
     168,   168,  1669,  1670,  1846,   129,  1547,  1424,   166,  1371,
    1551,  1552,  1363,    79,   166,    79,   164,  1069,   172,   558,
     144,  1846,  1536,  1537,  1538,   172,  1337,  1541,  1542,  1705,
     172,  1846,  1573,   172,  1396,  1604,  1605,  1846,   172,  1846,
     164,  1714,  1715,  1561,   168,   166,  1846,  1565,  1566,  1539,
    1846,   115,   116,  1771,  1106,   170,  1597,  1598,    77,   189,
    1371,  1579,  1424,   129,   169,   129,  1607,  1547,   169,   169,
     684,   170,   164,  1125,  1547,    85,    18,   172,   144,   255,
     144,   172,  1623,     3,  1395,  1607,   189,  1705,   120,   121,
     122,   123,   124,    13,    14,    15,    16,    17,   164,   170,
    1846,   166,   168,   167,    79,  1561,   166,   170,   166,  1565,
    1566,   170,  1846,   166,   170,   166,   166,  1658,   166,  1660,
     296,   170,  1539,  1579,   300,  1188,  1189,  1190,   166,  1181,
     166,   166,  1195,  1196,   163,   166,   166,  1561,   677,  1629,
     166,  1565,  1566,   166,  1561,   169,  1198,   169,  1565,  1566,
     764,   169,  1693,   169,   129,  1579,  1561,  1698,    79,    79,
    1565,  1566,  1579,  1975,  1705,   169,  1707,   169,   166,   144,
    1846,  1223,   166,   166,  1579,  1716,    79,  1539,  1896,   166,
    1561,  1909,  1547,   166,  1565,  1566,  1976,   166,   166,   164,
    1607,   163,   166,   168,  1735,   166,   169,  1249,  1579,  1561,
     170,  1742,  2044,  1565,  1566,   169,   166,   166,   129,   129,
      79,   166,  1629,  1693,   166,   166,   166,  1579,  1698,  2044,
    1693,   166,   166,   144,   144,  1698,   129,  1707,   767,  2044,
     166,   166,  1773,    22,  1707,  2044,   166,  2044,   166,   163,
     172,   144,  1732,   164,  2044,   172,   172,   168,  2044,    77,
    1561,   166,   170,  1939,  1565,  1566,  1797,   188,   172,     3,
     129,   164,   172,  1981,   166,   168,    79,  1629,  1579,    13,
      14,    15,    16,    17,  1337,   144,   815,   166,   454,   166,
     166,   166,    13,    14,    15,    16,    17,   166,    79,    79,
     188,  1642,   166,   166,   172,   164,  1607,   169,  2044,   168,
     170,  1939,   166,  1395,  1396,  1846,   163,   170,  1849,   170,
    2044,   166,   166,   166,  1731,  1732,   129,  1858,  1859,   163,
     163,    14,   164,   164,  1865,   164,   164,  1732,  1693,   164,
     164,   144,   164,  1698,   164,    79,   172,  1878,   129,   129,
    1392,   170,  1707,  1395,  1396,   171,   171,  1888,    79,  1890,
     166,   164,   189,   144,   144,   168,   895,  1409,   163,   166,
     166,  1879,  1903,   171,  1905,  1906,  1907,   188,  2044,  1849,
    1732,   171,   548,   164,   164,   169,  1849,   168,   168,   131,
    1894,   169,  2068,   163,  2070,   129,   169,   166,  2106,   166,
     166,   169,  2110,  2111,   169,  1936,   166,  1449,   129,   166,
     144,   166,  1943,   166,  2042,   163,  1947,   163,   189,   164,
    1462,  1952,    87,   144,   145,  1467,  1468,  1469,   164,    99,
    1731,  2139,  2108,  1879,    83,     1,  2044,   403,     4,  2044,
    2068,  2044,  2070,   163,  1975,   189,  1977,  2044,  1941,   189,
     189,   189,  2160,   164,  1861,   164,  2164,   189,   107,   189,
     189,    97,   166,  2120,   163,  1879,  1936,   172,  2176,  1973,
    2001,   163,  1879,  1936,   163,   166,  1969,   163,  2135,   169,
    2108,   166,  2013,   170,  1879,   651,  2017,   166,   166,    13,
      14,    15,    16,    17,  1849,  2026,   169,  2173,   166,    65,
     166,  2032,  2010,   163,   166,  1547,   672,   166,  1879,   166,
     166,   164,   189,  2044,  2045,   164,   171,    83,   166,   166,
      86,   166,   164,   164,   170,  1607,   163,  1879,    94,   163,
     169,  1573,  2040,  2045,   166,   131,   169,   133,   134,   135,
      82,   107,    82,   163,   110,  2173,  2077,   164,   114,    13,
      14,    15,    16,    17,    18,    79,   166,   164,   189,   189,
    1861,   189,   163,   166,  2010,  1607,  2074,   166,   164,   166,
     168,   167,   168,    82,  2105,  2045,   172,   173,  1879,  1108,
      82,  1936,  2045,  2114,   189,   180,   152,   180,  2119,    82,
     171,   163,   158,  2105,  2040,   161,  2010,   189,   164,   165,
     189,   189,   163,  2010,   163,   129,   180,   165,   163,   180,
     166,   177,   113,   164,  2145,  2010,  1658,  2148,  1660,  2150,
     144,   145,   171,   170,   165,   274,  2040,   166,  2074,   166,
     180,   169,   180,  2040,    82,  2105,   202,   189,  2169,  2010,
     166,   171,  2105,   209,   210,  2040,  2177,   165,   163,   166,
     163,  1693,   164,   819,   189,  2186,  1698,   189,  2010,   166,
    2074,   189,  1642,  1705,   830,  1707,   539,  2074,    18,  2040,
     618,   622,   619,   239,   240,   241,   620,  1167,   621,  2074,
    1358,   330,  1565,  1371,   254,  2070,   335,   336,  2040,   255,
    2045,   113,  2150,  1887,  1579,   117,   118,   119,   120,   121,
     122,   123,   124,  2074,  1879,  2101,  2059,   356,   274,  2010,
     276,  1781,    13,  2041,    64,    65,    66,    67,    68,    69,
      70,    71,  2074,  1973,  1777,  1764,  1764,  2111,  2040,    18,
     296,   415,  2164,    55,   300,  1398,   123,  1936,   304,  2040,
     403,   307,   164,   165,   310,   370,   912,  1999,  1542,   433,
    2105,  1420,  1796,   919,  1033,  1797,    13,    14,    15,    16,
      17,    18,  1392,   412,   330,   187,   694,   733,   826,   335,
     336,     0,   496,  2074,  1611,   851,   425,   426,   851,    68,
      69,    70,    71,    13,    14,    15,    16,    17,    18,   851,
     356,  1528,    -1,    -1,    95,    -1,    -1,   363,   364,  1328,
      -1,    -1,    -1,    -1,  1846,    -1,    -1,  1849,  1337,   144,
      -1,    -1,   113,   379,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,    -1,    -1,  1005,
      -1,    -1,  1371,    -1,    -1,    -1,   412,   813,    -1,   415,
      -1,   417,   536,    -1,    -1,    -1,    -1,    -1,   424,   425,
     426,    -1,    -1,   157,   158,   159,   160,   433,   434,   157,
     158,   159,   160,    -1,    68,    -1,   170,    -1,    -1,   168,
      -1,    94,   170,    -1,    -1,   851,   180,   453,   454,   455,
     456,    -1,   180,    -1,  1936,   189,    -1,    -1,    -1,    -1,
      -1,   189,    -1,    -1,   239,    -1,   872,    -1,    -1,    -1,
     876,    -1,    -1,    -1,    -1,    -1,   110,    -1,    -1,    -1,
     486,   487,    -1,   489,    -1,    -1,  1092,    -1,   122,   123,
    1983,    -1,   498,  1975,    -1,  1977,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,   512,    -1,   161,   515,
      -1,   517,   518,    -1,    -1,   521,    -1,    -1,    -1,    -1,
      -1,  1127,    -1,   626,   530,    -1,   650,   533,   534,   535,
     536,   165,    -1,    -1,    -1,    -1,    -1,   626,   662,    -1,
      -1,   630,   548,    -1,  2026,   551,   670,    -1,    -1,   202,
     325,   557,   558,    -1,   678,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2044,  2045,    72,    -1,    -1,  1536,  1537,  1538,
    1539,  1540,  1541,  1542,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   987,    -1,    -1,    -1,    -1,   592,    -1,   994,   595,
      -1,    -1,  1561,    -1,    -1,   229,  1565,  1566,    -1,    -1,
      -1,    -1,   255,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1579,    -1,    -1,    -1,    -1,    -1,    -1,   706,    -1,    -1,
     626,    -1,    -1,  2105,   630,    -1,    -1,    -1,    -1,   718,
     733,    -1,    -1,    -1,   409,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   276,    -1,   650,   651,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   742,    -1,    -1,   662,    -1,   664,   748,
     666,    -1,    -1,    -1,   670,    -1,   672,    -1,    -1,    -1,
      -1,   677,   678,   307,    -1,    -1,    -1,    -1,    -1,   113,
      -1,    -1,    86,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   698,    -1,    -1,  2177,   130,    -1,   132,    -1,
     706,    -1,    -1,    -1,  2186,    -1,    -1,    -1,    -1,    -1,
     813,   364,   718,  1319,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   810,    -1,   812,    -1,    -1,    -1,    -1,   831,    -1,
      -1,   165,    -1,    -1,   168,    -1,   742,   512,   744,   745,
     515,    -1,   748,    -1,   750,   379,   521,    -1,   851,    -1,
      -1,    -1,    -1,    -1,    -1,   530,    -1,    -1,    -1,   883,
      -1,    -1,  1168,    -1,    -1,    -1,  1172,    -1,    -1,   872,
      -1,    -1,    -1,   876,    -1,    -1,    -1,    -1,    -1,   413,
      -1,   434,    -1,   417,    -1,   560,   910,    -1,    -1,    -1,
     424,    -1,    -1,   917,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   454,   319,    -1,   810,    -1,   812,    -1,    -1,   815,
      -1,    -1,    -1,   819,    -1,    -1,    -1,    -1,  1424,    -1,
      -1,    -1,   456,    -1,   830,   831,    -1,   833,    -1,    -1,
      -1,    -1,    -1,    -1,   487,    -1,   240,   843,    -1,    -1,
      -1,    -1,    -1,    -1,   968,   851,   852,    -1,    -1,    -1,
      -1,    -1,    -1,   859,    -1,    -1,    -1,    -1,    -1,   512,
      -1,    -1,    -1,    -1,    -1,   518,   969,    -1,   521,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   883,    -1,    -1,
     969,    -1,  1288,    -1,   987,    -1,  1292,    -1,    -1,   895,
    1296,   994,   981,    -1,   983,    -1,    -1,    -1,    -1,  1023,
     304,    85,    -1,    -1,   910,    -1,   912,    -1,    -1,    -1,
    1879,   917,    -1,   919,    -1,    -1,    64,   551,    -1,  1043,
      -1,    -1,    -1,   557,    72,    73,    74,    75,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   113,    -1,   115,   346,   117,   118,   119,   120,
     121,   122,   123,   124,     1,    -1,   731,     4,    -1,   363,
      -1,   595,   968,   969,   739,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   983,    -1,    -1,
    1069,    -1,    -1,   758,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1116,    -1,   769,    -1,    -1,    -1,   651,  1005,
    1006,  1104,    -1,    -1,   113,   189,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,  1023,    65,    -1,
     168,   655,    -1,  1629,    -1,    -1,    -1,    -1,    -1,   433,
      -1,    -1,   666,    -1,    -1,    -1,  1125,  1043,   186,    -1,
      -1,  2010,    -1,    -1,    -1,   698,    -1,    94,    -1,   453,
     684,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,  1069,    -1,  1168,    -1,   114,    -1,  1172,
    1476,  2040,    -1,    -1,  1480,    -1,    -1,  1483,    -1,    -1,
     189,    -1,   486,    -1,    -1,    -1,  1092,    -1,   489,    -1,
     724,   744,   745,  1217,  1218,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1108,    -1,  1510,  2074,   507,  1113,  1514,   510,
    1116,   158,  1518,    -1,   161,   749,    -1,    -1,    -1,  1125,
    1126,  1127,    -1,    -1,    -1,  1131,    -1,   189,    -1,    -1,
      -1,    -1,  1138,    13,    14,    15,    16,    17,   772,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    -1,
      -1,  1275,   786,  1159,    -1,   202,    -1,    -1,    -1,    -1,
    1249,    -1,    -1,    -1,    -1,   799,    -1,   113,   685,    -1,
     687,   117,   118,   119,   120,   121,   122,   123,   124,  1185,
      -1,    -1,    -1,    -1,    -1,  1288,    -1,    -1,   592,  1292,
     843,    -1,   239,  1296,   241,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,   859,    -1,    -1,    -1,
     113,  1217,  1218,    -1,   117,   118,   119,   120,   121,   122,
     123,   124,   125,    -1,  1630,    -1,    -1,   274,    -1,    -1,
      -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,   300,    -1,    -1,  1262,    -1,   662,    -1,
     664,    -1,   142,    -1,   144,   168,   919,    -1,    -1,  1275,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,   164,   165,    -1,    -1,  1894,   923,
      -1,    -1,   339,    -1,    -1,   175,   176,    -1,    -1,   346,
     240,    -1,    -1,  1078,    -1,    -1,    -1,   187,    -1,    -1,
     711,    -1,    -1,  1319,    -1,   255,  1091,   364,    -1,  1094,
    1409,    -1,  1328,  1098,   167,    -1,    -1,    -1,    -1,    -1,
    1454,  1337,    -1,    -1,    -1,    -1,   179,   277,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   750,     4,     5,     6,
       7,     8,     9,    10,    11,    12,   296,  1363,  1364,    -1,
     300,    -1,    -1,    -1,   304,  1371,    -1,  1973,    -1,    -1,
      -1,    -1,  1006,  1476,    -1,    -1,    -1,  1480,    -1,    -1,
    1483,   428,    -1,    -1,    -1,    -1,    -1,   434,    -1,  1395,
    1396,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,  1409,    -1,    -1,  1412,  1510,   455,    -1,
      -1,  1514,    -1,    -1,    -1,  1518,    -1,   934,  1424,    -1,
      -1,   938,    -1,   363,    -1,    -1,    -1,   944,    -1,    -1,
      -1,    -1,  1438,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     487,    -1,   489,    -1,    -1,    -1,    -1,    -1,  1454,  1455,
     851,   852,    -1,    -1,    -1,    -1,    -1,   179,  1547,    -1,
     507,   508,    -1,   510,   511,   512,   867,    -1,   515,   870,
     517,   518,    -1,    -1,   521,   415,    -1,    -1,  1131,    -1,
      -1,    -1,    -1,   530,    -1,    -1,   533,   534,   535,    -1,
      -1,    -1,  1126,    -1,    -1,  1012,    -1,    -1,    -1,    -1,
      -1,   548,    -1,    -1,    -1,    -1,   910,    -1,    -1,  1026,
      -1,    -1,    -1,   453,   454,    -1,  1033,    -1,    -1,  1036,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1630,    -1,    -1,
    1536,  1537,  1538,  1539,    -1,  1541,  1542,    -1,    -1,    -1,
      -1,  1547,  1548,    -1,    -1,    -1,   486,  1322,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1561,    -1,    -1,    -1,  1565,
    1566,  1336,    -1,    -1,   968,    -1,    -1,    -1,    -1,    -1,
    1576,    -1,    -1,  1579,    -1,    -1,    -1,    -1,    -1,   626,
      -1,  1356,    -1,   630,   631,    -1,    -1,    -1,  1363,    -1,
     637,    -1,    -1,    -1,    -1,    -1,   536,    -1,    -1,    -1,
     647,  1607,    -1,    -1,  1693,    -1,    -1,    -1,   548,  1698,
    1340,    -1,   552,    -1,    -1,    -1,  1705,    -1,  1707,  1023,
      -1,    -1,  1352,  1629,    -1,    -1,    -1,    -1,    -1,    -1,
     677,    -1,    -1,    -1,    -1,    -1,  1642,    -1,    -1,  1043,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1049,    -1,
      -1,   698,   592,    -1,    -1,  1056,  2062,    -1,    -1,  1060,
     707,    -1,    -1,  1064,   711,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1308,    -1,    -1,    -1,    -1,    -1,
      -1,  1198,    -1,    -1,    -1,    -1,    -1,  1693,  1694,    -1,
      -1,    -1,  1698,    -1,  1700,    -1,    -1,   744,   745,  1705,
    1104,  1707,    -1,    -1,    -1,    -1,  1223,    -1,    -1,    -1,
     650,   651,  1116,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,  1731,  1732,    -1,    -1,    -1,
     670,    -1,   672,    -1,    -1,    -1,    -1,   113,   678,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1846,  1159,  1412,
    1849,  1395,    -1,    -1,   811,    -1,   142,    -1,   815,    -1,
      -1,    -1,   819,    -1,    -1,   175,    -1,    -1,    -1,    -1,
    1555,  1556,    -1,   830,   831,    -1,   833,    -1,   164,   165,
      -1,    -1,    -1,    -1,    -1,   171,   843,    -1,    -1,   175,
     176,    -1,    -1,    -1,   851,   852,    -1,    -1,    -1,    -1,
     750,   187,   859,  1217,  1218,    -1,    -1,    -1,    -1,    -1,
     867,   868,    -1,   870,   871,  1831,  1832,    -1,    -1,    -1,
      -1,  1606,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1240,
    1846,    -1,    -1,  1849,    -1,    -1,    -1,  1936,   895,    -1,
      -1,    -1,    -1,    -1,    -1,  1861,    -1,    -1,  1262,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,  1597,  1598,    -1,
      -1,  1275,    -1,  1879,    -1,    -1,    -1,    -1,  1279,   819,
      -1,  1282,    -1,    -1,    -1,  1286,  1975,    -1,  1894,    -1,
     830,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,    -1,   129,  1548,    -1,    -1,    -1,    -1,    -1,
      -1,  1927,   969,  1576,   971,    -1,    -1,    -1,   144,    -1,
    1936,    -1,  1449,    -1,    -1,    -1,   983,    -1,    -1,    -1,
      -1,    -1,    -1,   883,    -1,  1462,    -1,    -1,   164,   165,
    1467,  1468,  1469,    -1,    -1,  2044,  2045,    -1,  1005,  2062,
    1364,    -1,    -1,    -1,    -1,    -1,    -1,  1973,    -1,  1975,
    1976,   187,   912,  1607,    -1,    -1,   113,   917,    -1,   919,
     117,   118,   119,   120,   121,   122,   123,   124,  1763,    -1,
      -1,    -1,    -1,  1999,    -1,    -1,    -1,    -1,  1045,    -1,
      -1,    -1,  1049,    -1,  2010,  1735,    -1,    -1,    -1,  1056,
    1057,    -1,  1742,  1060,  1061,    -1,  2105,  1064,  1065,    -1,
      -1,    -1,    -1,    -1,  1071,    -1,    -1,  1428,   165,    -1,
      -1,   168,    -1,    -1,  2040,    -1,    -1,    -1,  2044,  2045,
      -1,    -1,    -1,  1773,    -1,  1092,    -1,  1700,    -1,    -1,
    1454,  1455,    -1,    -1,    -1,    -1,  1573,    -1,    -1,    -1,
      -1,  1108,    -1,    -1,    -1,  1005,    -1,    -1,  2074,    -1,
    1117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1127,    -1,    79,    -1,  1131,    -1,    -1,    -1,    -1,    -1,
      -1,  1138,    -1,    -1,    -1,    -1,    -1,  1731,    -1,  2105,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1159,    -1,    -1,    -1,   113,    -1,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,  1858,  1859,
      -1,    -1,   129,    -1,    -1,  1865,    -1,    -1,  1185,    -1,
      -1,  1658,    -1,  1660,    -1,   142,    -1,   144,  1878,    -1,
      -1,    -1,  1092,    -1,    -1,    -1,    -1,    -1,  1888,    -1,
    1890,    -1,    -1,    -1,    -1,    -1,    79,   164,   165,    -1,
      -1,   168,    -1,  1903,    -1,  1905,  1906,  1907,   175,   176,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1127,    -1,    -1,
     187,    -1,    -1,  1240,  1241,    -1,  1243,  1244,  1245,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,  1943,    -1,    -1,   129,  1947,    -1,    -1,
      -1,    -1,  1952,    -1,    -1,    -1,    -1,  1861,    -1,   142,
      -1,   144,  1279,  1280,    -1,  1282,  1283,    -1,    -1,  1286,
    1287,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
       4,   164,   165,    -1,    -1,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2001,  1319,    -1,   187,    -1,    -1,    -1,    -1,    -1,
    1797,  1328,    -1,  2013,    -1,    -1,    -1,  2017,    -1,    -1,
    1337,    -1,    -1,  1694,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2032,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    65,    -1,    -1,    -1,    -1,  1363,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1371,    -1,    -1,    -1,    -1,  2104,
      -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      94,    -1,  1976,    -1,    -1,    -1,    -1,  2077,    -1,  1396,
      -1,    -1,    -1,    -1,    -1,    -1,   110,    -1,    -1,    -1,
     114,  2136,    -1,    -1,   113,  1412,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,  1424,    -1,  1319,
      -1,  1428,  1429,    -1,  2114,    -1,    -1,    -1,    -1,  2119,
      -1,  1438,    -1,    -1,   148,    -1,    -1,    -1,   152,    -1,
      -1,    -1,    -1,    -1,   158,    -1,    -1,   161,    -1,    -1,
      -1,   165,    -1,    -1,    -1,  2145,   165,    -1,  2148,   168,
    2150,    -1,   176,   177,  1364,   179,    -1,    -1,    -1,    -1,
    1831,  1832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2169,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,   202,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1424,   239,   240,   241,    -1,  1536,
    1537,  1538,  1539,    -1,  1541,  1542,    -1,    -1,   164,   165,
      -1,   255,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,    -1,    -1,    -1,  1561,    -1,    -1,    -1,  1565,  1566,
      -1,   187,   276,   277,    -1,    -1,  1927,    79,    -1,  1576,
      -1,    -1,  1579,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   296,    -1,    -1,    -1,   300,    -1,    -1,    -1,
     304,    -1,    -1,   307,    -1,    -1,   310,    -1,    -1,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,  1975,    -1,    -1,   129,    -1,    -1,
      -1,    -1,  1629,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,   345,   144,    -1,  1995,  1642,    -1,    -1,  1999,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   362,   363,
     364,    -1,   164,   165,    -1,    -1,   168,    -1,    -1,    -1,
     374,    -1,    -1,   175,   176,   379,    -1,    -1,    -1,    -1,
      -1,   385,    -1,    79,    -1,   187,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2044,  2045,    -1,   400,  1694,    -1,   403,
      -1,    -1,    -1,  1700,   408,    -1,   410,    -1,    -1,   413,
     414,   415,    -1,   417,    -1,  1712,    -1,   113,    -1,    -1,
     424,   117,   118,   119,   120,   121,   122,   123,   124,   433,
     434,    -1,    -1,   129,    -1,  1732,    -1,    -1,    -1,  1629,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,   453,
     454,    -1,   456,    -1,  2105,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,    -1,   486,   487,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   187,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   239,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   512,    -1,
      -1,   515,    -1,   517,   518,   519,    -1,   521,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   530,    79,    -1,   533,
     534,   535,   536,    -1,  1831,  1832,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   548,    -1,    -1,   551,   552,    -1,
    1847,    -1,    -1,   557,   558,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,  1879,    -1,    -1,    -1,    62,    63,   592,    -1,
     142,   595,   144,    -1,    -1,    -1,    -1,  1894,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   626,   175,   176,    -1,   102,    -1,    -1,    -1,
    1927,    -1,    -1,    -1,    -1,   187,    -1,    -1,  1935,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   650,   651,    -1,    -1,
      -1,   655,    -1,    -1,    -1,    -1,   660,    -1,   662,    -1,
     664,    -1,   666,    -1,    -1,    -1,   670,    -1,   672,    -1,
      -1,    -1,    -1,   677,   678,    -1,  1973,   153,  1975,   113,
     156,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,   698,   171,    -1,    -1,  1995,  1996,
      -1,    -1,  1999,    -1,  1894,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2010,    -1,   191,    -1,    -1,    -1,    -1,
     724,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   733,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   172,    -1,
     744,   745,    -1,  2040,    -1,   749,   750,  2044,  2045,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,   767,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   512,    -1,    -1,   515,    -1,  2074,    -1,   142,
      -1,   521,   786,  1973,    -1,    -1,    -1,    -1,    -1,    -1,
     530,    -1,    -1,    -1,    -1,   799,    -1,   273,    -1,    -1,
      -1,   164,   165,    -1,    -1,    -1,    -1,    -1,  2105,   813,
      -1,   815,   175,   176,    -1,   819,    -1,    -1,    -1,    -1,
     560,   297,    -1,    -1,   187,   301,   830,   831,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,   843,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   851,   324,    -1,
      -1,    -1,   113,    -1,    -1,   859,   117,   118,   119,   120,
     121,   122,   123,   124,   125,    -1,    -1,    -1,   872,   130,
      -1,   132,   876,    -1,    -1,    -1,    -1,    -1,    -1,   883,
      -1,    -1,    -1,   623,    -1,    -1,    -1,    -1,    -1,    -1,
     366,   895,    -1,    -1,    -1,    79,    -1,    -1,    -1,   375,
     376,    -1,    -1,    -1,   165,    -1,   910,   168,   912,    -1,
     386,    -1,    -1,   917,    -1,   919,    -1,    -1,    -1,   923,
      -1,    -1,    -1,    -1,    -1,   401,    -1,    -1,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,   419,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,   968,   969,    -1,   443,   444,    -1,
      -1,    -1,   448,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,   165,    -1,   987,    -1,    -1,    -1,    79,    -1,    -1,
     994,   175,   176,    -1,    -1,    -1,   472,    -1,    -1,   475,
     476,  1005,  1006,   187,   744,   745,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,  1023,
     496,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,   129,   113,  1043,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     142,   113,   144,    -1,    -1,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,   175,   176,    -1,    -1,    -1,  1092,    -1,
      -1,    -1,    -1,   168,    -1,   187,    -1,   573,    -1,    -1,
    1104,    -1,   164,   165,  1108,    -1,    -1,    -1,    -1,  1113,
      -1,    -1,  1116,   175,   176,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1126,  1127,    -1,   187,   113,  1131,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   111,  1143,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,  1168,    -1,    -1,    -1,  1172,    -1,
      -1,    -1,    -1,    -1,    -1,  1179,    -1,    -1,   654,    -1,
     167,  1185,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    -1,  1217,  1218,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    79,  1262,    62,
      -1,    -1,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,  1275,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,  1288,    -1,    -1,    -1,  1292,    -1,
      -1,   113,  1296,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,  1308,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1319,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,  1328,    -1,   167,   803,    -1,    -1,
      -1,    -1,    79,  1337,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   817,   164,   165,    -1,    -1,    -1,    -1,     1,    -1,
     826,     4,    -1,   175,   176,    -1,    -1,    -1,    -1,  1363,
    1364,    -1,    -1,   166,    -1,   187,   113,  1371,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,   189,    -1,    -1,    -1,
      -1,  1395,  1396,    -1,    -1,   142,   113,   144,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,  1412,    -1,
      -1,    -1,    65,    -1,    -1,    -1,    -1,   164,   165,    -1,
    1424,    -1,    -1,    -1,    -1,   142,    -1,    -1,   175,   176,
      -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    94,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,
    1454,  1455,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    -1,  1476,    -1,    -1,    -1,  1480,   113,    -1,  1483,
      -1,   117,   118,   119,   120,   121,   122,   123,   124,   125,
      -1,    -1,    -1,    -1,   130,   148,   132,    -1,    -1,   152,
      -1,    -1,    -1,    -1,    -1,   158,  1510,    -1,    -1,    -1,
    1514,   113,    -1,    -1,  1518,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   177,    -1,    -1,    -1,   130,   165,
     132,    -1,  1536,  1537,  1538,  1539,  1540,  1541,  1542,    -1,
      -1,    -1,    -1,    -1,  1548,    -1,    -1,    -1,    -1,  1025,
      -1,    -1,    -1,    -1,    -1,    -1,   209,  1561,    -1,    -1,
      -1,  1565,  1566,   165,  1304,    -1,   168,    -1,    -1,   108,
      -1,    -1,  1576,    -1,   113,  1579,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,   240,   241,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   255,  1607,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,  1622,   142,
      -1,    -1,    -1,  1363,   277,  1629,  1630,    -1,    -1,    -1,
      -1,  1107,    -1,    -1,    -1,    -1,    -1,    -1,  1642,    -1,
      -1,   164,   165,   296,    -1,   168,    -1,   300,    -1,    -1,
      -1,   304,   175,   176,    -1,   164,    -1,   310,    -1,    -1,
      -1,    -1,    -1,    -1,   187,   188,    -1,    -1,    -1,    -1,
      -1,  1147,   146,   147,   148,   149,   150,   151,   152,   153,
     154,   155,   156,   336,    -1,    -1,    -1,   161,    -1,    -1,
      -1,    -1,   345,    -1,    -1,    -1,  1700,    -1,    -1,    -1,
      -1,    -1,    -1,   356,    -1,    -1,    -1,    -1,   361,   362,
     363,   364,    -1,    -1,   188,    -1,    -1,    -1,    -1,    -1,
      -1,   374,    -1,    -1,    -1,    -1,    -1,  1731,  1732,    -1,
      -1,    -1,   385,    -1,    -1,   388,    -1,    -1,    -1,   392,
      -1,    -1,    -1,    -1,   397,    -1,    -1,    -1,    -1,    -1,
     403,    -1,    -1,    -1,    -1,   408,    -1,   410,    -1,    -1,
      -1,   414,   415,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   426,    -1,    -1,    -1,    -1,    -1,   113,
     433,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     453,   454,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1561,  1562,    -1,    -1,  1565,  1566,    -1,    -1,    -1,
     164,  1571,    -1,   486,    -1,  1575,    -1,  1577,    -1,  1579,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1861,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1343,  1344,  1345,
      -1,    -1,  1876,    -1,    -1,  1879,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   536,    -1,    -1,    -1,    -1,    -1,    -1,
    1894,    -1,    -1,    -1,    -1,   548,    -1,    -1,    -1,   552,
      -1,  1377,    -1,    -1,    -1,   558,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1398,    -1,    -1,    -1,    -1,  1403,    -1,    -1,
      -1,  1407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1957,  1958,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1973,
      -1,    -1,  1976,   626,    -1,    -1,    -1,   630,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1727,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   650,   651,    -1,
      -1,    -1,    -1,    -1,   657,    -1,  2010,  1747,  1748,   662,
      -1,   664,    -1,    -1,    -1,    -1,    -1,   670,    -1,   672,
      -1,    -1,  2026,    -1,   677,   678,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2040,    -1,  1778,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2062,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2074,    -1,    -1,    -1,    -1,  1551,  1552,    -1,    -1,    -1,
     733,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   750,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   767,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1863,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1871,    -1,  1873,    -1,    -1,  1876,  1877,    -1,  1879,
      -1,    -1,    -1,    -1,  1884,    -1,    -1,  1623,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     813,    -1,   815,    -1,    -1,    -1,   819,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,   831,    -1,
     833,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   851,   852,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1957,    -1,   872,
      -1,    -1,    -1,   876,  1964,  1965,    -1,    -1,    -1,    -1,
     883,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1716,    -1,   895,    -1,    -1,    -1,    -1,  1987,    -1,    -1,
      -1,    -1,    -1,    65,    -1,    -1,    -1,   910,    -1,   912,
      -1,    -1,    -1,    -1,   917,    -1,   919,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    86,    -1,    -1,    -1,  2018,    -1,
    2020,    -1,    -1,  2023,  2024,    -1,    -1,    -1,    -1,    -1,
    2030,  2031,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   968,   969,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   980,    -1,    -1,
      -1,    -1,    -1,    -1,   987,    -1,   148,    -1,    -1,    -1,
      -1,   994,    -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,
      -1,    -1,  1005,    -1,    -1,    -1,    -1,  2097,  2098,  2099,
      -1,    -1,    -1,    -1,    -1,   177,    -1,    -1,    -1,    -1,
    1023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2125,  2126,  2127,    -1,    -1,
    1043,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   240,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1092,
      -1,    -1,    -1,   255,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1104,    -1,    -1,    -1,  1108,    -1,    -1,    -1,    -1,
    1113,    -1,    -1,  1116,    -1,   277,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1127,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   296,    -1,    -1,    -1,   300,    -1,
      -1,    -1,   304,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1168,    -1,    -1,    -1,  1172,
      -1,    -1,    -1,    -1,    -1,    -1,  1179,    -1,    -1,    -1,
      -1,    -1,    -1,   345,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   363,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   374,    -1,  1217,  1218,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   385,    -1,    -1,   388,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   403,    -1,    -1,  1247,    -1,    -1,    -1,   410,    -1,
      -1,    -1,   414,   415,    -1,    -1,    -1,    -1,    -1,  1262,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   433,  1275,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1288,    -1,    -1,    -1,  1292,
      -1,   453,   454,  1296,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,  1319,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   486,  1328,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1337,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1364,    -1,    -1,    -1,    -1,    -1,    -1,  1371,    -1,
      -1,    -1,    -1,    -1,   536,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   548,    -1,    -1,    -1,
     552,    -1,    -1,  1396,    -1,    -1,    86,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1424,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,
     592,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1454,  1455,    -1,    -1,    -1,    -1,    -1,   148,    -1,
      -1,    -1,    -1,    -1,   626,    -1,    -1,    -1,   158,    -1,
      -1,    -1,    -1,  1476,    -1,    -1,    -1,  1480,    -1,    -1,
    1483,    -1,    -1,    -1,    -1,    -1,    -1,   177,   650,   651,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     662,    -1,   664,    -1,    -1,    -1,    -1,  1510,   670,    -1,
     672,  1514,    -1,    -1,    -1,  1518,   678,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1536,  1537,  1538,  1539,  1540,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     240,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1561,    -1,
      -1,    -1,  1565,  1566,    -1,   255,    -1,    -1,    -1,    -1,
      -1,   733,    -1,    -1,    -1,    -1,  1579,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   277,   750,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   767,   296,    -1,    -1,    -1,
     300,    -1,    -1,    -1,   304,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1629,  1630,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   813,    -1,   815,    -1,   345,    -1,   819,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,   831,
      -1,    -1,    -1,   363,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   374,    -1,    -1,    -1,    -1,   851,
      -1,    -1,    -1,    -1,    -1,   385,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1707,    -1,    -1,    -1,    -1,    -1,
     872,    -1,    -1,   403,   876,    -1,    -1,    -1,    -1,    -1,
     410,   883,    -1,    -1,   414,   415,    -1,    -1,    -1,  1732,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   433,    -1,    -1,    -1,    -1,   910,    -1,
     912,    -1,    -1,    -1,    -1,   917,    -1,   919,    -1,    -1,
      -1,    -1,    -1,   453,   454,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   486,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   968,   969,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   987,    -1,    -1,    -1,  1832,
      -1,    -1,   994,    -1,    -1,    -1,    -1,    -1,    65,    -1,
      -1,    -1,    -1,  1005,    -1,    -1,   536,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   548,    -1,
      -1,  1023,   552,    -1,    -1,    -1,    -1,    94,    -1,    -1,
      -1,    -1,    -1,  1876,    -1,    -1,  1879,    -1,    -1,    -1,
      -1,  1043,    -1,    -1,    -1,    -1,    -1,   114,    -1,    -1,
      -1,  1894,    -1,    -1,    -1,    -1,   123,    -1,    -1,    -1,
      -1,    -1,   592,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   148,    -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,
    1092,   158,    -1,    -1,   161,    -1,   626,    -1,    -1,    -1,
      -1,    -1,  1104,    -1,    -1,    -1,  1108,    -1,    -1,    -1,
      -1,  1113,    -1,    -1,  1116,    -1,    -1,    -1,    -1,    -1,
     650,   651,    -1,    -1,    -1,  1127,    -1,    -1,    -1,    -1,
    1973,    -1,   662,    -1,   664,   202,    -1,    -1,    -1,    -1,
     670,    -1,   672,    -1,    -1,    -1,    -1,    -1,   678,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1168,  2010,    -1,    -1,
    1172,    -1,   239,    -1,   241,    -1,    -1,  1179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2040,    -1,    -1,
      -1,    -1,  2045,   733,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1217,  1218,    -1,    -1,  2062,
     750,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2074,    -1,    -1,    -1,    -1,    -1,   767,    -1,    -1,
      -1,    -1,    -1,   310,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1262,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1275,    -1,    -1,    -1,    -1,   345,    -1,
      -1,    -1,    -1,   813,    -1,   815,  1288,    -1,    -1,   819,
    1292,    -1,    -1,    -1,  1296,   362,    -1,   364,    -1,    -1,
     830,   831,    -1,   370,    -1,    -1,    -1,   374,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1319,   385,    -1,
      -1,   851,    -1,    -1,    -1,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1337,    -1,    -1,    -1,    -1,
      -1,   408,   872,   410,    -1,    -1,   876,   414,    -1,    -1,
      -1,    -1,    -1,   883,    94,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1364,    -1,    -1,    -1,    -1,   434,    -1,  1371,
      -1,    -1,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,
     910,    -1,   912,    -1,    -1,    -1,    -1,   917,    -1,   919,
      -1,    -1,    -1,    -1,  1396,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,    -1,
      -1,    -1,   152,    -1,    -1,    -1,    -1,    -1,   158,    -1,
     487,   161,  1424,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   968,   969,
      -1,    -1,    -1,    -1,    -1,   512,    -1,    -1,   515,    -1,
     517,   518,  1454,  1455,   521,    -1,    -1,   987,    -1,    -1,
      -1,    -1,   202,   530,   994,    -1,   533,   534,   535,    -1,
      -1,    -1,    -1,    -1,  1476,  1005,    -1,    -1,  1480,    -1,
      -1,  1483,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   558,    -1,  1023,    -1,    -1,    -1,    -1,    -1,   239,
      -1,   241,    -1,    -1,    -1,    -1,    -1,    -1,  1510,    -1,
      -1,    -1,  1514,  1043,    -1,    -1,  1518,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1561,
      -1,    -1,  1092,  1565,  1566,    -1,    -1,    -1,    -1,    -1,
     310,    -1,    -1,    -1,  1104,    -1,    -1,  1579,  1108,    -1,
      -1,    -1,    -1,  1113,    -1,    -1,  1116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1127,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   345,    -1,    -1,    -1,    -1,
     677,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   362,    -1,   364,    -1,    -1,  1629,  1630,    -1,
     370,   698,    -1,    -1,   374,    -1,    -1,    -1,  1168,    -1,
      -1,    -1,  1172,    -1,    -1,   385,    -1,    -1,    -1,  1179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   408,    -1,
     410,    -1,    -1,    -1,   414,    -1,    -1,   744,   745,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1217,  1218,    -1,
      -1,    -1,    -1,    -1,   434,    -1,    -1,    -1,    -1,    -1,
     767,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1732,    -1,  1262,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1275,    -1,   487,   815,    -1,
      -1,     1,    -1,    -1,     4,    -1,    -1,    -1,  1288,    -1,
      -1,    -1,  1292,    -1,    -1,    -1,  1296,    -1,    -1,    -1,
      -1,    -1,   512,    -1,    -1,   515,   843,   517,   518,    -1,
      -1,   521,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1319,
     530,    -1,   859,   533,   534,   535,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,  1337,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    65,    -1,    -1,   558,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   895,    -1,
      -1,    -1,    -1,    -1,  1364,    -1,    86,    -1,    -1,    -1,
      -1,  1371,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,    -1,    -1,    -1,   114,    -1,  1396,    -1,    -1,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1879,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1894,    -1,  1424,    -1,    -1,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,
      -1,    -1,    -1,    -1,    -1,   165,    -1,    -1,   114,    -1,
      -1,    -1,    -1,    -1,  1454,  1455,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   677,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1476,    -1,    -1,    -1,
    1480,    -1,   148,  1483,    -1,    -1,   152,    -1,   698,    -1,
      -1,    -1,   158,    -1,    -1,   161,    -1,    -1,    -1,    -1,
      -1,  1973,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1510,    -1,    -1,    -1,  1514,    -1,    -1,    -1,  1518,    -1,
     240,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   744,   745,   202,    -1,  2010,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   767,    -1,    -1,
      -1,  1561,    -1,    -1,    -1,  1565,  1566,    -1,  2040,    -1,
      -1,  1108,    -1,   239,    -1,   241,    -1,    -1,    -1,  1579,
      -1,    -1,    -1,    -1,   304,    -1,    -1,    -1,    -1,    -1,
    2062,    -1,    -1,    -1,  1131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2074,    -1,    -1,   815,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1629,
    1630,    -1,    -1,   843,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   363,   310,    -1,    -1,    -1,  1185,   859,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   379,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   345,
      -1,    -1,    -1,    -1,    -1,   895,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   362,    -1,   364,    -1,
      -1,    -1,    -1,    -1,   370,    -1,    -1,    -1,   374,    -1,
      -1,    -1,    -1,   433,    -1,    -1,    -1,    -1,    -1,   385,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1732,   453,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   408,    -1,   410,    -1,    -1,    -1,   414,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   486,    -1,   434,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1337,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
      -1,   487,    -1,    -1,    -1,    -1,  1363,    -1,    -1,    -1,
      -1,   122,    -1,   124,  1371,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   512,    -1,    -1,   515,
      -1,   517,   518,    -1,    -1,   521,    -1,    -1,    -1,  1396,
      -1,    -1,    -1,    -1,   530,    -1,    -1,   533,   534,   535,
      -1,    -1,   592,    -1,   165,  1412,  1876,   168,   169,  1879,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   558,    -1,  1894,    -1,    -1,    -1,  1108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   229,    -1,
      -1,    -1,   662,    -1,   664,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1973,    -1,  1185,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   276,    -1,    -1,    -1,    -1,
      -1,   282,    -1,   284,    -1,    -1,    -1,    -1,    -1,  1536,
    1537,  1538,  1539,  1540,  1541,  1542,    -1,    -1,    -1,    -1,
    2010,   677,    -1,    -1,    -1,    -1,   307,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1561,    -1,    -1,    -1,  1565,  1566,
     750,    -1,   698,    -1,    -1,    -1,   132,    -1,    -1,  1576,
    2040,    -1,  1579,    -1,    -1,    -1,    -1,    -1,    -1,   145,
      -1,   147,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2062,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2074,    -1,    -1,    -1,   744,   745,
      -1,    -1,   178,    -1,    -1,    -1,    -1,    -1,   379,    -1,
     381,   382,    -1,    -1,    -1,   815,    -1,    -1,    -1,   819,
      -1,   767,    -1,    -1,    -1,  1642,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1328,    -1,
      -1,    -1,   413,    -1,    -1,    -1,   417,  1337,    -1,   225,
      -1,    -1,    -1,   424,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   815,
      -1,    -1,    -1,  1363,    -1,    -1,   194,    -1,    -1,    -1,
      -1,  1371,    -1,  1700,    -1,   456,    -1,   458,   459,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   843,    -1,    -1,
      -1,    -1,   220,    -1,   222,    -1,  1396,    -1,   226,   227,
     910,    -1,   288,   859,    -1,  1732,    -1,    -1,    -1,   237,
     238,    -1,  1412,    -1,    -1,    -1,    -1,   498,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   255,   256,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   895,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   342,   343,   968,    -1,
     346,    -1,    -1,   349,   350,    -1,   352,    -1,   354,    -1,
     551,    -1,    -1,    -1,    -1,    -1,   557,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1023,   595,    -1,   597,   598,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1043,    -1,    -1,  1536,  1537,  1538,  1539,
    1540,  1541,  1542,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1879,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1561,    -1,    -1,    -1,  1565,  1566,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   655,   656,  1576,    -1,    -1,  1579,
      -1,    -1,  1092,    -1,    -1,   666,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1108,    -1,
      -1,    -1,    -1,   684,    -1,    -1,  1116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1126,  1127,    -1,    -1,
      -1,   507,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   718,    -1,    -1,
      -1,    -1,  1642,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1108,   481,    -1,    -1,    -1,    -1,    -1,   487,
      -1,   742,    -1,    -1,    -1,    -1,   747,    -1,   749,    -1,
      -1,    -1,    83,    -1,    -1,  1131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2010,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   772,    -1,   774,   775,    -1,    -1,    -1,    -1,    -1,
    1700,    -1,    -1,    -1,    -1,   786,    -1,  1217,  1218,    -1,
      -1,    -1,    -1,  2040,    -1,    -1,    -1,    -1,   799,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1185,
      -1,   812,  1732,    -1,   145,    -1,   147,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   631,    -1,  2074,   634,   635,
      -1,   637,  1262,   639,   640,    -1,    -1,    -1,   644,   645,
      -1,    -1,    -1,    -1,    -1,  1275,    -1,   178,    -1,   180,
      -1,    -1,    -1,    -1,    -1,    -1,   604,   605,   606,   607,
     608,   609,   610,   611,   612,   613,   614,   615,   616,   617,
     618,   619,   620,   621,   622,    -1,    -1,    -1,    -1,   210,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1319,
      -1,    -1,    -1,    -1,   225,    -1,    -1,    -1,    -1,    -1,
      -1,   649,    -1,    -1,    -1,    -1,    -1,  1337,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   722,    -1,    -1,    -1,
      -1,    -1,   923,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1364,    -1,    -1,    -1,    -1,    -1,
      -1,  1371,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1328,    -1,    -1,    -1,    -1,   288,    -1,  1879,
      -1,  1337,    -1,    -1,    -1,  1395,  1396,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   983,    -1,    -1,    -1,    -1,  1363,    -1,    -1,
      -1,    -1,    -1,    -1,  1424,  1371,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   335,  1006,    -1,    -1,   339,    -1,
    1011,   342,   343,    -1,    -1,   346,    -1,    -1,   349,   350,
    1396,   352,    -1,   354,  1454,  1455,    -1,    -1,    -1,    -1,
     836,    -1,   838,    -1,    -1,    -1,  1412,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   855,
     856,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   867,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2010,    -1,    -1,    -1,   425,    -1,    -1,   428,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1539,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2040,    -1,    -1,    -1,    -1,  1126,    -1,    -1,    -1,    -1,
      -1,  1561,    -1,    -1,    -1,  1565,  1566,  1138,    -1,    -1,
      -1,    -1,    -1,   891,    -1,    -1,    -1,    -1,   896,  1579,
      -1,    -1,    -1,    -1,  2074,    -1,    -1,    -1,  1159,   907,
    1536,  1537,  1538,  1539,  1540,  1541,  1542,    -1,    -1,    -1,
      -1,   919,    -1,    -1,    -1,   981,   507,  1607,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1561,    -1,    -1,    -1,  1565,
    1566,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1629,
    1576,    -1,    -1,  1579,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   966,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1248,    -1,    -1,
    1056,    -1,    -1,    -1,  1060,    -1,    -1,    -1,  1064,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1642,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,  1037,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     631,  1731,  1732,   634,   635,    -1,   637,    -1,   639,   640,
      -1,    -1,    -1,   644,   645,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1700,    -1,    -1,    -1,    -1,    -1,
      -1,  1137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   202,    55,    -1,    -1,    58,    -1,    60,    61,    -1,
      63,    -1,    -1,    -1,    -1,    -1,  1732,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   706,    -1,    -1,    -1,   240,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   722,    -1,    -1,  1395,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,  1861,    -1,    -1,  1240,    -1,    -1,  1243,  1244,    -1,
    1188,  1189,  1190,   304,    -1,  1251,  1252,  1195,  1196,  1879,
      -1,   164,    -1,    -1,   167,   168,    -1,    -1,   319,    -1,
     321,   174,   175,   176,   177,   178,   179,   180,  1274,    -1,
    1276,    -1,    -1,  1279,    -1,    -1,  1282,    -1,    -1,   810,
    1286,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1236,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1879,    -1,   836,    -1,   838,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1266,    -1,
    1268,    -1,    -1,    -1,   855,   856,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   867,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1548,    -1,    -1,
      -1,    -1,    -1,    -1,   415,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   423,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2010,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1337,
      -1,    -1,   453,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1607,    -1,    -1,    -1,
    2040,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   479,    -1,
      -1,    -1,  1428,    -1,    -1,   486,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2010,    -1,   497,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2074,    -1,    -1,    -1,    -1,    -1,
     981,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   523,    -1,  2040,    -1,    -1,    -1,    -1,  1475,
      -1,    -1,    -1,    -1,    -1,   536,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     551,   552,    -1,   554,    -1,    -1,    -1,    -1,  2074,    -1,
      -1,    -1,   563,    -1,   565,    -1,    -1,    -1,    -1,    -1,
      -1,   572,    -1,   574,   575,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1056,    -1,    -1,   589,  1060,
    1731,   592,    -1,  1064,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   627,    -1,   629,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1588,    -1,    -1,    -1,    -1,    -1,    -1,   650,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1137,    -1,    -1,   670,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   682,    -1,    -1,    -1,   686,   687,    -1,    -1,    -1,
      -1,    -1,   693,    -1,    -1,    -1,    -1,   698,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1849,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1861,    -1,    -1,   724,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1677,    -1,    -1,    -1,    -1,    -1,  1625,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   750,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1240,
      -1,    -1,  1243,  1244,    -1,    -1,    -1,    -1,    -1,    -1,
    1251,  1252,    -1,    -1,    -1,    -1,  1927,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1274,    -1,  1276,    -1,    -1,  1279,    -1,
      -1,  1282,    -1,    -1,    -1,  1286,    -1,  1958,  1959,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1976,    -1,    -1,    -1,    -1,
      -1,    -1,  1788,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   859,    -1,
      -1,    -1,  1808,  1809,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,    -1,  1777,
      -1,    -1,  1838,  1839,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   903,    -1,    -1,  1851,    -1,   908,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   916,   917,    -1,    -1,    -1,
      -1,    -1,   923,    -1,    -1,    -1,    -1,   928,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   944,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1428,    -1,    -1,
      -1,    -1,    -1,    -1,  2105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    94,    -1,  1929,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,    -1,    -1,
      -1,    -1,    -1,    -1,  1475,  1006,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1033,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,    -1,  1044,    -1,    -1,    -1,    -1,   161,  1995,
      -1,  1939,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   202,
      -1,    -1,    -1,    -1,    -1,  1983,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1588,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   239,   240,    -1,    -1,
    1131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   255,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2042,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   276,    -1,    -1,  1167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2068,    -1,  2070,   296,    -1,    -1,    -1,   300,    -1,    -1,
      -1,   304,    -1,    -1,   307,    -1,    -1,  1198,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1677,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1232,    -1,    -1,  1705,    -1,    -1,    -1,    -1,    -1,
      -1,  1712,    -1,    -1,    -1,    -1,    -1,  1248,    -1,    -1,
     363,   364,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   379,    -1,    -1,    -1,
      -1,  1272,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   415,    -1,   417,  1306,    -1,  1308,    -1,    -1,
      -1,   424,    -1,    -1,    -1,    -1,    -1,  1788,    -1,    -1,
     433,   434,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1808,  1809,    -1,
     453,   454,    -1,   456,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1357,  1358,    -1,    -1,
      -1,    -1,    -1,  1364,    -1,    -1,    -1,  1838,  1839,    -1,
      -1,    -1,    -1,   486,   487,  1846,    -1,    -1,    -1,    -1,
    1851,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   512,
      -1,    -1,   515,    -1,   517,   518,    -1,    -1,   521,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   530,    -1,    -1,
     533,   534,   535,   536,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1432,    -1,    -1,    -1,   548,    -1,    -1,   551,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1449,    -1,
    1451,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1929,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,
      -1,    -1,   595,    -1,    -1,    -1,  1487,  1488,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1995,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   650,   651,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   662,
      -1,   664,    -1,  1554,    -1,    -1,    -1,   670,    -1,   672,
      -1,    -1,    -1,    -1,    -1,   678,    -1,    -1,    -1,     3,
      -1,    -1,  1573,  2044,    -1,  1576,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,   698,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,
      -1,   744,   745,    -1,    -1,    -1,    -1,   750,    -1,  1640,
      -1,    -1,  1643,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1663,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,  1686,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,  1700,
      -1,    -1,    -1,    -1,    -1,  1706,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
     843,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
      18,   175,   176,    -1,    -1,    -1,   859,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1768,    -1,    -1,
     883,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1797,   910,    -1,   912,
      78,    -1,    80,    81,   917,    83,   919,    -1,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,    -1,
     108,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,    -1,   130,   131,   132,   968,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,    -1,  1005,  1006,   172,    -1,   174,   175,   176,   177,
     178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1023,   189,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
    1043,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    -1,  1984,    -1,    76,    -1,    -1,    79,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1113,    -1,    -1,  1116,     5,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,  1131,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    55,    -1,    -1,    58,    -1,    60,
      61,    -1,    63,    -1,    -1,    -1,   157,   158,   159,   160,
      -1,    -1,  1185,   164,   165,   166,   167,   168,    79,    80,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   189,    -1,
      -1,    -1,    -1,    -1,  1217,  1218,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,  1262,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1275,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,  1328,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
    1363,  1364,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,  1395,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1412,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    55,   129,    -1,    58,    -1,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,  1454,  1455,    -1,    -1,    -1,    -1,    78,    -1,    80,
      81,    -1,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,    -1,   108,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,    -1,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1536,  1537,  1538,    -1,    -1,  1541,  1542,
      -1,    -1,   163,   164,    -1,  1548,   167,   168,    -1,    -1,
      -1,   172,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   189,     1,
      -1,    -1,    -1,  1576,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1607,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1642,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    80,    81,
      -1,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,    -1,   108,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,  1700,   130,   131,
     132,    -1,   134,   135,    -1,    -1,    -1,    -1,    55,    -1,
     142,    58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1731,    -1,
      -1,   163,   164,    80,    -1,   167,   168,    -1,    -1,    -1,
     172,    -1,   174,   175,   176,   177,   178,   179,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   189,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,   159,   160,    -1,    -1,    -1,   164,   165,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,  1861,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,  1894,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    -1,    -1,    76,    -1,    78,    79,    80,    81,    -1,
      83,    -1,    -1,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,    -1,   108,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,   132,
    1973,   134,   135,  1976,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,   172,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   189,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    -1,    -1,    -1,
      76,    -1,    78,    79,    80,    81,    -1,    83,    -1,    -1,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,    -1,   108,    -1,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,    -1,   172,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   189,     3,     4,     5,     6,     7,     8,
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
     129,   130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
     159,   160,    -1,    -1,    -1,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     189,     3,     4,     5,     6,     7,     8,     9,    10,    11,
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
     122,   123,   124,    -1,    -1,    -1,   128,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   175,   176,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   187,    -1,   189,     3,     4,
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
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
     165,    -1,   167,   168,    -1,    -1,    -1,   172,    -1,    -1,
     175,   176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   187,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   175,   176,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,   145,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   187,     4,     5,     6,     7,     8,     9,    10,    11,
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
     122,   123,   124,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,
     172,    -1,    -1,   175,   176,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   187,     4,     5,     6,     7,
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
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    -1,    -1,    76,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,    -1,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    -1,    -1,
      -1,    76,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
      -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,     4,     5,     6,     7,
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
      -1,   129,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,
     168,   169,   170,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
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
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,   165,    -1,   167,   168,   169,   170,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
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
     124,   125,   126,   127,    -1,   129,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,   165,    -1,   167,   168,   169,   170,    -1,    -1,    -1,
     174,   175,   176,   177,   178,   179,   180,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
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
     127,    -1,   129,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,
     167,   168,   169,   170,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,     4,     5,     6,     7,     8,     9,
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
     130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,   169,
     170,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
     123,   124,   125,   126,   127,    -1,   129,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,   165,    -1,   167,   168,   169,   170,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,     4,     5,
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
     126,   127,    -1,   129,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,   169,    -1,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
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
     129,   130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,
     169,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,     4,     5,     6,     7,     8,     9,    10,    11,
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
     132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   164,    -1,    -1,   167,   168,   169,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
     125,   126,   127,    -1,   129,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
      -1,    -1,   167,   168,   169,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,     4,     5,     6,     7,
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
      -1,   129,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,   169,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
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
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,   169,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
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
     124,   125,   126,   127,    -1,   129,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
     174,   175,   176,   177,   178,   179,   180,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
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
     127,    -1,   129,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,     4,     5,     6,     7,     8,     9,
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
     130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
     123,   124,   125,   126,   127,    -1,   129,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    -1,    -1,    76,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    -1,    -1,    -1,    76,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,     1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,     1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,     1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    13,    14,
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
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
       1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,    13,    14,    15,    16,    17,    18,    -1,    20,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,     1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,    13,    14,    15,    16,
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
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,     1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,    13,    14,    15,    16,    17,    18,
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
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,   172,    -1,    -1,   175,   176,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,   187,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,   187,    22,    23,    24,    25,    26,
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
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
     187,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   187,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    -1,    -1,    -1,
      76,    -1,    -1,    79,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,     3,     4,     5,     6,     7,     8,     9,    10,    11,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,    -1,   167,   168,    -1,     3,    -1,
       5,    -1,    -1,   175,   176,    10,    -1,    -1,    13,    14,
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
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
      -1,    -1,   167,   168,    -1,     3,    -1,     5,    -1,    -1,
     175,   176,    10,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,    -1,   167,
     168,    -1,     3,    -1,     5,    -1,    -1,   175,   176,    10,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,    -1,   167,   168,    -1,     3,
      -1,     5,    -1,    -1,   175,   176,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   175,   176,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   175,   176,     4,     5,     6,
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
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
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
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,   145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,    -1,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   175,   176,     4,     5,     6,     7,     8,     9,    10,
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
      -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,   145,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,     4,     5,     6,     7,
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
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,     4,
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
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,   169,    -1,    -1,    -1,    -1,    -1,
     175,   176,     4,     5,     6,     7,     8,     9,    10,    11,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,   145,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   175,   176,     4,     5,     6,     7,     8,
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
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,   145,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,     4,     5,
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
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,     4,     5,     6,     7,     8,     9,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   175,   176,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
      78,    -1,    80,    81,    -1,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      -1,    -1,   100,   101,   102,   103,   104,   105,   106,    -1,
     108,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,    -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,    -1,    -1,    -1,   172,    -1,   174,   175,   176,   177,
     178,   179,   180,    -1,    55,    -1,    -1,    58,    -1,    60,
      61,   189,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    80,
      81,    -1,    83,    -1,    -1,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    -1,    -1,   100,
     101,   102,   103,   104,   105,   106,    -1,   108,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,    -1,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,   172,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   189,     3,
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
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,    -1,    -1,   172,     3,
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
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,    -1,    -1,   172,     3,
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
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,    -1,    -1,   172,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    -1,    -1,    97,    -1,    99,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,   172,    -1,
     174,   175,   176,   177,   178,   179,   180,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,    -1,   172,    -1,   174,   175,
     176,   177,   178,   179,   180,    13,    14,    15,    16,    17,
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
      -1,   129,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,    13,    14,    15,    16,    17,    18,    -1,
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
     130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,
      -1,    -1,   172,    -1,   174,   175,   176,   177,   178,   179,
     180,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
     132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,
     172,    -1,   174,   175,   176,   177,   178,   179,   180,    13,
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
     124,   125,   126,   127,    -1,   129,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
     174,   175,   176,   177,   178,   179,   180,    13,    14,    15,
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
     126,   127,    -1,   129,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,    13,    14,    15,    16,    17,
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
      -1,   129,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    55,   115,   116,    58,    -1,    60,    61,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,
      -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
     171,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
     132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,   158,   159,   160,    -1,
      -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,     3,
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
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,    -1,    -1,    -1,    -1,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,   167,    22,    23,    24,    25,    26,    27,
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
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   144,   145,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
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
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,    13,    14,    15,    16,    17,    18,
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
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    13,    14,
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
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,     4,     5,     6,     7,     8,     9,    10,    11,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   144,   145,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,     4,     5,     6,     7,
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
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   144,   145,    -1,    -1,
      -1,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,   167,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,   167,    22,    23,    24,    25,
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
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   175,   176,    13,    14,    15,    16,    17,
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
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   175,   176,    13,    14,    15,    16,    17,    18,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   175,   176,    13,    14,    15,
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
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   175,   176,    13,    14,    15,    16,    17,
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
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    13,
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
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   175,   176,    13,    14,    15,    16,    17,    18,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   175,   176,    13,    14,    15,
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
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   175,   176,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
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
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,   167,    22,
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
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,    13,    14,    15,    16,    17,    18,
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
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    13,    14,
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
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,    13,    14,    15,    16,    17,    18,    -1,    20,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,    13,    14,    15,    16,
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
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
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
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,    13,    14,    15,
      16,    17,   175,   176,    20,    -1,    22,    23,    24,    25,
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
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    -1,    13,    14,    15,    16,    17,   175,
     176,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
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
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,    13,    14,    15,    16,    17,   175,   176,    20,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   175,   176,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
      -1,    -1,   167,   168,   169,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,    20,    -1,    22,    23,
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
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
     174,   175,   176,   177,   178,   179,   180,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
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
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,   167,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,   167,    22,    23,
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
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    20,   167,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    79,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,   115,   116,    57,
      -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,   144,    -1,    -1,    55,
      -1,    -1,    58,    -1,    60,    61,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
      -1,    -1,    -1,    -1,    80,   113,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,   144,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,    13,    14,    15,    16,    17,
      -1,   187,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      20,    79,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      13,    14,    15,    16,    17,    -1,   144,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   144,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    -1,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,   144,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    55,   134,   135,    58,    -1,    60,    61,    -1,
      63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,
      -1,   172,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   164,   165,    -1,   167,   168,    -1,   170,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,
     165,    -1,   167,   168,    -1,    -1,    -1,   172,    -1,   174,
     175,   176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,
     167,   168,   169,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    55,   134,   135,    58,    -1,    60,
      61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,
      -1,    -1,    -1,   172,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    55,   134,   135,    58,    -1,    60,    61,    -1,
      63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,   172,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,    -1,    -1,   171,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,
      -1,    -1,   167,   168,   169,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,
     167,   168,   169,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    55,   134,   135,    58,    -1,    60,
      61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    55,   134,   135,    58,    -1,    60,    61,    -1,
      63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,   172,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,
      -1,    -1,   167,   168,    -1,    -1,    -1,   172,    -1,   174,
     175,   176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   164,    -1,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    55,   134,   135,    58,    -1,    60,
      61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    55,    -1,    -1,
      58,   142,    60,    61,    -1,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,   164,   165,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    55,   134,   135,    58,    -1,
      60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    55,   134,   135,    58,    -1,    60,    61,
      -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    55,   134,   135,    58,    -1,    60,    61,    -1,    63,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    55,
     134,   135,    58,    -1,    60,    61,    -1,    63,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
     174,   175,   176,   177,   178,   179,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    55,   134,   135,
      58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,   165,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    55,   134,   135,    58,    -1,
      60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    55,   134,   135,    58,    -1,    60,    61,
      -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    55,   134,   135,    58,    -1,    60,    61,    -1,    63,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    55,
     134,   135,    58,    -1,    60,    61,    -1,    63,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
     174,   175,   176,   177,   178,   179,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    55,   134,   135,
      58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,   165,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    55,   134,   135,    58,    -1,
      60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    55,   134,   135,    58,    -1,    60,    61,
      -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    55,   134,   135,    58,    -1,    60,    61,    -1,    63,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    55,
     134,   135,    58,    -1,    60,    61,    -1,    63,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
     174,   175,   176,   177,   178,   179,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    55,   134,   135,
      58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    55,   134,   135,    58,    -1,
      60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    55,   134,   135,    58,    -1,    60,    61,
      -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    55,   134,   135,    58,    -1,    60,    61,    -1,    63,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    55,
     134,   135,    58,    -1,    60,    61,    -1,    63,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
     174,   175,   176,   177,   178,   179,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    55,   134,   135,
      58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   191,   418,   419,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    57,    59,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    76,    79,    80,   108,   112,   113,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   128,
     129,   142,   144,   164,   165,   167,   168,   175,   176,   187,
     189,   194,   195,   196,   209,   304,   305,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   315,   318,   321,   323,
     324,   325,   326,   327,   328,   329,   330,   331,   332,   334,
     336,   337,   338,   340,   341,   345,   346,   347,   348,   349,
     351,   357,   358,   359,   360,   372,   377,   410,   413,   423,
     429,   431,   437,   441,   446,   447,   448,   449,   450,   451,
     452,   453,   479,   497,   498,   499,   500,     0,   191,   113,
     195,   209,   308,   310,   321,   324,   327,   337,   341,   346,
     127,   164,    65,    68,    69,    71,   164,   164,   375,   435,
     436,   437,   333,   334,   115,   116,   195,   197,   411,   412,
     197,   164,   423,   164,   164,     4,   113,   115,   116,   325,
     330,   331,   164,    13,    95,   197,   438,   439,   440,   209,
     436,   441,   447,   448,   449,   451,   452,   453,   115,   348,
      55,    58,    60,    61,    63,    64,    80,   110,   111,   113,
     114,   125,   126,   127,   130,   131,   132,   134,   135,   164,
     168,   169,   174,   175,   177,   178,   179,   180,   193,   194,
     198,   199,   200,   203,   208,   209,   210,   211,   214,   215,
     216,   217,   218,   219,   220,   221,   222,   223,   224,   225,
     230,   235,   310,   311,   320,   322,   324,   328,   329,   336,
     337,   343,   344,   345,   346,   347,   350,   357,   358,   377,
     384,   385,   386,   387,   388,   389,   479,   492,   493,   494,
     495,   500,   501,   195,   168,   311,   321,   324,   446,   450,
     479,   496,   497,   500,   501,   189,   189,   192,   161,   172,
     188,   233,   393,    96,   170,   430,   108,   197,   434,   170,
     170,   170,   189,   115,   116,   164,   209,   316,   317,   441,
     442,   443,   444,   445,   446,   450,   454,   455,   456,   457,
     458,   459,   460,   461,   462,   468,     3,    53,    54,    56,
      62,   339,     3,   168,   209,   310,   311,   325,   329,   331,
     342,   347,   426,   446,   450,   500,    76,   308,   310,   324,
     337,   341,   346,   427,   446,   450,    72,   330,   330,   325,
     331,   319,   330,   331,   339,   358,   325,   330,   325,   167,
     435,   170,   192,   164,   172,   241,   435,   435,     3,   299,
     300,   315,   318,   324,   328,   168,   321,   324,   498,   500,
     197,   197,   423,   188,   324,   164,   209,   432,   441,   442,
     446,   455,   459,   168,   209,   311,   424,   425,    64,    72,
      73,    74,    75,   168,   186,   197,   399,   401,   405,   407,
     408,   347,   166,   168,   209,   320,   324,   337,   344,   346,
     389,   492,   500,   435,   115,   116,   179,   195,   347,   376,
     468,   437,   164,   406,   407,   164,   164,   438,   198,   168,
     208,   209,   225,   226,   347,   166,   168,   209,   230,   321,
     391,   392,   409,   496,   501,   169,   170,   164,   324,   447,
     448,   449,   451,   452,   453,   166,   166,   166,   166,   166,
     166,   166,   164,   208,   164,   164,   208,   164,   164,   435,
     211,   164,   208,   164,   113,   115,   116,   325,   330,   331,
     164,   208,   208,    19,    21,    92,   168,   177,   178,   212,
     213,   230,   237,   241,   360,   391,   500,   165,   169,   170,
     230,   324,   328,   115,   168,   195,   321,   324,   479,   498,
     164,   200,   169,   168,   173,   168,   173,   127,   131,   133,
     134,   135,   164,   167,   168,   172,   173,   146,   147,   148,
     149,   150,   151,   152,   153,   154,   155,   156,   188,   232,
     233,   234,   168,   211,   322,   324,   337,   344,   346,   491,
     492,   500,   501,   211,   181,   175,   182,   183,   177,   178,
     136,   137,   138,   139,   184,   185,   140,   141,   176,   174,
     186,   142,   143,   187,   169,   164,   164,   168,   176,   188,
     209,   441,   463,   464,   465,   466,   467,   468,   469,   470,
     471,   479,   481,   482,   483,   484,   485,   486,   503,   145,
     168,   209,   350,   494,   500,   324,   344,   330,   325,   167,
     435,   169,   170,   169,   170,   322,   324,   493,   500,   197,
     168,   322,   479,   493,   500,   164,   197,   169,   168,   446,
     450,   500,   168,   170,   113,   167,   168,   172,   194,   196,
     230,   394,   395,   396,   397,   398,    22,   394,   164,   197,
     241,   164,   164,   195,   432,   195,   436,   441,   443,   444,
     445,   454,   456,   457,   458,   460,   461,   462,   324,   442,
     455,   459,   170,   434,   168,   435,   476,   479,   434,   435,
     435,   430,   299,   164,   435,   476,   434,   435,   435,   430,
     435,   435,   324,   432,   164,   164,   323,   324,   321,   324,
     168,   169,   321,   496,   501,   434,   349,   172,   430,   299,
     197,   197,   393,   310,   329,   428,   446,   450,   172,   430,
     299,   411,   324,   337,   324,   324,   115,   348,   115,   116,
     195,   347,   352,   411,   145,   195,   324,   381,   382,   386,
     387,   390,   163,   191,   241,   315,   189,   446,   459,   324,
     175,   230,   385,   500,   197,   434,   164,   434,   192,   230,
     436,   441,   324,   164,   385,   421,   172,   164,   197,   172,
     197,   145,   175,   176,   404,   166,   170,   197,   408,   166,
     169,   164,   176,   209,   500,   166,   195,   376,   468,   373,
     172,   376,   399,   188,   399,   438,   166,   166,   230,   166,
     170,   164,   209,   472,   473,   474,   475,   476,   166,   170,
     439,   187,   227,   228,   229,   230,   166,   166,   166,   166,
     166,   166,   164,   435,   476,   479,   164,   476,   479,   391,
     501,   200,   391,   168,   391,   392,   195,   391,   501,   230,
     391,   166,   391,   391,   391,   169,   166,   177,   178,   213,
      18,   326,   166,   170,   166,   175,   176,   166,   170,   502,
     164,   322,   479,   493,   500,   169,   170,   168,   175,   209,
     230,   347,   230,   324,   164,   164,   321,   498,   172,   230,
     195,   230,   195,   125,   168,   195,   227,   125,   168,   197,
     360,   230,   227,   195,   172,   230,   500,   211,   214,   214,
     214,   215,   215,   216,   216,   217,   217,   217,   217,   218,
     218,   219,   220,   221,   222,   223,   171,   237,   191,   164,
     381,   441,   464,   465,   466,   469,   482,   483,   484,   169,
     191,    18,   230,   324,   463,   467,   481,   164,   435,   485,
     503,   435,   435,   503,   164,   435,   485,   435,   435,   503,
     435,   435,   479,   169,   226,   169,   324,   322,   491,   501,
     197,   324,   168,   195,   195,   385,   388,   388,   389,   503,
     322,   493,   500,   191,   503,   191,   168,   196,   225,   226,
     433,   395,   171,   170,   502,   394,   167,   168,   188,   398,
     409,   164,   198,   191,   188,   441,   443,   444,   445,   454,
     456,   457,   458,   460,   461,   462,   166,   166,   166,   166,
     166,   166,   166,   166,   166,   166,   442,   455,   459,   435,
     188,   169,   230,   331,   347,   477,   393,   241,   430,   381,
     393,   241,   432,   237,   392,   237,   392,   432,   115,   421,
     241,   430,   172,   172,   430,   299,   421,   241,   430,   354,
     355,   353,   172,   166,   170,   166,   170,    77,   301,   302,
     189,   169,   169,   170,   197,   434,   191,   441,   423,   421,
     197,   169,     1,   308,   310,   322,   324,   414,   415,   416,
     417,   164,   403,   401,   402,    85,   335,    18,   324,   435,
     172,   435,   376,    10,   174,   376,   378,   379,   172,   166,
     392,   166,   166,   189,   198,   381,   473,   474,   475,   324,
     472,   435,   435,   230,   392,   195,   166,   170,   164,   435,
     476,   479,   164,   476,   479,   381,   381,   166,   166,   170,
     166,   170,   166,   166,   166,   170,   166,   211,   166,   166,
     166,   211,    18,   326,   230,   166,   166,   165,   172,   211,
     165,   230,   236,   169,   145,   383,   384,   385,   322,   493,
     500,   169,   236,   169,   169,   169,   230,   191,   191,   227,
     169,   169,   125,   130,   132,   196,   204,   205,   206,   166,
     204,   169,   170,   163,   395,   225,   171,   383,   469,   166,
     166,   166,   166,   166,   166,   166,   166,     5,   324,   164,
     435,   441,   468,   463,   467,   481,   381,   381,   169,   503,
     204,   169,   170,   383,   197,   204,   145,   169,   180,   169,
     502,   394,   396,   163,   166,   191,   166,   383,   230,   166,
     166,   166,   166,   166,   166,   166,   166,   166,   164,   435,
     476,   479,   164,   435,   476,   479,   164,   435,   476,   479,
     432,    22,   479,   158,   170,   180,   478,   169,   170,   241,
     166,   166,   166,   166,   166,   419,   420,   241,   163,   414,
     421,   241,   430,   420,   241,   172,   172,   172,   361,   145,
     386,   387,   195,   197,   303,    18,    78,    80,    81,    83,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,   100,   101,   102,   103,   104,   105,   106,   108,
     115,   116,   128,   164,   168,   197,   237,   238,   239,   240,
     241,   242,   243,   245,   246,   255,   262,   263,   264,   265,
     266,   267,   272,   273,   276,   277,   278,   279,   280,   281,
     282,   288,   289,   290,   304,   324,   328,   431,    77,   434,
     383,   422,   420,   166,   432,   163,   415,   170,   189,   170,
     189,   409,   188,   400,   400,   374,   378,   376,   172,   347,
     170,   502,   197,   378,   172,   166,   166,   166,   166,   166,
     166,   472,   188,   229,   381,   381,   166,   166,   320,   195,
      85,   201,   202,   391,   211,   211,   211,   211,   211,   172,
     395,   170,   502,   166,   170,   170,   502,   169,   383,   383,
     163,   207,   168,   205,   207,   207,   169,   170,   133,   167,
     169,   236,   502,   225,   192,   166,   164,   435,   476,   479,
     164,   435,   485,   164,   435,   485,   479,   323,     5,   175,
     192,   230,   441,   435,   435,   166,   166,   169,   388,   192,
     393,   169,   226,   226,   163,   394,   435,   383,   435,   192,
     164,   435,   476,   479,   164,   435,   476,   479,   164,   435,
     476,   479,   381,   381,   381,   434,   237,   230,   230,   331,
     347,   422,   163,   420,   241,   422,   361,   361,   361,     3,
       5,    10,    80,   163,   305,   312,   313,   321,   324,   362,
     368,   496,   170,   189,   164,    68,    69,   189,   241,   304,
     431,   164,   164,    18,   239,   164,   164,   189,   197,   189,
     197,   175,   197,   172,   238,   164,   164,   164,   239,   164,
     241,   230,   231,   231,    14,   291,   267,   278,   171,   189,
     192,   243,    85,   189,   197,    98,    99,   271,   275,   119,
     143,   270,   118,   142,   274,   270,   390,   324,   303,   192,
     422,   197,   197,   432,   166,   392,   406,   406,   376,   502,
     172,   378,    10,   379,   163,   188,   380,   502,   163,   414,
     164,   435,   476,   479,   230,   166,   166,   480,   481,   166,
     171,   166,   170,   171,   395,   502,   165,   230,   169,   145,
     385,   145,   169,   192,   192,   131,   204,   205,   168,   205,
     168,   205,   169,   170,   163,   166,   381,   381,   381,   230,
     230,   192,   169,   192,   166,   169,   192,   166,   381,   381,
     381,   166,   166,   166,   393,   169,   478,   163,   422,   163,
     163,   163,   163,   321,   321,   360,   369,   496,   321,   368,
     164,   356,   189,   189,   189,   164,   171,   209,   363,   364,
     365,   371,   441,   442,   455,   459,   170,   189,   197,   197,
     227,   189,   241,   189,   241,   237,   247,   304,   306,   309,
     315,   324,   328,   237,    87,   166,   247,   157,   158,   159,
     160,   165,   166,   189,   237,   256,   257,   259,   304,   189,
     189,   237,   189,   395,   189,   237,   409,   237,   256,   120,
     121,   122,   123,   124,   283,   285,   286,   189,   107,   189,
      91,   164,   166,   435,   163,   189,   189,   164,   164,   239,
     239,   267,   164,   277,   267,   277,   241,   189,   166,   163,
     404,   172,   163,   378,   502,   347,   197,   172,   226,   163,
     163,   381,   166,   230,   202,   230,   502,   163,   166,   166,
     169,   204,   204,   166,   166,   166,   192,   192,   169,   169,
     166,   435,   166,   166,   166,   230,   163,   356,   356,   356,
     363,   164,   209,   366,   367,   476,   487,   488,   489,   490,
     189,   170,   189,   363,   189,   409,   436,   441,   230,   324,
     163,   170,   189,   370,   371,   370,   370,   197,   166,   166,
     237,   324,   166,   164,   239,   166,   157,   158,   159,   160,
     180,   189,   260,   261,   239,   238,   189,   261,   166,   171,
     237,   165,   237,   238,   259,   189,   502,   166,   166,   166,
     166,   241,   285,   286,   164,   230,   164,   198,     1,   239,
     211,   268,   237,    82,   117,   269,   271,    82,   435,   400,
     378,   502,   163,   380,   395,   166,   163,   435,   435,   169,
     169,   169,   169,   189,   488,   489,   490,   324,   487,   170,
     189,   435,   435,   189,   166,   441,   435,   239,   239,    84,
      85,   172,   250,   251,   252,   166,   237,    82,   239,   237,
     165,   237,    82,   189,   165,   237,   238,   259,   324,   346,
     165,   237,   239,   257,   261,   261,   189,   237,   163,   172,
     252,   239,   239,   164,   287,   322,   324,   496,   189,   198,
     166,   171,   166,   170,   171,   166,   239,   164,   239,   239,
     239,   406,   502,   163,   502,   166,   166,   166,   487,   435,
     365,    82,     1,   226,   248,   249,   433,     1,   171,     1,
     191,   239,   250,    82,   189,   166,   239,    82,   189,   180,
     180,   239,   238,   261,   261,   189,    64,   237,   258,   347,
     180,   180,    82,   165,   237,   165,   237,   238,   189,     1,
     191,   287,   189,   284,   164,   209,   432,   487,   195,   171,
     189,   168,   198,   292,   293,   294,   211,   227,   237,   270,
     163,   163,   164,   435,   476,   479,   367,   239,   145,     1,
     170,   171,   163,   297,   298,   304,   239,    82,   189,   239,
     237,   165,   165,   237,   165,   237,   165,   237,   238,   195,
     347,   165,   237,   165,   237,   239,   180,   180,   180,   180,
     163,   297,   284,   225,   166,   324,   171,   113,   164,   166,
     171,   170,   166,   166,    82,   266,   381,   226,   248,   251,
     253,   254,   304,   239,   180,   180,   180,   180,   165,   165,
     237,   165,   237,   165,   237,   253,   166,   241,   292,   169,
     226,   189,   292,   294,   239,    82,   166,   239,   244,   192,
     251,   165,   165,   237,   165,   237,   165,   237,   192,   241,
     171,   198,   166,   166,   171,   239,     1,   239,   163,   244,
     163,   198,   295,   164,   189,   295,   170,   171,   226,   166,
     198,   195,   296,   166,   189,   166,   170,   189,   195
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   190,   191,   192,   193,   193,   193,   193,   193,   194,
     194,   194,   194,   194,   194,   194,   194,   195,   195,   196,
     196,   197,   197,   197,   198,   199,   199,   200,   200,   200,
     200,   200,   200,   200,   200,   200,   200,   200,   200,   200,
     200,   200,   201,   201,   202,   202,   203,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     204,   204,   205,   205,   205,   205,   205,   205,   205,   206,
     206,   206,   207,   207,   208,   208,   208,   208,   208,   208,
     208,   208,   208,   208,   208,   208,   208,   208,   208,   208,
     208,   208,   209,   209,   209,   210,   210,   210,   210,   211,
     211,   211,   211,   211,   211,   211,   211,   211,   212,   212,
     212,   212,   213,   213,   214,   214,   215,   215,   215,   215,
     216,   216,   216,   217,   217,   217,   218,   218,   218,   218,
     218,   219,   219,   219,   220,   220,   221,   221,   222,   222,
     223,   223,   224,   224,   225,   225,   225,   226,   227,   227,
     228,   228,   229,   229,   229,   230,   230,   230,   231,   231,
     232,   232,   233,   233,   234,   234,   234,   234,   234,   234,
     234,   234,   234,   234,   234,   235,   235,   235,   235,   235,
     236,   236,   236,   236,   237,   237,   238,   238,   239,   239,
     239,   239,   239,   239,   239,   239,   239,   239,   239,   239,
     239,   239,   239,   239,   240,   240,   241,   241,   242,   242,
     243,   243,   243,   243,   243,   244,   244,   244,   245,   246,
     246,   246,   246,   246,   246,   246,   246,   247,   247,   247,
     247,   248,   248,   248,   249,   249,   250,   250,   250,   250,
     250,   251,   251,   252,   253,   253,   254,   254,   255,   255,
     255,   255,   255,   255,   255,   255,   255,   255,   255,   255,
     256,   256,   257,   257,   257,   257,   257,   257,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   258,   258,   258,
     259,   259,   259,   259,   260,   260,   260,   261,   261,   261,
     262,   262,   262,   262,   262,   262,   262,   262,   262,   262,
     262,   262,   262,   262,   262,   262,   262,   262,   262,   262,
     263,   263,   264,   265,   266,   267,   267,   268,   268,   269,
     270,   270,   271,   271,   272,   272,   272,   272,   272,   272,
     273,   274,   274,   275,   276,   276,   277,   277,   278,   278,
     278,   279,   280,   281,   282,   282,   282,   283,   283,   284,
     284,   285,   285,   285,   285,   286,   287,   287,   287,   287,
     287,   288,   289,   289,   290,   290,   290,   290,   290,   291,
     291,   292,   292,   293,   293,   294,   294,   295,   295,   295,
     296,   296,   297,   297,   298,   298,   299,   299,   300,   300,
     301,   301,   302,   302,   303,   303,   304,   304,   304,   305,
     305,   306,   306,   306,   306,   306,   307,   307,   307,   308,
     308,   308,   308,   308,   308,   309,   309,   309,   309,   309,
     310,   310,   310,   310,   311,   311,   312,   312,   312,   313,
     313,   313,   313,   313,   314,   314,   315,   315,   315,   315,
     316,   316,   316,   316,   316,   317,   317,   318,   318,   318,
     318,   319,   319,   319,   320,   320,   320,   321,   321,   321,
     322,   322,   322,   323,   323,   324,   324,   325,   325,   326,
     326,   326,   326,   326,   327,   328,   328,   328,   329,   329,
     330,   330,   330,   330,   330,   330,   330,   330,   330,   331,
     332,   332,   332,   332,   332,   332,   332,   332,   332,   332,
     332,   332,   332,   332,   332,   332,   332,   332,   332,   332,
     332,   332,   332,   332,   332,   332,   332,   332,   332,   332,
     332,   332,   332,   332,   333,   333,   334,   335,   335,   336,
     336,   336,   336,   336,   337,   337,   338,   338,   338,   338,
     339,   339,   339,   339,   339,   339,   340,   340,   340,   340,
     341,   342,   341,   341,   343,   343,   343,   343,   344,   344,
     344,   345,   345,   345,   345,   346,   346,   346,   347,   347,
     347,   347,   347,   347,   348,   348,   348,   349,   349,   350,
     350,   352,   351,   353,   351,   354,   351,   355,   351,   351,
     356,   356,   357,   357,   358,   358,   359,   359,   359,   360,
     360,   360,   360,   360,   360,   360,   360,   361,   361,   362,
     362,   362,   362,   362,   362,   362,   362,   362,   362,   362,
     362,   363,   363,   364,   364,   365,   365,   365,   365,   366,
     366,   366,   367,   368,   368,   369,   369,   370,   370,   371,
     372,   372,   373,   372,   372,   374,   372,   372,   372,   375,
     375,   376,   376,   377,   377,   378,   378,   378,   378,   378,
     379,   379,   380,   380,   380,   381,   381,   381,   381,   382,
     382,   382,   382,   383,   383,   383,   383,   383,   383,   383,
     384,   384,   384,   384,   385,   385,   386,   386,   387,   387,
     388,   388,   388,   388,   388,   389,   389,   389,   389,   389,
     390,   390,   391,   391,   391,   392,   392,   393,   393,   393,
     393,   394,   394,   395,   395,   395,   395,   395,   396,   396,
     397,   397,   398,   398,   398,   398,   398,   399,   399,   400,
     400,   402,   401,   403,   401,   401,   401,   401,   404,   404,
     404,   404,   405,   405,   405,   405,   406,   406,   407,   407,
     408,   408,   409,   409,   409,   409,   410,   410,   410,   411,
     411,   412,   412,   413,   413,   413,   413,   414,   414,   415,
     415,   416,   416,   416,   417,   417,   417,   418,   418,   419,
     419,   420,   420,   421,   422,   423,   423,   423,   423,   423,
     423,   423,   423,   423,   423,   423,   424,   423,   425,   423,
     426,   423,   427,   423,   428,   423,   423,   429,   429,   429,
     430,   430,   431,   431,   431,   431,   431,   431,   431,   431,
     431,   431,   432,   432,   432,   432,   433,   434,   434,   435,
     435,   436,   436,   437,   437,   437,   437,   438,   438,   439,
     439,   439,   440,   440,   440,   441,   441,   441,   442,   442,
     442,   442,   443,   443,   443,   443,   444,   444,   444,   444,
     444,   444,   444,   445,   445,   445,   445,   446,   446,   446,
     447,   447,   447,   447,   447,   448,   448,   448,   448,   449,
     449,   449,   449,   449,   449,   450,   450,   450,   451,   451,
     451,   451,   451,   452,   452,   452,   452,   453,   453,   453,
     453,   453,   453,   454,   454,   455,   455,   455,   455,   456,
     456,   456,   456,   457,   457,   457,   457,   457,   457,   457,
     458,   458,   458,   458,   459,   459,   459,   460,   460,   460,
     460,   460,   461,   461,   461,   461,   462,   462,   462,   462,
     462,   462,   463,   463,   463,   463,   463,   464,   464,   464,
     465,   465,   465,   465,   466,   466,   466,   467,   467,   467,
     467,   467,   468,   468,   469,   469,   469,   470,   470,   471,
     471,   472,   472,   472,   473,   473,   473,   473,   473,   474,
     474,   474,   474,   475,   475,   475,   476,   476,   476,   476,
     476,   477,   477,   477,   477,   477,   477,   478,   478,   479,
     479,   479,   479,   480,   480,   481,   481,   481,   481,   482,
     482,   482,   482,   482,   483,   483,   483,   483,   484,   484,
     484,   485,   485,   485,   486,   486,   486,   486,   486,   486,
     487,   487,   487,   488,   488,   488,   488,   488,   489,   489,
     489,   489,   490,   490,   491,   491,   491,   492,   492,   492,
     493,   493,   493,   493,   493,   493,   494,   494,   494,   494,
     494,   494,   494,   494,   494,   494,   494,   494,   494,   494,
     494,   495,   495,   495,   495,   496,   496,   496,   497,   497,
     498,   498,   498,   498,   498,   498,   499,   499,   499,   499,
     499,   499,   500,   500,   500,   501,   501,   501,   502,   502,
     503,   503
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
       1,     1,     5,     5,     4,     0,     1,     1,     3,     4,
       1,     1,     4,     6,     3,     5,     5,     5,     8,     9,
       1,     1,     1,     4,     3,     3,     1,     3,     1,     3,
       5,     1,     2,     5,     3,     3,     4,     6,     7,     0,
       2,     1,     1,     1,     1,     2,     1,     2,     2,     2,
       1,     3,     1,     1,     6,     8,    10,    12,    14,     0,
       1,     0,     1,     1,     3,     4,     7,     0,     1,     3,
       1,     3,     0,     1,     1,     2,     0,     1,     2,     3,
       0,     1,     3,     4,     1,     3,     2,     2,     2,     6,
       4,     1,     1,     1,     1,     1,     2,     3,     6,     3,
       3,     4,     5,     2,     3,     1,     2,     2,     3,     8,
       9,     9,     8,     8,     3,     5,     2,     2,     3,     3,
       3,     4,     3,     4,     4,     5,     2,     1,     1,     1,
       3,     3,     2,     4,     6,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     4,     1,     2,     3,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
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
       2,     0,     1,     1,     4,     1,     2,     2,     2,     0,
       1,     4,     1,     2,     3,     1,     2,     0,     1,     2,
       7,     8,     0,     9,     8,     0,    11,    10,     1,     2,
       3,     0,     1,     3,     3,     0,     3,     2,     5,     4,
       1,     1,     0,     2,     5,     0,     1,     1,     3,     1,
       1,     3,     3,     0,     1,     1,     1,     3,     3,     3,
       1,     3,     3,     5,     1,     3,     3,     3,     2,     3,
       1,     3,     3,     4,     1,     1,     1,     1,     2,     1,
       1,     3,     1,     1,     2,     1,     1,     0,     2,     2,
       4,     1,     4,     0,     1,     2,     3,     4,     2,     2,
       1,     2,     2,     3,     3,     5,     4,     1,     3,     0,
       2,     0,     5,     0,     5,     3,     1,     8,     0,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     2,
       5,     4,     1,     1,     3,     3,     2,     3,     3,     2,
       4,     1,     4,     7,     5,     8,     6,     1,     2,     2,
       2,     1,     1,     3,     2,     3,     1,     0,     1,     3,
       4,     0,     1,     0,     0,     1,     1,     2,     2,     2,
       2,     2,     2,     1,     2,     5,     0,     6,     0,     8,
       0,     7,     0,     7,     0,     8,     1,     1,     2,     3,
       0,     5,     3,     4,     4,     4,     4,     5,     5,     5,
       5,     6,     1,     1,     1,     1,     3,     0,     5,     0,
       1,     1,     2,     6,     4,     3,     1,     1,     3,     0,
       1,     4,     1,     1,     1,     1,     2,     3,     2,     1,
       2,     2,     2,     3,     4,     5,     2,     4,     5,     4,
       5,     3,     4,     6,     7,     3,     4,     2,     1,     2,
       4,     6,     7,     3,     4,     2,     3,     4,     5,     4,
       5,     4,     5,     3,     4,     1,     1,     1,     4,     6,
       7,     3,     4,     2,     3,     3,     4,     4,     5,     4,
       5,     3,     4,     1,     3,     2,     1,     2,     2,     2,
       3,     4,     5,     2,     4,     5,     4,     5,     3,     4,
       6,     7,     3,     4,     2,     1,     2,     4,     6,     7,
       3,     4,     2,     3,     4,     5,     4,     5,     4,     5,
       3,     4,     2,     4,     1,     2,     2,     2,     3,     4,
       2,     4,     4,     3,     4,     6,     3,     2,     4,     1,
       2,     2,     1,     1,     2,     3,     4,     2,     4,     4,
       6,     1,     2,     2,     1,     2,     2,     3,     4,     1,
       4,     4,     3,     3,     6,     3,     2,     3,     5,     3,
       1,     1,     1,     3,     3,     3,     5,     1,     1,     3,
       3,     4,     4,     0,     1,     1,     3,     2,     2,     1,
       2,     2,     3,     4,     1,     4,     4,     3,     3,     6,
       3,     1,     2,     1,     2,     6,     5,     6,     7,     7,
       1,     2,     2,     1,     2,     2,     3,     4,     1,     4,
       4,     3,     6,     3,     1,     1,     2,     1,     1,     2,
       2,     3,     2,     3,     2,     3,     3,     3,     2,     2,
       4,     4,     3,     3,     2,     2,     3,     2,     4,     3,
       2,     4,     4,     4,     5,     1,     2,     1,     1,     1,
       2,     3,     2,     3,     2,     3,     3,     4,     2,     3,
       4,     2,     3,     4,     5,     5,     6,     6,     0,     1,
       0,     2
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
#line 10007 "Parser/parser.cc"
    break;

  case 3:
#line 644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 10013 "Parser/parser.cc"
    break;

  case 4:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 10019 "Parser/parser.cc"
    break;

  case 5:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 10025 "Parser/parser.cc"
    break;

  case 6:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 10031 "Parser/parser.cc"
    break;

  case 7:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 10037 "Parser/parser.cc"
    break;

  case 8:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 10043 "Parser/parser.cc"
    break;

  case 20:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 10049 "Parser/parser.cc"
    break;

  case 24:
#line 687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 10055 "Parser/parser.cc"
    break;

  case 25:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 10061 "Parser/parser.cc"
    break;

  case 26:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 10071 "Parser/parser.cc"
    break;

  case 27:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 10077 "Parser/parser.cc"
    break;

  case 28:
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 10083 "Parser/parser.cc"
    break;

  case 29:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 10089 "Parser/parser.cc"
    break;

  case 31:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10095 "Parser/parser.cc"
    break;

  case 32:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 10101 "Parser/parser.cc"
    break;

  case 33:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10107 "Parser/parser.cc"
    break;

  case 34:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10113 "Parser/parser.cc"
    break;

  case 35:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 10123 "Parser/parser.cc"
    break;

  case 36:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 10129 "Parser/parser.cc"
    break;

  case 37:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 10135 "Parser/parser.cc"
    break;

  case 38:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 10141 "Parser/parser.cc"
    break;

  case 39:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10147 "Parser/parser.cc"
    break;

  case 40:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10153 "Parser/parser.cc"
    break;

  case 41:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10159 "Parser/parser.cc"
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
#line 10171 "Parser/parser.cc"
    break;

  case 44:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 10180 "Parser/parser.cc"
    break;

  case 45:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 10186 "Parser/parser.cc"
    break;

  case 47:
#line 770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 10192 "Parser/parser.cc"
    break;

  case 48:
#line 776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10198 "Parser/parser.cc"
    break;

  case 49:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10204 "Parser/parser.cc"
    break;

  case 50:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10210 "Parser/parser.cc"
    break;

  case 51:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 10220 "Parser/parser.cc"
    break;

  case 52:
#line 788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10226 "Parser/parser.cc"
    break;

  case 53:
#line 790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_va_arg( yylloc, (yyvsp[-4].expr), ( (yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl) ) ) ); }
#line 10232 "Parser/parser.cc"
    break;

  case 54:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10238 "Parser/parser.cc"
    break;

  case 55:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10244 "Parser/parser.cc"
    break;

  case 56:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10250 "Parser/parser.cc"
    break;

  case 57:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10256 "Parser/parser.cc"
    break;

  case 58:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10262 "Parser/parser.cc"
    break;

  case 59:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10268 "Parser/parser.cc"
    break;

  case 60:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10274 "Parser/parser.cc"
    break;

  case 61:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 10280 "Parser/parser.cc"
    break;

  case 62:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10286 "Parser/parser.cc"
    break;

  case 63:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10292 "Parser/parser.cc"
    break;

  case 64:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10298 "Parser/parser.cc"
    break;

  case 65:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 10304 "Parser/parser.cc"
    break;

  case 66:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 10310 "Parser/parser.cc"
    break;

  case 67:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 10316 "Parser/parser.cc"
    break;

  case 68:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 10322 "Parser/parser.cc"
    break;

  case 69:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 10332 "Parser/parser.cc"
    break;

  case 71:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10338 "Parser/parser.cc"
    break;

  case 73:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10344 "Parser/parser.cc"
    break;

  case 74:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10350 "Parser/parser.cc"
    break;

  case 75:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10356 "Parser/parser.cc"
    break;

  case 76:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10362 "Parser/parser.cc"
    break;

  case 77:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10368 "Parser/parser.cc"
    break;

  case 78:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10374 "Parser/parser.cc"
    break;

  case 79:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10380 "Parser/parser.cc"
    break;

  case 80:
#line 873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10386 "Parser/parser.cc"
    break;

  case 81:
#line 875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 10394 "Parser/parser.cc"
    break;

  case 82:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10400 "Parser/parser.cc"
    break;

  case 83:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 10409 "Parser/parser.cc"
    break;

  case 86:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10415 "Parser/parser.cc"
    break;

  case 87:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 10421 "Parser/parser.cc"
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
#line 10441 "Parser/parser.cc"
    break;

  case 89:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 10447 "Parser/parser.cc"
    break;

  case 90:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 10453 "Parser/parser.cc"
    break;

  case 91:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 10459 "Parser/parser.cc"
    break;

  case 92:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10465 "Parser/parser.cc"
    break;

  case 93:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10471 "Parser/parser.cc"
    break;

  case 94:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10477 "Parser/parser.cc"
    break;

  case 95:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10483 "Parser/parser.cc"
    break;

  case 96:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10489 "Parser/parser.cc"
    break;

  case 97:
#line 938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10495 "Parser/parser.cc"
    break;

  case 98:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 10501 "Parser/parser.cc"
    break;

  case 99:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 10510 "Parser/parser.cc"
    break;

  case 100:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10516 "Parser/parser.cc"
    break;

  case 101:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10522 "Parser/parser.cc"
    break;

  case 102:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 10528 "Parser/parser.cc"
    break;

  case 103:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 10534 "Parser/parser.cc"
    break;

  case 104:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 10540 "Parser/parser.cc"
    break;

  case 105:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 10546 "Parser/parser.cc"
    break;

  case 106:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 10552 "Parser/parser.cc"
    break;

  case 107:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 10558 "Parser/parser.cc"
    break;

  case 108:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 10564 "Parser/parser.cc"
    break;

  case 110:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 10570 "Parser/parser.cc"
    break;

  case 111:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10576 "Parser/parser.cc"
    break;

  case 112:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10582 "Parser/parser.cc"
    break;

  case 113:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 10588 "Parser/parser.cc"
    break;

  case 114:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 10594 "Parser/parser.cc"
    break;

  case 115:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::ReturnCast ) ); }
#line 10600 "Parser/parser.cc"
    break;

  case 116:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10606 "Parser/parser.cc"
    break;

  case 117:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10612 "Parser/parser.cc"
    break;

  case 125:
#line 1004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10618 "Parser/parser.cc"
    break;

  case 127:
#line 1010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10624 "Parser/parser.cc"
    break;

  case 128:
#line 1012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10630 "Parser/parser.cc"
    break;

  case 129:
#line 1014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10636 "Parser/parser.cc"
    break;

  case 131:
#line 1020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10642 "Parser/parser.cc"
    break;

  case 132:
#line 1022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10648 "Parser/parser.cc"
    break;

  case 134:
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10654 "Parser/parser.cc"
    break;

  case 135:
#line 1030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10660 "Parser/parser.cc"
    break;

  case 137:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10666 "Parser/parser.cc"
    break;

  case 138:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10672 "Parser/parser.cc"
    break;

  case 139:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10678 "Parser/parser.cc"
    break;

  case 140:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10684 "Parser/parser.cc"
    break;

  case 142:
#line 1048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10690 "Parser/parser.cc"
    break;

  case 143:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10696 "Parser/parser.cc"
    break;

  case 145:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10702 "Parser/parser.cc"
    break;

  case 147:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10708 "Parser/parser.cc"
    break;

  case 149:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10714 "Parser/parser.cc"
    break;

  case 151:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 10720 "Parser/parser.cc"
    break;

  case 153:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 10726 "Parser/parser.cc"
    break;

  case 155:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10732 "Parser/parser.cc"
    break;

  case 156:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 10738 "Parser/parser.cc"
    break;

  case 158:
#line 1097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10744 "Parser/parser.cc"
    break;

  case 161:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10750 "Parser/parser.cc"
    break;

  case 162:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 10756 "Parser/parser.cc"
    break;

  case 163:
#line 1114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10762 "Parser/parser.cc"
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
#line 10774 "Parser/parser.cc"
    break;

  case 167:
#line 1130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10780 "Parser/parser.cc"
    break;

  case 168:
#line 1135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10786 "Parser/parser.cc"
    break;

  case 172:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 10792 "Parser/parser.cc"
    break;

  case 173:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 10798 "Parser/parser.cc"
    break;

  case 174:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 10804 "Parser/parser.cc"
    break;

  case 175:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 10810 "Parser/parser.cc"
    break;

  case 176:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 10816 "Parser/parser.cc"
    break;

  case 177:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 10822 "Parser/parser.cc"
    break;

  case 178:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 10828 "Parser/parser.cc"
    break;

  case 179:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 10834 "Parser/parser.cc"
    break;

  case 180:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 10840 "Parser/parser.cc"
    break;

  case 181:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 10846 "Parser/parser.cc"
    break;

  case 182:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 10852 "Parser/parser.cc"
    break;

  case 183:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 10858 "Parser/parser.cc"
    break;

  case 184:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 10864 "Parser/parser.cc"
    break;

  case 185:
#line 1168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Empty tuple is meaningless." ); (yyval.expr) = nullptr; }
#line 10870 "Parser/parser.cc"
    break;

  case 186:
#line 1170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-2].expr) ) ); }
#line 10876 "Parser/parser.cc"
    break;

  case 187:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10882 "Parser/parser.cc"
    break;

  case 188:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-2].expr) ) ) ); }
#line 10888 "Parser/parser.cc"
    break;

  case 189:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10894 "Parser/parser.cc"
    break;

  case 191:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10900 "Parser/parser.cc"
    break;

  case 192:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10906 "Parser/parser.cc"
    break;

  case 193:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10912 "Parser/parser.cc"
    break;

  case 195:
#line 1192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10918 "Parser/parser.cc"
    break;

  case 196:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10924 "Parser/parser.cc"
    break;

  case 211:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10930 "Parser/parser.cc"
    break;

  case 213:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 10936 "Parser/parser.cc"
    break;

  case 214:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 10942 "Parser/parser.cc"
    break;

  case 215:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 10953 "Parser/parser.cc"
    break;

  case 216:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 10959 "Parser/parser.cc"
    break;

  case 217:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 10965 "Parser/parser.cc"
    break;

  case 219:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10971 "Parser/parser.cc"
    break;

  case 220:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10977 "Parser/parser.cc"
    break;

  case 221:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10983 "Parser/parser.cc"
    break;

  case 222:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10989 "Parser/parser.cc"
    break;

  case 223:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10995 "Parser/parser.cc"
    break;

  case 226:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 11001 "Parser/parser.cc"
    break;

  case 227:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 11008 "Parser/parser.cc"
    break;

  case 228:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 11014 "Parser/parser.cc"
    break;

  case 229:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 11020 "Parser/parser.cc"
    break;

  case 230:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11026 "Parser/parser.cc"
    break;

  case 231:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 11032 "Parser/parser.cc"
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
#line 11046 "Parser/parser.cc"
    break;

  case 233:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 11052 "Parser/parser.cc"
    break;

  case 234:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 11058 "Parser/parser.cc"
    break;

  case 235:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 11067 "Parser/parser.cc"
    break;

  case 236:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 11073 "Parser/parser.cc"
    break;

  case 237:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( nullptr, (yyvsp[0].expr) ); }
#line 11079 "Parser/parser.cc"
    break;

  case 238:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 11085 "Parser/parser.cc"
    break;

  case 239:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 11091 "Parser/parser.cc"
    break;

  case 240:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 11097 "Parser/parser.cc"
    break;

  case 241:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 11103 "Parser/parser.cc"
    break;

  case 242:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 11109 "Parser/parser.cc"
    break;

  case 244:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 11115 "Parser/parser.cc"
    break;

  case 245:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 11121 "Parser/parser.cc"
    break;

  case 246:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 11127 "Parser/parser.cc"
    break;

  case 247:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 11133 "Parser/parser.cc"
    break;

  case 248:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 11139 "Parser/parser.cc"
    break;

  case 249:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 11145 "Parser/parser.cc"
    break;

  case 250:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 11151 "Parser/parser.cc"
    break;

  case 252:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 11157 "Parser/parser.cc"
    break;

  case 253:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11163 "Parser/parser.cc"
    break;

  case 254:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 11169 "Parser/parser.cc"
    break;

  case 256:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11175 "Parser/parser.cc"
    break;

  case 257:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 11181 "Parser/parser.cc"
    break;

  case 258:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11187 "Parser/parser.cc"
    break;

  case 259:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11196 "Parser/parser.cc"
    break;

  case 260:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11202 "Parser/parser.cc"
    break;

  case 261:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11208 "Parser/parser.cc"
    break;

  case 262:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 11214 "Parser/parser.cc"
    break;

  case 263:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11223 "Parser/parser.cc"
    break;

  case 264:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 11229 "Parser/parser.cc"
    break;

  case 265:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11235 "Parser/parser.cc"
    break;

  case 266:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11241 "Parser/parser.cc"
    break;

  case 267:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11250 "Parser/parser.cc"
    break;

  case 268:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11256 "Parser/parser.cc"
    break;

  case 269:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11262 "Parser/parser.cc"
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
#line 11281 "Parser/parser.cc"
    break;

  case 272:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11287 "Parser/parser.cc"
    break;

  case 273:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 11295 "Parser/parser.cc"
    break;

  case 274:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11301 "Parser/parser.cc"
    break;

  case 275:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 11307 "Parser/parser.cc"
    break;

  case 276:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11313 "Parser/parser.cc"
    break;

  case 277:
#line 1472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11319 "Parser/parser.cc"
    break;

  case 278:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11325 "Parser/parser.cc"
    break;

  case 279:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11331 "Parser/parser.cc"
    break;

  case 280:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11340 "Parser/parser.cc"
    break;

  case 281:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11349 "Parser/parser.cc"
    break;

  case 282:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11355 "Parser/parser.cc"
    break;

  case 283:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11364 "Parser/parser.cc"
    break;

  case 284:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11373 "Parser/parser.cc"
    break;

  case 285:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11379 "Parser/parser.cc"
    break;

  case 286:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11385 "Parser/parser.cc"
    break;

  case 287:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11391 "Parser/parser.cc"
    break;

  case 288:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11397 "Parser/parser.cc"
    break;

  case 289:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11403 "Parser/parser.cc"
    break;

  case 290:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11409 "Parser/parser.cc"
    break;

  case 291:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11415 "Parser/parser.cc"
    break;

  case 292:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11421 "Parser/parser.cc"
    break;

  case 293:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11430 "Parser/parser.cc"
    break;

  case 294:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11440 "Parser/parser.cc"
    break;

  case 295:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11446 "Parser/parser.cc"
    break;

  case 296:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11452 "Parser/parser.cc"
    break;

  case 297:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11461 "Parser/parser.cc"
    break;

  case 298:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11471 "Parser/parser.cc"
    break;

  case 299:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11477 "Parser/parser.cc"
    break;

  case 300:
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11486 "Parser/parser.cc"
    break;

  case 301:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11496 "Parser/parser.cc"
    break;

  case 302:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11502 "Parser/parser.cc"
    break;

  case 303:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 11508 "Parser/parser.cc"
    break;

  case 304:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11514 "Parser/parser.cc"
    break;

  case 305:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11520 "Parser/parser.cc"
    break;

  case 306:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11529 "Parser/parser.cc"
    break;

  case 307:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11539 "Parser/parser.cc"
    break;

  case 308:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11545 "Parser/parser.cc"
    break;

  case 309:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11554 "Parser/parser.cc"
    break;

  case 310:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11564 "Parser/parser.cc"
    break;

  case 311:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11570 "Parser/parser.cc"
    break;

  case 312:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11579 "Parser/parser.cc"
    break;

  case 313:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11589 "Parser/parser.cc"
    break;

  case 314:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11595 "Parser/parser.cc"
    break;

  case 315:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 11603 "Parser/parser.cc"
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
#line 11615 "Parser/parser.cc"
    break;

  case 317:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11624 "Parser/parser.cc"
    break;

  case 318:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false );
		}
#line 11633 "Parser/parser.cc"
    break;

  case 319:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11642 "Parser/parser.cc"
    break;

  case 320:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11648 "Parser/parser.cc"
    break;

  case 321:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11654 "Parser/parser.cc"
    break;

  case 322:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11660 "Parser/parser.cc"
    break;

  case 323:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11666 "Parser/parser.cc"
    break;

  case 324:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11672 "Parser/parser.cc"
    break;

  case 325:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11678 "Parser/parser.cc"
    break;

  case 326:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11684 "Parser/parser.cc"
    break;

  case 328:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11690 "Parser/parser.cc"
    break;

  case 329:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11696 "Parser/parser.cc"
    break;

  case 330:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 11702 "Parser/parser.cc"
    break;

  case 331:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 11708 "Parser/parser.cc"
    break;

  case 332:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 11714 "Parser/parser.cc"
    break;

  case 333:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 11720 "Parser/parser.cc"
    break;

  case 334:
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 11726 "Parser/parser.cc"
    break;

  case 335:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 11732 "Parser/parser.cc"
    break;

  case 336:
#line 1695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 11738 "Parser/parser.cc"
    break;

  case 337:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 11744 "Parser/parser.cc"
    break;

  case 338:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 11750 "Parser/parser.cc"
    break;

  case 339:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 11756 "Parser/parser.cc"
    break;

  case 340:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 11762 "Parser/parser.cc"
    break;

  case 341:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 11768 "Parser/parser.cc"
    break;

  case 342:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 11774 "Parser/parser.cc"
    break;

  case 343:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 11780 "Parser/parser.cc"
    break;

  case 344:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 11786 "Parser/parser.cc"
    break;

  case 345:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 11792 "Parser/parser.cc"
    break;

  case 346:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 11798 "Parser/parser.cc"
    break;

  case 347:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 11804 "Parser/parser.cc"
    break;

  case 348:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 11810 "Parser/parser.cc"
    break;

  case 349:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 11816 "Parser/parser.cc"
    break;

  case 352:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11822 "Parser/parser.cc"
    break;

  case 353:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 11831 "Parser/parser.cc"
    break;

  case 354:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11837 "Parser/parser.cc"
    break;

  case 355:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11843 "Parser/parser.cc"
    break;

  case 358:
#line 1759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11849 "Parser/parser.cc"
    break;

  case 359:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11855 "Parser/parser.cc"
    break;

  case 362:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11861 "Parser/parser.cc"
    break;

  case 363:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 11867 "Parser/parser.cc"
    break;

  case 364:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11873 "Parser/parser.cc"
    break;

  case 365:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11879 "Parser/parser.cc"
    break;

  case 366:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11885 "Parser/parser.cc"
    break;

  case 367:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11891 "Parser/parser.cc"
    break;

  case 368:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 11897 "Parser/parser.cc"
    break;

  case 369:
#line 1791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11903 "Parser/parser.cc"
    break;

  case 370:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 11909 "Parser/parser.cc"
    break;

  case 373:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11915 "Parser/parser.cc"
    break;

  case 374:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11921 "Parser/parser.cc"
    break;

  case 375:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 11927 "Parser/parser.cc"
    break;

  case 376:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11933 "Parser/parser.cc"
    break;

  case 377:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11939 "Parser/parser.cc"
    break;

  case 378:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11945 "Parser/parser.cc"
    break;

  case 379:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11951 "Parser/parser.cc"
    break;

  case 380:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11957 "Parser/parser.cc"
    break;

  case 381:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 11963 "Parser/parser.cc"
    break;

  case 382:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 11969 "Parser/parser.cc"
    break;

  case 383:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11975 "Parser/parser.cc"
    break;

  case 384:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 11981 "Parser/parser.cc"
    break;

  case 385:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 11987 "Parser/parser.cc"
    break;

  case 386:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 11993 "Parser/parser.cc"
    break;

  case 387:
#line 1858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11999 "Parser/parser.cc"
    break;

  case 388:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-6].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 12005 "Parser/parser.cc"
    break;

  case 389:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12011 "Parser/parser.cc"
    break;

  case 390:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 12017 "Parser/parser.cc"
    break;

  case 391:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 12023 "Parser/parser.cc"
    break;

  case 392:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 12029 "Parser/parser.cc"
    break;

  case 393:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 12035 "Parser/parser.cc"
    break;

  case 394:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 12041 "Parser/parser.cc"
    break;

  case 395:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 12047 "Parser/parser.cc"
    break;

  case 397:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12053 "Parser/parser.cc"
    break;

  case 398:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12059 "Parser/parser.cc"
    break;

  case 399:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 12065 "Parser/parser.cc"
    break;

  case 404:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 12071 "Parser/parser.cc"
    break;

  case 405:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12077 "Parser/parser.cc"
    break;

  case 406:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12083 "Parser/parser.cc"
    break;

  case 407:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12089 "Parser/parser.cc"
    break;

  case 408:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 12095 "Parser/parser.cc"
    break;

  case 409:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 12101 "Parser/parser.cc"
    break;

  case 410:
#line 1918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 12107 "Parser/parser.cc"
    break;

  case 411:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12113 "Parser/parser.cc"
    break;

  case 414:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12119 "Parser/parser.cc"
    break;

  case 415:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 12125 "Parser/parser.cc"
    break;

  case 416:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 12134 "Parser/parser.cc"
    break;

  case 417:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12140 "Parser/parser.cc"
    break;

  case 418:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 12146 "Parser/parser.cc"
    break;

  case 419:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12152 "Parser/parser.cc"
    break;

  case 420:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 12161 "Parser/parser.cc"
    break;

  case 421:
#line 1959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 12170 "Parser/parser.cc"
    break;

  case 422:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12176 "Parser/parser.cc"
    break;

  case 425:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12182 "Parser/parser.cc"
    break;

  case 426:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12188 "Parser/parser.cc"
    break;

  case 428:
#line 1987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12194 "Parser/parser.cc"
    break;

  case 429:
#line 1989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 12200 "Parser/parser.cc"
    break;

  case 439:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 12206 "Parser/parser.cc"
    break;

  case 440:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 12212 "Parser/parser.cc"
    break;

  case 444:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12218 "Parser/parser.cc"
    break;

  case 446:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 12224 "Parser/parser.cc"
    break;

  case 447:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12230 "Parser/parser.cc"
    break;

  case 448:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12236 "Parser/parser.cc"
    break;

  case 449:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12242 "Parser/parser.cc"
    break;

  case 450:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12248 "Parser/parser.cc"
    break;

  case 451:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12254 "Parser/parser.cc"
    break;

  case 452:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12260 "Parser/parser.cc"
    break;

  case 453:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12266 "Parser/parser.cc"
    break;

  case 454:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12272 "Parser/parser.cc"
    break;

  case 456:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12278 "Parser/parser.cc"
    break;

  case 457:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12284 "Parser/parser.cc"
    break;

  case 458:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12290 "Parser/parser.cc"
    break;

  case 459:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 12301 "Parser/parser.cc"
    break;

  case 460:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12307 "Parser/parser.cc"
    break;

  case 461:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12313 "Parser/parser.cc"
    break;

  case 462:
#line 2107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12319 "Parser/parser.cc"
    break;

  case 463:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12325 "Parser/parser.cc"
    break;

  case 464:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 12331 "Parser/parser.cc"
    break;

  case 465:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) ); }
#line 12337 "Parser/parser.cc"
    break;

  case 466:
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 12346 "Parser/parser.cc"
    break;

  case 467:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 12355 "Parser/parser.cc"
    break;

  case 468:
#line 2132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 12364 "Parser/parser.cc"
    break;

  case 469:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 12375 "Parser/parser.cc"
    break;

  case 470:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 12384 "Parser/parser.cc"
    break;

  case 471:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12390 "Parser/parser.cc"
    break;

  case 472:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12396 "Parser/parser.cc"
    break;

  case 473:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12402 "Parser/parser.cc"
    break;

  case 474:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 12410 "Parser/parser.cc"
    break;

  case 475:
#line 2169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 12418 "Parser/parser.cc"
    break;

  case 476:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 12424 "Parser/parser.cc"
    break;

  case 479:
#line 2180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12439 "Parser/parser.cc"
    break;

  case 480:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12445 "Parser/parser.cc"
    break;

  case 481:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12451 "Parser/parser.cc"
    break;

  case 482:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 12457 "Parser/parser.cc"
    break;

  case 483:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 12463 "Parser/parser.cc"
    break;

  case 484:
#line 2206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12469 "Parser/parser.cc"
    break;

  case 490:
#line 2219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 12479 "Parser/parser.cc"
    break;

  case 503:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12485 "Parser/parser.cc"
    break;

  case 506:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12491 "Parser/parser.cc"
    break;

  case 507:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12497 "Parser/parser.cc"
    break;

  case 509:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 12503 "Parser/parser.cc"
    break;

  case 510:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 12509 "Parser/parser.cc"
    break;

  case 511:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 12515 "Parser/parser.cc"
    break;

  case 512:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 12521 "Parser/parser.cc"
    break;

  case 513:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 12527 "Parser/parser.cc"
    break;

  case 514:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12533 "Parser/parser.cc"
    break;

  case 516:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12539 "Parser/parser.cc"
    break;

  case 517:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12545 "Parser/parser.cc"
    break;

  case 519:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12551 "Parser/parser.cc"
    break;

  case 520:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 12557 "Parser/parser.cc"
    break;

  case 521:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 12563 "Parser/parser.cc"
    break;

  case 522:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 12569 "Parser/parser.cc"
    break;

  case 523:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 12575 "Parser/parser.cc"
    break;

  case 524:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 12581 "Parser/parser.cc"
    break;

  case 525:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 12587 "Parser/parser.cc"
    break;

  case 526:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 12593 "Parser/parser.cc"
    break;

  case 527:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 12599 "Parser/parser.cc"
    break;

  case 528:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 12605 "Parser/parser.cc"
    break;

  case 529:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12611 "Parser/parser.cc"
    break;

  case 530:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 12617 "Parser/parser.cc"
    break;

  case 531:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 12623 "Parser/parser.cc"
    break;

  case 532:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 12629 "Parser/parser.cc"
    break;

  case 533:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 12635 "Parser/parser.cc"
    break;

  case 534:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 12641 "Parser/parser.cc"
    break;

  case 535:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 12647 "Parser/parser.cc"
    break;

  case 536:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 12653 "Parser/parser.cc"
    break;

  case 537:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 12659 "Parser/parser.cc"
    break;

  case 538:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float80 ); }
#line 12665 "Parser/parser.cc"
    break;

  case 539:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 12671 "Parser/parser.cc"
    break;

  case 540:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float16 ); }
#line 12677 "Parser/parser.cc"
    break;

  case 541:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32 ); }
#line 12683 "Parser/parser.cc"
    break;

  case 542:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x ); }
#line 12689 "Parser/parser.cc"
    break;

  case 543:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64 ); }
#line 12695 "Parser/parser.cc"
    break;

  case 544:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x ); }
#line 12701 "Parser/parser.cc"
    break;

  case 545:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128 ); }
#line 12707 "Parser/parser.cc"
    break;

  case 546:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128x ); }
#line 12713 "Parser/parser.cc"
    break;

  case 547:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x4 ); }
#line 12719 "Parser/parser.cc"
    break;

  case 548:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x2 ); }
#line 12725 "Parser/parser.cc"
    break;

  case 549:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat32 ); }
#line 12731 "Parser/parser.cc"
    break;

  case 550:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat64 ); }
#line 12737 "Parser/parser.cc"
    break;

  case 551:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svbool ); }
#line 12743 "Parser/parser.cc"
    break;

  case 552:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12749 "Parser/parser.cc"
    break;

  case 553:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12755 "Parser/parser.cc"
    break;

  case 554:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12761 "Parser/parser.cc"
    break;

  case 555:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 12767 "Parser/parser.cc"
    break;

  case 556:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 12773 "Parser/parser.cc"
    break;

  case 557:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 12779 "Parser/parser.cc"
    break;

  case 558:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 12785 "Parser/parser.cc"
    break;

  case 559:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 12791 "Parser/parser.cc"
    break;

  case 560:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 12797 "Parser/parser.cc"
    break;

  case 561:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 12803 "Parser/parser.cc"
    break;

  case 562:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 12809 "Parser/parser.cc"
    break;

  case 564:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12815 "Parser/parser.cc"
    break;

  case 566:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 12821 "Parser/parser.cc"
    break;

  case 567:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12827 "Parser/parser.cc"
    break;

  case 568:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12833 "Parser/parser.cc"
    break;

  case 570:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12839 "Parser/parser.cc"
    break;

  case 571:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12845 "Parser/parser.cc"
    break;

  case 572:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12851 "Parser/parser.cc"
    break;

  case 573:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 12857 "Parser/parser.cc"
    break;

  case 575:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12863 "Parser/parser.cc"
    break;

  case 577:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12869 "Parser/parser.cc"
    break;

  case 578:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12875 "Parser/parser.cc"
    break;

  case 579:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 12881 "Parser/parser.cc"
    break;

  case 580:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12887 "Parser/parser.cc"
    break;

  case 581:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 12893 "Parser/parser.cc"
    break;

  case 582:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 12899 "Parser/parser.cc"
    break;

  case 583:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 12905 "Parser/parser.cc"
    break;

  case 584:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 12911 "Parser/parser.cc"
    break;

  case 585:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 12917 "Parser/parser.cc"
    break;

  case 587:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12923 "Parser/parser.cc"
    break;

  case 588:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12929 "Parser/parser.cc"
    break;

  case 589:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12935 "Parser/parser.cc"
    break;

  case 591:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 12941 "Parser/parser.cc"
    break;

  case 592:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12947 "Parser/parser.cc"
    break;

  case 593:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 12956 "Parser/parser.cc"
    break;

  case 595:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12962 "Parser/parser.cc"
    break;

  case 596:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12968 "Parser/parser.cc"
    break;

  case 597:
#line 2517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12974 "Parser/parser.cc"
    break;

  case 599:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12980 "Parser/parser.cc"
    break;

  case 600:
#line 2525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12986 "Parser/parser.cc"
    break;

  case 602:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12992 "Parser/parser.cc"
    break;

  case 603:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12998 "Parser/parser.cc"
    break;

  case 604:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13004 "Parser/parser.cc"
    break;

  case 605:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 13010 "Parser/parser.cc"
    break;

  case 606:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 13016 "Parser/parser.cc"
    break;

  case 607:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13022 "Parser/parser.cc"
    break;

  case 608:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 13028 "Parser/parser.cc"
    break;

  case 609:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 13034 "Parser/parser.cc"
    break;

  case 610:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 13040 "Parser/parser.cc"
    break;

  case 612:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 13046 "Parser/parser.cc"
    break;

  case 613:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 13052 "Parser/parser.cc"
    break;

  case 614:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 13058 "Parser/parser.cc"
    break;

  case 615:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 13064 "Parser/parser.cc"
    break;

  case 616:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13070 "Parser/parser.cc"
    break;

  case 621:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 13076 "Parser/parser.cc"
    break;

  case 622:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 13082 "Parser/parser.cc"
    break;

  case 623:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 13091 "Parser/parser.cc"
    break;

  case 624:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 13099 "Parser/parser.cc"
    break;

  case 625:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 13108 "Parser/parser.cc"
    break;

  case 626:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 13117 "Parser/parser.cc"
    break;

  case 627:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 13126 "Parser/parser.cc"
    break;

  case 628:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 13135 "Parser/parser.cc"
    break;

  case 630:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13141 "Parser/parser.cc"
    break;

  case 631:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13147 "Parser/parser.cc"
    break;

  case 632:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13157 "Parser/parser.cc"
    break;

  case 633:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 13176 "Parser/parser.cc"
    break;

  case 636:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 13182 "Parser/parser.cc"
    break;

  case 637:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 13188 "Parser/parser.cc"
    break;

  case 638:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 13194 "Parser/parser.cc"
    break;

  case 639:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 13200 "Parser/parser.cc"
    break;

  case 640:
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 13206 "Parser/parser.cc"
    break;

  case 641:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 13212 "Parser/parser.cc"
    break;

  case 642:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13221 "Parser/parser.cc"
    break;

  case 643:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 13227 "Parser/parser.cc"
    break;

  case 644:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13236 "Parser/parser.cc"
    break;

  case 645:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 13242 "Parser/parser.cc"
    break;

  case 646:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13251 "Parser/parser.cc"
    break;

  case 647:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13257 "Parser/parser.cc"
    break;

  case 648:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 13263 "Parser/parser.cc"
    break;

  case 649:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
		}
#line 13271 "Parser/parser.cc"
    break;

  case 650:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 13280 "Parser/parser.cc"
    break;

  case 651:
#line 2710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 13286 "Parser/parser.cc"
    break;

  case 652:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13292 "Parser/parser.cc"
    break;

  case 653:
#line 2714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 13305 "Parser/parser.cc"
    break;

  case 654:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13311 "Parser/parser.cc"
    break;

  case 657:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 13317 "Parser/parser.cc"
    break;

  case 658:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13323 "Parser/parser.cc"
    break;

  case 661:
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13329 "Parser/parser.cc"
    break;

  case 664:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13335 "Parser/parser.cc"
    break;

  case 665:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 13341 "Parser/parser.cc"
    break;

  case 666:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13347 "Parser/parser.cc"
    break;

  case 667:
#line 2754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13353 "Parser/parser.cc"
    break;

  case 668:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13359 "Parser/parser.cc"
    break;

  case 669:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13365 "Parser/parser.cc"
    break;

  case 671:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13371 "Parser/parser.cc"
    break;

  case 673:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 13377 "Parser/parser.cc"
    break;

  case 674:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13383 "Parser/parser.cc"
    break;

  case 676:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 13389 "Parser/parser.cc"
    break;

  case 677:
#line 2790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13395 "Parser/parser.cc"
    break;

  case 679:
#line 2796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13401 "Parser/parser.cc"
    break;

  case 680:
#line 2804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 13412 "Parser/parser.cc"
    break;

  case 681:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 13426 "Parser/parser.cc"
    break;

  case 682:
#line 2823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 13432 "Parser/parser.cc"
    break;

  case 683:
#line 2825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 13438 "Parser/parser.cc"
    break;

  case 684:
#line 2827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 13444 "Parser/parser.cc"
    break;

  case 685:
#line 2829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 13455 "Parser/parser.cc"
    break;

  case 686:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 13461 "Parser/parser.cc"
    break;

  case 687:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 13467 "Parser/parser.cc"
    break;

  case 689:
#line 2846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13473 "Parser/parser.cc"
    break;

  case 690:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13479 "Parser/parser.cc"
    break;

  case 691:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13485 "Parser/parser.cc"
    break;

  case 692:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 13491 "Parser/parser.cc"
    break;

  case 693:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13500 "Parser/parser.cc"
    break;

  case 694:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13509 "Parser/parser.cc"
    break;

  case 695:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 13515 "Parser/parser.cc"
    break;

  case 696:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 13521 "Parser/parser.cc"
    break;

  case 697:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 13531 "Parser/parser.cc"
    break;

  case 698:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 13537 "Parser/parser.cc"
    break;

  case 699:
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 13543 "Parser/parser.cc"
    break;

  case 701:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13549 "Parser/parser.cc"
    break;

  case 702:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13555 "Parser/parser.cc"
    break;

  case 703:
#line 2897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13561 "Parser/parser.cc"
    break;

  case 704:
#line 2898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13567 "Parser/parser.cc"
    break;

  case 705:
#line 2907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13573 "Parser/parser.cc"
    break;

  case 706:
#line 2909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13579 "Parser/parser.cc"
    break;

  case 708:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13585 "Parser/parser.cc"
    break;

  case 711:
#line 2919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13591 "Parser/parser.cc"
    break;

  case 712:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13597 "Parser/parser.cc"
    break;

  case 713:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13603 "Parser/parser.cc"
    break;

  case 714:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13609 "Parser/parser.cc"
    break;

  case 717:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13615 "Parser/parser.cc"
    break;

  case 718:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13621 "Parser/parser.cc"
    break;

  case 719:
#line 2936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13627 "Parser/parser.cc"
    break;

  case 721:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13633 "Parser/parser.cc"
    break;

  case 722:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13639 "Parser/parser.cc"
    break;

  case 723:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 13645 "Parser/parser.cc"
    break;

  case 725:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13651 "Parser/parser.cc"
    break;

  case 726:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13657 "Parser/parser.cc"
    break;

  case 727:
#line 2965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13663 "Parser/parser.cc"
    break;

  case 728:
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13669 "Parser/parser.cc"
    break;

  case 729:
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13675 "Parser/parser.cc"
    break;

  case 731:
#line 2978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13681 "Parser/parser.cc"
    break;

  case 732:
#line 2981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13687 "Parser/parser.cc"
    break;

  case 733:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13693 "Parser/parser.cc"
    break;

  case 738:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13699 "Parser/parser.cc"
    break;

  case 740:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13705 "Parser/parser.cc"
    break;

  case 741:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 13711 "Parser/parser.cc"
    break;

  case 744:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 13717 "Parser/parser.cc"
    break;

  case 747:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13723 "Parser/parser.cc"
    break;

  case 748:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 13729 "Parser/parser.cc"
    break;

  case 749:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 13735 "Parser/parser.cc"
    break;

  case 750:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13741 "Parser/parser.cc"
    break;

  case 751:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13747 "Parser/parser.cc"
    break;

  case 752:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13753 "Parser/parser.cc"
    break;

  case 753:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13759 "Parser/parser.cc"
    break;

  case 755:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 13765 "Parser/parser.cc"
    break;

  case 756:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 13771 "Parser/parser.cc"
    break;

  case 757:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 13777 "Parser/parser.cc"
    break;

  case 759:
#line 3055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 13783 "Parser/parser.cc"
    break;

  case 761:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 13789 "Parser/parser.cc"
    break;

  case 762:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 13795 "Parser/parser.cc"
    break;

  case 763:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13801 "Parser/parser.cc"
    break;

  case 764:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13807 "Parser/parser.cc"
    break;

  case 765:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 13813 "Parser/parser.cc"
    break;

  case 766:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13819 "Parser/parser.cc"
    break;

  case 768:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13825 "Parser/parser.cc"
    break;

  case 769:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13831 "Parser/parser.cc"
    break;

  case 770:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13837 "Parser/parser.cc"
    break;

  case 771:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 13848 "Parser/parser.cc"
    break;

  case 772:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13854 "Parser/parser.cc"
    break;

  case 773:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 13860 "Parser/parser.cc"
    break;

  case 774:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13866 "Parser/parser.cc"
    break;

  case 775:
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 13875 "Parser/parser.cc"
    break;

  case 776:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 13881 "Parser/parser.cc"
    break;

  case 777:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 13891 "Parser/parser.cc"
    break;

  case 778:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13897 "Parser/parser.cc"
    break;

  case 779:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13903 "Parser/parser.cc"
    break;

  case 780:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 13909 "Parser/parser.cc"
    break;

  case 781:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13915 "Parser/parser.cc"
    break;

  case 782:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13921 "Parser/parser.cc"
    break;

  case 783:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13927 "Parser/parser.cc"
    break;

  case 784:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 13933 "Parser/parser.cc"
    break;

  case 785:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13939 "Parser/parser.cc"
    break;

  case 786:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13945 "Parser/parser.cc"
    break;

  case 789:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13951 "Parser/parser.cc"
    break;

  case 790:
#line 3176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13957 "Parser/parser.cc"
    break;

  case 791:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13963 "Parser/parser.cc"
    break;

  case 792:
#line 3185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13969 "Parser/parser.cc"
    break;

  case 794:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13975 "Parser/parser.cc"
    break;

  case 795:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 13981 "Parser/parser.cc"
    break;

  case 796:
#line 3195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13987 "Parser/parser.cc"
    break;

  case 797:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13993 "Parser/parser.cc"
    break;

  case 798:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 13999 "Parser/parser.cc"
    break;

  case 799:
#line 3204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 14005 "Parser/parser.cc"
    break;

  case 800:
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 14011 "Parser/parser.cc"
    break;

  case 801:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 14020 "Parser/parser.cc"
    break;

  case 802:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 14029 "Parser/parser.cc"
    break;

  case 803:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 14038 "Parser/parser.cc"
    break;

  case 804:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 14044 "Parser/parser.cc"
    break;

  case 805:
#line 3231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-6].tok), (yyvsp[-4].decl), (yyvsp[-1].decl) );
		}
#line 14053 "Parser/parser.cc"
    break;

  case 806:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-3].tok), (yyvsp[-5].decl), (yyvsp[-1].decl) ); }
#line 14059 "Parser/parser.cc"
    break;

  case 808:
#line 3242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 14065 "Parser/parser.cc"
    break;

  case 813:
#line 3254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 14071 "Parser/parser.cc"
    break;

  case 814:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 14077 "Parser/parser.cc"
    break;

  case 815:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 14083 "Parser/parser.cc"
    break;

  case 816:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Possible cause is declaring an aggregate or enumeration type in a trait." ); (yyval.decl) = nullptr; }
#line 14089 "Parser/parser.cc"
    break;

  case 818:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 14095 "Parser/parser.cc"
    break;

  case 819:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14101 "Parser/parser.cc"
    break;

  case 820:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 14107 "Parser/parser.cc"
    break;

  case 821:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14113 "Parser/parser.cc"
    break;

  case 823:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 14119 "Parser/parser.cc"
    break;

  case 824:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 14125 "Parser/parser.cc"
    break;

  case 825:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 14131 "Parser/parser.cc"
    break;

  case 826:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 14147 "Parser/parser.cc"
    break;

  case 827:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 14153 "Parser/parser.cc"
    break;

  case 828:
#line 3314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 14159 "Parser/parser.cc"
    break;

  case 829:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 14165 "Parser/parser.cc"
    break;

  case 830:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14171 "Parser/parser.cc"
    break;

  case 831:
#line 3320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14177 "Parser/parser.cc"
    break;

  case 832:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14183 "Parser/parser.cc"
    break;

  case 834:
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 14192 "Parser/parser.cc"
    break;

  case 835:
#line 3330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 14198 "Parser/parser.cc"
    break;

  case 836:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 14207 "Parser/parser.cc"
    break;

  case 837:
#line 3337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 14217 "Parser/parser.cc"
    break;

  case 838:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 14226 "Parser/parser.cc"
    break;

  case 839:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14236 "Parser/parser.cc"
    break;

  case 840:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 14247 "Parser/parser.cc"
    break;

  case 841:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14257 "Parser/parser.cc"
    break;

  case 842:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 14268 "Parser/parser.cc"
    break;

  case 843:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14278 "Parser/parser.cc"
    break;

  case 844:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 14289 "Parser/parser.cc"
    break;

  case 845:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14299 "Parser/parser.cc"
    break;

  case 846:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14305 "Parser/parser.cc"
    break;

  case 848:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14311 "Parser/parser.cc"
    break;

  case 849:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14317 "Parser/parser.cc"
    break;

  case 850:
#line 3412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 14323 "Parser/parser.cc"
    break;

  case 851:
#line 3414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 14335 "Parser/parser.cc"
    break;

  case 852:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14346 "Parser/parser.cc"
    break;

  case 853:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14355 "Parser/parser.cc"
    break;

  case 854:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14364 "Parser/parser.cc"
    break;

  case 855:
#line 3443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14370 "Parser/parser.cc"
    break;

  case 856:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14376 "Parser/parser.cc"
    break;

  case 857:
#line 3449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14382 "Parser/parser.cc"
    break;

  case 858:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 14391 "Parser/parser.cc"
    break;

  case 859:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14397 "Parser/parser.cc"
    break;

  case 860:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14403 "Parser/parser.cc"
    break;

  case 861:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 14409 "Parser/parser.cc"
    break;

  case 866:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 14415 "Parser/parser.cc"
    break;

  case 867:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14421 "Parser/parser.cc"
    break;

  case 868:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 14431 "Parser/parser.cc"
    break;

  case 869:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14437 "Parser/parser.cc"
    break;

  case 872:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14443 "Parser/parser.cc"
    break;

  case 873:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 14449 "Parser/parser.cc"
    break;

  case 874:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14455 "Parser/parser.cc"
    break;

  case 875:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14461 "Parser/parser.cc"
    break;

  case 876:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14467 "Parser/parser.cc"
    break;

  case 878:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14473 "Parser/parser.cc"
    break;

  case 879:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14479 "Parser/parser.cc"
    break;

  case 880:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14485 "Parser/parser.cc"
    break;

  case 881:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 14491 "Parser/parser.cc"
    break;

  case 883:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 14497 "Parser/parser.cc"
    break;

  case 884:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 14503 "Parser/parser.cc"
    break;

  case 885:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14509 "Parser/parser.cc"
    break;

  case 886:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14515 "Parser/parser.cc"
    break;

  case 887:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14521 "Parser/parser.cc"
    break;

  case 888:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14527 "Parser/parser.cc"
    break;

  case 890:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14533 "Parser/parser.cc"
    break;

  case 891:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14539 "Parser/parser.cc"
    break;

  case 892:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14545 "Parser/parser.cc"
    break;

  case 893:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14551 "Parser/parser.cc"
    break;

  case 894:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14557 "Parser/parser.cc"
    break;

  case 895:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14563 "Parser/parser.cc"
    break;

  case 896:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14569 "Parser/parser.cc"
    break;

  case 897:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14575 "Parser/parser.cc"
    break;

  case 898:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14581 "Parser/parser.cc"
    break;

  case 899:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14587 "Parser/parser.cc"
    break;

  case 900:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14593 "Parser/parser.cc"
    break;

  case 901:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14599 "Parser/parser.cc"
    break;

  case 902:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14605 "Parser/parser.cc"
    break;

  case 903:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14611 "Parser/parser.cc"
    break;

  case 904:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14617 "Parser/parser.cc"
    break;

  case 905:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14623 "Parser/parser.cc"
    break;

  case 906:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14629 "Parser/parser.cc"
    break;

  case 907:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14635 "Parser/parser.cc"
    break;

  case 909:
#line 3639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14641 "Parser/parser.cc"
    break;

  case 910:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14647 "Parser/parser.cc"
    break;

  case 911:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14653 "Parser/parser.cc"
    break;

  case 912:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14659 "Parser/parser.cc"
    break;

  case 913:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14665 "Parser/parser.cc"
    break;

  case 914:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14671 "Parser/parser.cc"
    break;

  case 915:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14677 "Parser/parser.cc"
    break;

  case 916:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14683 "Parser/parser.cc"
    break;

  case 917:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14689 "Parser/parser.cc"
    break;

  case 918:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14695 "Parser/parser.cc"
    break;

  case 919:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14701 "Parser/parser.cc"
    break;

  case 920:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14707 "Parser/parser.cc"
    break;

  case 921:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14713 "Parser/parser.cc"
    break;

  case 922:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14719 "Parser/parser.cc"
    break;

  case 923:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14725 "Parser/parser.cc"
    break;

  case 924:
#line 3678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14731 "Parser/parser.cc"
    break;

  case 928:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 14737 "Parser/parser.cc"
    break;

  case 929:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14743 "Parser/parser.cc"
    break;

  case 930:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14749 "Parser/parser.cc"
    break;

  case 931:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14755 "Parser/parser.cc"
    break;

  case 932:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14761 "Parser/parser.cc"
    break;

  case 933:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14767 "Parser/parser.cc"
    break;

  case 934:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14773 "Parser/parser.cc"
    break;

  case 935:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14779 "Parser/parser.cc"
    break;

  case 936:
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14785 "Parser/parser.cc"
    break;

  case 937:
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14791 "Parser/parser.cc"
    break;

  case 938:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14797 "Parser/parser.cc"
    break;

  case 939:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14803 "Parser/parser.cc"
    break;

  case 940:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14809 "Parser/parser.cc"
    break;

  case 941:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14815 "Parser/parser.cc"
    break;

  case 942:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14821 "Parser/parser.cc"
    break;

  case 943:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 14830 "Parser/parser.cc"
    break;

  case 944:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14836 "Parser/parser.cc"
    break;

  case 945:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14842 "Parser/parser.cc"
    break;

  case 947:
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14848 "Parser/parser.cc"
    break;

  case 948:
#line 3757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14854 "Parser/parser.cc"
    break;

  case 949:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14860 "Parser/parser.cc"
    break;

  case 950:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14866 "Parser/parser.cc"
    break;

  case 951:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14872 "Parser/parser.cc"
    break;

  case 952:
#line 3768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14878 "Parser/parser.cc"
    break;

  case 953:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14884 "Parser/parser.cc"
    break;

  case 954:
#line 3775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14890 "Parser/parser.cc"
    break;

  case 955:
#line 3777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14896 "Parser/parser.cc"
    break;

  case 956:
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14902 "Parser/parser.cc"
    break;

  case 957:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14908 "Parser/parser.cc"
    break;

  case 958:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14914 "Parser/parser.cc"
    break;

  case 959:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14920 "Parser/parser.cc"
    break;

  case 960:
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14926 "Parser/parser.cc"
    break;

  case 961:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14932 "Parser/parser.cc"
    break;

  case 962:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14938 "Parser/parser.cc"
    break;

  case 963:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14944 "Parser/parser.cc"
    break;

  case 964:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14950 "Parser/parser.cc"
    break;

  case 966:
#line 3808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14956 "Parser/parser.cc"
    break;

  case 967:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14962 "Parser/parser.cc"
    break;

  case 968:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14968 "Parser/parser.cc"
    break;

  case 969:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14974 "Parser/parser.cc"
    break;

  case 970:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14980 "Parser/parser.cc"
    break;

  case 971:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14986 "Parser/parser.cc"
    break;

  case 972:
#line 3826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14992 "Parser/parser.cc"
    break;

  case 973:
#line 3828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14998 "Parser/parser.cc"
    break;

  case 974:
#line 3830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15004 "Parser/parser.cc"
    break;

  case 975:
#line 3832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15010 "Parser/parser.cc"
    break;

  case 976:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15016 "Parser/parser.cc"
    break;

  case 977:
#line 3839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15022 "Parser/parser.cc"
    break;

  case 978:
#line 3841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15028 "Parser/parser.cc"
    break;

  case 979:
#line 3843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15034 "Parser/parser.cc"
    break;

  case 980:
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15040 "Parser/parser.cc"
    break;

  case 981:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15046 "Parser/parser.cc"
    break;

  case 982:
#line 3857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15052 "Parser/parser.cc"
    break;

  case 983:
#line 3859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15059 "Parser/parser.cc"
    break;

  case 985:
#line 3863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15065 "Parser/parser.cc"
    break;

  case 986:
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15071 "Parser/parser.cc"
    break;

  case 987:
#line 3870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15077 "Parser/parser.cc"
    break;

  case 988:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15083 "Parser/parser.cc"
    break;

  case 989:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15089 "Parser/parser.cc"
    break;

  case 990:
#line 3879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15095 "Parser/parser.cc"
    break;

  case 991:
#line 3881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15101 "Parser/parser.cc"
    break;

  case 992:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15107 "Parser/parser.cc"
    break;

  case 993:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15113 "Parser/parser.cc"
    break;

  case 994:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15119 "Parser/parser.cc"
    break;

  case 995:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15125 "Parser/parser.cc"
    break;

  case 996:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15131 "Parser/parser.cc"
    break;

  case 997:
#line 3908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15137 "Parser/parser.cc"
    break;

  case 998:
#line 3910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15144 "Parser/parser.cc"
    break;

  case 1000:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15150 "Parser/parser.cc"
    break;

  case 1001:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15156 "Parser/parser.cc"
    break;

  case 1002:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 15162 "Parser/parser.cc"
    break;

  case 1003:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 15168 "Parser/parser.cc"
    break;

  case 1004:
#line 3928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15174 "Parser/parser.cc"
    break;

  case 1005:
#line 3930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15180 "Parser/parser.cc"
    break;

  case 1006:
#line 3932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15186 "Parser/parser.cc"
    break;

  case 1007:
#line 3937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15192 "Parser/parser.cc"
    break;

  case 1008:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15198 "Parser/parser.cc"
    break;

  case 1009:
#line 3944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15204 "Parser/parser.cc"
    break;

  case 1010:
#line 3946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15210 "Parser/parser.cc"
    break;

  case 1012:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15216 "Parser/parser.cc"
    break;

  case 1013:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15222 "Parser/parser.cc"
    break;

  case 1014:
#line 3971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 15228 "Parser/parser.cc"
    break;

  case 1015:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15234 "Parser/parser.cc"
    break;

  case 1016:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15240 "Parser/parser.cc"
    break;

  case 1017:
#line 3977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15246 "Parser/parser.cc"
    break;

  case 1018:
#line 3979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15252 "Parser/parser.cc"
    break;

  case 1020:
#line 3985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15258 "Parser/parser.cc"
    break;

  case 1021:
#line 3987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15264 "Parser/parser.cc"
    break;

  case 1022:
#line 3989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15270 "Parser/parser.cc"
    break;

  case 1023:
#line 3994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15276 "Parser/parser.cc"
    break;

  case 1024:
#line 3996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15282 "Parser/parser.cc"
    break;

  case 1025:
#line 3998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15288 "Parser/parser.cc"
    break;

  case 1026:
#line 4004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15294 "Parser/parser.cc"
    break;

  case 1027:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 15300 "Parser/parser.cc"
    break;

  case 1028:
#line 4009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-3].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 15306 "Parser/parser.cc"
    break;

  case 1029:
#line 4016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15312 "Parser/parser.cc"
    break;

  case 1031:
#line 4027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 15318 "Parser/parser.cc"
    break;

  case 1032:
#line 4029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 15324 "Parser/parser.cc"
    break;

  case 1034:
#line 4032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 15330 "Parser/parser.cc"
    break;

  case 1035:
#line 4034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 15336 "Parser/parser.cc"
    break;

  case 1037:
#line 4040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 15342 "Parser/parser.cc"
    break;

  case 1038:
#line 4042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 15348 "Parser/parser.cc"
    break;

  case 1039:
#line 4047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15354 "Parser/parser.cc"
    break;

  case 1040:
#line 4049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 15360 "Parser/parser.cc"
    break;

  case 1041:
#line 4051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 15366 "Parser/parser.cc"
    break;

  case 1042:
#line 4053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 15372 "Parser/parser.cc"
    break;

  case 1043:
#line 4087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 15378 "Parser/parser.cc"
    break;

  case 1046:
#line 4094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 15385 "Parser/parser.cc"
    break;

  case 1047:
#line 4097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15391 "Parser/parser.cc"
    break;

  case 1048:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15397 "Parser/parser.cc"
    break;

  case 1049:
#line 4104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 15403 "Parser/parser.cc"
    break;

  case 1050:
#line 4106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15409 "Parser/parser.cc"
    break;

  case 1051:
#line 4108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15415 "Parser/parser.cc"
    break;

  case 1052:
#line 4110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15421 "Parser/parser.cc"
    break;

  case 1053:
#line 4112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15427 "Parser/parser.cc"
    break;

  case 1055:
#line 4118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15433 "Parser/parser.cc"
    break;

  case 1056:
#line 4120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15439 "Parser/parser.cc"
    break;

  case 1057:
#line 4122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15445 "Parser/parser.cc"
    break;

  case 1058:
#line 4127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15451 "Parser/parser.cc"
    break;

  case 1059:
#line 4129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15457 "Parser/parser.cc"
    break;

  case 1060:
#line 4131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15463 "Parser/parser.cc"
    break;

  case 1062:
#line 4138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15469 "Parser/parser.cc"
    break;

  case 1064:
#line 4149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15475 "Parser/parser.cc"
    break;

  case 1065:
#line 4152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 15481 "Parser/parser.cc"
    break;

  case 1066:
#line 4154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 15487 "Parser/parser.cc"
    break;

  case 1067:
#line 4157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 15493 "Parser/parser.cc"
    break;

  case 1068:
#line 4159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 15499 "Parser/parser.cc"
    break;

  case 1069:
#line 4161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 15505 "Parser/parser.cc"
    break;

  case 1071:
#line 4176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15511 "Parser/parser.cc"
    break;

  case 1072:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15517 "Parser/parser.cc"
    break;

  case 1073:
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 15523 "Parser/parser.cc"
    break;

  case 1074:
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15529 "Parser/parser.cc"
    break;

  case 1075:
#line 4187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15535 "Parser/parser.cc"
    break;

  case 1076:
#line 4189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15541 "Parser/parser.cc"
    break;

  case 1077:
#line 4191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15547 "Parser/parser.cc"
    break;

  case 1079:
#line 4197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15553 "Parser/parser.cc"
    break;

  case 1080:
#line 4199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15559 "Parser/parser.cc"
    break;

  case 1081:
#line 4201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15565 "Parser/parser.cc"
    break;

  case 1082:
#line 4206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15571 "Parser/parser.cc"
    break;

  case 1083:
#line 4208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15577 "Parser/parser.cc"
    break;

  case 1086:
#line 4218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15583 "Parser/parser.cc"
    break;

  case 1089:
#line 4225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15589 "Parser/parser.cc"
    break;

  case 1090:
#line 4231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15595 "Parser/parser.cc"
    break;

  case 1091:
#line 4233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15601 "Parser/parser.cc"
    break;

  case 1092:
#line 4235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15607 "Parser/parser.cc"
    break;

  case 1093:
#line 4237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15613 "Parser/parser.cc"
    break;

  case 1094:
#line 4239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15619 "Parser/parser.cc"
    break;

  case 1095:
#line 4241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15625 "Parser/parser.cc"
    break;

  case 1096:
#line 4248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15631 "Parser/parser.cc"
    break;

  case 1097:
#line 4250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15637 "Parser/parser.cc"
    break;

  case 1098:
#line 4252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15643 "Parser/parser.cc"
    break;

  case 1099:
#line 4254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15649 "Parser/parser.cc"
    break;

  case 1100:
#line 4256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15655 "Parser/parser.cc"
    break;

  case 1101:
#line 4258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15661 "Parser/parser.cc"
    break;

  case 1102:
#line 4260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15667 "Parser/parser.cc"
    break;

  case 1103:
#line 4262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15673 "Parser/parser.cc"
    break;

  case 1104:
#line 4264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15679 "Parser/parser.cc"
    break;

  case 1105:
#line 4266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15685 "Parser/parser.cc"
    break;

  case 1106:
#line 4269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15691 "Parser/parser.cc"
    break;

  case 1107:
#line 4271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15697 "Parser/parser.cc"
    break;

  case 1108:
#line 4273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15703 "Parser/parser.cc"
    break;

  case 1109:
#line 4275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15709 "Parser/parser.cc"
    break;

  case 1110:
#line 4277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15715 "Parser/parser.cc"
    break;

  case 1111:
#line 4282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-2].decl) ); }
#line 15721 "Parser/parser.cc"
    break;

  case 1112:
#line 4284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), false ); }
#line 15727 "Parser/parser.cc"
    break;

  case 1113:
#line 4289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), true ); }
#line 15733 "Parser/parser.cc"
    break;

  case 1114:
#line 4291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) ), true ); }
#line 15739 "Parser/parser.cc"
    break;

  case 1116:
#line 4318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15745 "Parser/parser.cc"
    break;

  case 1120:
#line 4329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15751 "Parser/parser.cc"
    break;

  case 1121:
#line 4331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15757 "Parser/parser.cc"
    break;

  case 1122:
#line 4333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15763 "Parser/parser.cc"
    break;

  case 1123:
#line 4335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15769 "Parser/parser.cc"
    break;

  case 1124:
#line 4337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15775 "Parser/parser.cc"
    break;

  case 1125:
#line 4339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15781 "Parser/parser.cc"
    break;

  case 1126:
#line 4346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15787 "Parser/parser.cc"
    break;

  case 1127:
#line 4348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15793 "Parser/parser.cc"
    break;

  case 1128:
#line 4350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15799 "Parser/parser.cc"
    break;

  case 1129:
#line 4352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15805 "Parser/parser.cc"
    break;

  case 1130:
#line 4354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15811 "Parser/parser.cc"
    break;

  case 1131:
#line 4356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15817 "Parser/parser.cc"
    break;

  case 1132:
#line 4361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 15823 "Parser/parser.cc"
    break;

  case 1133:
#line 4363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15829 "Parser/parser.cc"
    break;

  case 1134:
#line 4365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15835 "Parser/parser.cc"
    break;

  case 1135:
#line 4370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 15841 "Parser/parser.cc"
    break;

  case 1136:
#line 4372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15847 "Parser/parser.cc"
    break;

  case 1137:
#line 4374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15853 "Parser/parser.cc"
    break;

  case 1140:
#line 4398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 15859 "Parser/parser.cc"
    break;

  case 1141:
#line 4400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 15865 "Parser/parser.cc"
    break;


#line 15869 "Parser/parser.cc"

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
#line 4403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
