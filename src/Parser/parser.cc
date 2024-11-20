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
#line 317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 758 "Parser/parser.cc"

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
#define YYLAST   33354

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  190
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1140
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
       0,   641,   641,   645,   652,   653,   654,   655,   656,   660,
     661,   662,   663,   664,   665,   666,   667,   671,   672,   676,
     677,   682,   683,   684,   688,   692,   693,   704,   706,   708,
     710,   711,   713,   715,   717,   719,   729,   731,   733,   735,
     737,   739,   744,   745,   756,   761,   766,   767,   772,   778,
     780,   782,   788,   790,   792,   794,   796,   816,   819,   821,
     823,   825,   827,   829,   831,   833,   835,   837,   839,   841,
     850,   851,   855,   856,   858,   860,   862,   864,   866,   871,
     873,   875,   883,   884,   892,   895,   896,   898,   903,   919,
     921,   923,   925,   927,   929,   931,   936,   938,   941,   943,
     948,   950,   955,   956,   958,   962,   963,   964,   965,   969,
     970,   972,   974,   976,   978,   980,   982,   984,   991,   992,
     993,   994,   998,   999,  1003,  1004,  1009,  1010,  1012,  1014,
    1019,  1020,  1022,  1027,  1028,  1030,  1035,  1036,  1038,  1040,
    1042,  1047,  1048,  1050,  1055,  1056,  1061,  1062,  1067,  1068,
    1073,  1074,  1079,  1080,  1085,  1086,  1088,  1093,  1098,  1099,
    1103,  1105,  1110,  1113,  1116,  1121,  1122,  1130,  1136,  1137,
    1141,  1142,  1146,  1147,  1151,  1152,  1153,  1154,  1155,  1156,
    1157,  1158,  1159,  1160,  1161,  1167,  1170,  1172,  1174,  1176,
    1181,  1182,  1184,  1186,  1191,  1192,  1198,  1199,  1205,  1206,
    1207,  1208,  1209,  1210,  1211,  1212,  1213,  1214,  1215,  1216,
    1217,  1218,  1220,  1221,  1229,  1231,  1241,  1243,  1251,  1252,
    1257,  1259,  1261,  1263,  1265,  1269,  1270,  1272,  1278,  1307,
    1310,  1312,  1314,  1324,  1326,  1328,  1333,  1338,  1340,  1342,
    1344,  1352,  1353,  1355,  1359,  1361,  1365,  1367,  1368,  1370,
    1372,  1377,  1378,  1382,  1387,  1388,  1392,  1394,  1399,  1401,
    1406,  1408,  1410,  1412,  1417,  1419,  1421,  1423,  1428,  1430,
    1435,  1436,  1458,  1460,  1464,  1467,  1469,  1472,  1474,  1477,
    1479,  1484,  1489,  1491,  1496,  1501,  1503,  1505,  1507,  1509,
    1514,  1516,  1519,  1521,  1526,  1532,  1535,  1537,  1542,  1548,
    1550,  1555,  1561,  1565,  1567,  1570,  1572,  1577,  1584,  1586,
    1591,  1597,  1599,  1604,  1610,  1613,  1617,  1628,  1633,  1638,
    1649,  1651,  1653,  1655,  1660,  1662,  1664,  1669,  1670,  1672,
    1677,  1679,  1684,  1686,  1688,  1690,  1693,  1697,  1700,  1704,
    1706,  1708,  1710,  1712,  1714,  1716,  1718,  1720,  1722,  1724,
    1729,  1730,  1734,  1740,  1748,  1753,  1754,  1758,  1759,  1764,
    1768,  1769,  1772,  1774,  1779,  1782,  1784,  1786,  1789,  1791,
    1796,  1801,  1802,  1806,  1811,  1813,  1818,  1820,  1825,  1827,
    1829,  1834,  1839,  1844,  1849,  1851,  1853,  1858,  1860,  1866,
    1867,  1871,  1872,  1873,  1874,  1878,  1883,  1884,  1886,  1888,
    1890,  1894,  1898,  1899,  1903,  1905,  1907,  1909,  1911,  1917,
    1918,  1924,  1925,  1929,  1930,  1935,  1937,  1946,  1947,  1949,
    1954,  1959,  1970,  1971,  1975,  1976,  1982,  1983,  1987,  1989,
    1993,  1995,  1999,  2000,  2004,  2005,  2009,  2010,  2011,  2015,
    2017,  2032,  2033,  2034,  2035,  2037,  2041,  2043,  2047,  2054,
    2056,  2058,  2060,  2068,  2070,  2075,  2076,  2078,  2080,  2082,
    2092,  2094,  2106,  2109,  2114,  2116,  2122,  2127,  2132,  2143,
    2150,  2155,  2157,  2159,  2165,  2169,  2176,  2178,  2179,  2180,
    2196,  2198,  2201,  2203,  2206,  2211,  2212,  2216,  2217,  2218,
    2219,  2228,  2229,  2230,  2239,  2240,  2241,  2245,  2246,  2247,
    2256,  2257,  2258,  2263,  2264,  2273,  2274,  2279,  2281,  2285,
    2287,  2289,  2291,  2298,  2303,  2308,  2309,  2311,  2321,  2322,
    2327,  2329,  2331,  2333,  2335,  2337,  2340,  2342,  2344,  2349,
    2355,  2357,  2359,  2361,  2363,  2365,  2367,  2369,  2371,  2373,
    2375,  2377,  2379,  2381,  2383,  2385,  2388,  2390,  2392,  2394,
    2396,  2398,  2400,  2402,  2404,  2406,  2408,  2410,  2412,  2414,
    2416,  2418,  2420,  2422,  2427,  2428,  2432,  2438,  2439,  2445,
    2446,  2448,  2450,  2452,  2457,  2459,  2464,  2465,  2467,  2469,
    2474,  2476,  2478,  2480,  2482,  2484,  2489,  2490,  2492,  2494,
    2499,  2501,  2500,  2504,  2512,  2513,  2515,  2517,  2522,  2523,
    2525,  2530,  2531,  2533,  2535,  2540,  2542,  2544,  2549,  2551,
    2553,  2555,  2556,  2558,  2563,  2565,  2567,  2572,  2573,  2577,
    2578,  2585,  2584,  2589,  2588,  2598,  2597,  2608,  2607,  2617,
    2622,  2623,  2628,  2634,  2652,  2653,  2657,  2659,  2661,  2666,
    2668,  2670,  2672,  2677,  2679,  2684,  2686,  2695,  2696,  2701,
    2710,  2715,  2717,  2719,  2728,  2730,  2731,  2732,  2734,  2736,
    2737,  2742,  2743,  2744,  2749,  2751,  2754,  2757,  2764,  2765,
    2766,  2772,  2777,  2779,  2785,  2786,  2792,  2793,  2797,  2805,
    2812,  2825,  2824,  2828,  2831,  2830,  2839,  2843,  2847,  2849,
    2855,  2856,  2861,  2866,  2875,  2876,  2878,  2884,  2886,  2891,
    2892,  2898,  2899,  2900,  2909,  2910,  2912,  2913,  2918,  2919,
    2920,  2922,  2928,  2929,  2931,  2932,  2933,  2935,  2937,  2944,
    2945,  2947,  2949,  2954,  2955,  2964,  2966,  2971,  2973,  2978,
    2979,  2981,  2984,  2986,  2990,  2991,  2992,  2994,  2996,  3004,
    3006,  3011,  3012,  3013,  3018,  3019,  3024,  3025,  3026,  3027,
    3031,  3032,  3037,  3038,  3039,  3040,  3041,  3055,  3056,  3061,
    3062,  3067,  3069,  3071,  3073,  3075,  3098,  3099,  3105,  3106,
    3112,  3111,  3121,  3120,  3124,  3130,  3132,  3142,  3143,  3145,
    3149,  3154,  3156,  3158,  3160,  3166,  3167,  3171,  3172,  3177,
    3179,  3186,  3188,  3189,  3191,  3196,  3198,  3200,  3205,  3207,
    3212,  3217,  3225,  3230,  3232,  3237,  3242,  3243,  3248,  3249,
    3253,  3254,  3255,  3261,  3263,  3265,  3271,  3273,  3278,  3280,
    3286,  3287,  3291,  3295,  3299,  3301,  3313,  3315,  3317,  3319,
    3321,  3323,  3325,  3326,  3331,  3334,  3333,  3345,  3344,  3357,
    3356,  3370,  3369,  3383,  3382,  3395,  3400,  3406,  3408,  3414,
    3415,  3426,  3433,  3438,  3444,  3447,  3450,  3454,  3460,  3463,
    3466,  3471,  3472,  3473,  3474,  3478,  3486,  3487,  3499,  3500,
    3504,  3505,  3510,  3512,  3514,  3516,  3521,  3522,  3528,  3529,
    3531,  3536,  3537,  3539,  3574,  3576,  3579,  3584,  3586,  3587,
    3589,  3594,  3596,  3598,  3600,  3605,  3607,  3609,  3611,  3613,
    3615,  3617,  3622,  3624,  3626,  3628,  3637,  3639,  3640,  3645,
    3647,  3649,  3651,  3653,  3658,  3660,  3662,  3664,  3669,  3671,
    3673,  3675,  3677,  3679,  3691,  3692,  3693,  3697,  3699,  3701,
    3703,  3705,  3710,  3712,  3714,  3716,  3721,  3723,  3725,  3727,
    3729,  3731,  3743,  3748,  3753,  3755,  3756,  3758,  3763,  3765,
    3767,  3769,  3774,  3776,  3778,  3780,  3782,  3784,  3786,  3791,
    3793,  3795,  3797,  3806,  3808,  3809,  3814,  3816,  3818,  3820,
    3822,  3827,  3829,  3831,  3833,  3838,  3840,  3842,  3844,  3846,
    3848,  3858,  3860,  3863,  3864,  3866,  3871,  3873,  3875,  3880,
    3882,  3884,  3886,  3891,  3893,  3895,  3909,  3911,  3914,  3915,
    3917,  3922,  3924,  3929,  3931,  3933,  3938,  3940,  3945,  3947,
    3964,  3965,  3967,  3972,  3974,  3976,  3978,  3980,  3985,  3986,
    3988,  3990,  3995,  3997,  3999,  4005,  4007,  4010,  4017,  4019,
    4028,  4030,  4032,  4033,  4035,  4037,  4041,  4043,  4048,  4050,
    4052,  4054,  4089,  4090,  4094,  4095,  4098,  4100,  4105,  4107,
    4109,  4111,  4113,  4118,  4119,  4121,  4123,  4128,  4130,  4132,
    4138,  4139,  4141,  4150,  4153,  4155,  4158,  4160,  4162,  4176,
    4177,  4179,  4184,  4186,  4188,  4190,  4192,  4197,  4198,  4200,
    4202,  4207,  4209,  4217,  4218,  4219,  4224,  4225,  4226,  4232,
    4234,  4236,  4238,  4240,  4242,  4249,  4251,  4253,  4255,  4257,
    4259,  4261,  4263,  4265,  4267,  4270,  4272,  4274,  4276,  4278,
    4283,  4285,  4287,  4292,  4318,  4319,  4321,  4325,  4326,  4330,
    4332,  4334,  4336,  4338,  4340,  4347,  4349,  4351,  4353,  4355,
    4357,  4362,  4364,  4366,  4371,  4373,  4375,  4393,  4395,  4400,
    4401
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
     415,   416,   417,   125,    40,    64,    41,    46,    91,    93,
      44,    58,   123,    96,    94,    42,    38,    43,    45,    33,
     126,    92,    47,    37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1862)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1139)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const int yypact[] =
{
     129, 12784,   250,   336, 24748,   -62, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,   226,   834,
     248, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862,   500,   276, -1862, -1862, -1862, -1862,
   -1862, -1862,  5867,  5867,   298, 12784,   376,   394, 29939, -1862,
     414, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862,   420,  3081, -1862,   749, 15310, -1862, -1862,  2452,
   -1862, -1862, -1862, -1862, 18480, -1862,   328,   449,   198,   458,
      83, -1862,  4959,   450,   480,   504,   494,  3738,   683,   899,
   12971, -1862, -1862,   616, 18314,   978, -1862, -1862, -1862, -1862,
    2595,   710,  8416, 11189,  1187,  2595,  1257,   566, -1862, -1862,
   -1862, -1862,    69, -1862, -1862, -1862, -1862,   577, -1862, -1862,
   -1862, -1862, -1862,   572,   599,    69, -1862,    69, 22691, -1862,
   -1862, -1862, 26197,  5867, -1862, -1862,  5867, -1862, 12784, -1862,
     556, 26361, -1862, -1862,  5439, 28641, -1862, -1862,  1339,  1339,
     620,  3992, -1862, -1862, -1862, -1862,   422, 20862,    69,  3863,
      69, -1862, -1862, -1862, -1862, -1862, -1862,   675, -1862,   666,
     708,  2247, -1862,   677, 32706, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, 23187,  2471,  2959,  3081,   218,   723,   730,   747,
     760,   792,   800, -1862, -1862, 32784,   751, 32862,   804,   811,
      69, 32706, 32940,   840, 30065, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, 33018, 33018, 23019, 14248, 24912,  5034,   838, -1862,
   -1862, -1862, -1862,   285, -1862,   640,   905, -1862,  2067,  2625,
   23691, 32706, -1862,   845,   651,   729,   802,    -3,   837,   859,
     869,   885,   940,   202, -1862,   934, -1862, -1862,  5376,  4787,
     977, 21554, 29587,  2595,  2595,   994,  2595,  1853,  2595,  1908,
     958, -1862, -1862,    69, -1862,   894,   944, -1862, -1862, -1862,
   -1862, 26525,  5867, -1862, -1862, 26689,  5690, -1862, -1862, 15487,
    1001, -1862, 18812, -1862, -1862, 26853, -1862, -1862,  1011, -1862,
   -1862, -1862,   960, -1862, 30361,  1119, 30673, -1862,  1032,  5867,
     599,  1063,  1077, -1862,  2452,  5439,  2452, -1862, -1862, -1862,
    3396,  5047,  1051,  1136,    43,  1136, -1862,    69,    69,    23,
   22351,   371,  1136, -1862,    69,    69,    23,    69, -1862,    69,
   -1862,  5721, -1862, -1862,  1086,  1103,  1339, 29733, 21035, 18480,
   -1862,  4959, -1862,  2595, -1862,  1585,   566,  1075,  1185, 22351,
    5867,  5867,   458, -1862, 13524, -1862,  1339,  1339,  1117,  1185,
   22351,  5867, -1862, 30188, -1862, -1862, -1862,  1339, -1862, -1862,
   -1862, -1862,  1339, -1862,  1073,  4582,  5867, -1862, 24429,  1121,
   -1862, -1862, -1862, 29441,   599, 22521,  1111,  5439, 24283, 29733,
   16372, -1862, 28798, -1862,  5867,  1136,   179, -1862, 32706, 28798,
    4703,  5721, -1862,   431, -1862, -1862, -1862, -1862, 21727, 26361,
    1136, -1862,  1131,  1141, -1862, -1862, -1862, -1862,  5867,  4253,
     427,   716, -1862,  5867,   666, -1862,  1039, -1862, 15664, 27017,
     920, 21554, -1862,  1339,  1339,  1126, -1862,  1011,  3664,   428,
     574, -1862,   527,   566,  1151,  1129, -1862,  3992,  1142,   666,
    3992, -1862, -1862,  2471, -1862,   759, -1862,  1164,  1174, 30439,
   -1862, 32706, -1862,   799,   958, -1862, 14425, 23859, -1862,   795,
   -1862, -1862,   843, -1862, -1862,   863,  2959,  1183,  1191,  1194,
    1199,  1202,  1214, -1862, -1862,   635,  1215, -1862,   878,  1215,
   23355, -1862,  5034, 23523, -1862, 27181, 26361,  5156, -1862, 23355,
   -1862, 32706, -1862, -1862, -1862, -1862, -1862, -1862, 23523, -1862,
   -1862, 25541, 27181, 27181, 14602,  1762,  2177,   618,  2249, -1862,
     888,  1219,  1043,  1222, -1862,  1220, 24584,  1224,  1109, 16549,
   24027,  1232, 33096,  1236, -1862, 28955, 26853, -1862, 30751,  1233,
   -1862, 32706,  2452, 32706,  2452, -1862, -1862,  3486, -1862, -1862,
    8679,  4311, 32706,  8679,  2452, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862,  1235, 32706, -1862, -1862,
   14779, -1862, -1862, 27345, -1862,  1339,  1339, -1862, -1862,  1011,
   -1862, -1862, 32706, 32706, 32706, 32706, 32706, 32706, 32706, 32706,
   32706, 32706, 32706, 32706, 32706, 32706, 32706, 32706, 32706, 32706,
   32706, 30829, -1862, -1862, 13340, 30907,  1401, 32706,  3895,   637,
    1227, -1862,    69,    69,  1227,   673, -1862,    69,    69,  1253,
    1227, -1862,    69,    69, -1862,  1215, -1862, 30985, 15841, 27017,
   -1862, -1862,  5867, 25395,  1339,  1339, -1862,  4803,  5156, -1862,
   21727, -1862, 21727, -1862, 28484, -1862, -1862,  1227, 16726, -1862,
   26525, -1862, -1862, -1862,   197, 25705, 21900, -1862, -1862, -1862,
   33174, -1862, 10884,  4324, 30439, 30361,  1252,  1255, -1862, -1862,
    1261, 30673,   397, -1862, -1862, -1862, 23859,  1273, -1862,   677,
   -1862, -1862, -1862,  1254,  3396,   850,  1274,  1282,  1284,   872,
    1291,  1295,  1310,  1322,  1325,  1337,  5047, -1862, -1862, -1862,
      69,  1264, 29112, -1862, -1862,  1253,   458, -1862, -1862,   599,
    1185, 25085, -1862, -1862,   458, -1862, -1862,   599, -1862, -1862,
    5721, -1862, 23859, 23859, -1862,  1339,  5439, 29879, 16018,  3191,
   -1862, -1862, -1862, -1862, -1862, -1862,   599,  1185,  1343,  1338,
   -1862, -1862,  2595,  1340,  1185, 22351, -1862,   599,  1185, -1862,
   30246, -1862,  1339,  1339, -1862, -1862,  1359,    87,  1362,   566,
    1370, -1862, -1862, -1862, 25395,  1353,  1376, -1862, -1862,   931,
   -1862,  1471, -1862,  1361, -1862, -1862, -1862, 27518,  1394,  1399,
    1144,  5867,  1136, -1862, -1862, -1862, -1862, -1862,  4703,   908,
    5721, 25085,  1409, 12784, -1862,  5867,  1420, 18079,  1432, -1862,
   -1862, -1862, -1862, -1862,  3992, -1862, -1862,  1517, 25869, 20343,
    1586,  2259, -1862, -1862,    69,  1440,    69,  1129,   324,  1441,
     966, 26361,   976,   992, -1862,  2471,  8679,  1426,  1449, -1862,
     677, 20516,  2392, -1862, -1862,    69,    69, -1862, -1862, 23859,
   -1862, -1862,   756,  1215, -1862,   995,  1215, 25085, -1862, -1862,
    1253, 25085, -1862,  1253,  1456,  1458,  1455,  1461, 16195,  1459,
    1464, -1862,  1465,  1469,  1466,  1474, 32706,  1475,  1494,  1495,
   27664, 32706, -1862, -1862,  2291, -1862, -1862, -1862, 32706, -1862,
    1497,  1498, 30517, 31063,  1468, 20689, -1862, 26525, -1862, -1862,
   -1862, 30595, 14956,  1496, 23691,  1500,   958,  1505, 16903, -1862,
   -1862, -1862, -1862,  8679,  1506, -1862,  1510, -1862, -1862,  4477,
   -1862,  2452,  1515,  1512, -1862, -1862, -1862,  4477, -1862, -1862,
    1168,  1521, -1862, 30361, -1862, -1862, -1862,   845,   845,   845,
     651,   651,   729,   729,   802,   802,   802,   802,    -3,    -3,
     837,   859,   869,   885,   940, 32706,  1258, 20689, 13340,  1519,
    1005,  1523,  1524,  1528,  1530,  1531,  1542,  1543, -1862,  1686,
    2605, -1862,  3895, -1862, -1862, -1862, 25085, -1862, -1862, -1862,
   -1862, -1862, -1862, 25085, -1862, -1862, -1862, -1862, -1862, -1862,
   -1862,  1253, -1862,  1518, 26689, 16549, -1862, -1862, -1862,  1227,
    1339,  4477, -1862, -1862,  1286, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, 20689, -1862,  5867,  4477, -1862,  1546,   429,  1544,
    1261, -1862, 30361,  1549, -1862,  1938, 32706, -1862, -1862,  1002,
   -1862,  1551, 20689, 32706,  1018,  1553,  1554,  1557,  1065,  1559,
    1562,  1563,  1564,  1565,  1567,   883,  1215, -1862, -1862,   893,
    1215, -1862, -1862,   901,  1215, -1862, -1862, -1862,  5439,  1715,
    1215,   603, -1862,   958,  1301, -1862, -1862,   599,  1574, -1862,
   -1862, -1862,  1069,  1576,  1083,  1577, -1862,  1232,  1587, -1862,
     599, 11391, -1862,   599,  1185,  1587, -1862,   599,  1580,  1581,
    1584, -1862, -1862, 25249, -1862,  2452,  5867, 12223,  1671, -1862,
   -1862, -1862, 21900,  1136, -1862, 20689,  1088, -1862,  1587,  1593,
   -1862, -1862, -1862, -1862,  5439, 27828, 17423, -1862,   -21,   130,
   23859,  1573, -1862,  1573, -1862, -1862,    69,  2259, -1862,   324,
    1129,  1590,   422, -1862, -1862,  1594,  5867,   324, -1862, -1862,
    1591,  1601, -1862,  1602, -1862,  1607,  1615,  1619,  1620,  1621,
    2392, -1862, -1862, -1862, -1862, -1862, 25085, -1862, -1862,  1253,
   25085, -1862,  1253,  1623,  1626,   590, -1862, 25395,   590,  2452,
   -1862,   590, -1862, 26033,   590, -1862, 32706, 32706, 32706, -1862,
   -1862, -1862, -1862, 32706, 32706,  1625, 30361, -1862, -1862, -1862,
    1628, -1862, -1862,  1627,  1636,  1637, -1862, -1862, -1862, -1862,
    1628, -1862, -1862, -1862,  1643, 20689, 20689,  1650, -1862, -1862,
   -1862,  3426, -1862, -1862,  1336, -1862,    95,  1629, -1862,  8679,
    1347, -1862, 31063, -1862,  1261, -1862, 32706, -1862,  1648, -1862,
     980,  1215, -1862,  1006,  1023,  1215, -1862,  1339, 18146,  2946,
   -1862,    69,    69, -1862, -1862, -1862,  1653,  1654, -1862, -1862,
    1354, -1862, 21727, -1862,   458,  1358, 32706, -1862, 32706, -1862,
    1659, -1862, 30673, -1862,    69, 20689,    69, -1862, -1862,  1078,
    1215, -1862,  1133,  1215, -1862, -1862,  1203,  1215, 25085, -1862,
   -1862,  1253, 25085, -1862, -1862,  1253, 25085, -1862, -1862,  1253,
    1136, -1862,  1253, -1862, 32706, -1862, 32706, -1862, 29273, -1862,
   -1862, -1862, -1862, -1862, -1862,  1662, -1862, -1862, -1862, 17587,
    1587, -1862,   599, -1862, -1862, -1862, -1862, -1862, 19338, -1862,
   -1862, -1862, -1862, -1862,   286,   437,    74, 14071,  1664,  1665,
   22164,  1667,  1670,  2548,  3629,  4277, 31141,  1675, -1862, -1862,
    1678,  1679, 22164,  1680, -1862, -1862,   599, 32706, 32706,  1812,
    1681,   596, -1862, 22851, 15133,  1684,  1676,  1646, -1862, -1862,
   -1862, 12036, -1862, -1862, -1862, -1862, -1862,   879, -1862, -1862,
   -1862,  1438,   327, -1862,   441, -1862,   327, -1862, -1862, -1862,
   -1862, -1862,  2452, -1862, -1862, 13156, 18646, -1862,  5867, -1862,
   -1862, -1862, -1862,  5867, -1862, -1862, -1862,  5867, -1862,  5439,
   -1862,  1089, 26361,   666,   666,  1129,  1594,  1677,   324,   566,
     266,  1687,  1668,  1594, 17751, -1862, -1862, -1862, -1862,  1305,
    1215, -1862, -1862,  1693,  1695, -1862, -1862,  1112,  1696,  1692,
    1095, -1862,  1700, -1862, -1862, -1862, -1862, -1862, 30361,  1261,
   31219,  1703, -1862, 21208, 21381,  1705, -1862, -1862, -1862, -1862,
    1744,  4477, -1862,  1744,  1744, -1862,  4477,  4818,  5185, 32706,
   -1862, -1862,  1369,  1719, -1862,  1712,    69, 25085, -1862, -1862,
    1253, 25085, -1862, -1862, 25085, -1862, -1862,  1253, 32706, 32706,
    1711,  1716, -1862,  1722, -1862, -1862, -1862, -1862, -1862, -1862,
    1724, -1862, -1862,  1723, -1862, -1862, -1862, -1862, -1862, -1862,
    1725, 25085, -1862, -1862,  1253, 25085, -1862, -1862,  1253, 25085,
   -1862, -1862,  1253,  1727,  1737,  1743,   458,  1375, -1862,    61,
   -1862,   958,  1748, -1862, -1862, -1862,  1752, 19505, 19672, 19839,
   27992, 29733, 27181, 27181,  1755,  1738,   290,   401,  2712, 20170,
   -1862,   417,  5867,  5867, -1862,  8679,   296,   355, -1862, -1862,
   -1862, -1862, 14071, 32706,  1765,  1829, 13893, 12410, -1862,  1754,
   -1862,  1758, 32706,  1767, 30361,  1768, 32706, 23859, 32706, -1862,
   12597,  1776, -1862,  1770,   -12, -1862,    33,  1827,   385,    69,
   -1862,  1774, -1862,  1771, -1862,  1782,  1780,  1799, 22164, 22164,
   -1862, -1862,  1836, -1862, -1862,    19,    19,   318, 13708,   421,
    1807,  1815,   427, -1862, -1862, -1862, -1862, -1862, -1862,  1808,
    1820,   324,  1594,   422,  5867, -1862, 31297, -1862,  1821, -1862,
   17915, 25085, -1862, -1862,  1253, -1862, -1862,  1825, -1862, -1862,
   32706, -1862, 26033, 32706,  1261,  1830, -1862, -1862, -1862, -1862,
    1822, -1862, -1862,  1837,  1841, -1862,  1384, -1862,  4477, -1862,
    4477, -1862, -1862, -1862, 31219, -1862, -1862,  1842,  1843,  1844,
   -1862, -1862,  1846, -1862,  1849, -1862, -1862,  1845,    69,  1856,
    1858,  1862, -1862, -1862, -1862, -1862, -1862, 32706, -1862,  1833,
   -1862,  1755,  1755,  1755,  4386,  1350,  1850,   443, -1862,  4386,
     451, 23859, -1862, -1862, -1862, -1862,  5246, 32706,  6638,   260,
   -1862, -1862,    21,  1859,  1859,  1859,  5867, -1862, -1862, -1862,
    1866, -1862, -1862, -1862, -1862,  1676,  1872, 32706,   449,  1870,
     494, 20013, 27992,  1098,  1877, 22164,  1879, -1862, -1862, -1862,
   -1862,   932, 22164, 32706,   538,   644, -1862, 32706, 30278, -1862,
   -1862,   492, -1862,  1261, -1862,  1120,  1128,  1140,   661, -1862,
   -1862, -1862, -1862,   599,  1776,  1885, -1862, -1862, 32706, -1862,
    1900,   677, -1862, 11804, -1862, -1862, -1862, 32706, 32706, -1862,
   -1862,   464,    19, -1862,   546, -1862, -1862, -1862,    69, -1862,
    1573,   324, -1862,  1594,  1902,   566,  1668, 30361, -1862, -1862,
   -1862,  1901, -1862, -1862, -1862, -1862,  1907, -1862,    69,    69,
   -1862,  1389,  1397, -1862, -1862, -1862,  1903,  1904, -1862, -1862,
   -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862, -1862,
     505,  1350,  3011,   520, -1862, -1862, -1862, -1862,    69,    69,
   -1862, -1862, -1862,   534, -1862,  1149,  5246,   707, -1862,  6638,
   -1862,    69, -1862, -1862, -1862, -1862, -1862, -1862, 22164, 22164,
    1676, 22073,    77, 31375,  1992, 22164, -1862, -1862, -1862, -1862,
   -1862, 32706, -1862, 31453,  1993,  1890, 24105, 31531, 22164, 12597,
    1676,  1057,   617,  1891, 32706, -1862,  1919,   116, 22164, -1862,
   22164, -1862,  1920, -1862, 28156,  1896,   677,   679, -1862, -1862,
    1921,  1400,  1154, 22164,  1922, 22164, 22164, 22164, -1862,   666,
    1594,  1925, -1862, -1862,  1261, -1862, -1862, -1862, -1862, -1862,
   -1862, -1862, -1862, -1862,  1924,  1926,  1927,  3011, -1862,    69,
   -1862, -1862, -1862, -1862, -1862,  1928,  4386, -1862,  2012,  7869,
      59, 17083, -1862,  9766, -1862,    32,  1155, 22164,  2016,   565,
    1929,    68, 22164, 32706,  1057,   617,  1911, 31614,  1064,  1339,
    1930,   353,  2026, -1862, 31692, 31770, 32706,  1676,  1923, 17259,
   -1862, -1862, -1862, 28156,  1931,  4931, 28320,  2452, -1862,  1940,
    1932,   118, -1862, 32706,  8679, -1862, -1862, 32706,   327, -1862,
   -1862, -1862,  1952, -1862,  1954,  1345,  1215, -1862, -1862,  1350,
   -1862, 22164, -1862,    94, -1862,   256, -1862, -1862, -1862,  1956,
   18997, -1862, -1862, 22164, -1862,    65, -1862, 22164, 32706,  1961,
   31848, -1862, -1862, 31926, 32004, 32706,  5156,  1676, -1862,   958,
   32082, 32160, 22164,  1948,   610,  1949,   649, -1862, -1862,  1969,
   18997,  1931, 32706,  1967,  2825,  4608, -1862, -1862, -1862,  1963,
   -1862,  2022,  1972,   727,  1970, -1862, -1862,  1973,  1163,    57,
   -1862, -1862, 25085, -1862, -1862,  1253, -1862, -1862, 32706, -1862,
   32706, -1862, -1862,  1492, 19171, -1862, -1862, 22164, -1862, -1862,
    1676, -1862, -1862,  1676,  1962,   668,  1975,   686, -1862, -1862,
     566, -1862,  1676, -1862,  1676, -1862,  1976, 32238, 32316, 32394,
   -1862,  1492,  1977, -1862,   599,  4608,   118,  1981, 32706,  1971,
     118,   118, -1862, -1862, 22164,  2070,  1990, -1862, -1862,  9766,
   -1862,  1492, -1862, -1862,  1997, 32472, 32550, 32628, -1862, -1862,
    1676, -1862,  1676, -1862,  1676, -1862,   599, -1862,  1994,   677,
    1998, -1862,   750, -1862, -1862, 22164, -1862, -1862, 11520,  2000,
    9766, -1862, -1862,  1676, -1862,  1676, -1862,  1676,  2006, -1862,
     677,  2007, -1862,  1984,   677, -1862, -1862, -1862, -1862, 11672,
   -1862, -1862,  1411, 32706, -1862,  1192,   677,  2452,  2009,  1988,
   -1862, -1862,  1207, -1862, -1862,  1989,  2452, -1862, -1862
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
     553,   554,   561,   562,   868,   564,   637,   638,   641,   643,
     639,   645,     0,     0,     0,   503,     0,     0,    17,   608,
     614,     9,    10,    11,    12,    13,    14,    15,    16,   824,
     875,   104,     0,     0,    20,     0,   503,   102,   103,     0,
     845,    18,    19,   884,   503,   825,     0,     0,   441,   746,
     443,   455,   866,   442,   477,   478,     0,     0,     0,     0,
     591,   505,   507,   513,   503,   515,   518,   576,   529,   563,
     487,   569,   574,   489,   586,   488,   601,   605,   611,   590,
     617,   629,   868,   634,   635,   618,   687,   444,   445,     3,
     832,   846,   508,     0,     0,   868,   907,   868,   503,   924,
     925,   926,   503,     0,  1117,  1118,     0,     1,   503,    17,
       0,   503,   466,   467,     0,   591,   513,   497,   498,   499,
     835,     0,   640,   642,   644,   646,     0,   503,   868,   690,
     869,   870,   636,   565,    22,    23,    21,   800,   795,   785,
       0,   878,   833,     0,     0,   520,   826,   830,   831,   827,
     828,   829,   503,   878,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   609,   612,     0,     0,     0,     0,     0,
     868,     0,     0,     0,    27,    29,     4,     8,    25,     5,
       6,     7,     0,     0,   503,   503,   503,     0,   102,   105,
     106,   107,   108,    85,    28,    86,    24,    46,    84,   109,
     503,     0,   124,   126,   130,   133,   136,   141,   144,   146,
     148,   150,   152,   154,   165,     0,    30,   733,     0,  1139,
       0,   504,   503,   515,   494,   569,   495,   594,   496,   601,
     605,   598,   619,   868,   620,     0,     0,   729,   734,   719,
     723,   503,   735,  1086,  1087,   503,   736,   738,   885,   503,
       0,  1119,   591,   914,   932,   503,  1123,  1116,  1114,  1121,
     438,   437,     0,   173,   752,   172,     0,   446,     0,     0,
       0,     0,     0,   453,     0,     0,     0,   436,  1001,  1002,
       0,     0,   476,   866,   868,   866,   888,   868,   868,   486,
     503,   868,   866,   945,   868,   868,   485,   868,   964,   868,
     942,     0,   584,   585,     0,     0,   503,   503,   503,   503,
     456,   866,   506,   516,   577,     0,   606,     0,   849,   503,
       0,     0,   746,   457,   591,   570,   587,   602,     0,   849,
     503,     0,   519,   571,   578,   579,   490,   588,   492,   493,
     491,   593,   603,   607,     0,   621,     0,   818,   503,     2,
     847,   906,   908,   503,     0,   503,     0,     0,   591,   503,
     503,  1127,   591,  1130,     0,   866,   866,     3,     0,   591,
       0,     0,   469,   868,   861,   863,   862,   864,   503,   503,
     866,   822,     0,     0,   781,   783,   782,   784,     0,     0,
     777,     0,   766,     0,   775,   787,     0,   688,   503,   503,
    1139,   504,   569,   594,   601,     0,   735,   736,   690,   608,
     614,   691,   692,   693,     0,   690,   871,     0,   798,   786,
       0,   883,   882,   878,   881,     0,   876,   879,     0,     0,
     109,     0,   157,     0,     0,   615,   503,   503,   792,   742,
     744,   791,     0,   741,   745,     0,     0,     0,     0,     0,
       0,     0,     0,   886,   912,   868,   922,   930,   934,   940,
     503,    92,     0,   503,   100,   503,   503,     0,    87,   503,
      94,     0,    36,    40,    41,    37,    38,    39,   503,    90,
      91,   503,   503,   503,   503,   105,   106,     0,     0,   194,
       0,     0,   635,     0,  1114,  1137,   503,     0,     0,   504,
     503,   608,     0,     0,  1125,   591,   503,  1128,     0,     0,
    1039,     0,     0,     0,     0,    26,    59,     0,    65,    66,
     158,     0,     0,   158,     0,   174,   175,   176,   177,   178,
     179,   180,   181,   182,   183,   184,   172,     0,   170,   171,
     503,    88,  1089,   504,   500,   501,   502,  1093,  1083,  1084,
    1091,    89,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1038,     2,   503,     2,   103,     0,  1048,   868,
    1139,   983,   868,   868,  1139,   868,   998,   868,   868,  1062,
    1139,  1044,   868,   868,  1053,  1060,   727,     0,   503,   503,
     599,  1088,   737,   504,   595,   596,   600,     0,     0,   464,
     503,  1131,   503,  1103,   504,  1109,  1104,  1139,   503,  1097,
     503,  1106,  1098,     2,  1139,   503,   503,   915,   933,  1115,
       0,     2,    27,     0,     0,   752,    28,     0,   750,   753,
    1137,     0,     0,   759,   748,   747,   503,     0,   851,     0,
       2,   468,   470,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   891,   948,   971,
     868,   482,     0,   887,   895,  1029,   746,   889,   890,     0,
     849,   503,   944,   952,   746,   946,   947,     0,   963,   965,
       0,   472,   503,   503,   575,   504,     0,   591,   503,     0,
    1120,  1124,  1122,   454,   592,   822,     0,   849,     0,     0,
     447,   458,   517,     0,   849,   503,   822,     0,   849,   796,
     572,   573,   589,   604,   610,   613,   608,   614,   632,   633,
       0,   797,   705,   739,   504,     0,   706,   708,   709,     0,
     216,   430,   848,     0,   428,   486,   485,   591,   102,     0,
       0,     0,   866,   449,     2,   450,   819,   474,     0,     0,
       0,   503,     0,   503,   822,     0,     0,     0,     0,   780,
     779,   778,   772,   514,     0,   770,   788,   567,   503,   503,
     103,  1048,   737,   689,   868,     0,   868,   690,   690,     0,
       0,   503,     0,     0,   873,   878,   158,     0,     0,   440,
       0,   503,  1013,   743,  1010,   868,   868,  1018,   616,   503,
     874,   913,   868,   923,   931,   935,   941,   503,   916,   918,
     920,   503,   936,   938,     0,     0,     0,     0,   503,     0,
       0,   692,     0,     0,     0,     0,     0,     0,     0,     0,
     503,     0,   123,   122,     0,   119,   118,    31,     0,    32,
       0,     0,     0,  1138,     0,   503,  1095,   503,  1105,  1096,
     185,     0,   503,   102,   503,     0,   606,     0,   504,     2,
       2,  1126,  1129,   158,     0,    55,     0,    56,    63,     0,
      62,   162,     0,   159,   160,   164,    58,     0,    57,    61,
       0,     0,    54,   752,   166,  1085,   125,   127,   128,   129,
     131,   132,   134,   135,   139,   140,   137,   138,   142,   143,
     145,   147,   149,   151,   153,     0,     0,   503,   503,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1063,     0,
     868,  1140,  1049,   986,  1003,  1050,   503,   981,   989,   725,
     984,   985,   726,   503,   996,  1006,   999,  1000,   728,  1046,
    1047,  1061,  1132,     0,   503,   504,  1090,  1094,  1092,  1139,
     597,     0,    33,   632,     0,   721,   720,   724,   730,  1101,
    1108,  1102,   503,   731,     0,     0,   761,   157,     0,     0,
    1137,   758,  1138,     0,   754,     0,     0,   757,   760,     0,
       2,     0,   503,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   868,   900,   904,   943,   868,
     957,   961,   969,   868,   979,   892,   949,   972,     0,     0,
    1025,     0,  1030,  1031,     0,   480,   852,     0,     0,   481,
     853,   473,     0,     0,     0,     0,   471,     0,     2,   854,
       0,     0,   822,     0,   849,     2,   855,     0,     0,     0,
       0,   647,   909,   503,   927,     0,     0,   503,   431,   429,
    1041,  1040,   503,   866,   451,   503,     0,   823,     2,     0,
     774,   815,   810,   811,     0,   504,     0,   806,     0,     0,
     503,   768,   767,   768,   568,   566,   868,  1049,   684,   690,
     690,     0,     0,   700,   699,  1137,     0,   690,   801,   799,
       0,     0,   877,     0,   834,     0,     0,     0,     0,     0,
    1014,  1015,  1011,  1012,   794,   793,   503,   917,   919,   921,
     503,   937,   939,     0,     0,    93,    96,   503,   101,     0,
      99,    95,    97,   503,     0,   113,     0,     0,     0,   117,
     121,   120,   195,     0,     0,     0,   752,   110,   191,   190,
    1137,   187,   713,     0,   714,   715,  1099,  1107,  1100,   186,
    1137,  1110,  1111,  1112,     0,   503,   503,     0,    49,    50,
      82,     0,    82,    82,     0,    70,    72,     0,    52,     0,
       0,    48,     0,    51,  1137,   156,     0,     3,     0,  1057,
     868,   992,   995,   868,   868,  1056,  1059,   503,     3,     0,
    1045,   868,   868,   987,  1004,  1051,     0,     0,  1133,   732,
       0,   465,   503,     3,   746,     0,     0,   762,     0,   763,
       0,   755,     0,   749,   868,   503,   868,     3,   475,   868,
     901,   905,   868,   958,   962,   970,   868,   980,   503,   893,
     896,   898,   503,   950,   953,   955,   503,   973,   975,   977,
     866,   483,  1026,  1037,     0,  1036,     0,  1028,     0,   857,
     966,   581,   580,   583,   582,     2,   823,   858,   803,     0,
       2,   856,     0,   823,   859,   647,   647,   647,   503,   707,
     710,   711,   740,   434,     0,     0,     0,   503,     0,     0,
     355,     0,     0,     0,     0,     0,   196,     0,   350,   351,
       0,     0,   355,     0,   403,   402,     0,   168,   168,   409,
     608,   614,   213,   503,   503,     0,   197,     0,   224,   198,
     199,   503,   218,   200,   201,   202,   203,     0,   204,   205,
     356,     0,   370,   206,   376,   378,   381,   207,   208,   209,
     210,   211,     0,   212,   220,   591,   503,   222,     0,   452,
       3,   836,   823,     0,   813,   790,   807,     0,   808,     0,
     809,     0,   503,   785,   785,   690,  1137,     0,   690,   696,
     690,     0,   701,  1137,     0,   872,   880,   439,  1022,   868,
    1021,  1024,  1016,     0,     0,   910,   928,  1042,     0,     0,
       0,    42,     0,   114,   116,   115,   112,   111,   752,  1137,
    1138,     0,  1134,   503,   503,     0,  1113,     3,     3,    69,
      79,     0,    73,    80,    81,    64,     0,     0,     0,     0,
     161,    60,     0,     0,   155,     0,   868,   503,   988,   990,
     991,   503,  1005,  1007,   503,  1052,  1054,  1055,     0,     0,
     102,     0,     3,     0,   982,   997,   993,  1008,    34,   722,
       0,   448,   765,     0,   865,   751,   756,   850,     3,   867,
       0,   503,   894,   897,   899,   503,   951,   954,   956,   503,
     974,   976,   978,     0,     0,     0,   746,     0,  1032,     0,
    1033,  1034,     0,   805,   823,   860,     0,   503,   503,   503,
     503,   503,   503,   503,   630,     0,     0,     0,   661,   591,
     648,     0,     0,     0,   432,   158,     0,     0,   341,   342,
     221,   223,   503,     0,     0,     0,   503,   503,   337,     0,
     335,     0,     0,     0,   752,     0,     0,   503,     0,   382,
     503,     0,   169,     0,     0,   410,     0,     0,     0,   868,
     228,     0,   219,     0,   332,     0,     0,     0,   355,   355,
     361,   360,   355,   372,   371,   355,   355,     0,   591,     0,
       0,     0,   777,   812,   814,   789,   769,   773,   771,     0,
       0,   690,  1137,     0,     0,   679,     0,   695,     0,   802,
       0,   503,  1017,  1019,  1020,   911,   929,     0,  1043,    98,
       0,    35,   503,     0,  1137,     0,   193,   192,   189,   717,
     716,   718,   188,     0,     0,    83,     0,    71,     0,    77,
       0,    75,   163,    47,     0,   167,  1136,     0,     0,     0,
       3,     3,     0,  1065,     0,  1135,   764,     0,   868,     0,
       0,     0,   902,   959,   967,   484,  1027,     0,   840,     0,
     842,   630,   630,   630,   661,   668,   635,     0,   674,   661,
       0,   503,   622,   660,   659,   655,     0,     0,     0,     0,
     662,   664,   868,   676,   676,   676,     0,   656,   672,   435,
       0,   345,   346,   343,   344,   237,     0,     0,   239,   443,
     238,   591,   503,     0,     0,   355,     0,   320,   322,   321,
     323,     0,   355,   196,   277,     0,   270,     0,   196,   338,
     336,     0,   330,  1137,   339,     0,     0,     0,     0,   391,
     392,   393,   394,     0,   384,     0,   385,   347,     0,   348,
       0,     0,   375,     0,   217,   334,   333,     0,     0,   364,
     374,     0,   355,   377,     0,   379,   401,   433,   868,   838,
     768,   690,   680,  1137,     0,   698,   701,   752,   702,   683,
     804,     0,    53,    45,    43,    44,     0,    67,   868,   868,
      74,     0,     0,   994,  1009,  1058,     0,     0,  1064,  1066,
     459,   463,   903,   960,   968,  1035,   844,   626,   628,   624,
       0,     0,  1072,     0,   669,  1077,   671,  1069,   868,   868,
     654,   675,   658,     0,   657,     0,     0,     0,   678,     0,
     650,   868,   649,   665,   677,   666,   667,   673,   355,   355,
     240,   591,     0,     0,   258,   355,   325,   328,   326,   329,
     324,     0,   327,     0,   266,     0,   196,     0,   355,   503,
     278,     0,   303,     0,     0,   331,     0,     0,   355,   354,
     355,   395,     0,   386,   503,     0,     0,     0,   215,   214,
     357,     0,     0,   355,     0,   355,   355,   355,   462,   785,
    1137,     0,   682,   697,  1137,  1023,    68,   461,   460,    78,
      76,  1067,  1068,   652,     0,     0,     0,  1073,  1074,   868,
     653,  1070,  1071,   651,   631,     0,     0,   353,   229,     0,
       0,     0,   251,   355,   231,     0,     0,   355,   260,   275,
     286,   280,   355,   196,     0,   290,     0,     0,     0,   315,
     281,   279,   268,   271,     0,     0,   196,   304,     0,     0,
     234,   352,   383,   503,   389,   396,   504,   400,   349,     0,
       0,   411,   362,     0,   158,   373,   366,     0,   367,   365,
     380,   776,     0,   686,     0,   868,  1080,  1082,  1075,     0,
     663,   355,   246,   241,   244,     0,   243,   250,   249,     0,
     503,   253,   252,   355,   262,     0,   259,   355,     0,     0,
       0,   267,   272,     0,     0,   196,     0,   291,   316,   317,
       0,     0,   355,     0,   306,   307,   305,   274,   340,     0,
     503,   389,     0,     0,     0,  1072,   397,   398,   399,     0,
     404,     0,     0,     0,   412,   413,   358,     0,     0,     0,
     685,   703,   503,  1076,  1078,  1079,   670,   230,     0,   248,
       0,   247,   233,   254,   503,   424,   263,   355,   264,   261,
     276,   289,   287,   283,   295,   293,   294,   292,   273,   318,
     319,   288,   284,   285,   282,   269,     0,     0,     0,     0,
     236,   254,     0,   390,     0,  1073,   411,     0,     0,     0,
     411,     0,   363,   359,   355,     0,     0,   242,   245,   355,
       3,   255,   425,   265,     0,     0,     0,     0,   314,   312,
     309,   313,   310,   311,   308,     3,     0,   387,     0,     0,
       0,   405,     0,   414,   368,   355,  1081,   225,     0,     0,
     355,   302,   300,   297,   301,   298,   299,   296,     0,   388,
     417,     0,   415,     0,   417,   369,   227,   226,   232,     0,
     235,   418,     0,     0,   406,     0,     0,     0,     0,     0,
     419,   420,     0,   416,   407,     0,     0,   408,   421
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1862,    12,   -54, -1862,    -1,   553,  2685,  9186,  -170, -1862,
    -198, -1862,   539, -1862,  -867, -1101, -1862,   361,  3347,  1800,
   -1862,   -33, -1862,  1645,   341,   985,   986,   887,   972,  1570,
    1566,  1569,  1589,  1568, -1862,   227,  -140,  -549, -1862,   967,
    9503,   841, -1862,  1955, -1862, -1862,  -824,  4484, -1155,  3235,
   -1862,   382, -1862,   832,    55, -1862, -1862,   643,   136, -1862,
   -1802, -1861,   329,   114, -1862, -1862,   646,   344, -1862, -1670,
   -1862, -1650, -1862, -1862, -1862, -1862,   165, -1296, -1862, -1862,
   -1371,   452, -1862, -1862, -1862, -1862, -1862,     0, -1327, -1862,
   -1862, -1862, -1862, -1862,   196,   468,   472,   268, -1862, -1862,
   -1862, -1862,  -732, -1862,   131,    79, -1862,   205, -1862,  -294,
   -1862, -1862, -1862,   848,  -271, -1103,  -187, -1862,     3,    11,
     106,  8188, -1056,  -996, -1862,   -58, -1862, -1862,    27, -1862,
     195,  1082,    40,  -350,  5373,  5171,  -496,   204,   228,   777,
    2102,  2634, -1862, -1862,  2199, -1862,   274,  6196, -1862,  2132,
   -1862,   144, -1862, -1862,  2571,   351,  6746,  4117,   -35,  1892,
     -97, -1862, -1862, -1862, -1862, -1862,  -672,  7545,  6988, -1862,
    -230,    85, -1862,  -777,   322, -1862,   271,   728, -1862,  -105,
    -269, -1862, -1862, -1862, -1862,  -132,  8132, -1090,   857,   483,
    2166, -1862,  -490,   -17,  1769,  4660,  2512,  -594,  -169,   889,
     209,  -463,  -370,  -281,  -660,  1248, -1862,  1596,   191, -1119,
    1467, -1862, -1862,   670, -1862, -1384,  -188,  -318,  -686, -1862,
     253, -1862, -1862, -1071, -1109, -1862, -1862, -1862,  2285, -1021,
    -591, -1218,   -43, -1862, -1862, -1862, -1862, -1862, -1862,  -153,
   -1064,  -299, -1843,   132,  8855,   -74,  8546,  -122,  1448, -1862,
    1717,   -28,  -317,  -297,  -290,    18,   -78,   -65,   -49,   559,
     -52,   -45,   -32,  -289,   -16,  -286,  -253,  -228,    52,  -226,
    -212,  -184,  -583,  -565,  -551,  -544,  -573,  -131,  -535, -1862,
   -1862,  -820,  1443,  1447,  1450,  2242, -1862,   770,  2761, -1862,
    -607,  -587,  -568,  -540,  -566, -1862, -1761, -1773, -1732, -1723,
    -577,  1378,  -158,  -255, -1862,   105,   243,  -125, -1862, 10137,
    3279,  -569,  -397
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   312,   243,   244,   186,    93,  1365,   245,   246,
     247,  1440,  1441,   248,  1224,  1225,  1226,  1460,   249,   481,
     251,   252,   537,   538,   253,   254,   255,   256,   257,   258,
     259,   260,   261,   262,   263,   264,  1028,   932,   933,   934,
     539,  1583,   587,   316,   589,   266,  1200,  1366,  1367,  1368,
    1369,  1370,  1371,  1372,  2148,  1373,  1374,  1726,  2004,  2005,
    1942,  1943,  1944,  2120,  2121,  1375,  1745,  1746,  2028,  1747,
    1872,  1873,  1376,  1377,  1378,  1379,  1380,  1381,  1901,  1905,
    1606,  1598,  1382,  1383,  1605,  1599,  1384,  1385,  1386,  1387,
    1388,  1389,  1390,  1764,  2043,  1765,  1766,  1974,  1391,  1392,
    1393,  1586,  2053,  2054,  2055,  2172,  2182,  2073,  2074,   404,
     405,  1107,  1108,  1334,    95,    96,    97,    98,    99,  1729,
     267,   300,   103,   104,   105,   106,   332,   333,   407,   386,
     269,   489,   270,   109,   419,   111,   112,   166,   272,   273,
     116,   117,   118,   182,   119,  1135,   274,   167,   122,   356,
     123,   168,   365,   276,   453,   278,   169,   484,   128,   129,
     281,   130,   780,  1100,  1098,  1099,  1702,   282,   283,   133,
     134,  1328,  1550,  1709,  1710,  1833,  1834,  1551,  1697,  1853,
    1711,   135,   837,  1415,   178,  1144,   284,  1145,  1146,  1627,
     969,   786,  1203,   285,   286,   787,   288,   289,   290,   789,
     490,   491,   317,   689,   690,   691,   692,   693,   441,  1413,
     442,  1133,  1131,   822,   443,   468,   444,   445,   492,   137,
     188,   189,   138,  1126,  1127,  1128,  1129,     2,  1315,  1316,
     813,  1401,   139,   431,   432,   367,   378,   763,   140,   320,
     141,   422,  1029,   753,   723,   180,   142,   475,   476,   477,
     143,   424,   336,   337,   338,   425,   145,   146,   147,   148,
     149,   150,   151,   341,   426,   343,   344,   345,   427,   347,
     348,   349,   630,   631,   632,   633,   634,   350,   636,   637,
     638,   853,   854,   855,   856,   724,  1074,  1306,   291,  1637,
     640,   641,   642,   643,   644,   645,  1836,  1837,  1838,  1839,
     597,   292,   293,   294,   295,   493,   307,   154,   155,   156,
     297,   904,   646
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      91,   469,   760,    91,   542,   207,   744,   162,   455,   205,
    1039,  1602,   100,   706,  1414,   158,   651,  1406,   208,   144,
    1319,   985,   192,   478,   941,  1030,   702,   413,   107,  1617,
    1618,   210,  1161,   707,   209,   695,  1588,   975,   211,   559,
     708,   709,   896,  1397,   710,   983,   730,   464,   465,  1416,
     214,   212,   741,   880,   483,   984,   976,  1423,  1924,   971,
    2007,    91,    91,   988,    91,   170,  1015,  1587,  1016,   995,
    1230,  1928,  1007,   972,  1323,   757,   100,   711,  1884,   335,
     973,   495,    91,   144,   977,   397,   768,  1210,    91,   974,
     406,   342,   107,    91,  1877,  1768,  2006,  1402,  1532,  1925,
      64,    91,   712,   706,   713,  1536,    91,   101,  1926,    91,
     163,   557,   303,    91,  2013,   417,  1351,   372,   714,   318,
    1462,  1033,    64,   707,  1770,   339,   826,   497,   368,  -816,
     708,   709,   379,   665,   710,   610,   611,   671,   635,  2114,
     498,  2012,  1556,  1557,  1260,   124,   715,  2077,    64,  1407,
      80,   826,    91,   500,  1351,    91,   499,    91,  1265,   346,
     501,  1939,  1940,    91,  1088,    82,  1998,   711,  1408,   100,
      91,   101,    80,   502,   650,  1095,   144,  1769,    91,   318,
     413,   612,   613,  1782,  1611,   107,   729,    82,   518,   722,
      91,  1575,   712,   737,   713,  -849,   651,  1771,    80,   306,
    1939,  1940,    91,    91,    91,   113,  1957,   398,   714,   124,
    1406,   722,  2119,    82,  2047,   756,   360,   591,   601,  1303,
     373,  2014,   303,  1118,   985,  1545,   767,  2006,  1467,   114,
    2008,  1964,  1965,   989,   323,    91,   715,   992,   898,  2068,
    2119,  1305,   975,   998,   153,   228,   399,   153,  2020,  1941,
     157,   202,   207,   319,  2078,  -849,   704,  2069,   376,  -627,
    2150,   976,  1468,  1558,   101,   208,   306,    91,    91,   113,
    1018,  1924,  1546,  1561,  1268,   120,  1623,  1023,   210,  1083,
    1085,   209,   406,  1234,  1928,   211,  2051,   321,  1969,   977,
     592,    91,   124,   114,   651,    91,    20,  1153,   212,  1534,
    1409,    91,   124,   717,  2023,  2024,  1781,  1397,   153,  1587,
    1784,   406,  1925,   686,   876,   718,  1689,  2012,    91,  1410,
     677,  1926,   406,    91,    91,    91,   835,   836,  1622,    91,
      91,   663,  1547,   839,  1142,   669,  -817,  1007,   651,   120,
    1432,   939,   207,   804,  1998,   620,   808,   793,  2012,   303,
      91,   843,   125,  1630,   650,   208,  1075,   153,    91,   775,
      91,   804,   113,   806,  1079,  1657,  1659,  1661,    -3,    91,
      91,   209,   450,    91,  1217,  1255,   409,   303,  1149,   335,
      91,   975,   398,   719,   503,   627,   114,  1045,   908,   621,
     171,   342,   764,   717,    91,    91,  1165,    91,  1191,  1253,
     976,   153,    91,   971,   988,   718,    91,  1046,  1472,  1254,
    1034,   791,   176,    91,  1047,  1048,   125,   972,  1049,    91,
      91,   482,   120,  1850,   973,   795,  2070,  2071,   977, -1138,
    1851,   922,   120,  1238,  1545,  1545,  1545,    91,    91,   303,
    1143,  1081,    91,   543,  1411,   461,  1600,  1086,   591,  1852,
      64,  1050,   650,   561,   591,  1588,  1553,    91,   562,   796,
     324,  1270,   191,   550,   751,   721,    91,   726,   399,    91,
    1601,  1094,    91,   719,   734,  1554,  1051,  1237,  1052,  1704,
     761,  1546,  1546,  1546,   124,  1721,  1587,  -694,  1105,   592,
     399,  1045,  1053,  1017,  -694,    91,   650,   635,  1143,   125,
      80,  1320,   172,   461,  1600,   173,   174,  1003,   175,   125,
      64,  1046,  1020,   124,   677,    82,    91,   310,  1047,  1048,
    1054,  1406,  1049,   898,   124,  1991,   400,   399,  1601,  1041,
    1255,  1793,  1263,  2031,   751,   731,  1449,    69,    70,   722,
     193,  1547,  1547,  1547,  1723,    91,  1903,   803,   805,   124,
     557,  1772,  1277,   376,    92,  1050,   650,   160,   194,  1603,
      80,    91,  1596,    91,  1035,  1036,    91,   650,   409,   946,
      91,   325,   819,    91,  1266,    82,  1421,  1077,   202,    64,
    1051,  1904,  1052,  1604,   203,  1037,   906,  1716,  1875,    85,
    1705,  1553,   306,  1883,  1656,   811,  1053,   409,  1267,   722,
   -1001,  1555,   820,   821,  1090,   450,  1717, -1001,   409,  1268,
    1787,  1093,  1259,  1841,   120,  1097,   541,  2059,    92,   313,
     324,  1716,   775,    91,  1054,  1400,   497,    91,  1907,    80,
     314,  1451,  1842,   409,   769,   967,    92,   979,   311,   498,
    1844,  1455,   298,   120,    82,  1597,   315,    92,   214,   781,
     325,    91,   500,   304,   120,   499,    91,    91,   840,   501,
      92,   842,   898,    92,   177,  1473,   340,    92,  1499,   369,
     908,  1909,   502,   380,   326,  1851,   550,  1483,  1486,   120,
    1155,  1885,    91,   327,   686,  1022,   351,   650,  1065,  1006,
    1929,   125,   371,  1024,  1923,  1866,  1867,  1868,  1869,  -681,
    1066,  1910,   698,    91,  1851,  1141,  -681,   406,   898,  1930,
    1019,    92,  1042,  -497,    64,    91,    64,    92,  1870,   874,
     125,  1956,   877,  1933,   879,  1457,  1458,  1876,   882,   450,
     497,   125,   462,   394,   677,   898,   398,   885,   202,    91,
     887,   888,   889,   498,   418,    91, -1002,   396,    91,  1207,
     651,  1271,    64, -1002,  2018,  1195,   125,    92,    92,   499,
     202,  1303,  1196,   304,    80,   557,    80,   -23,  1067,  1300,
    1117,   399,   622,  1304,  1866,  1867,  1868,  1869,   115,    82,
     898,    82,  1065,  1305,   891,  1508,   792,   898,  1644,   553,
    2097,  1811,  -837,  1812,  1066,   892,   893,  1870,  2022,   867,
      91,   986,    80,   722,   228,   625,  -197,    91,   563,    91,
    1878,  2037,    91,   564,    91,  1879,  1115,    82,   550,   898,
    1122,   650,    92,    91,   100,  1404,   603,  1890,   677,  2099,
    1638,   144,  1879,   604,   605,    64,  1394,   993,   898,   467,
     107,   625,   115,   450,    91,  1980,   908,  1620,  2125,  1252,
    1981,   635,   439,  1185,  1628,    92,   898,  1124,  1189,  1015,
    1016,   678,  1067,   450,   213,    70,  2127,    92,   906,  1197,
    2088,   450,   470,   503,   482,   722,   550,   701,    92,   703,
    1645,   591,   823,    92,    92,    80,   824,   363,  1204,   504,
     304,  1756,   541,  2109,  1501,   541,   505,  1488,  2110,   172,
      82,   541,   173,   174,    92,   175,   606,   607,   650,   124,
     541,  1027,    92,   506,  1753,   512,  2163,  1830,   304,   101,
    1166,  2164,  1843,  1123,   722,   844,   507,    92,    91,   845,
      91,  1215,  1216,   765,  1114,   115,    91,    81,   608,   609,
     541,  1322,   686,  1017,   947,   948,   949,  1206,   778,  1616,
    1204,   783,   352,   353,  1006,   354,   160,   124,   508,   851,
      92,   355,    64,   722,  1593,   849,   509,    91,   515,   850,
      87,    88,    64,    92,    92,   516,   550,   614,   615,    91,
      64,    91,   195,     6,     7,     8,     9,    10,    11,    12,
      13,  1506,   159,   409,   184,   185,    71,    72,    73,    74,
      75,    76,    77,    78,   521,  1204,  1720,   560,  1417,   858,
      91,   834,    80,   859,   398,  1845,   503,   113,   722,  1827,
    1828,  1829,    80,    91,    91,  1204,   602,    82,   650,   860,
      80,   686,   565,   845,    91,   616,   731,    82,  1058,   120,
     722,   114,   871,   617,   906,    82,   722,  1288,   363,    92,
     381,   722,  1275,  1794,   897,   678,   153,  1292,   898,    64,
     153,   722,    81,   659,   660,  1296,  1560,    91,  1594,   722,
     881,   618,   811,   450,   503,  1806,   722,    14,    15,    16,
      17,    18,   619,   108,   829,    64,   164,   120,   625,  1866,
    1867,  1868,  1869,  1017,  1122,    87,   830,  1104,  1204,   553,
    1394,  1105,    64,   622,    91,    91,   686,   450,   627,    80,
    1614,  1076,  1870,   661,   662,   925,   125,   927,   100,  1080,
     930,  1871,   647,    91,    82,   657,  1503,   942,  1504,  1122,
     681,  1124,  1148,   542,   107,    80,   824,  1914,  1089,  -500,
     550,   694,  1150,    64,  1477,    91,   824,   108,   722,  1096,
      82,   762,    80,  1443,  1444,  1445,  1685,    64,  1151,  1170,
    1446,  1447,   845,   722,   125,   623,  1124,    82,  1274,   986,
    1481,   503,   859,   625,   625,   673,   301,    92,    91,    69,
      70,    92,   398,  1475,  1886,   363,   722,  1484,   774,    70,
    -498,   625,  1235,    80,  1491,   686,   696,  1123,  1204,  1204,
      14,    15,    16,    17,    18,   827,   394,    80,    82,  1500,
    1012,  1013,    64,   101,  1866,  1867,  1868,  1869,   900,   901,
      91,   720,    82,  1510,  1911,   469,   469,   699,   363,   731,
     108,    85,  1123,   722,   411,  1311,    92,  1870,    92,   898,
     108,   700,  1511,   301,   321,  1399,   722,   755,    91,  1313,
     742,   124,   811,   898,    81,  1615,   722,    92,  1204,   859,
    -499,  1641,    80,   482,  1862,  1642,    64,   743,   898,    92,
      14,    15,    16,    17,    18,   678,   829,    82,   622,   911,
     625,   318,    64,  1619,   790,  1017,  1887,    87,   830,   766,
     898,  1727,   833,    92,  1888,  1727,  1748,  1515,   859,    92,
     794,   722,   553,   814,   518,   815,  1889,   450,   461,  1748,
     898,   113,  1696,   661,  1112,  1934,    80,  1591,   554,   859,
    1985,  2015,  1122,   838,   898,   898,   363,   158,   846,  2113,
     841,    82,    80,   898,   153,  1396,    64,  1231,  1232,   686,
     847,  1992,    91,    91,    91,  1994,  1610,    82,   100,   861,
     153,   686,    14,    15,    16,    17,    18,   862,  2179,  1124,
     863,    92,  2176,    92,   107,   864,    92,  1519,   865,   153,
     686,   722,  1437,  2185,  2138,  1728,    91,  2186,  2142,  1728,
     866,   120,   100,   552,    64,   899,    80,   411,   902,   706,
     903,    91,  1442,   910,    91,    91,   919,    91,   107,   372,
     920,    82,    91,  1653,  1654,   923,    91,   943,    91,   707,
    1537,  1538,  1539,   368,   379,   627,   708,   709,    64,   980,
     710,   680,   108,   -18,    64,  1123,  1031,  1122,   898,  1236,
     762,  1032,  1526,  1552,    80,  2057,  1672,  1040,  1674,   746,
    1055,   750,  1043,   101,  1854,  1854,  1854,   686,  1056,    82,
    1057,   108,  1069,   711,  1677,  1261,  1262,  1059,   125,  1309,
      91,  1060,   108,  1474,  1124,    91,    91,    91,    80,  1631,
    1307,  1308,  1317,   722,    80,  1321,  1061,   101,   712,  1324,
     713,   124,    92,    82,  1227,   164,  1798,   108,  1062,    82,
      92,  1063,    81,   482,   714,   482,    92,   954,   955,   956,
     957,   360,   373,  1064,  1730,  1465,  1466,   804,  1730,  2062,
    1091,   750,  1092,   722,  1831,   124,  1471,  1466,   722,  1102,
    1713,    92,   715,  1498,  1466,    87,    88,  1502,  1466,  1045,
    1123,  -625,  1714,    92,  -623,    92,  1596,  1597,  1663,  1664,
     376,   113,  1101,   162,  1686,   898,  1103,    91,  1106,  1046,
    1109,    91,    91,  1810,  1466,   456,  1047,  1048,  1919,  1466,
    1049,   363,   153,  1110,    92,  1396,  1920,  1466,  1111,   301,
    1983,  1984,   543,   686,   762,   113,  1939,  1940,    92,   662,
     153,  2176,  2177,  1463,  1464,    92,   958,   959,    92,  1120,
     115,   950,   951,  1050,   952,   953,  1130,   686,   686,  1396,
    1715,  1897,  1134,    19,  1136,  1783,  1785,    91,  1748,  1855,
    1856,   120,  1139,  1147,   153,  1154,  1816,  1817,  1051,   911,
    1052,    92,  1175,    91,  1176,  1177,   764,  1178,   598,  1179,
    1180,  1181,  1846,  1122,  1053,  1182,  1183,  1201,   921,   153,
    1184,  1186,  1552,  1552,  1552,   120,   163,  1698,  1552,    54,
      55,    56,    57,    58,    59,    60,    61,    91,  1332,    91,
    1187,  1188,  1054,  1193,  1194,  1211,  1713,   153,   101,  1212,
    1124,  1713,   101,   101,  1213,  1218,  2046,    92,  1714,  1219,
     717,  1228,  1229,  1714,  1233,  1239,   101,  1258,   125,  1240,
    1241,  1247,   718,    91,  1242,   762,  1243,  1244,    91,    14,
      15,    16,    17,    18,  1535,    91,   124,    91,  1245,  1246,
     124,   124,  1273,  1269,   761,    91,  -165,  1276,  1559,  1279,
    1280,   469,   125,  1281,   124,  1282,  1979,   706,  1283,  1284,
    1285,  1286,  1438,  1287,   686,   372,  1123,  1301,  1581,  2075,
    1310,   686,  1312,  1314,  1900,   541,  1715,   707,  1398,    92,
    -820,  1715,  1325,  1326,   708,   709,  1327,   554,   710,  1403,
     719,  1412,  1418,  1424,  1420,    64,   113,  1425,  1426,  2075,
     113,   113,   686,  1427,    92,  -123,  -123,  -123,  -123,  -123,
    -123,  1428,   363,   153,   113,  1429,  1430,  1431,   456,  1435,
    1732,   711,  1436,  1452,  1732,  1732,   686,  1448,  1450,  2003,
     206,    94,    92,  2122,   161,   153,  1453,  1454,  1732,   153,
     153,  2052,  1456,  1459,  1476,    80,   712,  1469,   713,  1496,
    1497,  1065,  1505,   153,   334,  -821,  1585,   598,  1562,  1563,
      82,  1566,   714,  1066,  1567,  1590,   120,   360,   373,  1576,
     120,   120,  1577,  1578,  1580,    91,   898,   108,    91,  1621,
    1625,  1442,   -22,   482,   120,  1589,  1626,   686,   686,  1635,
     715,  1636,  1639,  1640,   686,    94,    14,    15,    16,    17,
      18,  1643,  1648,   153,  1652,  1655,   376,   686,  1666,   650,
      -3,   423,  1665,   204,   115,  1673,   250,   686,   503,   686,
    1675,  1678,  1676,  1682,    94,   108,  1759,  1760,  1761,  1762,
    1763,  1067,   686,  1683,   686,   686,   686,   331,  1713,  1684,
     359,  1688,   456,   125,    94,  1690,  1734,   125,   125,  1701,
    1714,    14,    15,    16,    17,    18,  1597,  1703,  2117,  1555,
    2003,   125,    64,  1351,  1975,    91,  2052,  1774,  1722,  1724,
    2052,  2052,   686,  1749,  1777,   783,   686,  1750,    92,    92,
    2056,   686,   161,  2010,   369,   380,  1752,  1754,    94,  1767,
    1775,   161,    92,  1778,   421,   429,   207,   761,  2140,  2161,
     808,  1776,   554,  1788,    91,   153,    91,   449,  1789,   208,
    1791,  2040,    80,  1792,  1799,   101,   629,    64,  1715,  1786,
    2171,  1802,  1262,  1807,  2171,   209,  1826,    82,  -501,  1977,
     686,    92,   487,  1808,   204,   204,  2180,  1809,  1813,  1814,
    1815,  1820,   686,  1975,    92,  1818,   686,   717,  1819,    92,
      92,    92,  1822,   124,  1823,    91,   456,   598,  1824,   718,
    1707,   686,  1858,  2178,   487,   250,   161,    80,  1859,  1840,
     319,  1863,   423,    91,    91,  1865,   456,   705,   334,  1894,
     250,   159,    82,  -502,   456,    71,    72,    73,    74,    75,
      76,    77,    78,   303,  1896,  1912,  2149,  1915,   423,   628,
    1916,   649,  1921,  1922,  1947,  1952,   686,  1065,  1977,  1953,
    1966,  2158,  1968,   113,  1973,  1978,  1987,  1982,  1993,  1066,
    1995,   449,  1996,  1997,  2001,   449,   722,   719,  2017,   250,
    2025,    92,   359,    84,    91,   161,  1025,  1732,  2032,  2019,
    2030,  2049,  2038,   686,   115,  2060,   101,  2061,   686,  2072,
    2042,  2050,   153,   677,   423,   421,  2081,    92,  2096,  2098,
     331,   331,  2100,  2104,  2106,  2107,  2108,   809,   423,  2112,
    2111,  2128,  2124,  2136,   686,  1891,   101,   686,   115,   686,
    2139,   421,  2145,   120,   124,  2126,  2146,  1067,   449,    94,
    2141,    92,  2151,  2168,  2162,  2160,   482,   765,   686,  2170,
     200,  2173,   363,  2174,   359,  2183,    91,  2184,  2187,   800,
     101,  1804,   894,   961,   124,    91,   960,   962,   964,   108,
    -122,  -122,  -122,  -122,  -122,  -122,  1470,   812,   566,  1584,
     567,   568,   569,  1592,   588,  2169,  2118,   421,   963,  1736,
     250,    92,   429,    92,   113,  2135,  1970,   382,   124,   429,
     421,   421,   383,  1963,  2115,   387,  1758,   392,   449,   161,
     125,   570,  1892,  1906,   571,   572,  1893,  2102,  1732,   573,
     574,  2041,  2143,  2175,   113,  2101,  1609,    92,   250,   449,
     831,   649,    92,   153,   183,   389,   456,   754,  2000,    92,
     471,    92,    14,    15,    16,    17,    18,   895,  1732,  2103,
    2066,  1700,    14,    15,    16,    17,    18,  1624,   113,  1913,
    1272,  1607,  1790,   153,   120,     3,   250,   487,  1038,   852,
     456,  1132,   598,  1152,  1157,   482,   204,   482,  1158,  1687,
       0,  1159,  1732,   800,    14,    15,    16,    17,    18,  1190,
     487,     0,     0,   487,   120,   161,   161,   153,     0,   487,
       0,     0,     0,     0,     0,     0,   526,     0,   487,     0,
       0,   161,   161,   161,   250,   482,     0,     0,    64,   115,
       0,   970,   472,   115,   115,   629,   449,     0,   120,   914,
      92,     0,     0,     0,     0,   429,   161,   115,     0,     0,
     159,   125,   184,   185,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,     0,   382,   383,     0,   655,     0,
     392,     0,     0,     0,     0,   762,     0,     0,    80,     0,
     250,   125,     0,   649,     0,     0,     0,     0,     0,    92,
     482,    81,    92,    82,     0,    14,    15,    16,    17,    18,
    1548,   473,     0,     0,     0,     0,     0,   800,     0,   108,
       0,  1044,     0,   829,   628,   125,     0,   625,   628,  1014,
       0,     0,     0,   334,    87,   830,     0,   800,     0,     0,
       0,     0,     0,     0,     0,   800,     0,     0,   250,   449,
       0,     0,     0,   108,     0,     0,     0,   423,     0,     0,
     449,     0,   449,   423,   649,   382,     0,     0,   250,     0,
     449,    64,     0,     0,     0,   161,   449,     0,     0,     0,
       0,     0,     0,     0,   471,     0,  2137,     0,     0,    92,
     456,     0,     0,     0,     0,     0,   487,     0,     0,     0,
       0,     0,     0,     0,   331,     0,     0,     0,   363,     0,
       0,     0,     0,     0,     0,     0,   331,     0,  2159,     0,
       0,    80,     0,     0,     0,  1116,     0,   423,    92,     0,
    2048,     0,     0,     0,    81,     0,    82,     0,     0,     0,
     421,     0,   487,   487,     0,     0,   421,     0,   250,     0,
       0,     0,     0,     0,     0,     0,   851,     0,     0,     0,
     722,     0,     0,     0,   785,   159,   472,    87,    88,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,  2089,
       0,     0,     0,   733,   159,     0,   184,   185,    71,    72,
      73,    74,    75,    76,    77,    78,     0,    92,    92,   195,
       6,     7,     8,     9,    10,    11,    12,    13,   421,     0,
     421,     0,     0,    94,     0,     0,     0,   161,     0,  1548,
    1548,  1548,   164,  1694,  1695,  1699,     0,     0,   449,   831,
       0,   831,     0,     0,     0,     0,     0,     0,   762,     0,
       0,   161,     0,     0,   108,     0,     0,   800,   108,   108,
       0,   852,   852,     0,     0,     0,   115,   277,    92,   487,
       0,   159,   108,   184,   185,    71,    72,    73,    74,    75,
      76,    77,    78,     0,  1205,     0,     0,     0,   250,     0,
       0,   800,     0,     0,    64,   970,     0,     0,     0,     0,
     161,     0,     0,     0,     0,     0,     0,  1251,     0,   629,
       0,     0,   201,     0,     0,   449,     0,   449,     0,     0,
       0,     0,   250,     0,   250,   351,     0,     0,   159,     0,
     328,   329,    71,    72,    73,    74,    75,    76,    77,    78,
    2181,   857,     0,     0,    80,     0,  1205,  1568,     0,  2188,
       0,     0,     0,     0,   364,     0,     0,   869,     0,    82,
     872,     0,     0,     0,     0,     0,   385,   388,     0,     0,
       0,     0,   152,     0,     0,   152,     0,   449,   628,  1249,
      84,   575,   576,   577,   578,   579,   580,   581,   582,   583,
     584,   585,   628,     0,   526,   423,   313,   115,     0,     0,
       0,  1205,    89,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,   449,   914,   277,     0,     0,     0,
       0,  1205,     0,   586,     0,     0,     0,   115,     0,     0,
       0,   595,   449,     0,     0,   159,   152,   328,   329,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,     0,
       0,   423,   449,   654,     0,     0,     0,     0,     0,     0,
       0,   115,     0,     0,    81,   305,     0,     0,   527,     0,
       0,     0,   595,     0,   382,     0,   595,     0,   421,     0,
     277,     0,     0,     0,     0,   152,  1706,    84,     0,     0,
       0,     0,     0,  1707,  1205,     0,     0,    87,    88,     0,
       0,   161,     0,     0,     0,     0,     0,  1078,     0,    89,
       0,     0,     0,     0,    64,   364,     0,   487,     0,     0,
     788,     0,   449,     0,     0,   449,     0,     0,     0,   152,
       0,     0,   305,     0,   421,   429,   161,     0,     0,   277,
     487,     0,     0,     0,     0,     0,   364,   831,   159,     0,
     328,   329,    71,    72,    73,    74,    75,    76,    77,    78,
       0,   733,     0,     0,    80,     0,     0,     0,     0,     0,
     852,   108,     0,   305,     0,     0,  1493,    81,     0,    82,
       0,     0,    14,    15,    16,    17,    18,   785,     0,     0,
       0,   277,     0,   161,  1205,  1205,     0,     0,     0,  2044,
      84,     0,     0,   722,     0,   305,     0,   556,     0,   687,
      87,    88,     0,     0,     0,     0,     0,     0,   364,     0,
       0,     0,    89,     0,     0,   449,   449,  1156,     0,   277,
     595,     0,     0,     0,    14,    15,    16,    17,    18,     0,
     639,     0,     0,  1173,     0,     0,     0,  1174,    64,     0,
       0,     0,   364,     0,  1205,     0,   364,     0,     0,     0,
       0,     0,     0,   364,     0,     0,   670,   277,     0,   159,
       0,     0,   449,    71,    72,    73,    74,    75,    76,    77,
      78,     0,   159,     0,     0,   449,    71,    72,    73,    74,
      75,    76,    77,    78,     0,   364,     0,     0,    80,     0,
      64,     0,   108,   857,   857,   725,     0,     0,     0,     0,
       0,    81,   725,    82,  1168,   277,     0,  1171,     0,     0,
    1249,    84,     0,     0,     0,     0,     0,   595,     0,   161,
     305,   654,   108,    83,    84,     0,   423,     0,   161,     0,
       0,     0,     0,    89,    87,    88,   788,   487,     0,     0,
      80,     0,     0,     0,     0,     0,    89,     0,     0,     0,
       0,     0,  1256,    81,     0,    82,   108,     0,     0,  1257,
      64,   277,     0,   487,   250,     0,     0,     0,     0,     0,
       0,   487,     0,     0,     0,  1831,     0,     0,     0,   722,
       0,     0,     0,   364,   725,     0,    87,    88,     0,   364,
     305,     0,     0,     0,   159,   359,    94,     0,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,   421,
      80,   639,   161,     0,     0,     0,     0,     0,     0,   277,
     595,     0,  1650,    81,   161,    82,     0,   364,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   831,     0,   277,
       0,   595,     0,   788,     0,    83,    84,   277,   305,     0,
     725,     0,     0,   449,   449,     0,    87,    88,     0,     0,
       0,     0,     0,     0,     0,  1712,   725,   870,    89,   725,
     873,   305,     0,     0,   305,     0,   305,   305,     0,     0,
     305,     0,     0,     0,     0,     0,     0,   364,     0,   305,
     733,     0,   305,   305,   305,     0,     0,  1290,   364,     0,
       0,  1294,     0,     0,   159,  1298,  1087,   907,    71,    72,
      73,    74,    75,    76,    77,    78,   527,     0,     0,   277,
       0,     0,     0,   788,     0,     0,     0,     0,     0,     0,
       0,     0,  1433,     0,     0,     0,  1434,   161,   161,   161,
     161,   788,   161,   161,     0,     0,     0,     0,  1708,   429,
       0,     0,     0,     0,     0,     0,  1072,     0,     0,     0,
       0,     0,   487,   788,     0,     0,   487,   487,  1026,     0,
     687,     0,     0,   309,     0,     0,     0,   487,     0,   788,
     487,   364,     0,   788,     0,   639,     0,     0,   595,   639,
     639,     0,     0,     0,     0,     0,   639,     0,     0,   595,
       0,     0,   857,     0,   364,     0,  1001,     0,   359,     0,
       0,  1712,     0,     0,     0,     0,  1712,     0,   364,     0,
       0,     0,     0,  1847,     0,  1712,     0,     0,     0,     0,
     161,   364,     0,     0,     0,     0,   556,     0,     0,     0,
     309,     0,   161,     0,     0,     0,     0,     0,     0,   277,
       0,     0,     0,     0,  1523,     0,     0,   305,  1524,     0,
       0,     0,  1525,     0,     0,     0,   725,     0,     0,     0,
     725,     0,     0,     0,     0,    64,     0,     0,   595,     0,
     788,   494,  1479,   277,     0,   595,     0,     0,     0,     0,
       0,     0,     0,     0,  1708,  1832,     0,     0,   788,  1708,
       0,   487,     0,   305,   305,   788,  1708,     0,  1708,   159,
       0,   328,   329,    71,    72,    73,    74,    75,    76,    77,
      78,  1513,     0,     0,  1517,    80,     0,     0,  1521,   600,
       0,   429,   161,     0,     0,     0,     0,     0,    81,   159,
      82,   480,     0,    71,    72,    73,    74,    75,    76,    77,
      78,  1220,   364,     0,     0,     0,  1221,     0,  1222,     0,
     330,    84,   511,  1935,   514,     0,  1712,     0,   480,   520,
     725,    87,    88,     0,   152,   595,     0,     0,   152,   529,
     530,     0,     0,    89,     0,     0,     0,     0,     0,   907,
     639,    84,   639,     0,  1461,     0,     0,   480,   480,   159,
       0,     0,   305,    71,    72,    73,    74,    75,    76,    77,
      78,   928,   725,   725,  1223,  1331,     0,     0,     0,     0,
     305,     0,  1223,   725,  1169,     0,   725,  1172,   687,     0,
       0,  1832,  1832,     0,     0,     0,     0,     0,   752,   364,
       0,     0,     0,  1667,     0,     0,  1708,  1668,     0,  1708,
    1669,   556,     0,  1712,   929,     0,     0,     0,     0,     0,
       0,   429,   595,     0,     0,     0,     0,     0,     0,     0,
       0,  1633,     0,     0,     0,     0,     0,  1679,   788,   487,
       0,  1680,   788,     0,     0,  1681,     0,     0,     0,     0,
       0,     0,   423,     0,   161,     0,  1223,   595,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   752,     0,
    1223,     0,     0,     0,     0,     0,     0,   687,     0,     0,
    1026,     0,     0,     0,     0,     0,     0,  1832,   600,   639,
       0,   639,     0,     0,     0,     0,  1708,     0,     0,     0,
       0,     0,   159,   639,   184,   185,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,     0,   364,
       0,   809,   423,     0,     0,   907,   309,     0,     0,     0,
       0,     0,     0,   161,     0,  2045,   429,   159,     0,   328,
     329,    71,    72,    73,    74,    75,    76,    77,    78,   875,
       0,     0,     0,     0,     0,   494,     0,  1801,   883,  1832,
     788,     0,     0,     0,   788,   725,     0,     0,   788,   725,
     161,     0,     0,     0,     0,     0,   725,  1291,  1570,     0,
     725,  1295,   423,     0,   725,  1299,     0,     0,   480,     0,
       0,  1302,     0,     0,   480,     0,     0,     0,     0,     0,
     161,     0,     0,   461,  2045,  2045,     0,     0,     0,     0,
       0,   159,   152,   328,   329,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,     0,   152,     0,
       0,     0,     0,     0,   161,     0,     0,   725,     0,     0,
      81,   687,     0,     0,     0,     0,     0,   152,     0,     0,
     595,   305,     0,     0,     0,     0,     0,     0,   639,     0,
       0,     0,   330,    84,     0,  2045,  1223,     0,    14,    15,
      16,    17,    18,    87,    88,     0,     0,     0,     0,     0,
       0,   725,     0,     0,     0,    89,     0,     0,  1008,     0,
       0,     0,     0,     0,     0,   277,     0,  1835,     0,     0,
       0,     0,  1530,     0,   305,     0,     0,     0,     0,   480,
     480,   480,   480,   480,   480,   480,   480,   480,   480,   480,
     480,   480,   480,   480,   480,   480,   480,   480,     0,     0,
       0,     0,     0,     0,    64,   494,   159,     0,   459,   460,
      71,    72,    73,    74,    75,    76,    77,    78,     0,   788,
       0,     0,     0,   788,   480,   595,   788,     0,     0,     0,
       0,   725,  1480,     0,   639,   639,  1487,     0,   159,     0,
     328,   329,    71,    72,    73,    74,    75,    76,    77,    78,
       0,   494,   494,   788,    80,     0,     0,   788,     0,   364,
      85,   788,     0,     0,     0,     0,     0,    81,     0,    82,
     725,  1514,   461,   725,  1518,     0,     0,   725,  1522,     0,
       0,     0,     0,     0,     0,     0,   433,     0,     0,   624,
      84,     0,     0,   625,   434,   435,   436,   437,     0,     0,
      87,   626,     0,  1835,  1835,     0,     0,     0,     0,     0,
     152,     0,    89,     0,     0,     0,     0,     0,     0,   305,
       0,     0,     0,     0,     0,     0,     0,     0,   152,     0,
       0,     0,     0,     0,     0,   159,     0,   184,   185,    71,
      72,    73,    74,    75,    76,    77,    78,     0,   127,     0,
     494,   127,     0,     0,   305,     0,     0,     0,     0,     0,
       0,     0,   152,   687,     0,     0,     0,     0,   494,     0,
       0,     0,     0,   788,     0,     0,  1223,     0,     0,     0,
       0,  1223,  1223,  1223,     0,     0,     0,   152,     0,     0,
     438,     0,     0,     0,     0,     0,     0,     0,     0,  1835,
       0,     0,     0,   305,     0,     0,     0,     0,   439,     0,
       0,     0,   127,   364,     0,   152,     0,     0,     0,     0,
     725,  1634,     0,  1008,     0,     0,     0,     0,   639,     0,
       0,   595,     0,   280,     0,     0,     0,     0,     0,     0,
       0,   127,     0,     0,     0,     0,     0,  1835,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   366,  2116,     0,
       0,   127,     0,   480,     0,     0,     0,  2064,   480,     0,
       0,  1835,   364,     0,     0,     0,     0,     0,     0,   480,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   687,
       0,   480,     0,     0,     0,   127,     0,     0,     0,   127,
       0,     0,     0,     0,     0,   127,     0,     0,   127,     0,
       0,     0,   366,     0,     0,     0,  1835,  1835,     0,     0,
       0,     0,     0,   446,   127,     0,   463,     0,   305,   305,
     305,   152,     0,   305,   305,     0,     0,     0,     0,     0,
       0,     0,   480,     0,     0,     0,     0,     0,     0,   280,
       0,     0,     0,   152,     0,     0,     0,   152,   152,    19,
       0,     0,     0,     0,     0,     0,     0,     0,   305,     0,
       0,   152,     0,  1223,     0,  1223,     0,  1835,     0,     0,
       0,   280,   280,   127,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   159,   280,   184,   185,
      71,    72,    73,    74,    75,    76,    77,    78,     0,    58,
      59,    60,    61,   480,     0,     0,     0,     0,   366,   127,
     159,   152,   184,   185,    71,    72,    73,    74,    75,    76,
      77,    78,     0,   305,     0,     0,     0,     0,   127,   494,
       0,     0,   127,     0,     0,     0,   280,     0,     0,   366,
       0,     0,   127,     0,   159,   817,   184,   185,    71,    72,
      73,    74,    75,    76,    77,    78,   936,   159,     0,   213,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,  1572,     0,     0,     0,   725,   127,     0,     0,
       0,     0,   305,     0,     0,   595,     0,     0,     0,     0,
       0,     0,     0,   725,   127,   127,   127,     0,     0,   937,
       0,     0,   687,     0,     0,     0,   127,     0,     0,    84,
       0,   366,  1025,   152,     0,   364,     0,   127,     0,   159,
       0,   328,   329,    71,    72,    73,    74,    75,    76,    77,
      78,     0,   779,     0,     0,   127,     0,     0,     0,     0,
     127,     0,   127,     0,     0,   366,   127,   280,    81,   366,
       0,     0,     0,   480,   480,   480,   366,     0,     0,     0,
     480,   480,     0,     0,   595,   127,   127,     0,     0,     0,
    1706,    84,     0,     0,     0,     0,     0,  1707,     0,     0,
       0,    87,    88,     0,     0,   280,   127,     0,   366,     0,
       0,     0,     0,    89,   788,  1565,     0,     0,     0,     0,
       0,     0,     0,   480,     0,     0,     0,  1579,     0,     0,
     159,     0,   725,   725,    71,    72,    73,    74,    75,    76,
      77,    78,  1220,   280,   280,     0,     0,  1221,   725,  1222,
     364,     0,     0,   480,     0,   480,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,   280,     0,     0,
     280,     0,   127,   127,   463,     0,   280,     0,     0,     0,
     152,     0,    84,     0,     0,   280,     0,     0,   127,   127,
     127,   280,     0,     0,     0,   305,     0,     0,     0,     0,
       0,     0,     0,   127,     0,     0,   916,   280,     0,     0,
       0,     0,   366,   127,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   480,     0,     0,    64,   725,     0,
       0,   494,     0,     0,     0,   159,   725,   776,   777,    71,
      72,    73,    74,    75,    76,    77,    78,   280,     0,     0,
     366,     0,     0,     0,     0,     0,     0,     0,   540,     0,
       0,   159,     0,   328,   329,    71,    72,    73,    74,    75,
      76,    77,    78,     0,   305,     0,   725,    80,     0,     0,
       0,   127,     0,     0,     0,     0,   287,     0,     0,    85,
      81,     0,    82,     0,     0,     0,   725,  2065,     0,     0,
     725,     0,     0,     0,     0,   280,   127,     0,     0,     0,
     366,   152,  2044,    84,     0,   779,   722,   127,     0,   127,
       0,   366,    64,    87,    88,   280,     0,   127,     0,     0,
       0,     0,   127,   127,     0,    89,     0,     0,     0,     0,
       0,   152,     0,     0,     0,   725,   725,     0,     0,     0,
       0,     0,     0,   280,     0,     0,   159,     0,   328,   329,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
       0,     0,    80,  1779,  1780,   152,     0,     0,     0,  1073,
       0,     0,     0,     0,     0,    81,     0,    82,   127,     0,
       0,     0,     0,     0,     0,     0,   494,     0,     0,   280,
     280,     0,     0,     0,   366,   280,   725,   420,    84,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    87,    88,
       0,     0,   127,     0,     0,     0,     0,     0,     0,     0,
      89,     0,     0,     0,     0,   287,     0,     0,     0,     0,
     159,   366,   328,   329,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,   366,     0,   159,     0,   774,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   127,    81,
     127,   159,     0,     0,   127,    71,    72,    73,    74,    75,
      76,    77,    78,  1220,     0,   127,   127,     0,  1221,     0,
    1222,   624,    84,     0,     0,   625,     0,     0,   127,   287,
       0,     0,    87,   626,     0,     0,     0,     0,   127,     0,
    1864,  1011,     0,   480,    89,   627,   280,  1874,     0,     0,
     494,     0,     0,    84,   127,     0,  1658,     0,   127,     0,
       0,     0,     0,     0,   540,   280,     0,   540,     0,     0,
       0,     0,     0,   540,     0,     0,     0,   127,  1899,     0,
       0,     0,   540,     0,     0,     0,     0,     0,   287,     0,
       0,     0,   127,     0,   127,     0,     0,     0,     0,   280,
       0,   280,     0,     0,     0,   916,     0,     0,     0,     0,
       0,     0,   540,     0,   159,     0,   328,   329,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,   321,     0,     0,
       0,     0,   159,    81,   184,   185,    71,    72,    73,    74,
      75,    76,    77,    78,   127,   127,     0,     0,   287,     0,
       0,     0,     0,  1937,  1938,  2044,    84,     0,     0,   722,
    1948,     0,     0,   127,     0,   966,    87,    88,   287,     0,
     127,     0,     0,  1962,     0,     0,     0,     0,    89,     0,
       0,   127,   916,  1971,   480,  1972,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1986,   127,
    1988,  1989,  1990,     0,     0,   223,   287,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   127,
     159,     0,   328,   329,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,     0,     0,    80,     0,  2011,     0,
       0,     0,  2016,     0,     0,     0,     0,  2021,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,   558,     0,
       0,    85,   479,     0,     0,     0,     0,     0,   127,     0,
       0,   330,    84,     0,     0,     0,     0,     0,     0,     0,
     127,     0,    87,    88,   280,     0,  1082,  1084,     0,   127,
       0,     0,   127,     0,    89,     0,  2067,     0,     0,   199,
       0,     0,   366,   127,     0,     0,     0,   280,  2076,     0,
     287,     0,  2079,     0,     0,     0,     0,     0,     0,  1419,
       0,     0,     0,     0,     0,     0,     0,  2095,     0,   159,
       0,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   362,     0,   127,     0,     0,   480,   127,     0,     0,
       0,     0,     0,   384,   127,   391,     0,   393,   159,     0,
     127,     0,    71,    72,    73,    74,    75,    76,    77,    78,
    1220,     0,  2123,     0,     0,  1221,     0,  1222,     0,     0,
     287,     0,   287,    85,     0,    64,     0,     0,     0,     0,
     480,     0,   127,   127,     0,     0,   362,     0,     0,   391,
     393,     0,     0,     0,     0,     0,     0,     0,     0,  2144,
      84,     0,     0,  1660,  2147,     0,     0,     0,     0,   159,
       0,   328,   329,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,   110,    80,     0,   165,     0,   127,
    2165,     0,     0,  2167,     0,  2147,     0,     0,    81,   480,
      82,     0,   127,     0,     0,   525,     0,     0,     0,     0,
       0,     0,     0,     0,  2167,   127,     0,     0,   287,   127,
    1706,    84,     0,   127,     0,   480,     0,   480,     0,     0,
       0,    87,    88,     0,     0,  1531,     0,     0,     0,     0,
       0,     0,     0,    89,     0,     0,   127,     0,   110,     0,
       0,     0,   362,     0,     0,   127,     0,     0,   656,     0,
     393,     0,     0,     0,   280,   480,     0,     0,     0,   271,
       0,     0,     0,     0,     0,     0,     0,   302,     0,     0,
       0,     0,     0,   362,     0,     0,     0,     0,     0,     0,
     280,   280,     0,     0,     0,     0,     0,   374,   280,   159,
       0,   184,   185,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   366,   127,     0,     0,     0,     0,     0,     0,
     480,   408,     0,     0,     0,   412,     0,     0,     0,   127,
       0,   110,     0,     0,     0,     0,     0,     0,     0,     0,
     623,   127,     0,     0,     0,   362,     0,   391,   393,     0,
     451,     0,   159,     0,   328,   329,    71,    72,    73,    74,
      75,    76,    77,    78,     0,   287,     0,     0,     0,     0,
     127,   127,     0,     0,     0,     0,     0,   496,     0,   362,
       0,    81,     0,   362,     0,     0,     0,     0,     0,     0,
     362,     0,     0,     0,   127,     0,     0,     0,   127,     0,
       0,   127,     0,   420,    84,     0,     0,     0,   549,   555,
       0,     0,     0,     0,    87,    88,     0,     0,     0,     0,
       0,     0,   362,   593,   656,   393,    89,   287,   127,     0,
       0,     0,   127,     0,     0,     0,   127,     0,     0,     0,
       0,     0,     0,     0,     0,   653,     0,     0,     0,     0,
       0,     0,     0,     0,   127,   127,   127,   127,   127,   127,
     127,     0,     0,     0,   664,     0,   366,   362,   664,     0,
       0,     0,   271,     0,     0,     0,     0,     0,   555,   280,
       0,     0,   287,   280,   280,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   280,     0,     0,   280,     0,     0,
       0,     0,   287,     0,   716,     0,     0,     0,     0,     0,
       0,     0,     0,   408,     0,     0,     0,     0,     0,     0,
     362,     0,     0,     0,   740,   366,   362,     0,     0,   745,
     747,   271,   302,     0,    14,    15,    16,    17,    18,     0,
    1795,     0,   408,     0,     0,     0,     0,   127,   127,     0,
       0,     0,     0,   408,     0,     0,   770,     0,     0,   127,
     772,     0,     0,  1330,   362,   773,   656,   393,     0,     0,
       0,   784,     0,     0,     0,   287,   747,     0,   408,     0,
       0,     0,   797,   451,     0,     0,     0,     0,  1527,     0,
       0,     0,     0,     0,   810,     0,     0,     0,     0,     0,
      64,   271,     0,   159,     0,   184,   185,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,     0,   280,     0,
       0,   549,   593,     0,   362,   656,     0,     0,     0,     0,
       0,     0,     0,     0,   159,   362,   328,   329,    71,    72,
      73,    74,    75,    76,    77,    78,     0,   540,   366,   127,
      80,     0,     0,   525,   673,     0,     0,     0,     0,   271,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   287,   287,     0,     0,     0,
       0,     0,     0,     0,     0,   420,    84,   362,     0,     0,
       0,     0,     0,     0,     0,     0,    87,    88,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   451,    89,     0,
       0,   362,     0,     0,     0,     0,   362,     0,   362,   664,
       0,     0,   287,   918,     0,     0,     0,     0,     0,   555,
       0,     0,     0,     0,     0,   287,     0,     0,     0,     0,
       0,   362,     0,   362,   362,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,     0,     0,   549,     0,     0,     0,     0,   362,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   366,     0,
     159,   362,   184,   185,    71,    72,    73,    74,    75,    76,
      77,    78,     0,   280,     0,     0,   280,   784,     0,     0,
       0,   982,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   127,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1005,   593,     0,   287,     0,     0,     0,  1010,     0,
       0,     0,     0,   271,     0,   271,     0,     0,     0,     0,
       0,   451,     0,   664,     0,     0,  1725,  1733,   555,   451,
    1725,  1744,     0,     0,     0,     0,  1751,     0,     0,     0,
    1755,     0,  1757,     0,  1744,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2029,   366,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
     127,     0,     0,   366,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   784,     0,     0,     0,     0,     0,
       0,     0,     0,   287,   287,     0,     0,     0,     0,     0,
       0,   549,     0,     0,     0,     0,     0,   127,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   408,     0,
       0,     0,     0,  2090,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,   127,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   362,     0,     0,   127,
       0,   362,     0,     0,   784,     0,   110,     0,     0,     0,
    1125,   127,     0,     0,     0,     0,     0,   121,     0,     0,
       0,   664,   784,     0,  1137,     0,     0,     0,     0,     0,
       0,  1860,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   784,  1160,     0,     0,     0,     0,
       0,  1880,  1882,     0,     0,     0,     0,     0,     0,     0,
     784,     0,     0,     0,   784,     0,     0,     0,     0,     0,
       0,   451,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   121,  1902,   555,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   271,     0,
     664,     0,   275,     0,     0,  1005,     0,   593,     0,     0,
       0,     0,     0,     0,     0,     0,   362,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   362,     0,
     375,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,     0,     0,     0,
     271,   784,     0,     0,   121,     0,     0,  1946,     0,     0,
       0,     0,  1248,     0,   121,  1949,     0,  1951,     0,   784,
    1955,  1961,     0,  1744,     0,     0,   784,     0,  1967,     0,
       0,     0,     0,   452,     0,     0,     0,   664,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   271,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   271,     0,     0,     0,   362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   275,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2027,     0,     0,     0,     0,   594,     0,  2034,  2036,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1125,     0,     0,     0,   375,     0,
       0,  2058,     0,     0,     0,     0,   784,     0,     0,     0,
    1395,     0,     0,     0,     0,   451,     0,   594,   271,     0,
       0,   594,     0,     0,     0,   275,     0,     0,     0,  1125,
       0,     0,  2080,     0,  2083,     0,     0,  2085,  2087,     0,
       0,     0,     0,     0,  2092,  2094,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,   784,
       0,     0,     0,   784,     0,     0,     0,     0,     0,     0,
     784,     0,     0,     0,   275,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   121,   362,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,   771,
       0,  2130,  2132,  2134,     0,     0,     0,     0,   271,   271,
       0,     0,     0,     0,   452,     0,     0,     0,     0,     0,
       0,   121,     0,     0,     0,   375,   275,     0,     0,  2153,
    2155,  2157,     0,     0,     0,     0,     0,     0,     0,     0,
     745,     0,     0,     0,   452,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   271,     0,     0,     0,     0,
       0,     0,     0,     0,   275,   594,     0,     0,   271,     0,
       0,    14,    15,    16,    17,    18,     0,     0,     0,     0,
       0,   784,     0,     0,     0,   784,     0,     0,     0,   784,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   275,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1125,     0,     0,     0,     0,     0,     0,     0,
       0,  1549,     0,     0,     0,     0,     0,     0,     0,     0,
    1395,     0,     0,     0,     0,     0,     0,    64,     0,     0,
     362,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     275,     0,     0,     0,     0,     0,     0,   271,     0,     0,
       0,     0,   594,     0,  1395,     0,   375,   126,     0,     0,
       0,   159,     0,   328,   329,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,    80,     0,  1608,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
      81,     0,    82,     0,     0,     0,   275,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1125,     0,     0,
       0,     0,  1706,    84,     0,     0,     0,     0,     0,     0,
       0,   126,     0,    87,    88,     0,     0,     0,     0,     0,
     452,     0,     0,     0,     0,    89,   271,   271,     0,     0,
       0,     0,   279,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   275,   594,     0,     0,     0,     0,
     784,     0,     0,     0,   784,     0,   452,   784,   452,     0,
     377,     0,     0,     0,   275,     0,   594,     0,     0,     0,
       0,     0,   275,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   784,     0,     0,     0,   784,     0,
       0,     0,   784,     0,   126,     0,     0,     0,     0,     0,
       0,     0,   362,     0,   126,     0,     0,     0,     0,     0,
    1549,  1549,  1549,   165,   747,     0,     0,     0,     0,     0,
       0,     0,     0,   454,     0,     0,     0,   452,     0,     0,
       0,     0,     0,     0,     0,  1731,     0,     0,     0,  1731,
    1731,     0,     0,     0,   275,     0,     0,     0,     0,     0,
       0,     0,     0,  1731,     0,     0,     0,     0,     0,     0,
       0,   121,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   279,     0,     0,     0,     0,     0,     0,     0,   132,
       0,     0,   132,     0,     0,     0,   596,     0,     0,     0,
       0,     0,     0,  1125,   784,     0,     0,   452,     0,   121,
       0,     0,     0,   594,     0,     0,     0,     0,   377,     0,
     362,     0,     0,     0,   594,   452,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,     0,   596,     0,     0,
       0,   596,     0,     0,     0,   279,     0,   452,     0,     0,
       0,     0,     0,   132,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   452,     0,     0,     0,   452,     0,     0,
       0,     0,     0,     0,   275,     0,     0,     0,     0,     0,
       0,  1849,   132,     0,     0,     0,   126,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   362,     0,
       0,   452,   132,   594,   279,  1861,     0,     0,   275,     0,
     594,   390,     0,     0,     0,   126,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   126,     0,     0,   362,
     393,     0,     0,     0,     0,     0,   132,     0,     0,     0,
     132,     0,     0,     0,   454,     0,   132,   362,     0,   132,
       0,   126,     0,     0,     0,   377,   279,     0,     0,     0,
       0,     0,     0,   452,   452,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   454,     0,     0,     0,     0,     0,
       0,     0,   452,     0,     0,     0,     0,     0,     0,   452,
     132,     0,     0,     0,   279,   596,     0,     0,     0,     0,
     594,     0,     0,     0,     0,  1927,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   452,     0,
       0,     0,   132,     0,   132,     0,     0,     0,     0,     0,
       0,     0,   279,     0,     0,     0,     0,     0,   452,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1958,
       0,     0,  1731,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1976,     0,     0,
       0,     0,     0,     0,     0,     0,   362,     0,     0,     0,
     279,     0,     0,     0,     0,     0,     0,   594,     0,     0,
       0,     0,   596,   132,     0,     0,   377,     0,     0,   452,
       0,     0,     0,   121,     0,     0,     0,     0,   452,     0,
       0,   452,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   594,     0,     0,     0,     0,     0,   132,     0,
       0,     0,     0,     0,     0,     0,   279,     0,     0,     0,
       0,     0,     0,     0,     0,   132,  1976,   132,     0,     0,
       0,     0,     0,   132,     0,     0,     0,   132,     0,     0,
       0,     0,   452,     0,     0,     0,   452,     0,   132,     0,
     454,     0,     0,   452,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1731,     0,     0,     0,     0,     0,     0,
       0,   132,     0,   132,   279,   596,     0,   132,     0,     0,
       0,     0,     0,     0,     0,     0,   454,     0,   454,     0,
       0,   452,   452,  1731,   279,     0,   596,   132,  2105,     0,
       0,     0,   279,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   784,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1731,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   452,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   452,     0,     0,     0,   132,     0,   454,     0,     0,
       0,     0,     0,     0,   452,     0,     0,     0,   452,     0,
       0,     0,   452,     0,   279,     0,     0,     0,   132,     0,
       0,   132,     0,   132,   132,     0,     0,   132,     0,     0,
       0,   126,     0,     0,     0,   594,   132,     0,     0,   132,
     132,   132,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   132,     0,   131,     0,     0,   131,
       0,     0,     0,     0,     0,     0,     0,   454,     0,   126,
     275,     0,     0,   596,     0,     0,     0,   121,     0,     0,
       0,     0,     0,     0,   596,   454,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,     0,     0,     0,     0,   454,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     131,     0,     0,   454,     0,     0,     0,   454,     0,     0,
     594,     0,     0,     0,   279,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   131,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   452,
     452,   454,     0,   596,     0,     0,     0,     0,   279,   131,
     596,     0,     0,   132,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   452,     0,     0,     0,   452,     0,     0,
     452,     0,     0,     0,   132,     0,     0,     0,     0,     0,
       0,     0,     0,   131,     0,     0,     0,   131,     0,     0,
       0,     0,     0,   131,     0,     0,   131,   452,     0,     0,
       0,   452,     0,   454,   454,   452,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     132,   132,   454,     0,     0,     0,     0,     0,     0,   454,
       0,     0,     0,     0,     0,     0,     0,   131,     0,     0,
     596,     0,     0,   132,     0,     0,     0,     0,   121,     0,
       0,     0,   121,   121,     0,     0,     0,     0,   454,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,   131,
       0,   131,     0,     0,     0,     0,     0,     0,   454,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   132,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   594,   452,     0,   132,
       0,     0,     0,     0,     0,     0,     0,   596,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   132,     0,   454,
     131,     0,     0,   126,     0,     0,     0,     0,   454,     0,
       0,   454,     0,     0,     0,     0,     0,     0,     0,     0,
    2002,     0,   596,     0,     0,     0,     0,     0,   132,     0,
       0,     0,     0,     0,     0,   131,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   131,     0,   131,     0,     0,     0,     0,     0,
     131,     0,   454,     0,   131,     0,   454,     0,     0,     0,
       0,     0,     0,   454,   215,   131,     0,   216,   375,   217,
     218,     0,   219,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   131,   221,
     131,     0,     0,     0,   131,     0,     0,     0,     0,     0,
       0,   454,   454,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   131,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,     0,   229,
     230,   231,     0,   232,   233,     0,     0,     0,   454,     0,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   454,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   131,   234,   454,     0,    85,   479,   454,     0,
       0,     0,   454,   237,    87,    88,   239,   240,   241,   242,
       0,     0,     0,     0,     0,   131,     0,     0,   131,     0,
     131,   131,     0,     0,   131,   596,     0,     0,     0,     0,
       0,     0,     0,   131,     0,   121,   131,   131,   131,     0,
       0,     0,     0,   126,     0,     0,     0,     0,     0,     0,
     594,     0,     0,     0,     0,   132,     0,     0,     0,     0,
       0,   131,     0,     0,     0,     0,     0,     0,     0,     0,
     279,     0,     0,     0,     0,     0,     0,   126,   132,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   136,     0,     0,   136,     0,     0,     0,
       0,     0,   377,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   594,
     596,   132,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   102,
       0,     0,   102,     0,     0,     0,     0,   136,     0,   454,
     454,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     131,     0,     0,   454,     0,     0,   136,   454,     0,     0,
     454,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,   131,     0,     0,     0,     0,   136,     0,     0,     0,
       0,     0,     0,   102,     0,     0,     0,   454,   452,     0,
       0,   454,     0,     0,     0,   454,     0,     0,     0,     0,
     121,     0,     0,     0,   268,     0,     0,     0,     0,     0,
     136,     0,     0,     0,   136,     0,     0,   131,   131,     0,
     136,     0,     0,   136,     0,     0,     0,     0,   361,     0,
       0,     0,   102,     0,     0,     0,     0,     0,   126,     0,
     131,     0,   126,   126,     0,     0,   132,     0,     0,     0,
       0,     0,     0,     0,     0,   132,   126,     0,     0,     0,
       0,     0,     0,     0,   136,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   102,     0,     0,     0,
       0,   132,     0,   430,     0,     0,     0,     0,   131,   132,
       0,     0,     0,     0,     0,     0,   136,     0,   136,     0,
       0,     0,     0,     0,     0,     0,   596,   454,     0,     0,
       0,     0,     0,     0,   132,     0,   131,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     132,     0,     0,     0,   131,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   268,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,   131,    20,   136,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,   136,    52,     0,    53,     0,     0,   377,     0,
       0,     0,     0,     0,     0,     0,     0,   268,     0,   136,
       0,   136,     0,     0,     0,    64,     0,   136,     0,     0,
       0,   136,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   136,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   132,   132,   132,   132,   132,
     132,   132,     0,     0,     0,   136,     0,   136,     0,     0,
       0,   136,     0,     0,     0,    80,   268,     0,     0,     0,
     132,     0,     0,     0,   132,   132,     0,     0,     0,     0,
      82,   136,   361,     0,     0,   132,     0,     0,   132,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   268,     0,     0,   136,
       0,     0,  1959,     0,     0,   126,     0,     0,     0,   181,
     132,     0,     0,     0,     0,     0,   268,     0,     0,     0,
     596,     0,   136,     0,     0,   136,     0,   136,   136,     0,
       0,   136,   131,     0,     0,     0,     0,     0,     0,     0,
     136,     0,     0,   136,   136,   136,     0,     0,     0,     0,
       0,     0,     0,     0,   268,   131,     0,     0,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   136,   132,
       0,   181,     0,   181,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   596,
     132,     0,     0,     0,   181,     0,   466,     0,   131,     0,
       0,     0,     0,     0,   215,     0,     0,   216,     0,   217,
     218,     0,   219,     0,     0,     0,     0,     0,     0,     0,
       0,   466,     0,     0,     0,     0,   126,     0,     0,   221,
       0,     0,     0,     0,     0,     0,   181,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   268,     0,
       0,     0,     0,     0,     0,     0,   126,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,   136,   454,   229,
     230,   231,     0,   232,   233,     0,     0,     0,     0,     0,
     126,    81,     0,     0,     0,     0,     0,     0,   136,   181,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   234,     0,     0,    85,   479,   268,     0,
     268,     0,     0,   237,    87,    88,   239,   240,   241,   242,
       0,     0,     0,     0,     0,     0,   931,   132,     0,     0,
       0,     0,     0,   131,   136,   136,   181,     0,     0,     0,
     181,     0,   131,   181,   181,     0,     0,   181,     0,     0,
     181,   181,     0,   181,     0,   181,     0,   136,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   131,   179,
       0,     0,     0,     0,     0,     0,   131,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   268,     0,     0,     0,
       0,   131,     0,     0,     0,   136,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   131,     0,     0,
       0,     0,     0,     0,     0,     0,   181,     0,     0,   181,
       0,     0,     0,   136,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   395,     0,     0,
       0,   136,     0,     0,     0,     0,     0,     0,   132,     0,
     401,   102,   402,     0,     0,   102,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   136,     0,     0,     0,     0,     0,   132,     0,
       0,     0,     0,   458,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   132,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   517,     0,     0,     0,     0,
       0,     0,   131,   131,   131,   131,   131,   131,   131,     0,
       0,     0,     0,   268,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   131,     0,     0,
       0,   131,   131,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   131,     0,     0,   131,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   658,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   268,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,     0,     0,   181,   181,
       0,   181,     0,   181,   181,     0,     0,   131,   181,   181,
       0,     0,   727,   728,     0,     0,   732,     0,     0,   735,
     736,     0,   738,     0,   739,     0,     0,     0,     0,     0,
     268,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     268,     0,     0,     0,     0,     0,     0,     0,     0,   136,
       0,     0,     0,     0,     0,     0,   131,     0,   187,   190,
     466,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   136,     0,     0,     0,   181,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   131,     0,   102,
       0,     0,     0,     0,     0,     0,     0,     0,   322,     0,
       0,     0,     0,     0,     0,   102,     0,     0,     0,     0,
       0,     0,     0,   268,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   430,   102,   136,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   415,
       0,     0,   416,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   466,     0,     0,   440,     0,     0,
     868,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   474,     0,     0,
     181,     0,   181,     0,     0,     0,     0,     0,     0,   474,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,   181,   268,   268,     0,     0,     0,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   131,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     268,     0,     0,     0,   322,     0,     0,     0,     0,     0,
     136,     0,     0,   268,     0,     0,     0,     0,     0,   136,
       0,     0,     0,     0,     0,     0,     0,     0,   667,     0,
       0,     0,   674,     0,   987,     0,     0,   990,   991,     0,
     994,     0,   996,   997,     0,   136,     0,   999,  1000,     0,
       0,     0,     0,   136,     0,   697,     0,   102,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   102,   181,     0,   136,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   136,     0,     0,   322,     0,     0,
       0,     0,   268,     0,     0,   131,   758,   759,     0,   102,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,  1068,     0,     0,     0,     0,
       0,     0,   187,   361,   102,   131,     0,     0,     0,   265,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     802,   181,     0,     0,     0,   181,     0,     0,     0,   181,
       0,     0,   102,     0,     0,     0,     0,     0,     0,   131,
       0,     0,     0,     0,   816,   818,     0,     0,     0,   825,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   268,   268,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   440,     0,     0,   440,     0,     0,   474,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   136,
     136,   136,   136,   136,   136,   136,     0,     0,     0,     0,
       0,     0,   181,     0,     0,     0,     0,     0,     0,  1138,
       0,  1140,     0,     0,   136,     0,     0,     0,   136,   136,
       0,     0,     0,     0,     0,   488,     0,     0,     0,   136,
    1162,  1163,   136,     0,     0,     0,     0,  1167,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   102,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   548,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     102,     0,     0,     0,   102,   102,     0,   938,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   102,     0,
       0,     0,     0,     0,   136,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1564,     0,   181,     0,     0,   181,
     181,     0,     0,     0,     0,     0,   361,   181,   181,     0,
       0,     0,   265,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   688,   102,   688,
     181,   215,   181,     0,   216,   181,   217,   218,   181,   219,
       0,     0,   181,   136,     0,  1250,     0,     0,  1009,     0,
       0,     0,     0,     0,  1336,     0,   221,  1338,     0,  1339,
    1939,  1940,  1340,  1341,  1342,  1343,  1344,  1345,  1346,  1347,
    1348,  1349,  1350,  1351,   136,     0,  1352,  1353,  1354,  1355,
    1356,  1357,  1358,     0,  1359,     0,   222,   223,     0,   682,
     225,  1360,  1361,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,  1362,     0,   229,   230,   231,     0,
     232,   233,     0,     0,     0,     0,     0,     0,    81,     0,
    1289,     0,     0,   799,  1293,     0,     0,     0,  1297,   361,
     102,   807,     0,     0,     0,     0,     0,     0,     0,     0,
    1363,     0,     0,    85,   479,     0,     0,     0,   399,     0,
     237,    87,    88,   239,   240,   241,   242,     0,     0,     0,
       0,   265,     0,     0,     0,  -196,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,     0,     0,     0,     0,
       0,     0,   848,     0,     0,     0,     0,  1113,     0,   548,
       0,  1250,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1119,     0,     0,     0,     0,     0,     0,     0,     0,
     440,   136,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,     0,   884,     0,     0,     0,     0,     0,
       0,   474,     0,     0,     0,     0,     0,   548,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   361,
       0,     0,   915,   917,     0,   265,     0,     0,     0,     0,
       0,     0,     0,     0,   924,     0,   926,   102,     0,     0,
       0,     0,     0,   935,     0,   940,   935,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     944,     0,     0,   548,     0,  1478,     0,     0,  1482,  1485,
       0,     0,     0,     0,     0,     0,  1494,  1495,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   265,  1507,
     981,  1509,     0,     0,  1512,   181,     0,  1516,     0,     0,
       0,  1520,   136,     0,     0,     0,     0,     0,     0,     0,
       0,   265,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   265,   136,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   799,     0,     0,     0,   848,   688,     0,
       0,     0,     0,     0,   688,     0,     0,     0,   102,   488,
       0,     0,     0,     0,     0,     0,   136,     0,     0,     0,
    1264,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   296,   181,  1071,     0,     0,   102,     0,
       0,   308,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   370,     0,     0,
       0,   265,   181,     0,     0,     0,     0,     0,   181,     0,
       0,     0,   102,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1632,     0,     0,     0,     0,   414,
       0,     0,  1333,     0,     0,     0,     0,     0,   308,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   457,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1482,  1422,     0,   181,     0,     0,     0,     0,   308,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   935,
       0,     0,     0,     0,   181,   181,     0,     0,     0,     0,
       0,     0,  1164,     0,     0,     0,     0,     0,     0,     0,
       0,   544,   296,     0,     0,     0,     0,     0,     0,     0,
       0,   265,     0,     0,   181,   181,     0,   599,     0,     0,
       0,     0,   466,     0,     0,     0,     0,   181,     0,     0,
       0,  1192,     0,     0,     0,     0,  1199,     0,   652,     0,
       0,     0,     0,     0,  1199,   548,     0,     0,     0,     0,
       0,  1214,     0,     0,     0,     0,   935,     0,   666,     0,
       0,     0,   672,     0,     0,     0,   296,     0,     0,   679,
       0,     0,     0,     0,  1773,     0,   688,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   296,   308,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   915,     0,
       0,   370,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1569,
    1571,  1573,     0,  1821,     0,   688,     0,     0,     0,     0,
       0,   181,     0,     0,     0,     0,  1278,   457,     0,   801,
       0,     0,     0,     0,     0,     0,   679,     0,     0,     0,
       0,     0,     0,  1595,     0,   296,   308,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1333,   296,   599,     0,   832,  1612,
     181,     0,     0,  1613,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   296,   308,     0,     0,     0,     0,     0,
       0,     0,     0,   488,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1908,     0,     0,     0,   308,     0,     0,
     544,     0,   544,   308,     0,     0,   308,     0,     0,     0,
       0,     0,     0,  1917,  1918,   544,     0,     0,   544,   544,
     544,   457,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   909,     0,     0,   652,     0,     0,     0,
       0,     0,     0,  1931,  1932,     0,     0,     0,     0,   688,
       0,     0,     0,     0,     0,     0,  1936,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   296,     0,     0,
     945,     0,   935,     0,     0,  1199,     0,     0,  1718,  1719,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1492,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   688,     0,     0,     0,     0,
       0,     0,     0,     0,  1999,   457,   599,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   296,     0,   296,
       0,     0,     0,     0,     0,   457,     0,  1021,     0,  1528,
    1796,  1529,     0,   457,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   308,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2063,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1582,  1582,     0,     0,     0,     0,     0,   548,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   308,
     308,     0,     0,     0,     0,   296,     0,     0,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,  1857,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,     0,     0,
       0,   688,     0,  1647,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,   909,     0,     0,     0,     0,
       0,     0,  1662,     0,     0,     0,     0,     0,   308,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1670,  1671,     0,     0,     0,   308,   522,     0,   523,
     524,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,   457,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   296,     0,  1208,     0,     0,     0,     0,   457,
       0,   599,     0,     0,     0,   -17,     0,     0,   935,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   688,     0,     0,
     488,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   296,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   909,   832,  1803,     0,     0,  1805,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   296,
       0,     0,     0,     0,     0,     0,     0,  1647,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   296,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1825,     0,     0,   195,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,   488,     0,     0,    19,     0,    20,
    1848,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,   352,   353,     0,   354,    52,     0,    53,   457,
       0,   355,   296,     0,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,   308,     0,     0,
       0,  1895,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     688,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     544,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   296,   296,     0,  -479,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -479,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1121,     0,     0,     0,     0,     0,     0,   296,
       0,     0,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,   296,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -503,  -503,     0,  -503,    52,     0,
      53,     0,     0,  -503,     0,   220,    55,    56,    57,    58,
      59,    60,    61,     0,     0,   544,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   935,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     544,   296,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,  2166,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   370,    81,     0,    82,     0,     0,  1564,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   308,
       0,     0,     0,     0,  1318,     0,     0,     0,    85,    86,
       0,     0,     0,     0,     0,     0,    87,    88,     0,     0,
       0,     0,     0,     0,     0,   215,     0,     0,   216,     0,
     217,   218,     0,   219,     0,     0,     0,     0,     0,     0,
     296,   296,     0,     0,     0,     0,     0,     0,  1336,     0,
     221,  1338,     0,  1339,  -256,  -256,  1340,  1341,  1342,  1343,
    1344,  1345,  1346,  1347,  1348,  1349,  1350,  1351,  -355,  -355,
    1352,  1353,  1354,  1355,  1356,  1357,  1358,     0,  1359,     0,
     222,   223,     0,   682,   225,  1360,  1361,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,  1362,     0,
     229,   230,   231,     0,   232,   233,     0,     0,     0,     0,
       0,     0,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2166,   544,   544,   544,     0,     0,   544,
     544,     0,     0,  -256,  1363,     0,   679,    85,   479,     0,
    1564,     0,   399,     0,   237,    87,    88,   239,   240,   241,
     242,     0,     0,     0,     0,     0,     0,     0,     0,  -196,
       0,     0,     0,     0,   308,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   215,     0,     0,
     216,     0,   217,   218,     0,   219,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,     0,     0,
    1336,     0,   221,  1338,     0,  1339,  -257,  -257,  1340,  1341,
    1342,  1343,  1344,  1345,  1346,  1347,  1348,  1349,  1350,  1351,
    -355,  -355,  1352,  1353,  1354,  1355,  1356,  1357,  1358,   544,
    1359,     0,   222,   223,     0,   682,   225,  1360,  1361,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
    1362,     0,   229,   230,   231,  1898,   232,   233,     0,     0,
       0,     0,     0,     0,    81,     0,     0,     0,     0,     0,
       0,     0,  1564,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -257,  1363,     0,   308,    85,
     479,     0,     0,     0,   399,     0,   237,    87,    88,   239,
     240,   241,   242,     0,     0,     0,     0,     0,     0,   215,
       0,  -196,   216,     0,   217,   218,     0,   219,   370,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1336,     0,   221,  1338,     0,  1339,     0,     0,
    1340,  1341,  1342,  1343,  1344,  1345,  1346,  1347,  1348,  1349,
    1350,  1351,  -355,  -355,  1352,  1353,  1354,  1355,  1356,  1357,
    1358,     0,  1359,     0,   222,   223,     0,   682,   225,  1360,
    1361,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,  1362,     0,   229,   230,   231,     0,   232,   233,
       0,     0,     0,     0,     0,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1363,     0,
       0,    85,   479,     0,     0,     0,   399,     0,   237,    87,
      88,   239,   240,   241,   242,     0,     0,     0,     0,     0,
       0,     0,     0,  -196,     0,     0,     0,     0,   370,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   544,     0,     0,     0,     0,     0,     0,     0,     4,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,  1335,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   215,     0,    52,   216,    53,   217,   218,     0,   219,
      54,    55,    56,    57,    58,    59,    60,    61,    62,     0,
     544,     0,    63,   679,  1336,    64,  1337,  1338,     0,  1339,
       0,     0,  1340,  1341,  1342,  1343,  1344,  1345,  1346,  1347,
    1348,  1349,  1350,  1351,  -355,  -355,  1352,  1353,  1354,  1355,
    1356,  1357,  1358,     0,  1359,     0,   222,   223,    67,   682,
     225,  1360,  1361,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,  1362,    80,   229,   230,   231,     0,
     232,   233,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -3,
    1363,     0,     0,    85,  1364,     0,     0,     0,   399,     0,
     237,    87,    88,   239,   240,   241,   242,     0,     0,     0,
       0,     0,     0,     0,     0,  -196,     4,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,  1335,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   215,     0,
      52,   216,    53,   217,   218,     0,   219,    54,    55,    56,
      57,    58,    59,    60,    61,    62,     0,     0,     0,    63,
       0,  1336,    64,  1337,  1338,     0,  1339,     0,     0,  1340,
    1341,  1342,  1343,  1344,  1345,  1346,  1347,  1348,  1349,  1350,
    1351,  -355,  -355,  1352,  1353,  1354,  1355,  1356,  1357,  1358,
       0,  1359,     0,   222,   223,    67,   682,   225,  1360,  1361,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,  1362,    80,   229,   230,   231,     0,   232,   233,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1363,     0,     0,
      85,  1364,     0,     0,     0,   399,     0,   237,    87,    88,
     239,   240,   241,   242,     0,     0,     0,     0,     0,     0,
       0,     0,  -196,     4,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   215,     0,    52,   216,    53,
     217,   218,     0,   219,    54,    55,    56,    57,    58,    59,
      60,    61,    62,     0,     0,     0,    63,     0,     0,    64,
     221,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     222,   223,    67,   224,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,     0,    80,
     229,   230,   231,     0,   232,   233,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1737,  1738,  1739,
    1740,     0,     0,     0,   234,  1741,  1742,    85,  1364,     0,
       0,     0,     0,     0,   237,    87,    88,   239,   240,   241,
     242,     0,     0,     0,     0,     0,     0,     0,     0,  1743,
       4,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   215,     0,    52,   216,    53,   217,   218,     0,
     219,    54,    55,    56,    57,    58,    59,    60,    61,    62,
       0,     0,     0,    63,     0,     0,    64,   221,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   222,   223,    67,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,    80,   229,   230,   231,
       0,   232,   233,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1737,  1738,  1739,  1740,     0,     0,
       0,   234,  1741,     0,    85,  1364,     0,     0,     0,     0,
       0,   237,    87,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,     0,  1743,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,    62,     0,     0,     0,
      63,     0,     0,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    66,     0,     0,     0,    67,    68,     0,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,    79,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    83,    84,
       0,    85,    86,     0,     0,     0,     0,     0,     0,    87,
      88,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    89,     0,    90,   357,   195,     6,     7,     8,     9,
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
       0,     0,     0,     0,   159,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,     0,    85,   358,
       0,     0,     0,  -839,     0,     0,    87,    88,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    89,   357,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,   159,
       0,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      83,    84,     0,    85,   358,     0,     0,     0,     0,     0,
       0,    87,    88,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    89,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   220,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   159,     0,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,   782,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   968,    84,  -704,    85,   625,     0,
       0,     0,     0,     0,     0,    87,    88,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    89,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,  -504,  -504,     0,
    -504,    52,     0,    53,     0,     0,  -504,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   159,     0,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    83,    84,
       0,    85,   358,     0,     0,     0,  -843,     0,     0,    87,
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
       0,     0,    83,    84,     0,    85,   358,     0,     0,     0,
       0,     0,     0,    87,    88,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    89,     4,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   215,     0,
      52,   216,    53,   217,   218,     0,   219,    54,    55,    56,
      57,    58,    59,    60,    61,    62,     0,     0,     0,    63,
       0,     0,    64,   221,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   222,   223,    67,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,    80,   229,   230,   231,     0,   232,   233,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   234,     0,  1735,
      85,  1364,     0,     0,     0,     0,     0,   237,    87,    88,
     239,   240,   241,   242,     4,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   215,     0,    52,   216,
      53,   217,   218,     0,   219,    54,    55,    56,    57,    58,
      59,    60,    61,    62,     0,     0,     0,    63,     0,     0,
      64,   221,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   222,   223,    67,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
      80,   229,   230,   231,     0,   232,   233,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   234,     0,     0,    85,  1364,
       0,     0,     0,     0,     0,   237,    87,    88,   239,   240,
     241,   242,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   215,     0,    52,   216,    53,   217,   218,
       0,   219,   220,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   221,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   222,   223,
       0,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,    80,   229,   230,
     231,     0,   232,   233,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   234,   545,     0,    85,   235,   546,   547,     0,
       0,     0,   237,   238,    88,   239,   240,   241,   242,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     215,     0,    52,   216,    53,   217,   218,     0,   219,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,   221,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,    80,   229,   230,   231,     0,   232,
     233,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   234,
     545,     0,    85,   235,   675,   547,     0,     0,     0,   237,
     238,    88,   239,   240,   241,   242,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   215,     0,    52,
     216,    53,   217,   218,     0,   219,   220,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   221,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,    80,   229,   230,   231,     0,   232,   233,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   234,   545,     0,    85,
     590,   890,   547,     0,     0,     0,   237,   238,    88,   239,
     240,   241,   242,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   215,     0,    52,   216,    53,   217,
     218,     0,   219,   220,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   221,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,    80,   229,
     230,   231,     0,   232,   233,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   234,   545,     0,    85,   235,   828,   547,
       0,     0,     0,   237,   238,    88,   239,   240,   241,   242,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   215,     0,    52,   216,    53,   217,   218,     0,   219,
     220,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   221,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,    80,   229,   230,   231,     0,
     232,   233,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     234,   545,     0,    85,   590,  1004,   547,     0,     0,     0,
     237,   238,    88,   239,   240,   241,   242,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   215,     0,
      52,   216,    53,   217,   218,     0,   219,   220,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,   221,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,    80,   229,   230,   231,     0,   232,   233,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   234,   545,     0,
      85,   235,   236,   547,     0,     0,     0,   237,   238,    88,
     239,   240,   241,   242,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   215,     0,    52,   216,    53,
     217,   218,     0,   219,   220,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     221,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     222,   223,     0,   224,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,     0,    80,
     229,   230,   231,     0,   232,   233,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   234,     0,     0,    85,   235,   236,
       0,     0,     0,     0,   237,   238,    88,   239,   240,   241,
     242,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   215,     0,    52,   216,    53,   217,   218,     0,
     219,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,   221,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,    80,   229,   230,   231,
       0,   232,   233,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   234,     0,     0,    85,   235,   675,     0,     0,     0,
       0,   237,   238,    88,   239,   240,   241,   242,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   215,
       0,    52,   216,    53,   217,   218,     0,   219,   220,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   221,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   222,   223,     0,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,     0,    80,   229,   230,   231,     0,   232,   233,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   234,     0,
       0,    85,   235,   828,     0,     0,     0,     0,   237,   238,
      88,   239,   240,   241,   242,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   215,     0,    52,   216,
      53,   217,   218,     0,   219,   220,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,   221,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
      80,   229,   230,   231,     0,   232,   233,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   234,     0,     0,    85,   590,
    1004,     0,     0,     0,     0,   237,   238,    88,   239,   240,
     241,   242,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   215,     0,    52,   216,    53,   217,   218,
       0,   219,   220,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   221,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   222,   223,
       0,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,    80,   229,   230,
     231,     0,   232,   233,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   234,     0,     0,    85,   235,   546,     0,     0,
       0,     0,   237,   238,    88,   239,   240,   241,   242,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     215,     0,    52,   216,    53,   217,   218,     0,   219,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,   221,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,    80,   229,   230,   231,     0,   232,
     233,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   234,
       0,     0,    85,   590,   890,     0,     0,     0,     0,   237,
     238,    88,   239,   240,   241,   242,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   215,     0,    52,
     216,    53,   217,   218,     0,   219,   220,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   221,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,    80,   229,   230,   231,     0,   232,   233,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   234,     0,     0,    85,
     590,     0,     0,     0,     0,     0,   237,   798,    88,   239,
     240,   241,   242,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   215,     0,    52,   216,    53,   217,
     218,     0,   219,   220,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   221,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,    80,   229,
     230,   231,     0,   232,   233,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   234,     0,     0,    85,   912,     0,     0,
       0,     0,     0,   237,   913,    88,   239,   240,   241,   242,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   215,     0,    52,   216,    53,   217,   218,     0,   219,
     220,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   221,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,    80,   229,   230,   231,     0,
     232,   233,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     234,     0,     0,    85,   590,     0,     0,     0,     0,     0,
     237,   238,    88,   239,   240,   241,   242,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   215,     0,
      52,   216,    53,   217,   218,     0,   219,   220,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,   221,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,    80,   229,   230,   231,     0,   232,   233,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   234,     0,     0,
      85,   479,     0,     0,     0,     0,     0,   237,    87,    88,
     239,   240,   241,   242,  2009,     0,    -2,    -2,    -2,    -2,
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
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,    -2,    -2,
    2039,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,     0,     0,    -2,     0,     0,    -2,     0,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,  1121,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,  -503,  -503,     0,  -503,
      52,     0,    53,     0,     0,  -503,     0,   220,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1405,     0,  1121,     0,
      85,    86,     0,     0,     0,     0,     0,     0,    87,    88,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    -503,  -503,     0,  -503,    52,     0,    53,     0,     0,  -503,
       0,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1533,     0,  1121,     0,    85,    86,     0,     0,     0,     0,
       0,     0,    87,    88,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -503,  -503,     0,  -503,    52,     0,
      53,     0,     0,  -503,     0,   220,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1629,     0,  1121,     0,    85,    86,
       0,     0,     0,     0,     0,     0,    87,    88,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -503,  -503,
       0,  -503,    52,     0,    53,     0,     0,  -503,     0,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1800,     0,
    1121,     0,    85,    86,     0,     0,     0,     0,     0,     0,
      87,    88,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,  -503,  -503,     0,  -503,    52,     0,    53,     0,
       0,  -503,     0,   220,    55,    56,    57,    58,    59,    60,
      61,  1489,     0,     0,     0,     0,     0,     0,    64,    14,
      15,    16,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,   215,     0,     0,   216,     0,   217,   218,    80,   219,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,    64,   221,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,    86,     0,     0,
       0,     0,     0,     0,    87,    88,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,    80,   229,   230,   231,     0,
     232,   233,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     234,     0,     0,    85,   479,     0,     0,     0,     0,     0,
     237,  1490,    88,   239,   240,   241,   242,    14,    15,    16,
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
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    83,    84,
       0,    85,    86,     0,     0,     0,  -841,     0,     0,    87,
      88,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,    89,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,    54,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   159,     0,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    83,    84,     0,    85,   299,     0,
       0,     0,     0,     0,     0,    87,    88,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,    89,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
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
     676,     0,     0,     0,     0,     0,     0,    87,    88,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    89,
       4,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,    62,
       0,     0,     0,    63,     0,     0,    64,     0,     0,     0,
       0,  -422,  -422,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    67,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -422,     0,     0,     0,    85,    86,     0,     0,     0,     0,
       0,     0,    87,    88,     4,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,    62,     0,     0,     0,    63,     0,     0,
      64,     0,     0,     0,     0,  -423,  -423,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    67,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -423,     0,     0,     0,    85,    86,
       0,  1540,     0,  1541,     0,     0,    87,    88,  1542,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,    54,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,  1543,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1544,     0,     0,     0,    85,   878,     0,  1540,     0,
    1541,     0,     0,    87,    88,  1542,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,  1543,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    67,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1691,     0,
       0,     0,    85,   878,     0,  1540,     0,  1541,     0,     0,
      87,    88,  1542,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,  1543,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    67,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1692,     0,     0,     0,    85,
     878,     0,  1540,     0,  1541,     0,     0,    87,    88,  1542,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,  1543,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    67,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1693,     0,     0,     0,    85,   878,     0,     0,
       0,     0,     0,     0,    87,    88,   357,   195,     6,     7,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   357,     0,     0,     0,     0,     0,     0,
      85,   358,     0,    14,    15,    16,    17,    18,    87,    88,
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
       0,     0,     0,     0,     0,     0,     0,    85,   676,     0,
       0,     0,     0,     0,     0,    87,    88,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,   220,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,   782,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   829,     0,  -704,
      85,   625,     0,     0,     0,     0,     0,     0,    87,    88,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     220,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,   782,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     851,     0,  -704,    85,   722,     0,     0,     0,     0,     0,
       0,    87,    88,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   220,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,  1202,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -712,    85,   748,     0,     0,
       0,     0,     0,     0,    87,    88,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   220,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   447,    85,
     448,     0,     0,     0,     0,     0,     0,    87,    88,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   220,
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
       0,     0,    85,   748,   749,     0,     0,     0,     0,     0,
      87,    88,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   220,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,  1649,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,   748,     0,     0,     0,
       0,     0,     0,    87,    88,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,   220,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,  1651,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   748,
       0,     0,     0,     0,     0,     0,    87,    88,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   220,    55,
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
       0,    85,   648,     0,     0,     0,     0,     0,     0,    87,
      88,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    85,   748,     0,     0,     0,     0,
       0,     0,    87,    88,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   220,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,   448,     0,
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
       0,     0,  1564,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,   215,
       0,     0,   216,     0,   217,   218,     0,   219,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,   358,  1336,     0,   221,  1338,     0,  1339,    87,    88,
    1340,  1341,  1342,  1343,  1344,  1345,  1346,  1347,  1348,  1349,
    1350,  1351,     0,     0,  1352,  1353,  1354,  1355,  1356,  1357,
    1358,     0,  1359,     0,   222,   223,     0,   682,   225,  1360,
    1361,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,  1362,     0,   229,   230,   231,     0,   232,   233,
       0,     0,     0,     0,     0,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1363,     0,
       0,    85,   479,     0,     0,     0,   399,     0,   237,    87,
      88,   239,   240,   241,   242,     0,     0,     0,     0,     0,
       0,     0,     0,  -196,   403,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -426,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,     0,
       0,     0,     0,  -426,   403,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -427,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,     0,
       0,     0,     0,  -427,   403,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
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
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,     0,
       0,     0,     0,  -426,    14,    15,    16,    17,    18,    19,
     531,    20,   532,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   215,     0,    52,   216,
      53,   217,   218,     0,   219,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,   221,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   533,     0,     0,     0,     0,  1351,     0,
    -355,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
      80,   229,   230,   231,     0,   232,   233,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1363,     0,     0,    85,   534,
       0,     0,     0,   399,     0,   237,    87,    88,   535,   536,
     241,   242,    14,    15,    16,    17,    18,    19,   531,    20,
     532,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   215,     0,    52,   216,    53,   217,
     218,     0,   219,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   221,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   533,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,    80,   229,
     230,   231,     0,   232,   233,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   234,     0,     0,    85,   534,     0,     0,
       0,   399,     0,   237,    87,    88,   535,   536,   241,   242,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   215,     0,    52,   216,    53,   217,   218,     0,
     219,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,   221,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,    80,   229,   230,   231,
       0,   232,   233,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   234,     0,   485,    85,   486,     0,     0,     0,     0,
       0,   237,    87,    88,   239,   240,   241,   242,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     215,     0,    52,   216,    53,   217,   218,     0,   219,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,   221,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,    80,   229,   230,   231,     0,   232,
     233,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   234,
       0,     0,    85,   486,     0,     0,     0,   399,     0,   237,
      87,    88,   239,   240,   241,   242,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   215,     0,
      52,   216,    53,   217,   218,     0,   219,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,   221,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,    80,   229,   230,   231,     0,   232,   233,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   234,     0,     0,
      85,   534,     0,     0,     0,   399,     0,   237,    87,    88,
     239,   240,   241,   242,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   215,     0,    52,   216,
      53,   217,   218,     0,   219,   220,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,   221,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
      80,   229,   230,   231,     0,   232,   233,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   234,     0,     0,    85,   590,
       0,     0,     0,     0,     0,   237,    87,    88,   239,   240,
     241,   242,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   215,     0,    52,   216,    53,   217,
     218,     0,   219,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   221,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,    80,   229,
     230,   231,     0,   232,   233,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   234,     0,     0,    85,   486,     0,     0,
       0,     0,     0,   237,    87,    88,   239,   240,   241,   242,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   215,     0,    52,   216,    53,   217,   218,     0,
     219,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,   221,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,    80,   229,   230,   231,
     215,   232,   233,   216,     0,   217,   218,     0,   219,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,   221,     0,     0,     0,     0,
       0,   234,     0,     0,    85,   479,     0,     0,     0,     0,
       0,   237,    87,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,    80,   229,   230,   231,     0,   232,
     233,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1737,  1738,  1739,  1740,     0,     0,     0,   234,
    1954,     0,    85,   479,     0,     0,     0,     0,     0,   237,
      87,    88,   239,   240,   241,   242,   357,   195,     6,     7,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    82,     0,     0,
       0,     0,     0,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
      85,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   220,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   159,     0,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    82,   782,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -704,    85,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   220,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   159,     0,   551,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   905,     0,
       0,    85,   668,     0,     0,     0,     0,     0,     0,    87,
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
       0,   159,     0,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,    86,     0,     0,     0,
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
       0,     0,     0,     0,     0,   159,     0,   551,    70,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    85,
     552,     0,     0,     0,     0,     0,     0,    87,    88,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    82,
     782,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -704,    85,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   220,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    82,  1329,     0,     0,     0,     0,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,    85,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,    85,    21,    22,    23,    24,    25,    26,    27,
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
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   886,    85,   878,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,   905,
       0,     0,    85,   552,     0,     0,     0,     0,     0,     0,
      87,    88,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   220,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   905,     0,     0,    85,   668,     0,     0,
       0,     0,     0,     0,    87,    88,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,  1439,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,   878,     0,     0,     0,     0,     0,     0,    87,    88,
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
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    85,   410,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   299,
       0,     0,     0,     0,     0,     0,    87,    88,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   220,
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
       0,     0,    85,   410,     0,     0,     0,     0,     0,     0,
      87,    88,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   220,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,   668,     0,     0,
       0,     0,     0,     0,    87,    88,    14,    15,    16,    17,
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
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,   680,     0,     0,     0,     0,     0,     0,    87,    88,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    85,   448,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   878,
       0,     0,     0,     0,     0,     0,    87,    88,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   220,
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
       0,     0,    85,   648,     0,     0,     0,     0,     0,     0,
      87,    88,   195,     6,     7,     8,     9,    10,    11,    12,
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
       0,    85,   552,     0,     0,     0,     0,     0,     0,    87,
      88,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   220,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,   428,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    85,
      86,     0,     0,     0,     0,     0,     0,    87,    88,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     220,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,   878,     0,     0,     0,     0,     0,
       0,    87,    88,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   220,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,   676,     0,
       0,     0,     0,     0,     0,    87,    88,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   220,    55,
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
       0,    85,     0,     0,    14,    15,    16,    17,    18,    87,
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
       0,     0,     0,     0,     0,     0,     0,     0,    85,   428,
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
       0,     0,     0,     0,     0,    85,   676,     0,    14,    15,
      16,    17,    18,    87,    88,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -504,  -504,
       0,  -504,    52,     0,    53,     0,     0,  -504,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,     0,     0,     0,     0,     0,     0,     0,
      87,    88,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   215,     0,    52,
     216,    53,   217,   218,     0,   219,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,     0,   229,   230,   231,     0,   232,   233,     0,     0,
       0,     0,     0,     0,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   234,     0,     0,    85,
     479,  1070,     0,     0,     0,     0,   237,   238,    88,   239,
     240,   241,   242,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   215,     0,
      52,   216,    53,   217,   218,     0,   219,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   221,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,     0,   229,   230,   231,     0,   232,   233,     0,
       0,     0,     0,     0,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   234,     0,     0,
      85,   479,     0,     0,     0,     0,     0,   237,    87,    88,
     239,   240,   241,   242,    14,    15,    16,    17,    18,    19,
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
       0,     0,     0,     0,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,    85,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,    85,    21,    22,    23,    24,    25,
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
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      85,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,  -504,  -504,     0,  -504,    52,     0,    53,     0,
       0,  -504,     0,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    64,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,    69,    70,    52,     0,    53,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    64,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,   522,     0,
     523,   524,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,    20,    82,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -503,  -503,     0,  -503,    52,     0,    53,     0,     0,
    -503,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,    20,    64,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,  -504,
    -504,     0,  -504,    52,     0,    53,     0,     0,  -504,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,    82,   215,     0,     0,   216,     0,   217,   218,
       0,   219,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   222,   223,
      82,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,     0,   229,   230,
     231,     0,   232,   233,     0,     0,   215,     0,     0,   216,
      81,   217,   218,     0,   219,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1737,  1738,  1739,  1740,     0,
       0,   221,   234,  1881,     0,    85,   479,     0,     0,     0,
       0,     0,   237,    87,    88,   239,   240,   241,   242,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   682,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
       0,   229,   230,   231,   215,   232,   233,   216,     0,   217,
     218,     0,   219,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   221,
       0,     0,     0,     0,     0,   234,    84,     0,   683,   684,
       0,     0,     0,   685,     0,   237,    87,    88,   239,   240,
     241,   242,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,     0,   229,
     230,   231,   215,   232,   233,   216,     0,   217,   218,     0,
     219,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   221,     0,     0,
       0,     0,     0,   234,   545,     0,    85,   479,     0,   547,
       0,     0,     0,   237,    87,    88,   239,   240,   241,   242,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,     0,   229,   230,   231,
     215,   232,   233,   216,     0,   217,   218,     0,   219,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,     0,     0,     0,     0,
       0,   234,  1195,     0,    85,   479,     0,     0,     0,  1196,
       0,   237,    87,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,     0,   229,   230,   231,   215,   232,
     233,   216,     0,   217,   218,     0,   219,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   221,     0,     0,     0,     0,     0,   234,
    1198,     0,    85,   479,  1209,     0,     0,     0,     0,   237,
      87,    88,   239,   240,   241,   242,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,     0,   229,   230,   231,   215,   232,   233,   216,
       0,   217,   218,     0,   219,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   221,     0,     0,     0,     0,     0,   234,     0,     0,
      85,   479,     0,     0,     0,   685,     0,   237,    87,    88,
     239,   240,   241,   242,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
       0,   229,   230,   231,   215,   232,   233,   216,     0,   217,
     218,     0,   219,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   221,
       0,     0,     0,     0,     0,   234,     0,     0,    85,   479,
       0,     0,     0,   399,     0,   237,    87,    88,   239,   240,
     241,   242,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,     0,   229,
     230,   231,   215,   232,   233,   216,     0,   217,   218,     0,
     219,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   221,     0,     0,
       0,     0,     0,   234,     0,     0,    85,   479,     0,     0,
     965,     0,     0,   237,    87,    88,   239,   240,   241,   242,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,     0,   229,   230,   231,
     215,   232,   233,   216,     0,   217,   218,     0,   219,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,     0,     0,     0,     0,
       0,   234,     0,     0,    85,   479,   978,     0,     0,     0,
       0,   237,   238,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,     0,   229,   230,   231,   215,   232,
     233,   216,     0,   217,   218,     0,   219,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   221,     0,     0,     0,     0,     0,   234,
       0,     0,    85,   479,  1002,     0,     0,     0,     0,   237,
      87,    88,   239,   240,   241,   242,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,     0,   229,   230,   231,   215,   232,   233,   216,
       0,   217,   218,     0,   219,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   221,     0,     0,     0,     0,     0,   234,  1198,     0,
      85,   479,     0,     0,     0,     0,     0,   237,    87,    88,
     239,   240,   241,   242,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
       0,   229,   230,   231,   215,   232,   233,   216,     0,   217,
     218,     0,   219,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   221,
       0,     0,     0,     0,     0,   234,     0,     0,    85,   479,
       0,     0,     0,  1574,     0,   237,    87,    88,   239,   240,
     241,   242,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,     0,   229,
     230,   231,   215,   232,   233,   216,     0,   217,   218,     0,
     219,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   221,     0,     0,
       0,     0,     0,   234,  1646,     0,    85,   479,     0,     0,
       0,     0,     0,   237,    87,    88,   239,   240,   241,   242,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,     0,   229,   230,   231,
     215,   232,   233,   216,     0,   217,   218,     0,   219,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,     0,     0,     0,     0,
       0,   234,     0,     0,    85,   479,     0,     0,     0,  1797,
       0,   237,    87,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,     0,   229,   230,   231,   215,   232,
     233,   216,     0,   217,   218,     0,   219,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   221,     0,     0,     0,     0,     0,   234,
       0,  1945,    85,   479,     0,     0,     0,     0,     0,   237,
      87,    88,   239,   240,   241,   242,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,     0,   229,   230,   231,   215,   232,   233,   216,
       0,   217,   218,     0,   219,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   221,     0,     0,     0,     0,     0,   234,  1950,     0,
      85,   479,     0,     0,     0,     0,     0,   237,    87,    88,
     239,   240,   241,   242,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
       0,   229,   230,   231,     0,   232,   233,     0,     0,   215,
       0,     0,   216,    81,   217,   218,     0,   219,  2026,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,   234,  1960,     0,    85,   479,
       0,     0,     0,     0,     0,   237,    87,    88,   239,   240,
     241,   242,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   222,   223,     0,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,     0,     0,   229,   230,   231,   215,   232,   233,
     216,     0,   217,   218,     0,   219,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,     0,     0,     0,     0,     0,   234,     0,
       0,    85,   479,     0,     0,     0,     0,     0,   237,    87,
      88,   239,   240,   241,   242,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,     0,   229,   230,   231,   215,   232,   233,   216,     0,
     217,   218,     0,   219,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     221,     0,     0,     0,     0,     0,   234,  2033,     0,    85,
     479,     0,     0,     0,     0,     0,   237,    87,    88,   239,
     240,   241,   242,     0,     0,     0,     0,     0,     0,     0,
     222,   223,     0,   224,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,     0,     0,
     229,   230,   231,   215,   232,   233,   216,     0,   217,   218,
       0,   219,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,     0,
       0,     0,     0,     0,   234,  2035,     0,    85,   479,     0,
       0,     0,     0,     0,   237,    87,    88,   239,   240,   241,
     242,     0,     0,     0,     0,     0,     0,     0,   222,   223,
       0,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,     0,   229,   230,
     231,   215,   232,   233,   216,     0,   217,   218,     0,   219,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,     0,     0,     0,
       0,     0,   234,  2082,     0,    85,   479,     0,     0,     0,
       0,     0,   237,    87,    88,   239,   240,   241,   242,     0,
       0,     0,     0,     0,     0,     0,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,     0,   229,   230,   231,   215,
     232,   233,   216,     0,   217,   218,     0,   219,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,     0,     0,     0,     0,     0,
     234,  2084,     0,    85,   479,     0,     0,     0,     0,     0,
     237,    87,    88,   239,   240,   241,   242,     0,     0,     0,
       0,     0,     0,     0,   222,   223,     0,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,     0,     0,   229,   230,   231,   215,   232,   233,
     216,     0,   217,   218,     0,   219,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,     0,     0,     0,     0,     0,   234,  2086,
       0,    85,   479,     0,     0,     0,     0,     0,   237,    87,
      88,   239,   240,   241,   242,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,     0,   229,   230,   231,   215,   232,   233,   216,     0,
     217,   218,     0,   219,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     221,     0,     0,     0,     0,     0,   234,  2091,     0,    85,
     479,     0,     0,     0,     0,     0,   237,    87,    88,   239,
     240,   241,   242,     0,     0,     0,     0,     0,     0,     0,
     222,   223,     0,   224,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,     0,     0,
     229,   230,   231,   215,   232,   233,   216,     0,   217,   218,
       0,   219,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,     0,
       0,     0,     0,     0,   234,  2093,     0,    85,   479,     0,
       0,     0,     0,     0,   237,    87,    88,   239,   240,   241,
     242,     0,     0,     0,     0,     0,     0,     0,   222,   223,
       0,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,     0,   229,   230,
     231,   215,   232,   233,   216,     0,   217,   218,     0,   219,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,     0,     0,     0,
       0,     0,   234,  2129,     0,    85,   479,     0,     0,     0,
       0,     0,   237,    87,    88,   239,   240,   241,   242,     0,
       0,     0,     0,     0,     0,     0,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,     0,   229,   230,   231,   215,
     232,   233,   216,     0,   217,   218,     0,   219,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,     0,     0,     0,     0,     0,
     234,  2131,     0,    85,   479,     0,     0,     0,     0,     0,
     237,    87,    88,   239,   240,   241,   242,     0,     0,     0,
       0,     0,     0,     0,   222,   223,     0,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,     0,     0,   229,   230,   231,   215,   232,   233,
     216,     0,   217,   218,     0,   219,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,     0,     0,     0,     0,     0,   234,  2133,
       0,    85,   479,     0,     0,     0,     0,     0,   237,    87,
      88,   239,   240,   241,   242,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,     0,   229,   230,   231,   215,   232,   233,   216,     0,
     217,   218,     0,   219,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     221,     0,     0,     0,     0,     0,   234,  2152,     0,    85,
     479,     0,     0,     0,     0,     0,   237,    87,    88,   239,
     240,   241,   242,     0,     0,     0,     0,     0,     0,     0,
     222,   223,     0,   224,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,     0,     0,
     229,   230,   231,   215,   232,   233,   216,     0,   217,   218,
       0,   219,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,     0,
       0,     0,     0,     0,   234,  2154,     0,    85,   479,     0,
       0,     0,     0,     0,   237,    87,    88,   239,   240,   241,
     242,     0,     0,     0,     0,     0,     0,     0,   222,   223,
       0,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,     0,   229,   230,
     231,   215,   232,   233,   216,     0,   217,   218,     0,   219,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,     0,     0,     0,
       0,     0,   234,  2156,     0,    85,   479,     0,     0,     0,
       0,     0,   237,    87,    88,   239,   240,   241,   242,     0,
       0,     0,     0,     0,     0,     0,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,     0,   229,   230,   231,   215,
     232,   233,   216,     0,   217,   218,     0,   219,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,     0,     0,     0,     0,     0,
     234,     0,     0,    85,   479,     0,     0,     0,     0,     0,
     237,    87,    88,   239,   240,   241,   242,     0,     0,     0,
       0,     0,     0,     0,   222,   223,     0,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,     0,     0,   229,   230,   231,   215,   232,   233,
     216,     0,   217,   218,     0,   219,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,     0,     0,     0,     0,     0,   510,     0,
       0,    85,   479,     0,     0,     0,     0,     0,   237,    87,
      88,   239,   240,   241,   242,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,     0,   229,   230,   231,   215,   232,   233,   216,     0,
     217,   218,     0,   219,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     221,     0,     0,     0,     0,     0,   513,     0,     0,    85,
     479,     0,     0,     0,     0,     0,   237,    87,    88,   239,
     240,   241,   242,     0,     0,     0,     0,     0,     0,     0,
     222,   223,     0,   224,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,     0,     0,
     229,   230,   231,   215,   232,   233,   216,     0,   217,   218,
       0,   219,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,     0,
       0,     0,     0,     0,   519,     0,     0,    85,   479,     0,
       0,     0,     0,     0,   237,    87,    88,   239,   240,   241,
     242,     0,     0,     0,     0,     0,     0,     0,   222,   223,
       0,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,     0,   229,   230,
     231,   215,   232,   233,   216,     0,   217,   218,     0,   219,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,     0,     0,     0,
       0,     0,   528,     0,     0,    85,   479,     0,     0,     0,
       0,     0,   237,    87,    88,   239,   240,   241,   242,     0,
       0,     0,     0,     0,     0,     0,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,     0,   229,   230,   231,   215,
     232,   233,   216,     0,   217,   218,     0,   219,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,     0,     0,     0,     0,     0,
     234,     0,     0,    85,   479,     0,     0,     0,     0,     0,
     237,   238,    88,   239,   240,   241,   242,     0,     0,     0,
       0,     0,     0,     0,   222,   223,     0,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,     0,     0,   229,   230,   231,     0,   232,   233,
       0,     0,     0,     0,     0,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   234,     0,
       0,    85,   479,     0,     0,     0,     0,     0,   237,   798,
      88,   239,   240,   241,   242
};

static const yytype_int16 yycheck[] =
{
       1,   189,   372,     4,   234,    83,   356,     4,   177,    83,
     696,  1382,     1,   330,  1133,     3,   271,  1126,    83,     1,
    1091,   628,    65,   193,   573,   685,   325,   152,     1,  1413,
    1414,    83,   852,   330,    83,   316,  1363,   624,    83,   237,
     330,   330,   538,  1107,   330,   628,   340,   179,   179,  1139,
      85,    83,   351,   516,   194,   628,   624,  1147,  1831,   624,
       1,    62,    63,   629,    65,   127,   660,  1363,   662,   635,
     937,  1832,   649,   624,  1095,   369,    65,   330,  1748,   107,
     624,   203,    83,    65,   624,   139,   380,   911,    89,   624,
     148,   107,    65,    94,  1744,   107,  1939,  1118,  1316,  1831,
      79,   102,   330,   420,   330,  1323,   107,     1,  1831,   110,
       4,   236,    94,   114,    82,   158,    97,   114,   330,    96,
    1221,   690,    79,   420,    91,   107,   444,   205,   110,     0,
     420,   420,   114,   291,   420,   138,   139,   295,   269,    82,
     205,  1943,    68,    69,  1011,     1,   330,    82,    79,   170,
     129,   469,   153,   205,    97,   156,   205,   158,  1025,   107,
     205,    84,    85,   164,   755,   144,  1927,   420,   189,   158,
     171,    65,   129,   205,   271,   766,   158,   189,   179,    96,
     305,   184,   185,   164,  1402,   158,   339,   144,   221,   168,
     191,  1346,   420,   346,   420,   172,   451,   164,   129,    94,
      84,    85,   203,   204,   205,     1,  1876,   164,   420,    65,
    1319,   168,  2073,   144,  1975,   368,   110,   250,   251,   158,
     114,   189,   204,   814,   831,  1328,   379,  2070,   133,     1,
     171,  1881,  1882,   630,   102,   236,   420,   634,   170,   145,
    2101,   180,   829,   640,     1,   127,   172,     4,   180,   172,
       0,   164,   330,   170,   189,   172,   330,     1,   114,   172,
    2121,   829,   167,   189,   158,   330,   161,   268,   269,    65,
     667,  2044,  1328,  1337,   180,     1,    10,   674,   330,   742,
     743,   330,   340,   943,  2045,   330,   168,   108,   172,   829,
     250,   292,   148,    65,   549,   296,    20,   846,   330,  1320,
     170,   302,   158,   331,  1954,  1955,  1602,  1371,    65,  1605,
    1606,   369,  2044,   314,   512,   331,  1534,  2119,   319,   189,
     302,  2044,   380,   324,   325,   326,   458,   458,  1418,   330,
     331,   291,  1328,   465,    10,   295,     0,   914,   593,    65,
    1160,   571,   420,   164,  2105,   143,   420,   405,  2150,   331,
     351,   473,     1,  1424,   451,   420,   726,   114,   359,   394,
     361,   164,   158,   417,   734,  1466,  1467,  1468,   170,   370,
     371,   420,   177,   374,   923,   982,   148,   359,   841,   407,
     381,   968,   164,   331,   166,   188,   158,   704,   546,   187,
     164,   407,   374,   421,   395,   396,   859,   398,   894,   982,
     968,   158,   403,   968,   970,   421,   407,   704,  1232,   982,
     691,   399,   164,   414,   704,   704,    65,   968,   704,   420,
     421,   194,   148,   163,   968,   407,   170,   171,   968,   163,
     170,   556,   158,   968,  1537,  1538,  1539,   438,   439,   421,
     174,   740,   443,   234,  1130,   179,   119,   746,   481,   189,
      79,   704,   549,   168,   487,  1782,   170,   458,   173,   407,
     170,  1030,   164,   235,   359,   333,   467,   335,   172,   470,
     143,   765,   473,   421,   342,   189,   704,   967,   704,   189,
     374,  1537,  1538,  1539,   340,   189,  1782,   163,   170,   449,
     172,   808,   704,   662,   170,   496,   593,   628,   174,   148,
     129,  1092,    65,   179,   119,    68,    69,   647,    71,   158,
      79,   808,   670,   369,   496,   144,   517,   189,   808,   808,
     704,  1630,   808,   170,   380,  1909,   144,   172,   143,   699,
    1137,  1621,  1022,   180,   429,   164,  1196,   115,   116,   168,
     164,  1537,  1538,  1539,   189,   546,    82,   415,   416,   405,
     675,   166,  1042,   409,     1,   808,   653,     4,   164,   118,
     129,   562,    98,   564,   167,   168,   567,   664,   340,   602,
     571,   170,   145,   574,   145,   144,  1145,   730,   164,    79,
     808,   117,   808,   142,   164,   188,   546,   170,  1743,   167,
     189,   170,   487,  1748,  1461,   164,   808,   369,   169,   168,
     172,   164,   175,   176,   757,   410,   189,   179,   380,   180,
     189,   764,  1009,   170,   340,   768,   234,  1988,    65,   161,
     170,   170,   657,   624,   808,  1115,   704,   628,    82,   129,
     172,  1200,   189,   405,   381,   623,    83,   625,   189,   704,
     189,  1210,    89,   369,   144,    99,   188,    94,   683,   396,
     170,   652,   704,    94,   380,   704,   657,   658,   467,   704,
     107,   470,   170,   110,   164,  1234,   107,   114,  1262,   110,
     828,  1790,   704,   114,   170,   170,   448,  1243,  1244,   405,
     850,   189,   683,   189,   685,   673,     3,   784,   716,   649,
     170,   340,    76,   681,   189,   157,   158,   159,   160,   172,
     716,  1791,   320,   704,   170,   837,   179,   765,   170,   189,
     670,   158,   700,     3,    79,   716,    79,   164,   180,   510,
     369,  1876,   513,   189,   515,  1215,  1216,   189,   519,   534,
     808,   380,   179,   167,   716,   170,   164,   528,   164,   740,
     531,   532,   533,   808,   188,   746,   172,   170,   749,   907,
    1005,  1032,    79,   179,   189,   165,   405,   204,   205,   808,
     164,   158,   172,   204,   129,   890,   129,   171,   716,  1068,
     813,   172,   169,   170,   157,   158,   159,   160,     1,   144,
     170,   144,   810,   180,   166,  1275,   404,   170,  1448,   236,
     180,  1658,   172,  1660,   810,   177,   178,   180,  1953,   164,
     801,   164,   129,   168,   127,   168,   189,   808,   168,   810,
     166,  1966,   813,   173,   815,   171,   804,   144,   590,   170,
     817,   918,   269,   824,   813,  1124,   175,   166,   810,   180,
    1437,   813,   171,   182,   183,    79,  1107,   164,   170,   164,
     813,   168,    65,   648,   845,   166,  1004,  1416,   180,   980,
     171,   982,   186,   886,  1423,   302,   170,   817,   891,  1453,
    1454,   302,   810,   668,   115,   116,   180,   314,   828,   902,
    2025,   676,   164,   166,   647,   168,   648,   324,   325,   326,
    1449,   914,   166,   330,   331,   129,   170,   110,   905,   166,
     331,  1577,   510,   166,  1264,   513,   166,  1247,   171,    65,
     144,   519,    68,    69,   351,    71,   177,   178,  1005,   765,
     528,   684,   359,   166,  1574,   164,   166,  1694,   359,   813,
     164,   171,  1699,   817,   168,   166,   166,   374,   929,   170,
     931,   919,   920,   374,   802,   158,   937,   142,   136,   137,
     558,  1094,   943,  1112,   603,   604,   605,   907,   395,  1412,
     967,   398,    53,    54,   914,    56,   403,   813,   166,   164,
     407,    62,    79,   168,    85,   166,   166,   968,   164,   170,
     175,   176,    79,   420,   421,   164,   748,   140,   141,   980,
      79,   982,     4,     5,     6,     7,     8,     9,    10,    11,
      12,  1272,   113,   765,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   164,  1022,  1555,   169,  1140,   166,
    1011,   458,   129,   170,   164,  1701,   166,   813,   168,  1691,
    1692,  1693,   129,  1024,  1025,  1042,   181,   144,  1125,   166,
     129,  1032,   127,   170,  1035,   176,   164,   144,   166,   765,
     168,   813,   164,   174,  1004,   144,   168,   164,   271,   496,
      72,   168,  1040,  1622,   166,   496,   813,   164,   170,    79,
     817,   168,   142,   169,   170,   164,  1337,  1068,   189,   168,
     517,   186,   164,   878,   166,  1644,   168,    13,    14,    15,
      16,    17,   142,     1,   164,    79,     4,   813,   168,   157,
     158,   159,   160,  1262,  1091,   175,   176,   166,  1115,   546,
    1371,   170,    79,   169,  1105,  1106,  1107,   912,   188,   129,
    1409,   729,   180,   169,   170,   562,   765,   564,  1107,   737,
     567,   189,   145,  1124,   144,   167,  1266,   574,  1268,  1126,
     170,  1091,   166,  1363,  1107,   129,   170,  1797,   756,   145,
     912,    22,   166,    79,   164,  1146,   170,    65,   168,   767,
     144,   374,   129,  1186,  1187,  1188,  1526,    79,   166,   164,
    1193,  1194,   170,   168,   813,   164,  1126,   144,   166,   164,
     164,   166,   170,   168,   168,   164,    94,   624,  1179,   115,
     116,   628,   164,  1237,  1753,   408,   168,   164,   115,   116,
       3,   168,   965,   129,  1248,  1196,   164,  1091,  1215,  1216,
      13,    14,    15,    16,    17,   166,   167,   129,   144,  1263,
     657,   658,    79,  1107,   157,   158,   159,   160,   175,   176,
    1221,   170,   144,  1277,  1793,  1413,  1414,   164,   451,   164,
     148,   167,  1126,   168,   152,   166,   683,   180,   685,   170,
     158,   164,   164,   161,   108,  1113,   168,   172,  1249,   166,
     164,  1107,   164,   170,   142,   166,   168,   704,  1275,   170,
       3,   166,   129,  1036,   166,   170,    79,   164,   170,   716,
      13,    14,    15,    16,    17,   716,   164,   144,   169,   170,
     168,    96,    79,  1415,   163,  1454,   166,   175,   176,   172,
     170,  1562,   166,   740,   166,  1566,  1567,   164,   170,   746,
     189,   168,   749,   172,  1337,   164,   166,  1112,   179,  1580,
     170,  1107,  1542,   169,   170,   166,   129,  1371,   236,   170,
     166,   166,  1319,   172,   170,   170,   549,  1315,   164,   166,
     188,   144,   129,   170,  1091,  1107,    79,   169,   170,  1340,
     166,  1910,  1343,  1344,  1345,  1914,  1400,   144,  1337,   166,
    1107,  1352,    13,    14,    15,    16,    17,   166,   166,  1319,
     166,   808,   170,   810,  1337,   166,   813,   164,   166,  1126,
    1371,   168,  1177,   166,  2106,  1562,  1377,   170,  2110,  1566,
     166,  1107,  1371,   168,    79,   166,   129,   305,   166,  1706,
     170,  1392,  1183,   169,  1395,  1396,   164,  1398,  1371,  1396,
     164,   144,  1403,  1457,  1458,   172,  1407,   172,  1409,  1706,
    1325,  1326,  1327,  1395,  1396,   188,  1706,  1706,    79,    18,
    1706,   168,   340,   171,    79,  1319,   171,  1424,   170,   171,
     653,   170,  1300,  1328,   129,  1984,  1490,   164,  1492,   357,
     166,   359,   188,  1337,  1713,  1714,  1715,  1448,   166,   144,
     166,   369,   188,  1706,  1508,   169,   170,   166,  1107,  1077,
    1461,   166,   380,  1236,  1424,  1466,  1467,  1468,   129,   164,
     169,   170,  1090,   168,   129,  1093,   166,  1371,  1706,  1097,
    1706,  1337,   929,   144,   931,   403,  1626,   405,   166,   144,
     937,   166,   142,  1266,  1706,  1268,   943,   610,   611,   612,
     613,  1395,  1396,   166,  1562,   169,   170,   164,  1566,   164,
     172,   429,   172,   168,   164,  1371,   169,   170,   168,   166,
    1548,   968,  1706,   169,   170,   175,   176,   169,   170,  1846,
    1424,   172,  1548,   980,   172,   982,    98,    99,   169,   170,
    1396,  1337,   172,  1540,   169,   170,   170,  1548,    77,  1846,
     189,  1552,  1553,   169,   170,   177,  1846,  1846,   169,   170,
    1846,   784,  1319,   169,  1011,  1337,   169,   170,   169,   487,
     170,   171,  1363,  1574,   797,  1371,    84,    85,  1025,   170,
    1337,   170,   171,  1222,  1223,  1032,   614,   615,  1035,   169,
     813,   606,   607,  1846,   608,   609,   164,  1598,  1599,  1371,
    1548,  1771,    85,    18,    18,  1605,  1606,  1608,  1879,  1714,
    1715,  1337,   172,   172,  1371,   189,  1670,  1671,  1846,   170,
    1846,  1068,   166,  1624,   166,   170,  1608,   166,   250,   170,
     166,   166,  1706,  1630,  1846,   166,   170,   169,   556,  1396,
     166,   166,  1537,  1538,  1539,  1371,  1540,  1542,  1543,    64,
      65,    66,    67,    68,    69,    70,    71,  1658,  1105,  1660,
     166,   166,  1846,   166,   166,   169,  1694,  1424,  1562,   169,
    1630,  1699,  1566,  1567,   169,   169,  1975,  1124,  1694,   169,
    1708,   166,   170,  1699,   163,   166,  1580,   169,  1337,   166,
     166,     5,  1708,  1694,   166,   918,   166,   166,  1699,    13,
      14,    15,    16,    17,  1322,  1706,  1562,  1708,   166,   166,
    1566,  1567,   163,   169,  1608,  1716,   170,   166,  1336,   166,
     166,  1909,  1371,   166,  1580,   166,  1896,  2044,   166,   166,
     166,   166,  1179,   166,  1735,  1732,  1630,    22,  1356,  2010,
     166,  1742,   166,   166,  1777,  1363,  1694,  2044,    77,  1196,
     163,  1699,   172,   172,  2044,  2044,   172,   675,  2044,   166,
    1708,   188,   172,   172,   170,    79,  1562,   166,   166,  2040,
    1566,  1567,  1773,   166,  1221,    13,    14,    15,    16,    17,
      18,   166,  1005,  1540,  1580,   166,   166,   166,   410,   166,
    1562,  2044,   166,   166,  1566,  1567,  1797,   172,   170,  1939,
      83,     1,  1249,  2074,     4,  1562,   170,   170,  1580,  1566,
    1567,  1981,   169,   163,   166,   129,  2044,   188,  2044,   166,
     166,  1849,   163,  1580,   107,   163,    14,   449,   164,   164,
     144,   164,  2044,  1849,   164,   189,  1562,  1731,  1732,   164,
    1566,  1567,   164,   164,   164,  1846,   170,   765,  1849,   172,
     163,  1642,   171,  1626,  1580,   171,   188,  1858,  1859,   166,
    2044,   166,   166,   171,  1865,    65,    13,    14,    15,    16,
      17,   171,   169,  1630,   169,   131,  1732,  1878,   166,  1976,
     169,   164,   163,    83,  1107,   169,    86,  1888,   166,  1890,
     166,   166,   169,   166,    94,   813,   120,   121,   122,   123,
     124,  1849,  1903,   166,  1905,  1906,  1907,   107,  1936,   166,
     110,   163,   534,  1562,   114,   163,    87,  1566,  1567,   164,
    1936,    13,    14,    15,    16,    17,    99,   189,  2068,   164,
    2070,  1580,    79,    97,  1894,  1936,  2106,   163,  1556,  1557,
    2110,  2111,  1943,   189,   164,  1392,  1947,   189,  1395,  1396,
    1983,  1952,   152,  1941,  1395,  1396,   189,   189,   158,   189,
     189,   161,  1409,   164,   164,   165,  2044,  1861,  2108,  2139,
    2044,   189,   890,   166,  1975,  1732,  1977,   177,   163,  2044,
     172,  1969,   129,   163,   163,  1879,   269,    79,  1936,  1607,
    2160,   166,   170,   163,  2164,  2044,   163,   144,   145,  1894,
    2001,  1448,   202,   166,   204,   205,  2176,   166,   166,   166,
     166,   166,  2013,  1973,  1461,   169,  2017,  2045,   169,  1466,
    1467,  1468,   166,  1879,   166,  2026,   648,   649,   166,  2045,
     171,  2032,   166,  2173,   234,   235,   236,   129,   166,   189,
     170,   164,   325,  2044,  2045,   166,   668,   330,   331,   164,
     250,   113,   144,   145,   676,   117,   118,   119,   120,   121,
     122,   123,   124,  2045,   164,   163,  2120,   166,   351,   269,
     163,   271,   169,   169,    82,    82,  2077,  2105,  1973,   189,
     189,  2135,   163,  1879,   164,   189,   164,   166,   163,  2105,
     166,   291,   166,   166,    82,   295,   168,  2045,    82,   299,
     189,  1548,   302,   165,  2105,   305,   168,  1879,    82,   180,
     180,   171,   189,  2114,  1337,   163,  2010,   163,  2119,   163,
     189,   189,  1879,  2105,   407,   325,   165,  1574,   180,   180,
     330,   331,   163,   166,   171,   113,   164,   420,   421,   166,
     170,   165,   180,   166,  2145,  1763,  2040,  2148,  1371,  2150,
     169,   351,    82,  1879,  2010,   180,   166,  2105,   358,   359,
     189,  1608,   165,   163,   166,   171,  1939,  1608,  2169,   163,
      68,   164,  1395,   189,   374,   166,  2177,   189,   189,   410,
    2074,  1642,   537,   617,  2040,  2186,   616,   618,   620,  1107,
      13,    14,    15,    16,    17,    18,  1229,   428,   131,  1358,
     133,   134,   135,  1371,   249,  2150,  2070,   407,   619,  1566,
     410,  1658,   412,  1660,  2010,  2101,  1887,   115,  2074,   419,
     420,   421,   120,  1879,  2059,   123,  1580,   125,   428,   429,
    1879,   164,  1764,  1781,   167,   168,  1764,  2041,  2010,   172,
     173,  1973,  2111,  2164,  2040,  2040,  1398,  1694,   448,   449,
     450,   451,  1699,  2010,    55,   123,   878,   365,  1936,  1706,
      13,  1708,    13,    14,    15,    16,    17,    18,  2040,  2042,
    1999,  1543,    13,    14,    15,    16,    17,  1420,  2074,  1796,
    1032,  1392,  1612,  2040,  2010,     0,   486,   487,   692,   489,
     912,   824,   914,   845,   851,  2068,   496,  2070,   851,  1529,
      -1,   851,  2074,   534,    13,    14,    15,    16,    17,    18,
     510,    -1,    -1,   513,  2040,   515,   516,  2074,    -1,   519,
      -1,    -1,    -1,    -1,    -1,    -1,   224,    -1,   528,    -1,
      -1,   531,   532,   533,   534,  2108,    -1,    -1,    79,  1562,
      -1,   624,    95,  1566,  1567,   628,   546,    -1,  2074,   549,
    1797,    -1,    -1,    -1,    -1,   555,   556,  1580,    -1,    -1,
     113,  2010,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,    -1,   273,   274,    -1,   276,    -1,
     278,    -1,    -1,    -1,    -1,  1608,    -1,    -1,   129,    -1,
     590,  2040,    -1,   593,    -1,    -1,    -1,    -1,    -1,  1846,
    2173,   142,  1849,   144,    -1,    13,    14,    15,    16,    17,
    1328,   164,    -1,    -1,    -1,    -1,    -1,   648,    -1,  1337,
      -1,   704,    -1,   164,   624,  2074,    -1,   168,   628,   660,
      -1,    -1,    -1,   716,   175,   176,    -1,   668,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   676,    -1,    -1,   648,   649,
      -1,    -1,    -1,  1371,    -1,    -1,    -1,   740,    -1,    -1,
     660,    -1,   662,   746,   664,   363,    -1,    -1,   668,    -1,
     670,    79,    -1,    -1,    -1,   675,   676,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    -1,  2104,    -1,    -1,  1936,
    1112,    -1,    -1,    -1,    -1,    -1,   696,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   704,    -1,    -1,    -1,  1731,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   716,    -1,  2136,    -1,
      -1,   129,    -1,    -1,    -1,   808,    -1,   810,  1975,    -1,
    1977,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
     740,    -1,   742,   743,    -1,    -1,   746,    -1,   748,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,    -1,
     168,    -1,    -1,    -1,   398,   113,    95,   175,   176,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,  2026,
      -1,    -1,    -1,   341,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,  2044,  2045,     4,
       5,     6,     7,     8,     9,    10,    11,    12,   808,    -1,
     810,    -1,    -1,   813,    -1,    -1,    -1,   817,    -1,  1537,
    1538,  1539,  1540,  1541,  1542,  1543,    -1,    -1,   828,   829,
      -1,   831,    -1,    -1,    -1,    -1,    -1,    -1,  1861,    -1,
      -1,   841,    -1,    -1,  1562,    -1,    -1,   878,  1566,  1567,
      -1,   851,   852,    -1,    -1,    -1,  1879,    86,  2105,   859,
      -1,   113,  1580,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,   905,    -1,    -1,    -1,   878,    -1,
      -1,   912,    -1,    -1,    79,   968,    -1,    -1,    -1,    -1,
     890,    -1,    -1,    -1,    -1,    -1,    -1,   980,    -1,   982,
      -1,    -1,    68,    -1,    -1,   905,    -1,   907,    -1,    -1,
      -1,    -1,   912,    -1,   914,     3,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
    2177,   489,    -1,    -1,   129,    -1,   967,   189,    -1,  2186,
      -1,    -1,    -1,    -1,   110,    -1,    -1,   505,    -1,   144,
     508,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,   967,   968,   164,
     165,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   982,    -1,   682,  1068,   161,  2010,    -1,    -1,
      -1,  1022,   187,    -1,    -1,    -1,    -1,    -1,    -1,   165,
      -1,    -1,    -1,    -1,  1004,  1005,   235,    -1,    -1,    -1,
      -1,  1042,    -1,   188,    -1,    -1,    -1,  2040,    -1,    -1,
      -1,   250,  1022,    -1,    -1,   113,    65,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,  1124,  1042,   272,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2074,    -1,    -1,   142,    94,    -1,    -1,   224,    -1,
      -1,    -1,   291,    -1,   762,    -1,   295,    -1,  1068,    -1,
     299,    -1,    -1,    -1,    -1,   114,   164,   165,    -1,    -1,
      -1,    -1,    -1,   171,  1115,    -1,    -1,   175,   176,    -1,
      -1,  1091,    -1,    -1,    -1,    -1,    -1,   731,    -1,   187,
      -1,    -1,    -1,    -1,    79,   271,    -1,  1107,    -1,    -1,
     398,    -1,  1112,    -1,    -1,  1115,    -1,    -1,    -1,   158,
      -1,    -1,   161,    -1,  1124,  1125,  1126,    -1,    -1,   358,
    1130,    -1,    -1,    -1,    -1,    -1,   302,  1137,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,   709,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
    1160,  1879,    -1,   202,    -1,    -1,  1249,   142,    -1,   144,
      -1,    -1,    13,    14,    15,    16,    17,   811,    -1,    -1,
      -1,   410,    -1,  1183,  1215,  1216,    -1,    -1,    -1,   164,
     165,    -1,    -1,   168,    -1,   234,    -1,   236,    -1,   314,
     175,   176,    -1,    -1,    -1,    -1,    -1,    -1,   374,    -1,
      -1,    -1,   187,    -1,    -1,  1215,  1216,   851,    -1,   448,
     449,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
     269,    -1,    -1,   867,    -1,    -1,    -1,   871,    79,    -1,
      -1,    -1,   408,    -1,  1275,    -1,   412,    -1,    -1,    -1,
      -1,    -1,    -1,   419,    -1,    -1,   295,   486,    -1,   113,
      -1,    -1,  1262,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   113,    -1,    -1,  1275,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,   451,    -1,    -1,   129,    -1,
      79,    -1,  2010,   851,   852,   334,    -1,    -1,    -1,    -1,
      -1,   142,   341,   144,   862,   534,    -1,   865,    -1,    -1,
     164,   165,    -1,    -1,    -1,    -1,    -1,   546,    -1,  1319,
     359,   550,  2040,   164,   165,    -1,  1409,    -1,  1328,    -1,
      -1,    -1,    -1,   187,   175,   176,   624,  1337,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,   187,    -1,    -1,    -1,
      -1,    -1,   986,   142,    -1,   144,  2074,    -1,    -1,   993,
      79,   590,    -1,  1363,  1364,    -1,    -1,    -1,    -1,    -1,
      -1,  1371,    -1,    -1,    -1,   164,    -1,    -1,    -1,   168,
      -1,    -1,    -1,   549,   423,    -1,   175,   176,    -1,   555,
     429,    -1,    -1,    -1,   113,  1395,  1396,    -1,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,  1409,
     129,   450,  1412,    -1,    -1,    -1,    -1,    -1,    -1,   648,
     649,    -1,  1453,   142,  1424,   144,    -1,   593,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1437,    -1,   668,
      -1,   670,    -1,   731,    -1,   164,   165,   676,   487,    -1,
     489,    -1,    -1,  1453,  1454,    -1,   175,   176,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1548,   505,   506,   187,   508,
     509,   510,    -1,    -1,   513,    -1,   515,   516,    -1,    -1,
     519,    -1,    -1,    -1,    -1,    -1,    -1,   653,    -1,   528,
    1048,    -1,   531,   532,   533,    -1,    -1,  1055,   664,    -1,
      -1,  1059,    -1,    -1,   113,  1063,   115,   546,   117,   118,
     119,   120,   121,   122,   123,   124,   682,    -1,    -1,   748,
      -1,    -1,    -1,   811,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1166,    -1,    -1,    -1,  1170,  1537,  1538,  1539,
    1540,   829,  1542,  1543,    -1,    -1,    -1,    -1,  1548,  1549,
      -1,    -1,    -1,    -1,    -1,    -1,   722,    -1,    -1,    -1,
      -1,    -1,  1562,   851,    -1,    -1,  1566,  1567,   683,    -1,
     685,    -1,    -1,    94,    -1,    -1,    -1,  1577,    -1,   867,
    1580,   747,    -1,   871,    -1,   624,    -1,    -1,   817,   628,
     629,    -1,    -1,    -1,    -1,    -1,   635,    -1,    -1,   828,
      -1,    -1,  1160,    -1,   770,    -1,   645,    -1,  1608,    -1,
      -1,  1694,    -1,    -1,    -1,    -1,  1699,    -1,   784,    -1,
      -1,    -1,    -1,  1706,    -1,  1708,    -1,    -1,    -1,    -1,
    1630,   797,    -1,    -1,    -1,    -1,   675,    -1,    -1,    -1,
     161,    -1,  1642,    -1,    -1,    -1,    -1,    -1,    -1,   878,
      -1,    -1,    -1,    -1,  1288,    -1,    -1,   696,  1292,    -1,
      -1,    -1,  1296,    -1,    -1,    -1,   705,    -1,    -1,    -1,
     709,    -1,    -1,    -1,    -1,    79,    -1,    -1,   907,    -1,
     968,   202,  1240,   912,    -1,   914,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1694,  1695,    -1,    -1,   986,  1699,
      -1,  1701,    -1,   742,   743,   993,  1706,    -1,  1708,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,  1279,    -1,    -1,  1282,   129,    -1,    -1,  1286,   250,
      -1,  1731,  1732,    -1,    -1,    -1,    -1,    -1,   142,   113,
     144,   194,    -1,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   918,    -1,    -1,    -1,   130,    -1,   132,    -1,
     164,   165,   215,  1846,   217,    -1,  1849,    -1,   221,   222,
     809,   175,   176,    -1,   813,  1004,    -1,    -1,   817,   232,
     233,    -1,    -1,   187,    -1,    -1,    -1,    -1,    -1,   828,
     829,   165,   831,    -1,   168,    -1,    -1,   250,   251,   113,
      -1,    -1,   841,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   851,   852,   929,  1103,    -1,    -1,    -1,    -1,
     859,    -1,   937,   862,   863,    -1,   865,   866,   943,    -1,
      -1,  1831,  1832,    -1,    -1,    -1,    -1,    -1,   359,  1005,
      -1,    -1,    -1,  1477,    -1,    -1,  1846,  1481,    -1,  1849,
    1484,   890,    -1,  1936,   168,    -1,    -1,    -1,    -1,    -1,
      -1,  1861,  1091,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1429,    -1,    -1,    -1,    -1,    -1,  1511,  1166,  1879,
      -1,  1515,  1170,    -1,    -1,  1519,    -1,    -1,    -1,    -1,
      -1,    -1,  1975,    -1,  1894,    -1,  1011,  1126,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   429,    -1,
    1025,    -1,    -1,    -1,    -1,    -1,    -1,  1032,    -1,    -1,
    1035,    -1,    -1,    -1,    -1,    -1,    -1,  1927,   449,   968,
      -1,   970,    -1,    -1,    -1,    -1,  1936,    -1,    -1,    -1,
      -1,    -1,   113,   982,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,    -1,  1125,
      -1,  2044,  2045,    -1,    -1,  1004,   487,    -1,    -1,    -1,
      -1,    -1,    -1,  1973,    -1,  1975,  1976,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   510,
      -1,    -1,    -1,    -1,    -1,   516,    -1,  1631,   519,  1999,
    1288,    -1,    -1,    -1,  1292,  1044,    -1,    -1,  1296,  1048,
    2010,    -1,    -1,    -1,    -1,    -1,  1055,  1056,   189,    -1,
    1059,  1060,  2105,    -1,  1063,  1064,    -1,    -1,   481,    -1,
      -1,  1070,    -1,    -1,   487,    -1,    -1,    -1,    -1,    -1,
    2040,    -1,    -1,   179,  2044,  2045,    -1,    -1,    -1,    -1,
      -1,   113,  1091,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,    -1,  1107,    -1,
      -1,    -1,    -1,    -1,  2074,    -1,    -1,  1116,    -1,    -1,
     142,  1196,    -1,    -1,    -1,    -1,    -1,  1126,    -1,    -1,
    1319,  1130,    -1,    -1,    -1,    -1,    -1,    -1,  1137,    -1,
      -1,    -1,   164,   165,    -1,  2105,  1221,    -1,    13,    14,
      15,    16,    17,   175,   176,    -1,    -1,    -1,    -1,    -1,
      -1,  1160,    -1,    -1,    -1,   187,    -1,    -1,   649,    -1,
      -1,    -1,    -1,    -1,    -1,  1364,    -1,  1695,    -1,    -1,
      -1,    -1,  1308,    -1,  1183,    -1,    -1,    -1,    -1,   602,
     603,   604,   605,   606,   607,   608,   609,   610,   611,   612,
     613,   614,   615,   616,   617,   618,   619,   620,    -1,    -1,
      -1,    -1,    -1,    -1,    79,   696,   113,    -1,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,  1477,
      -1,    -1,    -1,  1481,   647,  1424,  1484,    -1,    -1,    -1,
      -1,  1240,  1241,    -1,  1243,  1244,  1245,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,   742,   743,  1511,   129,    -1,    -1,  1515,    -1,  1395,
     167,  1519,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
    1279,  1280,   179,  1282,  1283,    -1,    -1,  1286,  1287,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,   164,
     165,    -1,    -1,   168,    72,    73,    74,    75,    -1,    -1,
     175,   176,    -1,  1831,  1832,    -1,    -1,    -1,    -1,    -1,
    1319,    -1,   187,    -1,    -1,    -1,    -1,    -1,    -1,  1328,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1337,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,     1,    -1,
     841,     4,    -1,    -1,  1363,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1371,  1448,    -1,    -1,    -1,    -1,   859,    -1,
      -1,    -1,    -1,  1631,    -1,    -1,  1461,    -1,    -1,    -1,
      -1,  1466,  1467,  1468,    -1,    -1,    -1,  1396,    -1,    -1,
     168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1927,
      -1,    -1,    -1,  1412,    -1,    -1,    -1,    -1,   186,    -1,
      -1,    -1,    65,  1549,    -1,  1424,    -1,    -1,    -1,    -1,
    1429,  1430,    -1,   914,    -1,    -1,    -1,    -1,  1437,    -1,
      -1,  1630,    -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    94,    -1,    -1,    -1,    -1,    -1,  1975,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,  2062,    -1,
      -1,   114,    -1,   886,    -1,    -1,    -1,  1995,   891,    -1,
      -1,  1999,  1608,    -1,    -1,    -1,    -1,    -1,    -1,   902,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1574,
      -1,   914,    -1,    -1,    -1,   148,    -1,    -1,    -1,   152,
      -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,   161,    -1,
      -1,    -1,   165,    -1,    -1,    -1,  2044,  2045,    -1,    -1,
      -1,    -1,    -1,   176,   177,    -1,   179,    -1,  1537,  1538,
    1539,  1540,    -1,  1542,  1543,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   965,    -1,    -1,    -1,    -1,    -1,    -1,   202,
      -1,    -1,    -1,  1562,    -1,    -1,    -1,  1566,  1567,    18,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1577,    -1,
      -1,  1580,    -1,  1658,    -1,  1660,    -1,  2105,    -1,    -1,
      -1,   234,   235,   236,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1731,   113,   250,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    68,
      69,    70,    71,  1036,    -1,    -1,    -1,    -1,   271,   272,
     113,  1630,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,  1642,    -1,    -1,    -1,    -1,   291,  1130,
      -1,    -1,   295,    -1,    -1,    -1,   299,    -1,    -1,   302,
      -1,    -1,   305,    -1,   113,   172,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,   175,    -1,    -1,    -1,  1695,   340,    -1,    -1,
      -1,    -1,  1701,    -1,    -1,  1894,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1712,   357,   358,   359,    -1,    -1,   168,
      -1,    -1,  1797,    -1,    -1,    -1,   369,    -1,    -1,   165,
      -1,   374,   168,  1732,    -1,  1861,    -1,   380,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   395,    -1,    -1,   398,    -1,    -1,    -1,    -1,
     403,    -1,   405,    -1,    -1,   408,   409,   410,   142,   412,
      -1,    -1,    -1,  1186,  1187,  1188,   419,    -1,    -1,    -1,
    1193,  1194,    -1,    -1,  1973,   428,   429,    -1,    -1,    -1,
     164,   165,    -1,    -1,    -1,    -1,    -1,   171,    -1,    -1,
      -1,   175,   176,    -1,    -1,   448,   449,    -1,   451,    -1,
      -1,    -1,    -1,   187,  2062,  1340,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1236,    -1,    -1,    -1,  1352,    -1,    -1,
     113,    -1,  1831,  1832,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   486,   487,    -1,    -1,   130,  1847,   132,
    1976,    -1,    -1,  1266,    -1,  1268,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,   510,    -1,    -1,
     513,    -1,   515,   516,   517,    -1,   519,    -1,    -1,    -1,
    1879,    -1,   165,    -1,    -1,   528,    -1,    -1,   531,   532,
     533,   534,    -1,    -1,    -1,  1894,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   546,    -1,    -1,   549,   550,    -1,    -1,
      -1,    -1,   555,   556,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1337,    -1,    -1,    79,  1927,    -1,
      -1,  1412,    -1,    -1,    -1,   113,  1935,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   590,    -1,    -1,
     593,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   234,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,  1973,    -1,  1975,   129,    -1,    -1,
      -1,   624,    -1,    -1,    -1,    -1,    86,    -1,    -1,   167,
     142,    -1,   144,    -1,    -1,    -1,  1995,  1996,    -1,    -1,
    1999,    -1,    -1,    -1,    -1,   648,   649,    -1,    -1,    -1,
     653,  2010,   164,   165,    -1,   658,   168,   660,    -1,   662,
      -1,   664,    79,   175,   176,   668,    -1,   670,    -1,    -1,
      -1,    -1,   675,   676,    -1,   187,    -1,    -1,    -1,    -1,
      -1,  2040,    -1,    -1,    -1,  2044,  2045,    -1,    -1,    -1,
      -1,    -1,    -1,   696,    -1,    -1,   113,    -1,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
      -1,    -1,   129,  1598,  1599,  2074,    -1,    -1,    -1,   722,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,   731,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1577,    -1,    -1,   742,
     743,    -1,    -1,    -1,   747,   748,  2105,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
      -1,    -1,   765,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    -1,    -1,    -1,    -1,   235,    -1,    -1,    -1,    -1,
     113,   784,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,   797,    -1,   113,    -1,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   811,   142,
     813,   113,    -1,    -1,   817,   117,   118,   119,   120,   121,
     122,   123,   124,   125,    -1,   828,   829,    -1,   130,    -1,
     132,   164,   165,    -1,    -1,   168,    -1,    -1,   841,   299,
      -1,    -1,   175,   176,    -1,    -1,    -1,    -1,   851,    -1,
    1735,   168,    -1,  1626,   187,   188,   859,  1742,    -1,    -1,
    1701,    -1,    -1,   165,   867,    -1,   168,    -1,   871,    -1,
      -1,    -1,    -1,    -1,   510,   878,    -1,   513,    -1,    -1,
      -1,    -1,    -1,   519,    -1,    -1,    -1,   890,  1773,    -1,
      -1,    -1,   528,    -1,    -1,    -1,    -1,    -1,   358,    -1,
      -1,    -1,   905,    -1,   907,    -1,    -1,    -1,    -1,   912,
      -1,   914,    -1,    -1,    -1,   918,    -1,    -1,    -1,    -1,
      -1,    -1,   558,    -1,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,   108,    -1,    -1,
      -1,    -1,   113,   142,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   967,   968,    -1,    -1,   428,    -1,
      -1,    -1,    -1,  1858,  1859,   164,   165,    -1,    -1,   168,
    1865,    -1,    -1,   986,    -1,   621,   175,   176,   448,    -1,
     993,    -1,    -1,  1878,    -1,    -1,    -1,    -1,   187,    -1,
      -1,  1004,  1005,  1888,  1777,  1890,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1903,  1022,
    1905,  1906,  1907,    -1,    -1,   111,   486,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,  1042,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,    -1,    -1,   129,    -1,  1943,    -1,
      -1,    -1,  1947,    -1,    -1,    -1,    -1,  1952,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,  1091,    -1,
      -1,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1103,    -1,   175,   176,  1107,    -1,   742,   743,    -1,  1112,
      -1,    -1,  1115,    -1,   187,    -1,  2001,    -1,    -1,    68,
      -1,    -1,  1125,  1126,    -1,    -1,    -1,  1130,  2013,    -1,
     590,    -1,  2017,    -1,    -1,    -1,    -1,    -1,    -1,  1142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2032,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   110,    -1,  1166,    -1,    -1,  1939,  1170,    -1,    -1,
      -1,    -1,    -1,   122,  1177,   124,    -1,   126,   113,    -1,
    1183,    -1,   117,   118,   119,   120,   121,   122,   123,   124,
     125,    -1,  2077,    -1,    -1,   130,    -1,   132,    -1,    -1,
     660,    -1,   662,   167,    -1,    79,    -1,    -1,    -1,    -1,
    1983,    -1,  1215,  1216,    -1,    -1,   165,    -1,    -1,   168,
     169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2114,
     165,    -1,    -1,   168,  2119,    -1,    -1,    -1,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,     1,   129,    -1,     4,    -1,  1262,
    2145,    -1,    -1,  2148,    -1,  2150,    -1,    -1,   142,  2042,
     144,    -1,  1275,    -1,    -1,   224,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2169,  1288,    -1,    -1,   748,  1292,
     164,   165,    -1,  1296,    -1,  2068,    -1,  2070,    -1,    -1,
      -1,   175,   176,    -1,    -1,  1308,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   187,    -1,    -1,  1319,    -1,    65,    -1,
      -1,    -1,   271,    -1,    -1,  1328,    -1,    -1,   277,    -1,
     279,    -1,    -1,    -1,  1337,  2108,    -1,    -1,    -1,    86,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    94,    -1,    -1,
      -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,
    1363,  1364,    -1,    -1,    -1,    -1,    -1,   114,  1371,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1395,  1396,    -1,    -1,    -1,    -1,    -1,    -1,
    2173,   148,    -1,    -1,    -1,   152,    -1,    -1,    -1,  1412,
      -1,   158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,  1424,    -1,    -1,    -1,   374,    -1,   376,   377,    -1,
     177,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,   905,    -1,    -1,    -1,    -1,
    1453,  1454,    -1,    -1,    -1,    -1,    -1,   204,    -1,   408,
      -1,   142,    -1,   412,    -1,    -1,    -1,    -1,    -1,    -1,
     419,    -1,    -1,    -1,  1477,    -1,    -1,    -1,  1481,    -1,
      -1,  1484,    -1,   164,   165,    -1,    -1,    -1,   235,   236,
      -1,    -1,    -1,    -1,   175,   176,    -1,    -1,    -1,    -1,
      -1,    -1,   451,   250,   453,   454,   187,   967,  1511,    -1,
      -1,    -1,  1515,    -1,    -1,    -1,  1519,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   272,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1537,  1538,  1539,  1540,  1541,  1542,
    1543,    -1,    -1,    -1,   291,    -1,  1549,   496,   295,    -1,
      -1,    -1,   299,    -1,    -1,    -1,    -1,    -1,   305,  1562,
      -1,    -1,  1022,  1566,  1567,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1577,    -1,    -1,  1580,    -1,    -1,
      -1,    -1,  1042,    -1,   331,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   340,    -1,    -1,    -1,    -1,    -1,    -1,
     549,    -1,    -1,    -1,   351,  1608,   555,    -1,    -1,   356,
     357,   358,   359,    -1,    13,    14,    15,    16,    17,    -1,
    1623,    -1,   369,    -1,    -1,    -1,    -1,  1630,  1631,    -1,
      -1,    -1,    -1,   380,    -1,    -1,   383,    -1,    -1,  1642,
     387,    -1,    -1,  1103,   593,   392,   595,   596,    -1,    -1,
      -1,   398,    -1,    -1,    -1,  1115,   403,    -1,   405,    -1,
      -1,    -1,   409,   410,    -1,    -1,    -1,    -1,  1304,    -1,
      -1,    -1,    -1,    -1,   421,    -1,    -1,    -1,    -1,    -1,
      79,   428,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,    -1,  1701,    -1,
      -1,   448,   449,    -1,   653,   654,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   113,   664,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,  1363,  1731,  1732,
     129,    -1,    -1,   682,   164,    -1,    -1,    -1,    -1,   486,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1215,  1216,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,   165,   716,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   534,   187,    -1,
      -1,   740,    -1,    -1,    -1,    -1,   745,    -1,   747,   546,
      -1,    -1,  1262,   550,    -1,    -1,    -1,    -1,    -1,   556,
      -1,    -1,    -1,    -1,    -1,  1275,    -1,    -1,    -1,    -1,
      -1,   770,    -1,   772,   773,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   784,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   590,    -1,    -1,    -1,    -1,   797,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1861,    -1,
     113,   810,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,  1876,    -1,    -1,  1879,   624,    -1,    -1,
      -1,   628,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1894,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   648,   649,    -1,  1364,    -1,    -1,    -1,   655,    -1,
      -1,    -1,    -1,   660,    -1,   662,    -1,    -1,    -1,    -1,
      -1,   668,    -1,   670,    -1,    -1,  1562,  1563,   675,   676,
    1566,  1567,    -1,    -1,    -1,    -1,  1572,    -1,    -1,    -1,
    1576,    -1,  1578,    -1,  1580,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1957,  1958,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   918,
    1973,    -1,    -1,  1976,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   731,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1453,  1454,    -1,    -1,    -1,    -1,    -1,
      -1,   748,    -1,    -1,    -1,    -1,    -1,  2010,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   765,    -1,
      -1,    -1,    -1,  2026,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   982,    -1,    -1,    -1,  2040,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1005,    -1,    -1,  2062,
      -1,  1010,    -1,    -1,   811,    -1,   813,    -1,    -1,    -1,
     817,  2074,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,   828,   829,    -1,   831,    -1,    -1,    -1,    -1,    -1,
      -1,  1727,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   851,   852,    -1,    -1,    -1,    -1,
      -1,  1747,  1748,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     867,    -1,    -1,    -1,   871,    -1,    -1,    -1,    -1,    -1,
      -1,   878,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    65,  1778,   890,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   905,    -1,
     907,    -1,    86,    -1,    -1,   912,    -1,   914,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1125,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1137,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     967,   968,    -1,    -1,   148,    -1,    -1,  1863,    -1,    -1,
      -1,    -1,   979,    -1,   158,  1871,    -1,  1873,    -1,   986,
    1876,  1877,    -1,  1879,    -1,    -1,   993,    -1,  1884,    -1,
      -1,    -1,    -1,   177,    -1,    -1,    -1,  1004,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1022,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1042,    -1,    -1,    -1,  1248,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   235,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1957,    -1,    -1,    -1,    -1,   250,    -1,  1964,  1965,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1091,    -1,    -1,    -1,   272,    -1,
      -1,  1987,    -1,    -1,    -1,    -1,  1103,    -1,    -1,    -1,
    1107,    -1,    -1,    -1,    -1,  1112,    -1,   291,  1115,    -1,
      -1,   295,    -1,    -1,    -1,   299,    -1,    -1,    -1,  1126,
      -1,    -1,  2018,    -1,  2020,    -1,    -1,  2023,  2024,    -1,
      -1,    -1,    -1,    -1,  2030,  2031,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   340,    -1,    -1,  1166,
      -1,    -1,    -1,  1170,    -1,    -1,    -1,    -1,    -1,    -1,
    1177,    -1,    -1,    -1,   358,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   369,  1395,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   380,    -1,    -1,   383,
      -1,  2097,  2098,  2099,    -1,    -1,    -1,    -1,  1215,  1216,
      -1,    -1,    -1,    -1,   398,    -1,    -1,    -1,    -1,    -1,
      -1,   405,    -1,    -1,    -1,   409,   410,    -1,    -1,  2125,
    2126,  2127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1247,    -1,    -1,    -1,   428,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1262,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   448,   449,    -1,    -1,  1275,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,  1288,    -1,    -1,    -1,  1292,    -1,    -1,    -1,  1296,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   486,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1319,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1337,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
    1549,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     534,    -1,    -1,    -1,    -1,    -1,    -1,  1364,    -1,    -1,
      -1,    -1,   546,    -1,  1371,    -1,   550,     1,    -1,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,   129,    -1,  1396,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1608,
     142,    -1,   144,    -1,    -1,    -1,   590,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1424,    -1,    -1,
      -1,    -1,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    65,    -1,   175,   176,    -1,    -1,    -1,    -1,    -1,
     624,    -1,    -1,    -1,    -1,   187,  1453,  1454,    -1,    -1,
      -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   648,   649,    -1,    -1,    -1,    -1,
    1477,    -1,    -1,    -1,  1481,    -1,   660,  1484,   662,    -1,
     114,    -1,    -1,    -1,   668,    -1,   670,    -1,    -1,    -1,
      -1,    -1,   676,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1511,    -1,    -1,    -1,  1515,    -1,
      -1,    -1,  1519,    -1,   148,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1731,    -1,   158,    -1,    -1,    -1,    -1,    -1,
    1537,  1538,  1539,  1540,  1541,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   177,    -1,    -1,    -1,   731,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1562,    -1,    -1,    -1,  1566,
    1567,    -1,    -1,    -1,   748,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1580,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   765,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   235,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,     4,    -1,    -1,    -1,   250,    -1,    -1,    -1,
      -1,    -1,    -1,  1630,  1631,    -1,    -1,   811,    -1,   813,
      -1,    -1,    -1,   817,    -1,    -1,    -1,    -1,   272,    -1,
    1849,    -1,    -1,    -1,   828,   829,    -1,    -1,    -1,    -1,
      -1,    -1,  1861,    -1,    -1,    -1,    -1,   291,    -1,    -1,
      -1,   295,    -1,    -1,    -1,   299,    -1,   851,    -1,    -1,
      -1,    -1,    -1,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   867,    -1,    -1,    -1,   871,    -1,    -1,
      -1,    -1,    -1,    -1,   878,    -1,    -1,    -1,    -1,    -1,
      -1,  1708,    94,    -1,    -1,    -1,   340,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1927,    -1,
      -1,   905,   114,   907,   358,  1732,    -1,    -1,   912,    -1,
     914,   123,    -1,    -1,    -1,   369,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   380,    -1,    -1,  1958,
    1959,    -1,    -1,    -1,    -1,    -1,   148,    -1,    -1,    -1,
     152,    -1,    -1,    -1,   398,    -1,   158,  1976,    -1,   161,
      -1,   405,    -1,    -1,    -1,   409,   410,    -1,    -1,    -1,
      -1,    -1,    -1,   967,   968,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   428,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   986,    -1,    -1,    -1,    -1,    -1,    -1,   993,
     202,    -1,    -1,    -1,   448,   449,    -1,    -1,    -1,    -1,
    1004,    -1,    -1,    -1,    -1,  1832,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1022,    -1,
      -1,    -1,   234,    -1,   236,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   486,    -1,    -1,    -1,    -1,    -1,  1042,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1876,
      -1,    -1,  1879,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1894,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2105,    -1,    -1,    -1,
     534,    -1,    -1,    -1,    -1,    -1,    -1,  1091,    -1,    -1,
      -1,    -1,   546,   305,    -1,    -1,   550,    -1,    -1,  1103,
      -1,    -1,    -1,  1107,    -1,    -1,    -1,    -1,  1112,    -1,
      -1,  1115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1126,    -1,    -1,    -1,    -1,    -1,   340,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   590,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   357,  1973,   359,    -1,    -1,
      -1,    -1,    -1,   365,    -1,    -1,    -1,   369,    -1,    -1,
      -1,    -1,  1166,    -1,    -1,    -1,  1170,    -1,   380,    -1,
     624,    -1,    -1,  1177,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2010,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   403,    -1,   405,   648,   649,    -1,   409,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   660,    -1,   662,    -1,
      -1,  1215,  1216,  2040,   668,    -1,   670,   429,  2045,    -1,
      -1,    -1,   676,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2062,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2074,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1262,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1275,    -1,    -1,    -1,   487,    -1,   731,    -1,    -1,
      -1,    -1,    -1,    -1,  1288,    -1,    -1,    -1,  1292,    -1,
      -1,    -1,  1296,    -1,   748,    -1,    -1,    -1,   510,    -1,
      -1,   513,    -1,   515,   516,    -1,    -1,   519,    -1,    -1,
      -1,   765,    -1,    -1,    -1,  1319,   528,    -1,    -1,   531,
     532,   533,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1337,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   556,    -1,     1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   811,    -1,   813,
    1364,    -1,    -1,   817,    -1,    -1,    -1,  1371,    -1,    -1,
      -1,    -1,    -1,    -1,   828,   829,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1396,    -1,    -1,    -1,    -1,   851,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      65,    -1,    -1,   867,    -1,    -1,    -1,   871,    -1,    -1,
    1424,    -1,    -1,    -1,   878,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    94,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1453,
    1454,   905,    -1,   907,    -1,    -1,    -1,    -1,   912,   114,
     914,    -1,    -1,   675,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1477,    -1,    -1,    -1,  1481,    -1,    -1,
    1484,    -1,    -1,    -1,   696,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,    -1,    -1,    -1,   152,    -1,    -1,
      -1,    -1,    -1,   158,    -1,    -1,   161,  1511,    -1,    -1,
      -1,  1515,    -1,   967,   968,  1519,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     742,   743,   986,    -1,    -1,    -1,    -1,    -1,    -1,   993,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   202,    -1,    -1,
    1004,    -1,    -1,   765,    -1,    -1,    -1,    -1,  1562,    -1,
      -1,    -1,  1566,  1567,    -1,    -1,    -1,    -1,  1022,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1580,    -1,    -1,   234,
      -1,   236,    -1,    -1,    -1,    -1,    -1,    -1,  1042,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   813,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1630,  1631,    -1,   841,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1091,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   859,    -1,  1103,
     305,    -1,    -1,  1107,    -1,    -1,    -1,    -1,  1112,    -1,
      -1,  1115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,  1126,    -1,    -1,    -1,    -1,    -1,   890,    -1,
      -1,    -1,    -1,    -1,    -1,   340,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   357,    -1,   359,    -1,    -1,    -1,    -1,    -1,
     365,    -1,  1166,    -1,   369,    -1,  1170,    -1,    -1,    -1,
      -1,    -1,    -1,  1177,    55,   380,    -1,    58,  1732,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   403,    80,
     405,    -1,    -1,    -1,   409,    -1,    -1,    -1,    -1,    -1,
      -1,  1215,  1216,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   429,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,  1262,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1275,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   487,   164,  1288,    -1,   167,   168,  1292,    -1,
      -1,    -1,  1296,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,   510,    -1,    -1,   513,    -1,
     515,   516,    -1,    -1,   519,  1319,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   528,    -1,  1879,   531,   532,   533,    -1,
      -1,    -1,    -1,  1337,    -1,    -1,    -1,    -1,    -1,    -1,
    1894,    -1,    -1,    -1,    -1,  1107,    -1,    -1,    -1,    -1,
      -1,   556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1364,    -1,    -1,    -1,    -1,    -1,    -1,  1371,  1130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,  1396,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1973,
    1424,  1183,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    65,    -1,  1453,
    1454,    -1,    -1,    -1,    -1,    -1,  2010,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     675,    -1,    -1,  1477,    -1,    -1,    94,  1481,    -1,    -1,
    1484,    -1,    -1,    -1,    -1,    -1,  2040,    -1,    -1,    -1,
      -1,   696,    -1,    -1,    -1,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,    65,    -1,    -1,    -1,  1511,  2062,    -1,
      -1,  1515,    -1,    -1,    -1,  1519,    -1,    -1,    -1,    -1,
    2074,    -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,
     148,    -1,    -1,    -1,   152,    -1,    -1,   742,   743,    -1,
     158,    -1,    -1,   161,    -1,    -1,    -1,    -1,   110,    -1,
      -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,  1562,    -1,
     765,    -1,  1566,  1567,    -1,    -1,  1328,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1337,  1580,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   202,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,
      -1,  1363,    -1,   165,    -1,    -1,    -1,    -1,   813,  1371,
      -1,    -1,    -1,    -1,    -1,    -1,   234,    -1,   236,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1630,  1631,    -1,    -1,
      -1,    -1,    -1,    -1,  1396,    -1,   841,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1412,    -1,    -1,    -1,   859,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   235,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,   890,    20,   305,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,   340,    57,    -1,    59,    -1,    -1,  1732,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   299,    -1,   357,
      -1,   359,    -1,    -1,    -1,    79,    -1,   365,    -1,    -1,
      -1,   369,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   380,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1537,  1538,  1539,  1540,  1541,
    1542,  1543,    -1,    -1,    -1,   403,    -1,   405,    -1,    -1,
      -1,   409,    -1,    -1,    -1,   129,   358,    -1,    -1,    -1,
    1562,    -1,    -1,    -1,  1566,  1567,    -1,    -1,    -1,    -1,
     144,   429,   374,    -1,    -1,  1577,    -1,    -1,  1580,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   428,    -1,    -1,   487,
      -1,    -1,  1876,    -1,    -1,  1879,    -1,    -1,    -1,    83,
    1642,    -1,    -1,    -1,    -1,    -1,   448,    -1,    -1,    -1,
    1894,    -1,   510,    -1,    -1,   513,    -1,   515,   516,    -1,
      -1,   519,  1107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     528,    -1,    -1,   531,   532,   533,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   486,  1130,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   556,  1701,
      -1,   145,    -1,   147,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1973,
    1732,    -1,    -1,    -1,   178,    -1,   180,    -1,  1183,    -1,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    -1,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   205,    -1,    -1,    -1,    -1,  2010,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,   220,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   590,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2040,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   675,  2062,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
    2074,   142,    -1,    -1,    -1,    -1,    -1,    -1,   696,   283,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,   660,    -1,
     662,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,   187,  1879,    -1,    -1,
      -1,    -1,    -1,  1328,   742,   743,   330,    -1,    -1,    -1,
     334,    -1,  1337,   337,   338,    -1,    -1,   341,    -1,    -1,
     344,   345,    -1,   347,    -1,   349,    -1,   765,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1363,    54,
      -1,    -1,    -1,    -1,    -1,    -1,  1371,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   748,    -1,    -1,    -1,
      -1,  1396,    -1,    -1,    -1,   813,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1412,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   420,    -1,    -1,   423,
      -1,    -1,    -1,   841,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,   859,    -1,    -1,    -1,    -1,    -1,    -1,  2010,    -1,
     145,   813,   147,    -1,    -1,   817,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   890,    -1,    -1,    -1,    -1,    -1,  2040,    -1,
      -1,    -1,    -1,   178,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   505,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2074,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   220,    -1,    -1,    -1,    -1,
      -1,    -1,  1537,  1538,  1539,  1540,  1541,  1542,  1543,    -1,
      -1,    -1,    -1,   905,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1562,    -1,    -1,
      -1,  1566,  1567,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1577,    -1,    -1,  1580,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   283,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   967,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   629,    -1,    -1,   632,   633,
      -1,   635,    -1,   637,   638,    -1,    -1,  1642,   642,   643,
      -1,    -1,   337,   338,    -1,    -1,   341,    -1,    -1,   344,
     345,    -1,   347,    -1,   349,    -1,    -1,    -1,    -1,    -1,
    1022,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1042,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1107,
      -1,    -1,    -1,    -1,    -1,    -1,  1701,    -1,    62,    63,
     704,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1130,    -1,    -1,    -1,   720,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1732,    -1,  1091,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,
      -1,    -1,    -1,    -1,    -1,  1107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1125,  1126,  1183,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,
      -1,    -1,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   808,    -1,    -1,   171,    -1,    -1,
     505,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,    -1,    -1,
     834,    -1,   836,    -1,    -1,    -1,    -1,    -1,    -1,   203,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   855,   856,  1215,  1216,    -1,    -1,    -1,   862,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1879,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1262,    -1,    -1,    -1,   268,    -1,    -1,    -1,    -1,    -1,
    1328,    -1,    -1,  1275,    -1,    -1,    -1,    -1,    -1,  1337,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   292,    -1,
      -1,    -1,   296,    -1,   629,    -1,    -1,   632,   633,    -1,
     635,    -1,   637,   638,    -1,  1363,    -1,   642,   643,    -1,
      -1,    -1,    -1,  1371,    -1,   319,    -1,  1319,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1337,   980,    -1,  1396,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1412,    -1,    -1,   361,    -1,    -1,
      -1,    -1,  1364,    -1,    -1,  2010,   370,   371,    -1,  1371,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   381,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   720,    -1,    -1,    -1,    -1,
      -1,    -1,   396,  1395,  1396,  2040,    -1,    -1,    -1,    86,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     414,  1055,    -1,    -1,    -1,  1059,    -1,    -1,    -1,  1063,
      -1,    -1,  1424,    -1,    -1,    -1,    -1,    -1,    -1,  2074,
      -1,    -1,    -1,    -1,   438,   439,    -1,    -1,    -1,   443,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1453,  1454,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   467,    -1,    -1,   470,    -1,    -1,   473,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1537,
    1538,  1539,  1540,  1541,  1542,  1543,    -1,    -1,    -1,    -1,
      -1,    -1,  1136,    -1,    -1,    -1,    -1,    -1,    -1,   834,
      -1,   836,    -1,    -1,  1562,    -1,    -1,    -1,  1566,  1567,
      -1,    -1,    -1,    -1,    -1,   202,    -1,    -1,    -1,  1577,
     855,   856,  1580,    -1,    -1,    -1,    -1,   862,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1540,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   235,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1562,    -1,    -1,    -1,  1566,  1567,    -1,   571,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1580,    -1,
      -1,    -1,    -1,    -1,  1642,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,    -1,  1240,    -1,    -1,  1243,
    1244,    -1,    -1,    -1,    -1,    -1,  1608,  1251,  1252,    -1,
      -1,    -1,   299,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   314,  1630,   316,
    1274,    55,  1276,    -1,    58,  1279,    60,    61,  1282,    63,
      -1,    -1,  1286,  1701,    -1,   980,    -1,    -1,   652,    -1,
      -1,    -1,    -1,    -1,    78,    -1,    80,    81,    -1,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,  1732,    -1,   100,   101,   102,   103,
     104,   105,   106,    -1,   108,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,    -1,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
    1055,    -1,    -1,   410,  1059,    -1,    -1,    -1,  1063,  1731,
    1732,   418,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,   172,    -1,
     174,   175,   176,   177,   178,   179,   180,    -1,    -1,    -1,
      -1,   448,    -1,    -1,    -1,   189,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1429,    -1,    -1,    -1,    -1,
      -1,    -1,   479,    -1,    -1,    -1,    -1,   801,    -1,   486,
      -1,  1136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   815,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     824,  1879,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1476,    -1,   521,    -1,    -1,    -1,    -1,    -1,
      -1,   845,    -1,    -1,    -1,    -1,    -1,   534,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1861,
      -1,    -1,   549,   550,    -1,   552,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   561,    -1,   563,  1879,    -1,    -1,
      -1,    -1,    -1,   570,    -1,   572,   573,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     587,    -1,    -1,   590,    -1,  1240,    -1,    -1,  1243,  1244,
      -1,    -1,    -1,    -1,    -1,    -1,  1251,  1252,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   625,  1274,
     627,  1276,    -1,    -1,  1279,  1589,    -1,  1282,    -1,    -1,
      -1,  1286,  2010,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   648,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   668,  2040,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   680,    -1,    -1,    -1,   684,   685,    -1,
      -1,    -1,    -1,    -1,   691,    -1,    -1,    -1,  2010,   696,
      -1,    -1,    -1,    -1,    -1,    -1,  2074,    -1,    -1,    -1,
    1024,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    86,  1678,   722,    -1,    -1,  2040,    -1,
      -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,    -1,    -1,
      -1,   748,  1706,    -1,    -1,    -1,    -1,    -1,  1712,    -1,
      -1,    -1,  2074,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1429,    -1,    -1,    -1,    -1,   152,
      -1,    -1,  1106,    -1,    -1,    -1,    -1,    -1,   161,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1476,  1146,    -1,  1788,    -1,    -1,    -1,    -1,   202,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   846,
      -1,    -1,    -1,    -1,  1808,  1809,    -1,    -1,    -1,    -1,
      -1,    -1,   859,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   234,   235,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   878,    -1,    -1,  1838,  1839,    -1,   250,    -1,    -1,
      -1,    -1,  1846,    -1,    -1,    -1,    -1,  1851,    -1,    -1,
      -1,   898,    -1,    -1,    -1,    -1,   903,    -1,   271,    -1,
      -1,    -1,    -1,    -1,   911,   912,    -1,    -1,    -1,    -1,
      -1,   918,    -1,    -1,    -1,    -1,   923,    -1,   291,    -1,
      -1,    -1,   295,    -1,    -1,    -1,   299,    -1,    -1,   302,
      -1,    -1,    -1,    -1,  1589,    -1,   943,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1929,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   358,   359,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1005,    -1,
      -1,   374,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1343,
    1344,  1345,    -1,  1678,    -1,  1032,    -1,    -1,    -1,    -1,
      -1,  1995,    -1,    -1,    -1,    -1,  1043,   410,    -1,   412,
      -1,    -1,    -1,    -1,    -1,    -1,   419,    -1,    -1,    -1,
      -1,    -1,    -1,  1377,    -1,   428,   429,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1398,   448,   449,    -1,   451,  1403,
    2044,    -1,    -1,  1407,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   486,   487,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1788,    -1,    -1,    -1,   510,    -1,    -1,
     513,    -1,   515,   516,    -1,    -1,   519,    -1,    -1,    -1,
      -1,    -1,    -1,  1808,  1809,   528,    -1,    -1,   531,   532,
     533,   534,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   546,    -1,    -1,   549,    -1,    -1,    -1,
      -1,    -1,    -1,  1838,  1839,    -1,    -1,    -1,    -1,  1196,
      -1,    -1,    -1,    -1,    -1,    -1,  1851,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   590,    -1,    -1,
     593,    -1,  1229,    -1,    -1,  1232,    -1,    -1,  1552,  1553,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1248,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1272,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1929,   648,   649,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   660,    -1,   662,
      -1,    -1,    -1,    -1,    -1,   668,    -1,   670,    -1,  1306,
    1624,  1308,    -1,   676,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   696,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1995,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1357,  1358,    -1,    -1,    -1,    -1,    -1,  1364,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   742,
     743,    -1,    -1,    -1,    -1,   748,    -1,    -1,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,  1716,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1448,    -1,  1450,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,   828,    -1,    -1,    -1,    -1,
      -1,    -1,  1469,    -1,    -1,    -1,    -1,    -1,   841,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1488,  1489,    -1,    -1,    -1,   859,   113,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,   878,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   905,    -1,   907,    -1,    -1,    -1,    -1,   912,
      -1,   914,    -1,    -1,    -1,   171,    -1,    -1,  1555,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1574,    -1,    -1,
    1577,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   967,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1004,  1005,  1640,    -1,    -1,  1643,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1022,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1664,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1042,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1687,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,  1701,    -1,    -1,    18,    -1,    20,
    1707,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,  1112,
      -1,    62,  1115,    -1,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,  1130,    -1,    -1,
      -1,  1768,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1797,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1183,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1215,  1216,    -1,   166,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   189,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,  1262,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,  1275,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,  1328,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1984,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1363,  1364,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1395,   142,    -1,   144,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1412,
      -1,    -1,    -1,    -1,   163,    -1,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    -1,
      60,    61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,
    1453,  1454,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,
      80,    81,    -1,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,    -1,   108,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,    -1,
     130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,  1537,  1538,  1539,    -1,    -1,  1542,
    1543,    -1,    -1,   163,   164,    -1,  1549,   167,   168,    -1,
      18,    -1,   172,    -1,   174,   175,   176,   177,   178,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   189,
      -1,    -1,    -1,    -1,  1577,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1608,    -1,    -1,    -1,    -1,
      78,    -1,    80,    81,    -1,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,  1642,
     108,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,    -1,   130,   131,   132,     1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,  1701,   167,
     168,    -1,    -1,    -1,   172,    -1,   174,   175,   176,   177,
     178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    55,
      -1,   189,    58,    -1,    60,    61,    -1,    63,  1731,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    78,    -1,    80,    81,    -1,    83,    -1,    -1,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,    -1,   108,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,    -1,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,    -1,   172,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   189,    -1,    -1,    -1,    -1,  1861,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1894,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    -1,
    1973,    -1,    76,  1976,    78,    79,    80,    81,    -1,    83,
      -1,    -1,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,    -1,   108,    -1,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,   172,    -1,
     174,   175,   176,   177,   178,   179,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   189,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    -1,    -1,    -1,    76,
      -1,    78,    79,    80,    81,    -1,    83,    -1,    -1,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
      -1,   108,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,
     167,   168,    -1,    -1,    -1,   172,    -1,   174,   175,   176,
     177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   189,     3,     4,     5,     6,     7,     8,     9,
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
     130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,   159,
     160,    -1,    -1,    -1,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   189,
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
      -1,    -1,    -1,    -1,   157,   158,   159,   160,    -1,    -1,
      -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   189,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    -1,    -1,    -1,
      76,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,    -1,    -1,    -1,   112,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,   128,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,   165,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   187,    -1,   189,     3,     4,     5,     6,     7,     8,
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
      -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,   172,    -1,    -1,   175,   176,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,     3,
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
     124,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   175,   176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   187,     4,     5,     6,     7,     8,     9,
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
      -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   175,   176,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,     4,     5,
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
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,   165,
      -1,   167,   168,    -1,    -1,    -1,   172,    -1,    -1,   175,
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
      -1,    -1,    -1,   175,   176,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   187,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    -1,    -1,    -1,    76,
      -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,   129,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,     3,     4,     5,     6,     7,     8,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
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
      -1,    -1,   164,   165,    -1,   167,   168,   169,   170,    -1,
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
     165,    -1,   167,   168,   169,   170,    -1,    -1,    -1,   174,
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
      -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,   169,
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
      -1,   164,    -1,    -1,   167,   168,   169,    -1,    -1,    -1,
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
     168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
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
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
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
     177,   178,   179,   180,     1,    -1,     3,     4,     5,     6,
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
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    -1,    -1,    -1,    76,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   112,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
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
      71,     5,    -1,    -1,    -1,    -1,    -1,    -1,    79,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    55,    -1,    -1,    58,    -1,    60,    61,   129,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    79,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,   110,   111,    -1,   113,
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
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,   165,
      -1,   167,   168,    -1,    -1,    -1,   172,    -1,    -1,   175,
     176,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,   187,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   175,   176,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,   187,    22,    23,
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
     124,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   175,   176,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    20,   187,    22,    23,    24,    25,    26,    27,
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
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
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
      -1,    -1,   175,   176,     3,     4,     5,     6,     7,     8,
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
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,    -1,    -1,   167,   168,
      -1,     3,    -1,     5,    -1,    -1,   175,   176,    10,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   163,    -1,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,     3,     4,     5,     6,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,    13,    14,    15,    16,    17,   175,   176,
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
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    55,
      -1,    -1,    58,    -1,    60,    61,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    78,    -1,    80,    81,    -1,    83,   175,   176,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    -1,    -1,   100,   101,   102,   103,   104,   105,
     106,    -1,   108,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,    -1,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,    -1,   172,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   189,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,    -1,    -1,   172,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,    -1,    -1,   172,     3,     4,     5,     6,     7,     8,
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
      -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,    -1,    -1,   172,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,    97,    -1,
      99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,
      -1,    -1,    -1,   172,    -1,   174,   175,   176,   177,   178,
     179,   180,    13,    14,    15,    16,    17,    18,    19,    20,
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
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,   172,    -1,   174,   175,   176,   177,   178,   179,   180,
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
      -1,   164,    -1,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    13,    14,
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
      -1,    -1,   167,   168,    -1,    -1,    -1,   172,    -1,   174,
     175,   176,   177,   178,   179,   180,    13,    14,    15,    16,
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
     167,   168,    -1,    -1,    -1,   172,    -1,   174,   175,   176,
     177,   178,   179,   180,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,    13,    14,    15,    16,    17,    18,    -1,    20,
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
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,   159,   160,    -1,    -1,    -1,   164,
     165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,     3,     4,     5,     6,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,
      -1,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
     167,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   144,   145,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    13,    14,    15,
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
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
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
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,   129,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,
     145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   166,   167,     4,     5,     6,     7,     8,     9,    10,
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
      -1,    -1,    -1,   144,   145,    -1,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,   167,    22,    23,    24,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,   167,    22,    23,    24,    25,    26,    27,    28,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
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
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    85,    -1,
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
     175,   176,     4,     5,     6,     7,     8,     9,    10,    11,
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
      -1,   167,    -1,    -1,    13,    14,    15,    16,    17,   175,
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
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    13,    14,
      15,    16,    17,   175,   176,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,   169,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,   167,    22,    23,    24,    25,    26,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
     167,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    79,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,   115,   116,    57,    -1,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,   144,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    20,   144,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,    79,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,   144,    55,    -1,    -1,    58,    -1,    60,    61,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
     144,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    -1,   134,   135,    -1,    -1,    55,    -1,    -1,    58,
     142,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,   158,   159,   160,    -1,
      -1,    80,   164,   165,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    55,   134,   135,    58,    -1,    60,
      61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,   172,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    55,   134,   135,    58,    -1,    60,    61,    -1,
      63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,   170,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,   172,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,
     165,    -1,   167,   168,   169,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,
     167,   168,    -1,    -1,    -1,   172,    -1,   174,   175,   176,
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
     171,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,   169,    -1,    -1,    -1,
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
      -1,    80,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,
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
      -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,   172,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,
      -1,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,    55,
      -1,    -1,    58,   142,    60,    61,    -1,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     126,   127,    -1,    -1,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   191,   417,   418,     3,     4,     5,     6,     7,     8,
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
     351,   357,   358,   359,   360,   371,   376,   409,   412,   422,
     428,   430,   436,   440,   445,   446,   447,   448,   449,   450,
     451,   452,   478,   496,   497,   498,   499,     0,   191,   113,
     195,   209,   308,   310,   321,   324,   327,   337,   341,   346,
     127,   164,    65,    68,    69,    71,   164,   164,   374,   434,
     435,   436,   333,   334,   115,   116,   195,   197,   410,   411,
     197,   164,   422,   164,   164,     4,   113,   115,   116,   325,
     330,   331,   164,   164,   209,   435,   440,   446,   447,   448,
     450,   451,   452,   115,   348,    55,    58,    60,    61,    63,
      64,    80,   110,   111,   113,   114,   125,   126,   127,   130,
     131,   132,   134,   135,   164,   168,   169,   174,   175,   177,
     178,   179,   180,   193,   194,   198,   199,   200,   203,   208,
     209,   210,   211,   214,   215,   216,   217,   218,   219,   220,
     221,   222,   223,   224,   225,   230,   235,   310,   311,   320,
     322,   324,   328,   329,   336,   337,   343,   344,   345,   346,
     347,   350,   357,   358,   376,   383,   384,   385,   386,   387,
     388,   478,   491,   492,   493,   494,   499,   500,   195,   168,
     311,   321,   324,   445,   449,   478,   495,   496,   499,   500,
     189,   189,   192,   161,   172,   188,   233,   392,    96,   170,
     429,   108,   197,   433,   170,   170,   170,   189,   115,   116,
     164,   209,   316,   317,   440,   441,   442,   443,   444,   445,
     449,   453,   454,   455,   456,   457,   458,   459,   460,   461,
     467,     3,    53,    54,    56,    62,   339,     3,   168,   209,
     310,   311,   325,   329,   331,   342,   347,   425,   445,   449,
     499,    76,   308,   310,   324,   337,   341,   346,   426,   445,
     449,    72,   330,   330,   325,   331,   319,   330,   331,   339,
     358,   325,   330,   325,   167,   434,   170,   192,   164,   172,
     241,   434,   434,     3,   299,   300,   315,   318,   324,   328,
     168,   321,   324,   497,   499,   197,   197,   422,   188,   324,
     164,   209,   431,   440,   441,   445,   454,   458,   168,   209,
     311,   423,   424,    64,    72,    73,    74,    75,   168,   186,
     197,   398,   400,   404,   406,   407,   347,   166,   168,   209,
     320,   324,   337,   344,   346,   388,   491,   499,   434,   115,
     116,   179,   195,   347,   375,   467,   436,   164,   405,   406,
     164,    13,    95,   164,   197,   437,   438,   439,   198,   168,
     208,   209,   225,   226,   347,   166,   168,   209,   230,   321,
     390,   391,   408,   495,   500,   437,   324,   446,   447,   448,
     450,   451,   452,   166,   166,   166,   166,   166,   166,   166,
     164,   208,   164,   164,   208,   164,   164,   434,   211,   164,
     208,   164,   113,   115,   116,   325,   330,   331,   164,   208,
     208,    19,    21,    92,   168,   177,   178,   212,   213,   230,
     237,   241,   360,   390,   499,   165,   169,   170,   230,   324,
     328,   115,   168,   195,   321,   324,   478,   497,   164,   200,
     169,   168,   173,   168,   173,   127,   131,   133,   134,   135,
     164,   167,   168,   172,   173,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,   156,   188,   232,   233,   234,
     168,   211,   322,   324,   337,   344,   346,   490,   491,   499,
     500,   211,   181,   175,   182,   183,   177,   178,   136,   137,
     138,   139,   184,   185,   140,   141,   176,   174,   186,   142,
     143,   187,   169,   164,   164,   168,   176,   188,   209,   440,
     462,   463,   464,   465,   466,   467,   468,   469,   470,   478,
     480,   481,   482,   483,   484,   485,   502,   145,   168,   209,
     350,   493,   499,   324,   344,   330,   325,   167,   434,   169,
     170,   169,   170,   322,   324,   492,   499,   197,   168,   322,
     478,   492,   499,   164,   197,   169,   168,   445,   449,   499,
     168,   170,   113,   167,   168,   172,   194,   196,   230,   393,
     394,   395,   396,   397,    22,   393,   164,   197,   241,   164,
     164,   195,   431,   195,   435,   440,   442,   443,   444,   453,
     455,   456,   457,   459,   460,   461,   324,   441,   454,   458,
     170,   433,   168,   434,   475,   478,   433,   434,   434,   429,
     299,   164,   434,   475,   433,   434,   434,   429,   434,   434,
     324,   431,   164,   164,   323,   324,   321,   324,   168,   169,
     321,   495,   500,   433,   349,   172,   429,   299,   197,   197,
     392,   310,   329,   427,   445,   449,   172,   429,   299,   410,
     324,   337,   324,   324,   115,   348,   115,   116,   195,   347,
     352,   410,   145,   195,   324,   380,   381,   385,   386,   389,
     163,   191,   241,   315,   189,   445,   458,   324,   175,   230,
     384,   499,   197,   433,   164,   433,   192,   230,   435,   440,
     324,   164,   384,   420,   172,   164,   197,   172,   197,   145,
     175,   176,   403,   166,   170,   197,   407,   166,   169,   164,
     176,   209,   499,   166,   195,   375,   467,   372,   172,   375,
     398,   188,   398,   437,   166,   170,   164,   166,   230,   166,
     170,   164,   209,   471,   472,   473,   474,   475,   166,   170,
     166,   166,   166,   166,   166,   166,   166,   164,   434,   475,
     478,   164,   475,   478,   390,   500,   200,   390,   168,   390,
     391,   195,   390,   500,   230,   390,   166,   390,   390,   390,
     169,   166,   177,   178,   213,    18,   326,   166,   170,   166,
     175,   176,   166,   170,   501,   164,   322,   478,   492,   499,
     169,   170,   168,   175,   209,   230,   347,   230,   324,   164,
     164,   321,   497,   172,   230,   195,   230,   195,   125,   168,
     195,   187,   227,   228,   229,   230,   125,   168,   197,   360,
     230,   227,   195,   172,   230,   499,   211,   214,   214,   214,
     215,   215,   216,   216,   217,   217,   217,   217,   218,   218,
     219,   220,   221,   222,   223,   171,   237,   191,   164,   380,
     440,   463,   464,   465,   468,   481,   482,   483,   169,   191,
      18,   230,   324,   462,   466,   480,   164,   434,   484,   502,
     434,   434,   502,   164,   434,   484,   434,   434,   502,   434,
     434,   478,   169,   226,   169,   324,   322,   490,   500,   197,
     324,   168,   195,   195,   384,   387,   387,   388,   502,   322,
     492,   499,   191,   502,   191,   168,   196,   225,   226,   432,
     394,   171,   170,   501,   393,   167,   168,   188,   397,   408,
     164,   198,   191,   188,   440,   442,   443,   444,   453,   455,
     456,   457,   459,   460,   461,   166,   166,   166,   166,   166,
     166,   166,   166,   166,   166,   441,   454,   458,   434,   188,
     169,   230,   331,   347,   476,   392,   241,   429,   380,   392,
     241,   431,   237,   391,   237,   391,   431,   115,   420,   241,
     429,   172,   172,   429,   299,   420,   241,   429,   354,   355,
     353,   172,   166,   170,   166,   170,    77,   301,   302,   189,
     169,   169,   170,   197,   433,   191,   440,   422,   420,   197,
     169,     1,   308,   310,   322,   324,   413,   414,   415,   416,
     164,   402,   400,   401,    85,   335,    18,   324,   434,   172,
     434,   375,    10,   174,   375,   377,   378,   172,   166,   391,
     166,   166,   438,   227,   189,   198,   380,   472,   473,   474,
     324,   471,   434,   434,   230,   391,   164,   434,   475,   478,
     164,   475,   478,   380,   380,   166,   166,   170,   166,   170,
     166,   166,   166,   170,   166,   211,   166,   166,   166,   211,
      18,   326,   230,   166,   166,   165,   172,   211,   165,   230,
     236,   169,   145,   382,   383,   384,   322,   492,   499,   169,
     236,   169,   169,   169,   230,   191,   191,   227,   169,   169,
     125,   130,   132,   196,   204,   205,   206,   195,   166,   170,
     204,   169,   170,   163,   394,   225,   171,   382,   468,   166,
     166,   166,   166,   166,   166,   166,   166,     5,   324,   164,
     434,   440,   467,   462,   466,   480,   380,   380,   169,   502,
     204,   169,   170,   382,   197,   204,   145,   169,   180,   169,
     501,   393,   395,   163,   166,   191,   166,   382,   230,   166,
     166,   166,   166,   166,   166,   166,   166,   166,   164,   434,
     475,   478,   164,   434,   475,   478,   164,   434,   475,   478,
     431,    22,   478,   158,   170,   180,   477,   169,   170,   241,
     166,   166,   166,   166,   166,   418,   419,   241,   163,   413,
     420,   241,   429,   419,   241,   172,   172,   172,   361,   145,
     385,   386,   195,   197,   303,    18,    78,    80,    81,    83,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,   100,   101,   102,   103,   104,   105,   106,   108,
     115,   116,   128,   164,   168,   197,   237,   238,   239,   240,
     241,   242,   243,   245,   246,   255,   262,   263,   264,   265,
     266,   267,   272,   273,   276,   277,   278,   279,   280,   281,
     282,   288,   289,   290,   304,   324,   328,   430,    77,   433,
     382,   421,   419,   166,   431,   163,   414,   170,   189,   170,
     189,   408,   188,   399,   399,   373,   377,   375,   172,   347,
     170,   501,   197,   377,   172,   166,   166,   166,   166,   166,
     166,   166,   471,   380,   380,   166,   166,   320,   195,    85,
     201,   202,   390,   211,   211,   211,   211,   211,   172,   394,
     170,   501,   166,   170,   170,   501,   169,   382,   382,   163,
     207,   168,   205,   207,   207,   169,   170,   133,   167,   188,
     229,   169,   236,   501,   225,   192,   166,   164,   434,   475,
     478,   164,   434,   484,   164,   434,   484,   478,   323,     5,
     175,   192,   230,   440,   434,   434,   166,   166,   169,   387,
     192,   392,   169,   226,   226,   163,   393,   434,   382,   434,
     192,   164,   434,   475,   478,   164,   434,   475,   478,   164,
     434,   475,   478,   380,   380,   380,   433,   237,   230,   230,
     331,   347,   421,   163,   419,   241,   421,   361,   361,   361,
       3,     5,    10,    80,   163,   305,   312,   313,   321,   324,
     362,   367,   495,   170,   189,   164,    68,    69,   189,   241,
     304,   430,   164,   164,    18,   239,   164,   164,   189,   197,
     189,   197,   175,   197,   172,   238,   164,   164,   164,   239,
     164,   241,   230,   231,   231,    14,   291,   267,   278,   171,
     189,   192,   243,    85,   189,   197,    98,    99,   271,   275,
     119,   143,   270,   118,   142,   274,   270,   389,   324,   303,
     192,   421,   197,   197,   431,   166,   391,   405,   405,   375,
     501,   172,   377,    10,   378,   163,   188,   379,   501,   163,
     413,   164,   434,   475,   478,   166,   166,   479,   480,   166,
     171,   166,   170,   171,   394,   501,   165,   230,   169,   145,
     384,   145,   169,   192,   192,   131,   204,   205,   168,   205,
     168,   205,   230,   169,   170,   163,   166,   380,   380,   380,
     230,   230,   192,   169,   192,   166,   169,   192,   166,   380,
     380,   380,   166,   166,   166,   392,   169,   477,   163,   421,
     163,   163,   163,   163,   321,   321,   360,   368,   495,   321,
     367,   164,   356,   189,   189,   189,   164,   171,   209,   363,
     364,   370,   440,   441,   454,   458,   170,   189,   197,   197,
     227,   189,   241,   189,   241,   237,   247,   304,   306,   309,
     315,   324,   328,   237,    87,   166,   247,   157,   158,   159,
     160,   165,   166,   189,   237,   256,   257,   259,   304,   189,
     189,   237,   189,   394,   189,   237,   408,   237,   256,   120,
     121,   122,   123,   124,   283,   285,   286,   189,   107,   189,
      91,   164,   166,   434,   163,   189,   189,   164,   164,   239,
     239,   267,   164,   277,   267,   277,   241,   189,   166,   163,
     403,   172,   163,   377,   501,   347,   197,   172,   226,   163,
     163,   380,   166,   230,   202,   230,   501,   163,   166,   166,
     169,   204,   204,   166,   166,   166,   192,   192,   169,   169,
     166,   434,   166,   166,   166,   230,   163,   356,   356,   356,
     363,   164,   209,   365,   366,   475,   486,   487,   488,   489,
     189,   170,   189,   363,   189,   408,   435,   440,   230,   324,
     163,   170,   189,   369,   370,   369,   369,   197,   166,   166,
     237,   324,   166,   164,   239,   166,   157,   158,   159,   160,
     180,   189,   260,   261,   239,   238,   189,   261,   166,   171,
     237,   165,   237,   238,   259,   189,   501,   166,   166,   166,
     166,   241,   285,   286,   164,   230,   164,   198,     1,   239,
     211,   268,   237,    82,   117,   269,   271,    82,   434,   399,
     377,   501,   163,   379,   394,   166,   163,   434,   434,   169,
     169,   169,   169,   189,   487,   488,   489,   324,   486,   170,
     189,   434,   434,   189,   166,   440,   434,   239,   239,    84,
      85,   172,   250,   251,   252,   166,   237,    82,   239,   237,
     165,   237,    82,   189,   165,   237,   238,   259,   324,   346,
     165,   237,   239,   257,   261,   261,   189,   237,   163,   172,
     252,   239,   239,   164,   287,   322,   324,   495,   189,   198,
     166,   171,   166,   170,   171,   166,   239,   164,   239,   239,
     239,   405,   501,   163,   501,   166,   166,   166,   486,   434,
     364,    82,     1,   226,   248,   249,   432,     1,   171,     1,
     191,   239,   250,    82,   189,   166,   239,    82,   189,   180,
     180,   239,   238,   261,   261,   189,    64,   237,   258,   347,
     180,   180,    82,   165,   237,   165,   237,   238,   189,     1,
     191,   287,   189,   284,   164,   209,   431,   486,   195,   171,
     189,   168,   198,   292,   293,   294,   211,   227,   237,   270,
     163,   163,   164,   434,   475,   478,   366,   239,   145,     1,
     170,   171,   163,   297,   298,   304,   239,    82,   189,   239,
     237,   165,   165,   237,   165,   237,   165,   237,   238,   195,
     347,   165,   237,   165,   237,   239,   180,   180,   180,   180,
     163,   297,   284,   225,   166,   324,   171,   113,   164,   166,
     171,   170,   166,   166,    82,   266,   380,   226,   248,   251,
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
     362,   363,   363,   363,   364,   364,   364,   364,   365,   365,
     365,   366,   367,   367,   368,   368,   369,   369,   370,   371,
     371,   372,   371,   371,   373,   371,   371,   371,   374,   374,
     375,   375,   376,   376,   377,   377,   377,   377,   377,   378,
     378,   379,   379,   379,   380,   380,   380,   380,   381,   381,
     381,   381,   382,   382,   382,   382,   382,   382,   382,   383,
     383,   383,   383,   384,   384,   385,   385,   386,   386,   387,
     387,   387,   387,   387,   388,   388,   388,   388,   388,   389,
     389,   390,   390,   390,   391,   391,   392,   392,   392,   392,
     393,   393,   394,   394,   394,   394,   394,   395,   395,   396,
     396,   397,   397,   397,   397,   397,   398,   398,   399,   399,
     401,   400,   402,   400,   400,   400,   400,   403,   403,   403,
     403,   404,   404,   404,   404,   405,   405,   406,   406,   407,
     407,   408,   408,   408,   408,   409,   409,   409,   410,   410,
     411,   411,   412,   412,   412,   412,   413,   413,   414,   414,
     415,   415,   415,   416,   416,   416,   417,   417,   418,   418,
     419,   419,   420,   421,   422,   422,   422,   422,   422,   422,
     422,   422,   422,   422,   422,   423,   422,   424,   422,   425,
     422,   426,   422,   427,   422,   422,   428,   428,   428,   429,
     429,   430,   430,   430,   430,   430,   430,   430,   430,   430,
     430,   431,   431,   431,   431,   432,   433,   433,   434,   434,
     435,   435,   436,   436,   436,   436,   437,   437,   438,   438,
     438,   439,   439,   439,   440,   440,   440,   441,   441,   441,
     441,   442,   442,   442,   442,   443,   443,   443,   443,   443,
     443,   443,   444,   444,   444,   444,   445,   445,   445,   446,
     446,   446,   446,   446,   447,   447,   447,   447,   448,   448,
     448,   448,   448,   448,   449,   449,   449,   450,   450,   450,
     450,   450,   451,   451,   451,   451,   452,   452,   452,   452,
     452,   452,   453,   453,   454,   454,   454,   454,   455,   455,
     455,   455,   456,   456,   456,   456,   456,   456,   456,   457,
     457,   457,   457,   458,   458,   458,   459,   459,   459,   459,
     459,   460,   460,   460,   460,   461,   461,   461,   461,   461,
     461,   462,   462,   462,   462,   462,   463,   463,   463,   464,
     464,   464,   464,   465,   465,   465,   466,   466,   466,   466,
     466,   467,   467,   468,   468,   468,   469,   469,   470,   470,
     471,   471,   471,   472,   472,   472,   472,   472,   473,   473,
     473,   473,   474,   474,   474,   475,   475,   475,   475,   475,
     476,   476,   476,   476,   476,   476,   477,   477,   478,   478,
     478,   478,   479,   479,   480,   480,   480,   480,   481,   481,
     481,   481,   481,   482,   482,   482,   482,   483,   483,   483,
     484,   484,   484,   485,   485,   485,   485,   485,   485,   486,
     486,   486,   487,   487,   487,   487,   487,   488,   488,   488,
     488,   489,   489,   490,   490,   490,   491,   491,   491,   492,
     492,   492,   492,   492,   492,   493,   493,   493,   493,   493,
     493,   493,   493,   493,   493,   493,   493,   493,   493,   493,
     494,   494,   494,   494,   495,   495,   495,   496,   496,   497,
     497,   497,   497,   497,   497,   498,   498,   498,   498,   498,
     498,   499,   499,   499,   500,   500,   500,   501,   501,   502,
     502
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
       2,     2,     3,     3,     5,     4,     1,     3,     0,     2,
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
       1,     2,     6,     4,     4,     1,     1,     3,     0,     1,
       4,     1,     1,     1,     1,     2,     3,     2,     1,     2,
       2,     2,     3,     4,     5,     2,     4,     5,     4,     5,
       3,     4,     6,     7,     3,     4,     2,     1,     2,     4,
       6,     7,     3,     4,     2,     3,     4,     5,     4,     5,
       4,     5,     3,     4,     1,     1,     1,     4,     6,     7,
       3,     4,     2,     3,     3,     4,     4,     5,     4,     5,
       3,     4,     1,     3,     2,     1,     2,     2,     2,     3,
       4,     5,     2,     4,     5,     4,     5,     3,     4,     6,
       7,     3,     4,     2,     1,     2,     4,     6,     7,     3,
       4,     2,     3,     4,     5,     4,     5,     4,     5,     3,
       4,     2,     4,     1,     2,     2,     2,     3,     4,     2,
       4,     4,     3,     4,     6,     3,     2,     4,     1,     2,
       2,     1,     1,     2,     3,     4,     2,     4,     4,     6,
       1,     2,     2,     1,     2,     2,     3,     4,     1,     4,
       4,     3,     3,     6,     3,     2,     3,     5,     3,     1,
       1,     1,     3,     3,     3,     5,     1,     1,     3,     3,
       4,     4,     0,     1,     1,     3,     2,     2,     1,     2,
       2,     3,     4,     1,     4,     4,     3,     3,     6,     3,
       1,     2,     1,     2,     6,     5,     6,     7,     7,     1,
       2,     2,     1,     2,     2,     3,     4,     1,     4,     4,
       3,     6,     3,     1,     1,     2,     1,     1,     2,     2,
       3,     2,     3,     2,     3,     3,     3,     2,     2,     4,
       4,     3,     3,     2,     2,     3,     2,     4,     3,     2,
       4,     4,     4,     5,     1,     2,     1,     1,     1,     2,
       3,     2,     3,     2,     3,     3,     4,     2,     3,     4,
       2,     3,     4,     5,     5,     6,     6,     0,     1,     0,
       2
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
#line 641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 9927 "Parser/parser.cc"
    break;

  case 3:
#line 645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 9933 "Parser/parser.cc"
    break;

  case 4:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 9939 "Parser/parser.cc"
    break;

  case 5:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9945 "Parser/parser.cc"
    break;

  case 6:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9951 "Parser/parser.cc"
    break;

  case 7:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9957 "Parser/parser.cc"
    break;

  case 8:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 9963 "Parser/parser.cc"
    break;

  case 20:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 9969 "Parser/parser.cc"
    break;

  case 24:
#line 688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 9975 "Parser/parser.cc"
    break;

  case 25:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 9981 "Parser/parser.cc"
    break;

  case 26:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 9991 "Parser/parser.cc"
    break;

  case 27:
#line 705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 9997 "Parser/parser.cc"
    break;

  case 28:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 10003 "Parser/parser.cc"
    break;

  case 29:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 10009 "Parser/parser.cc"
    break;

  case 31:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10015 "Parser/parser.cc"
    break;

  case 32:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 10021 "Parser/parser.cc"
    break;

  case 33:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10027 "Parser/parser.cc"
    break;

  case 34:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10033 "Parser/parser.cc"
    break;

  case 35:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 10043 "Parser/parser.cc"
    break;

  case 36:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 10049 "Parser/parser.cc"
    break;

  case 37:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 10055 "Parser/parser.cc"
    break;

  case 38:
#line 734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 10061 "Parser/parser.cc"
    break;

  case 39:
#line 736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10067 "Parser/parser.cc"
    break;

  case 40:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10073 "Parser/parser.cc"
    break;

  case 41:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10079 "Parser/parser.cc"
    break;

  case 43:
#line 746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 10091 "Parser/parser.cc"
    break;

  case 44:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 10100 "Parser/parser.cc"
    break;

  case 45:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 10106 "Parser/parser.cc"
    break;

  case 47:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 10112 "Parser/parser.cc"
    break;

  case 48:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10118 "Parser/parser.cc"
    break;

  case 49:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10124 "Parser/parser.cc"
    break;

  case 50:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10130 "Parser/parser.cc"
    break;

  case 51:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 10140 "Parser/parser.cc"
    break;

  case 52:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10146 "Parser/parser.cc"
    break;

  case 53:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_va_arg( yylloc, (yyvsp[-4].expr), ( (yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl) ) ) ); }
#line 10152 "Parser/parser.cc"
    break;

  case 54:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10158 "Parser/parser.cc"
    break;

  case 55:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10164 "Parser/parser.cc"
    break;

  case 56:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10170 "Parser/parser.cc"
    break;

  case 57:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10176 "Parser/parser.cc"
    break;

  case 58:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10182 "Parser/parser.cc"
    break;

  case 59:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10188 "Parser/parser.cc"
    break;

  case 60:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10194 "Parser/parser.cc"
    break;

  case 61:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 10200 "Parser/parser.cc"
    break;

  case 62:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10206 "Parser/parser.cc"
    break;

  case 63:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10212 "Parser/parser.cc"
    break;

  case 64:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10218 "Parser/parser.cc"
    break;

  case 65:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 10224 "Parser/parser.cc"
    break;

  case 66:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 10230 "Parser/parser.cc"
    break;

  case 67:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 10236 "Parser/parser.cc"
    break;

  case 68:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 10242 "Parser/parser.cc"
    break;

  case 69:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 10252 "Parser/parser.cc"
    break;

  case 71:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10258 "Parser/parser.cc"
    break;

  case 73:
#line 857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10264 "Parser/parser.cc"
    break;

  case 74:
#line 859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10270 "Parser/parser.cc"
    break;

  case 75:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10276 "Parser/parser.cc"
    break;

  case 76:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10282 "Parser/parser.cc"
    break;

  case 77:
#line 865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10288 "Parser/parser.cc"
    break;

  case 78:
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10294 "Parser/parser.cc"
    break;

  case 79:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10300 "Parser/parser.cc"
    break;

  case 80:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10306 "Parser/parser.cc"
    break;

  case 81:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 10314 "Parser/parser.cc"
    break;

  case 82:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10320 "Parser/parser.cc"
    break;

  case 83:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 10329 "Parser/parser.cc"
    break;

  case 86:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10335 "Parser/parser.cc"
    break;

  case 87:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 10341 "Parser/parser.cc"
    break;

  case 88:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10361 "Parser/parser.cc"
    break;

  case 89:
#line 920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 10367 "Parser/parser.cc"
    break;

  case 90:
#line 922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 10373 "Parser/parser.cc"
    break;

  case 91:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 10379 "Parser/parser.cc"
    break;

  case 92:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10385 "Parser/parser.cc"
    break;

  case 93:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10391 "Parser/parser.cc"
    break;

  case 94:
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10397 "Parser/parser.cc"
    break;

  case 95:
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10403 "Parser/parser.cc"
    break;

  case 96:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10409 "Parser/parser.cc"
    break;

  case 97:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10415 "Parser/parser.cc"
    break;

  case 98:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 10421 "Parser/parser.cc"
    break;

  case 99:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 10430 "Parser/parser.cc"
    break;

  case 100:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10436 "Parser/parser.cc"
    break;

  case 101:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10442 "Parser/parser.cc"
    break;

  case 102:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 10448 "Parser/parser.cc"
    break;

  case 103:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 10454 "Parser/parser.cc"
    break;

  case 104:
#line 958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 10460 "Parser/parser.cc"
    break;

  case 105:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 10466 "Parser/parser.cc"
    break;

  case 106:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 10472 "Parser/parser.cc"
    break;

  case 107:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 10478 "Parser/parser.cc"
    break;

  case 108:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 10484 "Parser/parser.cc"
    break;

  case 110:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 10490 "Parser/parser.cc"
    break;

  case 111:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10496 "Parser/parser.cc"
    break;

  case 112:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10502 "Parser/parser.cc"
    break;

  case 113:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 10508 "Parser/parser.cc"
    break;

  case 114:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 10514 "Parser/parser.cc"
    break;

  case 115:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::ReturnCast ) ); }
#line 10520 "Parser/parser.cc"
    break;

  case 116:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10526 "Parser/parser.cc"
    break;

  case 117:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10532 "Parser/parser.cc"
    break;

  case 125:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10538 "Parser/parser.cc"
    break;

  case 127:
#line 1011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10544 "Parser/parser.cc"
    break;

  case 128:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10550 "Parser/parser.cc"
    break;

  case 129:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10556 "Parser/parser.cc"
    break;

  case 131:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10562 "Parser/parser.cc"
    break;

  case 132:
#line 1023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10568 "Parser/parser.cc"
    break;

  case 134:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10574 "Parser/parser.cc"
    break;

  case 135:
#line 1031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10580 "Parser/parser.cc"
    break;

  case 137:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10586 "Parser/parser.cc"
    break;

  case 138:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10592 "Parser/parser.cc"
    break;

  case 139:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10598 "Parser/parser.cc"
    break;

  case 140:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10604 "Parser/parser.cc"
    break;

  case 142:
#line 1049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10610 "Parser/parser.cc"
    break;

  case 143:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10616 "Parser/parser.cc"
    break;

  case 145:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10622 "Parser/parser.cc"
    break;

  case 147:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10628 "Parser/parser.cc"
    break;

  case 149:
#line 1069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10634 "Parser/parser.cc"
    break;

  case 151:
#line 1075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 10640 "Parser/parser.cc"
    break;

  case 153:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 10646 "Parser/parser.cc"
    break;

  case 155:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10652 "Parser/parser.cc"
    break;

  case 156:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 10658 "Parser/parser.cc"
    break;

  case 158:
#line 1098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10664 "Parser/parser.cc"
    break;

  case 161:
#line 1106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10670 "Parser/parser.cc"
    break;

  case 162:
#line 1112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 10676 "Parser/parser.cc"
    break;

  case 163:
#line 1115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10682 "Parser/parser.cc"
    break;

  case 166:
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 10694 "Parser/parser.cc"
    break;

  case 167:
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10700 "Parser/parser.cc"
    break;

  case 168:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10706 "Parser/parser.cc"
    break;

  case 172:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 10712 "Parser/parser.cc"
    break;

  case 173:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 10718 "Parser/parser.cc"
    break;

  case 174:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 10724 "Parser/parser.cc"
    break;

  case 175:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 10730 "Parser/parser.cc"
    break;

  case 176:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 10736 "Parser/parser.cc"
    break;

  case 177:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 10742 "Parser/parser.cc"
    break;

  case 178:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 10748 "Parser/parser.cc"
    break;

  case 179:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 10754 "Parser/parser.cc"
    break;

  case 180:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 10760 "Parser/parser.cc"
    break;

  case 181:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 10766 "Parser/parser.cc"
    break;

  case 182:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 10772 "Parser/parser.cc"
    break;

  case 183:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 10778 "Parser/parser.cc"
    break;

  case 184:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 10784 "Parser/parser.cc"
    break;

  case 185:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Empty tuple is meaningless." ); (yyval.expr) = nullptr; }
#line 10790 "Parser/parser.cc"
    break;

  case 186:
#line 1171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-2].expr) ) ); }
#line 10796 "Parser/parser.cc"
    break;

  case 187:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10802 "Parser/parser.cc"
    break;

  case 188:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-2].expr) ) ) ); }
#line 10808 "Parser/parser.cc"
    break;

  case 189:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10814 "Parser/parser.cc"
    break;

  case 191:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10820 "Parser/parser.cc"
    break;

  case 192:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10826 "Parser/parser.cc"
    break;

  case 193:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10832 "Parser/parser.cc"
    break;

  case 195:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10838 "Parser/parser.cc"
    break;

  case 196:
#line 1198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10844 "Parser/parser.cc"
    break;

  case 211:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10850 "Parser/parser.cc"
    break;

  case 213:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 10856 "Parser/parser.cc"
    break;

  case 214:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 10862 "Parser/parser.cc"
    break;

  case 215:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 10873 "Parser/parser.cc"
    break;

  case 216:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 10879 "Parser/parser.cc"
    break;

  case 217:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 10885 "Parser/parser.cc"
    break;

  case 219:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10891 "Parser/parser.cc"
    break;

  case 220:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10897 "Parser/parser.cc"
    break;

  case 221:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10903 "Parser/parser.cc"
    break;

  case 222:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10909 "Parser/parser.cc"
    break;

  case 223:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10915 "Parser/parser.cc"
    break;

  case 226:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10921 "Parser/parser.cc"
    break;

  case 227:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 10928 "Parser/parser.cc"
    break;

  case 228:
#line 1279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 10934 "Parser/parser.cc"
    break;

  case 229:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 10940 "Parser/parser.cc"
    break;

  case 230:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10946 "Parser/parser.cc"
    break;

  case 231:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10952 "Parser/parser.cc"
    break;

  case 232:
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
#line 10966 "Parser/parser.cc"
    break;

  case 233:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10972 "Parser/parser.cc"
    break;

  case 234:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10978 "Parser/parser.cc"
    break;

  case 235:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 10987 "Parser/parser.cc"
    break;

  case 236:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10993 "Parser/parser.cc"
    break;

  case 237:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( nullptr, (yyvsp[0].expr) ); }
#line 10999 "Parser/parser.cc"
    break;

  case 238:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 11005 "Parser/parser.cc"
    break;

  case 239:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 11011 "Parser/parser.cc"
    break;

  case 240:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 11017 "Parser/parser.cc"
    break;

  case 241:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 11023 "Parser/parser.cc"
    break;

  case 242:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 11029 "Parser/parser.cc"
    break;

  case 244:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 11035 "Parser/parser.cc"
    break;

  case 245:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 11041 "Parser/parser.cc"
    break;

  case 246:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 11047 "Parser/parser.cc"
    break;

  case 247:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 11053 "Parser/parser.cc"
    break;

  case 248:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 11059 "Parser/parser.cc"
    break;

  case 249:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 11065 "Parser/parser.cc"
    break;

  case 250:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 11071 "Parser/parser.cc"
    break;

  case 252:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 11077 "Parser/parser.cc"
    break;

  case 253:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11083 "Parser/parser.cc"
    break;

  case 254:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 11089 "Parser/parser.cc"
    break;

  case 256:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11095 "Parser/parser.cc"
    break;

  case 257:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 11101 "Parser/parser.cc"
    break;

  case 258:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11107 "Parser/parser.cc"
    break;

  case 259:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11116 "Parser/parser.cc"
    break;

  case 260:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11122 "Parser/parser.cc"
    break;

  case 261:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11128 "Parser/parser.cc"
    break;

  case 262:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 11134 "Parser/parser.cc"
    break;

  case 263:
#line 1413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11143 "Parser/parser.cc"
    break;

  case 264:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 11149 "Parser/parser.cc"
    break;

  case 265:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11155 "Parser/parser.cc"
    break;

  case 266:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11161 "Parser/parser.cc"
    break;

  case 267:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11170 "Parser/parser.cc"
    break;

  case 268:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11176 "Parser/parser.cc"
    break;

  case 269:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11182 "Parser/parser.cc"
    break;

  case 271:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11201 "Parser/parser.cc"
    break;

  case 272:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11207 "Parser/parser.cc"
    break;

  case 273:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 11215 "Parser/parser.cc"
    break;

  case 274:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11221 "Parser/parser.cc"
    break;

  case 275:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 11227 "Parser/parser.cc"
    break;

  case 276:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11233 "Parser/parser.cc"
    break;

  case 277:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11239 "Parser/parser.cc"
    break;

  case 278:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11245 "Parser/parser.cc"
    break;

  case 279:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11251 "Parser/parser.cc"
    break;

  case 280:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11260 "Parser/parser.cc"
    break;

  case 281:
#line 1485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11269 "Parser/parser.cc"
    break;

  case 282:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11275 "Parser/parser.cc"
    break;

  case 283:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11284 "Parser/parser.cc"
    break;

  case 284:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11293 "Parser/parser.cc"
    break;

  case 285:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11299 "Parser/parser.cc"
    break;

  case 286:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11305 "Parser/parser.cc"
    break;

  case 287:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11311 "Parser/parser.cc"
    break;

  case 288:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11317 "Parser/parser.cc"
    break;

  case 289:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11323 "Parser/parser.cc"
    break;

  case 290:
#line 1515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11329 "Parser/parser.cc"
    break;

  case 291:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11335 "Parser/parser.cc"
    break;

  case 292:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11341 "Parser/parser.cc"
    break;

  case 293:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11350 "Parser/parser.cc"
    break;

  case 294:
#line 1527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11360 "Parser/parser.cc"
    break;

  case 295:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11366 "Parser/parser.cc"
    break;

  case 296:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11372 "Parser/parser.cc"
    break;

  case 297:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11381 "Parser/parser.cc"
    break;

  case 298:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11391 "Parser/parser.cc"
    break;

  case 299:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11397 "Parser/parser.cc"
    break;

  case 300:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11406 "Parser/parser.cc"
    break;

  case 301:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11416 "Parser/parser.cc"
    break;

  case 302:
#line 1562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11422 "Parser/parser.cc"
    break;

  case 303:
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 11428 "Parser/parser.cc"
    break;

  case 304:
#line 1568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11434 "Parser/parser.cc"
    break;

  case 305:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11440 "Parser/parser.cc"
    break;

  case 306:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11449 "Parser/parser.cc"
    break;

  case 307:
#line 1578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11459 "Parser/parser.cc"
    break;

  case 308:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11465 "Parser/parser.cc"
    break;

  case 309:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11474 "Parser/parser.cc"
    break;

  case 310:
#line 1592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11484 "Parser/parser.cc"
    break;

  case 311:
#line 1598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11490 "Parser/parser.cc"
    break;

  case 312:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11499 "Parser/parser.cc"
    break;

  case 313:
#line 1605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11509 "Parser/parser.cc"
    break;

  case 314:
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11515 "Parser/parser.cc"
    break;

  case 315:
#line 1614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 11523 "Parser/parser.cc"
    break;

  case 316:
#line 1618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); (yyval.forctrl) = nullptr;
				(yyvsp[-1].oper) = OperKinds::GEThan;
			} // if
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-3].expr), (yyvsp[-1].oper), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 11535 "Parser/parser.cc"
    break;

  case 317:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11544 "Parser/parser.cc"
    break;

  case 318:
#line 1634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false );
		}
#line 11553 "Parser/parser.cc"
    break;

  case 319:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11562 "Parser/parser.cc"
    break;

  case 320:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11568 "Parser/parser.cc"
    break;

  case 321:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11574 "Parser/parser.cc"
    break;

  case 322:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11580 "Parser/parser.cc"
    break;

  case 323:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11586 "Parser/parser.cc"
    break;

  case 324:
#line 1661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11592 "Parser/parser.cc"
    break;

  case 325:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11598 "Parser/parser.cc"
    break;

  case 326:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11604 "Parser/parser.cc"
    break;

  case 328:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11610 "Parser/parser.cc"
    break;

  case 329:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11616 "Parser/parser.cc"
    break;

  case 330:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 11622 "Parser/parser.cc"
    break;

  case 331:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 11628 "Parser/parser.cc"
    break;

  case 332:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 11634 "Parser/parser.cc"
    break;

  case 333:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 11640 "Parser/parser.cc"
    break;

  case 334:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 11646 "Parser/parser.cc"
    break;

  case 335:
#line 1692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 11652 "Parser/parser.cc"
    break;

  case 336:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 11658 "Parser/parser.cc"
    break;

  case 337:
#line 1699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 11664 "Parser/parser.cc"
    break;

  case 338:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 11670 "Parser/parser.cc"
    break;

  case 339:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 11676 "Parser/parser.cc"
    break;

  case 340:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 11682 "Parser/parser.cc"
    break;

  case 341:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 11688 "Parser/parser.cc"
    break;

  case 342:
#line 1711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 11694 "Parser/parser.cc"
    break;

  case 343:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 11700 "Parser/parser.cc"
    break;

  case 344:
#line 1715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 11706 "Parser/parser.cc"
    break;

  case 345:
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 11712 "Parser/parser.cc"
    break;

  case 346:
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 11718 "Parser/parser.cc"
    break;

  case 347:
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 11724 "Parser/parser.cc"
    break;

  case 348:
#line 1723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 11730 "Parser/parser.cc"
    break;

  case 349:
#line 1725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 11736 "Parser/parser.cc"
    break;

  case 352:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11742 "Parser/parser.cc"
    break;

  case 353:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 11751 "Parser/parser.cc"
    break;

  case 354:
#line 1748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11757 "Parser/parser.cc"
    break;

  case 355:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11763 "Parser/parser.cc"
    break;

  case 358:
#line 1760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11769 "Parser/parser.cc"
    break;

  case 359:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11775 "Parser/parser.cc"
    break;

  case 362:
#line 1773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11781 "Parser/parser.cc"
    break;

  case 363:
#line 1775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 11787 "Parser/parser.cc"
    break;

  case 364:
#line 1781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11793 "Parser/parser.cc"
    break;

  case 365:
#line 1783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11799 "Parser/parser.cc"
    break;

  case 366:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11805 "Parser/parser.cc"
    break;

  case 367:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11811 "Parser/parser.cc"
    break;

  case 368:
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 11817 "Parser/parser.cc"
    break;

  case 369:
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11823 "Parser/parser.cc"
    break;

  case 370:
#line 1797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 11829 "Parser/parser.cc"
    break;

  case 373:
#line 1807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11835 "Parser/parser.cc"
    break;

  case 374:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11841 "Parser/parser.cc"
    break;

  case 375:
#line 1814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 11847 "Parser/parser.cc"
    break;

  case 376:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11853 "Parser/parser.cc"
    break;

  case 377:
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11859 "Parser/parser.cc"
    break;

  case 378:
#line 1826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11865 "Parser/parser.cc"
    break;

  case 379:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11871 "Parser/parser.cc"
    break;

  case 380:
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11877 "Parser/parser.cc"
    break;

  case 381:
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 11883 "Parser/parser.cc"
    break;

  case 382:
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 11889 "Parser/parser.cc"
    break;

  case 383:
#line 1845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11895 "Parser/parser.cc"
    break;

  case 384:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 11901 "Parser/parser.cc"
    break;

  case 385:
#line 1852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 11907 "Parser/parser.cc"
    break;

  case 386:
#line 1854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 11913 "Parser/parser.cc"
    break;

  case 387:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11919 "Parser/parser.cc"
    break;

  case 388:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-6].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 11925 "Parser/parser.cc"
    break;

  case 389:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11931 "Parser/parser.cc"
    break;

  case 390:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 11937 "Parser/parser.cc"
    break;

  case 391:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11943 "Parser/parser.cc"
    break;

  case 392:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11949 "Parser/parser.cc"
    break;

  case 393:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 11955 "Parser/parser.cc"
    break;

  case 394:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 11961 "Parser/parser.cc"
    break;

  case 395:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 11967 "Parser/parser.cc"
    break;

  case 397:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11973 "Parser/parser.cc"
    break;

  case 398:
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11979 "Parser/parser.cc"
    break;

  case 399:
#line 1889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11985 "Parser/parser.cc"
    break;

  case 404:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 11991 "Parser/parser.cc"
    break;

  case 405:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11997 "Parser/parser.cc"
    break;

  case 406:
#line 1908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12003 "Parser/parser.cc"
    break;

  case 407:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12009 "Parser/parser.cc"
    break;

  case 408:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 12015 "Parser/parser.cc"
    break;

  case 409:
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 12021 "Parser/parser.cc"
    break;

  case 410:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 12027 "Parser/parser.cc"
    break;

  case 411:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12033 "Parser/parser.cc"
    break;

  case 414:
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12039 "Parser/parser.cc"
    break;

  case 415:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 12045 "Parser/parser.cc"
    break;

  case 416:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 12054 "Parser/parser.cc"
    break;

  case 417:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12060 "Parser/parser.cc"
    break;

  case 418:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 12066 "Parser/parser.cc"
    break;

  case 419:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12072 "Parser/parser.cc"
    break;

  case 420:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 12081 "Parser/parser.cc"
    break;

  case 421:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 12090 "Parser/parser.cc"
    break;

  case 422:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12096 "Parser/parser.cc"
    break;

  case 425:
#line 1977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12102 "Parser/parser.cc"
    break;

  case 426:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12108 "Parser/parser.cc"
    break;

  case 428:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12114 "Parser/parser.cc"
    break;

  case 429:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 12120 "Parser/parser.cc"
    break;

  case 439:
#line 2016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 12126 "Parser/parser.cc"
    break;

  case 440:
#line 2018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 12132 "Parser/parser.cc"
    break;

  case 444:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12138 "Parser/parser.cc"
    break;

  case 446:
#line 2042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 12144 "Parser/parser.cc"
    break;

  case 447:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12150 "Parser/parser.cc"
    break;

  case 448:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12156 "Parser/parser.cc"
    break;

  case 449:
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12162 "Parser/parser.cc"
    break;

  case 450:
#line 2057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12168 "Parser/parser.cc"
    break;

  case 451:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12174 "Parser/parser.cc"
    break;

  case 452:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12180 "Parser/parser.cc"
    break;

  case 453:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12186 "Parser/parser.cc"
    break;

  case 454:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12192 "Parser/parser.cc"
    break;

  case 456:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12198 "Parser/parser.cc"
    break;

  case 457:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12204 "Parser/parser.cc"
    break;

  case 458:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12210 "Parser/parser.cc"
    break;

  case 459:
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 12221 "Parser/parser.cc"
    break;

  case 460:
#line 2093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12227 "Parser/parser.cc"
    break;

  case 461:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12233 "Parser/parser.cc"
    break;

  case 462:
#line 2108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12239 "Parser/parser.cc"
    break;

  case 463:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12245 "Parser/parser.cc"
    break;

  case 464:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 12251 "Parser/parser.cc"
    break;

  case 465:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) ); }
#line 12257 "Parser/parser.cc"
    break;

  case 466:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 12266 "Parser/parser.cc"
    break;

  case 467:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 12275 "Parser/parser.cc"
    break;

  case 468:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 12284 "Parser/parser.cc"
    break;

  case 469:
#line 2144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 12295 "Parser/parser.cc"
    break;

  case 470:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 12304 "Parser/parser.cc"
    break;

  case 471:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12310 "Parser/parser.cc"
    break;

  case 472:
#line 2158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12316 "Parser/parser.cc"
    break;

  case 473:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12322 "Parser/parser.cc"
    break;

  case 474:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 12330 "Parser/parser.cc"
    break;

  case 475:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 12338 "Parser/parser.cc"
    break;

  case 476:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 12344 "Parser/parser.cc"
    break;

  case 479:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12359 "Parser/parser.cc"
    break;

  case 480:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 481:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12371 "Parser/parser.cc"
    break;

  case 482:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 12377 "Parser/parser.cc"
    break;

  case 483:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 12383 "Parser/parser.cc"
    break;

  case 484:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12389 "Parser/parser.cc"
    break;

  case 490:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 12399 "Parser/parser.cc"
    break;

  case 503:
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12405 "Parser/parser.cc"
    break;

  case 506:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12411 "Parser/parser.cc"
    break;

  case 507:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12417 "Parser/parser.cc"
    break;

  case 509:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 12423 "Parser/parser.cc"
    break;

  case 510:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 12429 "Parser/parser.cc"
    break;

  case 511:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 12435 "Parser/parser.cc"
    break;

  case 512:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 12441 "Parser/parser.cc"
    break;

  case 513:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 12447 "Parser/parser.cc"
    break;

  case 514:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12453 "Parser/parser.cc"
    break;

  case 516:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12459 "Parser/parser.cc"
    break;

  case 517:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12465 "Parser/parser.cc"
    break;

  case 519:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12471 "Parser/parser.cc"
    break;

  case 520:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 12477 "Parser/parser.cc"
    break;

  case 521:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 12483 "Parser/parser.cc"
    break;

  case 522:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 12489 "Parser/parser.cc"
    break;

  case 523:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 12495 "Parser/parser.cc"
    break;

  case 524:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 12501 "Parser/parser.cc"
    break;

  case 525:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 12507 "Parser/parser.cc"
    break;

  case 526:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 12513 "Parser/parser.cc"
    break;

  case 527:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 12519 "Parser/parser.cc"
    break;

  case 528:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 12525 "Parser/parser.cc"
    break;

  case 529:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12531 "Parser/parser.cc"
    break;

  case 530:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 12537 "Parser/parser.cc"
    break;

  case 531:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 12543 "Parser/parser.cc"
    break;

  case 532:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 12549 "Parser/parser.cc"
    break;

  case 533:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 12555 "Parser/parser.cc"
    break;

  case 534:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 12561 "Parser/parser.cc"
    break;

  case 535:
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 12567 "Parser/parser.cc"
    break;

  case 536:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 12573 "Parser/parser.cc"
    break;

  case 537:
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 12579 "Parser/parser.cc"
    break;

  case 538:
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float80 ); }
#line 12585 "Parser/parser.cc"
    break;

  case 539:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 12591 "Parser/parser.cc"
    break;

  case 540:
#line 2376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float16 ); }
#line 12597 "Parser/parser.cc"
    break;

  case 541:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32 ); }
#line 12603 "Parser/parser.cc"
    break;

  case 542:
#line 2380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x ); }
#line 12609 "Parser/parser.cc"
    break;

  case 543:
#line 2382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64 ); }
#line 12615 "Parser/parser.cc"
    break;

  case 544:
#line 2384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x ); }
#line 12621 "Parser/parser.cc"
    break;

  case 545:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128 ); }
#line 12627 "Parser/parser.cc"
    break;

  case 546:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128x ); }
#line 12633 "Parser/parser.cc"
    break;

  case 547:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x4 ); }
#line 12639 "Parser/parser.cc"
    break;

  case 548:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x2 ); }
#line 12645 "Parser/parser.cc"
    break;

  case 549:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat32 ); }
#line 12651 "Parser/parser.cc"
    break;

  case 550:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat64 ); }
#line 12657 "Parser/parser.cc"
    break;

  case 551:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svbool ); }
#line 12663 "Parser/parser.cc"
    break;

  case 552:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12669 "Parser/parser.cc"
    break;

  case 553:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12675 "Parser/parser.cc"
    break;

  case 554:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12681 "Parser/parser.cc"
    break;

  case 555:
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 12687 "Parser/parser.cc"
    break;

  case 556:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 12693 "Parser/parser.cc"
    break;

  case 557:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 12699 "Parser/parser.cc"
    break;

  case 558:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 12705 "Parser/parser.cc"
    break;

  case 559:
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 12711 "Parser/parser.cc"
    break;

  case 560:
#line 2417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 12717 "Parser/parser.cc"
    break;

  case 561:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 12723 "Parser/parser.cc"
    break;

  case 562:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 12729 "Parser/parser.cc"
    break;

  case 564:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12735 "Parser/parser.cc"
    break;

  case 566:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 12741 "Parser/parser.cc"
    break;

  case 567:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12747 "Parser/parser.cc"
    break;

  case 568:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12753 "Parser/parser.cc"
    break;

  case 570:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12759 "Parser/parser.cc"
    break;

  case 571:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12765 "Parser/parser.cc"
    break;

  case 572:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12771 "Parser/parser.cc"
    break;

  case 573:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 12777 "Parser/parser.cc"
    break;

  case 575:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12783 "Parser/parser.cc"
    break;

  case 577:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12789 "Parser/parser.cc"
    break;

  case 578:
#line 2468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12795 "Parser/parser.cc"
    break;

  case 579:
#line 2470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 12801 "Parser/parser.cc"
    break;

  case 580:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12807 "Parser/parser.cc"
    break;

  case 581:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 12813 "Parser/parser.cc"
    break;

  case 582:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 12819 "Parser/parser.cc"
    break;

  case 583:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 12825 "Parser/parser.cc"
    break;

  case 584:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 12831 "Parser/parser.cc"
    break;

  case 585:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 12837 "Parser/parser.cc"
    break;

  case 587:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12843 "Parser/parser.cc"
    break;

  case 588:
#line 2493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12849 "Parser/parser.cc"
    break;

  case 589:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12855 "Parser/parser.cc"
    break;

  case 591:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 12861 "Parser/parser.cc"
    break;

  case 592:
#line 2503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12867 "Parser/parser.cc"
    break;

  case 593:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 12876 "Parser/parser.cc"
    break;

  case 595:
#line 2514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12882 "Parser/parser.cc"
    break;

  case 596:
#line 2516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12888 "Parser/parser.cc"
    break;

  case 597:
#line 2518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12894 "Parser/parser.cc"
    break;

  case 599:
#line 2524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12900 "Parser/parser.cc"
    break;

  case 600:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12906 "Parser/parser.cc"
    break;

  case 602:
#line 2532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12912 "Parser/parser.cc"
    break;

  case 603:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12918 "Parser/parser.cc"
    break;

  case 604:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12924 "Parser/parser.cc"
    break;

  case 605:
#line 2541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12930 "Parser/parser.cc"
    break;

  case 606:
#line 2543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12936 "Parser/parser.cc"
    break;

  case 607:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12942 "Parser/parser.cc"
    break;

  case 608:
#line 2550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 12948 "Parser/parser.cc"
    break;

  case 609:
#line 2552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 12954 "Parser/parser.cc"
    break;

  case 610:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 12960 "Parser/parser.cc"
    break;

  case 612:
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 12966 "Parser/parser.cc"
    break;

  case 613:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 12972 "Parser/parser.cc"
    break;

  case 614:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 12978 "Parser/parser.cc"
    break;

  case 615:
#line 2566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 12984 "Parser/parser.cc"
    break;

  case 616:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12990 "Parser/parser.cc"
    break;

  case 621:
#line 2585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 12996 "Parser/parser.cc"
    break;

  case 622:
#line 2587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 13002 "Parser/parser.cc"
    break;

  case 623:
#line 2589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 13011 "Parser/parser.cc"
    break;

  case 624:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 13019 "Parser/parser.cc"
    break;

  case 625:
#line 2598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 13028 "Parser/parser.cc"
    break;

  case 626:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 13037 "Parser/parser.cc"
    break;

  case 627:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 13046 "Parser/parser.cc"
    break;

  case 628:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 13055 "Parser/parser.cc"
    break;

  case 630:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13061 "Parser/parser.cc"
    break;

  case 631:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13067 "Parser/parser.cc"
    break;

  case 632:
#line 2629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13077 "Parser/parser.cc"
    break;

  case 633:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 13096 "Parser/parser.cc"
    break;

  case 636:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 13102 "Parser/parser.cc"
    break;

  case 637:
#line 2660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 13108 "Parser/parser.cc"
    break;

  case 638:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 13114 "Parser/parser.cc"
    break;

  case 639:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 13120 "Parser/parser.cc"
    break;

  case 640:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 13126 "Parser/parser.cc"
    break;

  case 641:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 13132 "Parser/parser.cc"
    break;

  case 642:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13141 "Parser/parser.cc"
    break;

  case 643:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 13147 "Parser/parser.cc"
    break;

  case 644:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13156 "Parser/parser.cc"
    break;

  case 645:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 13162 "Parser/parser.cc"
    break;

  case 646:
#line 2687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13171 "Parser/parser.cc"
    break;

  case 647:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13177 "Parser/parser.cc"
    break;

  case 648:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 13183 "Parser/parser.cc"
    break;

  case 649:
#line 2702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 13196 "Parser/parser.cc"
    break;

  case 650:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 13205 "Parser/parser.cc"
    break;

  case 651:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 13211 "Parser/parser.cc"
    break;

  case 652:
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13217 "Parser/parser.cc"
    break;

  case 653:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 13230 "Parser/parser.cc"
    break;

  case 654:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13236 "Parser/parser.cc"
    break;

  case 657:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 13242 "Parser/parser.cc"
    break;

  case 658:
#line 2735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13248 "Parser/parser.cc"
    break;

  case 661:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13254 "Parser/parser.cc"
    break;

  case 663:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13260 "Parser/parser.cc"
    break;

  case 664:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 13266 "Parser/parser.cc"
    break;

  case 665:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13272 "Parser/parser.cc"
    break;

  case 666:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13278 "Parser/parser.cc"
    break;

  case 667:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13284 "Parser/parser.cc"
    break;

  case 668:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13290 "Parser/parser.cc"
    break;

  case 670:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13296 "Parser/parser.cc"
    break;

  case 672:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 13302 "Parser/parser.cc"
    break;

  case 673:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13308 "Parser/parser.cc"
    break;

  case 675:
#line 2787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 13314 "Parser/parser.cc"
    break;

  case 676:
#line 2792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13320 "Parser/parser.cc"
    break;

  case 678:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13326 "Parser/parser.cc"
    break;

  case 679:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 13337 "Parser/parser.cc"
    break;

  case 680:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 13351 "Parser/parser.cc"
    break;

  case 681:
#line 2825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 13357 "Parser/parser.cc"
    break;

  case 682:
#line 2827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 13363 "Parser/parser.cc"
    break;

  case 683:
#line 2829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 13369 "Parser/parser.cc"
    break;

  case 684:
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 13380 "Parser/parser.cc"
    break;

  case 685:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 13386 "Parser/parser.cc"
    break;

  case 686:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 13392 "Parser/parser.cc"
    break;

  case 688:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13398 "Parser/parser.cc"
    break;

  case 689:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13404 "Parser/parser.cc"
    break;

  case 690:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13410 "Parser/parser.cc"
    break;

  case 691:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 13416 "Parser/parser.cc"
    break;

  case 692:
#line 2862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13425 "Parser/parser.cc"
    break;

  case 693:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13434 "Parser/parser.cc"
    break;

  case 694:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 13440 "Parser/parser.cc"
    break;

  case 695:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 13446 "Parser/parser.cc"
    break;

  case 696:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 13456 "Parser/parser.cc"
    break;

  case 697:
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 13462 "Parser/parser.cc"
    break;

  case 698:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 13468 "Parser/parser.cc"
    break;

  case 700:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13474 "Parser/parser.cc"
    break;

  case 701:
#line 2898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13480 "Parser/parser.cc"
    break;

  case 702:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13486 "Parser/parser.cc"
    break;

  case 703:
#line 2900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13492 "Parser/parser.cc"
    break;

  case 704:
#line 2909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13498 "Parser/parser.cc"
    break;

  case 705:
#line 2911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13504 "Parser/parser.cc"
    break;

  case 707:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13510 "Parser/parser.cc"
    break;

  case 710:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13516 "Parser/parser.cc"
    break;

  case 711:
#line 2923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13522 "Parser/parser.cc"
    break;

  case 712:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13528 "Parser/parser.cc"
    break;

  case 713:
#line 2930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13534 "Parser/parser.cc"
    break;

  case 716:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13540 "Parser/parser.cc"
    break;

  case 717:
#line 2936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13546 "Parser/parser.cc"
    break;

  case 718:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13552 "Parser/parser.cc"
    break;

  case 720:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13558 "Parser/parser.cc"
    break;

  case 721:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13564 "Parser/parser.cc"
    break;

  case 722:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 13570 "Parser/parser.cc"
    break;

  case 724:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13576 "Parser/parser.cc"
    break;

  case 725:
#line 2965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13582 "Parser/parser.cc"
    break;

  case 726:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13588 "Parser/parser.cc"
    break;

  case 727:
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13594 "Parser/parser.cc"
    break;

  case 728:
#line 2974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13600 "Parser/parser.cc"
    break;

  case 730:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13606 "Parser/parser.cc"
    break;

  case 731:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13612 "Parser/parser.cc"
    break;

  case 732:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13618 "Parser/parser.cc"
    break;

  case 737:
#line 2995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13624 "Parser/parser.cc"
    break;

  case 739:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13630 "Parser/parser.cc"
    break;

  case 740:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 13636 "Parser/parser.cc"
    break;

  case 743:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 13642 "Parser/parser.cc"
    break;

  case 746:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13648 "Parser/parser.cc"
    break;

  case 747:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 13654 "Parser/parser.cc"
    break;

  case 748:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 13660 "Parser/parser.cc"
    break;

  case 749:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13666 "Parser/parser.cc"
    break;

  case 750:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13672 "Parser/parser.cc"
    break;

  case 751:
#line 3032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13678 "Parser/parser.cc"
    break;

  case 752:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13684 "Parser/parser.cc"
    break;

  case 754:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 13690 "Parser/parser.cc"
    break;

  case 755:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 13696 "Parser/parser.cc"
    break;

  case 756:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 13702 "Parser/parser.cc"
    break;

  case 758:
#line 3057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 13708 "Parser/parser.cc"
    break;

  case 760:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 13714 "Parser/parser.cc"
    break;

  case 761:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 13720 "Parser/parser.cc"
    break;

  case 762:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13726 "Parser/parser.cc"
    break;

  case 763:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13732 "Parser/parser.cc"
    break;

  case 764:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 13738 "Parser/parser.cc"
    break;

  case 765:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13744 "Parser/parser.cc"
    break;

  case 767:
#line 3100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13750 "Parser/parser.cc"
    break;

  case 768:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13756 "Parser/parser.cc"
    break;

  case 769:
#line 3107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13762 "Parser/parser.cc"
    break;

  case 770:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 13773 "Parser/parser.cc"
    break;

  case 771:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13779 "Parser/parser.cc"
    break;

  case 772:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 13785 "Parser/parser.cc"
    break;

  case 773:
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13791 "Parser/parser.cc"
    break;

  case 774:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 13800 "Parser/parser.cc"
    break;

  case 775:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 13806 "Parser/parser.cc"
    break;

  case 776:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 13816 "Parser/parser.cc"
    break;

  case 777:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13822 "Parser/parser.cc"
    break;

  case 778:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13828 "Parser/parser.cc"
    break;

  case 779:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 13834 "Parser/parser.cc"
    break;

  case 780:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13840 "Parser/parser.cc"
    break;

  case 781:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13846 "Parser/parser.cc"
    break;

  case 782:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13852 "Parser/parser.cc"
    break;

  case 783:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 13858 "Parser/parser.cc"
    break;

  case 784:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13864 "Parser/parser.cc"
    break;

  case 785:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13870 "Parser/parser.cc"
    break;

  case 788:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13876 "Parser/parser.cc"
    break;

  case 789:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13882 "Parser/parser.cc"
    break;

  case 790:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13888 "Parser/parser.cc"
    break;

  case 791:
#line 3187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13894 "Parser/parser.cc"
    break;

  case 793:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13900 "Parser/parser.cc"
    break;

  case 794:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 13906 "Parser/parser.cc"
    break;

  case 795:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13912 "Parser/parser.cc"
    break;

  case 796:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13918 "Parser/parser.cc"
    break;

  case 797:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 13924 "Parser/parser.cc"
    break;

  case 798:
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 13930 "Parser/parser.cc"
    break;

  case 799:
#line 3208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 13936 "Parser/parser.cc"
    break;

  case 800:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 13945 "Parser/parser.cc"
    break;

  case 801:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 13954 "Parser/parser.cc"
    break;

  case 802:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 13963 "Parser/parser.cc"
    break;

  case 803:
#line 3231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 13969 "Parser/parser.cc"
    break;

  case 804:
#line 3233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-6].tok), (yyvsp[-4].decl), (yyvsp[-1].decl) );
		}
#line 13978 "Parser/parser.cc"
    break;

  case 805:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-3].tok), (yyvsp[-5].decl), (yyvsp[-1].decl) ); }
#line 13984 "Parser/parser.cc"
    break;

  case 807:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13990 "Parser/parser.cc"
    break;

  case 812:
#line 3256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13996 "Parser/parser.cc"
    break;

  case 813:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 14002 "Parser/parser.cc"
    break;

  case 814:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 14008 "Parser/parser.cc"
    break;

  case 815:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Possible cause is declaring an aggregate or enumeration type in a trait." ); (yyval.decl) = nullptr; }
#line 14014 "Parser/parser.cc"
    break;

  case 817:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 14020 "Parser/parser.cc"
    break;

  case 818:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14026 "Parser/parser.cc"
    break;

  case 819:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 14032 "Parser/parser.cc"
    break;

  case 820:
#line 3286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14038 "Parser/parser.cc"
    break;

  case 822:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 14044 "Parser/parser.cc"
    break;

  case 823:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 14050 "Parser/parser.cc"
    break;

  case 824:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 14056 "Parser/parser.cc"
    break;

  case 825:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 14072 "Parser/parser.cc"
    break;

  case 826:
#line 3314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 14078 "Parser/parser.cc"
    break;

  case 827:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 14084 "Parser/parser.cc"
    break;

  case 828:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 14090 "Parser/parser.cc"
    break;

  case 829:
#line 3320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14096 "Parser/parser.cc"
    break;

  case 830:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14102 "Parser/parser.cc"
    break;

  case 831:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14108 "Parser/parser.cc"
    break;

  case 833:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 14117 "Parser/parser.cc"
    break;

  case 834:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 14123 "Parser/parser.cc"
    break;

  case 835:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 14132 "Parser/parser.cc"
    break;

  case 836:
#line 3339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 14142 "Parser/parser.cc"
    break;

  case 837:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 14151 "Parser/parser.cc"
    break;

  case 838:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14161 "Parser/parser.cc"
    break;

  case 839:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 14172 "Parser/parser.cc"
    break;

  case 840:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14182 "Parser/parser.cc"
    break;

  case 841:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 14193 "Parser/parser.cc"
    break;

  case 842:
#line 3377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14203 "Parser/parser.cc"
    break;

  case 843:
#line 3383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 14214 "Parser/parser.cc"
    break;

  case 844:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14224 "Parser/parser.cc"
    break;

  case 845:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14230 "Parser/parser.cc"
    break;

  case 847:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14236 "Parser/parser.cc"
    break;

  case 848:
#line 3409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14242 "Parser/parser.cc"
    break;

  case 849:
#line 3414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 14248 "Parser/parser.cc"
    break;

  case 850:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 14260 "Parser/parser.cc"
    break;

  case 851:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14271 "Parser/parser.cc"
    break;

  case 852:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14280 "Parser/parser.cc"
    break;

  case 853:
#line 3439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14289 "Parser/parser.cc"
    break;

  case 854:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14295 "Parser/parser.cc"
    break;

  case 855:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14301 "Parser/parser.cc"
    break;

  case 856:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14307 "Parser/parser.cc"
    break;

  case 857:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 14316 "Parser/parser.cc"
    break;

  case 858:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14322 "Parser/parser.cc"
    break;

  case 859:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14328 "Parser/parser.cc"
    break;

  case 860:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 14334 "Parser/parser.cc"
    break;

  case 865:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 14340 "Parser/parser.cc"
    break;

  case 866:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14346 "Parser/parser.cc"
    break;

  case 867:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 14356 "Parser/parser.cc"
    break;

  case 868:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14362 "Parser/parser.cc"
    break;

  case 871:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14368 "Parser/parser.cc"
    break;

  case 872:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 14374 "Parser/parser.cc"
    break;

  case 873:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14380 "Parser/parser.cc"
    break;

  case 874:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14386 "Parser/parser.cc"
    break;

  case 875:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14392 "Parser/parser.cc"
    break;

  case 877:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14398 "Parser/parser.cc"
    break;

  case 878:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14404 "Parser/parser.cc"
    break;

  case 879:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14410 "Parser/parser.cc"
    break;

  case 880:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 14416 "Parser/parser.cc"
    break;

  case 882:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 14422 "Parser/parser.cc"
    break;

  case 883:
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 14428 "Parser/parser.cc"
    break;

  case 884:
#line 3575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14434 "Parser/parser.cc"
    break;

  case 885:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14440 "Parser/parser.cc"
    break;

  case 886:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14446 "Parser/parser.cc"
    break;

  case 887:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14452 "Parser/parser.cc"
    break;

  case 889:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14458 "Parser/parser.cc"
    break;

  case 890:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14464 "Parser/parser.cc"
    break;

  case 891:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14470 "Parser/parser.cc"
    break;

  case 892:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14476 "Parser/parser.cc"
    break;

  case 893:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14482 "Parser/parser.cc"
    break;

  case 894:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14488 "Parser/parser.cc"
    break;

  case 895:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14494 "Parser/parser.cc"
    break;

  case 896:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14500 "Parser/parser.cc"
    break;

  case 897:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14506 "Parser/parser.cc"
    break;

  case 898:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14512 "Parser/parser.cc"
    break;

  case 899:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14518 "Parser/parser.cc"
    break;

  case 900:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14524 "Parser/parser.cc"
    break;

  case 901:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14530 "Parser/parser.cc"
    break;

  case 902:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14536 "Parser/parser.cc"
    break;

  case 903:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14542 "Parser/parser.cc"
    break;

  case 904:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14548 "Parser/parser.cc"
    break;

  case 905:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14554 "Parser/parser.cc"
    break;

  case 906:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14560 "Parser/parser.cc"
    break;

  case 908:
#line 3641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14566 "Parser/parser.cc"
    break;

  case 909:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14572 "Parser/parser.cc"
    break;

  case 910:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14578 "Parser/parser.cc"
    break;

  case 911:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14584 "Parser/parser.cc"
    break;

  case 912:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14590 "Parser/parser.cc"
    break;

  case 913:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14596 "Parser/parser.cc"
    break;

  case 914:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14602 "Parser/parser.cc"
    break;

  case 915:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14608 "Parser/parser.cc"
    break;

  case 916:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14614 "Parser/parser.cc"
    break;

  case 917:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14620 "Parser/parser.cc"
    break;

  case 918:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14626 "Parser/parser.cc"
    break;

  case 919:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14632 "Parser/parser.cc"
    break;

  case 920:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14638 "Parser/parser.cc"
    break;

  case 921:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14644 "Parser/parser.cc"
    break;

  case 922:
#line 3678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14650 "Parser/parser.cc"
    break;

  case 923:
#line 3680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14656 "Parser/parser.cc"
    break;

  case 927:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 14662 "Parser/parser.cc"
    break;

  case 928:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14668 "Parser/parser.cc"
    break;

  case 929:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14674 "Parser/parser.cc"
    break;

  case 930:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14680 "Parser/parser.cc"
    break;

  case 931:
#line 3706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14686 "Parser/parser.cc"
    break;

  case 932:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14692 "Parser/parser.cc"
    break;

  case 933:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14698 "Parser/parser.cc"
    break;

  case 934:
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14704 "Parser/parser.cc"
    break;

  case 935:
#line 3717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14710 "Parser/parser.cc"
    break;

  case 936:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14716 "Parser/parser.cc"
    break;

  case 937:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14722 "Parser/parser.cc"
    break;

  case 938:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14728 "Parser/parser.cc"
    break;

  case 939:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14734 "Parser/parser.cc"
    break;

  case 940:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14740 "Parser/parser.cc"
    break;

  case 941:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14746 "Parser/parser.cc"
    break;

  case 942:
#line 3744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 14755 "Parser/parser.cc"
    break;

  case 943:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14761 "Parser/parser.cc"
    break;

  case 944:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14767 "Parser/parser.cc"
    break;

  case 946:
#line 3757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14773 "Parser/parser.cc"
    break;

  case 947:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14779 "Parser/parser.cc"
    break;

  case 948:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14785 "Parser/parser.cc"
    break;

  case 949:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14791 "Parser/parser.cc"
    break;

  case 950:
#line 3768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14797 "Parser/parser.cc"
    break;

  case 951:
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14803 "Parser/parser.cc"
    break;

  case 952:
#line 3775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14809 "Parser/parser.cc"
    break;

  case 953:
#line 3777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14815 "Parser/parser.cc"
    break;

  case 954:
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14821 "Parser/parser.cc"
    break;

  case 955:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14827 "Parser/parser.cc"
    break;

  case 956:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14833 "Parser/parser.cc"
    break;

  case 957:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14839 "Parser/parser.cc"
    break;

  case 958:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14845 "Parser/parser.cc"
    break;

  case 959:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14851 "Parser/parser.cc"
    break;

  case 960:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14857 "Parser/parser.cc"
    break;

  case 961:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14863 "Parser/parser.cc"
    break;

  case 962:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14869 "Parser/parser.cc"
    break;

  case 963:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14875 "Parser/parser.cc"
    break;

  case 965:
#line 3810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14881 "Parser/parser.cc"
    break;

  case 966:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14887 "Parser/parser.cc"
    break;

  case 967:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14893 "Parser/parser.cc"
    break;

  case 968:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14899 "Parser/parser.cc"
    break;

  case 969:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14905 "Parser/parser.cc"
    break;

  case 970:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14911 "Parser/parser.cc"
    break;

  case 971:
#line 3828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14917 "Parser/parser.cc"
    break;

  case 972:
#line 3830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14923 "Parser/parser.cc"
    break;

  case 973:
#line 3832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14929 "Parser/parser.cc"
    break;

  case 974:
#line 3834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14935 "Parser/parser.cc"
    break;

  case 975:
#line 3839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14941 "Parser/parser.cc"
    break;

  case 976:
#line 3841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14947 "Parser/parser.cc"
    break;

  case 977:
#line 3843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14953 "Parser/parser.cc"
    break;

  case 978:
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14959 "Parser/parser.cc"
    break;

  case 979:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14965 "Parser/parser.cc"
    break;

  case 980:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14971 "Parser/parser.cc"
    break;

  case 981:
#line 3859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14977 "Parser/parser.cc"
    break;

  case 982:
#line 3861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14984 "Parser/parser.cc"
    break;

  case 984:
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14990 "Parser/parser.cc"
    break;

  case 985:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14996 "Parser/parser.cc"
    break;

  case 986:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15002 "Parser/parser.cc"
    break;

  case 987:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15008 "Parser/parser.cc"
    break;

  case 988:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15014 "Parser/parser.cc"
    break;

  case 989:
#line 3881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15020 "Parser/parser.cc"
    break;

  case 990:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15026 "Parser/parser.cc"
    break;

  case 991:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15032 "Parser/parser.cc"
    break;

  case 992:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15038 "Parser/parser.cc"
    break;

  case 993:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15044 "Parser/parser.cc"
    break;

  case 994:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15050 "Parser/parser.cc"
    break;

  case 995:
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15056 "Parser/parser.cc"
    break;

  case 996:
#line 3910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15062 "Parser/parser.cc"
    break;

  case 997:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15069 "Parser/parser.cc"
    break;

  case 999:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15075 "Parser/parser.cc"
    break;

  case 1000:
#line 3918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15081 "Parser/parser.cc"
    break;

  case 1001:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 15087 "Parser/parser.cc"
    break;

  case 1002:
#line 3925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 15093 "Parser/parser.cc"
    break;

  case 1003:
#line 3930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15099 "Parser/parser.cc"
    break;

  case 1004:
#line 3932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15105 "Parser/parser.cc"
    break;

  case 1005:
#line 3934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15111 "Parser/parser.cc"
    break;

  case 1006:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15117 "Parser/parser.cc"
    break;

  case 1007:
#line 3941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15123 "Parser/parser.cc"
    break;

  case 1008:
#line 3946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15129 "Parser/parser.cc"
    break;

  case 1009:
#line 3948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15135 "Parser/parser.cc"
    break;

  case 1011:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15141 "Parser/parser.cc"
    break;

  case 1012:
#line 3968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15147 "Parser/parser.cc"
    break;

  case 1013:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 15153 "Parser/parser.cc"
    break;

  case 1014:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15159 "Parser/parser.cc"
    break;

  case 1015:
#line 3977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15165 "Parser/parser.cc"
    break;

  case 1016:
#line 3979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15171 "Parser/parser.cc"
    break;

  case 1017:
#line 3981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15177 "Parser/parser.cc"
    break;

  case 1019:
#line 3987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15183 "Parser/parser.cc"
    break;

  case 1020:
#line 3989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15189 "Parser/parser.cc"
    break;

  case 1021:
#line 3991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15195 "Parser/parser.cc"
    break;

  case 1022:
#line 3996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15201 "Parser/parser.cc"
    break;

  case 1023:
#line 3998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15207 "Parser/parser.cc"
    break;

  case 1024:
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15213 "Parser/parser.cc"
    break;

  case 1025:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15219 "Parser/parser.cc"
    break;

  case 1026:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 15225 "Parser/parser.cc"
    break;

  case 1027:
#line 4011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-3].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 15231 "Parser/parser.cc"
    break;

  case 1028:
#line 4018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15237 "Parser/parser.cc"
    break;

  case 1030:
#line 4029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 15243 "Parser/parser.cc"
    break;

  case 1031:
#line 4031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 15249 "Parser/parser.cc"
    break;

  case 1033:
#line 4034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 15255 "Parser/parser.cc"
    break;

  case 1034:
#line 4036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 15261 "Parser/parser.cc"
    break;

  case 1036:
#line 4042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 15267 "Parser/parser.cc"
    break;

  case 1037:
#line 4044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 15273 "Parser/parser.cc"
    break;

  case 1038:
#line 4049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15279 "Parser/parser.cc"
    break;

  case 1039:
#line 4051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 15285 "Parser/parser.cc"
    break;

  case 1040:
#line 4053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 15291 "Parser/parser.cc"
    break;

  case 1041:
#line 4055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 15297 "Parser/parser.cc"
    break;

  case 1042:
#line 4089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 15303 "Parser/parser.cc"
    break;

  case 1045:
#line 4096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 15310 "Parser/parser.cc"
    break;

  case 1046:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15316 "Parser/parser.cc"
    break;

  case 1047:
#line 4101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15322 "Parser/parser.cc"
    break;

  case 1048:
#line 4106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 15328 "Parser/parser.cc"
    break;

  case 1049:
#line 4108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15334 "Parser/parser.cc"
    break;

  case 1050:
#line 4110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15340 "Parser/parser.cc"
    break;

  case 1051:
#line 4112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15346 "Parser/parser.cc"
    break;

  case 1052:
#line 4114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15352 "Parser/parser.cc"
    break;

  case 1054:
#line 4120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15358 "Parser/parser.cc"
    break;

  case 1055:
#line 4122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15364 "Parser/parser.cc"
    break;

  case 1056:
#line 4124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15370 "Parser/parser.cc"
    break;

  case 1057:
#line 4129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15376 "Parser/parser.cc"
    break;

  case 1058:
#line 4131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15382 "Parser/parser.cc"
    break;

  case 1059:
#line 4133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15388 "Parser/parser.cc"
    break;

  case 1061:
#line 4140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15394 "Parser/parser.cc"
    break;

  case 1063:
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15400 "Parser/parser.cc"
    break;

  case 1064:
#line 4154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 15406 "Parser/parser.cc"
    break;

  case 1065:
#line 4156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 15412 "Parser/parser.cc"
    break;

  case 1066:
#line 4159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 15418 "Parser/parser.cc"
    break;

  case 1067:
#line 4161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 15424 "Parser/parser.cc"
    break;

  case 1068:
#line 4163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 15430 "Parser/parser.cc"
    break;

  case 1070:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15436 "Parser/parser.cc"
    break;

  case 1071:
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15442 "Parser/parser.cc"
    break;

  case 1072:
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 15448 "Parser/parser.cc"
    break;

  case 1073:
#line 4187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15454 "Parser/parser.cc"
    break;

  case 1074:
#line 4189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15460 "Parser/parser.cc"
    break;

  case 1075:
#line 4191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15466 "Parser/parser.cc"
    break;

  case 1076:
#line 4193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15472 "Parser/parser.cc"
    break;

  case 1078:
#line 4199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15478 "Parser/parser.cc"
    break;

  case 1079:
#line 4201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15484 "Parser/parser.cc"
    break;

  case 1080:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15490 "Parser/parser.cc"
    break;

  case 1081:
#line 4208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15496 "Parser/parser.cc"
    break;

  case 1082:
#line 4210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15502 "Parser/parser.cc"
    break;

  case 1085:
#line 4220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15508 "Parser/parser.cc"
    break;

  case 1088:
#line 4227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15514 "Parser/parser.cc"
    break;

  case 1089:
#line 4233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15520 "Parser/parser.cc"
    break;

  case 1090:
#line 4235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15526 "Parser/parser.cc"
    break;

  case 1091:
#line 4237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15532 "Parser/parser.cc"
    break;

  case 1092:
#line 4239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15538 "Parser/parser.cc"
    break;

  case 1093:
#line 4241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15544 "Parser/parser.cc"
    break;

  case 1094:
#line 4243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15550 "Parser/parser.cc"
    break;

  case 1095:
#line 4250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15556 "Parser/parser.cc"
    break;

  case 1096:
#line 4252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15562 "Parser/parser.cc"
    break;

  case 1097:
#line 4254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15568 "Parser/parser.cc"
    break;

  case 1098:
#line 4256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15574 "Parser/parser.cc"
    break;

  case 1099:
#line 4258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15580 "Parser/parser.cc"
    break;

  case 1100:
#line 4260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15586 "Parser/parser.cc"
    break;

  case 1101:
#line 4262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15592 "Parser/parser.cc"
    break;

  case 1102:
#line 4264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15598 "Parser/parser.cc"
    break;

  case 1103:
#line 4266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15604 "Parser/parser.cc"
    break;

  case 1104:
#line 4268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15610 "Parser/parser.cc"
    break;

  case 1105:
#line 4271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15616 "Parser/parser.cc"
    break;

  case 1106:
#line 4273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15622 "Parser/parser.cc"
    break;

  case 1107:
#line 4275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15628 "Parser/parser.cc"
    break;

  case 1108:
#line 4277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15634 "Parser/parser.cc"
    break;

  case 1109:
#line 4279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15640 "Parser/parser.cc"
    break;

  case 1110:
#line 4284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-2].decl) ); }
#line 15646 "Parser/parser.cc"
    break;

  case 1111:
#line 4286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), false ); }
#line 15652 "Parser/parser.cc"
    break;

  case 1112:
#line 4291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), true ); }
#line 15658 "Parser/parser.cc"
    break;

  case 1113:
#line 4293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) ), true ); }
#line 15664 "Parser/parser.cc"
    break;

  case 1115:
#line 4320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15670 "Parser/parser.cc"
    break;

  case 1119:
#line 4331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15676 "Parser/parser.cc"
    break;

  case 1120:
#line 4333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15682 "Parser/parser.cc"
    break;

  case 1121:
#line 4335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15688 "Parser/parser.cc"
    break;

  case 1122:
#line 4337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15694 "Parser/parser.cc"
    break;

  case 1123:
#line 4339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15700 "Parser/parser.cc"
    break;

  case 1124:
#line 4341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15706 "Parser/parser.cc"
    break;

  case 1125:
#line 4348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15712 "Parser/parser.cc"
    break;

  case 1126:
#line 4350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15718 "Parser/parser.cc"
    break;

  case 1127:
#line 4352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15724 "Parser/parser.cc"
    break;

  case 1128:
#line 4354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15730 "Parser/parser.cc"
    break;

  case 1129:
#line 4356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15736 "Parser/parser.cc"
    break;

  case 1130:
#line 4358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15742 "Parser/parser.cc"
    break;

  case 1131:
#line 4363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 15748 "Parser/parser.cc"
    break;

  case 1132:
#line 4365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15754 "Parser/parser.cc"
    break;

  case 1133:
#line 4367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15760 "Parser/parser.cc"
    break;

  case 1134:
#line 4372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 15766 "Parser/parser.cc"
    break;

  case 1135:
#line 4374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15772 "Parser/parser.cc"
    break;

  case 1136:
#line 4376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15778 "Parser/parser.cc"
    break;

  case 1139:
#line 4400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 15784 "Parser/parser.cc"
    break;

  case 1140:
#line 4402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 15790 "Parser/parser.cc"
    break;


#line 15794 "Parser/parser.cc"

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
#line 4405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
