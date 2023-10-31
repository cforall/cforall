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

#include "DeclarationNode.h"                            // for DeclarationNode, ...
#include "ExpressionNode.h"                             // for ExpressionNode, ...
#include "InitializerNode.h"                            // for InitializerNode, ...
#include "ParserTypes.h"
#include "StatementNode.h"                              // for build_...
#include "TypedefTable.h"
#include "TypeData.h"
#include "Common/SemanticError.h"						// error_str
#include "Common/utility.h"								// for maybeMoveBuild, maybeBuild, CodeLo...

#include "SynTree/Attribute.h"							// for Attribute

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
	// distribute declaration_specifier across all declared variables, e.g., static, const, but not __attribute__.
	assert( declList );
	// printf( "distAttr1 typeSpec %p\n", typeSpec ); typeSpec->print( std::cout );
	DeclarationNode * cl = (new DeclarationNode)->addType( typeSpec );
	// printf( "distAttr2 cl %p\n", cl ); cl->type->print( std::cout );
	// cl->type->aggregate.name = cl->type->aggInst.aggregate->aggregate.name;

	for ( DeclarationNode * cur = dynamic_cast<DeclarationNode *>( declList->get_next() ); cur != nullptr; cur = dynamic_cast<DeclarationNode *>( cur->get_next() ) ) {
		cl->cloneBaseType( cur );
	} // for
	declList->addType( cl );
	// printf( "distAttr3 declList %p\n", declList ); declList->print( std::cout, 0 );
	return declList;
} // distAttr

void distExt( DeclarationNode * declaration ) {
	// distribute EXTENSION across all declarations
	for ( DeclarationNode *iter = declaration; iter != nullptr; iter = (DeclarationNode *)iter->get_next() ) {
		iter->set_extension( true );
	} // for
} // distExt

void distInl( DeclarationNode * declaration ) {
	// distribute INLINE across all declarations
	for ( DeclarationNode *iter = declaration; iter != nullptr; iter = (DeclarationNode *)iter->get_next() ) {
		iter->set_inLine( true );
	} // for
} // distInl

void distQual( DeclarationNode * declaration, DeclarationNode * qualifiers ) {
	// distribute qualifiers across all non-variable declarations in a distribution statemement
	for ( DeclarationNode * iter = declaration; iter != nullptr; iter = (DeclarationNode *)iter->get_next() ) {
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
	if ( ! fieldList ) {								// field declarator ?
		if ( ! ( typeSpec->type && (typeSpec->type->kind == TypeData::Aggregate || typeSpec->type->kind == TypeData::Enum) ) ) {
			stringstream ss;
			// printf( "fieldDecl1 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
			SemanticWarning( yylloc, Warning::SuperfluousDecl, ss.str().c_str() );
			return nullptr;
		} // if
		// printf( "fieldDecl2 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
		fieldList = DeclarationNode::newName( nullptr );
	} // if
//	return distAttr( typeSpec, fieldList );				// mark all fields in list

	// printf( "fieldDecl3 typeSpec %p\n", typeSpec ); typeSpec->print( std::cout, 0 );
	DeclarationNode * temp = distAttr( typeSpec, fieldList );				// mark all fields in list
	// printf( "fieldDecl4 temp %p\n", temp ); temp->print( std::cout, 0 );
	return temp;
} // fieldDecl

#define NEW_ZERO new ExpressionNode( build_constantInteger( yylloc, *new string( "0" ) ) )
#define NEW_ONE  new ExpressionNode( build_constantInteger( yylloc, *new string( "1" ) ) )
#define UPDOWN( compop, left, right ) (compop == OperKinds::LThan || compop == OperKinds::LEThan ? left : right)
#define MISSING_ANON_FIELD "syntax error, missing loop fields with an anonymous loop index is meaningless as loop index is unavailable in loop body."
#define MISSING_LOW "syntax error, missing low value for up-to range so index is uninitialized."
#define MISSING_HIGH "syntax error, missing high value for down-to range so index is uninitialized."

static ForCtrl * makeForCtrl(
		const CodeLocation & location,
		DeclarationNode * init,
		enum OperKinds compop,
		ExpressionNode * comp,
		ExpressionNode * inc ) {
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

ForCtrl * forCtrl( const CodeLocation & location, DeclarationNode * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( index->initializer ) {
		SemanticError( yylloc, "syntax error, direct initialization disallowed. Use instead: type var; initialization ~ comparison ~ increment." );
	} // if
	if ( index->next ) {
		SemanticError( yylloc, "syntax error, multiple loop indexes disallowed in for-loop declaration." );
	} // if
	DeclarationNode * initDecl = index->addInitializer( new InitializerNode( start ) );
	return makeForCtrl( location, initDecl, compop, comp, inc );
} // forCtrl

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, string * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	ast::ConstantExpr * constant = dynamic_cast<ast::ConstantExpr *>(type->expr.get());
	if ( constant && (constant->rep == "0" || constant->rep == "1") ) {
		type = new ExpressionNode( new ast::CastExpr( location, maybeMoveBuild(type), new ast::BasicType( ast::BasicType::SignedInt ) ) );
	} // if
	DeclarationNode * initDecl = distAttr(
		DeclarationNode::newTypeof( type, true ),
		DeclarationNode::newName( index )->addInitializer( new InitializerNode( start ) )
	);
	return makeForCtrl( location, initDecl, compop, comp, inc );
} // forCtrl

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, ExpressionNode * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index->expr.get()) ) {
		return forCtrl( location, type, new string( identifier->name ), start, compop, comp, inc );
	} else if ( auto commaExpr = dynamic_cast<ast::CommaExpr *>( index->expr.get() ) ) {
		if ( auto identifier = commaExpr->arg1.as<ast::NameExpr>() ) {
			return forCtrl( location, type, new string( identifier->name ), start, compop, comp, inc );
		} else {
			SemanticError( yylloc, "syntax error, loop-index name missing. Expression disallowed." ); return nullptr;
		} // if
	} else {
		SemanticError( yylloc, "syntax error, loop-index name missing. Expression disallowed. ." ); return nullptr;
	} // if
} // forCtrl

static void IdentifierBeforeIdentifier( string & identifier1, string & identifier2, const char * kind ) {
	SemanticError( yylloc, ::toString( "syntax error, adjacent identifiers \"", identifier1, "\" and \"", identifier2, "\" are not meaningful in a", kind, ".\n"
				   "Possible cause is misspelled type name or missing generic parameter." ) );
} // IdentifierBeforeIdentifier

static void IdentifierBeforeType( string & identifier, const char * kind ) {
	SemanticError( yylloc, ::toString( "syntax error, identifier \"", identifier, "\" cannot appear before a ", kind, ".\n"
				   "Possible cause is misspelled storage/CV qualifier, misspelled typename, or missing generic parameter." ) );
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

#line 327 "Parser/parser.cc"

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
    OFFSETOF = 309,
    BASETYPEOF = 310,
    TYPEID = 311,
    ENUM = 312,
    STRUCT = 313,
    UNION = 314,
    EXCEPTION = 315,
    GENERATOR = 316,
    COROUTINE = 317,
    MONITOR = 318,
    THREAD = 319,
    OTYPE = 320,
    FTYPE = 321,
    DTYPE = 322,
    TTYPE = 323,
    TRAIT = 324,
    LABEL = 325,
    SUSPEND = 326,
    ATTRIBUTE = 327,
    EXTENSION = 328,
    IF = 329,
    ELSE = 330,
    SWITCH = 331,
    CASE = 332,
    DEFAULT = 333,
    DO = 334,
    WHILE = 335,
    FOR = 336,
    BREAK = 337,
    CONTINUE = 338,
    GOTO = 339,
    RETURN = 340,
    CHOOSE = 341,
    FALLTHRU = 342,
    FALLTHROUGH = 343,
    WITH = 344,
    WHEN = 345,
    WAITFOR = 346,
    WAITUNTIL = 347,
    CORUN = 348,
    COFOR = 349,
    DISABLE = 350,
    ENABLE = 351,
    TRY = 352,
    THROW = 353,
    THROWRESUME = 354,
    AT = 355,
    ASM = 356,
    ALIGNAS = 357,
    ALIGNOF = 358,
    GENERIC = 359,
    STATICASSERT = 360,
    IDENTIFIER = 361,
    TYPEDIMname = 362,
    TYPEDEFname = 363,
    TYPEGENname = 364,
    TIMEOUT = 365,
    WAND = 366,
    WOR = 367,
    CATCH = 368,
    RECOVER = 369,
    CATCHRESUME = 370,
    FIXUP = 371,
    FINALLY = 372,
    INTEGERconstant = 373,
    CHARACTERconstant = 374,
    STRINGliteral = 375,
    DIRECTIVE = 376,
    FLOATING_DECIMALconstant = 377,
    FLOATING_FRACTIONconstant = 378,
    FLOATINGconstant = 379,
    ARROW = 380,
    ICR = 381,
    DECR = 382,
    LS = 383,
    RS = 384,
    LE = 385,
    GE = 386,
    EQ = 387,
    NE = 388,
    ANDAND = 389,
    OROR = 390,
    ELLIPSIS = 391,
    EXPassign = 392,
    MULTassign = 393,
    DIVassign = 394,
    MODassign = 395,
    PLUSassign = 396,
    MINUSassign = 397,
    LSassign = 398,
    RSassign = 399,
    ANDassign = 400,
    ERassign = 401,
    ORassign = 402,
    ErangeUpEq = 403,
    ErangeDown = 404,
    ErangeDownEq = 405,
    ATassign = 406,
    THEN = 407
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
#define OFFSETOF 309
#define BASETYPEOF 310
#define TYPEID 311
#define ENUM 312
#define STRUCT 313
#define UNION 314
#define EXCEPTION 315
#define GENERATOR 316
#define COROUTINE 317
#define MONITOR 318
#define THREAD 319
#define OTYPE 320
#define FTYPE 321
#define DTYPE 322
#define TTYPE 323
#define TRAIT 324
#define LABEL 325
#define SUSPEND 326
#define ATTRIBUTE 327
#define EXTENSION 328
#define IF 329
#define ELSE 330
#define SWITCH 331
#define CASE 332
#define DEFAULT 333
#define DO 334
#define WHILE 335
#define FOR 336
#define BREAK 337
#define CONTINUE 338
#define GOTO 339
#define RETURN 340
#define CHOOSE 341
#define FALLTHRU 342
#define FALLTHROUGH 343
#define WITH 344
#define WHEN 345
#define WAITFOR 346
#define WAITUNTIL 347
#define CORUN 348
#define COFOR 349
#define DISABLE 350
#define ENABLE 351
#define TRY 352
#define THROW 353
#define THROWRESUME 354
#define AT 355
#define ASM 356
#define ALIGNAS 357
#define ALIGNOF 358
#define GENERIC 359
#define STATICASSERT 360
#define IDENTIFIER 361
#define TYPEDIMname 362
#define TYPEDEFname 363
#define TYPEGENname 364
#define TIMEOUT 365
#define WAND 366
#define WOR 367
#define CATCH 368
#define RECOVER 369
#define CATCHRESUME 370
#define FIXUP 371
#define FINALLY 372
#define INTEGERconstant 373
#define CHARACTERconstant 374
#define STRINGliteral 375
#define DIRECTIVE 376
#define FLOATING_DECIMALconstant 377
#define FLOATING_FRACTIONconstant 378
#define FLOATINGconstant 379
#define ARROW 380
#define ICR 381
#define DECR 382
#define LS 383
#define RS 384
#define LE 385
#define GE 386
#define EQ 387
#define NE 388
#define ANDAND 389
#define OROR 390
#define ELLIPSIS 391
#define EXPassign 392
#define MULTassign 393
#define DIVassign 394
#define MODassign 395
#define PLUSassign 396
#define MINUSassign 397
#define LSassign 398
#define RSassign 399
#define ANDassign 400
#define ERassign 401
#define ORassign 402
#define ErangeUpEq 403
#define ErangeDown 404
#define ErangeDownEq 405
#define ATassign 406
#define THEN 407

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

	Token tok;
	ExpressionNode * expr;
	DeclarationNode * decl;
	ast::AggregateDecl::Aggregate aggKey;
	ast::TypeDecl::Kind tclass;
	StatementNode * stmt;
	ClauseNode * clause;
	ast::WaitForStmt * wfs;
    ast::WaitUntilStmt::ClauseNode * wucn;
	CondCtl * ifctl;
	ForCtrl * forctl;
	LabelNode * labels;
	InitializerNode * init;
	OperKinds oper;
	std::string * str;
	bool is_volatile;
	EnumHiding enum_hiding;
	ast::ExceptionKind except_kind;
	ast::GenericExpr * genexpr;

#line 705 "Parser/parser.cc"

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
#define YYFINAL  146
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   23741

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  180
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  311
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1108
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2191

#define YYUNDEFTOK  2
#define YYMAXUTOK   407


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
       2,     2,     2,   169,     2,     2,     2,   173,   166,     2,
     154,   156,   165,   167,   160,   168,   157,   172,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   161,   179,
     174,   178,   175,   177,   155,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   158,   171,   159,   164,     2,   163,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   162,   176,   153,   170,     2,     2,     2,
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
     145,   146,   147,   148,   149,   150,   151,   152
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   608,   608,   612,   619,   620,   621,   622,   623,   627,
     628,   629,   630,   631,   632,   633,   634,   638,   639,   643,
     644,   649,   653,   654,   665,   667,   669,   673,   674,   676,
     678,   680,   682,   692,   694,   696,   698,   700,   702,   707,
     708,   719,   724,   729,   730,   735,   741,   743,   745,   751,
     753,   757,   759,   761,   763,   765,   767,   769,   771,   773,
     775,   777,   779,   781,   783,   785,   787,   797,   798,   802,
     803,   808,   811,   815,   816,   820,   821,   823,   825,   827,
     829,   831,   836,   838,   840,   848,   849,   857,   860,   861,
     863,   868,   884,   886,   888,   890,   892,   894,   896,   898,
     900,   908,   909,   911,   915,   916,   917,   918,   922,   923,
     925,   927,   929,   931,   933,   935,   937,   944,   945,   946,
     947,   951,   952,   956,   957,   962,   963,   965,   967,   972,
     973,   975,   980,   981,   983,   988,   989,   991,   993,   995,
    1000,  1001,  1003,  1008,  1009,  1014,  1015,  1020,  1021,  1026,
    1027,  1032,  1033,  1038,  1039,  1042,  1047,  1052,  1053,  1061,
    1067,  1068,  1072,  1073,  1077,  1078,  1082,  1083,  1084,  1085,
    1086,  1087,  1088,  1089,  1090,  1091,  1092,  1102,  1104,  1109,
    1110,  1112,  1114,  1119,  1120,  1126,  1127,  1133,  1134,  1135,
    1136,  1137,  1138,  1139,  1140,  1141,  1142,  1143,  1144,  1145,
    1146,  1148,  1149,  1155,  1157,  1167,  1169,  1177,  1178,  1183,
    1185,  1187,  1189,  1191,  1195,  1196,  1198,  1203,  1210,  1212,
    1214,  1224,  1226,  1228,  1233,  1238,  1241,  1246,  1248,  1250,
    1252,  1260,  1261,  1263,  1267,  1269,  1273,  1275,  1276,  1278,
    1280,  1285,  1286,  1290,  1295,  1296,  1300,  1302,  1307,  1309,
    1314,  1316,  1318,  1320,  1325,  1327,  1329,  1331,  1336,  1338,
    1343,  1344,  1366,  1368,  1373,  1376,  1378,  1381,  1383,  1386,
    1388,  1393,  1398,  1400,  1405,  1410,  1412,  1414,  1416,  1418,
    1421,  1423,  1426,  1428,  1433,  1439,  1442,  1444,  1449,  1455,
    1457,  1462,  1468,  1471,  1473,  1476,  1478,  1483,  1490,  1492,
    1497,  1503,  1505,  1510,  1516,  1519,  1524,  1534,  1536,  1538,
    1543,  1545,  1550,  1551,  1553,  1558,  1560,  1565,  1567,  1569,
    1571,  1574,  1578,  1581,  1585,  1587,  1589,  1591,  1593,  1595,
    1597,  1599,  1601,  1603,  1605,  1610,  1611,  1615,  1621,  1629,
    1634,  1635,  1639,  1640,  1646,  1650,  1651,  1654,  1656,  1661,
    1664,  1666,  1668,  1671,  1673,  1678,  1683,  1684,  1688,  1693,
    1695,  1700,  1702,  1707,  1709,  1711,  1716,  1721,  1726,  1731,
    1733,  1735,  1740,  1742,  1748,  1749,  1753,  1754,  1755,  1756,
    1760,  1765,  1766,  1768,  1770,  1772,  1776,  1780,  1781,  1785,
    1787,  1789,  1791,  1793,  1799,  1800,  1806,  1807,  1811,  1812,
    1817,  1819,  1828,  1829,  1831,  1836,  1841,  1852,  1853,  1857,
    1858,  1864,  1865,  1869,  1871,  1875,  1877,  1881,  1882,  1886,
    1887,  1891,  1898,  1899,  1903,  1905,  1920,  1921,  1922,  1923,
    1925,  1929,  1931,  1935,  1942,  1944,  1946,  1951,  1952,  1954,
    1956,  1958,  1990,  1993,  1998,  2000,  2006,  2011,  2016,  2027,
    2034,  2039,  2041,  2043,  2049,  2053,  2060,  2062,  2063,  2064,
    2080,  2082,  2085,  2087,  2090,  2095,  2096,  2100,  2101,  2102,
    2103,  2113,  2114,  2115,  2124,  2125,  2126,  2130,  2131,  2132,
    2141,  2142,  2143,  2148,  2149,  2158,  2159,  2164,  2165,  2169,
    2171,  2173,  2175,  2177,  2182,  2187,  2188,  2190,  2200,  2201,
    2206,  2208,  2210,  2212,  2214,  2216,  2219,  2221,  2223,  2228,
    2230,  2232,  2234,  2236,  2238,  2240,  2242,  2244,  2246,  2248,
    2250,  2252,  2254,  2256,  2258,  2260,  2262,  2264,  2266,  2268,
    2270,  2272,  2274,  2276,  2278,  2280,  2282,  2287,  2288,  2292,
    2299,  2300,  2306,  2307,  2309,  2311,  2313,  2318,  2320,  2325,
    2326,  2328,  2330,  2335,  2337,  2339,  2341,  2343,  2345,  2350,
    2357,  2359,  2361,  2366,  2374,  2373,  2377,  2385,  2386,  2388,
    2390,  2395,  2396,  2398,  2403,  2404,  2406,  2408,  2413,  2414,
    2416,  2421,  2423,  2425,  2427,  2428,  2430,  2435,  2437,  2439,
    2444,  2451,  2455,  2456,  2461,  2460,  2465,  2464,  2474,  2473,
    2484,  2483,  2493,  2498,  2499,  2504,  2510,  2528,  2529,  2533,
    2535,  2537,  2543,  2545,  2547,  2549,  2554,  2556,  2561,  2563,
    2572,  2573,  2578,  2587,  2592,  2594,  2596,  2605,  2607,  2608,
    2609,  2611,  2613,  2614,  2619,  2620,  2621,  2626,  2628,  2631,
    2634,  2641,  2642,  2643,  2649,  2654,  2656,  2662,  2663,  2669,
    2670,  2674,  2679,  2681,  2684,  2683,  2687,  2689,  2696,  2698,
    2702,  2705,  2704,  2715,  2719,  2723,  2727,  2732,  2733,  2738,
    2743,  2751,  2753,  2755,  2757,  2762,  2763,  2769,  2770,  2771,
    2778,  2779,  2781,  2782,  2783,  2785,  2787,  2794,  2795,  2797,
    2799,  2804,  2805,  2811,  2812,  2814,  2815,  2820,  2821,  2822,
    2824,  2832,  2833,  2835,  2838,  2840,  2844,  2845,  2846,  2848,
    2850,  2855,  2857,  2862,  2864,  2873,  2875,  2880,  2881,  2882,
    2886,  2887,  2888,  2893,  2894,  2899,  2900,  2901,  2902,  2906,
    2907,  2912,  2913,  2914,  2915,  2916,  2930,  2931,  2936,  2937,
    2943,  2945,  2948,  2950,  2952,  2975,  2976,  2982,  2983,  2989,
    2988,  2998,  2997,  3001,  3007,  3013,  3014,  3016,  3020,  3025,
    3027,  3029,  3031,  3037,  3038,  3042,  3043,  3048,  3050,  3057,
    3059,  3060,  3062,  3067,  3069,  3071,  3076,  3078,  3083,  3088,
    3096,  3101,  3103,  3108,  3113,  3114,  3119,  3120,  3124,  3125,
    3126,  3131,  3133,  3139,  3141,  3146,  3148,  3154,  3155,  3159,
    3163,  3167,  3169,  3182,  3184,  3186,  3188,  3190,  3192,  3194,
    3195,  3200,  3203,  3202,  3214,  3213,  3226,  3225,  3239,  3238,
    3252,  3251,  3267,  3273,  3275,  3281,  3282,  3293,  3300,  3305,
    3311,  3314,  3317,  3321,  3327,  3330,  3333,  3338,  3339,  3340,
    3341,  3345,  3351,  3352,  3362,  3363,  3367,  3368,  3373,  3378,
    3379,  3385,  3386,  3388,  3393,  3394,  3395,  3396,  3397,  3399,
    3434,  3436,  3441,  3443,  3444,  3446,  3451,  3453,  3455,  3457,
    3462,  3464,  3466,  3468,  3470,  3472,  3474,  3479,  3481,  3483,
    3485,  3494,  3496,  3497,  3502,  3504,  3506,  3508,  3510,  3515,
    3517,  3519,  3521,  3526,  3528,  3530,  3532,  3534,  3536,  3548,
    3549,  3550,  3554,  3556,  3558,  3560,  3562,  3567,  3569,  3571,
    3573,  3578,  3580,  3582,  3584,  3586,  3588,  3600,  3605,  3610,
    3612,  3613,  3615,  3620,  3622,  3624,  3626,  3631,  3633,  3635,
    3637,  3639,  3641,  3643,  3648,  3650,  3652,  3654,  3663,  3665,
    3666,  3671,  3673,  3675,  3677,  3679,  3684,  3686,  3688,  3690,
    3695,  3697,  3699,  3701,  3703,  3705,  3715,  3717,  3719,  3720,
    3722,  3727,  3729,  3731,  3736,  3738,  3740,  3742,  3747,  3749,
    3751,  3765,  3767,  3769,  3770,  3772,  3777,  3779,  3784,  3786,
    3788,  3793,  3795,  3800,  3802,  3819,  3820,  3822,  3827,  3829,
    3831,  3833,  3835,  3840,  3841,  3843,  3845,  3850,  3852,  3854,
    3860,  3862,  3865,  3868,  3870,  3874,  3876,  3878,  3879,  3881,
    3883,  3887,  3889,  3894,  3896,  3898,  3900,  3935,  3936,  3940,
    3941,  3943,  3945,  3950,  3952,  3954,  3956,  3958,  3963,  3964,
    3966,  3968,  3973,  3975,  3977,  3983,  3984,  3986,  3995,  3998,
    4000,  4003,  4005,  4007,  4021,  4022,  4024,  4029,  4031,  4033,
    4035,  4037,  4042,  4043,  4045,  4047,  4052,  4054,  4062,  4063,
    4064,  4069,  4070,  4075,  4077,  4079,  4081,  4083,  4085,  4092,
    4094,  4096,  4098,  4100,  4103,  4105,  4107,  4109,  4111,  4116,
    4118,  4120,  4125,  4151,  4152,  4154,  4158,  4159,  4163,  4165,
    4167,  4169,  4171,  4173,  4180,  4182,  4184,  4186,  4188,  4190,
    4195,  4197,  4199,  4206,  4208,  4226,  4228,  4233,  4234
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
  "VA_LIST", "VA_ARG", "AUTO_TYPE", "OFFSETOF", "BASETYPEOF", "TYPEID",
  "ENUM", "STRUCT", "UNION", "EXCEPTION", "GENERATOR", "COROUTINE",
  "MONITOR", "THREAD", "OTYPE", "FTYPE", "DTYPE", "TTYPE", "TRAIT",
  "LABEL", "SUSPEND", "ATTRIBUTE", "EXTENSION", "IF", "ELSE", "SWITCH",
  "CASE", "DEFAULT", "DO", "WHILE", "FOR", "BREAK", "CONTINUE", "GOTO",
  "RETURN", "CHOOSE", "FALLTHRU", "FALLTHROUGH", "WITH", "WHEN", "WAITFOR",
  "WAITUNTIL", "CORUN", "COFOR", "DISABLE", "ENABLE", "TRY", "THROW",
  "THROWRESUME", "AT", "ASM", "ALIGNAS", "ALIGNOF", "GENERIC",
  "STATICASSERT", "IDENTIFIER", "TYPEDIMname", "TYPEDEFname",
  "TYPEGENname", "TIMEOUT", "WAND", "WOR", "CATCH", "RECOVER",
  "CATCHRESUME", "FIXUP", "FINALLY", "INTEGERconstant",
  "CHARACTERconstant", "STRINGliteral", "DIRECTIVE",
  "FLOATING_DECIMALconstant", "FLOATING_FRACTIONconstant",
  "FLOATINGconstant", "ARROW", "ICR", "DECR", "LS", "RS", "LE", "GE", "EQ",
  "NE", "ANDAND", "OROR", "ELLIPSIS", "EXPassign", "MULTassign",
  "DIVassign", "MODassign", "PLUSassign", "MINUSassign", "LSassign",
  "RSassign", "ANDassign", "ERassign", "ORassign", "ErangeUpEq",
  "ErangeDown", "ErangeDownEq", "ATassign", "THEN", "'}'", "'('", "'@'",
  "')'", "'.'", "'['", "']'", "','", "':'", "'{'", "'`'", "'^'", "'*'",
  "'&'", "'+'", "'-'", "'!'", "'~'", "'\\\\'", "'/'", "'%'", "'<'", "'>'",
  "'|'", "'?'", "'='", "';'", "$accept", "push", "pop", "constant",
  "quasi_keyword", "identifier", "identifier_at", "string_literal",
  "string_literal_list", "primary_expression", "generic_assoc_list",
  "generic_association", "postfix_expression",
  "argument_expression_list_opt", "argument_expression_list",
  "argument_expression", "field_name_list", "field", "field_name",
  "fraction_constants_opt", "unary_expression", "ptrref_operator",
  "unary_operator", "cast_expression", "qualifier_cast_list",
  "cast_modifier", "exponential_expression", "multiplicative_expression",
  "additive_expression", "shift_expression", "relational_expression",
  "equality_expression", "AND_expression", "exclusive_OR_expression",
  "inclusive_OR_expression", "logical_AND_expression",
  "logical_OR_expression", "conditional_expression", "constant_expression",
  "assignment_expression", "assignment_expression_opt",
  "assignment_operator", "simple_assignment_operator",
  "compound_assignment_operator", "tuple", "tuple_expression_list",
  "comma_expression", "comma_expression_opt", "statement",
  "labeled_statement", "compound_statement", "statement_decl_list",
  "statement_decl", "statement_list_nodecl", "expression_statement",
  "selection_statement", "if_statement", "conditional_declaration",
  "case_value", "case_value_list", "case_label", "case_label_list",
  "case_clause", "switch_clause_list_opt", "switch_clause_list",
  "iteration_statement", "for_control_expression_list",
  "for_control_expression", "downupdowneq", "updown", "updowneq",
  "jump_statement", "fall_through_name", "with_statement",
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
  "basic_type_name", "vtable_opt", "vtable", "default_opt",
  "basic_declaration_specifier", "basic_type_specifier", "direct_type",
  "indirect_type", "sue_declaration_specifier", "sue_type_specifier",
  "$@1", "sue_declaration_specifier_nobody", "sue_type_specifier_nobody",
  "type_declaration_specifier", "type_type_specifier", "type_name",
  "typegen_name", "elaborated_type", "elaborated_type_nobody",
  "aggregate_type", "$@2", "$@3", "$@4", "$@5", "type_parameters_opt",
  "aggregate_type_nobody", "aggregate_key", "aggregate_data",
  "aggregate_control", "field_declaration_list_opt", "field_declaration",
  "field_declaring_list_opt", "field_declarator",
  "field_abstract_list_opt", "field_abstract", "cfa_field_declaring_list",
  "cfa_field_abstract_list", "bit_subrange_size_opt", "bit_subrange_size",
  "enum_type", "$@6", "$@7", "hide_opt", "enum_type_nobody",
  "enumerator_list", "visible_hide_opt", "enumerator_value_opt",
  "cfa_parameter_ellipsis_list_opt", "cfa_parameter_list",
  "cfa_abstract_parameter_list", "parameter_type_list_opt",
  "parameter_list", "cfa_parameter_declaration",
  "cfa_abstract_parameter_declaration", "parameter_declaration",
  "abstract_parameter_declaration", "identifier_list",
  "identifier_or_type_name", "type_no_function", "type", "initializer_opt",
  "initializer", "initializer_list_opt", "designation", "designator_list",
  "designator", "type_parameter_list", "type_initializer_opt",
  "type_parameter", "$@8", "$@9", "new_type_class", "type_class",
  "assertion_list_opt", "assertion_list", "assertion", "type_list",
  "type_declaring_list", "type_declarator", "type_declarator_name",
  "trait_specifier", "trait_declaration_list", "trait_declaration",
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
     405,   406,   407,   125,    40,    64,    41,    46,    91,    93,
      44,    58,   123,    96,    94,    42,    38,    43,    45,    33,
     126,    92,    47,    37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1912)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1107)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      90, 12923,   134,   181, 18283,   116, -1912, -1912, -1912, -1912,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,    56,   868,
     221, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,    15,   344,
   -1912, -1912, -1912, -1912, -1912, -1912,  5986,  5986,   295, 12923,
     308,   357, 13305, -1912,   410, -1912, -1912, -1912, -1912, -1912,
   -1912, -1912, -1912, -1912, -1912,  2094, -1912,   739,   420, -1912,
   -1912, -1912, -1912, -1912, 18129, -1912, -1912,   411,   438,   343,
      25, -1912,  5986,   438,   460,   492,   479,  5554,   722,   974,
   13087, -1912, -1912,   720, 17975,  1497, -1912, -1912, -1912,  3055,
     806,  9652, 11510,  1041,  3055,  1196,   677, -1912, -1912, -1912,
   -1912,   784, -1912, -1912, -1912, -1912,   723, -1912, -1912, -1912,
   -1912, -1912,   711,   728,   784, -1912,   784, 16600, -1912, -1912,
   -1912, 19244,  5986, -1912, -1912,  5986, -1912, 12923, -1912,   745,
   19297, -1912, -1912,  5616, 20435, -1912, -1912,   925,   925,   771,
    2358, -1912, -1912, -1912, -1912,   477, 15191,  3594,   784, -1912,
   -1912, -1912, -1912, -1912, -1912,   789, -1912,   777,   802,   849,
   -1912,   900, 23115, -1912, -1912, -1912, -1912, -1912, -1912, -1912,
   16977,  3716,  2094,   627,   854,   874,   876,   903,   909,   911,
   -1912, -1912, 19451, 11922,   929, -1912, 18584, -1912, -1912, -1912,
   -1912,   931, -1912, -1912,   915, -1912,  4830,  1055, 21291, -1912,
     940,  5986,   728,   958,   962,  5616,  2414, -1912, -1912, -1912,
    3174,  4105,   967,  1036,   384,  1036, -1912,   784,   784,    59,
   16386,   430,  1036, -1912,   784,   784,    59,   784, -1912,   784,
   -1912,  4140, -1912, -1912,   988,   991,   925, 14755, -1912, 18129,
   -1912, -1912,  3055, -1912,  1046,   677,   996,  1077, 16386,  5986,
    5986,   343, -1912, 14376, -1912,   925,   925,  1012,  1077, 16386,
    5986, -1912,  9154, -1912, -1912, -1912,   925, -1912, -1912, -1912,
   -1912,   925, -1912,   801,  5289,  5986, -1912, 17830,  1073, -1912,
   -1912, -1912, 20942,   728, 16493,  1052,  5616, 17777, 14755,  3055,
   -1912, -1912, 20486, -1912,  1036,   266, -1912, 23115, 20435,  4899,
    4140, -1912,   452, -1912, -1912, -1912, -1912, -1912, 19297,  5986,
   -1912,  1032, -1912, -1912, -1912, -1912,  5986,  3331,   599,   319,
   -1912,  5986,   777, -1912,   812,   784,   784,  1074, 19504,   658,
   15680, 14918,  3055, -1912,  3055,   925,  3055,   925, -1912, -1912,
     784, -1912,  1109, -1912, 19658, -1912, -1912, -1912, 19711,   931,
   -1912,    92,   725,   408,  1122,   285,   677,  1102, -1912,  2358,
    1108,   777,  2358,  1304, -1912,  1136,  1178, 23191,  1146,  1150,
    1171, 23115, 23267,  1182, 14593, -1912, -1912, -1912, -1912, -1912,
   -1912, 23343, 23343, 16819,  1181,  4445, -1912, -1912, -1912, -1912,
     400, -1912,   794, -1912,  1180, -1912, 23115, 23115, -1912,  1139,
     743,   872,   921,   472,   964,  1191,  1210,  1168,  1259,   136,
   -1912,   563, -1912,  1240, -1912,   955,  5906, 17293, -1912, -1912,
     800,  1240, -1912, -1912,   761, -1912, -1912,  3716,  1246,  1266,
    1268,  1270,  1274,  1288, -1912, -1912,   464,  1293, -1912,   791,
    1293, -1912, -1912, 19244, -1912,   980,  1312, 17451, -1912, -1912,
    5523,  4913,  1339, 15680,  1356,   756,   890, -1912, -1912, -1912,
   -1912, -1912,  5986,  5677, -1912, -1912, -1912, -1912, -1912, -1912,
    5902,  2286,  1181,  4830,  1335,  1338, -1912, -1912,  1355, 21291,
     830, -1912, -1912, -1912, 21367,  1364, -1912, -1912, -1912, -1912,
    1346,  3174,   912,  1384,  1388,  1390,   998,  1393,  1414,  1416,
    1418,  1421,  1425,  4105, -1912, -1912, -1912,   784,  1434,  1436,
    1433, -1912, -1912,  1437,   343, -1912, -1912,   728,  1077, 18446,
   -1912, -1912,   343, -1912, -1912,   728, -1912, -1912,  4140, -1912,
   17293, 17293, -1912,   925,  5616, 21047, 15843, -1912, -1912, -1912,
   -1912, -1912,   728,  1077,   266,  1441, -1912, -1912,  3055,  1450,
    1077, 16386, -1912,   728,  1077, -1912, 16057, -1912,   925,   925,
   -1912, -1912,  1455,   524,  1464,   677,  1465, -1912, -1912, -1912,
   18883,  1443,  1474, -1912, -1912,   846, -1912,  1565, -1912,  1458,
   -1912, -1912, -1912, 19874, 23419, -1912, -1912, -1912, -1912, -1912,
    4899,  1013,  4140, 18446, 15843,  1036, 12923, -1912,  1480, -1912,
    1487, -1912, -1912, -1912, -1912, -1912,  2358, -1912, -1912,  1564,
    5452,  3088, 19711, 11922, -1912, 19927, -1912,   925,   925, -1912,
   -1912,   931, -1912, 14702,  1484,  1627, 23115,  1085,  1437,  1469,
   -1912,   784,   784, -1912,  1293, -1912, 19504, -1912, -1912, 18883,
     925,   925, -1912,  5452,   784, -1912, 20288, -1912, -1912, 19658,
   -1912,   477, -1912, -1912, -1912,  1488,  5986,   408,  1102,  1492,
     871, 19297,   916, -1912, -1912, -1912, -1912, -1912, -1912,   922,
   -1912,  1495,  1471, -1912, 17135, -1912,  4445, 20081, 20081, -1912,
   17135, -1912, 23115, -1912, -1912, -1912, -1912, -1912, -1912, 17135,
   -1912, -1912, 18936, 20081, 20081,   955,  1391,  1468,   718,  1518,
   -1912,   935,  1499,  1035,  1502, -1912, 21367, 23115, 21443,  1498,
   23115,  2414, 23115,  2414, -1912,  1018, -1912, -1912, 21519,  2709,
   23115, 21519,  2414, -1912, -1912, 23115, 23115, 23115, 23115, 23115,
   23115, 23115, 23115, 23115, 23115, 23115, 23115, 23115, 23115, 23115,
   23115, 23115, 23115, 23115, 21595,  1489,   900,  5211, 11922, -1912,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,
    1509, 23115, -1912, -1912, 14865,  1444, -1912, -1912,   784,   784,
   -1912, -1912, 17293, -1912,   503,  1293, -1912,   993,  1293, 18446,
   -1912, -1912,  1437, 18446, -1912,  1437, 23495, -1912, -1912, 11922,
    1513,  1514, 14213,  1663,  4314,   506,  1469, -1912,   784,   784,
    1469,   541, -1912,   784,   784, 23115,  5986,  1156,  1160,  1469,
     387, 15028, 15028,  5986, -1912, -1912, 23115,  1355, -1912,  4830,
    1529, -1912,  2192, -1912, -1912, -1912, -1912, -1912,   999, -1912,
   15028,  2414, 23115,  1014,  1527,  1530,  1536,  1026,  1539,  1542,
    1543,  1545,  1547,  1548,   605,  1293, -1912, -1912,   621,  1293,
   -1912, -1912,   630,  1293, -1912, -1912, -1912,  5616,   900,  1684,
    1293, 20633, -1912, -1912,   728,  1551, -1912, -1912, -1912,  1025,
    1552,  1027,  1553, -1912,  1532, -1912,   728, -1912,  1558, -1912,
     728,  1077,  1532, -1912,   728,  1550,  1556,  1562, -1912, -1912,
   18747, -1912,  2414,  5986, 10980,  1644, -1912,  1312, -1912, 15028,
    1038, -1912, -1912,  1532, -1912, 19297, 17293,  1549, -1912,  1549,
   -1912, -1912, -1912,   408,  1563,   784,   784, -1912, 19658, -1912,
   12089, 17609, -1912,  1573,  1575,  1580,  1581, -1912,  8168,   784,
   -1912,  1085, -1912, -1912, -1912, -1912,  1437, -1912, -1912, -1912,
     925, -1912,  4350, -1912, -1912,   677,   418,  1585,  1561,  1488,
    1572,   408, -1912, -1912,  1579,  1584,  1304, 21519, -1912,  1586,
    1583,   420,  1589,  1590,  1594,  1591,  1596, 23115,  1600,  1601,
    1602, 11922, 23115, -1912, -1912,  1649, -1912, -1912, -1912, 23115,
   -1912,  1604,  1605, 21215,  1163, -1912, 21519,  1606, -1912,  1608,
   -1912, -1912,  3347, -1912, -1912,  1037, -1912, -1912, -1912, -1912,
    3347, -1912, -1912,  1173,   137, -1912, -1912,  1139,  1139,  1139,
     743,   743,   872,   872,   921,   921,   921,   921,   472,   472,
     964,  1191,  1210,  1168,  1259, 23115,  1205, -1912,  1607,  3347,
   -1912, -1912,  4830, -1912,  1612,  1615,  1616,  1618,  1444, -1912,
   -1912, -1912, -1912, -1912, 18446, -1912, -1912,  1437, 18446, -1912,
    1437,  1619,  1620, -1912, -1912, 14213,  1021,  1621,  1623,  1625,
    1628,  2212,  4314, -1912, -1912, 18446, -1912, -1912, -1912, -1912,
   -1912, -1912, 18446, -1912, -1912, -1912, -1912,  1624, -1912,  1469,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,  1629,  1630,
   -1912,   343,  3347,  1209,   -68, -1912, -1912,  1592, -1912, 21291,
   -1912, 23115,   784, 21671, 15028, -1912, -1912, -1912,   633,  1293,
   -1912,   642,  1293, -1912, -1912,   648,  1293, 18446, -1912, -1912,
    1437, 18446, -1912, -1912,  1437, 18446, -1912, -1912,  1437,  1036,
    1631, -1912,  1437,    -3, -1912,  1240,  1636, -1912, -1912, -1912,
   -1912, -1912, -1912,  1638, -1912, -1912, -1912, 19297,  1532, -1912,
     728, -1912, -1912, -1912, -1912, -1912,  9336, -1912, -1912, -1912,
   -1912,   365, -1912,   454,   532, 11755,  1635, 16209,  1643,  1645,
    2456,  2595,  2444, 21747,  1646, -1912, -1912,  1647,  1651, 16209,
    1652, -1912, -1912,   728, 23115, 23115,  1784,  1642,   502, -1912,
   16661,  1734,  1650,  1633, -1912, -1912, -1912, 10803, -1912, -1912,
   -1912, -1912, -1912,  2427, -1912, -1912, -1912,  1280,   164, -1912,
     354, -1912,   164, -1912, -1912, -1912, -1912, -1912,  2414, -1912,
   -1912, 13251, 18129,  1648, -1912,  5986,  1654,  1655, -1912,  1217,
   -1912, -1912, -1912, -1912,  5616, -1912, -1912,  1639,  1657,  1058,
   19297,   777,   777,  1488,   408,  1102,  1102, -1912, -1912,  1181,
    1312, 17451, -1912,  1240, -1912, 12256, -1912,   650,  1293, -1912,
     925,  9729, -1912, -1912,   408,  1662,   784,   784,   477,  5986,
   -1912, 21823, -1912,  1664,   408,  1488,  1670, -1912, -1912,  1061,
     527, 18883, 11922,  2414, -1912,   527, 19090,   527, -1912, 23115,
   23115, 23115, -1912, -1912, -1912, -1912, 23115, 23115,  1666,  4830,
   -1912, -1912,  1669,   538, -1912, -1912, -1912,  4680, -1912, -1912,
    1223, -1912,   300, -1912, 21519,  1228, -1912, 21367, -1912, -1912,
   23115,  1658,  1236,  1241,  1355, -1912,   661,  1293, -1912, -1912,
    1674,  1675, -1912, -1912,  1682,   672,  1293, -1912,   686,  2630,
     784,   784, -1912, -1912,  1685,  1687, -1912,  1689, -1912, 15843,
   15843,  1693,  1690,  1692,  1698, -1912,  1696, 23115, 23115,  1269,
    1700, -1912, -1912, -1912, -1912, -1912, -1912, -1912,  1707, 18446,
   -1912, -1912,  1437, 18446, -1912, -1912,  1437, 18446, -1912, -1912,
    1437,  1708,  1710,  1713,   343,   784, -1912, -1912,  1278, 23115,
   20784,  1711,  1718, -1912, -1912, -1912,  1719, 13736, 13893, 14050,
   19297, 14755, 20081, 20081,  1720, -1912,  1694,   367,  2342,  6214,
   -1912,   371,  5986,  5986, -1912, 21519,   462,   474, -1912, -1912,
   -1912, -1912, 23115,  1721,  1797, 11587, 11157, -1912,  1699, -1912,
    1701, 23115,  1705,  4830,  1709, 23115, 21367, 23115, -1912, 11334,
    1238, -1912,  1715,    16, -1912,    55,  1787,   315,  1732, -1912,
   -1912,  1736, -1912,  1717, -1912,  1724,  1733,  1738, 16209, 16209,
   -1912, -1912,  1800, -1912, -1912,   332,   332,   671, 14539,   784,
     383, -1912, -1912,  1742, -1912,  1747, -1912,  1752, -1912,  1746,
   -1912,  1748, -1912, -1912, -1912, -1912,  1754,  1488,  1749,  1750,
   12423,  1756,  1757,  1759, -1912, 18446, -1912, -1912,  1437, 23115,
   23115,  1312,  1761, -1912,  1488,   408, -1912,  1102,   256,  1561,
    4830, -1912, -1912,  1488,  1760, -1912, 19297, -1912,   847,  1753,
    1766,  1064, -1912,  1769, -1912, -1912, -1912, -1912, -1912,  4830,
    1355, 21367, -1912,  1799,  3347, -1912,  1799,  1799, -1912,  3347,
    4803,  5877, -1912, -1912,  1283, -1912, -1912, -1912,  1778, 18446,
   -1912, -1912,  1437, -1912, -1912,   784, 18446, -1912, -1912,  1437,
   18446, -1912, -1912,  1776, -1912, -1912, -1912, -1912, -1912, -1912,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912,  1774, -1912, -1912,
   -1912, -1912,  1779,  1781,   784,  1785,  1788,  1791, -1912, -1912,
   -1912, -1912, -1912, 23115, -1912,    -3, -1912,  1240, -1912, -1912,
    1782,  1795, -1912,  1720,  1720,  1720,  5274,   850,  1770,   461,
   -1912,  5274,   508, 17293, -1912, -1912, -1912,  5082, 23115,  4653,
     641, -1912, -1912,    43,  1789,  1789,  1789,  5986, -1912, -1912,
   -1912,  1067, -1912, -1912, -1912, -1912,  1072,  1786, 16209,  1650,
    1796, 23115,   411,  1794,   479, 10007, 19297, -1912, -1912, -1912,
     971, 16209, 23115,   721,   811, -1912, 23115, 21057, -1912, -1912,
     552, -1912,  1355, -1912,  1086,  1099,  1101,   824, -1912, -1912,
   -1912, -1912,   728,  1238,  1801, -1912, -1912, 23115, -1912,  1802,
     900, -1912, 11755, -1912, -1912, -1912, -1912, 23115, 23115, -1912,
   -1912,   182,   332, -1912,   735, -1912, -1912, 10626, -1912,   784,
   15843, -1912, -1912, 19297, -1912, -1912, -1912,  1798,   408,   408,
   -1912, -1912, -1912,  1804,  1805, -1912, -1912,  1811, -1912,  1813,
    1820,  1488,  1102,  1814, -1912, -1912,  1355,  1822, -1912, -1912,
    1808, -1912, -1912, 23115, -1912, 19090, 23115,  1355,  1824,  1287,
   -1912,  1289, -1912,  3347, -1912,  3347, -1912, -1912, -1912,  1826,
    1830,  1832,  1294, 15354, 15517, -1912,  1819, -1912, -1912, -1912,
   -1912, -1912,  1310, 23115, -1912, -1912, -1912, -1912, -1912,   577,
     850,  1661,   583, -1912, -1912, -1912, -1912,   784,   784, -1912,
   -1912, -1912,   618, -1912,  1104,  5082,   821, -1912,  4653, -1912,
     784, -1912, -1912, -1912, -1912, -1912, -1912, 16209,   210, 21899,
    1883, 16209,  1650, 16006, -1912, -1912, -1912, -1912, 23115, -1912,
   21975,  1916,  1815, 21136, 22051, 16209, 11334,  1650,   749,  1285,
    1816, 23115, -1912,  1839,   419, 16209, -1912, 16209, -1912,  1842,
   -1912, -1912,  1823,   900,   838,  1845,  1852,  1279,  1106, 16209,
    1844, 16209, 16209, 16209, -1912, -1912, -1912, -1912,  5986,  5616,
   -1912,  1488,  1488, -1912, -1912,  1853,  1854, -1912, -1912, -1912,
    1861,  1857,   408,  1862, -1912,  1863, -1912, -1912, -1912, -1912,
    1870, -1912, -1912, -1912,  1314,  1330, -1912, -1912, -1912, -1912,
   -1912, -1912,  1868, -1912, -1912, -1912, -1912, -1912,  1869,  1871,
    1872,  1661, -1912,   784, -1912, -1912, -1912, -1912, -1912,  1876,
    5274, -1912,  3899,    94, 12593, -1912, 16102, -1912,    13,  1117,
   16209,  1954,   639,  1860,   429, 16209, 23115,  1879,   749,  1285,
    1859, 23571,  1873,   597,  1964, -1912, 22127, 22203, 23115,  1650,
    1865, 12759, -1912, -1912, -1912, -1912, 20134, -1912,  1880,  1866,
     -23, 16209, -1912, 23115, 21519, -1912, -1912, 23115,   164, -1912,
   -1912, -1912, -1912,  1887,  1896, -1912, -1912, -1912,   408,  1488,
   -1912, -1912, -1912, -1912, -1912, 15843,  1893,   705,  1293, -1912,
   -1912,   850, -1912, -1912,   111, -1912,    78, -1912, -1912, -1912,
    1901, 13415, -1912, -1912, 16209, -1912,    18, -1912, 16209, 23115,
    1900, 22279, -1912, -1912, 22355, 22431, 23115,  1879,  1650, 22507,
   22583, 16209,  1886,   669,  1890,   682, -1912, -1912,  1906, 13415,
   20134, -1912,  5491, 19927,  2414,  1902, -1912,  1955,  1908,   839,
    1909, -1912,  1991, -1912,  1118,  1120,   227, -1912, -1912,  1488,
    1915, -1912, 18446, -1912, -1912,  1437, -1912, 23115, -1912, 23115,
   -1912, -1912,  1417, 13579, -1912, -1912, 16209, -1912, -1912,  1650,
   -1912, -1912,  1650,  1903,   708,  1904,   754, -1912, -1912,  1650,
   -1912,  1650, -1912,  1917, 22659, 22735, 22811, -1912,  1417, -1912,
    1891,  2819,  4382, -1912, -1912, -1912,   -23,  1920, 23115,  1897,
     -23,   -23, 16209, -1912, -1912, 16209,  2000,  1928, -1912,  1929,
   -1912, -1912, 16102, -1912,  1417, -1912, -1912,  1927, 22887, 22963,
   23039, -1912, -1912,  1650, -1912,  1650, -1912,  1650, -1912,  1891,
   23115,  1930,  4382,  1932,   900,  1933, -1912,   853, -1912, -1912,
   -1912, 16209, -1912, -1912, -1912, 10303,  1935, 16102, -1912, -1912,
    1650, -1912,  1650, -1912,  1650,  1941,  1939, -1912,   728,   900,
    1942, -1912,  1918,   900, -1912, -1912, -1912, -1912, 10502, -1912,
     728, -1912, -1912,  1353, 23115, -1912,  1125, -1912,   900,  2414,
    1943,  1921, -1912, -1912,  1131, -1912, -1912,  1937,  2414, -1912,
   -1912
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   483,     0,     2,   483,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   489,   491,   490,   492,     0,     0,
       0,   509,   511,   532,   512,   533,   515,   516,   530,   531,
     510,   528,   529,   513,   514,   517,   518,   519,   520,   521,
     522,   523,   524,   525,   526,   527,   534,   535,   844,   537,
     610,   611,   614,   616,   612,   618,     0,     0,     0,   483,
       0,     0,    17,   581,   587,     9,    10,    11,    12,    13,
      14,    15,    16,   801,   103,     0,    20,     0,     2,   101,
     102,    18,    19,   860,   483,   802,   423,     0,   426,   725,
     428,   437,     0,   427,   457,   458,     0,     0,     0,     0,
     564,   485,   487,   493,   483,   495,   498,   549,   536,   467,
     542,   547,   469,   559,   468,   574,   578,   584,   563,   590,
     602,   844,   607,   608,   591,   666,   429,   430,     3,   809,
     822,   488,     0,     0,   844,   882,   844,   483,   899,   900,
     901,   483,     0,  1086,  1087,     0,     1,   483,    17,     0,
     483,   446,   447,     0,   564,   493,   477,   478,   479,   812,
       0,   613,   615,   617,   619,     0,   483,     0,   845,   846,
     609,   538,   718,   719,   717,   778,   773,   763,     0,     0,
     810,     0,     0,   500,   803,   807,   808,   804,   805,   806,
     483,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     582,   585,   483,   483,     0,  1088,   564,   889,   907,  1092,
    1085,  1083,  1090,   422,     0,   165,   731,   164,     0,   431,
       0,     0,     0,     0,     0,     0,     0,   421,   976,   977,
       0,     0,   456,   842,   844,   842,   863,   844,   844,   466,
     483,   844,   842,   920,   844,   844,   465,   844,   939,   844,
     917,     0,   557,   558,     0,     0,   483,   483,     2,   483,
     438,   486,   496,   550,     0,   579,     0,   825,   483,     0,
       0,   725,   439,   564,   543,   560,   575,     0,   825,   483,
       0,   499,   544,   551,   552,   470,   561,   472,   473,   471,
     566,   576,   580,     0,   594,     0,   795,   483,     2,   823,
     881,   883,   483,     0,   483,     0,     0,   564,   483,   495,
       2,  1096,   564,  1099,   842,   842,     3,     0,   564,     0,
       0,   449,   844,   837,   839,   838,   840,     2,   483,     0,
     799,     0,   759,   761,   760,   762,     0,     0,   755,     0,
     745,     0,   754,   765,     0,   844,   844,     2,   483,  1107,
     484,   483,   474,   542,   475,   567,   476,   574,   571,   592,
     844,   593,     0,   706,   483,   707,  1061,  1062,   483,   708,
     710,   581,   587,   667,     0,   669,   670,   667,   847,     0,
     776,   764,     0,   851,    22,     0,    21,     0,     0,     0,
       0,     0,     0,     0,    24,    26,     4,     8,     5,     6,
       7,     0,     0,   483,     2,     0,   104,   105,   106,   107,
      88,    25,    89,    43,    87,   108,     0,     0,   123,   125,
     129,   132,   135,   140,   143,   145,   147,   149,   151,   153,
     156,     0,    27,     0,   588,     2,   108,   483,   157,   770,
     721,   578,   723,   769,     0,   720,   724,     0,     0,     0,
       0,     0,     0,     0,   861,   887,   844,   897,   905,   909,
     915,     2,  1094,   483,  1097,     2,   101,   483,     3,   705,
       0,  1107,     0,   484,   542,   567,   574,     3,     3,   687,
     691,   701,   707,   708,     2,   890,   908,  1084,     2,     2,
      24,     0,     2,   731,    25,     0,   729,   732,  1105,     0,
       0,   738,   727,   726,     0,     0,   827,     2,     2,   450,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   866,   923,   946,   844,     0,   462,
       2,   862,   870,  1004,   725,   864,   865,     0,   825,   483,
     919,   927,   725,   921,   922,     0,   938,   940,     0,   452,
     483,   483,   548,   484,     0,   564,   483,  1089,  1093,  1091,
     565,   799,     0,   825,   842,     0,   432,   440,   497,     0,
     825,   483,   799,     0,   825,   774,   545,   546,   562,   577,
     583,   586,   581,   587,   605,   606,     0,   775,   694,   715,
     484,     0,   695,   698,   697,     0,   205,   415,   824,     0,
     413,   466,   465,   564,     0,   434,     2,   435,   796,   454,
       0,     0,     0,   483,   483,   842,   483,   799,     0,     2,
       0,   758,   757,   756,   751,   494,     0,   749,   766,   540,
       0,     0,   483,   483,  1063,   484,   480,   481,   482,  1067,
    1058,  1059,  1065,   483,     2,   102,     0,  1023,  1037,  1107,
    1019,   844,   844,  1028,  1035,   713,   483,   572,   709,   484,
     568,   569,   573,     0,   844,  1073,   484,  1078,  1070,   483,
    1075,     0,   676,   668,   675,  1105,     0,   667,   667,     0,
       0,   483,     0,   859,   858,   854,   856,   857,   855,     0,
     849,   852,     0,    23,   483,    95,     0,   483,   483,    90,
     483,    97,     0,    33,    37,    38,    34,    35,    36,   483,
      93,    94,   483,   483,   483,     2,   104,   105,     0,     0,
     183,     0,     0,   608,     0,  1083,     0,     0,     0,     0,
       0,     0,     0,     0,    56,     0,    62,    63,    67,     0,
       0,    67,     0,    91,    92,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   483,   166,
     167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
     164,     0,   162,   163,   483,   988,   722,   985,   844,   844,
     993,   589,   483,   888,   844,   898,   906,   910,   916,   483,
     891,   893,   895,   483,   911,   913,     0,  1095,  1098,   483,
       0,     0,   483,   102,  1023,   844,  1107,   958,   844,   844,
    1107,   844,   973,   844,   844,     3,   709,     0,     0,  1107,
    1107,   483,   483,     0,     2,   740,     0,  1105,   737,  1106,
       0,   733,     0,     2,   736,   739,   180,   179,     0,     2,
     483,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   844,   875,   879,   918,   844,   932,
     936,   944,   844,   954,   867,   924,   947,     0,     0,     0,
    1000,     0,   460,   828,     0,     0,   461,   829,   453,     0,
       0,     0,     0,   451,     2,   830,     0,   436,     2,   799,
       0,   825,     2,   831,     0,     0,     0,     0,   620,   884,
     483,   902,     0,     0,   483,   416,   414,   101,     3,   483,
       0,     3,   800,     2,   753,   483,   483,   747,   746,   747,
     541,   539,   669,   667,     0,   844,   844,  1069,   483,  1074,
     484,   483,  1060,     0,     0,     0,     0,  1038,     0,   844,
    1108,  1024,  1025,   714,  1021,  1022,  1036,  1064,  1068,  1066,
     570,   605,     0,  1072,  1077,   672,   667,     0,   677,  1105,
       0,   667,   779,   777,     0,     0,   851,    67,   811,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   483,     0,   122,   121,     0,   118,   117,    28,     0,
      29,     0,     0,     0,     0,     3,    67,     0,    52,     0,
      53,    60,     0,    59,    71,     0,    68,    69,    72,    55,
       0,    54,    58,     0,     0,    51,   124,   126,   127,   128,
     130,   131,   133,   134,   138,   139,   136,   137,   141,   142,
     144,   146,   148,   150,   152,     0,     0,   425,     0,     0,
      30,     3,   731,   158,     0,     0,     0,     0,   989,   990,
     986,   987,   772,   771,   483,   892,   894,   896,   483,   912,
     914,     0,     0,  1014,  1013,   483,     0,     0,     0,     0,
       0,   844,  1024,   961,   978,   483,   956,   964,   711,   959,
     960,   712,   483,   971,   981,   974,   975,     0,     3,  1107,
     444,     2,  1100,     2,   702,   703,   681,     3,     3,     3,
       3,   725,     0,   156,     0,     3,     3,     0,   734,     0,
     728,     0,   844,     0,   483,     3,   448,   455,   844,   876,
     880,   844,   933,   937,   945,   844,   955,   483,   868,   871,
     873,   483,   925,   928,   930,   483,   948,   950,   952,   842,
       0,   463,  1001,     3,  1005,  1006,     3,   833,   941,   554,
     553,   556,   555,     2,   800,   834,   781,   483,     2,   832,
       0,   800,   835,   620,   620,   620,   483,   696,   700,   699,
     716,     0,   419,     0,     0,   483,     0,   340,     0,     0,
       0,     0,     0,   185,     0,   335,   336,     0,     0,   340,
       0,   388,   387,     0,   160,   160,   394,   581,   587,   202,
     483,     0,   186,     0,   213,   187,   188,   483,   207,   189,
     190,   191,   192,     0,   193,   194,   341,     0,   355,   195,
     361,   363,   366,   196,   197,   198,   199,   200,     0,   201,
     209,   564,   483,     0,   211,     0,     0,     0,     3,     0,
     813,   800,   788,   789,     0,     3,   784,     3,     3,     0,
     483,   763,   763,  1105,   667,   667,   667,  1071,  1076,     2,
     101,   483,     3,   579,     3,   484,  1032,   844,  1031,  1034,
     483,     3,  1020,  1026,   667,     0,   844,   844,     0,     0,
     652,     0,   671,     0,   667,  1105,     2,   848,   850,     0,
      96,   483,   483,     0,   100,    98,   483,     0,   112,     0,
       0,     0,   116,   120,   119,   184,     0,     0,     0,   731,
     109,   177,     0,     0,    46,    47,    85,     0,    85,    85,
       0,    73,    75,    49,     0,     0,    45,     0,    48,   155,
       0,     0,     0,     0,  1105,   997,   844,   996,   999,   991,
       0,     0,   885,   903,     0,   844,   967,   970,   844,     0,
     844,   844,   962,   979,     0,     0,  1101,     0,   704,   483,
     483,     0,     0,     0,     0,   433,     3,     0,     0,     0,
       0,   730,   735,     3,   826,   182,   181,     3,     0,   483,
     869,   872,   874,   483,   926,   929,   931,   483,   949,   951,
     953,     0,     0,     0,   725,   844,  1012,  1011,     0,     0,
       0,     0,     0,     3,   800,   836,     0,   483,   483,   483,
     483,   483,   483,   483,   603,   633,     3,     0,   634,   564,
     621,     0,     0,     0,   417,    67,     0,     0,   326,   327,
     210,   212,     0,     0,     0,   483,   483,   322,     0,   320,
       0,     0,     0,   731,     0,     0,     0,     0,   367,   483,
       0,   161,     0,     0,   395,     0,     0,     0,     0,     3,
     217,     0,   208,     0,   317,     0,     0,     0,   340,   340,
     346,   345,   340,   357,   356,   340,   340,     0,   564,   844,
       0,  1016,  1015,     0,     2,     0,   791,     2,   786,     0,
     787,     0,   767,   748,   752,   750,     0,  1105,     0,     0,
     483,     0,     0,     0,     3,   483,  1027,  1029,  1030,     0,
       0,   101,     0,     3,  1105,   667,   661,   667,   677,   677,
     731,   678,   653,  1105,     0,   780,   483,   853,  1017,     0,
       0,     0,    39,     0,   113,   115,   114,   111,   110,   731,
    1105,     0,    66,    82,     0,    76,    83,    84,    61,     0,
       0,     0,    70,    57,     0,   154,   424,    31,     0,   483,
     992,   994,   995,   886,   904,   844,   483,   963,   965,   966,
     483,   980,   982,     0,   957,   972,   968,   983,  1102,     3,
     689,   688,   692,  1104,     2,     2,  1103,     0,     3,   841,
     741,   742,     0,     0,   844,     0,     0,     0,   877,   934,
     942,   464,   843,     0,  1007,     0,  1008,  1009,  1003,   817,
       2,     0,   819,   603,   603,   603,   634,   641,   608,     0,
     647,   634,     0,   483,   595,   632,   628,     0,     0,     0,
       0,   635,   637,   844,   649,   649,   649,     0,   629,   645,
     420,     0,   330,   331,   328,   329,     0,     0,   340,   227,
       0,     0,   229,   428,   228,   564,   483,   308,   307,   309,
       0,   340,   185,   267,     0,   260,     0,   185,   323,   321,
       0,   315,  1105,   324,     0,     0,     0,     0,   376,   377,
     378,   379,     0,   369,     0,   370,   332,     0,   333,     0,
       0,   360,   483,   218,   206,   319,   318,     0,     0,   349,
     359,     0,   340,   362,     0,   364,   386,     0,   418,   844,
     483,   815,   768,   483,     2,     2,   659,     0,   667,   667,
    1079,  1080,  1081,     0,     0,     3,     3,     0,  1040,     0,
       0,  1105,   667,     0,   674,   673,  1105,     0,   656,     3,
       0,  1018,    99,     0,    32,   483,     0,  1105,     0,     0,
      86,     0,    74,     0,    80,     0,    78,    44,   159,     0,
       0,     0,     0,   483,   483,   744,     0,   441,   443,   878,
     935,   943,     0,     0,   783,   821,   599,   601,   597,     0,
       0,  1047,     0,   642,  1052,   644,  1044,   844,   844,   627,
     648,   631,     0,   630,     0,     0,     0,   651,     0,   623,
     844,   622,   638,   650,   639,   640,   646,   340,     0,     0,
     248,   340,   230,   564,   313,   311,   314,   310,     0,   312,
       0,   256,     0,   185,     0,   340,   483,   268,     0,   293,
       0,     0,   316,     0,     0,   340,   339,   340,   380,     0,
     371,     2,     0,     0,     0,     0,   342,     0,     0,   340,
       0,   340,   340,   340,   204,   203,   442,   785,     0,     0,
     660,  1105,  1105,  1082,  1033,     0,     0,  1039,  1041,   657,
       0,     0,   667,     0,   655,     2,    50,    42,    40,    41,
       0,    64,   178,    77,     0,     0,   998,   969,   984,   445,
       2,   686,     3,   685,   743,  1002,  1010,   625,     0,     0,
       0,  1048,  1049,   844,   626,  1045,  1046,   624,   604,     0,
       0,   338,     0,     0,     0,   241,   340,   219,     0,     0,
     340,   250,   265,   276,   270,   340,   185,   305,     0,   280,
       0,     0,   271,   269,   258,   261,     0,     0,   185,   294,
       0,     0,   222,   337,   368,     2,   483,   334,     0,     0,
     396,   340,   347,     0,    67,   358,   351,     0,   352,   350,
     365,   790,   792,     0,     0,  1042,  1043,   658,   667,  1105,
     679,   782,    65,    81,    79,   483,     0,   844,  1055,  1057,
    1050,     0,   636,   236,   231,   234,     0,   233,   240,   239,
       0,   483,   243,   242,   340,   252,     0,   249,   340,     0,
       0,     0,   257,   262,     0,     0,   185,   306,   281,     0,
       0,   340,     0,   296,   297,   295,   264,   325,     0,   483,
     483,     3,   381,   484,   385,     0,   389,     0,     0,     0,
     397,   398,   225,   343,     0,     0,     0,   663,   665,  1105,
       0,   690,   483,  1051,  1053,  1054,   643,     0,   238,     0,
     237,   221,   244,   483,   409,   253,   340,   254,   251,   266,
     279,   277,   273,   285,   283,   284,   282,   263,   278,   274,
     275,   272,   259,     0,     0,     0,     0,   224,   244,     3,
     374,     0,  1047,   382,   383,   384,   396,     0,     0,     0,
     396,     0,   340,   348,   344,   340,     0,     0,   664,     0,
     232,   235,   340,     3,   245,   410,   255,     0,     0,     0,
       0,   304,   302,   299,   303,   300,   301,   298,     3,   374,
       0,     0,  1048,     0,     0,     0,   390,     0,   399,   226,
     353,   340,   662,  1056,   214,     0,     0,   340,   292,   290,
     287,   291,   288,   289,   286,     0,     0,   375,     0,   402,
       0,   400,     0,   402,   354,   216,   215,   220,     0,   223,
       0,   372,   403,     0,     0,   391,     0,   373,     0,     0,
       0,     0,   404,   405,     0,   401,   392,     0,     0,   393,
     406
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1912,  6592,  5357, -1912,    -1,   122,  1926,     2, -1912,  -345,
   -1912,   350, -1912,  -729, -1912,   772,  -928,  -887, -1912,   199,
    2604,  1967, -1912,   280, -1912,  1389,   501,   815,   834,   593,
     809,  1350,  1352,  1357,  1360,  1362, -1912,   254,  -174,  8821,
     914, -1912,  1691, -1912, -1912,  -702,  4748, -1143,  1015, -1912,
      86, -1912,   906,   -19, -1912, -1912, -1912,   427,    73, -1912,
   -1832, -1911,   290,    51, -1912, -1912,   688,   305, -1599, -1912,
   -1272, -1912, -1912, -1912, -1912,   103, -1179, -1912, -1912, -1222,
     437, -1912, -1912, -1912, -1912, -1912,   120, -1195, -1912, -1912,
   -1912, -1912, -1912,    22,   457,   467,   124, -1912, -1912, -1912,
   -1912,  -803, -1912,    52,    -8, -1912,   135, -1912,   147, -1912,
   -1912, -1912,   918,  -885,  -849, -1373, -1912,     3,    84,  1889,
    4250,  -709,  -668, -1912,   -51, -1912, -1912,   139, -1912,  -140,
    8565,  -295,  -252,  2867,   570,  -662,    17,   174,   123,  1438,
    3270, -1912,  2119, -1912,   376,  4506, -1912,  2059, -1912,   251,
   -1912, -1912,  2134,   436,  5228,  4041,   -31,  1910,  -312, -1912,
   -1912, -1912, -1912, -1912,  -262,  8149,  6869, -1912,  -389,   155,
   -1912,  -608,   252, -1912,   186,   759, -1912,   -38,  -265, -1912,
   -1912, -1912,  -364,  8492,  -667,  1213,    87,  -610,  -703,  -231,
     190, -1912, -1335,  -145,   128,   923,   946,  7241,  -356,  -518,
    -251,  -201,  -457,  1351, -1912,  1695,   493,  1260,  1568, -1912,
   -1912, -1912, -1912,   361,  -175,   102,  -917, -1912,   153, -1912,
   -1912, -1128,   468, -1912, -1912, -1912,  2196,  -696,  -509, -1009,
     -36, -1912, -1912, -1912, -1912, -1912, -1912,   -35,  -814,  -185,
   -1824,  -205,  7524,   -74,  5032, -1912,  1225, -1912,  2040,   -55,
    -187,  -181,  -165,    53,   -70,   -59,   -56,   448,   -34,    26,
      45,  -149,    89,  -146,  -126,  -125,    91,  -117,  -112,  -111,
    -766,  -730,  -723,  -682,  -672,   -40,  -663, -1912, -1912,  -724,
    1415,  1429,  1431,  2171, -1912,   601,  6336, -1912,  -602,  -632,
    -599,  -566,  -751, -1912, -1626, -1723, -1684, -1659,  -629,  -107,
    -293, -1912, -1912,   -33,    21,   -79, -1912,  7181,  1454,  1578,
    -450
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1211,   224,   410,   411,    82,    83,   412,   386,   413,
    1541,  1542,   414,  1015,  1016,  1017,  1330,  1331,  1332,  1553,
     436,   416,   417,   418,   718,   719,   419,   420,   421,   422,
     423,   424,   425,   426,   427,   428,   429,   438,  1114,   720,
    1462,   781,   218,   783,   432,   848,  1212,  1213,  1214,  1215,
    1216,  1217,  1218,  2145,  1219,  1220,  1469,  1660,  1995,  1996,
    1925,  1926,  1927,  2113,  2114,  1221,  1674,  1675,  1676,  1829,
    1830,  1222,  1223,  1224,  1225,  1226,  1227,  1857,  1861,  1486,
    1478,  1228,  1229,  1485,  1479,  1230,  1231,  1232,  1233,  1234,
    1235,  1236,  1693,  2131,  1694,  1695,  2031,  1237,  1238,  1239,
    1465,  2039,  2040,  2041,  2173,  2184,  2062,  2063,   303,   304,
     914,   915,  1181,    85,    86,    87,    88,    89,  1663,    91,
      92,    93,    94,    95,    96,   232,   233,   306,   285,   471,
      98,   472,    99,   590,   101,   102,   155,   351,   309,   106,
     107,   170,   108,   931,   352,   156,   111,   256,   112,   157,
     264,   354,   355,   356,   158,   433,   117,   118,   358,   119,
     586,   907,   905,   906,  1634,   120,   121,   122,   123,  1176,
    1430,  1640,  1641,  1792,  1793,  1431,  1629,  1812,  1642,   124,
     678,  1742,   674,   125,   675,   676,  1292,  1107,   477,   478,
     943,   592,   479,   480,   593,   594,   595,  1243,   442,   443,
     219,   497,   498,   499,   500,   501,   339,  1261,   340,   929,
     927,   624,   341,   380,   342,   343,   444,   126,   176,   177,
     127,  1255,  1256,  1257,  1258,     2,  1163,  1164,   616,  1250,
     128,   330,   331,   266,   277,   569,   129,   222,   130,   321,
    1116,   897,   531,   168,   131,   689,   690,   691,   132,   323,
     236,   237,   238,   324,   134,   135,   136,   137,   138,   139,
     140,   241,   325,   243,   244,   245,   326,   247,   248,   249,
     816,   817,   818,   819,   820,   250,   822,   823,   824,   786,
     787,   788,   789,   532,  1156,  1409,   141,  1750,   649,   650,
     651,   652,   653,   654,  1795,  1796,  1797,  1798,   639,   482,
     366,   367,   368,   445,   210,   143,   144,   145,   370,   840,
     655
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      81,   192,   381,    81,   552,   194,  1482,   151,   431,  1259,
     969,   944,  1024,   679,   723,  1467,   195,   503,   103,   196,
     566,   362,   142,   180,  1004,   142,   349,   958,   529,  1240,
     534,  1466,   890,   892,  1590,  1591,   837,   542,   657,  1413,
     509,   197,   235,   513,   945,   952,   201,   724,  1083,   514,
    1454,   209,   894,   634,   133,    81,    81,   997,    81,   365,
     729,  1059,   313,   902,  1087,   515,   549,  1908,  1377,   665,
    1094,   667,  1662,   668,    81,   670,   103,   946,  1841,  2058,
     142,   516,  1077,    81,   517,    90,   305,    58,  2004,  1078,
    -793,    81,  1335,  2066,  2003,  1998,    81,   384,  1997,    81,
    1244,   198,  1378,    81,   518,   519,  1909,   271,   923,   605,
     607,   316,   133,   520,   220,    58,  1697,   209,   521,   522,
     199,  1342,   448,   464,   105,   142,   149,   377,  1108,  1108,
    1079,  1910,   513,   449,   146,  2037,   450,   207,   514,  1699,
      97,    81,  1084,    90,    81,  1406,    81,  1108,   220,  1080,
     239,  2112,    81,   267,   515,  1412,   511,   278,   451,    81,
     194,   657,  1416,   973,   103,  1912,    81,  1407,   142,   166,
     516,   195,   634,   517,   196,   104,   524,  2112,   174,   174,
     944,  -794,   105,   385,  1376,   221,   242,  -825,   246,   305,
      81,    81,  2005,   518,   519,  1698,   197,  2067,    97,   953,
     133,   530,   520,  2147,   537,    81,  1171,   521,   522,  1700,
     160,   545,   952,   945,   174,   494,  1108,   305,   452,   299,
      81,  -825,  1110,   262,    81,    81,   558,  1251,   305,    81,
      81,    90,   562,   104,  1941,  1997,   159,   453,  2059,  2060,
    1125,   640,  1495,   573,   207,   610,   946,  2057,  1299,   194,
      81,   235,   113,   599,  -976,  1999,   198,  1859,    81,   485,
     195,  -976,   581,   196,   174,   524,  1263,   174,    81,    81,
     105,   763,    81,  1476,  1063,   199,  1480,  1323,   657,    81,
    2003,  1378,   174,   882,   207,  1990,    97,  1922,  1923,   375,
    1338,   886,  1860,    81,    81,   558,    81,  1334,   841,  1481,
    1440,    81,  2105,  1711,  1295,    81,  1466,  1714,   506,  1248,
     113,   308,   207,   764,   970,  2003,  1362,  1198,    81,    81,
     525,   104,   526,   657,   854,  1087,   570,  1425,    81,  1662,
     855,   481,  1240,  1314,  1349,    81,    81,   937,   979,   939,
      81,   982,   983,   174,   984,  1077,   856,   657,   510,  1283,
    1022,   980,  1078,   986,   657,   275,   988,   989,   990,   601,
     640,   957,   857,   888,    20,   858,  1088,   528,  1908,   893,
    1091,  1441,  1924,   207,   963,   165,   964,   109,    81,  1104,
    1105,    81,   688,   921,   808,   859,   860,   538,   113,   598,
    1168,   174,   174,  1079,   861,   242,   568,   602,   113,   862,
     863,  1834,   174,  1244,   209,  1621,  2094,  1909,  1749,   525,
    1363,   526,  1354,   293,   308,   563,   584,   174,   671,   589,
     606,  1108,  1198,   854,   149,  1560,   574,  1480,  1288,   855,
     262,   821,  1910,   575,  1291,   109,   430,   114,  1590,  1591,
    1555,   448,   308,   944,   628,   856,    81,  -654,   587,   179,
    1481,   174,   449,   308,  -654,   450,    58,  1561,   174,   174,
     201,   857,   181,   174,   858,  1483,  1912,  1426,   874,    81,
      81,  1701,  1414,   262,   349,   625,   945,   451,   308,   626,
    1283,    81,    81,   628,   859,   860,  1712,   591,  1484,   722,
      81,   113,   494,   861,   215,   114,  1922,  1923,   862,   863,
     485,   174,    58,   884,   174,   216,  1990,   365,  1427,   946,
      81,   182,   161,   109,  1387,   162,   163,  1467,   164,   113,
     305,   217,    81,   109,    58,  1433,  1517,   225,   896,  1832,
     113,  1647,   208,  1466,  1840,   900,    58,   452,   297,   904,
     448,   606,   530,  1433,  1434,   240,  1636,    81,   268,   640,
    1648,   449,   279,    81,   450,   113,   453,   874,   730,   275,
    1661,  1677,  1718,   731,   190,   646,  1946,  1947,  1425,  1425,
    1425, -1106,   672,   114,  1677,    58,   485,   673,    58,   202,
     922,  1951,   672,   114,   539,    63,    64,   673,   530,   999,
     213,   936,   174,  1436,  1437,  1344,   262,  1507,    -3,  2011,
    1109,  1109,   753,   754,   174,   174,   613,  1582,  1435,    81,
     530,    81,   875,    58,   876,    81,   109,  1524,   799,  1109,
     225,  1800,   530,   883,   298,    81,  1761,  1533,   657,    81,
      81,   887,   187,   103,    77,  1564,   298,   142,  1118,   208,
    1801,  1652,   958,  1267,   109,  1268,   755,   756,   895,  1368,
    2051,  1098,   226,  1654,   486,   109,   190,  1064,   227,   903,
    1085,   530,    81,  -719,   644,   485,  2014,  2015,  1647,   133,
     261,   699,  1762,  1764,  1766,    81,   114,    58,   190,   208,
     109,   283,  1318,   290,   481,   292,  -600,  1803,  1109,  1319,
    1940,  1552,  1149,    58,   298,  1092,   743,   744,  1334,   644,
      90,   875,    58,   876,   114,    58,  1651,   208,  1426,  1426,
    1426,  1438,   999,   262,    58,   114,  1804,   743,   901,   765,
      58,   571,    58,   766,   261,   251,   568,   290,   292,   885,
      81,  1842,    81,    58,    81,   621,   581,  1810,    81,   105,
     114,    81,  1503,  1913,    58,   308,  2046,   743,   174,  1427,
    1427,  1427,   932,   935,  1685,    97,  1907,   999,    58,  1137,
     921,   481,  1914,   530,   622,   623,    81,  2020,  1048,    14,
      15,    16,    17,    18,   821,  1141,   261,    58,  1810,   530,
     722,   297,   568,   454,  1145,   961,   722,  1389,   530,   270,
     104,   530,    74,  2013,  1809,   722,  1393,  1917,   174,   999,
     530,  1810,  1397,   591,  1515,  2026,   530,   941,   644,  -477,
    1863,    81,   643,    81,   722,  1569,   644,  1661,  2009,   530,
    1811,   723,   113,    79,   645,    81,  1576,  1477,    58,   999,
     530,   912,    81,   298,   293,  1894,   646,  1895,   494,  2084,
    1580,    81,   999,   261,   644,   290,   292,   200,    64,  1759,
      81,   349,  2086,  1008,   724,  1010,    58,  1013,  1741,  2052,
    1375,  1021,  1550,   530,  1025,   297,  1170,   113,   999,  1824,
    1825,  1826,   680,  2077,   992,   682,    81,   261,  2118,   190,
    1150,   999,   261,   295,   365,   993,   994,  -977,   261,  1050,
     298,  1827,  -481,  1109,  -977,   486,   481,  1824,  1825,  1826,
    1833,  1508,  1509,    14,    15,    16,    17,    18,   746,   580,
      64,    81,    81,   494,   999,   747,   748,   791,  1382,  1827,
     261,   792,  1287,   317,  2120,   662,   161,   292,  1252,   162,
     163,   103,   164,  -814,    74,   142,  1751,   481,    14,    15,
      16,    17,    18,   379,  1404,   803,   142,   109,   174,   530,
    1543,  1677,   732,   337,   784,   174,   382,   733,   530,   481,
     481,    81,    58,   657,   706,    79,    80,  1835,   629,   293,
    1157,   486,  1836,  1126,  1054,   688,   957,   454,   481,   530,
    1847,    74,  1165,   941,    74,  1836,  1169,   842,   843,  1071,
    1172,   844,   109,  1072,  1959,  2099,  1682,    58,    90,  1960,
    2100,   643,   911,   383,  1790,   644,   912,   114,   530,  2162,
     455,    81,    79,   645,  2163,    79,    80,   261,  1789,    81,
     384,   252,   253,  1802,   254,  1026,  -482,   972,  1519,   255,
     456,   626,   457,  1628,  1180,   174,   174,   105,  1178,   749,
     750,  1361,   821,   261,  -478,   662,   292,   481,    81,   751,
     752,   494,   114,    97,    14,    15,    16,    17,    18,   458,
     706,  1871,  1872,   262,    19,   459,   297,   460,   454,  1496,
     530,   921,   974,  1746,    81,   489,   626,   502,   975,   430,
      81,    81,   976,   484,  1286,   488,   381,   381,  1242,   363,
    1113,   998,  1757,   261,   504,   999,   757,   758,    14,    15,
      16,    17,    18,    48,    49,    50,    51,    52,    53,    54,
      55,    81,   507,    58,   202,   726,  2064,  1531,   261,  1824,
    1825,  1826,   508,   261,   148,   261,   363,   527,    65,    66,
      67,    68,    69,    70,    71,    72,  1011,   528,  1589,   632,
     726,  1827,   550,  1432,  2064,   551,   261,  1068,   261,   261,
    1828,   530,   539,  1611,   867,  1122,   530,    58,   561,  1123,
     261,  1538,   349,  1743,   640,   113,   220,   613,   297,   454,
    1252,   530,   530,   261,   572,  1085,  1012,   454,  2115,   644,
     539,  1159,   261,  1161,   530,   999,   494,   999,   142,    81,
      81,    81,   613,  1333,   617,   365,   530,  1334,   494,  -479,
    1001,  1002,   103,  1598,  1599,   261,   142,   662,   292,    14,
      15,    16,    17,    18,  1502,  1979,   494,  1537,   792,    74,
    1754,  1334,    81,  1817,  1755,  1592,   596,  1334,  1818,   261,
     662,   600,   999,   632,   103,  2044,   261,    81,   142,   643,
      81,    81,  1844,   644,    81,   271,   999,  1027,  1028,  1029,
      79,   645,   481,    81,  1350,  1845,  1415,  1846,  1351,  1123,
    1918,   999,  1965,   142,   792,   664,   999,  1308,    58,    90,
    1439,   673,  1312,  2006,  2103,  1364,  2104,   999,  1334,   921,
     999,  2181,  1365,  1320,   677,  2178,   681,  2187,    81,  1460,
     109,  2188,   692,  2133,   267,   278,   722,  2137,   693,  1339,
     696,    90,    81,   734,   697,   735,   736,   737,   105,   174,
     745,  2049,   174,   174,   174,  1100,  1101,   683,   494,  1102,
    1103,   174,  1321,  1123,    97,   698,    81,  1401,  1417,  1418,
    1419,  1402,  1336,  1337,   738,  1403,   702,   739,   740,   174,
     105,   726,   741,   742,   761,   174,  1034,  1035,  1036,  1037,
     114,  1688,  1689,  1690,  1691,  1692,    97,   759,    81,  1242,
     589,  1786,  1787,  1788,   262,   999,  1340,   174,  -157,  -157,
     349,  1476,  1477,  1644,   760,  1113,  1102,  1494,  1881,  1813,
    1813,  1813,  1558,  1559,  1432,  1432,  1432,  1563,  1559,  1630,
    1432,  1242,   684,   762,  1664,  1567,  1559,   767,   568,  1543,
    1074,  1551,   793,   365,  -122,  -122,  -122,  -122,  -122,  -122,
     685,   174,   686,   687,    65,    66,    67,    68,    69,    70,
      71,    72,   794,   151,   795,  1539,   796,    81,  1600,  1551,
     797,    81,    81,  1824,  1825,  1826,   113,  1074,  1613,  1963,
    1964,   142,  1767,  1123,   798,   999,  1892,  1123,  1893,  1559,
     513,   461,   494,  1899,  1900,  1827,   514,    14,    15,    16,
      17,    18,   103,   103,  -186,   699,   142,   142,   113,  1905,
     999,    -3,   515,  1983,  1559,   825,   103,   494,   494,   363,
     142,  -121,  -121,  -121,  -121,  -121,  -121,    81,   516,  1984,
    1559,   517,  -480,   275,  1922,  1923,   -18,   481,   481,   838,
     188,   183,     6,     7,     8,     9,    10,    11,    12,    13,
     261,   518,   519,  2178,  2179,   839,    58,  1645,   849,  1646,
     520,   261,  1653,  1655,   852,   521,   522,  1556,  1557,   494,
     261,    14,    15,    16,    17,    18,   996,   363,   212,  1252,
     864,   570,  1902,   281,   865,   430,   866,   282,   494,   868,
     286,   743,   291,    81,   174,   174,   363,   142,    81,    81,
      81,   109,   280,  1805,  1030,  1031,  1038,  1039,   105,   105,
     869,  1644,   870,  1716,   871,  1592,  1644,   872,    74,  1605,
     349,   873,   105,  1606,   524,  1032,  1033,  1607,   878,  1544,
    1545,  1546,   880,   109,  1565,   310,  1547,  1548,   784,   909,
     174,   174,   530,   898,   212,  1713,  1715,  1814,  1815,    79,
      80,   568,   899,   365,   879,  1744,  1745,  -598,   854,  1666,
    1666,   114,  1504,  1505,   855,    81,  -596,   908,   261,  1592,
      81,   430,   430,  1666,   910,   913,    81,   916,    81,   924,
     856,   926,   930,   947,   446,   949,    81,   646,   966,   977,
     978,  1664,   261,   114,   971,  1000,   857,   494,  1003,   858,
    1006,  2032,    14,    15,    16,    17,    18,  1313,  1047,   271,
     494,  1052,  1073,  1074,    14,    15,    16,    17,    18,   859,
     860,  1081,  1120,  1128,  1972,  -797,  1129,   142,   861,   268,
     279,   363,  1130,   862,   863,  1131,   113,   113,  1132,  1133,
     281,  1134,  1854,  1135,  1136,  1734,  1151,  1158,  1160,  1162,
     113,  1166,  1173,   559,  1245,  1645,   494,  1646,  1174,   103,
    1645,   657,  1646,   142,  1175,  1264,  1252,  1260,   525,  1276,
     526,  1277,   363,    58,  1294,  2032,  1278,  1279,  1290,  1291,
    1297,  1296,  1300,  1301,   142,  1381,  1304,   281,  1994,  1303,
    1305,  1306,  1307,   874,   363,   363,  1309,  1310,  1311,  1769,
    1316,  1317,    81,  1341,    81,  1324,  1770,  1325,  1345,   174,
    1771,  1346,  1347,   363,  1348,  1352,  1353,  1355,  1848,  1356,
     174,  1357,   559,  1366,  1358,  -683,  -682,  1405,   262,  1442,
     282,  -798,   661,   174,   291,    74,  1410,  1445,  1464,  1446,
    1455,  1456,   642,  -718,    81,  1457,  1459,    81,  1468,  1489,
     999,   261,  1470,  1491,  1492,  1790,   494,  1532,  1498,   530,
     494,   109,   109,  1535,  1525,   105,    79,    80,  1549,  1551,
    1573,  1574,   707,  1179,   494,   109,  1500,  1566,  1575,   174,
    1592,  1586,   363,  1587,   494,   261,   494,  2093,  1588,  1593,
    1594,   261,  1595,   103,  1596,  1958,  1559,   142,   494,  1601,
     494,   494,   494,  1604,  1608,  1644,  1609,    81,    81,  1610,
    1618,  1619,  1622,  1635,  1633,  1435,  1666,  1657,  1678,  1477,
    1679,   114,   114,  2110,  1681,  1994,  1702,  1707,  1683,  1704,
    1198,   212,  1708,   152,  1696,   114,  1705,   875,  1719,   876,
    1721,   481,   481,  1706,   513,  1722,  1724,  1726,  1725,  1752,
     514,  1728,  1729,  1748,   363,  1730,  1731,   275,  1732,    81,
    1738,   642,  1760,  2034,  2135,   494,   515,  1753,   707,   494,
    1756,  1768,   454,  1775,   494,  1784,   571,  1777,  1600,   174,
    1819,  1779,   516,   174,  1780,   517,   568,  1781,  1785,  1799,
    1638,  1870,  1821,   113,   221,  1851,  1853,   174,  1930,   105,
     494,  1874,  2038,  1873,  1886,   518,   519,   174,    84,   174,
    1877,   150,  1878,  1879,   520,  1884,  1882,  1891,  1904,   521,
     522,   174,  1896,   174,   174,   174,  1897,  1856,  1898,   260,
     174,  1935,  1950,   272,  1936,  1948,  1955,  2034,  1967,   261,
    2180,  1961,  1957,   494,   446,   446,   281,   494,  1962,  1645,
    1666,  1646,  1975,  1976,  1977,  1980,  1981,   610,   103,  1978,
     494,   194,   142,  1982,  -684,  1987,    84,  1988,  1989,  2008,
    2010,    81,   195,    81,   530,   196,  -581,   524,  2016,  2021,
    2047,  2035,   191,  2019,  2027,  2036,   103,   363,   174,  2048,
     142,    84,   174,  1900,  2061,  2070,  2083,   174,   261,  2087,
    2085,  2097,  2098,  2096,   231,   494,  2102,   259,  2108,  2101,
    2130,    84,  2121,  2117,  2119,  2141,  2136,   874,   109,  2134,
     103,  2142,  2148,   174,   142,  2143,  2158,   113,  2167,  2161,
      81,    81,   469,  2159,  2169,  2170,  2174,  2175,  2038,  2185,
    2186,   494,  2038,  2038,   494,  1888,  1562,   995,   150,  1040,
     959,   494,  1041,   481,    84,   193,  2189,   150,  1042,  1463,
     320,   328,  1043,  1472,   105,  1044,   174,   782,  2168,  1855,
     174,    81,  2111,   348,  1952,   446,  2160,   234,   114,  2128,
     494,  1945,   495,   174,   494,   207,   494,  1687,  1862,  2106,
    1849,  2156,   105,  2138,  2089,  2176,  2095,   437,   191,   191,
    1850,  2172,   567,  1490,  2088,  2172,    58,   494,   171,   150,
     467,   288,  1992,   259,   560,  1666,   430,  2056,    81,  1289,
    2182,   525,  1632,   526,  1487,   485,   105,    81,   174,  1262,
    1119,  1867,   320,   322,   928,   845,     3,   231,   231,  1055,
     148,  1298,  1444,  1666,    65,    66,    67,    68,    69,    70,
      71,    72,   109,  1056,  1458,  1057,  1783,     0,   320,     0,
       0,   875,     0,   876,   174,   363,    84,   174,    74,     0,
       0,     0,     0,     0,   174,   261,     0,  1666,     0,     0,
     259,     0,  2109,  2043,  2171,     0,   446,     0,    75,    76,
       0,     0,   113,   967,     0,     0,  2177,     0,     0,    79,
      80,     0,     0,   174,     0,   322,     0,   174,     0,   174,
     512,   234,   114,   320,     0,     0,     0,     0,     0,   328,
     113,     0,     0,     0,    58,   328,   320,   320,     0,     0,
     174,   322,   363,   363,     0,   150,     0,     0,   148,     0,
       0,  2183,    65,    66,    67,    68,    69,    70,    71,    72,
    2190,   430,     0,   430,   113,   348,   647,   656,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
       0,   348,     0,     0,     0,   348,     0,   475,     0,     0,
       0,     0,     0,     0,     0,   251,   322,    76,     0,     0,
     834,     0,   430,     0,     0,     0,     0,     0,     0,   611,
     322,     0,     0,     0,     0,     0,  1359,    76,     0,     0,
     437,     0,     0,     0,     0,     0,     0,   109,   261,     0,
     446,     0,     0,     0,  2157,     0,     0,     0,     0,     0,
       0,     0,   148,   261,   200,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   437,   109,     0,   785,     0,     0,
       0,     0,   541,     0,   191,  1117,     0,   835,     0,   495,
       0,     0,     0,   332,   333,   334,   335,     0,   430,     0,
     150,     0,     0,   363,   467,     0,     0,   114,   814,   109,
     656,    76,     0,     0,   834,   469,     0,     0,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,   148,   114,   172,   173,    65,    66,
      67,    68,    69,    70,    71,    72,    74,     0,   231,     0,
       0,   261,   637,     0,     0,   660,     0,     0,     0,     0,
     231,     0,     0,  1709,  1710,     0,  1637,    76,   637,   114,
       0,     0,   637,  1638,     0,  1473,     0,    79,    80,     0,
       0,   815,     0,     0,     0,   320,   336,   437,   437,     0,
     148,   320,   469,   348,    65,    66,    67,    68,    69,    70,
      71,    72,     0,   148,   337,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,  1293,     0,     0,
     148,   853,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,   148,   234,   172,   173,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,   320,     0,   320,
       0,   348,     0,    84,     0,     0,     0,     0,   322,     0,
       0,     0,     0,     0,   322,     0,     0,     0,     0,   348,
     467,   637,   656,   261,     0,     0,  1474,     0,     0,  1451,
     647,   790,     0,     0,   647,     0,     0,     0,     0,     0,
       0,     0,     0,   348,     0,     0,     0,   801,     0,     0,
     804,     0,     0,   656,     0,  1447,   348,     0,     0,     0,
       0,     0,     0,   363,     0,     0,     0,     0,   150,     0,
     920,     0,   322,     0,     0,     0,     0,   469,     0,     0,
       0,   437,     0,     0,   150,   150,     0,   437,     0,     0,
       0,     0,     0,  1820,     0,     0,   437,     0,     0,   150,
     150,   150,     0,     0,     0,     0,  1831,   541,     0,     0,
     475,     0,     0,     0,     0,     0,   363,   363,   469,     0,
       0,   148,   261,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   446,     0,     0,     0,     0,     0,
     469,   469,     0,     0,     0,   959,     0,    19,     0,     0,
       0,     0,  1865,     0,     0,   467,   148,     0,     0,   469,
      65,    66,    67,    68,    69,    70,    71,    72,   475,     0,
       0,   785,   785,     0,     0,     0,     0,     0,     0,   437,
       0,     0,     0,     0,     0,   495,   637,   475,   835,     0,
      52,    53,    54,    55,  1449,     0,   467,     0,     0,   814,
       0,   814,     0,     0,  1359,    76,   415,     0,     0,     0,
     637,     0,     0,     0,     0,     0,     0,     0,   348,   348,
       0,     0,     0,   637,     0,     0,     0,     0,   469,     0,
       0,     0,     0,     0,  1253,   148,     0,   348,     0,    65,
      66,    67,    68,    69,    70,    71,    72,  1019,     0,     0,
       0,     0,  1921,     0,     0,     0,  1931,     0,     0,     0,
       0,  1506,     0,     0,   320,     0,     0,     0,     0,     0,
    1944,     0,  1076,     0,   815,     0,     0,     0,     0,     0,
    1953,     0,  1954,     0,     0,     0,     0,  1020,   100,     0,
       0,   154,     0,  1534,  1966,     0,  1968,  1969,  1970,     0,
       0,   437,     0,     0,     0,     0,   348,     0,     0,     0,
       0,    58,   150,   437,     0,     0,     0,     0,     0,     0,
       0,     0,   475,     0,     0,   348,     0,  1271,   363,     0,
       0,     0,     0,     0,     0,     0,     0,   322,   647,     0,
       0,     0,  1568,     0,     0,   148,   100,   228,   229,    65,
      66,    67,    68,    69,    70,    71,    72,     0,  1329,     0,
       0,  2002,     0,   475,     0,  2007,  1329,     0,     0,     0,
    2012,   206,     0,    74,     0,   790,   790,     0,   467,     0,
       0,     0,     0,     0,     0,  1066,     0,     0,  1069,     0,
       0,   273,     0,  2091,    76,  1329,  2042,   530,   495,     0,
       0,     0,     0,     0,    79,    80,     0,     0,     0,     0,
       0,   695,     0,     0,     0,   415,   701,     0,     0,     0,
       0,     0,     0,     0,   307,   710,   711,     0,   312,     0,
       0,     0,     0,   469,   100,     0,     0,   318,     0,  2065,
     415,   415,     0,  2068,     0,   785,     0,     0,   541,     0,
       0,     0,     0,   350,     0,  1139,  2082,     0,  1329,  1143,
       0,   415,   814,  1147,     0,     0,     0,     0,     0,   814,
       0,     0,     0,     0,     0,     0,  1253,   318,   447,   183,
       6,     7,     8,     9,    10,    11,    12,    13,     0,   312,
     473,   415,   637,     0,     0,   660,     0,     0,     0,     0,
       0,  2116,     0,     0,     0,  1727,     0,   446,     0,     0,
       0,   348,     0,     0,     0,     0,     0,     0,   523,     0,
       0,     0,  1740,     0,     0,     0,     0,   307,     0,     0,
       0,  1747,     0,     0,     0,  1076,     0,  2139,   548,     0,
    2140,  1360,   815,   553,   555,   475,   206,  2144,  1758,     0,
     260,   272,     0,     0,   150,   307,     0,     0,     0,     0,
       0,     0,     0,   150,     0,     0,   307,     0,     0,   576,
       0,     0,   437,   578,     0,     0,  2164,     0,   579,     0,
    2166,     0,  2144,     0,     0,     0,     0,     0,     0,   555,
       0,   307,     0,     0,     0,   603,     0,   437,     0,     0,
       0,     0,     0,  2166,   437,     0,     0,   612,     0,     0,
       0,     0,     0,     0,   148,   318,   228,   229,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   259,    84,
       0,     0,     0,     0,     0,   635,     0,     0,   659,     0,
       0,   320,     0,     0,     0,     0,     0,   150,     0,   790,
       0,   666,     0,     0,     0,   666,     0,     0,   467,     0,
       0,     0,     0,     0,     0,   495,    58,     0,     0,     0,
     933,     0,     0,  1329,     0,     0,     0,   934,   469,   469,
    1843,     0,     0,     0,     0,     0,     0,     0,     0,   467,
     318,     0,     0,   150,     0,     0,     0,     0,     0,     0,
     148,     0,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,   322,     0,     0,     0,     0,  1391,
       0,     0,  1395,     0,   318,     0,  1399,     0,    74,   152,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1880,
       0,     0,     0,     0,  1883,     0,     0,     0,   230,    76,
     312,     0,   189,     0,   635,  1890,   348,   348,     0,    79,
      80,     0,     0,     0,     0,     0,     0,     0,     0,   415,
     415,   415,   415,   415,   415,   415,   415,   415,   415,   415,
     415,   415,   415,   415,   415,   415,   415,   415,     0,     0,
     263,     0,     0,     0,     0,     0,     0,   567,     0,   495,
       0,   284,   287,     0,   150,   150,   150,   150,     0,   150,
     150,     0,     0,     0,     0,  1639,   328,     0,     0,  1583,
       0,     0,     0,     0,     0,   637,     0,     0,     0,     0,
       0,     0,   437,   437,     0,     0,     0,   318,   318,     0,
       0,     0,     0,   473,   263,  1253,   437,     0,     0,   415,
       0,     0,     0,     0,     0,     0,   475,   148,   307,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,  1973,
    1974,     0,     0,   148,     0,   259,   495,    65,    66,    67,
      68,    69,    70,    71,    72,  1326,     0,     0,  1643,  1327,
       0,  1328,     0,     0,     0,   495,   263,   467,     0,     0,
    1329,   350,     0,   100,     0,  1329,  1329,  1329,     0,     0,
       0,     0,     0,   619,     0,     0,     0,     0,     0,   666,
     940,     0,    76,   150,     0,   647,     0,     0,     0,     0,
       0,     0,     0,     0,   951,     0,     0,  1571,     0,     0,
       0,     0,     0,   635,     0,     0,  1578,     0,   960,     0,
       0,     0,     0,     0,     0,     0,   666,     0,     0,     0,
       0,     0,     0,   263,     0,     0,     0,     0,   318,     0,
       0,     0,     0,     0,   260,   272,     0,  2050,     0,     0,
       0,   318,     0,     0,   318,   318,     0,   318,     0,     0,
       0,     0,     0,     0,     0,     0,   318,   263,     0,   318,
     318,   318,   263,     0,     0,     0,     0,     0,   263,     0,
       0,   415,     0,  1639,  1791,     0,   415,     0,  1639,     0,
     437,     0,     0,     0,  1639,     0,  1639,   415,     0,     0,
       0,     0,  1253,     0,     0,     0,     0,     0,     0,     0,
     263,     0,     0,     0,     0,     0,     0,  2107,     0,     0,
       0,     0,   328,   150,     0,   473,     0,     0,     0,     0,
       0,     0,     0,     0,   475,     0,     0,     0,     0,   415,
       0,     0,  1058,     0,     0,     0,     0,     0,     0,   318,
       0,     0,   469,   469,   708,     0,  1643,     0,     0,   437,
       0,  1643,     0,     0,     0,     0,   940,  1806,     0,  1643,
       0,  1082,     0,     0,     0,     0,     0,   348,     0,  1329,
     150,  1329,     0,     0,     0,     0,     0,     0,   473,   473,
     148,     0,   371,   372,    65,    66,    67,    68,    69,    70,
      71,    72,   567,     0,     0,     0,     0,   473,     0,     0,
       0,     0,   150,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,     0,     0,     0,     0,
     348,   348,     0,   263,     0,     0,     0,     0,     0,     0,
       0,    77,     0,     0,     0,     0,   373,  1791,  1791,     0,
     708,     0,     0,   374,     0,     0,     0,     0,     0,     0,
       0,     0,  1639,     0,     0,  1639,     0,     0,     0,     0,
       0,  1241,     0,     0,     0,     0,   473,     0,    58,   415,
     328,     0,   154,   318,     0,     0,     0,     0,  1794,     0,
       0,     0,     0,   437,     0,   666,     0,     0,  1275,     0,
       0,     0,     0,     0,     0,  1281,     0,     0,     0,     0,
       0,     0,   148,     0,     0,   263,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,   320,     0,     0,     0,
       0,     0,     0,     0,     0,  1919,   263,     0,  1643,     0,
      74,     0,     0,     0,     0,     0,     0,     0,   350,     0,
     263,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,    76,     0,   263,   469,   415,     0,     0,  1791,     0,
       0,    79,    80,     0,     0,     0,     0,  1639,     0,     0,
       0,     0,     0,     0,     0,   415,     0,     0,     0,     0,
    1993,     0,     0,     0,     0,   263,     0,     0,     0,   322,
       0,     0,     0,   415,   415,   415,     0,     0,     0,     0,
     415,   415,     0,   150,     0,     0,     0,     0,     0,   263,
       0,     0,     0,     0,     0,     0,   263,     0,     0,     0,
       0,     0,     0,     0,   415,     0,     0,     0,   387,     0,
       0,   388,   348,   389,     0,   390,     0,     0,  1791,     0,
    1643,  1794,  1794,     0,     0,     0,     0,     0,   150,     0,
       0,     0,   391,     0,     0,     0,     0,     0,     0,     0,
       0,   415,   415,     0,     0,     0,     0,     0,     0,     0,
       0,   473,     0,     0,     0,     0,   150,   150,     0,  2092,
     328,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,     0,   401,   402,     0,     0,     0,
     150,     0,     0,    74,   154,     0,     0,     0,     0,     0,
       0,     0,   116,  1429,     0,   116,     0,     0,     0,     0,
       0,     0,  1241,   403,     0,     0,    77,   404,  2092,  2092,
       0,     0,     0,   405,    79,    80,   406,   407,   408,   409,
       0,     0,   322,     0,     0,     0,     0,   318,     0,     0,
       0,     0,  1794,     0,  1241,     0,     0,     0,     0,     0,
     637,     0,     0,     0,     0,     0,     0,     0,     0,  2092,
     116,     0,     0,     0,     0,     0,     0,     0,     0,  1488,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,   116,     0,   318,     0,     0,
       0,   611,   322,     0,     0,     0,     0,     0,   635,     0,
       0,   265,     0,     0,     0,   116,     0,   553,     0,     0,
       0,  1154,     0,    14,    15,    16,    17,    18,  2054,     0,
       0,     0,  1794,     0,   637,     0,     0,     0,     0,   350,
       0,     0,   322,   318,     0,     0,     0,    58,   116,     0,
       0,     0,   116,     0,     0,     0,     0,     0,   116,     0,
       0,   116,     0,     0,     0,   265,     0,     0,     0,     0,
       0,     0,     0,  1794,     0,     0,   344,   116,   376,     0,
     263,   148,    58,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,   441,     0,     0,     0,     0,   473,   473,     0,    74,
       0,     0,     0,   116,   441,     0,   148,   265,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   230,
      76,     0,  1794,  1794,     0,     0,     0,     0,     0,     0,
      79,    80,     0,     0,    74,     0,     0,     0,     0,     0,
       0,   116,     0,     0,  1429,  1429,  1429,   154,   555,   318,
     318,     0,     0,     0,   319,    76,     0,     0,   116,     0,
     116,     0,     0,  1794,     0,    79,    80,     0,     0,   116,
       0,   415,  1665,  1665,   265,     0,     0,     0,     0,     0,
     116,     0,     0,     0,     0,     0,  1665,    14,    15,    16,
      17,    18,     0,     0,   204,   585,     0,     0,   116,     0,
       0,     0,     0,   116,     0,   116,     0,     0,   265,   116,
       0,     0,     0,   265,     0,     0,     0,     0,     0,   265,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   116,
       0,     0,     0,     0,     0,     0,     0,   350,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,   116,
       0,   265,   116,     0,     0,    14,    15,    16,    17,    18,
     204,     0,     0,   154,     0,   116,     0,     0,     0,   116,
       0,     0,     0,     0,     0,     0,   204,     0,     0,     0,
     148,     0,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
     204,     0,     0,     0,   441,     0,     0,     0,    74,     0,
       0,     0,     0,   470,    58,     0,   148,     0,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,   812,    76,
       0,     0,   644,     0,     0,     0,     0,     0,   441,    79,
     813,     0,     0,     0,     0,     0,     0,     0,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
     318,     0,     0,     0,   116,     0,  1808,   110,   441,   204,
       0,   263,  1284,     0,   265,     0,    74,     0,     0,  1285,
       0,     0,     0,     0,     0,     0,   415,     0,     0,     0,
       0,     0,     0,  1823,     0,     0,  2091,    76,     0,     0,
     530,     0,     0,     0,     0,   263,     0,    79,    80,   393,
       0,   394,   395,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,   110,     0,   415,     0,  1665,
       0,     0,     0,     0,     0,     0,     0,     0,   204,     0,
     116,     0,     0,     0,     0,     0,     0,   350,     0,     0,
     154,   441,   441,     0,     0,     0,   265,   116,   204,   728,
       0,     0,    77,   404,     0,     0,     0,     0,     0,     0,
     274,     0,   116,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   318,     0,     0,     0,     0,     0,     0,     0,
       0,   265,     0,     0,     0,     0,     0,     0,     0,     0,
     473,   473,     0,   110,   265,     0,     0,     0,     0,     0,
       0,     0,     0,   110,   116,   116,     0,   116,  1911,     0,
       0,   415,     0,   415,     0,     0,    14,    15,    16,    17,
      18,   376,   353,   116,   441,     0,   265,     0,     0,     0,
    1616,     0,     0,     0,   116,     0,     0,   204,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   116,     0,   263,
     265,     0,   415,  1665,   585,     0,     0,   265,     0,   474,
     116,     0,   965,     0,     0,     0,     0,   204,     0,     0,
       0,     0,   116,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,   415,   441,     0,     0,   116,   116,
       0,   441,     0,     0,     0,     0,   110,     0,     0,     0,
     441,     0,     0,   116,   116,   116,     0,     0,   263,   148,
       0,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,   110,     0,     0,     0,   415,     0,
       0,     0,     0,     0,     0,   110,   148,    74,   577,     0,
      65,    66,    67,    68,    69,    70,    71,    72,  1326,     0,
     204,   204,  1327,   353,  1328,     0,   470,  1637,    76,   441,
     110,     0,     0,     0,   274,     0,     0,     0,    79,    80,
       0,     0,     0,  2033,     0,   116,     0,     0,     0,     0,
       0,     0,     0,   441,     0,    76,     0,     0,  1554,     0,
     116,     0,     0,     0,   116,     0,     0,     0,     0,     0,
     441,     0,   473,   116,   636,     0,     0,   274,     0,     0,
       0,     0,     0,     0,   204,     0,     0,     0,  1665,     0,
     636,     0,   116,   116,   636,     0,     0,     0,     0,   387,
       0,     0,   388,   470,   389,     0,   390,     0,     0,     0,
       0,   116,     0,     0,     0,     0,  1665,  2033,     0,     0,
       0,     0,     0,   391,     0,     0,   204,     0,     0,   148,
       0,     0,     0,    65,    66,    67,    68,    69,    70,    71,
      72,  1326,  1155,     0,     0,  1327,     0,  1328,     0,     0,
    1665,   204,     0,   392,   393,   263,   490,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,   116,   398,   399,   400,   441,   401,   402,    76,  2132,
     116,  1763,     0,     0,    74,     0,   116,   441,     0,     0,
       0,    58,     0,   636,     0,     0,     0,     0,     0,   116,
       0,  1273,   441,     0,   403,    76,     0,   491,   492,     0,
       0,     0,   493,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,   148,     0,   228,   229,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   470,   148,
       0,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   441,    74,     0,     0,     0,     0,     0,     0,
       0,     0,   204,     0,     0,   353,     0,    74,     0,     0,
       0,     0,     0,   319,    76,     0,     0,     0,     0,   470,
       0,     0,   474,     0,    79,    80,     0,   812,    76,     0,
       0,   644,     0,     0,     0,     0,     0,   110,    79,   813,
     169,   470,   470,     0,     0,     0,     0,     0,     0,     0,
       0,   646,     0,   263,     0,     0,     0,     0,     0,     0,
     470,     0,     0,     0,     0,   116,     0,   169,     0,   116,
       0,     0,     0,     0,     0,     0,   116,     0,     0,   353,
     474,     0,   110,     0,     0,     0,   116,     0,     0,     0,
       0,     0,     0,   116,     0,     0,     0,     0,   636,   474,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   353,
       0,   721,     0,   169,    58,     0,     0,     0,     0,     0,
       0,     0,   636,     0,     0,   116,   169,     0,   169,   470,
       0,     0,     0,     0,     0,   636,   204,     0,   116,     0,
       0,     0,   116,     0,     0,     0,   116,     0,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
     378,     0,     0,     0,     0,     0,     0,     0,   116,     0,
       0,     0,     0,     0,     0,     0,    74,   116,     0,     0,
       0,     0,     0,     0,   378,     0,   441,     0,     0,   115,
       0,     0,     0,     0,     0,     0,  1637,    76,     0,     0,
       0,   204,     0,     0,     0,     0,     0,    79,    80,     0,
       0,   441,     0,     0,     0,     0,     0,     0,   441,     0,
       0,     0,   169,     0,     0,     0,   169,     0,     0,   169,
     169,     0,     0,   169,   474,     0,   169,   169,     0,   169,
       0,   169,   265,   116,     0,     0,     0,   115,     0,     0,
     353,     0,     0,     0,     0,     0,     0,     0,   889,   891,
       0,   116,     0,   263,     0,   353,     0,     0,     0,   353,
       0,     0,   441,     0,     0,   474,  1273,   148,   353,   580,
      64,    65,    66,    67,    68,    69,    70,    71,    72,  1528,
       0,     0,   276,     0,     0,     0,     0,   353,   353,     0,
       0,     0,   116,   441,     0,     0,     0,   116,     0,     0,
       0,   169,     0,     0,   169,     0,   353,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,  1049,
       0,     0,     0,     0,   470,   115,     0,   169,   169,     0,
     148,     0,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,   169,     0,   357,   148,     0,   582,   583,    65,
      66,    67,    68,    69,    70,    71,    72,     0,    74,     0,
     116,   116,     0,     0,     0,     0,   353,     0,     0,     0,
     110,     0,     0,     0,     0,   353,     0,     0,  1637,    76,
     116,   476,     0,     0,   116,  1638,     0,     0,   116,    79,
      80,     0,   721,     0,   636,   214,    77,   274,   721,     0,
       0,  1617,     0,     0,     0,     0,     0,   721,   116,   116,
     116,   116,   116,   116,   116,     0,     0,     0,   115,     0,
     265,     0,     0,     0,     0,     0,   721,     0,     0,     0,
       0,     0,     0,     0,     0,   296,   441,   441,   169,     0,
       0,     0,     0,     0,     0,     0,   115,   474,     0,     0,
     441,     0,     0,     0,     0,     0,     0,   115,     0,     0,
     204,     0,  1046,     0,     0,     0,     0,     0,     0,     0,
       0,   204,     0,     0,     0,   357,     0,     0,     0,   265,
       0,     0,   115,     0,     0,     0,   276,     0,     0,     0,
       0,     0,     0,   378,     0,     0,     0,     0,     0,     0,
       0,   441,   204,     0,     0,     0,   116,     0,   148,   169,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     353,     0,     0,     0,   353,     0,   638,   116,     0,   276,
       0,   353,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   353,   638,     0,     0,     0,   638,   148,   353,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,    77,
     116,     0,     0,     0,     0,     0,     0,   116,     0,   470,
     470,   116,     0,     0,     0,    74,     0,     0,     0,   148,
     353,   172,   173,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   378,   353,     0,  2091,    76,   353,     0,   530,
       0,   353,     0,     0,     0,     0,    79,    80,     0,     0,
     148,     0,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,     0,   608,   441,     0,     0,   484,     0,     0,
       0,     0,     0,   169,   169,     0,     0,     0,    74,     0,
       0,   110,     0,     0,     0,   638,   169,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   265,   116,   230,    76,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    79,
      80,     0,   148,   110,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   441,     0,     0,     0,     0,   274,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     204,   116,     0,     0,   116,     0,     0,   357,     0,     0,
     319,    76,     0,     0,     0,     0,     0,   636,     0,     0,
       0,    79,    80,   148,   476,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   116,     0,     0,   115,
       0,     0,     0,     0,     0,     0,     0,   353,   474,     0,
       0,     0,     0,     0,   116,   116,     0,     0,     0,     0,
     169,   169,     0,   810,     0,   811,   169,     0,     0,     0,
       0,   488,     0,     0,   827,   828,     0,     0,     0,     0,
       0,   357,   476,     0,   115,     0,     0,   169,     0,     0,
     169,   169,     0,   169,     0,   169,   169,     0,     0,     0,
     638,   476,     0,     0,   265,     0,     0,     0,     0,     0,
       0,   357,     0,     0,     0,   353,   353,   441,     0,     0,
       0,     0,     0,   204,   638,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   353,   169,   638,     0,   353,
     169,     0,     0,   353,   169,     0,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   110,   110,    46,     0,    47,     0,     0,   721,     0,
       0,     0,     0,     0,     0,   110,     0,   169,   169,     0,
     204,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,   169,     0,   148,     0,     0,     0,    65,    66,    67,
      68,    69,    70,    71,    72,  1326,   476,   116,     0,  1327,
       0,  1328,     0,     0,     0,     0,     0,     0,   703,     0,
     704,   705,   357,     0,     0,     0,   474,     0,     0,     0,
       0,   353,     0,   470,   470,     0,   116,   357,     0,     0,
       0,   357,    76,     0,     0,  1765,     0,   476,     0,     0,
     357,     0,   116,   769,   770,   771,   772,   773,   774,   775,
     776,   777,   778,   779,     0,     0,     0,   215,     0,   357,
     357,     0,     0,   -17,     0,     0,     0,     0,     0,     0,
     116,   116,     0,     0,   265,   353,     0,     0,   357,     0,
       0,     0,   353,     0,   780,     0,   353,     0,     0,     0,
       0,     0,   148,   116,   172,   173,    65,    66,    67,    68,
      69,    70,    71,    72,   116,     0,     0,     0,     0,     0,
       0,     0,     0,   169,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   357,     0,
       0,     0,   115,     0,     0,     0,     0,   357,     0,     0,
       0,     0,     0,     0,   169,     0,     0,     0,     0,     0,
     169,     0,     0,   169,     0,     0,   638,   169,     0,   276,
       0,     0,   274,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1097,     0,     0,     0,     0,     0,     0,     0,
    1656,     0,     0,  1659,  1673,     0,     0,     0,     0,  1680,
       0,     0,     0,  1684,     0,  1686,     0,  1673,   110,     0,
       0,     0,     0,     0,     0,     0,     0,   257,     0,   476,
       0,     0,     0,     0,     0,     0,   353,    14,    15,    16,
      17,    18,     0,     0,    20,   470,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -484,  -484,     0,  -484,    46,     0,    47,     0,  -484,
       0,     0,     0,     0,  1246,  1247,     0,     0,  1249,   353,
     353,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,   357,     0,     0,     0,   357,     0,     0,     0,
       0,     0,     0,   357,     0,     0,     0,     0,     0,   169,
       0,     0,     0,   357,     0,     0,     0,     0,   169,   169,
     357,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,     0,     0,    74,     0,
       0,     0,   357,     0,     0,     0,     0,     0,     0,     0,
       0,  1782,  1322,     0,     0,   357,     0,     0,     0,   357,
       0,    77,   327,   357,     0,     0,     0,     0,   169,    79,
      80,     0,     0,     0,     0,     0,     0,   169,     0,     0,
     169,     0,   169,   169,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1343,  1822,
       0,     0,     0,   115,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1837,  1839,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   169,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,     0,
    1659,     0,     0,     0,     0,  1367,  1858,     0,     0,     0,
       0,     0,   636,     0,  1371,  1372,  1373,  1374,     0,     0,
     276,     0,  1379,  1380,     0,     0,     0,     0,     0,     0,
       0,     0,  1388,     0,     0,     0,     0,     0,     0,     0,
       0,   353,     0,     0,     0,     0,     0,     0,     0,   638,
       0,     0,   364,     0,     0,     0,     0,   110,     0,     0,
    1408,     0,     0,  1411,     0,     0,     0,     0,     0,     0,
       0,   169,     0,     0,     0,     0,     0,     0,     0,   357,
     476,     0,     0,     0,     0,   110,   636,     0,   463,   364,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   353,     0,
       0,     0,     0,     0,     0,     0,     0,  1929,     0,   110,
     533,     0,     0,     0,  1471,     0,  1932,   533,  1934,     0,
       0,  1939,  1943,     0,  1673,     0,     0,     0,     0,  1949,
       0,     0,     1,     0,     0,   147,     0,   357,   357,     0,
       0,     0,     0,     0,     0,  1493,     0,   169,     0,     0,
       0,     0,  1497,     0,  1499,  1501,     0,   357,     0,     0,
       0,   357,     0,     0,     0,   357,     0,  1511,     0,  1512,
       0,  1513,     0,     0,     0,     0,   169,     0,  1522,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   533,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   169,
     203,     0,     0,   115,   115,   169,     0,     0,     0,     0,
       0,     0,     0,     0,   364,   648,     0,   115,     0,  2018,
       0,     0,     0,     0,  2023,  2025,     0,     0,     0,     0,
       0,     0,     0,     0,   669,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2045,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1597,     0,     0,     0,     0,   476,     0,
    1602,     0,     0,   357,  1603,     0,     0,     0,     0,     0,
       0,   169,     0,     0,     0,     0,     0,  2069,     0,  2072,
       0,     0,  2074,  2076,     0,     0,     0,  2079,  2081,     0,
    1620,     0,     0,     0,     0,     0,   533,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   533,   802,     0,   533,   805,   357,     0,     0,
       0,     0,     0,   364,   357,     0,     0,   648,   357,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1703,     0,     0,   169,
     169,     0,  2123,  2125,  2127,     0,     0,   378,     0,     0,
       0,     0,   169,     0,     0,     0,     0,     0,   533,     0,
     556,     0,   533,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2150,  2152,  2154,     0,
       0,  1733,     0,     0,     0,     0,     0,     0,  1737,     0,
    1739,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     597,     0,   364,     0,   276,     0,     0,     0,     0,     0,
       0,     0,   604,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   614,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,     0,     0,     0,     0,     0,     0,     0,     0,   633,
       0,     0,     0,     0,     0,   169,  1772,   533,   357,     0,
     364,     0,     0,     0,     0,  1776,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   938,   364,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   648,
       0,   289,     0,   648,     0,     0,     0,     0,     0,     0,
     956,     0,   364,     0,     0,     0,   727,     0,     0,     0,
       0,   357,   357,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   169,
       0,     0,     0,     0,     0,     0,     0,   768,     0,     0,
       0,     0,     0,     0,     0,   360,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   806,     0,     0,     0,   809,     0,     0,
       0,     0,     0,     0,   115,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,   831,     0,     0,     0,
     832,   833,     0,     0,   836,     0,     0,     0,     0,     0,
       0,     0,  1875,  1876,     0,     0,     0,     0,     0,   850,
     851,     0,     0,     0,   364,     0,  1885,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     533,   533,   881,   169,     0,     0,     0,     0,     0,     0,
     533,  1067,     0,   533,  1070,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,   648,     0,
     648,   648,     0,     0,     0,     0,     0,   648,     0,     0,
       0,     0,     0,     0,     0,     0,   360,   364,   364,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   638,     0,   364,     0,     0,   533,
       0,     0,     0,   533,     0,     0,     0,     0,   919,     0,
     533,  1140,     0,     0,   533,  1144,     0,     0,   533,  1148,
       0,   925,     0,   357,     0,     0,  1152,   360,     0,   360,
     360,     0,     0,     0,     0,     0,     0,     0,     0,   115,
       0,     0,     0,   360,     0,     0,   948,   360,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   533,   115,   638,  1986,
       0,     0,     0,     0,     0,   211,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     357,   269,     0,     0,     0,     0,     0,   648,     0,     0,
       0,   115,     0,     0,     0,     0,     0,   175,   178,     0,
       0,     0,     0,     0,     0,     0,     0,   991,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,   211,     0,   223,     0,   329,   360,     0,     0,     0,
       0,     0,   360,     0,     0,     0,     0,   369,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   211,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   314,   483,     0,   315,   487,  2090,     0,
       0,     0,     0,     0,   533,     0,     0,     0,     0,     0,
       0,   338,     0,     0,     0,     0,     0,     0,   360,     0,
       0,   648,   648,     0,     0,     0,     0,     0,   648,     0,
       0,     0,     0,     0,     0,   360,  1112,     0,     0,     0,
       0,     0,     0,     0,     0,  1121,     0,     0,     0,     0,
     211,  1124,     0,     0,     0,     0,  2129,     0,     0,     0,
       0,     0,     0,     0,   269,     0,     0,     0,     0,   360,
     364,     0,   505,     0,   533,  1392,     0,   533,  1396,     0,
    2146,   533,  1400,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,   360,     0,  2155,     1,     0,     0,     0,
    1167,     0,     0,     0,     1,     0,     0,     0,     0,   487,
       0,   360,   360,     0,   360,     0,     0,     0,     0,   211,
     564,   565,   360,     0,     0,     1,     0,     0,     0,     0,
       0,   175,     0,     0,     0,   360,     0,     0,   360,   641,
       0,   658,     0,     0,     0,   360,   175,     0,   360,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     615,     0,   167,  1302,     0,     0,     0,   618,   620,     0,
       0,     0,   627,     0,   725,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   648,  1518,     0,     0,     0,   211,     0,
     338,     0,     0,   338,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   360,   364,     0,
       0,     0,     0,     0,     0,   294,     0,     0,   641,     0,
       0,     0,     0,   360,   826,     0,     0,     0,   300,     0,
     301,     0,     0,     0,     0,     0,     0,     0,   360,     0,
       0,     0,   360,     0,     0,     0,     0,     0,   360,     0,
       0,   360,   533,  1572,     0,     0,     0,     0,     0,     0,
       0,   533,  1579,  1369,   648,  1370,     0,     0,     0,     0,
     360,   360,     0,     0,     0,   364,   364,     0,     0,     0,
       0,   223,     0,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,   829,   830,     0,     0,     0,     0,     0,
       0,   211,   211,     0,     0,     0,     0,   483,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   147,     0,     0,     0,     0,
       1,   535,   536,     0,     0,   540,     0,     0,   543,   544,
       0,   546,     0,   547,     0,     0,     0,     0,     0,   360,
       0,     0,     0,     0,     0,     0,     0,     0,   360,     0,
       0,     0,     0,     0,     0,   369,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   360,     0,   360,
     360,     0,     0,     0,   483,     0,   942,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   641,     0,     0,
       0,     0,     0,     0,     0,     0,   364,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     360,  1510,   211,     0,     0,     0,     0,   338,     0,   630,
     631,     0,     0,     0,   648,   725,     0,     0,   725,   725,
       0,   725,     0,     0,   663,     0,     0,     0,  1536,     0,
     725,     0,     0,   725,   725,   725,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   968,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   360,     0,     0,     0,   360,     0,     0,
       0,     0,     0,     0,   360,     0,     0,     0,     0,   483,
       0,     0,     0,     0,   360,     0,     0,     0,     0,     0,
       0,   360,     0,   533,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   211,     0,     0,     0,     0,     0,   533,
     800,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     483,     0,     0,   360,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
     360,     0,   483,   483,   360,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   483,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   877,     0,     0,     0,     0,   364,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1099,     0,     0,
       0,     0,     0,     0,  1111,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1720,     0,     0,  1723,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     483,     0,     0,     0,     0,     0,     0,   211,     0,   364,
     364,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   826,     0,     0,     0,     0,   533,   533,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,   533,     0,   360,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1182,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     360,   360,   369,  1280,     0,   954,   955,     0,     0,     0,
       0,    14,    15,    16,    17,    18,  1773,  1774,   962,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1723,     0,     0,     0,     0,   387,     0,     0,
     388,     0,   389,     0,   390,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   360,   360,
      58,   391,     0,     0,     0,     0,     0,   533,     0,     0,
       0,     0,     0,     0,     0,   533,     0,     0,   360,     0,
       0,     0,   360,     0,     0,     0,   360,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,     0,   401,   402,     0,     0,     0,     0,
       0,     0,    74,     0,     0,   483,     0,     0,     0,     0,
       0,     0,  1060,  1061,     0,   359,  1868,  1869,  1065,     0,
       0,   364,   403,   533,  2055,    77,   404,   533,     0,     0,
       0,     0,   405,   466,    80,   406,   407,   408,   409,  1086,
       0,     0,  1089,  1090,     0,  1093,     0,  1095,  1096,     0,
       0,     0,   359,     0,     0,     0,     0,   725,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   533,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,     0,   360,     0,     0,     0,  1138,     0,
       0,   725,  1142,     0,     0,     0,  1146,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   269,     0,     0,     0,     0,   533,   533,     0,
       0,  1448,  1450,  1452,     0,     0,     0,     0,   360,     0,
       0,   211,     0,  1956,     0,   360,   359,     0,     0,   360,
       0,     0,   641,     0,     0,     0,     0,     0,     0,  1265,
    1266,     0,     0,     0,  1475,     0,     0,     0,   533,     0,
       0,     0,     0,  1282,     0,     0,     0,  1723,     0,     0,
       0,     0,     0,   369,     0,     0,  1182,   725,     0,     0,
       0,     0,  1985,     0,     0,     0,     0,   359,     0,   359,
     359,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   359,     0,     0,  2001,   359,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1529,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2029,     0,     0,     0,  2030,     0,     0,
     483,   483,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   153,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,     0,     0,     0,     0,     0,   725,   725,
     725,     0,     0,   725,   725,  1282,     0,     0,     0,     0,
     487,     0,     0,     0,     0,     0,   359,     0,     0,     0,
       0,     0,   359,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,   360,     0,     0,  1384,     0,     0,   205,
       0,     0,  1390,     0,     0,  1394,     0,     0,   361,  1398,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   269,
       0,     0,     0,  1649,  1650,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   359,     0,
       0,   369,     0,     0,     0,   361,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   359,   311,     0,     0,     0,
       0,     0,     0,     0,     0,   205,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   359,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   440,     0,     0,     0,     0,
       0,     0,   359,   359,     0,     0,     0,   462,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   359,   359,     0,   359,     0,     0,     0,     0,   361,
       0,     0,   359,     0,     0,     0,     0,     0,     0,     0,
       0,  1516,     0,     0,     0,   359,     0,     0,   359,     0,
    1526,  1527,     0,     0,   211,   359,     0,     0,   359,     0,
       0,     0,   554,     0,   557,   360,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     361,     0,   361,   361,     0,     0,   269,     0,     0,     0,
       0,     0,     0,     0,   360,     0,   361,     0,     0,     0,
     361,     0,     0,     0,     0,     0,     0,   153,     0,     0,
    1570,     0,     0,     0,     0,     0,     0,     0,     0,  1577,
       0,     0,  1581,     0,  1584,  1585,     0,     0,  1816,     0,
       0,     0,     0,   557,     0,     0,     0,     0,     0,   360,
       0,   369,   360,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   359,     0,     0,
       0,   360,     0,     0,     0,     0,     0,     0,     0,  1612,
       0,     0,     0,   359,     0,     0,   725,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   359,     0,
       0,     0,   359,     0,   483,   483,     0,     0,   359,   361,
       0,   359,     0,     0,     0,   361,     0,     0,   440,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     359,   359,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   359,
       0,     0,   205,     0,   269,     0,     0,     0,     0,     0,
       0,   439,     0,  1717,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   468,     0,     0,     0,   807,     0,
       0,   361,     0,     0,     0,     0,     0,   496,     0,   496,
       0,     0,     0,     0,     0,     0,     0,     0,   361,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   359,
       0,     0,     0,     0,     0,     0,     0,     0,   359,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   361,     0,     0,     0,     0,   359,     0,   359,
     359,     0,     0,     0,     0,     0,     0,     0,     0,  1581,
       0,     0,     0,     0,     0,   361,   361,     0,     0,  1971,
       0,     0,     0,     0,     0,   440,   440,     0,     0,     0,
       0,     0,     0,     0,   361,   361,     0,   361,  1778,     0,
       0,     0,     0,     0,     0,   361,     0,   725,   609,     0,
     359,     0,     0,     0,     0,     0,     0,     0,   361,     0,
       0,   361,     0,     0,     0,     0,     0,     0,   361,     0,
       0,   361,     0,     0,     0,     0,   483,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,
       0,   725,     0,   359,   487,     0,     0,   359,     0,     0,
       0,     0,     0,     0,   359,     0,    58,     0,     0,     0,
       0,     0,     0,     0,   359,     0,     0,     0,     0,     0,
       0,   359,     0,  1866,     0,     0,   440,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   440,
     361,     0,   440,   440,     0,   440,     0,     0,     0,     0,
       0,     0,     0,   359,   440,     0,   361,   440,   440,   440,
       0,     0,     0,     0,     0,     0,   359,     0,     0,     0,
     359,   361,     0,     0,   359,   361,     0,     0,     0,     0,
       0,   361,     0,     0,   361,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   496,     0,     0,     0,     0,     0,
     496,  1915,  1916,   361,   361,   847,     0,     0,     0,     0,
       0,     0,     0,     0,  1920,     0,     0,     0,     0,  1420,
       0,  1421,   361,     0,     0,     0,  1422,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,   440,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,   361,     0,     0,     0,     0,     0,    58,  1423,
       0,   361,     0,     0,     0,     0,     0,     0,     0,     0,
     359,     0,     0,     0,   359,   918,     0,     0,     0,     0,
     361,     0,   361,   361,     0,     0,     0,  1991,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
     359,   359,     0,     0,   468,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   950,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   361,     0,     0,     0,     0,     0,  1424,
    1254,   440,     0,    77,   981,     0,     0,     0,     0,     0,
       0,    79,    80,     0,     0,     0,     0,     0,     0,     0,
       0,  2053,     0,     0,     0,     0,     0,     0,   359,   359,
       0,     0,     0,   985,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   359,     0,
       0,     0,   359,     0,     0,     0,   359,   847,  1005,     0,
       0,  1007,     0,  1009,     0,     0,   361,     0,     0,  1018,
     361,  1023,  1018,     0,     0,     0,     0,   361,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   361,     0,     0,
       0,     0,     0,     0,   361,     0,     0,     0,     0,  1051,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1053,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1062,     0,     0,   361,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   468,     0,   361,
    1051,     0,     0,   361,     0,     0,     0,   361,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1115,     0,   359,
     496,     0,     0,     0,   359,    14,    15,    16,    17,    18,
       0,     0,    20,  1127,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,  1153,    46,     0,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   359,     0,
       0,     0,     0,     0,    58,   359,     0,     0,     0,   359,
       0,     0,  1254,     0,  1520,     0,     0,     0,     0,     0,
       0,  1428,    14,    15,    16,    17,    18,   439,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1272,  1274,   361,     0,     0,     0,   361,     0,   468,
       0,     0,     0,     0,     0,   440,     0,     0,   387,     0,
       0,   388,     0,   389,     0,   390,     0,     0,     0,     0,
       0,     0,     0,   361,   361,     0,     0,     0,  1018,     0,
       0,    58,   391,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1051,     0,     0,     0,     0,     0,     0,     0,
    1315,     0,     0,     0,     0,   440,     0,  1018,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,     0,   401,   402,     0,     0,     0,
       0,   361,   361,    74,     0,     0,     0,     0,     0,   359,
       0,   440,     0,   496,     0,     0,     0,     0,     0,     0,
       0,   361,     0,   403,     0,   361,    77,   404,     0,   361,
       0,     0,     0,   405,  1521,    80,   406,   407,   408,   409,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   359,   359,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     496,     0,  1383,     0,  1386,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1428,  1428,  1428,   153,  1626,  1627,  1631,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   361,     0,     0,     0,     0,   361,     0,     0,
     257,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1461,  1461,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -484,  -484,     0,  -484,    46,     0,
      47,   361,  -484,     0,     0,     0,     0,     0,   361,     0,
       0,     0,   361,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1514,     0,     0,     0,
       0,  1254,  1523,     0,     0,   359,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,   468,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   359,     0,     0,     0,     0,     0,
     496,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1018,     0,     0,   847,     0,
       0,     0,     0,     0,    77,   258,     0,     0,     0,     0,
       0,     0,    79,    80,     0,     0,     0,     0,     0,   359,
       0,     0,   359,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   440,     0,
       0,   359,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   361,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1614,  1615,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1018,     0,     0,     0,
       0,     0,     0,     0,     0,   361,   361,     0,     0,     0,
       0,     0,     0,     0,   496,     0,     0,   847,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1254,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2165,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     440,  1443,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1005,     0,     0,     0,     0,     0,     0,     0,     0,
    1735,  1736,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   496,   387,     0,     0,   388,     0,   389,     0,   390,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     496,     0,   847,     0,  1184,     0,   391,    -2,     0,  1186,
    -246,  -246,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,
    1195,  1196,  1197,  1198,  -340,  -340,  1199,  1200,  1201,  1202,
    1203,  1204,  1205,     0,  1206,     0,   392,   393,     0,   490,
     395,  1207,  1208,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,  1209,   398,   399,   400,     0,   401,
     402,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   361,     0,
       0,     0,     0,     0,   439,     0,  -246,  1210,     0,  1807,
      77,   404,     0,     0,     0,   298,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,   361,     0,     0,
       0,     0,  -185,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2165,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1852,     0,
    1443,     0,   361,     0,     0,   361,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   361,     0,     0,     0,     0,     0,
       0,   387,     0,     0,   388,     0,   389,     0,   390,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1184,  1887,   391,    -2,  1889,  1186,  -247,
    -247,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,
    1196,  1197,  1198,  -340,  -340,  1199,  1200,  1201,  1202,  1203,
    1204,  1205,     0,  1206,  1906,   392,   393,     0,   490,   395,
    1207,  1208,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,  1209,   398,   399,   400,  1864,   401,   402,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,  1443,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -247,  1210,     0,     0,    77,
     404,     0,     0,     0,   298,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,   387,     0,     0,   388,     0,
     389,  -185,   390,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1184,     0,   391,
      -2,     0,  1186,     0,     0,  1187,  1188,  1189,  1190,  1191,
    1192,  1193,  1194,  1195,  1196,  1197,  1198,  -340,  -340,  1199,
    1200,  1201,  1202,  1203,  1204,  1205,     0,  1206,     0,   392,
     393,     0,   490,   395,  1207,  1208,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,  1209,   398,   399,
     400,     0,   401,   402,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1210,     0,     0,    77,   404,  1018,     0,     0,   298,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,     0,     0,  -185,     4,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,  1183,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   387,     0,    46,   388,    47,   389,     0,   390,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,  1184,    58,  1185,    -2,     0,  1186,
       0,     0,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,
    1195,  1196,  1197,  1198,  -340,  -340,  1199,  1200,  1201,  1202,
    1203,  1204,  1205,     0,  1206,     0,   392,   393,    61,   490,
     395,  1207,  1208,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,  1209,   398,   399,   400,     0,   401,
     402,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -3,  1210,     0,     0,
      77,   435,     0,     0,     0,   298,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,  -185,     4,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,  1183,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   387,
       0,    46,   388,    47,   389,     0,   390,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,  1184,    58,  1185,    -2,     0,  1186,     0,     0,  1187,
    1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,
    1198,  -340,  -340,  1199,  1200,  1201,  1202,  1203,  1204,  1205,
       0,  1206,     0,   392,   393,    61,   490,   395,  1207,  1208,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,  1209,   398,   399,   400,     0,   401,   402,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1210,     0,     0,    77,   435,     0,
       0,     0,   298,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,     0,     0,  -185,
       4,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   387,     0,    46,   388,
      47,   389,     0,   390,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
     391,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     392,   393,    61,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,     0,   401,   402,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1667,  1668,  1669,     0,     0,
       0,   403,  1670,  1671,    77,   435,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,     0,     0,
       0,     0,     0,     0,     0,     0,  1672,     4,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   387,     0,    46,   388,    47,   389,     0,
     390,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,   393,    61,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,     0,
     401,   402,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1667,  1668,  1669,     0,     0,     0,   403,  1670,
       0,    77,   435,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,     0,     0,  1672,   183,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,     0,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,   252,   253,     0,
     254,    46,     0,    47,     0,   255,     0,     0,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       4,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   387,     0,    46,   388,
      47,   389,     0,   390,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
     391,     0,     0,     0,     0,     0,  -459,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -459,
     392,   393,    61,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,     0,   401,   402,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   403,     0,  1658,    77,   435,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,     4,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   387,     0,    46,   388,    47,   389,
       0,   390,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,   391,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   392,   393,
      61,   394,   395,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   396,   397,   384,     0,   398,   399,   400,
       0,   401,   402,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   403,
       0,     0,    77,   435,     0,     0,     0,     0,     0,   405,
      79,    80,   406,   407,   408,   409,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   387,     0,    46,   388,    47,   389,     0,   390,   345,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   391,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,     0,   401,   402,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   403,     0,     0,    77,
     465,     0,     0,     0,     0,     0,   405,   466,    80,   406,
     407,   408,   409,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   387,     0,
      46,   388,    47,   389,     0,   390,   345,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   391,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,     0,   401,   402,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,     0,    77,  1269,     0,     0,
       0,     0,     0,   405,  1270,    80,   406,   407,   408,   409,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   387,     0,    46,   388,    47,
     389,     0,   390,   345,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   391,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,     0,   401,   402,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     403,     0,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   387,     0,    46,   388,    47,   389,     0,   390,
     345,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   391,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,     0,   401,
     402,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   403,     0,     0,
      77,   465,     0,     0,     0,     0,     0,   405,    79,    80,
     406,   407,   408,   409,  2000,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,    -2,     0,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
       0,     0,    -2,     0,     0,    -2,     0,     0,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,    -2,    -2,
    2028,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
      -2,     0,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,
       0,    -2,     0,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,    59,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    60,     0,     0,     0,    61,    62,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,    76,     0,
      77,    78,     0,     0,     0,     0,     0,     0,    79,    80,
     257,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -484,  -484,     0,  -484,    46,     0,
      47,     0,  -484,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   148,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,    76,     0,    77,   258,     0,     0,     0,  -816,
       0,     0,    79,    80,   257,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -484,  -484,
       0,  -484,    46,     0,    47,     0,  -484,     0,     0,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    58,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,   148,    47,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,    76,     0,    77,   258,
       0,   184,     0,   185,   186,     0,    79,    80,     4,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,     0,     0,
       0,     0,  -407,  -407,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -407,     0,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
      79,    80,     4,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,     0,     0,     0,     0,  -408,  -408,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -408,     0,     0,     0,    77,    78,     0,  1420,
       0,  1421,     0,     0,    79,    80,  1422,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1423,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1623,
       0,     0,     0,    77,   981,     0,  1420,     0,  1421,     0,
       0,    79,    80,  1422,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1423,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1624,     0,     0,     0,
      77,   981,     0,  1420,     0,  1421,     0,     0,    79,    80,
    1422,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,  1423,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1625,     0,     0,     0,    77,   981,     0,
       0,     0,     0,     0,     0,    79,    80,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     345,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   148,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,   588,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1075,    76,  -693,
      77,   644,     0,     0,     0,     0,     0,     0,    79,    80,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -484,  -484,     0,  -484,    46,     0,    47,
       0,  -484,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   148,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,    76,     0,    77,   258,     0,     0,     0,  -820,     0,
       0,    79,    80,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -484,  -484,     0,  -484,
      46,     0,    47,     0,  -484,     0,     0,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    58,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,   148,    47,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,    76,     0,    77,   258,     0,   703,
       0,   704,   705,     0,    79,    80,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   345,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,   588,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   643,     0,  -693,    77,
     644,     0,     0,    63,    64,     0,     0,    79,    80,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    77,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   345,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,   345,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,   588,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   784,
       0,  -693,    77,   530,     0,     0,    63,    64,     0,     0,
      79,    80,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    77,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   345,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,  1106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -680,    77,   347,     0,     0,     0,
       0,     0,     0,    79,    80,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   345,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   346,    77,   347,
       0,     0,     0,     0,     0,     0,    79,    80,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   345,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
    1901,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   347,     0,     0,     0,     0,     0,     0,    79,
      80,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   345,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,  1903,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,   347,     0,     0,     0,     0,
       0,     0,    79,    80,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   345,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   327,     0,
       0,     0,     0,     0,     0,    79,    80,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     345,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,   347,     0,     0,     0,     0,     0,     0,    79,    80,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -484,  -484,     0,  -484,    46,     0,    47,
       0,  -484,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -484,  -484,     0,  -484,    46,     0,
      47,     0,  -484,     0,    63,    64,     0,     0,     0,     0,
    1443,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,     0,     0,   388,     0,   389,     0,   390,     0,
       0,     0,     0,    77,   258,     0,     0,     0,     0,     0,
       0,    79,    80,  1184,     0,   391,    -2,     0,  1186,  1922,
    1923,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,
    1196,  1197,  1198,     0,     0,  1199,  1200,  1201,  1202,  1203,
    1204,  1205,     0,  1206,     0,   392,   393,     0,   490,   395,
    1207,  1208,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,  1209,   398,   399,   400,  1443,   401,   402,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1210,     0,   387,    77,
     404,   388,     0,   389,   298,   390,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,     0,
    1184,  -185,   391,    -2,     0,  1186,     0,     0,  1187,  1188,
    1189,  1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,  1198,
       0,     0,  1199,  1200,  1201,  1202,  1203,  1204,  1205,     0,
    1206,     0,   392,   393,     0,   490,   395,  1207,  1208,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
    1209,   398,   399,   400,     0,   401,   402,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1210,     0,     0,    77,   404,     0,     0,
       0,   298,     0,   405,    79,    80,   406,   407,   408,   409,
       0,     0,     0,     0,     0,     0,     0,     0,  -185,   302,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -411,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,   302,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,    77,    46,     0,    47,     0,  -411,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -412,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,   302,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
      77,    46,     0,    47,     0,  -412,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,    14,    15,    16,    17,    18,    19,
     712,    20,   713,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    63,    64,
     387,     0,    46,   388,    47,   389,     0,   390,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   391,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   714,     0,     0,     0,
       0,  1198,     0,  -340,     0,     0,     0,    77,     0,     0,
       0,     0,  -411,     0,   392,   393,     0,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,     0,   401,   402,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1210,     0,     0,    77,   715,
       0,     0,     0,   298,     0,   405,    79,    80,   716,   717,
     408,   409,    14,    15,    16,    17,    18,    19,   712,    20,
     713,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   387,     0,
      46,   388,    47,   389,     0,   390,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   391,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   714,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,     0,   401,   402,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,     0,    77,   715,     0,     0,
       0,   298,     0,   405,    79,    80,   716,   717,   408,   409,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   387,     0,    46,   388,
      47,   389,     0,   390,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     391,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,     0,   401,   402,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   403,     0,   434,    77,   435,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   387,     0,    46,   388,    47,   389,
       0,   390,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   391,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   392,   393,
       0,   394,   395,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   396,   397,   384,     0,   398,   399,   400,
       0,   401,   402,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   403,
       0,     0,    77,   715,     0,     0,     0,   298,     0,   405,
      79,    80,   406,   407,   408,   409,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   387,     0,    46,   388,    47,   389,     0,   390,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   391,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,     0,   401,
     402,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   403,     0,     0,
      77,   435,     0,     0,     0,     0,     0,   405,    79,    80,
     406,   407,   408,   409,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     387,     0,    46,   388,    47,   389,     0,   390,   345,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   391,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   392,   393,     0,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,     0,   401,   402,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   403,     0,     0,    77,   465,
       0,     0,     0,     0,     0,   405,    79,    80,   406,   407,
     408,   409,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   387,     0,
      46,   388,    47,   389,     0,   390,   345,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   391,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,     0,   401,   402,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,     0,    77,   404,     0,     0,
       0,     0,     0,   405,    79,    80,   406,   407,   408,   409,
     257,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -484,  -484,     0,  -484,    46,     0,
      47,     0,  -484,     0,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,   345,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,     0,   148,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   588,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -693,    77,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   148,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
      76,     0,    77,    78,     0,     0,     0,  -818,     0,     0,
      79,    80,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   148,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,    76,     0,    77,    78,     0,     0,
       0,     0,     0,     0,    79,    80,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   148,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,    79,    80,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   345,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   588,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,  -693,    77,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -484,  -484,     0,  -484,    46,     0,    47,     0,  -484,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,    76,
       0,    77,   327,     0,     0,     0,     0,     0,     0,    79,
      80,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   345,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1177,     0,     0,     0,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,    77,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     345,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   987,    77,   981,     0,     0,     0,     0,     0,
       0,    79,    80,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,  1540,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   981,     0,
       0,     0,     0,     0,     0,    79,    80,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   310,     0,     0,    63,    64,     0,     0,    79,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,    79,    80,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,   345,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,   461,
       0,     0,    63,    64,     0,     0,    79,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   347,     0,     0,     0,     0,     0,     0,    79,
      80,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   345,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,   345,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,   310,     0,     0,    63,
      64,     0,     0,    79,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,   461,
       0,     0,     0,     0,     0,     0,    79,    80,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -484,  -484,     0,  -484,    46,     0,    47,     0,  -484,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,   345,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,   327,     0,     0,     0,     0,
       0,     0,    79,    80,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,   345,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,   981,
       0,     0,    63,    64,     0,     0,    79,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   981,     0,     0,     0,     0,     0,     0,    79,
      80,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   345,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,     0,     0,    14,    15,
      16,    17,    18,    79,    80,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -484,  -484,     0,  -484,    46,     0,    47,     0,
    -484,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,    20,    58,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -484,  -484,     0,  -484,    46,     0,    47,
       0,  -484,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,   327,    63,    64,     0,     0,     0,     0,
      79,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,     0,     0,     0,     0,     0,     0,
       0,    79,    80,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   387,     0,    46,   388,    47,   389,     0,   390,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,     0,   401,
     402,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   403,     0,     0,
      77,   404,     0,     0,     0,     0,     0,   405,   466,    80,
     406,   407,   408,   409,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   387,     0,    46,   388,    47,   389,     0,
     390,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,     0,
     401,   402,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   403,     0,
       0,    77,   404,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   148,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -484,  -484,     0,  -484,    46,    77,
      47,     0,  -484,     0,     0,     0,   387,     0,     0,   388,
       0,   389,     0,   390,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,     0,   401,   402,   387,     0,     0,   388,     0,
     389,    74,   390,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,  1667,  1668,  1669,     0,   391,
       0,   403,  1838,     0,    77,   404,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,  1937,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,     0,   401,   402,   387,     0,     0,   388,     0,   389,
      74,   390,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1667,  1668,  1669,     0,   391,     0,
     403,  1938,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   392,   393,
       0,   394,   395,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   396,   397,   384,     0,   398,   399,   400,
     387,   401,   402,   388,     0,   389,     0,   390,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,     0,     0,   403,
    1318,     0,    77,   404,     0,     0,     0,  1319,     0,   405,
      79,    80,   406,   407,   408,   409,     0,     0,     0,     0,
       0,     0,     0,     0,   392,   393,     0,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,   387,   401,   402,   388,
       0,   389,     0,   390,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,     0,     0,   403,     0,     0,    77,   404,
       0,     0,     0,   493,     0,   405,    79,    80,   406,   407,
     408,   409,     0,     0,     0,     0,     0,     0,     0,     0,
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,   387,   401,   402,   388,     0,   389,     0,   390,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,     0,     0,
       0,   403,   846,     0,    77,   404,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,   387,   401,
     402,   388,     0,   389,     0,   390,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391,     0,     0,     0,     0,   403,     0,     0,
      77,   404,     0,     0,     0,   298,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,   387,   401,   402,   388,     0,   389,
       0,   390,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   391,     0,
       0,     0,     0,   403,  1014,     0,    77,   404,     0,     0,
       0,     0,     0,   405,    79,    80,   406,   407,   408,   409,
       0,     0,     0,     0,     0,     0,     0,     0,   392,   393,
       0,   394,   395,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   396,   397,   384,     0,   398,   399,   400,
     387,   401,   402,   388,     0,   389,     0,   390,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,     0,     0,   403,
       0,     0,    77,   404,     0,     0,  1045,     0,     0,   405,
      79,    80,   406,   407,   408,   409,     0,     0,     0,     0,
       0,     0,     0,     0,   392,   393,     0,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,   387,   401,   402,   388,
       0,   389,     0,   390,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,     0,     0,   403,  1385,     0,    77,   404,
       0,     0,     0,     0,     0,   405,    79,    80,   406,   407,
     408,   409,     0,     0,     0,     0,     0,     0,     0,     0,
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,   387,   401,   402,   388,     0,   389,     0,   390,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,     0,     0,
       0,   403,     0,     0,    77,   404,     0,     0,     0,  1453,
       0,   405,    79,    80,   406,   407,   408,   409,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,   387,   401,
     402,   388,     0,   389,     0,   390,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391,     0,     0,     0,     0,   403,     0,     0,
      77,   404,     0,     0,     0,  1530,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,   387,   401,   402,   388,     0,   389,
       0,   390,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   391,     0,
       0,     0,     0,   403,     0,  1928,    77,   404,     0,     0,
       0,     0,     0,   405,    79,    80,   406,   407,   408,   409,
       0,     0,     0,     0,     0,     0,     0,     0,   392,   393,
       0,   394,   395,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   396,   397,   384,     0,   398,   399,   400,
     387,   401,   402,   388,     0,   389,     0,   390,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,     0,     0,   403,
    1933,     0,    77,   404,     0,     0,     0,     0,     0,   405,
      79,    80,   406,   407,   408,   409,     0,     0,     0,     0,
       0,     0,     0,     0,   392,   393,     0,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,   387,   401,   402,   388,
       0,   389,     0,   390,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,     0,     0,   403,  1942,     0,    77,   404,
       0,     0,     0,     0,     0,   405,    79,    80,   406,   407,
     408,   409,     0,     0,     0,     0,     0,     0,     0,     0,
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,   387,   401,   402,   388,     0,   389,     0,   390,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,     0,     0,
       0,   403,  2022,     0,    77,   404,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,   387,   401,
     402,   388,     0,   389,     0,   390,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391,     0,     0,     0,     0,   403,  2024,     0,
      77,   404,     0,     0,     0,     0,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,   387,   401,   402,   388,     0,   389,
       0,   390,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   391,     0,
       0,     0,     0,   403,  2071,     0,    77,   404,     0,     0,
       0,     0,     0,   405,    79,    80,   406,   407,   408,   409,
       0,     0,     0,     0,     0,     0,     0,     0,   392,   393,
       0,   394,   395,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   396,   397,   384,     0,   398,   399,   400,
     387,   401,   402,   388,     0,   389,     0,   390,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,     0,     0,   403,
    2073,     0,    77,   404,     0,     0,     0,     0,     0,   405,
      79,    80,   406,   407,   408,   409,     0,     0,     0,     0,
       0,     0,     0,     0,   392,   393,     0,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,   387,   401,   402,   388,
       0,   389,     0,   390,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,     0,     0,   403,  2075,     0,    77,   404,
       0,     0,     0,     0,     0,   405,    79,    80,   406,   407,
     408,   409,     0,     0,     0,     0,     0,     0,     0,     0,
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,   387,   401,   402,   388,     0,   389,     0,   390,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,     0,     0,
       0,   403,  2078,     0,    77,   404,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,   387,   401,
     402,   388,     0,   389,     0,   390,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391,     0,     0,     0,     0,   403,  2080,     0,
      77,   404,     0,     0,     0,     0,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,   387,   401,   402,   388,     0,   389,
       0,   390,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   391,     0,
       0,     0,     0,   403,  2122,     0,    77,   404,     0,     0,
       0,     0,     0,   405,    79,    80,   406,   407,   408,   409,
       0,     0,     0,     0,     0,     0,     0,     0,   392,   393,
       0,   394,   395,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   396,   397,   384,     0,   398,   399,   400,
     387,   401,   402,   388,     0,   389,     0,   390,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,     0,     0,   403,
    2124,     0,    77,   404,     0,     0,     0,     0,     0,   405,
      79,    80,   406,   407,   408,   409,     0,     0,     0,     0,
       0,     0,     0,     0,   392,   393,     0,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,   387,   401,   402,   388,
       0,   389,     0,   390,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,     0,     0,   403,  2126,     0,    77,   404,
       0,     0,     0,     0,     0,   405,    79,    80,   406,   407,
     408,   409,     0,     0,     0,     0,     0,     0,     0,     0,
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,   387,   401,   402,   388,     0,   389,     0,   390,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,     0,     0,
       0,   403,  2149,     0,    77,   404,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,   387,   401,
     402,   388,     0,   389,     0,   390,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391,     0,     0,     0,     0,   403,  2151,     0,
      77,   404,     0,     0,     0,     0,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,   387,   401,   402,   388,     0,   389,
       0,   390,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   391,     0,
       0,     0,     0,   403,  2153,     0,    77,   404,     0,     0,
       0,     0,     0,   405,    79,    80,   406,   407,   408,   409,
       0,     0,     0,     0,     0,     0,     0,     0,   392,   393,
       0,   394,   395,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   396,   397,   384,     0,   398,   399,   400,
     387,   401,   402,   388,     0,   389,     0,   390,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,     0,     0,   403,
       0,     0,    77,   404,     0,     0,     0,     0,     0,   405,
      79,    80,   406,   407,   408,   409,     0,     0,     0,     0,
       0,     0,     0,     0,   392,   393,     0,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,   387,   401,   402,   388,
       0,   389,     0,   390,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,     0,     0,   694,     0,     0,    77,   404,
       0,     0,     0,     0,     0,   405,    79,    80,   406,   407,
     408,   409,     0,     0,     0,     0,     0,     0,     0,     0,
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,   387,   401,   402,   388,     0,   389,     0,   390,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,     0,     0,
       0,   700,     0,     0,    77,   404,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,   387,   401,
     402,   388,     0,   389,     0,   390,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391,     0,     0,     0,     0,   709,     0,     0,
      77,   404,     0,     0,     0,     0,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,   387,   401,   402,   388,     0,   389,
       0,   390,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   391,     0,
       0,     0,     0,   403,     0,     0,    77,   404,     0,     0,
       0,     0,     0,   405,   917,    80,   406,   407,   408,   409,
       0,     0,     0,     0,     0,     0,     0,     0,   392,   393,
       0,   394,   395,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   396,   397,   384,     0,   398,   399,   400,
     387,   401,   402,   388,     0,   389,     0,   390,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,     0,     0,   403,
       0,     0,    77,   404,     0,     0,     0,     0,     0,   405,
     466,    80,   406,   407,   408,   409,     0,     0,     0,     0,
       0,     0,     0,     0,   392,   393,     0,   394,   395,  2017,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,     0,   401,   402,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   403,     0,     0,    77,   404,
       0,     0,     0,     0,     0,   405,    79,    80,   406,   407,
     408,   409
};

static const yytype_int16 yycheck[] =
{
       1,    75,   177,     4,   256,    75,  1228,     4,   182,   926,
     677,   643,   741,   377,   403,  1210,    75,   218,     1,    75,
     271,   166,     1,    59,   726,     4,   166,   656,   233,   914,
     235,  1210,   550,   551,  1369,  1370,   493,   242,   350,  1167,
     225,    75,    97,   230,   643,   647,    77,   403,   814,   230,
    1193,    84,   561,   348,     1,    56,    57,   719,    59,   166,
     405,   785,   141,   572,   815,   230,   251,  1790,   136,   364,
     821,   364,  1445,   368,    75,   368,    59,   643,  1677,     1,
      59,   230,   812,    84,   230,     1,   137,    72,    75,   812,
       0,    92,  1020,    75,  1926,     1,    97,   120,  1922,   100,
     914,    75,   170,   104,   230,   230,  1790,   104,   617,   314,
     315,   147,    59,   230,    89,    72,   100,   150,   230,   230,
      75,  1049,   192,   202,     1,   104,     4,   167,   831,   832,
     812,  1790,   319,   192,     0,   158,   192,    84,   319,    84,
       1,   142,   814,    59,   145,   148,   147,   850,    89,   812,
      97,  2062,   153,   100,   319,  1164,   230,   104,   192,   160,
     230,   473,  1171,   681,   147,  1791,   167,   170,   147,   154,
     319,   230,   467,   319,   230,     1,   231,  2088,    56,    57,
     812,     0,    59,   181,  1112,   160,    97,   162,    97,   240,
     191,   192,   179,   319,   319,   179,   230,   179,    59,   649,
     147,   158,   319,  2114,   239,   206,   902,   319,   319,   154,
     154,   246,   814,   812,    92,   216,   919,   268,   192,   133,
     221,   162,   832,   100,   225,   226,   259,   923,   279,   230,
     231,   147,   267,    59,  1833,  2059,   120,   192,   160,   161,
     850,   348,  1251,   278,   191,   319,   812,   136,   977,   319,
     251,   306,     1,   304,   162,   161,   230,    75,   259,   206,
     319,   169,   293,   319,   142,   320,   933,   145,   269,   270,
     147,   135,   273,    91,   792,   230,   112,  1006,   590,   280,
    2112,   170,   160,   534,   231,  1911,   147,    77,    78,   167,
     153,   542,   110,   294,   295,   328,   297,   160,   499,   135,
    1185,   302,    75,  1482,   971,   306,  1485,  1486,   222,   919,
      59,   137,   259,   177,   678,  2147,  1082,    90,   319,   320,
     231,   147,   231,   635,   511,  1076,   273,  1176,   329,  1702,
     511,   203,  1217,   995,  1058,   336,   337,   632,   694,   632,
     341,   697,   698,   221,   700,  1075,   511,   659,   226,   951,
     739,   696,  1075,   709,   666,   104,   712,   713,   714,   306,
     467,   656,   511,   548,    20,   511,   816,   101,  2091,   554,
     820,  1185,   162,   320,   669,   154,   669,     1,   379,   829,
     830,   382,   383,   614,   463,   511,   511,   240,   137,   303,
     899,   269,   270,  1075,   511,   306,   273,   306,   147,   511,
     511,  1673,   280,  1217,   437,  1414,  2032,  2091,  1536,   320,
    1082,   320,  1075,   157,   240,   268,   294,   295,    10,   297,
     154,  1124,    90,   610,   302,   125,   279,   112,    10,   610,
     307,   471,  2091,   280,   178,    59,   182,     1,  1773,  1774,
    1327,   511,   268,  1075,   342,   610,   447,   162,   295,   154,
     135,   329,   511,   279,   169,   511,    72,   157,   336,   337,
     491,   610,   154,   341,   610,   111,  2092,  1176,   523,   470,
     471,   156,  1168,   350,   614,   156,  1075,   511,   304,   160,
    1082,   482,   483,   381,   610,   610,   154,   297,   134,   403,
     491,   240,   493,   610,   151,    59,    77,    78,   610,   610,
     447,   379,    72,   538,   382,   162,  2132,   614,  1176,  1075,
     511,   154,    58,   137,  1124,    61,    62,  1712,    64,   268,
     571,   178,   523,   147,    72,   160,  1277,   160,   563,  1672,
     279,   160,    84,  1712,  1677,   570,    72,   511,   154,   574,
     610,   154,   158,   160,   179,    97,   179,   548,   100,   656,
     179,   610,   104,   554,   610,   304,   511,   612,   158,   308,
    1445,  1446,   179,   163,   154,   178,  1838,  1839,  1417,  1418,
    1419,   153,   164,   137,  1459,    72,   523,   169,    72,   159,
     616,   162,   164,   147,   154,   108,   109,   169,   158,   160,
     179,   631,   470,    61,    62,  1052,   473,  1264,   160,   170,
     831,   832,   130,   131,   482,   483,   154,  1358,   154,   610,
     158,   612,   523,    72,   523,   616,   240,  1284,   154,   850,
     160,   160,   158,   537,   162,   626,  1554,  1294,   940,   630,
     631,   545,    62,   616,   157,  1337,   162,   616,   839,   191,
     179,   179,  1271,   938,   268,   938,   174,   175,   562,  1099,
    1985,   825,   160,   179,   206,   279,   154,   154,   179,   573,
     154,   158,   663,   161,   158,   612,  1938,  1939,   160,   616,
     100,   391,  1559,  1560,  1561,   676,   240,    72,   154,   231,
     304,   111,   155,   113,   556,   115,   162,   179,   919,   162,
    1833,   153,   877,    72,   162,   154,   416,   417,   160,   158,
     616,   612,    72,   612,   268,    72,  1435,   259,  1417,  1418,
    1419,   179,   160,   590,    72,   279,  1633,   437,   571,   156,
      72,   273,    72,   160,   154,     3,   603,   157,   158,   539,
     731,   179,   733,    72,   735,   136,   767,   160,   739,   616,
     304,   742,  1260,   160,    72,   571,  1968,   467,   626,  1417,
    1418,  1419,   630,   631,  1456,   616,   179,   160,    72,   154,
     991,   633,   179,   158,   165,   166,   767,   170,   766,    13,
      14,    15,    16,    17,   814,   154,   206,    72,   160,   158,
     694,   154,   659,   156,   154,   663,   700,   154,   158,    69,
     616,   158,   134,  1936,   153,   709,   154,   179,   676,   160,
     158,   160,   154,   613,   154,  1948,   158,   633,   158,     3,
      75,   812,   154,   814,   728,   154,   158,  1702,   179,   158,
     179,  1210,   571,   165,   166,   826,   154,    92,    72,   160,
     158,   160,   833,   162,   157,  1763,   178,  1765,   839,   170,
     154,   842,   160,   273,   158,   275,   276,   108,   109,  1551,
     851,   991,   170,   731,  1210,   733,    72,   735,  1525,   154,
    1111,   739,  1319,   158,   742,   154,   901,   616,   160,   148,
     149,   150,   379,  2016,   156,   382,   877,   307,   170,   154,
     878,   160,   312,   160,   991,   167,   168,   162,   318,   767,
     162,   170,   136,  1124,   169,   447,   768,   148,   149,   150,
     179,  1265,  1266,    13,    14,    15,    16,    17,   165,   108,
     109,   912,   913,   914,   160,   172,   173,   156,  1119,   170,
     350,   160,   962,   178,   170,   355,    58,   357,   925,    61,
      62,   914,    64,   162,   134,   914,  1538,   809,    13,    14,
      15,    16,    17,   154,  1149,   154,   925,   571,   826,   158,
    1306,  1836,   158,   176,   154,   833,   154,   163,   158,   831,
     832,   962,    72,  1275,   394,   165,   166,   156,   156,   157,
     884,   523,   161,   851,   784,   976,  1271,   156,   850,   158,
     156,   134,   896,   809,   134,   161,   900,   157,   158,   799,
     904,   161,   616,   803,   156,   156,  1453,    72,   914,   161,
     161,   154,   156,   154,   154,   158,   160,   571,   158,   156,
     156,  1012,   165,   166,   161,   165,   166,   447,  1626,  1020,
     120,    47,    48,  1631,    50,   745,   136,   156,  1280,    55,
     156,   160,   156,  1422,   912,   913,   914,   914,   910,   167,
     168,  1081,  1082,   473,     3,   475,   476,   919,  1049,   128,
     129,  1052,   616,   914,    13,    14,    15,    16,    17,   156,
     490,  1728,  1729,   940,    18,   156,   154,   156,   156,  1254,
     158,  1302,   156,  1530,  1075,   160,   160,    22,   156,   825,
    1081,  1082,   160,   154,   962,   154,  1261,  1262,   914,   166,
     836,   156,  1549,   523,   154,   160,   132,   133,    13,    14,
      15,    16,    17,    57,    58,    59,    60,    61,    62,    63,
      64,  1112,   154,    72,   159,   160,  2001,  1291,   548,   148,
     149,   150,   160,   553,   106,   555,   203,   160,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   101,  1369,   159,
     160,   170,   154,  1176,  2029,   154,   576,   154,   578,   579,
     179,   158,   154,  1404,   156,   156,   158,    72,   162,   160,
     590,  1301,  1302,  1527,  1271,   914,    89,   154,   154,   156,
    1167,   158,   158,   603,   162,   154,   158,   156,  2063,   158,
     154,   156,   612,   156,   158,   160,  1187,   160,  1167,  1190,
    1191,  1192,   154,   156,   162,  1302,   158,   160,  1199,     3,
     165,   166,  1185,  1377,  1378,   635,  1185,   637,   638,    13,
      14,    15,    16,    17,   156,  1882,  1217,   156,   160,   134,
     156,   160,  1223,   156,   160,  1370,   153,   160,   156,   659,
     660,   179,   160,   159,  1217,  1964,   666,  1238,  1217,   154,
    1241,  1242,   156,   158,  1245,  1242,   160,   746,   747,   748,
     165,   166,  1124,  1254,  1064,   156,  1170,   156,  1068,   160,
     156,   160,   156,  1242,   160,   156,   160,   987,    72,  1185,
    1184,   169,   992,   156,   156,  1085,   156,   160,   160,  1510,
     160,   156,  1092,  1003,   162,   160,   178,   156,  1289,  1203,
     914,   160,   156,  2096,  1241,  1242,  1210,  2100,   120,  1045,
     154,  1217,  1303,   123,   154,   125,   126,   127,  1185,  1187,
     171,  1978,  1190,  1191,  1192,   159,   160,    13,  1319,   159,
     160,  1199,   159,   160,  1185,   154,  1327,  1137,  1173,  1174,
    1175,  1141,   159,   160,   154,  1145,   154,   157,   158,  1217,
    1217,   160,   162,   163,   176,  1223,   753,   754,   755,   756,
     914,   113,   114,   115,   116,   117,  1217,   166,  1359,  1185,
    1238,  1623,  1624,  1625,  1241,   160,   161,  1245,   159,   160,
    1510,    91,    92,  1428,   164,  1121,   159,   160,  1742,  1644,
    1645,  1646,   159,   160,  1417,  1418,  1419,   159,   160,  1422,
    1423,  1217,    88,   134,  1445,   159,   160,   157,  1275,  1755,
     159,   160,   156,  1510,    13,    14,    15,    16,    17,    18,
     106,  1289,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   156,  1420,   156,  1303,   156,  1428,   159,   160,
     156,  1432,  1433,   148,   149,   150,  1185,   159,   160,   160,
     161,  1420,   159,   160,   156,   160,   159,   160,   159,   160,
    1637,   158,  1453,   159,   160,   170,  1637,    13,    14,    15,
      16,    17,  1445,  1446,   179,  1185,  1445,  1446,  1217,   159,
     160,   159,  1637,   159,   160,   136,  1459,  1478,  1479,   556,
    1459,    13,    14,    15,    16,    17,    18,  1488,  1637,   159,
     160,  1637,   136,  1242,    77,    78,   161,  1369,  1370,   161,
      62,     4,     5,     6,     7,     8,     9,    10,    11,    12,
     940,  1637,  1637,   160,   161,   160,    72,  1428,   154,  1428,
    1637,   951,  1436,  1437,   178,  1637,  1637,  1328,  1329,  1530,
     960,    13,    14,    15,    16,    17,    18,   614,    84,  1536,
     156,  1488,  1773,   105,   156,  1291,   156,   109,  1549,   156,
     112,  1271,   114,  1554,  1432,  1433,   633,  1536,  1559,  1560,
    1561,  1185,    65,  1637,   749,   750,   757,   758,  1445,  1446,
     156,  1626,   156,  1487,   156,  1720,  1631,   156,   134,  1389,
    1720,   156,  1459,  1393,  1639,   751,   752,  1397,   154,  1309,
    1310,  1311,   159,  1217,  1340,   158,  1316,  1317,   154,   156,
    1478,  1479,   158,   162,   150,  1485,  1486,  1645,  1646,   165,
     166,  1488,   162,  1720,   178,  1528,  1529,   162,  1805,  1445,
    1446,  1185,  1261,  1262,  1805,  1626,   162,   162,  1058,  1774,
    1631,  1377,  1378,  1459,   160,    70,  1637,   179,  1639,   159,
    1805,   154,    78,   159,   190,    18,  1647,   178,   160,   154,
     179,  1702,  1082,  1217,   162,   156,  1805,  1658,   156,  1805,
     162,  1956,    13,    14,    15,    16,    17,    18,   179,  1666,
    1671,   162,   159,   159,    13,    14,    15,    16,    17,  1805,
    1805,    18,   153,   156,  1869,   153,   156,  1666,  1805,  1241,
    1242,   768,   156,  1805,  1805,   156,  1445,  1446,   156,   156,
     262,   156,  1700,   156,   156,  1515,    22,   156,   156,   156,
    1459,   153,   162,   259,    70,  1626,  1717,  1626,   162,  1702,
    1631,  2033,  1631,  1702,   162,   162,  1723,   178,  1639,   156,
    1639,   156,   809,    72,   162,  2030,   156,   156,   153,   178,
     156,   162,   156,   160,  1723,   153,   156,   309,  1922,   160,
     156,   160,   156,  1808,   831,   832,   156,   156,   156,  1569,
     156,   156,  1763,   156,  1765,   159,  1576,   159,   156,  1647,
    1580,   156,   156,   850,   156,   156,   156,   156,  1692,   156,
    1658,   156,   328,   159,   156,   156,   156,   156,  1665,   154,
     352,   153,   354,  1671,   356,   134,   160,   154,    14,   154,
     154,   154,   348,   161,  1805,   154,   154,  1808,    74,   161,
     160,  1241,   179,   159,   159,   154,  1817,   153,   179,   158,
    1821,  1445,  1446,   153,   162,  1702,   165,   166,   162,   160,
     156,   156,   394,   910,  1835,  1459,   179,   179,   156,  1717,
    1985,   156,   919,   156,  1845,  1275,  1847,  2032,   159,   156,
     160,  1281,   160,  1836,   156,  1853,   160,  1836,  1859,   159,
    1861,  1862,  1863,   156,   156,  1920,   156,  1868,  1869,   156,
     159,   153,   153,   179,   154,   154,  1702,    80,   179,    92,
     179,  1445,  1446,  2057,   179,  2059,   154,   154,   179,   153,
      90,   437,   154,     4,   179,  1459,   179,  1808,   156,  1808,
     153,  1773,  1774,   179,  2091,   153,   160,   153,   160,   156,
    2091,   162,   162,   153,   991,   159,   159,  1666,   159,  1920,
     159,   467,   123,  1956,  2098,  1926,  2091,   161,   490,  1930,
     161,   153,   156,   159,  1935,   153,  1488,   156,   159,  1817,
     154,   156,  2091,  1821,   156,  2091,  1823,   156,   153,   179,
     161,   153,   156,  1702,   160,   154,   154,  1835,    75,  1836,
    1961,   156,  1960,   159,   156,  2091,  2091,  1845,     1,  1847,
     159,     4,   159,   153,  2091,   153,   162,   153,   159,  2091,
    2091,  1859,   156,  1861,  1862,  1863,   156,  1707,   156,   100,
    1868,    75,   153,   104,   179,   179,   154,  2030,   154,  1429,
    2174,   156,   179,  2004,   550,   551,   568,  2008,   156,  1920,
    1836,  1920,   159,   159,   153,   153,   153,  2091,  2001,   162,
    2021,  2091,  2001,   153,   156,   156,    59,   156,   156,    75,
     170,  2032,  2091,  2034,   158,  2091,   157,  2092,   179,    75,
     153,   161,    75,   170,   179,   179,  2029,  1124,  1926,   153,
    2029,    84,  1930,   160,   153,   155,   170,  1935,  1488,   153,
     170,   106,   154,   161,    97,  2066,    75,   100,   153,   160,
     179,   104,   155,   170,   170,    75,   179,  2132,  1702,   159,
    2063,   153,   155,  1961,  2063,   156,   156,  1836,   153,   156,
    2091,  2092,   203,   161,   153,   156,   154,   179,  2096,   156,
     179,  2102,  2100,  2101,  2105,  1755,  1334,   718,   141,   759,
     656,  2112,   760,  1985,   147,    75,   179,   150,   761,  1205,
     153,   154,   762,  1217,  2001,   763,  2004,   436,  2147,  1702,
    2008,  2132,  2059,   166,  1844,   681,  2134,    97,  1702,  2088,
    2141,  1836,   216,  2021,  2145,  2092,  2147,  1459,  1711,  2046,
    1693,  2129,  2029,  2101,  2030,  2163,  2034,   190,   191,   192,
    1693,  2159,   273,  1245,  2029,  2163,    72,  2168,    49,   202,
     203,   112,  1920,   206,   264,  2001,  1922,  1991,  2179,   966,
    2178,  2092,  1423,  2092,  1238,  2132,  2063,  2188,  2066,   929,
     839,  1723,   225,   153,   626,   500,     0,   230,   231,   784,
     106,   976,  1187,  2029,   110,   111,   112,   113,   114,   115,
     116,   117,  1836,   784,  1199,   784,  1615,    -1,   251,    -1,
      -1,  2132,    -1,  2132,  2102,  1302,   259,  2105,   134,    -1,
      -1,    -1,    -1,    -1,  2112,  1665,    -1,  2063,    -1,    -1,
     273,    -1,  2052,  1963,  2158,    -1,   792,    -1,   154,   155,
      -1,    -1,  2001,   675,    -1,    -1,  2170,    -1,    -1,   165,
     166,    -1,    -1,  2141,    -1,   225,    -1,  2145,    -1,  2147,
     230,   231,  1836,   306,    -1,    -1,    -1,    -1,    -1,   312,
    2029,    -1,    -1,    -1,    72,   318,   319,   320,    -1,    -1,
    2168,   251,  1369,  1370,    -1,   328,    -1,    -1,   106,    -1,
      -1,  2179,   110,   111,   112,   113,   114,   115,   116,   117,
    2188,  2057,    -1,  2059,  2063,   348,   349,   350,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   364,    -1,    -1,    -1,   368,    -1,   203,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,   306,   155,    -1,    -1,
     158,    -1,  2098,    -1,    -1,    -1,    -1,    -1,    -1,   319,
     320,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,
     403,    -1,    -1,    -1,    -1,    -1,    -1,  2001,  1808,    -1,
     926,    -1,    -1,    -1,  2130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,  1823,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   437,  2029,    -1,   440,    -1,    -1,
      -1,    -1,   241,    -1,   447,   837,    -1,   491,    -1,   493,
      -1,    -1,    -1,    65,    66,    67,    68,    -1,  2174,    -1,
     463,    -1,    -1,  1510,   467,    -1,    -1,  2001,   471,  2063,
     473,   155,    -1,    -1,   158,   556,    -1,    -1,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,    -1,   106,  2029,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   134,    -1,   511,    -1,
      -1,  1911,   348,    -1,    -1,   351,    -1,    -1,    -1,    -1,
     523,    -1,    -1,  1478,  1479,    -1,   154,   155,   364,  2063,
      -1,    -1,   368,   161,    -1,    78,    -1,   165,   166,    -1,
      -1,   471,    -1,    -1,    -1,   548,   158,   550,   551,    -1,
     106,   554,   633,   556,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   106,   176,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,   969,    -1,    -1,
     106,   511,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   106,   523,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,   610,    -1,   612,
      -1,   614,    -1,   616,    -1,    -1,    -1,    -1,   548,    -1,
      -1,    -1,    -1,    -1,   554,    -1,    -1,    -1,    -1,   632,
     633,   467,   635,  2033,    -1,    -1,   179,    -1,    -1,   165,
     643,   440,    -1,    -1,   647,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   656,    -1,    -1,    -1,   456,    -1,    -1,
     459,    -1,    -1,   666,    -1,   179,   669,    -1,    -1,    -1,
      -1,    -1,    -1,  1720,    -1,    -1,    -1,    -1,   681,    -1,
     610,    -1,   612,    -1,    -1,    -1,    -1,   768,    -1,    -1,
      -1,   694,    -1,    -1,   697,   698,    -1,   700,    -1,    -1,
      -1,    -1,    -1,  1658,    -1,    -1,   709,    -1,    -1,   712,
     713,   714,    -1,    -1,    -1,    -1,  1671,   516,    -1,    -1,
     556,    -1,    -1,    -1,    -1,    -1,  1773,  1774,   809,    -1,
      -1,   106,  2132,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,  1260,    -1,    -1,    -1,    -1,    -1,
     831,   832,    -1,    -1,    -1,  1271,    -1,    18,    -1,    -1,
      -1,    -1,  1717,    -1,    -1,   768,   106,    -1,    -1,   850,
     110,   111,   112,   113,   114,   115,   116,   117,   614,    -1,
      -1,   784,   785,    -1,    -1,    -1,    -1,    -1,    -1,   792,
      -1,    -1,    -1,    -1,    -1,   839,   632,   633,   842,    -1,
      61,    62,    63,    64,   179,    -1,   809,    -1,    -1,   812,
      -1,   814,    -1,    -1,   154,   155,   182,    -1,    -1,    -1,
     656,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   831,   832,
      -1,    -1,    -1,   669,    -1,    -1,    -1,    -1,   919,    -1,
      -1,    -1,    -1,    -1,   925,   106,    -1,   850,    -1,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,  1817,    -1,    -1,    -1,  1821,    -1,    -1,    -1,
      -1,  1263,    -1,    -1,   877,    -1,    -1,    -1,    -1,    -1,
    1835,    -1,   812,    -1,   814,    -1,    -1,    -1,    -1,    -1,
    1845,    -1,  1847,    -1,    -1,    -1,    -1,   158,     1,    -1,
      -1,     4,    -1,  1295,  1859,    -1,  1861,  1862,  1863,    -1,
      -1,   914,    -1,    -1,    -1,    -1,   919,    -1,    -1,    -1,
      -1,    72,   925,   926,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   768,    -1,    -1,   938,    -1,   940,  1985,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   877,   951,    -1,
      -1,    -1,  1344,    -1,    -1,   106,    59,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,  1012,    -1,
      -1,  1926,    -1,   809,    -1,  1930,  1020,    -1,    -1,    -1,
    1935,    84,    -1,   134,    -1,   784,   785,    -1,   991,    -1,
      -1,    -1,    -1,    -1,    -1,   794,    -1,    -1,   797,    -1,
      -1,   104,    -1,   154,   155,  1049,  1961,   158,  1052,    -1,
      -1,    -1,    -1,    -1,   165,   166,    -1,    -1,    -1,    -1,
      -1,   387,    -1,    -1,    -1,   391,   392,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   137,   401,   402,    -1,   141,    -1,
      -1,    -1,    -1,  1124,   147,    -1,    -1,   150,    -1,  2004,
     416,   417,    -1,  2008,    -1,  1058,    -1,    -1,   857,    -1,
      -1,    -1,    -1,   166,    -1,   864,  2021,    -1,  1112,   868,
      -1,   437,  1075,   872,    -1,    -1,    -1,    -1,    -1,  1082,
      -1,    -1,    -1,    -1,    -1,    -1,  1167,   190,   191,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,   202,
     203,   467,   938,    -1,    -1,   941,    -1,    -1,    -1,    -1,
      -1,  2066,    -1,    -1,    -1,  1507,    -1,  1633,    -1,    -1,
      -1,  1124,    -1,    -1,    -1,    -1,    -1,    -1,   231,    -1,
      -1,    -1,  1524,    -1,    -1,    -1,    -1,   240,    -1,    -1,
      -1,  1533,    -1,    -1,    -1,  1075,    -1,  2102,   251,    -1,
    2105,  1081,  1082,   256,   257,   991,   259,  2112,  1550,    -1,
    1241,  1242,    -1,    -1,  1167,   268,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1176,    -1,    -1,   279,    -1,    -1,   282,
      -1,    -1,  1185,   286,    -1,    -1,  2141,    -1,   291,    -1,
    2145,    -1,  2147,    -1,    -1,    -1,    -1,    -1,    -1,   302,
      -1,   304,    -1,    -1,    -1,   308,    -1,  1210,    -1,    -1,
      -1,    -1,    -1,  2168,  1217,    -1,    -1,   320,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   328,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,  1241,  1242,
      -1,    -1,    -1,    -1,    -1,   348,    -1,    -1,   351,    -1,
      -1,  1254,    -1,    -1,    -1,    -1,    -1,  1260,    -1,  1058,
      -1,   364,    -1,    -1,    -1,   368,    -1,    -1,  1271,    -1,
      -1,    -1,    -1,    -1,    -1,  1319,    72,    -1,    -1,    -1,
     162,    -1,    -1,  1327,    -1,    -1,    -1,   169,  1369,  1370,
    1682,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1302,
     403,    -1,    -1,  1306,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,  1254,    -1,    -1,    -1,    -1,  1128,
      -1,    -1,  1131,    -1,   437,    -1,  1135,    -1,   134,  1420,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1741,
      -1,    -1,    -1,    -1,  1746,    -1,    -1,    -1,   154,   155,
     463,    -1,    62,    -1,   467,  1757,  1369,  1370,    -1,   165,
     166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   745,
     746,   747,   748,   749,   750,   751,   752,   753,   754,   755,
     756,   757,   758,   759,   760,   761,   762,   763,    -1,    -1,
     100,    -1,    -1,    -1,    -1,    -1,    -1,  1488,    -1,  1453,
      -1,   111,   112,    -1,  1417,  1418,  1419,  1420,    -1,  1422,
    1423,    -1,    -1,    -1,    -1,  1428,  1429,    -1,    -1,  1359,
      -1,    -1,    -1,    -1,    -1,  1271,    -1,    -1,    -1,    -1,
      -1,    -1,  1445,  1446,    -1,    -1,    -1,   550,   551,    -1,
      -1,    -1,    -1,   556,   154,  1536,  1459,    -1,    -1,   825,
      -1,    -1,    -1,    -1,    -1,    -1,  1302,   106,   571,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,  1871,
    1872,    -1,    -1,   106,    -1,  1488,  1530,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,  1428,   122,
      -1,   124,    -1,    -1,    -1,  1549,   206,  1510,    -1,    -1,
    1554,   614,    -1,   616,    -1,  1559,  1560,  1561,    -1,    -1,
      -1,    -1,    -1,   162,    -1,    -1,    -1,    -1,    -1,   632,
     633,    -1,   155,  1536,    -1,  1538,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   647,    -1,    -1,  1346,    -1,    -1,
      -1,    -1,    -1,   656,    -1,    -1,  1355,    -1,   661,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   669,    -1,    -1,    -1,
      -1,    -1,    -1,   273,    -1,    -1,    -1,    -1,   681,    -1,
      -1,    -1,    -1,    -1,  1665,  1666,    -1,  1979,    -1,    -1,
      -1,   694,    -1,    -1,   697,   698,    -1,   700,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   709,   307,    -1,   712,
     713,   714,   312,    -1,    -1,    -1,    -1,    -1,   318,    -1,
      -1,   987,    -1,  1626,  1627,    -1,   992,    -1,  1631,    -1,
    1633,    -1,    -1,    -1,  1637,    -1,  1639,  1003,    -1,    -1,
      -1,    -1,  1723,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     350,    -1,    -1,    -1,    -1,    -1,    -1,  2049,    -1,    -1,
      -1,    -1,  1665,  1666,    -1,   768,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1510,    -1,    -1,    -1,    -1,  1045,
      -1,    -1,   785,    -1,    -1,    -1,    -1,    -1,    -1,   792,
      -1,    -1,  1773,  1774,   394,    -1,  1626,    -1,    -1,  1702,
      -1,  1631,    -1,    -1,    -1,    -1,   809,  1637,    -1,  1639,
      -1,   814,    -1,    -1,    -1,    -1,    -1,  1720,    -1,  1763,
    1723,  1765,    -1,    -1,    -1,    -1,    -1,    -1,   831,   832,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,  1823,    -1,    -1,    -1,    -1,   850,    -1,    -1,
      -1,    -1,  1755,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
    1773,  1774,    -1,   473,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,    -1,    -1,   162,  1790,  1791,    -1,
     490,    -1,    -1,   169,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1805,    -1,    -1,  1808,    -1,    -1,    -1,    -1,
      -1,   914,    -1,    -1,    -1,    -1,   919,    -1,    72,  1185,
    1823,    -1,   925,   926,    -1,    -1,    -1,    -1,  1627,    -1,
      -1,    -1,    -1,  1836,    -1,   938,    -1,    -1,   941,    -1,
      -1,    -1,    -1,    -1,    -1,   948,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,    -1,   555,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,  1869,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1805,   576,    -1,  1808,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   991,    -1,
     590,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,   155,    -1,   603,  1985,  1271,    -1,    -1,  1911,    -1,
      -1,   165,   166,    -1,    -1,    -1,    -1,  1920,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1291,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,   635,    -1,    -1,    -1,  1869,
      -1,    -1,    -1,  1309,  1310,  1311,    -1,    -1,    -1,    -1,
    1316,  1317,    -1,  1956,    -1,    -1,    -1,    -1,    -1,   659,
      -1,    -1,    -1,    -1,    -1,    -1,   666,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1340,    -1,    -1,    -1,    49,    -1,
      -1,    52,  1985,    54,    -1,    56,    -1,    -1,  1991,    -1,
    1920,  1790,  1791,    -1,    -1,    -1,    -1,    -1,  2001,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1377,  1378,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1124,    -1,    -1,    -1,    -1,  2029,  2030,    -1,  2032,
    2033,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
    2063,    -1,    -1,   134,  1167,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,  1176,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,  1185,   154,    -1,    -1,   157,   158,  2091,  2092,
      -1,    -1,    -1,   164,   165,   166,   167,   168,   169,   170,
      -1,    -1,  2032,    -1,    -1,    -1,    -1,  1210,    -1,    -1,
      -1,    -1,  1911,    -1,  1217,    -1,    -1,    -1,    -1,    -1,
    1956,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2132,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1242,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    84,    -1,  1260,    -1,    -1,
      -1,  2091,  2092,    -1,    -1,    -1,    -1,    -1,  1271,    -1,
      -1,   100,    -1,    -1,    -1,   104,    -1,  1280,    -1,    -1,
      -1,   881,    -1,    13,    14,    15,    16,    17,  1987,    -1,
      -1,    -1,  1991,    -1,  2030,    -1,    -1,    -1,    -1,  1302,
      -1,    -1,  2132,  1306,    -1,    -1,    -1,    72,   137,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,   147,    -1,
      -1,   150,    -1,    -1,    -1,   154,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2032,    -1,    -1,   165,   166,   167,    -1,
     940,   106,    72,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   190,    -1,    -1,    -1,    -1,  1369,  1370,    -1,   134,
      -1,    -1,    -1,   202,   203,    -1,   106,   206,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   154,
     155,    -1,  2091,  2092,    -1,    -1,    -1,    -1,    -1,    -1,
     165,   166,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   240,    -1,    -1,  1417,  1418,  1419,  1420,  1421,  1422,
    1423,    -1,    -1,    -1,   154,   155,    -1,    -1,   257,    -1,
     259,    -1,    -1,  2132,    -1,   165,   166,    -1,    -1,   268,
      -1,  1707,  1445,  1446,   273,    -1,    -1,    -1,    -1,    -1,
     279,    -1,    -1,    -1,    -1,    -1,  1459,    13,    14,    15,
      16,    17,    -1,    -1,    84,   294,    -1,    -1,   297,    -1,
      -1,    -1,    -1,   302,    -1,   304,    -1,    -1,   307,   308,
      -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,   318,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1510,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,   348,
      -1,   350,   351,    -1,    -1,    13,    14,    15,    16,    17,
     150,    -1,    -1,  1536,    -1,   364,    -1,    -1,    -1,   368,
      -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     190,    -1,    -1,    -1,   403,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,   203,    72,    -1,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   154,   155,
      -1,    -1,   158,    -1,    -1,    -1,    -1,    -1,   437,   165,
     166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
    1633,    -1,    -1,    -1,   463,    -1,  1639,     1,   467,   259,
      -1,  1241,   162,    -1,   473,    -1,   134,    -1,    -1,   169,
      -1,    -1,    -1,    -1,    -1,    -1,  1922,    -1,    -1,    -1,
      -1,    -1,    -1,  1666,    -1,    -1,   154,   155,    -1,    -1,
     158,    -1,    -1,    -1,    -1,  1275,    -1,   165,   166,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    59,    -1,  1963,    -1,  1702,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,    -1,
     539,    -1,    -1,    -1,    -1,    -1,    -1,  1720,    -1,    -1,
    1723,   550,   551,    -1,    -1,    -1,   555,   556,   348,   154,
      -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   571,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1755,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   590,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1773,  1774,    -1,   137,   603,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   147,   613,   614,    -1,   616,  1791,    -1,
      -1,  2057,    -1,  2059,    -1,    -1,    13,    14,    15,    16,
      17,   630,   166,   632,   633,    -1,   635,    -1,    -1,    -1,
    1410,    -1,    -1,    -1,   643,    -1,    -1,   437,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   656,    -1,  1429,
     659,    -1,  2098,  1836,   663,    -1,    -1,   666,    -1,   203,
     669,    -1,   671,    -1,    -1,    -1,    -1,   467,    -1,    -1,
      -1,    -1,   681,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2130,   694,    -1,    -1,   697,   698,
      -1,   700,    -1,    -1,    -1,    -1,   240,    -1,    -1,    -1,
     709,    -1,    -1,   712,   713,   714,    -1,    -1,  1488,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,   268,    -1,    -1,    -1,  2174,    -1,
      -1,    -1,    -1,    -1,    -1,   279,   106,   134,   282,    -1,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     550,   551,   122,   297,   124,    -1,   556,   154,   155,   768,
     304,    -1,    -1,    -1,   308,    -1,    -1,    -1,   165,   166,
      -1,    -1,    -1,  1956,    -1,   784,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   792,    -1,   155,    -1,    -1,   158,    -1,
     799,    -1,    -1,    -1,   803,    -1,    -1,    -1,    -1,    -1,
     809,    -1,  1985,   812,   348,    -1,    -1,   351,    -1,    -1,
      -1,    -1,    -1,    -1,   614,    -1,    -1,    -1,  2001,    -1,
     364,    -1,   831,   832,   368,    -1,    -1,    -1,    -1,    49,
      -1,    -1,    52,   633,    54,    -1,    56,    -1,    -1,    -1,
      -1,   850,    -1,    -1,    -1,    -1,  2029,  2030,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,   656,    -1,    -1,   106,
      -1,    -1,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   881,    -1,    -1,   122,    -1,   124,    -1,    -1,
    2063,   681,    -1,   103,   104,  1665,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   910,   122,   123,   124,   914,   126,   127,   155,  2092,
     919,   158,    -1,    -1,   134,    -1,   925,   926,    -1,    -1,
      -1,    72,    -1,   467,    -1,    -1,    -1,    -1,    -1,   938,
      -1,   940,   941,    -1,   154,   155,    -1,   157,   158,    -1,
      -1,    -1,   162,    -1,   164,   165,   166,   167,   168,   169,
     170,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   768,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   991,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   792,    -1,    -1,   539,    -1,   134,    -1,    -1,
      -1,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,   809,
      -1,    -1,   556,    -1,   165,   166,    -1,   154,   155,    -1,
      -1,   158,    -1,    -1,    -1,    -1,    -1,   571,   165,   166,
      48,   831,   832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   178,    -1,  1823,    -1,    -1,    -1,    -1,    -1,    -1,
     850,    -1,    -1,    -1,    -1,  1064,    -1,    75,    -1,  1068,
      -1,    -1,    -1,    -1,    -1,    -1,  1075,    -1,    -1,   613,
     614,    -1,   616,    -1,    -1,    -1,  1085,    -1,    -1,    -1,
      -1,    -1,    -1,  1092,    -1,    -1,    -1,    -1,   632,   633,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   643,
      -1,   403,    -1,   121,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   656,    -1,    -1,  1124,   134,    -1,   136,   919,
      -1,    -1,    -1,    -1,    -1,   669,   926,    -1,  1137,    -1,
      -1,    -1,  1141,    -1,    -1,    -1,  1145,    -1,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1167,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,  1176,    -1,    -1,
      -1,    -1,    -1,    -1,   192,    -1,  1185,    -1,    -1,     1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,
      -1,   991,    -1,    -1,    -1,    -1,    -1,   165,   166,    -1,
      -1,  1210,    -1,    -1,    -1,    -1,    -1,    -1,  1217,    -1,
      -1,    -1,   230,    -1,    -1,    -1,   234,    -1,    -1,   237,
     238,    -1,    -1,   241,   768,    -1,   244,   245,    -1,   247,
      -1,   249,  1241,  1242,    -1,    -1,    -1,    59,    -1,    -1,
     784,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   550,   551,
      -1,  1260,    -1,  2033,    -1,   799,    -1,    -1,    -1,   803,
      -1,    -1,  1271,    -1,    -1,   809,  1275,   106,   812,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,  1288,
      -1,    -1,   104,    -1,    -1,    -1,    -1,   831,   832,    -1,
      -1,    -1,  1301,  1302,    -1,    -1,    -1,  1306,    -1,    -1,
      -1,   319,    -1,    -1,   322,    -1,   850,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,   158,
      -1,    -1,    -1,    -1,  1124,   147,    -1,   345,   346,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   360,    -1,   166,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   134,    -1,
    1369,  1370,    -1,    -1,    -1,    -1,   910,    -1,    -1,    -1,
     914,    -1,    -1,    -1,    -1,   919,    -1,    -1,   154,   155,
    1389,   203,    -1,    -1,  1393,   161,    -1,    -1,  1397,   165,
     166,    -1,   694,    -1,   938,    88,   157,   941,   700,    -1,
      -1,  1410,    -1,    -1,    -1,    -1,    -1,   709,  1417,  1418,
    1419,  1420,  1421,  1422,  1423,    -1,    -1,    -1,   240,    -1,
    1429,    -1,    -1,    -1,    -1,    -1,   728,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   128,  1445,  1446,   456,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   268,   991,    -1,    -1,
    1459,    -1,    -1,    -1,    -1,    -1,    -1,   279,    -1,    -1,
    1260,    -1,   764,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1271,    -1,    -1,    -1,   297,    -1,    -1,    -1,  1488,
      -1,    -1,   304,    -1,    -1,    -1,   308,    -1,    -1,    -1,
      -1,    -1,    -1,   511,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1510,  1302,    -1,    -1,    -1,  1515,    -1,   106,   527,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
    1064,    -1,    -1,    -1,  1068,    -1,   348,  1536,    -1,   351,
      -1,  1075,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1085,   364,    -1,    -1,    -1,   368,   106,  1092,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   157,
    1569,    -1,    -1,    -1,    -1,    -1,    -1,  1576,    -1,  1369,
    1370,  1580,    -1,    -1,    -1,   134,    -1,    -1,    -1,   106,
    1124,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   610,  1137,    -1,   154,   155,  1141,    -1,   158,
      -1,  1145,    -1,    -1,    -1,    -1,   165,   166,    -1,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   316,  1633,    -1,    -1,   154,    -1,    -1,
      -1,    -1,    -1,   651,   652,    -1,    -1,    -1,   134,    -1,
      -1,  1185,    -1,    -1,    -1,   467,   664,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1665,  1666,   154,   155,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   165,
     166,    -1,   106,  1217,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1702,    -1,    -1,    -1,    -1,  1242,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1510,  1720,    -1,    -1,  1723,    -1,    -1,   539,    -1,    -1,
     154,   155,    -1,    -1,    -1,    -1,    -1,  1271,    -1,    -1,
      -1,   165,   166,   106,   556,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,  1755,    -1,    -1,   571,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1301,  1302,    -1,
      -1,    -1,    -1,    -1,  1773,  1774,    -1,    -1,    -1,    -1,
     788,   789,    -1,   466,    -1,   468,   794,    -1,    -1,    -1,
      -1,   154,    -1,    -1,   477,   478,    -1,    -1,    -1,    -1,
      -1,   613,   614,    -1,   616,    -1,    -1,   815,    -1,    -1,
     818,   819,    -1,   821,    -1,   823,   824,    -1,    -1,    -1,
     632,   633,    -1,    -1,  1823,    -1,    -1,    -1,    -1,    -1,
      -1,   643,    -1,    -1,    -1,  1369,  1370,  1836,    -1,    -1,
      -1,    -1,    -1,  1633,   656,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1389,   864,   669,    -1,  1393,
     868,    -1,    -1,  1397,   872,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,  1445,  1446,    51,    -1,    53,    -1,    -1,  1210,    -1,
      -1,    -1,    -1,    -1,    -1,  1459,    -1,   935,   936,    -1,
    1720,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,   949,    -1,   106,    -1,    -1,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   768,  1956,    -1,   122,
      -1,   124,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
     108,   109,   784,    -1,    -1,    -1,  1510,    -1,    -1,    -1,
      -1,  1515,    -1,  1773,  1774,    -1,  1985,   799,    -1,    -1,
      -1,   803,   155,    -1,    -1,   158,    -1,   809,    -1,    -1,
     812,    -1,  2001,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,    -1,    -1,    -1,   151,    -1,   831,
     832,    -1,    -1,   161,    -1,    -1,    -1,    -1,    -1,    -1,
    2029,  2030,    -1,    -1,  2033,  1569,    -1,    -1,   850,    -1,
      -1,    -1,  1576,    -1,   178,    -1,  1580,    -1,    -1,    -1,
      -1,    -1,   106,  2052,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,  2063,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1081,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   910,    -1,
      -1,    -1,   914,    -1,    -1,    -1,    -1,   919,    -1,    -1,
      -1,    -1,    -1,    -1,  1122,    -1,    -1,    -1,    -1,    -1,
    1128,    -1,    -1,  1131,    -1,    -1,   938,  1135,    -1,   941,
      -1,    -1,  1666,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   825,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1442,    -1,    -1,  1445,  1446,    -1,    -1,    -1,    -1,  1451,
      -1,    -1,    -1,  1455,    -1,  1457,    -1,  1459,  1702,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,   991,
      -1,    -1,    -1,    -1,    -1,    -1,  1720,    13,    14,    15,
      16,    17,    -1,    -1,    20,  1985,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,   917,   918,    -1,    -1,   921,  1773,
    1774,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,  1064,    -1,    -1,    -1,  1068,    -1,    -1,    -1,
      -1,    -1,    -1,  1075,    -1,    -1,    -1,    -1,    -1,  1277,
      -1,    -1,    -1,  1085,    -1,    -1,    -1,    -1,  1286,  1287,
    1092,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1836,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,  1124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1613,  1005,    -1,    -1,  1137,    -1,    -1,    -1,  1141,
      -1,   157,   158,  1145,    -1,    -1,    -1,    -1,  1346,   165,
     166,    -1,    -1,    -1,    -1,    -1,    -1,  1355,    -1,    -1,
    1358,    -1,  1360,  1361,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1051,  1661,
      -1,    -1,    -1,  1185,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1676,  1677,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1405,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1217,    -1,    -1,    -1,    -1,
    1702,    -1,    -1,    -1,    -1,  1098,  1708,    -1,    -1,    -1,
      -1,    -1,  1956,    -1,  1107,  1108,  1109,  1110,    -1,    -1,
    1242,    -1,  1115,  1116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1985,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1271,
      -1,    -1,   166,    -1,    -1,    -1,    -1,  2001,    -1,    -1,
    1153,    -1,    -1,  1156,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1489,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1301,
    1302,    -1,    -1,    -1,    -1,  2029,  2030,    -1,   202,   203,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2052,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1819,    -1,  2063,
     234,    -1,    -1,    -1,  1217,    -1,  1828,   241,  1830,    -1,
      -1,  1833,  1834,    -1,  1836,    -1,    -1,    -1,    -1,  1841,
      -1,    -1,     0,    -1,    -1,     3,    -1,  1369,  1370,    -1,
      -1,    -1,    -1,    -1,    -1,  1248,    -1,  1575,    -1,    -1,
      -1,    -1,  1255,    -1,  1257,  1258,    -1,  1389,    -1,    -1,
      -1,  1393,    -1,    -1,    -1,  1397,    -1,  1270,    -1,  1272,
      -1,  1274,    -1,    -1,    -1,    -1,  1604,    -1,  1281,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   322,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1637,
      78,    -1,    -1,  1445,  1446,  1643,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   348,   349,    -1,  1459,    -1,  1941,
      -1,    -1,    -1,    -1,  1946,  1947,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   368,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1967,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1376,    -1,    -1,    -1,    -1,  1510,    -1,
    1383,    -1,    -1,  1515,  1387,    -1,    -1,    -1,    -1,    -1,
      -1,  1719,    -1,    -1,    -1,    -1,    -1,  2009,    -1,  2011,
      -1,    -1,  2014,  2015,    -1,    -1,    -1,  2019,  2020,    -1,
    1413,    -1,    -1,    -1,    -1,    -1,   440,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   456,   457,    -1,   459,   460,  1569,    -1,    -1,
      -1,    -1,    -1,   467,  1576,    -1,    -1,   471,  1580,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1469,    -1,    -1,  1797,
    1798,    -1,  2084,  2085,  2086,    -1,    -1,  1805,    -1,    -1,
      -1,    -1,  1810,    -1,    -1,    -1,    -1,    -1,   512,    -1,
     258,    -1,   516,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2118,  2119,  2120,    -1,
      -1,  1514,    -1,    -1,    -1,    -1,    -1,    -1,  1521,    -1,
    1523,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     298,    -1,   556,    -1,  1666,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   310,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   327,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1702,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   347,
      -1,    -1,    -1,    -1,    -1,  1913,  1589,   611,  1720,    -1,
     614,    -1,    -1,    -1,    -1,  1598,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   632,   633,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   643,
      -1,   112,    -1,   647,    -1,    -1,    -1,    -1,    -1,    -1,
     654,    -1,   656,    -1,    -1,    -1,   404,    -1,    -1,    -1,
      -1,  1773,  1774,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1987,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   435,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   461,    -1,    -1,    -1,   465,    -1,    -1,
      -1,    -1,    -1,    -1,  1836,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   203,    -1,    -1,    -1,   484,    -1,    -1,    -1,
     488,   489,    -1,    -1,   492,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1735,  1736,    -1,    -1,    -1,    -1,    -1,   507,
     508,    -1,    -1,    -1,   768,    -1,  1749,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     784,   785,   530,  2091,    -1,    -1,    -1,    -1,    -1,    -1,
     794,   795,    -1,   797,   798,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   809,    -1,    -1,   812,    -1,
     814,   815,    -1,    -1,    -1,    -1,    -1,   821,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   297,   831,   832,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1956,    -1,   850,    -1,    -1,   853,
      -1,    -1,    -1,   857,    -1,    -1,    -1,    -1,   606,    -1,
     864,   865,    -1,    -1,   868,   869,    -1,    -1,   872,   873,
      -1,   619,    -1,  1985,    -1,    -1,   880,   348,    -1,   350,
     351,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2001,
      -1,    -1,    -1,   364,    -1,    -1,   644,   368,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   919,   920,  2029,  2030,  1902,
      -1,    -1,    -1,    -1,    -1,    84,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2052,   100,    -1,    -1,    -1,    -1,    -1,   951,    -1,    -1,
      -1,  2063,    -1,    -1,    -1,    -1,    -1,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   715,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   991,    -1,    -1,
      -1,   150,    -1,    92,    -1,   154,   467,    -1,    -1,    -1,
      -1,    -1,   473,    -1,    -1,    -1,    -1,   166,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   190,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,   203,    -1,   145,   206,  2031,    -1,
      -1,    -1,    -1,    -1,  1058,    -1,    -1,    -1,    -1,    -1,
      -1,   160,    -1,    -1,    -1,    -1,    -1,    -1,   539,    -1,
      -1,  1075,  1076,    -1,    -1,    -1,    -1,    -1,  1082,    -1,
      -1,    -1,    -1,    -1,    -1,   556,   834,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   843,    -1,    -1,    -1,    -1,
     259,   849,    -1,    -1,    -1,    -1,  2089,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,    -1,   590,
    1124,    -1,   221,    -1,  1128,  1129,    -1,  1131,  1132,    -1,
    2113,  1135,  1136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   613,   614,    -1,  2128,   894,    -1,    -1,    -1,
     898,    -1,    -1,    -1,   902,    -1,    -1,    -1,    -1,   318,
      -1,   632,   633,    -1,   635,    -1,    -1,    -1,    -1,   328,
     269,   270,   643,    -1,    -1,   923,    -1,    -1,    -1,    -1,
      -1,   280,    -1,    -1,    -1,   656,    -1,    -1,   659,   348,
      -1,   350,    -1,    -1,    -1,   666,   295,    -1,   669,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     329,    -1,    48,   981,    -1,    -1,    -1,   336,   337,    -1,
      -1,    -1,   341,    -1,   403,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1271,    -1,    -1,
      -1,    -1,    -1,  1277,  1278,    -1,    -1,    -1,   437,    -1,
     379,    -1,    -1,   382,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   768,  1302,    -1,
      -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,   467,    -1,
      -1,    -1,    -1,   784,   473,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,    -1,
      -1,    -1,   803,    -1,    -1,    -1,    -1,    -1,   809,    -1,
      -1,   812,  1346,  1347,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1355,  1356,  1101,  1358,  1103,    -1,    -1,    -1,    -1,
     831,   832,    -1,    -1,    -1,  1369,  1370,    -1,    -1,    -1,
      -1,   470,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   850,
      -1,    -1,    -1,   482,   483,    -1,    -1,    -1,    -1,    -1,
      -1,   550,   551,    -1,    -1,    -1,    -1,   556,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1163,    -1,    -1,    -1,    -1,
    1168,   237,   238,    -1,    -1,   241,    -1,    -1,   244,   245,
      -1,   247,    -1,   249,    -1,    -1,    -1,    -1,    -1,   910,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   919,    -1,
      -1,    -1,    -1,    -1,    -1,   614,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   938,    -1,   940,
     941,    -1,    -1,    -1,   633,    -1,   635,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   656,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1510,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     991,  1269,   681,    -1,    -1,    -1,    -1,   626,    -1,   345,
     346,    -1,    -1,    -1,  1538,   694,    -1,    -1,   697,   698,
      -1,   700,    -1,    -1,   360,    -1,    -1,    -1,  1296,    -1,
     709,    -1,    -1,   712,   713,   714,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   676,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1064,    -1,    -1,    -1,  1068,    -1,    -1,
      -1,    -1,    -1,    -1,  1075,    -1,    -1,    -1,    -1,   768,
      -1,    -1,    -1,    -1,  1085,    -1,    -1,    -1,    -1,    -1,
      -1,  1092,    -1,  1627,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   792,    -1,    -1,    -1,    -1,    -1,  1643,
     456,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     809,    -1,    -1,  1124,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1137,    -1,    -1,    -1,
    1141,    -1,   831,   832,  1145,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   850,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   527,    -1,    -1,    -1,    -1,  1720,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   826,    -1,    -1,
      -1,    -1,    -1,    -1,   833,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1494,    -1,    -1,  1497,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     919,    -1,    -1,    -1,    -1,    -1,    -1,   926,    -1,  1773,
    1774,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   940,    -1,    -1,    -1,    -1,  1790,  1791,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1271,    -1,  1806,    -1,  1275,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   913,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1301,  1302,   991,     5,    -1,   651,   652,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,  1594,  1595,   664,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1620,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1369,  1370,
      72,    73,    -1,    -1,    -1,    -1,    -1,  1911,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1919,    -1,    -1,  1389,    -1,
      -1,    -1,  1393,    -1,    -1,    -1,  1397,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,  1124,    -1,    -1,    -1,    -1,
      -1,    -1,   788,   789,    -1,   166,  1724,  1725,   794,    -1,
      -1,  1985,   154,  1987,  1988,   157,   158,  1991,    -1,    -1,
      -1,    -1,   164,   165,   166,   167,   168,   169,   170,   815,
      -1,    -1,   818,   819,    -1,   821,    -1,   823,   824,    -1,
      -1,    -1,   203,    -1,    -1,    -1,    -1,  1176,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2032,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1510,
      -1,    -1,    -1,    -1,  1515,    -1,    -1,    -1,   864,    -1,
      -1,  1210,   868,    -1,    -1,    -1,   872,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1241,    -1,    -1,    -1,    -1,  2091,  2092,    -1,
      -1,  1190,  1191,  1192,    -1,    -1,    -1,    -1,  1569,    -1,
      -1,  1260,    -1,  1851,    -1,  1576,   297,    -1,    -1,  1580,
      -1,    -1,  1271,    -1,    -1,    -1,    -1,    -1,    -1,   935,
     936,    -1,    -1,    -1,  1223,    -1,    -1,    -1,  2132,    -1,
      -1,    -1,    -1,   949,    -1,    -1,    -1,  1885,    -1,    -1,
      -1,    -1,    -1,  1302,    -1,    -1,  1245,  1306,    -1,    -1,
      -1,    -1,  1900,    -1,    -1,    -1,    -1,   348,    -1,   350,
     351,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   364,    -1,    -1,  1924,   368,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1289,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1951,    -1,    -1,    -1,  1955,    -1,    -1,
    1369,  1370,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1720,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1417,  1418,
    1419,    -1,    -1,  1422,  1423,  1081,    -1,    -1,    -1,    -1,
    1429,    -1,    -1,    -1,    -1,    -1,   467,    -1,    -1,    -1,
      -1,    -1,   473,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1773,  1774,    -1,    -1,  1122,    -1,    -1,    84,
      -1,    -1,  1128,    -1,    -1,  1131,    -1,    -1,   166,  1135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1488,
      -1,    -1,    -1,  1432,  1433,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   539,    -1,
      -1,  1510,    -1,    -1,    -1,   203,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   556,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   590,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   190,    -1,    -1,    -1,    -1,
      -1,    -1,   613,   614,    -1,    -1,    -1,   202,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   632,   633,    -1,   635,    -1,    -1,    -1,    -1,   297,
      -1,    -1,   643,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1277,    -1,    -1,    -1,   656,    -1,    -1,   659,    -1,
    1286,  1287,    -1,    -1,  1633,   666,    -1,    -1,   669,    -1,
      -1,    -1,   257,    -1,   259,  1956,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     348,    -1,   350,   351,    -1,    -1,  1665,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1985,    -1,   364,    -1,    -1,    -1,
     368,    -1,    -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,
    1346,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1355,
      -1,    -1,  1358,    -1,  1360,  1361,    -1,    -1,  1647,    -1,
      -1,    -1,    -1,   328,    -1,    -1,    -1,    -1,    -1,  2030,
      -1,  1720,  2033,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   768,    -1,    -1,
      -1,  2052,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1405,
      -1,    -1,    -1,   784,    -1,    -1,  1755,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,    -1,
      -1,    -1,   803,    -1,  1773,  1774,    -1,    -1,   809,   467,
      -1,   812,    -1,    -1,    -1,   473,    -1,    -1,   403,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     831,   832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   850,
      -1,    -1,   437,    -1,  1823,    -1,    -1,    -1,    -1,    -1,
      -1,   190,    -1,  1489,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   203,    -1,    -1,    -1,   463,    -1,
      -1,   539,    -1,    -1,    -1,    -1,    -1,   216,    -1,   218,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   556,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   910,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   919,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   590,    -1,    -1,    -1,    -1,   938,    -1,   940,
     941,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1575,
      -1,    -1,    -1,    -1,    -1,   613,   614,    -1,    -1,  1868,
      -1,    -1,    -1,    -1,    -1,   550,   551,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   632,   633,    -1,   635,  1604,    -1,
      -1,    -1,    -1,    -1,    -1,   643,    -1,  1956,   317,    -1,
     991,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   656,    -1,
      -1,   659,    -1,    -1,    -1,    -1,    -1,    -1,   666,    -1,
      -1,   669,    -1,    -1,    -1,    -1,  1985,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,  2030,    -1,  1064,  2033,    -1,    -1,  1068,    -1,    -1,
      -1,    -1,    -1,    -1,  1075,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1085,    -1,    -1,    -1,    -1,    -1,
      -1,  1092,    -1,  1719,    -1,    -1,   681,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   694,
     768,    -1,   697,   698,    -1,   700,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1124,   709,    -1,   784,   712,   713,   714,
      -1,    -1,    -1,    -1,    -1,    -1,  1137,    -1,    -1,    -1,
    1141,   799,    -1,    -1,  1145,   803,    -1,    -1,    -1,    -1,
      -1,   809,    -1,    -1,   812,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   493,    -1,    -1,    -1,    -1,    -1,
     499,  1797,  1798,   831,   832,   504,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1810,    -1,    -1,    -1,    -1,     3,
      -1,     5,   850,    -1,    -1,    -1,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,   792,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,   910,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,   919,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1271,    -1,    -1,    -1,  1275,   604,    -1,    -1,    -1,    -1,
     938,    -1,   940,   941,    -1,    -1,    -1,  1913,    -1,    -1,
      -1,   105,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
    1301,  1302,    -1,    -1,   633,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   646,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   991,    -1,    -1,    -1,    -1,    -1,   153,
     925,   926,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
      -1,   165,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1987,    -1,    -1,    -1,    -1,    -1,    -1,  1369,  1370,
      -1,    -1,    -1,   702,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1389,    -1,
      -1,    -1,  1393,    -1,    -1,    -1,  1397,   726,   727,    -1,
      -1,   730,    -1,   732,    -1,    -1,  1064,    -1,    -1,   738,
    1068,   740,   741,    -1,    -1,    -1,    -1,  1075,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1085,    -1,    -1,
      -1,    -1,    -1,    -1,  1092,    -1,    -1,    -1,    -1,   768,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   781,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   792,    -1,    -1,  1124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   806,    -1,  1137,
     809,    -1,    -1,  1141,    -1,    -1,    -1,  1145,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   836,    -1,  1510,
     839,    -1,    -1,    -1,  1515,    13,    14,    15,    16,    17,
      -1,    -1,    20,   852,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,   881,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1569,    -1,
      -1,    -1,    -1,    -1,    72,  1576,    -1,    -1,    -1,  1580,
      -1,    -1,  1167,    -1,     5,    -1,    -1,    -1,    -1,    -1,
      -1,  1176,    13,    14,    15,    16,    17,   926,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   940,   941,  1271,    -1,    -1,    -1,  1275,    -1,   948,
      -1,    -1,    -1,    -1,    -1,  1210,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1301,  1302,    -1,    -1,    -1,   977,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   991,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     999,    -1,    -1,    -1,    -1,  1260,    -1,  1006,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
      -1,  1369,  1370,   134,    -1,    -1,    -1,    -1,    -1,  1720,
      -1,  1306,    -1,  1052,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1389,    -1,   154,    -1,  1393,   157,   158,    -1,  1397,
      -1,    -1,    -1,   164,   165,   166,   167,   168,   169,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1773,  1774,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1119,    -1,  1121,    -1,  1123,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1417,  1418,  1419,  1420,  1421,  1422,  1423,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1510,    -1,    -1,    -1,    -1,  1515,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,  1204,  1205,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,  1569,    55,    -1,    -1,    -1,    -1,    -1,  1576,    -1,
      -1,    -1,  1580,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1275,    -1,    -1,    -1,
      -1,  1536,  1281,    -1,    -1,  1956,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,  1302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1985,    -1,    -1,    -1,    -1,    -1,
    1319,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1334,    -1,    -1,  1337,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,    -1,   165,   166,    -1,    -1,    -1,    -1,    -1,  2030,
      -1,    -1,  2033,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1633,    -1,
      -1,  2052,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1720,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1409,  1410,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1435,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1773,  1774,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1453,    -1,    -1,  1456,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1723,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1755,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1510,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1519,  1520,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1530,    49,    -1,    -1,    52,    -1,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1549,    -1,  1551,    -1,    71,    -1,    73,    74,    -1,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,    -1,   101,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1956,    -1,
      -1,    -1,    -1,    -1,  1633,    -1,   153,   154,    -1,  1638,
     157,   158,    -1,    -1,    -1,   162,    -1,   164,   165,   166,
     167,   168,   169,   170,    -1,    -1,    -1,  1985,    -1,    -1,
      -1,    -1,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1697,    -1,
      18,    -1,  2030,    -1,    -1,  2033,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2052,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,  1753,    73,    74,  1756,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,    -1,   101,  1783,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,     1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   153,   154,    -1,    -1,   157,
     158,    -1,    -1,    -1,   162,    -1,   164,   165,   166,   167,
     168,   169,   170,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,   179,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    73,
      74,    -1,    76,    -1,    -1,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,    -1,   101,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,    -1,    -1,   157,   158,  1964,    -1,    -1,   162,    -1,
     164,   165,   166,   167,   168,   169,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   179,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    71,    72,    73,    74,    -1,    76,
      -1,    -1,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,    -1,    -1,
     157,   158,    -1,    -1,    -1,   162,    -1,   164,   165,   166,
     167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   179,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    71,    72,    73,    74,    -1,    76,    -1,    -1,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   154,    -1,    -1,   157,   158,    -1,
      -1,    -1,   162,    -1,   164,   165,   166,   167,   168,   169,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,   149,   150,    -1,    -1,
      -1,   154,   155,   156,   157,   158,    -1,    -1,    -1,    -1,
      -1,   164,   165,   166,   167,   168,   169,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   179,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   148,   149,   150,    -1,    -1,    -1,   154,   155,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   179,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,    -1,   156,   157,   158,    -1,    -1,    -1,    -1,
      -1,   164,   165,   166,   167,   168,   169,   170,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
      -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,
     165,   166,   167,   168,   169,   170,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,   157,
     158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,
     168,   169,   170,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,    -1,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,   164,   165,   166,   167,   168,   169,   170,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,
     167,   168,   169,   170,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      77,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,    -1,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,    -1,    -1,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,    -1,   165,   166,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,   162,
      -1,    -1,   165,   166,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    72,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,   106,    53,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,   158,
      -1,   106,    -1,   108,   109,    -1,   165,   166,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    69,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    77,    78,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,    -1,
      -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,
     165,   166,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,    -1,    -1,    -1,   157,   158,    -1,     3,
      -1,     5,    -1,    -1,   165,   166,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,
      -1,    -1,    -1,   157,   158,    -1,     3,    -1,     5,    -1,
      -1,   165,   166,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,    -1,    -1,    -1,
     157,   158,    -1,     3,    -1,     5,    -1,    -1,   165,   166,
      10,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,    -1,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,    -1,   165,   166,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,   156,
     157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,   155,    -1,   157,   158,    -1,    -1,    -1,   162,    -1,
      -1,   165,   166,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    72,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,   106,    53,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,   155,    -1,   157,   158,    -1,   106,
      -1,   108,   109,    -1,   165,   166,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    13,    14,
      15,    16,    17,    18,    72,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
     108,   109,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,   156,   157,
     158,    -1,    -1,   108,   109,    -1,    -1,   165,   166,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,   157,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    13,    14,    15,    16,    17,    18,    72,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,   108,   109,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
      -1,   156,   157,   158,    -1,    -1,   108,   109,    -1,    -1,
     165,   166,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,   157,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   156,   157,   158,    -1,    -1,    -1,
      -1,    -1,    -1,   165,   166,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,   158,
      -1,    -1,    -1,    -1,    -1,    -1,   165,   166,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,
     166,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,    -1,   165,   166,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,    -1,   165,   166,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,    72,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,   108,   109,    -1,    -1,    -1,    -1,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,
      -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
      -1,   165,   166,    71,    -1,    73,    74,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    -1,    -1,    93,    94,    95,    96,    97,
      98,    99,    -1,   101,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    18,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    49,   157,
     158,    52,    -1,    54,   162,    56,   164,   165,   166,   167,
     168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,   179,    73,    74,    -1,    76,    -1,    -1,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      -1,    -1,    93,    94,    95,    96,    97,    98,    99,    -1,
     101,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,    -1,    -1,   157,   158,    -1,    -1,
      -1,   162,    -1,   164,   165,   166,   167,   168,   169,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,   157,    51,    -1,    53,    -1,   162,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
     157,    51,    -1,    53,    -1,   162,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,   108,   109,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,
      -1,    90,    -1,    92,    -1,    -1,    -1,   157,    -1,    -1,
      -1,    -1,   162,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,   157,   158,
      -1,    -1,    -1,   162,    -1,   164,   165,   166,   167,   168,
     169,   170,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,    -1,    -1,   157,   158,    -1,    -1,
      -1,   162,    -1,   164,   165,   166,   167,   168,   169,   170,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,    -1,   156,   157,   158,    -1,    -1,    -1,    -1,
      -1,   164,   165,   166,   167,   168,   169,   170,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
      -1,    -1,   157,   158,    -1,    -1,    -1,   162,    -1,   164,
     165,   166,   167,   168,   169,   170,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,
     167,   168,   169,   170,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,   157,   158,
      -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,
     169,   170,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,    -1,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,   164,   165,   166,   167,   168,   169,   170,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    72,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   108,   109,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   156,   157,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
     155,    -1,   157,   158,    -1,    -1,    -1,   162,    -1,    -1,
     165,   166,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,   155,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,    -1,   165,   166,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,   156,   157,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,
     166,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   136,    -1,    -1,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,   157,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    13,
      14,    15,    16,    17,    18,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,   108,   109,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,   157,   158,    -1,    -1,    -1,    -1,    -1,
      -1,   165,   166,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    78,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,    -1,   165,   166,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      13,    14,    15,    16,    17,    18,    72,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,   108,   109,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,   108,   109,    -1,    -1,   165,
     166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,    -1,   165,   166,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    13,    14,    15,
      16,    17,    18,    72,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   108,
     109,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,    -1,   108,   109,    -1,    -1,   165,   166,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,
     166,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    13,    14,    15,    16,    17,    18,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,   108,   109,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,   158,    -1,    -1,   108,
     109,    -1,    -1,   165,   166,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,    -1,    -1,    -1,    -1,    -1,   165,   166,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    18,    72,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,   108,   109,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,    -1,   165,   166,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    13,    14,    15,
      16,    17,    18,    72,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   108,
     109,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,    -1,   108,   109,    -1,    -1,   165,   166,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,
     166,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,    13,    14,
      15,    16,    17,   165,   166,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,    72,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,   108,   109,    -1,    -1,    -1,    -1,
     165,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   165,   166,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,
     167,   168,   169,   170,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,   157,
      53,    -1,    55,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    49,    -1,    -1,    52,    -1,
      54,   134,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   148,   149,   150,    -1,    73,
      -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,   164,   165,   166,   167,   168,   169,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    49,    -1,    -1,    52,    -1,    54,
     134,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   148,   149,   150,    -1,    73,    -1,
     154,   155,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,
     155,    -1,   157,   158,    -1,    -1,    -1,   162,    -1,   164,
     165,   166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   154,    -1,    -1,   157,   158,
      -1,    -1,    -1,   162,    -1,   164,   165,   166,   167,   168,
     169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,   164,   165,   166,   167,   168,   169,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   154,    -1,    -1,
     157,   158,    -1,    -1,    -1,   162,    -1,   164,   165,   166,
     167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   154,   155,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,   164,   165,   166,   167,   168,   169,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,
      -1,    -1,   157,   158,    -1,    -1,   161,    -1,    -1,   164,
     165,   166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,   158,
      -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,
     169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   154,    -1,    -1,   157,   158,    -1,    -1,    -1,   162,
      -1,   164,   165,   166,   167,   168,   169,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   154,    -1,    -1,
     157,   158,    -1,    -1,    -1,   162,    -1,   164,   165,   166,
     167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   154,    -1,   156,   157,   158,    -1,    -1,
      -1,    -1,    -1,   164,   165,   166,   167,   168,   169,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,
     155,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,
     165,   166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,   158,
      -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,
     169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,   164,   165,   166,   167,   168,   169,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   154,   155,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,
     167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   154,   155,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,   164,   165,   166,   167,   168,   169,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,
     155,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,
     165,   166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,   158,
      -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,
     169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,   164,   165,   166,   167,   168,   169,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   154,   155,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,
     167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   154,   155,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,   164,   165,   166,   167,   168,   169,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,
     155,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,
     165,   166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,   158,
      -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,
     169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,   164,   165,   166,   167,   168,   169,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   154,   155,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,
     167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   154,   155,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,   164,   165,   166,   167,   168,   169,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,
      -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,
     165,   166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   154,    -1,    -1,   157,   158,
      -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,
     169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   154,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,   164,   165,   166,   167,   168,   169,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   154,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,
     167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   154,    -1,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,   164,   165,   166,   167,   168,   169,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,
      -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,
     165,   166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,   157,   158,
      -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,
     169,   170
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   181,   405,   406,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    69,    72,    73,
     101,   105,   106,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   121,   134,   154,   155,   157,   158,   165,
     166,   184,   185,   186,   201,   293,   294,   295,   296,   297,
     298,   299,   300,   301,   302,   303,   304,   307,   310,   312,
     313,   314,   315,   316,   317,   318,   319,   320,   322,   324,
     325,   326,   328,   329,   333,   334,   335,   336,   337,   339,
     345,   346,   347,   348,   359,   363,   397,   400,   410,   416,
     418,   424,   428,   433,   434,   435,   436,   437,   438,   439,
     440,   466,   484,   485,   486,   487,     0,   181,   106,   185,
     201,   297,   299,   310,   313,   316,   325,   329,   334,   120,
     154,    58,    61,    62,    64,   154,   154,   422,   423,   424,
     321,   322,   108,   109,   185,   377,   398,   399,   377,   154,
     410,   154,   154,     4,   106,   108,   109,   314,   319,   320,
     154,   201,   423,   428,   434,   435,   436,   438,   439,   440,
     108,   336,   159,   181,   300,   310,   313,   433,   437,   483,
     484,   487,   488,   179,   182,   151,   162,   178,   222,   380,
      89,   160,   417,   377,   182,   160,   160,   179,   108,   109,
     154,   201,   305,   306,   428,   429,   430,   431,   432,   433,
     437,   441,   442,   443,   444,   445,   446,   447,   448,   449,
     455,     3,    47,    48,    50,    55,   327,     3,   158,   201,
     299,   314,   318,   320,   330,   335,   413,   433,   437,   487,
      69,   297,   299,   313,   325,   329,   334,   414,   433,   437,
      65,   319,   319,   314,   320,   308,   319,   320,   327,   346,
     314,   319,   314,   157,   422,   160,   182,   154,   162,   230,
     422,   422,     3,   288,   289,   304,   307,   313,   317,   318,
     158,   310,   313,   485,   377,   377,   410,   178,   313,   154,
     201,   419,   428,   429,   433,   442,   446,   158,   201,   487,
     411,   412,    65,    66,    67,    68,   158,   176,   377,   386,
     388,   392,   394,   395,   335,    57,   156,   158,   201,   309,
     313,   317,   324,   325,   331,   332,   333,   334,   338,   345,
     346,   363,   373,   375,   466,   479,   480,   481,   482,   487,
     488,   108,   109,   162,   169,   185,   335,   455,   424,   154,
     393,   394,   154,   154,   120,   187,   188,    49,    52,    54,
      56,    73,   103,   104,   106,   107,   118,   119,   122,   123,
     124,   126,   127,   154,   158,   164,   167,   168,   169,   170,
     183,   184,   187,   189,   192,   200,   201,   202,   203,   206,
     207,   208,   209,   210,   211,   212,   213,   214,   215,   216,
     217,   218,   224,   335,   156,   158,   200,   201,   217,   219,
     310,   335,   378,   379,   396,   483,   488,   313,   434,   435,
     436,   438,   439,   440,   156,   156,   156,   156,   156,   156,
     156,   158,   310,   466,   485,   158,   165,   201,   219,   299,
     300,   309,   311,   313,   325,   332,   334,   368,   369,   372,
     373,   374,   479,   487,   154,   433,   437,   487,   154,   160,
     106,   157,   158,   162,   184,   186,   219,   381,   382,   383,
     384,   385,    22,   381,   154,   377,   230,   154,   160,   419,
     185,   423,   428,   430,   431,   432,   441,   443,   444,   445,
     447,   448,   449,   313,   429,   442,   446,   160,   101,   421,
     158,   422,   463,   466,   421,   422,   422,   417,   288,   154,
     422,   463,   421,   422,   422,   417,   422,   422,   313,   419,
     154,   154,   312,   313,   310,   313,   181,   310,   483,   488,
     337,   162,   417,   288,   377,   377,   380,   299,   318,   415,
     433,   437,   162,   417,   288,   398,   313,   325,   313,   313,
     108,   336,   108,   109,   185,   335,   340,   398,   136,   185,
     313,   370,   371,   374,   375,   376,   153,   181,   230,   304,
     179,   433,   446,   313,   181,   421,   154,   421,   182,   219,
     423,   428,   313,   154,   181,   377,   408,   162,   377,   162,
     377,   136,   165,   166,   391,   156,   160,   377,   395,   156,
     422,   422,   159,   181,   311,   313,   325,   332,   334,   478,
     479,   487,   488,   154,   158,   166,   178,   201,   466,   468,
     469,   470,   471,   472,   473,   490,   201,   338,   487,   313,
     332,   319,   314,   422,   156,   311,   313,   480,   311,   466,
     480,    10,   164,   169,   362,   364,   365,   162,   360,   362,
     386,   178,   386,    13,    88,   106,   108,   109,   184,   425,
     426,   427,   156,   120,   154,   200,   154,   154,   154,   203,
     154,   200,   154,   106,   108,   109,   314,   319,   320,   154,
     200,   200,    19,    21,    85,   158,   167,   168,   204,   205,
     219,   226,   230,   348,   378,   487,   160,   181,   154,   189,
     158,   163,   158,   163,   123,   125,   126,   127,   154,   157,
     158,   162,   163,   203,   203,   171,   165,   172,   173,   167,
     168,   128,   129,   130,   131,   174,   175,   132,   133,   166,
     164,   176,   134,   135,   177,   156,   160,   157,   181,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     178,   221,   222,   223,   154,   201,   459,   460,   461,   462,
     463,   156,   160,   156,   156,   156,   156,   156,   156,   154,
     422,   463,   466,   154,   463,   466,   181,   310,   485,   181,
     182,   182,   154,   166,   201,   428,   450,   451,   452,   453,
     454,   455,   456,   457,   458,   136,   487,   182,   182,   377,
     377,   181,   181,   181,   158,   186,   181,   382,   161,   160,
     489,   381,   157,   158,   161,   385,   155,   219,   225,   154,
     181,   181,   178,   428,   430,   431,   432,   441,   443,   444,
     445,   447,   448,   449,   156,   156,   156,   156,   156,   156,
     156,   156,   156,   156,   429,   442,   446,   422,   154,   178,
     159,   181,   380,   230,   417,   370,   380,   230,   419,   226,
     379,   226,   379,   419,   408,   230,   417,   421,   162,   162,
     417,   288,   408,   230,   417,   342,   343,   341,   162,   156,
     160,   156,   160,    70,   290,   291,   179,   165,   219,   181,
     428,   369,   410,   408,   159,   181,   154,   390,   388,   389,
      78,   323,   185,   162,   169,   185,   455,   311,   466,   480,
     313,   317,   487,   370,   469,   470,   471,   159,   181,    18,
     219,   313,   468,   490,   422,   422,   466,   311,   478,   488,
     313,   185,   422,   311,   480,   335,   160,   489,   377,   364,
     362,   162,   156,   379,   156,   156,   160,   154,   179,   378,
     189,   158,   378,   378,   378,   219,   378,   156,   378,   378,
     378,   181,   156,   167,   168,   205,    18,   315,   156,   160,
     156,   165,   166,   156,   225,   219,   162,   219,   185,   219,
     185,   118,   158,   185,   155,   193,   194,   195,   219,   118,
     158,   185,   348,   219,   193,   185,   203,   206,   206,   206,
     207,   207,   208,   208,   209,   209,   209,   209,   210,   210,
     211,   212,   213,   214,   215,   161,   226,   179,   187,   158,
     185,   219,   162,   219,   370,   460,   461,   462,   313,   459,
     422,   422,   219,   379,   154,   422,   463,   466,   154,   463,
     466,   370,   370,   159,   159,   154,   428,   451,   452,   453,
     456,    18,   313,   450,   454,   154,   422,   472,   490,   422,
     422,   490,   154,   422,   472,   422,   422,   182,   218,   377,
     159,   160,   159,   160,   490,   490,   136,   367,   368,   369,
     367,   377,   181,   217,   218,   219,   420,   489,   381,   383,
     153,   181,   156,   160,   181,   367,   185,   219,   156,   156,
     156,   156,   156,   156,   156,   156,   156,   154,   422,   463,
     466,   154,   422,   463,   466,   154,   422,   463,   466,   419,
     187,    22,   466,   219,   320,   335,   464,   230,   156,   156,
     156,   156,   156,   406,   407,   230,   153,   181,   408,   230,
     417,   407,   230,   162,   162,   162,   349,   136,   374,   375,
     185,   292,   377,    18,    71,    73,    76,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    93,
      94,    95,    96,    97,    98,    99,   101,   108,   109,   121,
     154,   181,   226,   227,   228,   229,   230,   231,   232,   234,
     235,   245,   251,   252,   253,   254,   255,   256,   261,   262,
     265,   266,   267,   268,   269,   270,   271,   277,   278,   279,
     293,   313,   317,   377,   418,    70,   182,   182,   367,   182,
     409,   407,   297,   299,   310,   401,   402,   403,   404,   396,
     178,   387,   387,   364,   162,   422,   422,   311,   480,   158,
     165,   201,   219,   335,   219,   313,   156,   156,   156,   156,
       5,   313,   422,   468,   162,   169,   185,   455,    10,   365,
     153,   178,   366,   489,   162,   364,   162,   156,   426,   193,
     156,   160,   181,   160,   156,   156,   160,   156,   203,   156,
     156,   156,   203,    18,   315,   219,   156,   156,   155,   162,
     203,   159,   182,   193,   159,   159,   118,   122,   124,   186,
     196,   197,   198,   156,   160,   196,   159,   160,   153,   217,
     161,   156,   196,   182,   382,   156,   156,   156,   156,   459,
     370,   370,   156,   156,   456,   156,   156,   156,   156,   154,
     428,   455,   450,   454,   370,   370,   159,   182,   490,   181,
     181,   182,   182,   182,   182,   380,   196,   136,   170,   182,
     182,   153,   381,   219,   422,   155,   219,   367,   182,   154,
     422,   463,   466,   154,   422,   463,   466,   154,   422,   463,
     466,   370,   370,   370,   421,   156,   148,   170,   182,   465,
     160,   182,   409,   401,   407,   230,   409,   349,   349,   349,
       3,     5,    10,    73,   153,   294,   301,   302,   310,   313,
     350,   355,   483,   160,   179,   154,    61,    62,   179,   230,
     293,   418,   154,    18,   228,   154,   154,   179,   377,   179,
     377,   165,   377,   162,   227,   154,   154,   154,   228,   154,
     230,   219,   220,   220,    14,   280,   256,   267,    74,   236,
     179,   182,   232,    78,   179,   377,    91,    92,   260,   264,
     112,   135,   259,   111,   134,   263,   259,   376,   313,   161,
     292,   159,   159,   182,   160,   409,   419,   182,   179,   182,
     179,   182,   156,   379,   393,   393,   489,   364,   362,   362,
     181,   182,   182,   182,   219,   154,   422,   472,   466,   312,
       5,   165,   182,   219,   364,   162,   422,   422,   335,   377,
     162,   218,   153,   364,   489,   153,   181,   156,   309,   185,
      78,   190,   191,   378,   203,   203,   203,   203,   203,   162,
     382,   160,   153,   199,   158,   197,   199,   199,   159,   160,
     125,   157,   195,   159,   225,   217,   179,   159,   489,   154,
     422,   463,   466,   156,   156,   156,   154,   422,   463,   466,
     154,   422,   472,   428,   422,   422,   156,   156,   159,   369,
     372,   372,   373,   156,   160,   160,   156,   182,   218,   218,
     159,   159,   182,   182,   156,   370,   370,   370,   156,   156,
     156,   380,   422,   160,   219,   219,   320,   335,   159,   153,
     182,   409,   153,   153,   153,   153,   310,   310,   348,   356,
     483,   310,   355,   154,   344,   179,   179,   154,   161,   201,
     351,   352,   358,   428,   429,   442,   446,   160,   179,   377,
     377,   193,   179,   230,   179,   230,   226,    80,   156,   226,
     237,   293,   295,   298,   304,   313,   317,   148,   149,   150,
     155,   156,   179,   226,   246,   247,   248,   293,   179,   179,
     226,   179,   382,   179,   226,   225,   226,   246,   113,   114,
     115,   116,   117,   272,   274,   275,   179,   100,   179,    84,
     154,   156,   154,   182,   153,   179,   179,   154,   154,   228,
     228,   256,   154,   266,   256,   266,   230,   422,   179,   156,
     181,   153,   153,   181,   160,   160,   153,   489,   162,   162,
     159,   159,   159,   182,   370,   219,   219,   182,   159,   182,
     489,   364,   361,   362,   366,   366,   382,   489,   153,   401,
     467,   468,   156,   161,   156,   160,   161,   382,   489,   225,
     123,   196,   197,   158,   197,   158,   197,   159,   153,   370,
     370,   370,   182,   181,   181,   159,   182,   156,   422,   156,
     156,   156,   226,   465,   153,   153,   344,   344,   344,   351,
     154,   201,   353,   354,   463,   474,   475,   476,   477,   179,
     160,   179,   351,   179,   396,   423,   428,   219,   313,   153,
     160,   179,   357,   358,   357,   357,   377,   156,   156,   154,
     228,   156,   226,   313,   148,   149,   150,   170,   179,   249,
     250,   228,   227,   179,   250,   156,   161,   226,   155,   226,
     227,   248,   179,   489,   156,   156,   156,   156,   230,   274,
     275,   154,   219,   154,   187,   237,   203,   257,   226,    75,
     110,   258,   260,    75,     1,   228,   422,   402,   181,   181,
     153,   364,   364,   159,   156,   182,   182,   159,   159,   153,
     489,   362,   162,   489,   153,   182,   156,   219,   191,   219,
     489,   153,   159,   159,   196,   196,   156,   156,   156,   159,
     160,   136,   369,   136,   159,   159,   219,   179,   475,   476,
     477,   313,   474,   160,   179,   422,   422,   179,   156,   428,
     422,   228,    77,    78,   162,   240,   241,   242,   156,   226,
      75,   228,   226,   155,   226,    75,   179,   108,   155,   226,
     227,   248,   155,   226,   228,   247,   250,   250,   179,   226,
     153,   162,   242,   228,   228,   154,   181,   179,   187,   156,
     161,   156,   156,   160,   161,   156,   228,   154,   228,   228,
     228,   377,   419,   489,   489,   159,   159,   153,   162,   364,
     153,   153,   153,   159,   159,   181,   182,   156,   156,   156,
     474,   422,   352,     1,   218,   238,   239,   420,     1,   161,
       1,   181,   228,   240,    75,   179,   156,   228,    75,   179,
     170,   170,   228,   227,   250,   250,   179,   108,   226,   170,
     170,    75,   155,   226,   155,   226,   227,   179,     1,   181,
     181,   276,   311,   313,   483,   161,   179,   158,   187,   281,
     282,   283,   228,   203,   193,   226,   259,   153,   153,   364,
     489,   372,   154,   422,   463,   466,   354,   136,     1,   160,
     161,   153,   286,   287,   293,   228,    75,   179,   228,   226,
     155,   155,   226,   155,   226,   155,   226,   227,   155,   226,
     155,   226,   228,   170,   170,   170,   170,   153,   286,   276,
     182,   154,   201,   419,   474,   185,   161,   106,   154,   156,
     161,   160,    75,   156,   156,    75,   255,   489,   153,   370,
     218,   238,   241,   243,   244,   293,   228,   170,   170,   170,
     170,   155,   155,   226,   155,   226,   155,   226,   243,   182,
     179,   273,   313,   281,   159,   218,   179,   281,   283,   228,
     228,    75,   153,   156,   228,   233,   182,   241,   155,   155,
     226,   155,   226,   155,   226,   182,   273,   217,   156,   161,
     187,   156,   156,   161,   228,     1,   228,   153,   233,   153,
     156,   230,   187,   284,   154,   179,   284,   230,   160,   161,
     218,   156,   187,   185,   285,   156,   179,   156,   160,   179,
     185
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   180,   181,   182,   183,   183,   183,   183,   183,   184,
     184,   184,   184,   184,   184,   184,   184,   185,   185,   186,
     186,   187,   188,   188,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   190,
     190,   191,   191,   192,   192,   192,   192,   192,   192,   192,
     192,   192,   192,   192,   192,   192,   192,   192,   192,   192,
     192,   192,   192,   192,   192,   192,   192,   193,   193,   194,
     194,   195,   195,   196,   196,   197,   197,   197,   197,   197,
     197,   197,   198,   198,   198,   199,   199,   200,   200,   200,
     200,   200,   200,   200,   200,   200,   200,   200,   200,   200,
     200,   201,   201,   201,   202,   202,   202,   202,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   204,   204,   204,
     204,   205,   205,   206,   206,   207,   207,   207,   207,   208,
     208,   208,   209,   209,   209,   210,   210,   210,   210,   210,
     211,   211,   211,   212,   212,   213,   213,   214,   214,   215,
     215,   216,   216,   217,   217,   217,   218,   219,   219,   219,
     220,   220,   221,   221,   222,   222,   223,   223,   223,   223,
     223,   223,   223,   223,   223,   223,   223,   224,   224,   225,
     225,   225,   225,   226,   226,   227,   227,   228,   228,   228,
     228,   228,   228,   228,   228,   228,   228,   228,   228,   228,
     228,   228,   228,   229,   229,   230,   230,   231,   231,   232,
     232,   232,   232,   232,   233,   233,   233,   234,   235,   235,
     235,   235,   235,   235,   235,   236,   236,   237,   237,   237,
     237,   238,   238,   238,   239,   239,   240,   240,   240,   240,
     240,   241,   241,   242,   243,   243,   244,   244,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     246,   246,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   248,   248,   248,
     249,   249,   250,   250,   250,   251,   251,   251,   251,   251,
     251,   251,   251,   251,   251,   251,   251,   251,   251,   251,
     251,   251,   251,   251,   251,   252,   252,   253,   254,   255,
     256,   256,   257,   257,   258,   259,   259,   260,   260,   261,
     261,   261,   261,   261,   261,   262,   263,   263,   264,   265,
     265,   266,   266,   267,   267,   267,   268,   269,   270,   271,
     271,   271,   272,   272,   273,   273,   274,   274,   274,   274,
     275,   276,   276,   276,   276,   276,   277,   278,   278,   279,
     279,   279,   279,   279,   280,   280,   281,   281,   282,   282,
     283,   283,   284,   284,   284,   285,   285,   286,   286,   287,
     287,   288,   288,   289,   289,   290,   290,   291,   291,   292,
     292,   293,   293,   293,   294,   294,   295,   295,   295,   295,
     295,   296,   296,   296,   297,   297,   297,   298,   298,   298,
     298,   298,   299,   299,   300,   300,   301,   301,   301,   302,
     302,   302,   302,   302,   303,   303,   304,   304,   304,   304,
     305,   305,   305,   305,   305,   306,   306,   307,   307,   307,
     307,   308,   308,   308,   309,   309,   309,   310,   310,   310,
     311,   311,   311,   312,   312,   313,   313,   314,   314,   315,
     315,   315,   315,   315,   316,   317,   317,   317,   318,   318,
     319,   319,   319,   319,   319,   319,   319,   319,   319,   320,
     320,   320,   320,   320,   320,   320,   320,   320,   320,   320,
     320,   320,   320,   320,   320,   320,   320,   320,   320,   320,
     320,   320,   320,   320,   320,   320,   320,   321,   321,   322,
     323,   323,   324,   324,   324,   324,   324,   325,   325,   326,
     326,   326,   326,   327,   327,   327,   327,   327,   327,   328,
     328,   328,   328,   329,   330,   329,   329,   331,   331,   331,
     331,   332,   332,   332,   333,   333,   333,   333,   334,   334,
     334,   335,   335,   335,   335,   335,   335,   336,   336,   336,
     337,   337,   338,   338,   340,   339,   341,   339,   342,   339,
     343,   339,   339,   344,   344,   345,   345,   346,   346,   347,
     347,   347,   348,   348,   348,   348,   348,   348,   348,   348,
     349,   349,   350,   350,   350,   350,   350,   350,   350,   350,
     350,   350,   350,   350,   351,   351,   351,   352,   352,   352,
     352,   353,   353,   353,   354,   355,   355,   356,   356,   357,
     357,   358,   359,   359,   360,   359,   359,   359,   359,   359,
     359,   361,   359,   359,   359,   359,   359,   362,   362,   363,
     363,   364,   364,   364,   364,   365,   365,   366,   366,   366,
     367,   367,   367,   367,   367,   367,   367,   368,   368,   368,
     368,   369,   369,   370,   370,   370,   370,   371,   371,   371,
     371,   372,   372,   372,   372,   372,   373,   373,   373,   373,
     373,   374,   374,   375,   375,   376,   376,   377,   377,   377,
     378,   378,   378,   379,   379,   380,   380,   380,   380,   381,
     381,   382,   382,   382,   382,   382,   383,   383,   384,   384,
     385,   385,   385,   385,   385,   386,   386,   387,   387,   389,
     388,   390,   388,   388,   388,   391,   391,   391,   391,   392,
     392,   392,   392,   393,   393,   394,   394,   395,   395,   396,
     396,   396,   396,   397,   397,   397,   398,   398,   399,   399,
     400,   400,   400,   400,   401,   401,   402,   402,   403,   403,
     403,   404,   404,   405,   405,   406,   406,   407,   407,   408,
     409,   410,   410,   410,   410,   410,   410,   410,   410,   410,
     410,   410,   411,   410,   412,   410,   413,   410,   414,   410,
     415,   410,   416,   416,   416,   417,   417,   418,   418,   418,
     418,   418,   418,   418,   418,   418,   418,   419,   419,   419,
     419,   420,   421,   421,   422,   422,   423,   423,   424,   425,
     425,   426,   426,   426,   427,   427,   427,   427,   427,   427,
     428,   428,   429,   429,   429,   429,   430,   430,   430,   430,
     431,   431,   431,   431,   431,   431,   431,   432,   432,   432,
     432,   433,   433,   433,   434,   434,   434,   434,   434,   435,
     435,   435,   435,   436,   436,   436,   436,   436,   436,   437,
     437,   437,   438,   438,   438,   438,   438,   439,   439,   439,
     439,   440,   440,   440,   440,   440,   440,   441,   441,   442,
     442,   442,   442,   443,   443,   443,   443,   444,   444,   444,
     444,   444,   444,   444,   445,   445,   445,   445,   446,   446,
     446,   447,   447,   447,   447,   447,   448,   448,   448,   448,
     449,   449,   449,   449,   449,   449,   450,   450,   450,   450,
     450,   451,   451,   451,   452,   452,   452,   452,   453,   453,
     453,   454,   454,   454,   454,   454,   455,   455,   456,   456,
     456,   457,   457,   458,   458,   459,   459,   459,   460,   460,
     460,   460,   460,   461,   461,   461,   461,   462,   462,   462,
     463,   463,   463,   463,   463,   464,   464,   464,   464,   464,
     464,   465,   465,   466,   466,   466,   466,   467,   467,   468,
     468,   468,   468,   469,   469,   469,   469,   469,   470,   470,
     470,   470,   471,   471,   471,   472,   472,   472,   473,   473,
     473,   473,   473,   473,   474,   474,   474,   475,   475,   475,
     475,   475,   476,   476,   476,   476,   477,   477,   478,   478,
     478,   479,   479,   480,   480,   480,   480,   480,   480,   481,
     481,   481,   481,   481,   481,   481,   481,   481,   481,   482,
     482,   482,   482,   483,   483,   483,   484,   484,   485,   485,
     485,   485,   485,   485,   486,   486,   486,   486,   486,   486,
     487,   487,   487,   488,   488,   489,   489,   490,   490
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     1,     3,     3,
       3,     5,     6,     2,     2,     2,     2,     2,     2,     1,
       3,     3,     3,     1,     6,     4,     4,     4,     4,     4,
       7,     3,     3,     3,     3,     3,     2,     5,     3,     3,
       3,     5,     2,     2,     7,     8,     5,     0,     1,     1,
       3,     1,     1,     1,     3,     1,     2,     4,     3,     5,
       3,     5,     2,     2,     2,     0,     2,     1,     1,     1,
       2,     2,     2,     2,     2,     2,     4,     2,     4,     6,
       4,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       5,     5,     4,     5,     5,     5,     4,     2,     2,     3,
       3,     1,     1,     1,     3,     1,     3,     3,     3,     1,
       3,     3,     1,     3,     3,     1,     3,     3,     3,     3,
       1,     3,     3,     1,     3,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     5,     4,     1,     1,     3,     6,
       0,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     4,     7,     1,
       1,     3,     3,     1,     3,     0,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     4,     4,     2,     6,     1,     2,     1,
       2,     1,     2,     1,     1,     2,     2,     2,     3,     5,
      10,     7,     5,    10,     7,     5,     7,     1,     1,     1,
       2,     1,     3,     1,     1,     3,     2,     3,     3,     2,
       2,     1,     2,     2,     0,     1,     2,     3,     4,     6,
       5,     7,     6,     7,     7,     8,     4,     6,     5,     7,
       1,     3,     4,     5,     4,     3,     5,     1,     2,     3,
       3,     3,     5,     5,     5,     5,     3,     5,     5,     5,
       3,     4,     5,     5,     5,     5,     7,     7,     7,     7,
       7,     7,     7,     2,     3,     4,     4,     4,     6,     6,
       6,     6,     6,     6,     6,     3,     4,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     4,     2,     3,     3,
       2,     3,     2,     3,     3,     6,     2,     2,     3,     3,
       3,     3,     3,     3,     5,     1,     1,     5,     5,     4,
       0,     1,     1,     3,     4,     1,     1,     4,     6,     3,
       5,     5,     5,     8,     9,     1,     1,     1,     4,     3,
       3,     1,     3,     1,     3,     5,     1,     2,     5,     3,
       3,     4,     8,     9,     0,     2,     1,     1,     1,     1,
       2,     1,     2,     2,     2,     1,     3,     1,     1,     6,
       8,    10,    12,    14,     0,     1,     0,     1,     1,     3,
       4,     7,     0,     1,     3,     1,     3,     0,     1,     1,
       2,     0,     1,     2,     3,     0,     1,     3,     4,     1,
       3,     2,     2,     1,     7,     5,     1,     1,     1,     1,
       1,     2,     3,     6,     3,     3,     4,     1,     2,     2,
       3,     8,     8,     8,     5,     9,     2,     2,     5,     3,
       3,     4,     3,     4,     4,     5,     2,     1,     1,     1,
       3,     3,     2,     4,     6,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     4,     1,     2,     3,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     5,
       0,     1,     1,     2,     2,     3,     3,     1,     3,     1,
       2,     2,     2,     4,     4,     4,     4,     1,     1,     1,
       2,     2,     3,     1,     0,     3,     2,     1,     2,     2,
       3,     1,     2,     2,     1,     2,     2,     3,     1,     2,
       2,     1,     2,     3,     1,     2,     3,     1,     3,     4,
       1,     1,     1,     1,     0,     7,     0,     8,     0,     8,
       0,     8,     1,     0,     3,     3,     3,     1,     1,     2,
       1,     1,     1,     2,     1,     2,     1,     2,     1,     2,
       0,     2,     3,     3,     4,     4,     4,     3,     2,     2,
       3,     3,     2,     1,     0,     1,     4,     1,     2,     2,
       2,     0,     1,     4,     1,     2,     3,     1,     2,     0,
       1,     2,     6,     7,     0,     9,     8,     9,    10,     8,
       9,     0,    13,    11,    12,    11,     1,     0,     1,     3,
       3,     3,     2,     5,     5,     1,     1,     0,     2,     5,
       0,     1,     1,     1,     5,     5,     5,     1,     5,     5,
       9,     1,     5,     0,     1,     1,     3,     1,     1,     3,
       3,     1,     3,     3,     4,     1,     1,     1,     1,     2,
       1,     3,     3,     2,     3,     1,     3,     1,     1,     1,
       1,     1,     2,     1,     1,     0,     2,     2,     4,     1,
       4,     0,     1,     2,     3,     4,     2,     2,     1,     2,
       2,     5,     5,     7,     6,     1,     3,     0,     2,     0,
       5,     0,     5,     3,     1,     0,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     1,     2,     5,     6,     1,
       1,     3,     3,     2,     3,     3,     2,     4,     1,     4,
       7,     5,    10,     8,     1,     4,     2,     2,     1,     1,
       5,     2,     5,     0,     1,     3,     4,     0,     1,     0,
       0,     1,     1,     2,     2,     2,     2,     2,     2,     1,
       2,     5,     0,     6,     0,     8,     0,     7,     0,     7,
       0,     8,     1,     2,     3,     0,     5,     3,     4,     4,
       4,     4,     5,     5,     5,     5,     6,     1,     1,     1,
       1,     3,     0,     5,     0,     1,     1,     2,     6,     1,
       3,     0,     1,     4,     1,     1,     1,     1,     1,     1,
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
       5,     6,     7,     6,     6,     0,     1,     0,     2
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
#line 608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7940 "Parser/parser.cc"
    break;

  case 3:
#line 612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7946 "Parser/parser.cc"
    break;

  case 4:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 7952 "Parser/parser.cc"
    break;

  case 5:
#line 620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7958 "Parser/parser.cc"
    break;

  case 6:
#line 621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7964 "Parser/parser.cc"
    break;

  case 7:
#line 622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7970 "Parser/parser.cc"
    break;

  case 8:
#line 623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 7976 "Parser/parser.cc"
    break;

  case 20:
#line 645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7982 "Parser/parser.cc"
    break;

  case 21:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 7988 "Parser/parser.cc"
    break;

  case 22:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7994 "Parser/parser.cc"
    break;

  case 23:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8004 "Parser/parser.cc"
    break;

  case 24:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8010 "Parser/parser.cc"
    break;

  case 25:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8016 "Parser/parser.cc"
    break;

  case 26:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8022 "Parser/parser.cc"
    break;

  case 28:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8028 "Parser/parser.cc"
    break;

  case 29:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8034 "Parser/parser.cc"
    break;

  case 30:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, (yyvsp[-2].decl), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8040 "Parser/parser.cc"
    break;

  case 31:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8046 "Parser/parser.cc"
    break;

  case 32:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8056 "Parser/parser.cc"
    break;

  case 33:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.expr) = nullptr; }
#line 8062 "Parser/parser.cc"
    break;

  case 34:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8068 "Parser/parser.cc"
    break;

  case 35:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8074 "Parser/parser.cc"
    break;

  case 36:
#line 699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8080 "Parser/parser.cc"
    break;

  case 37:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8086 "Parser/parser.cc"
    break;

  case 38:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8092 "Parser/parser.cc"
    break;

  case 40:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8104 "Parser/parser.cc"
    break;

  case 41:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8113 "Parser/parser.cc"
    break;

  case 42:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8119 "Parser/parser.cc"
    break;

  case 44:
#line 734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) )) ) ); }
#line 8125 "Parser/parser.cc"
    break;

  case 45:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8131 "Parser/parser.cc"
    break;

  case 46:
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8137 "Parser/parser.cc"
    break;

  case 47:
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8143 "Parser/parser.cc"
    break;

  case 48:
#line 746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8153 "Parser/parser.cc"
    break;

  case 49:
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8159 "Parser/parser.cc"
    break;

  case 50:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 8166 "Parser/parser.cc"
    break;

  case 51:
#line 758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8172 "Parser/parser.cc"
    break;

  case 52:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8178 "Parser/parser.cc"
    break;

  case 53:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8184 "Parser/parser.cc"
    break;

  case 54:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8190 "Parser/parser.cc"
    break;

  case 55:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8196 "Parser/parser.cc"
    break;

  case 56:
#line 768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8202 "Parser/parser.cc"
    break;

  case 57:
#line 770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8208 "Parser/parser.cc"
    break;

  case 58:
#line 772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8214 "Parser/parser.cc"
    break;

  case 59:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8220 "Parser/parser.cc"
    break;

  case 60:
#line 776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8226 "Parser/parser.cc"
    break;

  case 61:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8232 "Parser/parser.cc"
    break;

  case 62:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8238 "Parser/parser.cc"
    break;

  case 63:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8244 "Parser/parser.cc"
    break;

  case 64:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8250 "Parser/parser.cc"
    break;

  case 65:
#line 786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8256 "Parser/parser.cc"
    break;

  case 66:
#line 788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8266 "Parser/parser.cc"
    break;

  case 67:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8272 "Parser/parser.cc"
    break;

  case 70:
#line 804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8278 "Parser/parser.cc"
    break;

  case 71:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8284 "Parser/parser.cc"
    break;

  case 74:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8290 "Parser/parser.cc"
    break;

  case 76:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8296 "Parser/parser.cc"
    break;

  case 77:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8302 "Parser/parser.cc"
    break;

  case 78:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8308 "Parser/parser.cc"
    break;

  case 79:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8314 "Parser/parser.cc"
    break;

  case 80:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8320 "Parser/parser.cc"
    break;

  case 81:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8326 "Parser/parser.cc"
    break;

  case 82:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8332 "Parser/parser.cc"
    break;

  case 83:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8338 "Parser/parser.cc"
    break;

  case 84:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8346 "Parser/parser.cc"
    break;

  case 85:
#line 848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8352 "Parser/parser.cc"
    break;

  case 86:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8361 "Parser/parser.cc"
    break;

  case 89:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8367 "Parser/parser.cc"
    break;

  case 90:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8373 "Parser/parser.cc"
    break;

  case 91:
#line 869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8393 "Parser/parser.cc"
    break;

  case 92:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8399 "Parser/parser.cc"
    break;

  case 93:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8405 "Parser/parser.cc"
    break;

  case 94:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8411 "Parser/parser.cc"
    break;

  case 95:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8417 "Parser/parser.cc"
    break;

  case 96:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8423 "Parser/parser.cc"
    break;

  case 97:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8429 "Parser/parser.cc"
    break;

  case 98:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8435 "Parser/parser.cc"
    break;

  case 99:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8441 "Parser/parser.cc"
    break;

  case 100:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8450 "Parser/parser.cc"
    break;

  case 101:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 8456 "Parser/parser.cc"
    break;

  case 102:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 8462 "Parser/parser.cc"
    break;

  case 103:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 8468 "Parser/parser.cc"
    break;

  case 104:
#line 915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 8474 "Parser/parser.cc"
    break;

  case 105:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 8480 "Parser/parser.cc"
    break;

  case 106:
#line 917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 8486 "Parser/parser.cc"
    break;

  case 107:
#line 918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 8492 "Parser/parser.cc"
    break;

  case 109:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 8498 "Parser/parser.cc"
    break;

  case 110:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8504 "Parser/parser.cc"
    break;

  case 111:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8510 "Parser/parser.cc"
    break;

  case 112:
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8516 "Parser/parser.cc"
    break;

  case 113:
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8522 "Parser/parser.cc"
    break;

  case 114:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 8528 "Parser/parser.cc"
    break;

  case 115:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8534 "Parser/parser.cc"
    break;

  case 116:
#line 938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8540 "Parser/parser.cc"
    break;

  case 124:
#line 958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8546 "Parser/parser.cc"
    break;

  case 126:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8552 "Parser/parser.cc"
    break;

  case 127:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8558 "Parser/parser.cc"
    break;

  case 128:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8564 "Parser/parser.cc"
    break;

  case 130:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8570 "Parser/parser.cc"
    break;

  case 131:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8576 "Parser/parser.cc"
    break;

  case 133:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8582 "Parser/parser.cc"
    break;

  case 134:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8588 "Parser/parser.cc"
    break;

  case 136:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8594 "Parser/parser.cc"
    break;

  case 137:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8600 "Parser/parser.cc"
    break;

  case 138:
#line 994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8606 "Parser/parser.cc"
    break;

  case 139:
#line 996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8612 "Parser/parser.cc"
    break;

  case 141:
#line 1002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8618 "Parser/parser.cc"
    break;

  case 142:
#line 1004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8624 "Parser/parser.cc"
    break;

  case 144:
#line 1010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8630 "Parser/parser.cc"
    break;

  case 146:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8636 "Parser/parser.cc"
    break;

  case 148:
#line 1022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8642 "Parser/parser.cc"
    break;

  case 150:
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 8648 "Parser/parser.cc"
    break;

  case 152:
#line 1034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 8654 "Parser/parser.cc"
    break;

  case 154:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8660 "Parser/parser.cc"
    break;

  case 155:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), (yyvsp[-3].expr)->clone(), (yyvsp[0].expr) ) ); }
#line 8666 "Parser/parser.cc"
    break;

  case 158:
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 8678 "Parser/parser.cc"
    break;

  case 159:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8684 "Parser/parser.cc"
    break;

  case 160:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8690 "Parser/parser.cc"
    break;

  case 164:
#line 1077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 8696 "Parser/parser.cc"
    break;

  case 165:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 8702 "Parser/parser.cc"
    break;

  case 166:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 8708 "Parser/parser.cc"
    break;

  case 167:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 8714 "Parser/parser.cc"
    break;

  case 168:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 8720 "Parser/parser.cc"
    break;

  case 169:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 8726 "Parser/parser.cc"
    break;

  case 170:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 8732 "Parser/parser.cc"
    break;

  case 171:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 8738 "Parser/parser.cc"
    break;

  case 172:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 8744 "Parser/parser.cc"
    break;

  case 173:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 8750 "Parser/parser.cc"
    break;

  case 174:
#line 1090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 8756 "Parser/parser.cc"
    break;

  case 175:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 8762 "Parser/parser.cc"
    break;

  case 176:
#line 1092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 8768 "Parser/parser.cc"
    break;

  case 177:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].expr) ) ) ); }
#line 8774 "Parser/parser.cc"
    break;

  case 178:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) )); }
#line 8780 "Parser/parser.cc"
    break;

  case 180:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8786 "Parser/parser.cc"
    break;

  case 181:
#line 1113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8792 "Parser/parser.cc"
    break;

  case 182:
#line 1115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8798 "Parser/parser.cc"
    break;

  case 184:
#line 1121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8804 "Parser/parser.cc"
    break;

  case 185:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8810 "Parser/parser.cc"
    break;

  case 200:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 8816 "Parser/parser.cc"
    break;

  case 202:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 8822 "Parser/parser.cc"
    break;

  case 203:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8828 "Parser/parser.cc"
    break;

  case 204:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntx error, label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.stmt) = nullptr;
		}
#line 8839 "Parser/parser.cc"
    break;

  case 205:
#line 1168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 8845 "Parser/parser.cc"
    break;

  case 206:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 8851 "Parser/parser.cc"
    break;

  case 208:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8857 "Parser/parser.cc"
    break;

  case 209:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8863 "Parser/parser.cc"
    break;

  case 210:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8869 "Parser/parser.cc"
    break;

  case 211:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8875 "Parser/parser.cc"
    break;

  case 212:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8881 "Parser/parser.cc"
    break;

  case 215:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8887 "Parser/parser.cc"
    break;

  case 216:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 8893 "Parser/parser.cc"
    break;

  case 217:
#line 1204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 8899 "Parser/parser.cc"
    break;

  case 218:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8905 "Parser/parser.cc"
    break;

  case 219:
#line 1213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 8911 "Parser/parser.cc"
    break;

  case 220:
#line 1215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8925 "Parser/parser.cc"
    break;

  case 221:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 8931 "Parser/parser.cc"
    break;

  case 222:
#line 1227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 8937 "Parser/parser.cc"
    break;

  case 223:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8946 "Parser/parser.cc"
    break;

  case 224:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 8952 "Parser/parser.cc"
    break;

  case 225:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 8958 "Parser/parser.cc"
    break;

  case 226:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 8964 "Parser/parser.cc"
    break;

  case 227:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 8970 "Parser/parser.cc"
    break;

  case 228:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8976 "Parser/parser.cc"
    break;

  case 229:
#line 1251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8982 "Parser/parser.cc"
    break;

  case 230:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 8988 "Parser/parser.cc"
    break;

  case 231:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 8994 "Parser/parser.cc"
    break;

  case 232:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9000 "Parser/parser.cc"
    break;

  case 234:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9006 "Parser/parser.cc"
    break;

  case 235:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9012 "Parser/parser.cc"
    break;

  case 236:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9018 "Parser/parser.cc"
    break;

  case 237:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9024 "Parser/parser.cc"
    break;

  case 238:
#line 1277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9030 "Parser/parser.cc"
    break;

  case 239:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9036 "Parser/parser.cc"
    break;

  case 240:
#line 1281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9042 "Parser/parser.cc"
    break;

  case 242:
#line 1286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9048 "Parser/parser.cc"
    break;

  case 243:
#line 1290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9054 "Parser/parser.cc"
    break;

  case 244:
#line 1295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9060 "Parser/parser.cc"
    break;

  case 246:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9066 "Parser/parser.cc"
    break;

  case 247:
#line 1303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9072 "Parser/parser.cc"
    break;

  case 248:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9078 "Parser/parser.cc"
    break;

  case 249:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9087 "Parser/parser.cc"
    break;

  case 250:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9093 "Parser/parser.cc"
    break;

  case 251:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9099 "Parser/parser.cc"
    break;

  case 252:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9105 "Parser/parser.cc"
    break;

  case 253:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9114 "Parser/parser.cc"
    break;

  case 254:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9120 "Parser/parser.cc"
    break;

  case 255:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9126 "Parser/parser.cc"
    break;

  case 256:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9132 "Parser/parser.cc"
    break;

  case 257:
#line 1332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9141 "Parser/parser.cc"
    break;

  case 258:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9147 "Parser/parser.cc"
    break;

  case 259:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9153 "Parser/parser.cc"
    break;

  case 261:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9172 "Parser/parser.cc"
    break;

  case 262:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9178 "Parser/parser.cc"
    break;

  case 263:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9187 "Parser/parser.cc"
    break;

  case 264:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9193 "Parser/parser.cc"
    break;

  case 265:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9199 "Parser/parser.cc"
    break;

  case 266:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9205 "Parser/parser.cc"
    break;

  case 267:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9211 "Parser/parser.cc"
    break;

  case 268:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9217 "Parser/parser.cc"
    break;

  case 269:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9223 "Parser/parser.cc"
    break;

  case 270:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9232 "Parser/parser.cc"
    break;

  case 271:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9241 "Parser/parser.cc"
    break;

  case 272:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9247 "Parser/parser.cc"
    break;

  case 273:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9256 "Parser/parser.cc"
    break;

  case 274:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9265 "Parser/parser.cc"
    break;

  case 275:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9271 "Parser/parser.cc"
    break;

  case 276:
#line 1413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9277 "Parser/parser.cc"
    break;

  case 277:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9283 "Parser/parser.cc"
    break;

  case 278:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9289 "Parser/parser.cc"
    break;

  case 279:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9295 "Parser/parser.cc"
    break;

  case 280:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9301 "Parser/parser.cc"
    break;

  case 281:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9307 "Parser/parser.cc"
    break;

  case 282:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9313 "Parser/parser.cc"
    break;

  case 283:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9322 "Parser/parser.cc"
    break;

  case 284:
#line 1434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9332 "Parser/parser.cc"
    break;

  case 285:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9338 "Parser/parser.cc"
    break;

  case 286:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9344 "Parser/parser.cc"
    break;

  case 287:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9353 "Parser/parser.cc"
    break;

  case 288:
#line 1450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9363 "Parser/parser.cc"
    break;

  case 289:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9369 "Parser/parser.cc"
    break;

  case 290:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9378 "Parser/parser.cc"
    break;

  case 291:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9388 "Parser/parser.cc"
    break;

  case 292:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9394 "Parser/parser.cc"
    break;

  case 293:
#line 1472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9400 "Parser/parser.cc"
    break;

  case 294:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9406 "Parser/parser.cc"
    break;

  case 295:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9412 "Parser/parser.cc"
    break;

  case 296:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9421 "Parser/parser.cc"
    break;

  case 297:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9431 "Parser/parser.cc"
    break;

  case 298:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9437 "Parser/parser.cc"
    break;

  case 299:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9446 "Parser/parser.cc"
    break;

  case 300:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9456 "Parser/parser.cc"
    break;

  case 301:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9462 "Parser/parser.cc"
    break;

  case 302:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9471 "Parser/parser.cc"
    break;

  case 303:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9481 "Parser/parser.cc"
    break;

  case 304:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9487 "Parser/parser.cc"
    break;

  case 305:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9496 "Parser/parser.cc"
    break;

  case 306:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 9507 "Parser/parser.cc"
    break;

  case 307:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9513 "Parser/parser.cc"
    break;

  case 308:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9519 "Parser/parser.cc"
    break;

  case 309:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9525 "Parser/parser.cc"
    break;

  case 310:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 9531 "Parser/parser.cc"
    break;

  case 311:
#line 1546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9537 "Parser/parser.cc"
    break;

  case 313:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9543 "Parser/parser.cc"
    break;

  case 314:
#line 1554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9549 "Parser/parser.cc"
    break;

  case 315:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 9555 "Parser/parser.cc"
    break;

  case 316:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 9561 "Parser/parser.cc"
    break;

  case 317:
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 9567 "Parser/parser.cc"
    break;

  case 318:
#line 1568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 9573 "Parser/parser.cc"
    break;

  case 319:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 9579 "Parser/parser.cc"
    break;

  case 320:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 9585 "Parser/parser.cc"
    break;

  case 321:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 9591 "Parser/parser.cc"
    break;

  case 322:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 9597 "Parser/parser.cc"
    break;

  case 323:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 9603 "Parser/parser.cc"
    break;

  case 324:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 9609 "Parser/parser.cc"
    break;

  case 325:
#line 1588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9615 "Parser/parser.cc"
    break;

  case 326:
#line 1590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 9621 "Parser/parser.cc"
    break;

  case 327:
#line 1592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 9627 "Parser/parser.cc"
    break;

  case 328:
#line 1594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 9633 "Parser/parser.cc"
    break;

  case 329:
#line 1596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 9639 "Parser/parser.cc"
    break;

  case 330:
#line 1598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 9645 "Parser/parser.cc"
    break;

  case 331:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 9651 "Parser/parser.cc"
    break;

  case 332:
#line 1602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 9657 "Parser/parser.cc"
    break;

  case 333:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 9663 "Parser/parser.cc"
    break;

  case 334:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9669 "Parser/parser.cc"
    break;

  case 337:
#line 1616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9675 "Parser/parser.cc"
    break;

  case 338:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 9684 "Parser/parser.cc"
    break;

  case 339:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9690 "Parser/parser.cc"
    break;

  case 340:
#line 1634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9696 "Parser/parser.cc"
    break;

  case 343:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9702 "Parser/parser.cc"
    break;

  case 344:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9708 "Parser/parser.cc"
    break;

  case 347:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9714 "Parser/parser.cc"
    break;

  case 348:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) )); }
#line 9720 "Parser/parser.cc"
    break;

  case 349:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9726 "Parser/parser.cc"
    break;

  case 350:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9732 "Parser/parser.cc"
    break;

  case 351:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9738 "Parser/parser.cc"
    break;

  case 352:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9744 "Parser/parser.cc"
    break;

  case 353:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9750 "Parser/parser.cc"
    break;

  case 354:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9756 "Parser/parser.cc"
    break;

  case 355:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 9762 "Parser/parser.cc"
    break;

  case 358:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9768 "Parser/parser.cc"
    break;

  case 359:
#line 1694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9774 "Parser/parser.cc"
    break;

  case 360:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 9780 "Parser/parser.cc"
    break;

  case 361:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9786 "Parser/parser.cc"
    break;

  case 362:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9792 "Parser/parser.cc"
    break;

  case 363:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9798 "Parser/parser.cc"
    break;

  case 364:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9804 "Parser/parser.cc"
    break;

  case 365:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9810 "Parser/parser.cc"
    break;

  case 366:
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 9816 "Parser/parser.cc"
    break;

  case 367:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 9822 "Parser/parser.cc"
    break;

  case 368:
#line 1727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "cofor statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9828 "Parser/parser.cc"
    break;

  case 369:
#line 1732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 9834 "Parser/parser.cc"
    break;

  case 370:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 9840 "Parser/parser.cc"
    break;

  case 371:
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 9846 "Parser/parser.cc"
    break;

  case 372:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9852 "Parser/parser.cc"
    break;

  case 373:
#line 1743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 9858 "Parser/parser.cc"
    break;

  case 374:
#line 1748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9864 "Parser/parser.cc"
    break;

  case 375:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9870 "Parser/parser.cc"
    break;

  case 376:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9876 "Parser/parser.cc"
    break;

  case 377:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9882 "Parser/parser.cc"
    break;

  case 378:
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 9888 "Parser/parser.cc"
    break;

  case 379:
#line 1756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 9894 "Parser/parser.cc"
    break;

  case 380:
#line 1760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 9900 "Parser/parser.cc"
    break;

  case 382:
#line 1767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9906 "Parser/parser.cc"
    break;

  case 383:
#line 1769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9912 "Parser/parser.cc"
    break;

  case 384:
#line 1771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9918 "Parser/parser.cc"
    break;

  case 389:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 9924 "Parser/parser.cc"
    break;

  case 390:
#line 1788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9930 "Parser/parser.cc"
    break;

  case 391:
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9936 "Parser/parser.cc"
    break;

  case 392:
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9942 "Parser/parser.cc"
    break;

  case 393:
#line 1794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 9948 "Parser/parser.cc"
    break;

  case 394:
#line 1799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 9954 "Parser/parser.cc"
    break;

  case 395:
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 9960 "Parser/parser.cc"
    break;

  case 396:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9966 "Parser/parser.cc"
    break;

  case 399:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 9972 "Parser/parser.cc"
    break;

  case 400:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 9978 "Parser/parser.cc"
    break;

  case 401:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 9987 "Parser/parser.cc"
    break;

  case 402:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9993 "Parser/parser.cc"
    break;

  case 403:
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 9999 "Parser/parser.cc"
    break;

  case 404:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ) ); }
#line 10005 "Parser/parser.cc"
    break;

  case 405:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10014 "Parser/parser.cc"
    break;

  case 406:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10023 "Parser/parser.cc"
    break;

  case 407:
#line 1852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10029 "Parser/parser.cc"
    break;

  case 410:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10035 "Parser/parser.cc"
    break;

  case 411:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10041 "Parser/parser.cc"
    break;

  case 413:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10047 "Parser/parser.cc"
    break;

  case 414:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-1].decl) ); }
#line 10053 "Parser/parser.cc"
    break;

  case 421:
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10064 "Parser/parser.cc"
    break;

  case 424:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10070 "Parser/parser.cc"
    break;

  case 425:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10076 "Parser/parser.cc"
    break;

  case 429:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10082 "Parser/parser.cc"
    break;

  case 431:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10088 "Parser/parser.cc"
    break;

  case 432:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10094 "Parser/parser.cc"
    break;

  case 433:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10100 "Parser/parser.cc"
    break;

  case 434:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10106 "Parser/parser.cc"
    break;

  case 435:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10112 "Parser/parser.cc"
    break;

  case 436:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10118 "Parser/parser.cc"
    break;

  case 438:
#line 1953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10124 "Parser/parser.cc"
    break;

  case 439:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10130 "Parser/parser.cc"
    break;

  case 440:
#line 1957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10136 "Parser/parser.cc"
    break;

  case 441:
#line 1959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10147 "Parser/parser.cc"
    break;

  case 442:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10153 "Parser/parser.cc"
    break;

  case 443:
#line 1994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10159 "Parser/parser.cc"
    break;

  case 444:
#line 1999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10165 "Parser/parser.cc"
    break;

  case 445:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 10171 "Parser/parser.cc"
    break;

  case 446:
#line 2007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10180 "Parser/parser.cc"
    break;

  case 447:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10189 "Parser/parser.cc"
    break;

  case 448:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10198 "Parser/parser.cc"
    break;

  case 449:
#line 2028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10209 "Parser/parser.cc"
    break;

  case 450:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10218 "Parser/parser.cc"
    break;

  case 451:
#line 2040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10224 "Parser/parser.cc"
    break;

  case 452:
#line 2042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10230 "Parser/parser.cc"
    break;

  case 453:
#line 2044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10236 "Parser/parser.cc"
    break;

  case 454:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10244 "Parser/parser.cc"
    break;

  case 455:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10252 "Parser/parser.cc"
    break;

  case 456:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10258 "Parser/parser.cc"
    break;

  case 459:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			assert( (yyvsp[0].decl)->type );
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {			// CV qualifiers ?
				SemanticError( yylloc, "syntax error, useless type qualifier(s) in empty declaration." ); (yyval.decl) = nullptr;
			}
			// enums are never empty declarations because there must have at least one enumeration.
			if ( (yyvsp[0].decl)->type->kind == TypeData::AggregateInst && (yyvsp[0].decl)->storageClasses.any() ) { // storage class ?
				SemanticError( yylloc, "syntax error, useless storage qualifier(s) in empty aggregate declaration." ); (yyval.decl) = nullptr;
			}
		}
#line 10273 "Parser/parser.cc"
    break;

  case 460:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10279 "Parser/parser.cc"
    break;

  case 461:
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10285 "Parser/parser.cc"
    break;

  case 462:
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10291 "Parser/parser.cc"
    break;

  case 463:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10297 "Parser/parser.cc"
    break;

  case 464:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10303 "Parser/parser.cc"
    break;

  case 470:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntax error, expecting ';' at end of ",
				(yyvsp[-1].decl)->type->enumeration.name ? "enum" : ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ),
				" declaration." ) );
			(yyval.decl) = nullptr;
		}
#line 10314 "Parser/parser.cc"
    break;

  case 483:
#line 2148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10320 "Parser/parser.cc"
    break;

  case 486:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10326 "Parser/parser.cc"
    break;

  case 489:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Const ); }
#line 10332 "Parser/parser.cc"
    break;

  case 490:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Restrict ); }
#line 10338 "Parser/parser.cc"
    break;

  case 491:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Volatile ); }
#line 10344 "Parser/parser.cc"
    break;

  case 492:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Atomic ); }
#line 10350 "Parser/parser.cc"
    break;

  case 493:
#line 2178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 10356 "Parser/parser.cc"
    break;

  case 494:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10362 "Parser/parser.cc"
    break;

  case 496:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10368 "Parser/parser.cc"
    break;

  case 497:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10374 "Parser/parser.cc"
    break;

  case 499:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10380 "Parser/parser.cc"
    break;

  case 500:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10386 "Parser/parser.cc"
    break;

  case 501:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 10392 "Parser/parser.cc"
    break;

  case 502:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 10398 "Parser/parser.cc"
    break;

  case 503:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 10404 "Parser/parser.cc"
    break;

  case 504:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 10410 "Parser/parser.cc"
    break;

  case 505:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 10416 "Parser/parser.cc"
    break;

  case 506:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 10422 "Parser/parser.cc"
    break;

  case 507:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 10428 "Parser/parser.cc"
    break;

  case 508:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 10434 "Parser/parser.cc"
    break;

  case 509:
#line 2229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10440 "Parser/parser.cc"
    break;

  case 510:
#line 2231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10446 "Parser/parser.cc"
    break;

  case 511:
#line 2233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10452 "Parser/parser.cc"
    break;

  case 512:
#line 2235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10458 "Parser/parser.cc"
    break;

  case 513:
#line 2237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10464 "Parser/parser.cc"
    break;

  case 514:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10470 "Parser/parser.cc"
    break;

  case 515:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10476 "Parser/parser.cc"
    break;

  case 516:
#line 2243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10482 "Parser/parser.cc"
    break;

  case 517:
#line 2245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10488 "Parser/parser.cc"
    break;

  case 518:
#line 2247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10494 "Parser/parser.cc"
    break;

  case 519:
#line 2249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10500 "Parser/parser.cc"
    break;

  case 520:
#line 2251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10506 "Parser/parser.cc"
    break;

  case 521:
#line 2253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10512 "Parser/parser.cc"
    break;

  case 522:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10518 "Parser/parser.cc"
    break;

  case 523:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10524 "Parser/parser.cc"
    break;

  case 524:
#line 2259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10530 "Parser/parser.cc"
    break;

  case 525:
#line 2261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10536 "Parser/parser.cc"
    break;

  case 526:
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10542 "Parser/parser.cc"
    break;

  case 527:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10548 "Parser/parser.cc"
    break;

  case 528:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10554 "Parser/parser.cc"
    break;

  case 529:
#line 2269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10560 "Parser/parser.cc"
    break;

  case 530:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10566 "Parser/parser.cc"
    break;

  case 531:
#line 2273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10572 "Parser/parser.cc"
    break;

  case 532:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10578 "Parser/parser.cc"
    break;

  case 533:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10584 "Parser/parser.cc"
    break;

  case 534:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10590 "Parser/parser.cc"
    break;

  case 535:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10596 "Parser/parser.cc"
    break;

  case 537:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10602 "Parser/parser.cc"
    break;

  case 539:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10608 "Parser/parser.cc"
    break;

  case 540:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10614 "Parser/parser.cc"
    break;

  case 541:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10620 "Parser/parser.cc"
    break;

  case 543:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10626 "Parser/parser.cc"
    break;

  case 544:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10632 "Parser/parser.cc"
    break;

  case 545:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10638 "Parser/parser.cc"
    break;

  case 546:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10644 "Parser/parser.cc"
    break;

  case 548:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10650 "Parser/parser.cc"
    break;

  case 550:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10656 "Parser/parser.cc"
    break;

  case 551:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10662 "Parser/parser.cc"
    break;

  case 552:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10668 "Parser/parser.cc"
    break;

  case 553:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10674 "Parser/parser.cc"
    break;

  case 554:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 10680 "Parser/parser.cc"
    break;

  case 555:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10686 "Parser/parser.cc"
    break;

  case 556:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 10692 "Parser/parser.cc"
    break;

  case 557:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10698 "Parser/parser.cc"
    break;

  case 558:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10704 "Parser/parser.cc"
    break;

  case 559:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10715 "Parser/parser.cc"
    break;

  case 560:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10721 "Parser/parser.cc"
    break;

  case 561:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10727 "Parser/parser.cc"
    break;

  case 562:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10733 "Parser/parser.cc"
    break;

  case 563:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10744 "Parser/parser.cc"
    break;

  case 564:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10750 "Parser/parser.cc"
    break;

  case 565:
#line 2376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10756 "Parser/parser.cc"
    break;

  case 566:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10765 "Parser/parser.cc"
    break;

  case 568:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10771 "Parser/parser.cc"
    break;

  case 569:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10777 "Parser/parser.cc"
    break;

  case 570:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10783 "Parser/parser.cc"
    break;

  case 572:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10789 "Parser/parser.cc"
    break;

  case 573:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10795 "Parser/parser.cc"
    break;

  case 575:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10801 "Parser/parser.cc"
    break;

  case 576:
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10807 "Parser/parser.cc"
    break;

  case 577:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10813 "Parser/parser.cc"
    break;

  case 579:
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10819 "Parser/parser.cc"
    break;

  case 580:
#line 2417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10825 "Parser/parser.cc"
    break;

  case 581:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10831 "Parser/parser.cc"
    break;

  case 582:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10837 "Parser/parser.cc"
    break;

  case 583:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10843 "Parser/parser.cc"
    break;

  case 585:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10849 "Parser/parser.cc"
    break;

  case 586:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10855 "Parser/parser.cc"
    break;

  case 587:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10861 "Parser/parser.cc"
    break;

  case 588:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10867 "Parser/parser.cc"
    break;

  case 589:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 10873 "Parser/parser.cc"
    break;

  case 590:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10884 "Parser/parser.cc"
    break;

  case 594:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10890 "Parser/parser.cc"
    break;

  case 595:
#line 2463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 596:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 10905 "Parser/parser.cc"
    break;

  case 597:
#line 2470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10913 "Parser/parser.cc"
    break;

  case 598:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 10922 "Parser/parser.cc"
    break;

  case 599:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10931 "Parser/parser.cc"
    break;

  case 600:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 10940 "Parser/parser.cc"
    break;

  case 601:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10949 "Parser/parser.cc"
    break;

  case 603:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10955 "Parser/parser.cc"
    break;

  case 604:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10961 "Parser/parser.cc"
    break;

  case 605:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10971 "Parser/parser.cc"
    break;

  case 606:
#line 2511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			forall = false;								// reset
			// Create new generic declaration with same name as previous forward declaration, where the IDENTIFIER is
			// switched to a TYPEGENname. Link any generic arguments from typegen_name to new generic declaration and
			// delete newFromTypeGen.
			if ( (yyvsp[0].decl)->type->kind == TypeData::SymbolicInst && ! (yyvsp[0].decl)->type->symbolic.isTypedef ) {
				(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) );
			} else {
				(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].decl)->type->symbolic.name, (yyvsp[0].decl)->type->symbolic.actuals, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
				(yyvsp[0].decl)->type->symbolic.name = nullptr;			// copied to $$
				(yyvsp[0].decl)->type->symbolic.actuals = nullptr;
				delete (yyvsp[0].decl);
			}
		}
#line 10990 "Parser/parser.cc"
    break;

  case 609:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 10996 "Parser/parser.cc"
    break;

  case 610:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11002 "Parser/parser.cc"
    break;

  case 611:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11008 "Parser/parser.cc"
    break;

  case 612:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11014 "Parser/parser.cc"
    break;

  case 613:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11020 "Parser/parser.cc"
    break;

  case 614:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11026 "Parser/parser.cc"
    break;

  case 615:
#line 2550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11035 "Parser/parser.cc"
    break;

  case 616:
#line 2555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11041 "Parser/parser.cc"
    break;

  case 617:
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11050 "Parser/parser.cc"
    break;

  case 618:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11056 "Parser/parser.cc"
    break;

  case 619:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11065 "Parser/parser.cc"
    break;

  case 620:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11071 "Parser/parser.cc"
    break;

  case 621:
#line 2574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11077 "Parser/parser.cc"
    break;

  case 622:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11090 "Parser/parser.cc"
    break;

  case 623:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntax error, expecting ';' at end of previous declaration." ) );
			(yyval.decl) = nullptr;
		}
#line 11099 "Parser/parser.cc"
    break;

  case 624:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11105 "Parser/parser.cc"
    break;

  case 625:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11111 "Parser/parser.cc"
    break;

  case 626:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11124 "Parser/parser.cc"
    break;

  case 627:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11130 "Parser/parser.cc"
    break;

  case 630:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11136 "Parser/parser.cc"
    break;

  case 631:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11142 "Parser/parser.cc"
    break;

  case 634:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11148 "Parser/parser.cc"
    break;

  case 636:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11154 "Parser/parser.cc"
    break;

  case 637:
#line 2627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11160 "Parser/parser.cc"
    break;

  case 638:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11166 "Parser/parser.cc"
    break;

  case 639:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11172 "Parser/parser.cc"
    break;

  case 640:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11178 "Parser/parser.cc"
    break;

  case 641:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11184 "Parser/parser.cc"
    break;

  case 643:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11190 "Parser/parser.cc"
    break;

  case 645:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11196 "Parser/parser.cc"
    break;

  case 646:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11202 "Parser/parser.cc"
    break;

  case 648:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11208 "Parser/parser.cc"
    break;

  case 649:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11214 "Parser/parser.cc"
    break;

  case 651:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11220 "Parser/parser.cc"
    break;

  case 652:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11226 "Parser/parser.cc"
    break;

  case 653:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11232 "Parser/parser.cc"
    break;

  case 654:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11238 "Parser/parser.cc"
    break;

  case 655:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11244 "Parser/parser.cc"
    break;

  case 656:
#line 2688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11250 "Parser/parser.cc"
    break;

  case 657:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11261 "Parser/parser.cc"
    break;

  case 658:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11267 "Parser/parser.cc"
    break;

  case 659:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11275 "Parser/parser.cc"
    break;

  case 660:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11281 "Parser/parser.cc"
    break;

  case 661:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl) && ((yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0 )) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11292 "Parser/parser.cc"
    break;

  case 662:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11300 "Parser/parser.cc"
    break;

  case 663:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11308 "Parser/parser.cc"
    break;

  case 664:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11316 "Parser/parser.cc"
    break;

  case 665:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11324 "Parser/parser.cc"
    break;

  case 667:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11330 "Parser/parser.cc"
    break;

  case 668:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11336 "Parser/parser.cc"
    break;

  case 669:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11345 "Parser/parser.cc"
    break;

  case 670:
#line 2744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11354 "Parser/parser.cc"
    break;

  case 671:
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11360 "Parser/parser.cc"
    break;

  case 672:
#line 2754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 11366 "Parser/parser.cc"
    break;

  case 673:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11372 "Parser/parser.cc"
    break;

  case 674:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11378 "Parser/parser.cc"
    break;

  case 676:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11384 "Parser/parser.cc"
    break;

  case 677:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11390 "Parser/parser.cc"
    break;

  case 678:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11396 "Parser/parser.cc"
    break;

  case 679:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11402 "Parser/parser.cc"
    break;

  case 680:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11408 "Parser/parser.cc"
    break;

  case 681:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11414 "Parser/parser.cc"
    break;

  case 684:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11420 "Parser/parser.cc"
    break;

  case 685:
#line 2786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11426 "Parser/parser.cc"
    break;

  case 686:
#line 2788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11432 "Parser/parser.cc"
    break;

  case 688:
#line 2796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11438 "Parser/parser.cc"
    break;

  case 689:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11444 "Parser/parser.cc"
    break;

  case 690:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11450 "Parser/parser.cc"
    break;

  case 692:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11456 "Parser/parser.cc"
    break;

  case 693:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11462 "Parser/parser.cc"
    break;

  case 694:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11468 "Parser/parser.cc"
    break;

  case 696:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11474 "Parser/parser.cc"
    break;

  case 699:
#line 2823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11480 "Parser/parser.cc"
    break;

  case 700:
#line 2825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11486 "Parser/parser.cc"
    break;

  case 702:
#line 2834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11492 "Parser/parser.cc"
    break;

  case 703:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11498 "Parser/parser.cc"
    break;

  case 704:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11504 "Parser/parser.cc"
    break;

  case 709:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11510 "Parser/parser.cc"
    break;

  case 711:
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11516 "Parser/parser.cc"
    break;

  case 712:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11522 "Parser/parser.cc"
    break;

  case 713:
#line 2863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11528 "Parser/parser.cc"
    break;

  case 714:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11534 "Parser/parser.cc"
    break;

  case 715:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11540 "Parser/parser.cc"
    break;

  case 716:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11546 "Parser/parser.cc"
    break;

  case 722:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11552 "Parser/parser.cc"
    break;

  case 725:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11558 "Parser/parser.cc"
    break;

  case 726:
#line 2900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 11564 "Parser/parser.cc"
    break;

  case 727:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 11570 "Parser/parser.cc"
    break;

  case 728:
#line 2902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11576 "Parser/parser.cc"
    break;

  case 729:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11582 "Parser/parser.cc"
    break;

  case 730:
#line 2907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11588 "Parser/parser.cc"
    break;

  case 731:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11594 "Parser/parser.cc"
    break;

  case 733:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 11600 "Parser/parser.cc"
    break;

  case 734:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (InitializerNode *)( (yyvsp[-2].init)->set_last( (yyvsp[0].init) ) ); }
#line 11606 "Parser/parser.cc"
    break;

  case 735:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (InitializerNode *)((yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) )); }
#line 11612 "Parser/parser.cc"
    break;

  case 737:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 11618 "Parser/parser.cc"
    break;

  case 739:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-1].expr)->set_last( (yyvsp[0].expr) )); }
#line 11624 "Parser/parser.cc"
    break;

  case 740:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 11630 "Parser/parser.cc"
    break;

  case 741:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11636 "Parser/parser.cc"
    break;

  case 742:
#line 2949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11642 "Parser/parser.cc"
    break;

  case 743:
#line 2951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 11648 "Parser/parser.cc"
    break;

  case 744:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11654 "Parser/parser.cc"
    break;

  case 746:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11660 "Parser/parser.cc"
    break;

  case 747:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11666 "Parser/parser.cc"
    break;

  case 748:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11672 "Parser/parser.cc"
    break;

  case 749:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11683 "Parser/parser.cc"
    break;

  case 750:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11689 "Parser/parser.cc"
    break;

  case 751:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 11695 "Parser/parser.cc"
    break;

  case 752:
#line 3000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11701 "Parser/parser.cc"
    break;

  case 753:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11710 "Parser/parser.cc"
    break;

  case 754:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11716 "Parser/parser.cc"
    break;

  case 755:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11722 "Parser/parser.cc"
    break;

  case 756:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11728 "Parser/parser.cc"
    break;

  case 757:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 11734 "Parser/parser.cc"
    break;

  case 758:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11740 "Parser/parser.cc"
    break;

  case 759:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11746 "Parser/parser.cc"
    break;

  case 760:
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11752 "Parser/parser.cc"
    break;

  case 761:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 11758 "Parser/parser.cc"
    break;

  case 762:
#line 3032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11764 "Parser/parser.cc"
    break;

  case 763:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11770 "Parser/parser.cc"
    break;

  case 766:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11776 "Parser/parser.cc"
    break;

  case 767:
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11782 "Parser/parser.cc"
    break;

  case 768:
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11788 "Parser/parser.cc"
    break;

  case 769:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11794 "Parser/parser.cc"
    break;

  case 771:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11800 "Parser/parser.cc"
    break;

  case 772:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 11806 "Parser/parser.cc"
    break;

  case 773:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11812 "Parser/parser.cc"
    break;

  case 774:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11818 "Parser/parser.cc"
    break;

  case 775:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11824 "Parser/parser.cc"
    break;

  case 776:
#line 3077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11830 "Parser/parser.cc"
    break;

  case 777:
#line 3079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11836 "Parser/parser.cc"
    break;

  case 778:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11845 "Parser/parser.cc"
    break;

  case 779:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11854 "Parser/parser.cc"
    break;

  case 780:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11863 "Parser/parser.cc"
    break;

  case 781:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11869 "Parser/parser.cc"
    break;

  case 782:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11878 "Parser/parser.cc"
    break;

  case 783:
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11884 "Parser/parser.cc"
    break;

  case 785:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11890 "Parser/parser.cc"
    break;

  case 790:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11896 "Parser/parser.cc"
    break;

  case 791:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11902 "Parser/parser.cc"
    break;

  case 792:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11908 "Parser/parser.cc"
    break;

  case 794:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11914 "Parser/parser.cc"
    break;

  case 795:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11920 "Parser/parser.cc"
    break;

  case 796:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11926 "Parser/parser.cc"
    break;

  case 797:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11932 "Parser/parser.cc"
    break;

  case 799:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11938 "Parser/parser.cc"
    break;

  case 800:
#line 3163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11944 "Parser/parser.cc"
    break;

  case 801:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 11950 "Parser/parser.cc"
    break;

  case 802:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Variable declarations of anonymous types requires creating a unique type-name across multiple translation
			// unit, which is a dubious task, especially because C uses name rather than structural typing; hence it is
			// disallowed at the moment.
			if ( (yyvsp[0].decl)->linkage == ast::Linkage::Cforall && ! (yyvsp[0].decl)->storageClasses.is_static && (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->kind == TypeData::AggregateInst ) {
				if ( (yyvsp[0].decl)->type->aggInst.aggregate->kind == TypeData::Enum && (yyvsp[0].decl)->type->aggInst.aggregate->enumeration.anon ) {
					SemanticError( yylloc, "extern anonymous enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
				} else if ( (yyvsp[0].decl)->type->aggInst.aggregate->aggregate.anon ) { // handles struct or union
					SemanticError( yylloc, "extern anonymous struct/union is currently unimplemented." ); (yyval.decl) = nullptr;
				}
			}
		}
#line 11967 "Parser/parser.cc"
    break;

  case 803:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11973 "Parser/parser.cc"
    break;

  case 804:
#line 3185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11979 "Parser/parser.cc"
    break;

  case 805:
#line 3187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11985 "Parser/parser.cc"
    break;

  case 806:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11991 "Parser/parser.cc"
    break;

  case 807:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11997 "Parser/parser.cc"
    break;

  case 808:
#line 3193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12003 "Parser/parser.cc"
    break;

  case 810:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12012 "Parser/parser.cc"
    break;

  case 811:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12018 "Parser/parser.cc"
    break;

  case 812:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12027 "Parser/parser.cc"
    break;

  case 813:
#line 3208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12037 "Parser/parser.cc"
    break;

  case 814:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12046 "Parser/parser.cc"
    break;

  case 815:
#line 3219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12056 "Parser/parser.cc"
    break;

  case 816:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12067 "Parser/parser.cc"
    break;

  case 817:
#line 3233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12077 "Parser/parser.cc"
    break;

  case 818:
#line 3239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12088 "Parser/parser.cc"
    break;

  case 819:
#line 3246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12098 "Parser/parser.cc"
    break;

  case 820:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12109 "Parser/parser.cc"
    break;

  case 821:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12119 "Parser/parser.cc"
    break;

  case 823:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12125 "Parser/parser.cc"
    break;

  case 824:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12131 "Parser/parser.cc"
    break;

  case 825:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12137 "Parser/parser.cc"
    break;

  case 826:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12149 "Parser/parser.cc"
    break;

  case 827:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12160 "Parser/parser.cc"
    break;

  case 828:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12169 "Parser/parser.cc"
    break;

  case 829:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12178 "Parser/parser.cc"
    break;

  case 830:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12184 "Parser/parser.cc"
    break;

  case 831:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12190 "Parser/parser.cc"
    break;

  case 832:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12196 "Parser/parser.cc"
    break;

  case 833:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12205 "Parser/parser.cc"
    break;

  case 834:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12211 "Parser/parser.cc"
    break;

  case 835:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12217 "Parser/parser.cc"
    break;

  case 836:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12223 "Parser/parser.cc"
    break;

  case 841:
#line 3346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12229 "Parser/parser.cc"
    break;

  case 842:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12235 "Parser/parser.cc"
    break;

  case 843:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12245 "Parser/parser.cc"
    break;

  case 844:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12251 "Parser/parser.cc"
    break;

  case 847:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12257 "Parser/parser.cc"
    break;

  case 848:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12263 "Parser/parser.cc"
    break;

  case 850:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12269 "Parser/parser.cc"
    break;

  case 851:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12275 "Parser/parser.cc"
    break;

  case 852:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12281 "Parser/parser.cc"
    break;

  case 853:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12287 "Parser/parser.cc"
    break;

  case 858:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12293 "Parser/parser.cc"
    break;

  case 859:
#line 3400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12299 "Parser/parser.cc"
    break;

  case 860:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12305 "Parser/parser.cc"
    break;

  case 861:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12311 "Parser/parser.cc"
    break;

  case 862:
#line 3442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12317 "Parser/parser.cc"
    break;

  case 864:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12323 "Parser/parser.cc"
    break;

  case 865:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12329 "Parser/parser.cc"
    break;

  case 866:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12335 "Parser/parser.cc"
    break;

  case 867:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12341 "Parser/parser.cc"
    break;

  case 868:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12347 "Parser/parser.cc"
    break;

  case 869:
#line 3458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12353 "Parser/parser.cc"
    break;

  case 870:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12359 "Parser/parser.cc"
    break;

  case 871:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 872:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12371 "Parser/parser.cc"
    break;

  case 873:
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12377 "Parser/parser.cc"
    break;

  case 874:
#line 3471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12383 "Parser/parser.cc"
    break;

  case 875:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12389 "Parser/parser.cc"
    break;

  case 876:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12395 "Parser/parser.cc"
    break;

  case 877:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12401 "Parser/parser.cc"
    break;

  case 878:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12407 "Parser/parser.cc"
    break;

  case 879:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12413 "Parser/parser.cc"
    break;

  case 880:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12419 "Parser/parser.cc"
    break;

  case 881:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12425 "Parser/parser.cc"
    break;

  case 883:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12431 "Parser/parser.cc"
    break;

  case 884:
#line 3503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12437 "Parser/parser.cc"
    break;

  case 885:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12443 "Parser/parser.cc"
    break;

  case 886:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12449 "Parser/parser.cc"
    break;

  case 887:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12455 "Parser/parser.cc"
    break;

  case 888:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12461 "Parser/parser.cc"
    break;

  case 889:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12467 "Parser/parser.cc"
    break;

  case 890:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12473 "Parser/parser.cc"
    break;

  case 891:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12479 "Parser/parser.cc"
    break;

  case 892:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12485 "Parser/parser.cc"
    break;

  case 893:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12491 "Parser/parser.cc"
    break;

  case 894:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12497 "Parser/parser.cc"
    break;

  case 895:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12503 "Parser/parser.cc"
    break;

  case 896:
#line 3533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12509 "Parser/parser.cc"
    break;

  case 897:
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12515 "Parser/parser.cc"
    break;

  case 898:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12521 "Parser/parser.cc"
    break;

  case 902:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12527 "Parser/parser.cc"
    break;

  case 903:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12533 "Parser/parser.cc"
    break;

  case 904:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12539 "Parser/parser.cc"
    break;

  case 905:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12545 "Parser/parser.cc"
    break;

  case 906:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12551 "Parser/parser.cc"
    break;

  case 907:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12557 "Parser/parser.cc"
    break;

  case 908:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12563 "Parser/parser.cc"
    break;

  case 909:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12569 "Parser/parser.cc"
    break;

  case 910:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12575 "Parser/parser.cc"
    break;

  case 911:
#line 3579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12581 "Parser/parser.cc"
    break;

  case 912:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12587 "Parser/parser.cc"
    break;

  case 913:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12593 "Parser/parser.cc"
    break;

  case 914:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12599 "Parser/parser.cc"
    break;

  case 915:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12605 "Parser/parser.cc"
    break;

  case 916:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12611 "Parser/parser.cc"
    break;

  case 917:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 12620 "Parser/parser.cc"
    break;

  case 918:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12626 "Parser/parser.cc"
    break;

  case 919:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12632 "Parser/parser.cc"
    break;

  case 921:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12638 "Parser/parser.cc"
    break;

  case 922:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12644 "Parser/parser.cc"
    break;

  case 923:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12650 "Parser/parser.cc"
    break;

  case 924:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12656 "Parser/parser.cc"
    break;

  case 925:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12662 "Parser/parser.cc"
    break;

  case 926:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12668 "Parser/parser.cc"
    break;

  case 927:
#line 3632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12674 "Parser/parser.cc"
    break;

  case 928:
#line 3634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12680 "Parser/parser.cc"
    break;

  case 929:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12686 "Parser/parser.cc"
    break;

  case 930:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12692 "Parser/parser.cc"
    break;

  case 931:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12698 "Parser/parser.cc"
    break;

  case 932:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12704 "Parser/parser.cc"
    break;

  case 933:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12710 "Parser/parser.cc"
    break;

  case 934:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12716 "Parser/parser.cc"
    break;

  case 935:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12722 "Parser/parser.cc"
    break;

  case 936:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12728 "Parser/parser.cc"
    break;

  case 937:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12734 "Parser/parser.cc"
    break;

  case 938:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12740 "Parser/parser.cc"
    break;

  case 940:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12746 "Parser/parser.cc"
    break;

  case 941:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12752 "Parser/parser.cc"
    break;

  case 942:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12758 "Parser/parser.cc"
    break;

  case 943:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12764 "Parser/parser.cc"
    break;

  case 944:
#line 3678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12770 "Parser/parser.cc"
    break;

  case 945:
#line 3680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12776 "Parser/parser.cc"
    break;

  case 946:
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12782 "Parser/parser.cc"
    break;

  case 947:
#line 3687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12788 "Parser/parser.cc"
    break;

  case 948:
#line 3689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12794 "Parser/parser.cc"
    break;

  case 949:
#line 3691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12800 "Parser/parser.cc"
    break;

  case 950:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12806 "Parser/parser.cc"
    break;

  case 951:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12812 "Parser/parser.cc"
    break;

  case 952:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12818 "Parser/parser.cc"
    break;

  case 953:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12824 "Parser/parser.cc"
    break;

  case 954:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12830 "Parser/parser.cc"
    break;

  case 955:
#line 3706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12836 "Parser/parser.cc"
    break;

  case 956:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12842 "Parser/parser.cc"
    break;

  case 957:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( ast::CV::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12848 "Parser/parser.cc"
    break;

  case 959:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12854 "Parser/parser.cc"
    break;

  case 960:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12860 "Parser/parser.cc"
    break;

  case 961:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12866 "Parser/parser.cc"
    break;

  case 962:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12872 "Parser/parser.cc"
    break;

  case 963:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12878 "Parser/parser.cc"
    break;

  case 964:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12884 "Parser/parser.cc"
    break;

  case 965:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12890 "Parser/parser.cc"
    break;

  case 966:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12896 "Parser/parser.cc"
    break;

  case 967:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12902 "Parser/parser.cc"
    break;

  case 968:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12908 "Parser/parser.cc"
    break;

  case 969:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12914 "Parser/parser.cc"
    break;

  case 970:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12920 "Parser/parser.cc"
    break;

  case 971:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12926 "Parser/parser.cc"
    break;

  case 972:
#line 3768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( ast::CV::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12932 "Parser/parser.cc"
    break;

  case 974:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12938 "Parser/parser.cc"
    break;

  case 975:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12944 "Parser/parser.cc"
    break;

  case 976:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12950 "Parser/parser.cc"
    break;

  case 977:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12956 "Parser/parser.cc"
    break;

  case 978:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12962 "Parser/parser.cc"
    break;

  case 979:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12968 "Parser/parser.cc"
    break;

  case 980:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12974 "Parser/parser.cc"
    break;

  case 981:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12980 "Parser/parser.cc"
    break;

  case 982:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12986 "Parser/parser.cc"
    break;

  case 983:
#line 3801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12992 "Parser/parser.cc"
    break;

  case 984:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12998 "Parser/parser.cc"
    break;

  case 986:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13004 "Parser/parser.cc"
    break;

  case 987:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13010 "Parser/parser.cc"
    break;

  case 988:
#line 3828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13016 "Parser/parser.cc"
    break;

  case 989:
#line 3830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13022 "Parser/parser.cc"
    break;

  case 990:
#line 3832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13028 "Parser/parser.cc"
    break;

  case 991:
#line 3834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13034 "Parser/parser.cc"
    break;

  case 992:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13040 "Parser/parser.cc"
    break;

  case 994:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13046 "Parser/parser.cc"
    break;

  case 995:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13052 "Parser/parser.cc"
    break;

  case 996:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13058 "Parser/parser.cc"
    break;

  case 997:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13064 "Parser/parser.cc"
    break;

  case 998:
#line 3853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13070 "Parser/parser.cc"
    break;

  case 999:
#line 3855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13076 "Parser/parser.cc"
    break;

  case 1000:
#line 3861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13082 "Parser/parser.cc"
    break;

  case 1001:
#line 3863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13088 "Parser/parser.cc"
    break;

  case 1002:
#line 3866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13094 "Parser/parser.cc"
    break;

  case 1003:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13100 "Parser/parser.cc"
    break;

  case 1005:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13106 "Parser/parser.cc"
    break;

  case 1006:
#line 3877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13112 "Parser/parser.cc"
    break;

  case 1008:
#line 3880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13118 "Parser/parser.cc"
    break;

  case 1009:
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13124 "Parser/parser.cc"
    break;

  case 1011:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13130 "Parser/parser.cc"
    break;

  case 1012:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13136 "Parser/parser.cc"
    break;

  case 1013:
#line 3895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13142 "Parser/parser.cc"
    break;

  case 1014:
#line 3897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13148 "Parser/parser.cc"
    break;

  case 1015:
#line 3899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13154 "Parser/parser.cc"
    break;

  case 1016:
#line 3901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13160 "Parser/parser.cc"
    break;

  case 1017:
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13166 "Parser/parser.cc"
    break;

  case 1020:
#line 3942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( ast::CV::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13172 "Parser/parser.cc"
    break;

  case 1021:
#line 3944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13178 "Parser/parser.cc"
    break;

  case 1022:
#line 3946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13184 "Parser/parser.cc"
    break;

  case 1023:
#line 3951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13190 "Parser/parser.cc"
    break;

  case 1024:
#line 3953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13196 "Parser/parser.cc"
    break;

  case 1025:
#line 3955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13202 "Parser/parser.cc"
    break;

  case 1026:
#line 3957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13208 "Parser/parser.cc"
    break;

  case 1027:
#line 3959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13214 "Parser/parser.cc"
    break;

  case 1029:
#line 3965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13220 "Parser/parser.cc"
    break;

  case 1030:
#line 3967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13226 "Parser/parser.cc"
    break;

  case 1031:
#line 3969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13232 "Parser/parser.cc"
    break;

  case 1032:
#line 3974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13238 "Parser/parser.cc"
    break;

  case 1033:
#line 3976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13244 "Parser/parser.cc"
    break;

  case 1034:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13250 "Parser/parser.cc"
    break;

  case 1036:
#line 3985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13256 "Parser/parser.cc"
    break;

  case 1038:
#line 3996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13262 "Parser/parser.cc"
    break;

  case 1039:
#line 3999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13268 "Parser/parser.cc"
    break;

  case 1040:
#line 4001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13274 "Parser/parser.cc"
    break;

  case 1041:
#line 4004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13280 "Parser/parser.cc"
    break;

  case 1042:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13286 "Parser/parser.cc"
    break;

  case 1043:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13292 "Parser/parser.cc"
    break;

  case 1045:
#line 4023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13298 "Parser/parser.cc"
    break;

  case 1046:
#line 4025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13304 "Parser/parser.cc"
    break;

  case 1047:
#line 4030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13310 "Parser/parser.cc"
    break;

  case 1048:
#line 4032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13316 "Parser/parser.cc"
    break;

  case 1049:
#line 4034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13322 "Parser/parser.cc"
    break;

  case 1050:
#line 4036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13328 "Parser/parser.cc"
    break;

  case 1051:
#line 4038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13334 "Parser/parser.cc"
    break;

  case 1053:
#line 4044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13340 "Parser/parser.cc"
    break;

  case 1054:
#line 4046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13346 "Parser/parser.cc"
    break;

  case 1055:
#line 4048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13352 "Parser/parser.cc"
    break;

  case 1056:
#line 4053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13358 "Parser/parser.cc"
    break;

  case 1057:
#line 4055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13364 "Parser/parser.cc"
    break;

  case 1060:
#line 4065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13370 "Parser/parser.cc"
    break;

  case 1063:
#line 4076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13376 "Parser/parser.cc"
    break;

  case 1064:
#line 4078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13382 "Parser/parser.cc"
    break;

  case 1065:
#line 4080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13388 "Parser/parser.cc"
    break;

  case 1066:
#line 4082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13394 "Parser/parser.cc"
    break;

  case 1067:
#line 4084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13400 "Parser/parser.cc"
    break;

  case 1068:
#line 4086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13406 "Parser/parser.cc"
    break;

  case 1069:
#line 4093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13412 "Parser/parser.cc"
    break;

  case 1070:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13418 "Parser/parser.cc"
    break;

  case 1071:
#line 4097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13424 "Parser/parser.cc"
    break;

  case 1072:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13430 "Parser/parser.cc"
    break;

  case 1073:
#line 4101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13436 "Parser/parser.cc"
    break;

  case 1074:
#line 4104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13442 "Parser/parser.cc"
    break;

  case 1075:
#line 4106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13448 "Parser/parser.cc"
    break;

  case 1076:
#line 4108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13454 "Parser/parser.cc"
    break;

  case 1077:
#line 4110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13460 "Parser/parser.cc"
    break;

  case 1078:
#line 4112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13466 "Parser/parser.cc"
    break;

  case 1079:
#line 4117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13472 "Parser/parser.cc"
    break;

  case 1080:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13478 "Parser/parser.cc"
    break;

  case 1081:
#line 4124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13484 "Parser/parser.cc"
    break;

  case 1082:
#line 4126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 13490 "Parser/parser.cc"
    break;

  case 1084:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13496 "Parser/parser.cc"
    break;

  case 1088:
#line 4164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13502 "Parser/parser.cc"
    break;

  case 1089:
#line 4166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13508 "Parser/parser.cc"
    break;

  case 1090:
#line 4168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13514 "Parser/parser.cc"
    break;

  case 1091:
#line 4170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13520 "Parser/parser.cc"
    break;

  case 1092:
#line 4172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13526 "Parser/parser.cc"
    break;

  case 1093:
#line 4174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13532 "Parser/parser.cc"
    break;

  case 1094:
#line 4181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13538 "Parser/parser.cc"
    break;

  case 1095:
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13544 "Parser/parser.cc"
    break;

  case 1096:
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13550 "Parser/parser.cc"
    break;

  case 1097:
#line 4187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13556 "Parser/parser.cc"
    break;

  case 1098:
#line 4189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13562 "Parser/parser.cc"
    break;

  case 1099:
#line 4191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13568 "Parser/parser.cc"
    break;

  case 1100:
#line 4196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13574 "Parser/parser.cc"
    break;

  case 1101:
#line 4198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13580 "Parser/parser.cc"
    break;

  case 1102:
#line 4200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13586 "Parser/parser.cc"
    break;

  case 1103:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13592 "Parser/parser.cc"
    break;

  case 1104:
#line 4209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13598 "Parser/parser.cc"
    break;

  case 1107:
#line 4233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13604 "Parser/parser.cc"
    break;

  case 1108:
#line 4235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13610 "Parser/parser.cc"
    break;


#line 13614 "Parser/parser.cc"

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
#line 4238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
