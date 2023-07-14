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

#include "SynTree/Type.h"                               // for Type
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

#line 328 "Parser/parser.cc"

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
    DISABLE = 348,
    ENABLE = 349,
    TRY = 350,
    THROW = 351,
    THROWRESUME = 352,
    AT = 353,
    ASM = 354,
    ALIGNAS = 355,
    ALIGNOF = 356,
    GENERIC = 357,
    STATICASSERT = 358,
    IDENTIFIER = 359,
    TYPEDIMname = 360,
    TYPEDEFname = 361,
    TYPEGENname = 362,
    TIMEOUT = 363,
    WAND = 364,
    WOR = 365,
    CATCH = 366,
    RECOVER = 367,
    CATCHRESUME = 368,
    FIXUP = 369,
    FINALLY = 370,
    INTEGERconstant = 371,
    CHARACTERconstant = 372,
    STRINGliteral = 373,
    DIRECTIVE = 374,
    FLOATING_DECIMALconstant = 375,
    FLOATING_FRACTIONconstant = 376,
    FLOATINGconstant = 377,
    ARROW = 378,
    ICR = 379,
    DECR = 380,
    LS = 381,
    RS = 382,
    LE = 383,
    GE = 384,
    EQ = 385,
    NE = 386,
    ANDAND = 387,
    OROR = 388,
    ELLIPSIS = 389,
    EXPassign = 390,
    MULTassign = 391,
    DIVassign = 392,
    MODassign = 393,
    PLUSassign = 394,
    MINUSassign = 395,
    LSassign = 396,
    RSassign = 397,
    ANDassign = 398,
    ERassign = 399,
    ORassign = 400,
    ErangeUpEq = 401,
    ErangeDown = 402,
    ErangeDownEq = 403,
    ATassign = 404,
    THEN = 405
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
#define DISABLE 348
#define ENABLE 349
#define TRY 350
#define THROW 351
#define THROWRESUME 352
#define AT 353
#define ASM 354
#define ALIGNAS 355
#define ALIGNOF 356
#define GENERIC 357
#define STATICASSERT 358
#define IDENTIFIER 359
#define TYPEDIMname 360
#define TYPEDEFname 361
#define TYPEGENname 362
#define TIMEOUT 363
#define WAND 364
#define WOR 365
#define CATCH 366
#define RECOVER 367
#define CATCHRESUME 368
#define FIXUP 369
#define FINALLY 370
#define INTEGERconstant 371
#define CHARACTERconstant 372
#define STRINGliteral 373
#define DIRECTIVE 374
#define FLOATING_DECIMALconstant 375
#define FLOATING_FRACTIONconstant 376
#define FLOATINGconstant 377
#define ARROW 378
#define ICR 379
#define DECR 380
#define LS 381
#define RS 382
#define LE 383
#define GE 384
#define EQ 385
#define NE 386
#define ANDAND 387
#define OROR 388
#define ELLIPSIS 389
#define EXPassign 390
#define MULTassign 391
#define DIVassign 392
#define MODassign 393
#define PLUSassign 394
#define MINUSassign 395
#define LSassign 396
#define RSassign 397
#define ANDassign 398
#define ERassign 399
#define ORassign 400
#define ErangeUpEq 401
#define ErangeDown 402
#define ErangeDownEq 403
#define ATassign 404
#define THEN 405

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 702 "Parser/parser.cc"

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
#define YYLAST   23932

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  178
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  309
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1107
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2190

#define YYUNDEFTOK  2
#define YYMAXUTOK   405


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
       2,     2,     2,   167,     2,     2,     2,   171,   164,     2,
     152,   154,   163,   165,   158,   166,   155,   170,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   159,   177,
     172,   176,   173,   175,   153,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   156,   169,   157,   162,     2,   161,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   160,   174,   151,   168,     2,     2,     2,
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
     145,   146,   147,   148,   149,   150
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   607,   607,   611,   618,   619,   620,   621,   622,   626,
     627,   628,   629,   630,   631,   632,   633,   637,   638,   642,
     643,   648,   652,   653,   664,   666,   668,   672,   673,   675,
     677,   679,   681,   691,   693,   695,   697,   699,   701,   706,
     707,   718,   723,   728,   729,   734,   740,   742,   744,   750,
     752,   756,   758,   760,   762,   764,   766,   768,   770,   772,
     774,   776,   778,   780,   782,   784,   786,   796,   797,   801,
     802,   807,   810,   814,   815,   819,   820,   822,   824,   826,
     828,   830,   835,   837,   839,   847,   848,   856,   859,   860,
     862,   867,   883,   885,   887,   889,   891,   893,   895,   897,
     899,   907,   908,   910,   914,   915,   916,   917,   921,   922,
     924,   926,   928,   930,   932,   934,   936,   943,   944,   945,
     946,   950,   951,   955,   956,   961,   962,   964,   966,   971,
     972,   974,   979,   980,   982,   987,   988,   990,   992,   994,
     999,  1000,  1002,  1007,  1008,  1013,  1014,  1019,  1020,  1025,
    1026,  1031,  1032,  1037,  1038,  1041,  1046,  1051,  1052,  1060,
    1066,  1067,  1071,  1072,  1076,  1077,  1081,  1082,  1083,  1084,
    1085,  1086,  1087,  1088,  1089,  1090,  1091,  1101,  1103,  1108,
    1109,  1111,  1113,  1118,  1119,  1125,  1126,  1132,  1133,  1134,
    1135,  1136,  1137,  1138,  1139,  1140,  1141,  1142,  1143,  1145,
    1146,  1152,  1154,  1164,  1166,  1174,  1175,  1180,  1182,  1184,
    1186,  1188,  1192,  1193,  1195,  1200,  1207,  1209,  1211,  1221,
    1223,  1225,  1230,  1235,  1238,  1243,  1245,  1247,  1249,  1257,
    1258,  1260,  1264,  1266,  1270,  1272,  1273,  1275,  1277,  1282,
    1283,  1287,  1292,  1293,  1297,  1299,  1304,  1306,  1311,  1313,
    1315,  1317,  1322,  1324,  1326,  1328,  1333,  1335,  1340,  1341,
    1363,  1365,  1370,  1373,  1375,  1378,  1380,  1383,  1385,  1390,
    1395,  1397,  1402,  1407,  1409,  1411,  1413,  1415,  1418,  1420,
    1423,  1425,  1430,  1436,  1439,  1441,  1446,  1452,  1454,  1459,
    1465,  1468,  1470,  1473,  1475,  1480,  1487,  1489,  1494,  1500,
    1502,  1507,  1513,  1516,  1521,  1531,  1533,  1535,  1540,  1542,
    1547,  1548,  1550,  1555,  1557,  1562,  1564,  1566,  1568,  1571,
    1575,  1578,  1582,  1584,  1586,  1588,  1590,  1592,  1594,  1596,
    1598,  1600,  1602,  1607,  1608,  1612,  1618,  1626,  1631,  1632,
    1636,  1637,  1643,  1647,  1648,  1651,  1653,  1658,  1661,  1663,
    1665,  1668,  1670,  1675,  1680,  1681,  1685,  1690,  1692,  1697,
    1699,  1704,  1706,  1708,  1710,  1713,  1715,  1723,  1732,  1734,
    1736,  1741,  1743,  1749,  1750,  1754,  1755,  1756,  1757,  1761,
    1766,  1767,  1769,  1771,  1773,  1777,  1781,  1782,  1786,  1788,
    1790,  1792,  1794,  1800,  1801,  1807,  1808,  1812,  1813,  1818,
    1820,  1829,  1830,  1832,  1837,  1842,  1853,  1854,  1858,  1859,
    1865,  1866,  1870,  1872,  1876,  1878,  1882,  1883,  1887,  1888,
    1892,  1899,  1900,  1904,  1906,  1921,  1922,  1923,  1924,  1926,
    1930,  1932,  1936,  1943,  1945,  1947,  1952,  1953,  1955,  1957,
    1959,  1991,  1994,  1999,  2001,  2007,  2012,  2017,  2028,  2035,
    2040,  2042,  2044,  2050,  2054,  2061,  2063,  2064,  2065,  2081,
    2083,  2086,  2088,  2091,  2096,  2097,  2101,  2102,  2103,  2104,
    2114,  2115,  2116,  2125,  2126,  2127,  2131,  2132,  2133,  2142,
    2143,  2144,  2149,  2150,  2159,  2160,  2165,  2166,  2170,  2172,
    2174,  2176,  2178,  2183,  2188,  2189,  2191,  2201,  2202,  2207,
    2209,  2211,  2213,  2215,  2217,  2220,  2222,  2224,  2229,  2231,
    2233,  2235,  2237,  2239,  2241,  2243,  2245,  2247,  2249,  2251,
    2253,  2255,  2257,  2259,  2261,  2263,  2265,  2267,  2269,  2271,
    2273,  2275,  2277,  2279,  2281,  2283,  2288,  2289,  2293,  2300,
    2301,  2307,  2308,  2310,  2312,  2314,  2319,  2321,  2326,  2327,
    2329,  2331,  2336,  2338,  2340,  2342,  2344,  2346,  2351,  2358,
    2360,  2362,  2367,  2375,  2374,  2378,  2386,  2387,  2389,  2391,
    2396,  2397,  2399,  2404,  2405,  2407,  2409,  2414,  2415,  2417,
    2422,  2424,  2426,  2428,  2429,  2431,  2436,  2438,  2440,  2445,
    2452,  2456,  2457,  2462,  2461,  2466,  2465,  2475,  2474,  2485,
    2484,  2494,  2499,  2500,  2505,  2511,  2529,  2530,  2534,  2536,
    2538,  2544,  2546,  2548,  2550,  2555,  2557,  2562,  2564,  2573,
    2574,  2579,  2588,  2593,  2595,  2597,  2606,  2608,  2609,  2610,
    2612,  2614,  2615,  2620,  2621,  2622,  2627,  2629,  2632,  2635,
    2642,  2643,  2644,  2650,  2655,  2657,  2663,  2664,  2670,  2671,
    2675,  2680,  2682,  2685,  2684,  2688,  2690,  2697,  2699,  2703,
    2706,  2705,  2716,  2720,  2724,  2728,  2733,  2734,  2739,  2744,
    2752,  2754,  2756,  2758,  2763,  2764,  2770,  2771,  2772,  2779,
    2780,  2782,  2783,  2784,  2786,  2788,  2795,  2796,  2798,  2800,
    2805,  2806,  2812,  2813,  2815,  2816,  2821,  2822,  2823,  2825,
    2833,  2834,  2836,  2839,  2841,  2845,  2846,  2847,  2849,  2851,
    2856,  2858,  2863,  2865,  2874,  2876,  2881,  2882,  2883,  2887,
    2888,  2889,  2894,  2895,  2900,  2901,  2902,  2903,  2907,  2908,
    2913,  2914,  2915,  2916,  2917,  2931,  2932,  2937,  2938,  2944,
    2946,  2949,  2951,  2953,  2976,  2977,  2983,  2984,  2990,  2989,
    2999,  2998,  3002,  3008,  3014,  3015,  3017,  3021,  3026,  3028,
    3030,  3032,  3038,  3039,  3043,  3044,  3049,  3051,  3058,  3060,
    3061,  3063,  3068,  3070,  3072,  3077,  3079,  3084,  3089,  3097,
    3102,  3104,  3109,  3114,  3115,  3120,  3121,  3125,  3126,  3127,
    3132,  3134,  3140,  3142,  3147,  3149,  3155,  3156,  3160,  3164,
    3168,  3170,  3183,  3185,  3187,  3189,  3191,  3193,  3195,  3196,
    3201,  3204,  3203,  3215,  3214,  3227,  3226,  3240,  3239,  3253,
    3252,  3268,  3274,  3276,  3282,  3283,  3294,  3301,  3306,  3312,
    3315,  3318,  3322,  3328,  3331,  3334,  3339,  3340,  3341,  3342,
    3346,  3352,  3353,  3363,  3364,  3368,  3369,  3374,  3379,  3380,
    3386,  3387,  3389,  3394,  3395,  3396,  3397,  3398,  3400,  3435,
    3437,  3442,  3444,  3445,  3447,  3452,  3454,  3456,  3458,  3463,
    3465,  3467,  3469,  3471,  3473,  3475,  3480,  3482,  3484,  3486,
    3495,  3497,  3498,  3503,  3505,  3507,  3509,  3511,  3516,  3518,
    3520,  3522,  3527,  3529,  3531,  3533,  3535,  3537,  3549,  3550,
    3551,  3555,  3557,  3559,  3561,  3563,  3568,  3570,  3572,  3574,
    3579,  3581,  3583,  3585,  3587,  3589,  3601,  3606,  3611,  3613,
    3614,  3616,  3621,  3623,  3625,  3627,  3632,  3634,  3636,  3638,
    3640,  3642,  3644,  3649,  3651,  3653,  3655,  3664,  3666,  3667,
    3672,  3674,  3676,  3678,  3680,  3685,  3687,  3689,  3691,  3696,
    3698,  3700,  3702,  3704,  3706,  3716,  3718,  3720,  3721,  3723,
    3728,  3730,  3732,  3737,  3739,  3741,  3743,  3748,  3750,  3752,
    3766,  3768,  3770,  3771,  3773,  3778,  3780,  3785,  3787,  3789,
    3794,  3796,  3801,  3803,  3820,  3821,  3823,  3828,  3830,  3832,
    3834,  3836,  3841,  3842,  3844,  3846,  3851,  3853,  3855,  3861,
    3863,  3866,  3869,  3871,  3875,  3877,  3879,  3880,  3882,  3884,
    3888,  3890,  3895,  3897,  3899,  3901,  3936,  3937,  3941,  3942,
    3944,  3946,  3951,  3953,  3955,  3957,  3959,  3964,  3965,  3967,
    3969,  3974,  3976,  3978,  3984,  3985,  3987,  3996,  3999,  4001,
    4004,  4006,  4008,  4022,  4023,  4025,  4030,  4032,  4034,  4036,
    4038,  4043,  4044,  4046,  4048,  4053,  4055,  4063,  4064,  4065,
    4070,  4071,  4076,  4078,  4080,  4082,  4084,  4086,  4093,  4095,
    4097,  4099,  4101,  4104,  4106,  4108,  4110,  4112,  4117,  4119,
    4121,  4126,  4152,  4153,  4155,  4159,  4160,  4164,  4166,  4168,
    4170,  4172,  4174,  4181,  4183,  4185,  4187,  4189,  4191,  4196,
    4198,  4200,  4207,  4209,  4227,  4229,  4234,  4235
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
  "WAITUNTIL", "DISABLE", "ENABLE", "TRY", "THROW", "THROWRESUME", "AT",
  "ASM", "ALIGNAS", "ALIGNOF", "GENERIC", "STATICASSERT", "IDENTIFIER",
  "TYPEDIMname", "TYPEDEFname", "TYPEGENname", "TIMEOUT", "WAND", "WOR",
  "CATCH", "RECOVER", "CATCHRESUME", "FIXUP", "FINALLY", "INTEGERconstant",
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
  "waituntil_statement", "exception_statement", "handler_clause",
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
     405,   125,    40,    64,    41,    46,    91,    93,    44,    58,
     123,    96,    94,    42,    38,    43,    45,    33,   126,    92,
      47,    37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1912)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1106)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      64, 11993,    90,   127, 17947,    58, -1912, -1912, -1912, -1912,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,    27,  1229,
      36, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,    75,   162,
   -1912, -1912, -1912, -1912, -1912, -1912,  4317,  4317,   165, 11993,
     193,   199,  9536, -1912,   205, -1912, -1912, -1912, -1912, -1912,
   -1912, -1912, -1912, -1912, -1912,  2270, -1912,   615,   240, -1912,
   -1912, -1912, -1912, -1912, 17795, -1912, -1912,    87,   289,   403,
      43, -1912,  4317,   289,   316,   327,   279,  4874,   485,   967,
   12155, -1912, -1912,   492, 17643,  2285, -1912, -1912, -1912,  3349,
     583,  5532,  9440,   989,  3349,  1111,   443, -1912, -1912, -1912,
   -1912,   623, -1912, -1912, -1912, -1912,   539, -1912, -1912, -1912,
   -1912, -1912,   593,   594,   623, -1912,   623, 16084, -1912, -1912,
   -1912, 18790,  4317, -1912, -1912,  4317, -1912, 11993, -1912,   580,
   18942, -1912, -1912,  5265, 20363, -1912, -1912,   928,   928,   611,
    1604, -1912, -1912, -1912, -1912,   676, 14695,  2796,   623, -1912,
   -1912, -1912, -1912, -1912, -1912,   622, -1912,   632,   657,   681,
   -1912,   763, 23156, -1912, -1912, -1912, -1912, -1912, -1912, -1912,
   16501,  3402,  2270,   101,   751,   754,   792,   794,   806,   808,
   -1912, -1912, 19094, 10934,   826, -1912, 18244, -1912, -1912, -1912,
   -1912,   834, -1912, -1912,   835, -1912, 21232,   974, 21380, -1912,
     855,  4317,   594,   866,   841,  5265,  1191, -1912, -1912, -1912,
    3809,  4563,   869,   942,    91,   942, -1912,   623,   623,    78,
   15874,   349,   942, -1912,   623,   623,    78,   623, -1912,   623,
   -1912,  4797, -1912, -1912,   892,   925,   928, 20559, -1912, 17795,
   -1912, -1912,  3349, -1912,  2297,   443,   935,  1008, 15874,  4317,
    4317,   403, -1912, 13890, -1912,   928,   928,   943,  1008, 15874,
    4317, -1912,  8751, -1912, -1912, -1912,   928, -1912, -1912, -1912,
   -1912,   928, -1912,   827,  3883,  4317, -1912, 17500,   959, -1912,
   -1912, -1912, 20958,   594, 15979,   936,  5265, 17395, 20559,  3349,
   -1912, -1912, 20508, -1912,   942,    13, -1912, 23156, 20363,  3954,
    4797, -1912,   445, -1912, -1912, -1912, -1912, -1912, 18942,  4317,
   -1912,   970, -1912, -1912, -1912, -1912,  4317,  3377,   680,   527,
   -1912,  4317,   632, -1912,   898,   623,   623,   964, 19246,   906,
   15178, 21061,  3349, -1912,  3349,   928,  3349,   928, -1912, -1912,
     623, -1912,   981, -1912, 19398, -1912, -1912, -1912, 19550,   834,
   -1912,   148,   361,    49,   978,   625,   443,   982, -1912,  1604,
     985,   632,  1604,  2505, -1912,   997,  1069, 23230,  1040,  1042,
    1049, 23156, 23304,  1060, 23765, -1912, -1912, -1912, -1912, -1912,
   -1912, 23378, 23378, 16345,  1006,  3418, -1912, -1912, -1912, -1912,
     647, -1912,   741, -1912,  1120, -1912, 23156, 23156, -1912,  1050,
     876,  1098,  1212,   111,  1233,  1068,  1062,  1074,  1119,     9,
   -1912,   555, -1912,  1128, -1912,  1242,  4315, 16813, -1912, -1912,
    1002,  1128, -1912, -1912,   637, -1912, -1912,  3402,  1099,  1138,
    1140,  1143,  1153,  1156, -1912, -1912,   462,  1162, -1912,   693,
    1162, -1912, -1912, 18790, -1912,  1267,  1163, 16969, -1912, -1912,
    4544,  4576,  1199, 15178,  1211,   764,   915, -1912, -1912, -1912,
   -1912, -1912,  4317,  4649, -1912, -1912, -1912, -1912, -1912, -1912,
   17290,  4114,  1006, 21232,  1202,  1214, -1912, -1912,  1220, 21380,
     865, -1912, -1912, -1912, 21454,  1259, -1912, -1912, -1912, -1912,
    1205,  3809,   911,  1265,  1296,  1309,   952,  1314,  1316,  1326,
    1328,  1340,  1342,  4563, -1912, -1912, -1912,   623,  1261,  1335,
    1264, -1912, -1912,  1359,   403, -1912, -1912,   594,  1008, 18108,
   -1912, -1912,   403, -1912, -1912,   594, -1912, -1912,  4797, -1912,
   16813, 16813, -1912,   928,  5265,  6747, 15339, -1912, -1912, -1912,
   -1912, -1912,   594,  1008,    13,  1368, -1912, -1912,  3349,  1374,
    1008, 15874, -1912,   594,  1008, -1912, 23860, -1912,   928,   928,
   -1912, -1912,  1409,   107,  1411,   443,  1424, -1912, -1912, -1912,
   18539,  1394,  1438, -1912, -1912,   699, -1912,  1512, -1912,  1425,
   -1912, -1912, -1912, 19711, 23452, -1912, -1912, -1912, -1912, -1912,
    3954,   990,  4797, 18108, 15339,   942, 11993, -1912,  1458, -1912,
    1466, -1912, -1912, -1912, -1912, -1912,  1604, -1912, -1912,  1543,
    4026,  2826, 19550, 10934, -1912, 19762, -1912,   928,   928, -1912,
   -1912,   834, -1912, 14212,  1468,  1613, 23156,   923,  1359,  1457,
   -1912,   623,   623, -1912,  1162, -1912, 19246, -1912, -1912, 18539,
     928,   928, -1912,  4026,   623, -1912, 20218, -1912, -1912, 19398,
   -1912,   676, -1912, -1912, -1912,  1481,  4317,    49,   982,  1482,
     766, 18942,   934, -1912, -1912, -1912, -1912, -1912, -1912,   940,
   -1912,  1489,  1469, -1912, 16657, -1912,  3418, 19914, 19914, -1912,
   16657, -1912, 23156, -1912, -1912, -1912, -1912, -1912, -1912, 16657,
   -1912, -1912,  7908, 19914, 19914,  1242,  1313,  1353,   847,  1377,
   -1912,   961,  1491,  1266,  1494, -1912, 21454, 23156, 21528,  1490,
   23156,  1191, 23156,  1191, -1912,  2928, -1912, -1912, 21602,  2948,
   23156, 21602,  1191, -1912, -1912, 23156, 23156, 23156, 23156, 23156,
   23156, 23156, 23156, 23156, 23156, 23156, 23156, 23156, 23156, 23156,
   23156, 23156, 23156, 23156, 21676,  1477,   763,  3539, 10934, -1912,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,
    1496, 23156, -1912, -1912, 14373,   953, -1912, -1912,   623,   623,
   -1912, -1912, 16813, -1912,   538,  1162, -1912,   908,  1162, 18108,
   -1912, -1912,  1359, 18108, -1912,  1359, 23526, -1912, -1912, 10934,
    1492,  1500, 13729,  1643,  4080,   551,  1457, -1912,   623,   623,
    1457,   617, -1912,   623,   623, 23156,  4317,  1284,  1287,  1457,
     196, 14534, 14534,  4317, -1912, -1912, 23156,  1220, -1912, 21232,
    1514, -1912,  3165, -1912, -1912, -1912, -1912, -1912,   975, -1912,
   14534,  1191, 23156,  1016,  1509,  1513,  1522,  1017,  1523,  1526,
    1527,  1528,  1532,  1533,   665,  1162, -1912, -1912,   671,  1162,
   -1912, -1912,   672,  1162, -1912, -1912, -1912,  5265,   763,  1670,
    1162, 20653, -1912, -1912,   594,  1540, -1912, -1912, -1912,  1030,
    1541,  1046,  1545, -1912,  1549, -1912,   594, -1912,  1550, -1912,
     594,  1008,  1549, -1912,   594,  1542,  1547,  1560, -1912, -1912,
   18405, -1912,  1191,  4317, 10078,  1633, -1912,  1163, -1912, 14534,
    1051, -1912, -1912,  1549, -1912, 18942, 16813,  1529, -1912,  1529,
   -1912, -1912, -1912,    49,  1561,   623,   623, -1912, 19398, -1912,
   11099, 17125, -1912,  1570,  1581,  1582,  1584, -1912,  9222,   623,
   -1912,   923, -1912, -1912, -1912, -1912,  1359, -1912, -1912, -1912,
     928, -1912,  3129, -1912, -1912,   443,   501,  1589,  1565,  1481,
    1583,    49, -1912, -1912,  1585,  1588,  2505, 21602, -1912,  1590,
    1591,   240,  1592,  1593,  1599,  1598,  1605, 23156,  1607,  1609,
    1611, 10934, 23156, -1912, -1912,  1472, -1912, -1912, -1912, 23156,
   -1912,  1612,  1614, 21306,  1297, -1912, 21602,  1601, -1912,  1610,
   -1912, -1912,  3967, -1912, -1912,  1064, -1912, -1912, -1912, -1912,
    3967, -1912, -1912,  1320,   697, -1912, -1912,  1050,  1050,  1050,
     876,   876,  1098,  1098,  1212,  1212,  1212,  1212,   111,   111,
    1233,  1068,  1062,  1074,  1119, 23156,  1351, -1912,  1616,  3967,
   -1912, -1912, 21232, -1912,  1622,  1623,  1625,  1626,   953, -1912,
   -1912, -1912, -1912, -1912, 18108, -1912, -1912,  1359, 18108, -1912,
    1359,  1627,  1629, -1912, -1912, 13729,  1059,  1631,  1634,  1636,
    1638,  1619,  4080, -1912, -1912, 18108, -1912, -1912, -1912, -1912,
   -1912, -1912, 18108, -1912, -1912, -1912, -1912,  1630, -1912,  1457,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,  1639,  1644,
   -1912,   403,  3967,  1356,   112, -1912, -1912,  1648, -1912, 21380,
   -1912, 23156,   623, 21750, 14534, -1912, -1912, -1912,   683,  1162,
   -1912,   714,  1162, -1912, -1912,   716,  1162, 18108, -1912, -1912,
    1359, 18108, -1912, -1912,  1359, 18108, -1912, -1912,  1359,   942,
    1647, -1912,  1359,   158, -1912,  1128,  1645, -1912, -1912, -1912,
   -1912, -1912, -1912,  1651, -1912, -1912, -1912, 18942,  1549, -1912,
     594, -1912, -1912, -1912, -1912, -1912, 12796, -1912, -1912, -1912,
   -1912,   261, -1912,   612,   391, 10769,  1653, 15699,  1654,  1656,
    2585,  2619,  3290, 21824,  1658, -1912, -1912,  1659,  1660, -1912,
   -1912,   594, 23156, 23156,  1800,  1657,   725, -1912, 16189,  1743,
    1661,  1641, -1912, -1912, -1912,  9903, -1912, -1912, -1912, -1912,
   -1912,  1391, -1912, -1912, -1912,  1426,   142, -1912,   298, -1912,
     142, -1912, -1912, -1912,  1191, -1912, -1912, 12317, 17795,  1662,
   -1912,  4317,  1665,  1668, -1912,  1362, -1912, -1912, -1912, -1912,
    5265, -1912, -1912,  1649,  1652,  1071, 18942,   632,   632,  1481,
      49,   982,   982, -1912, -1912,  1006,  1163, 16969, -1912,  1128,
   -1912, 11264, -1912,   722,  1162, -1912,   928, 11827, -1912, -1912,
      49,  1672,   623,   623,   676,  4317, -1912, 21898, -1912,  1677,
      49,  1481,  1679, -1912, -1912,  1076,   811, 18539, 10934,  1191,
   -1912,   811, 18638,   811, -1912, 23156, 23156, 23156, -1912, -1912,
   -1912, -1912, 23156, 23156,  1673, 21232, -1912, -1912,  1676,   830,
   -1912, -1912, -1912,  3015, -1912, -1912,  1364, -1912,   220, -1912,
   21602,  1367, -1912, 21454, -1912, -1912, 23156,  1669,  1369,  1373,
    1220, -1912,   733,  1162, -1912, -1912,  1681,  1684, -1912, -1912,
    1685,   743,  1162, -1912,   744,  2533,   623,   623, -1912, -1912,
    1688,  1691, -1912,  1694, -1912, 15339, 15339,  1702,  1699,  1704,
    1710, -1912,  1707, 23156, 23156,  1375,  1709, -1912, -1912, -1912,
   -1912, -1912, -1912, -1912,  1713, 18108, -1912, -1912,  1359, 18108,
   -1912, -1912,  1359, 18108, -1912, -1912,  1359,  1714,  1716,  1717,
     403,   623, -1912, -1912,  1381, 23156, 20802,  1715,  1722, -1912,
   -1912, -1912,  1724, 12951, 13106, 13261, 18942, 20559, 19914, 19914,
    1725, -1912,  1706,   317,  3316, 13568, -1912,   423,  4317,  4317,
   -1912, 21602,   571,   586, -1912, -1912, -1912, -1912, 23156,  1729,
    1796, 10603, 10253, -1912,  1708, -1912,  1718, 23156,  1726, 21232,
    1727, 23156, 21454, 23156,  1324, -1912,  1731,    59, -1912,    37,
    1792,   550,  1734, -1912, -1912,  1736, -1912,  1732, -1912,  1733,
    1738,  1739, 15699, 15699, -1912, -1912,  1804, -1912, -1912,    83,
      83,   219, 14051,   623,   477, -1912, -1912,  1748, -1912,  1755,
   -1912,  1760, -1912,  1758, -1912,  1761, -1912, -1912, -1912, -1912,
    1769,  1481,  1765,  1766, 11429,  1764,  1771,  1772, -1912, 18108,
   -1912, -1912,  1359, 23156, 23156,  1163,  1775, -1912,  1481,    49,
   -1912,   982,   209,  1565, 21232, -1912, -1912,  1481,  1782, -1912,
   18942, -1912,  1007,  1785,  1777,  1077, -1912,  1784, -1912, -1912,
   -1912, -1912, -1912, 21232,  1220, 21454, -1912,  1824,  3967, -1912,
    1824,  1824, -1912,  3967,  3626,  3689, -1912, -1912,  1383, -1912,
   -1912, -1912,  1797, 18108, -1912, -1912,  1359, -1912, -1912,   623,
   18108, -1912, -1912,  1359, 18108, -1912, -1912,  1793, -1912, -1912,
   -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912, -1912,
   -1912,  1794, -1912, -1912, -1912, -1912,  1798,  1802,   623,  1803,
    1806,  1809, -1912, -1912, -1912, -1912, -1912, 23156, -1912,   158,
   -1912,  1128, -1912, -1912,  1799,  1807, -1912,  1725,  1725,  1725,
    4712,  1053,  1787,   478, -1912,  4712,   495, 16813, -1912, -1912,
   -1912,  4053, 23156,  5136,   259, -1912, -1912,    77,  1795,  1795,
    1795,  4317, -1912, -1912, -1912,  1088, -1912, -1912, -1912, -1912,
    1112,  1813, 15699,  1661,  1814, 23156,    87,  1822,   279, 13423,
   18942, -1912, -1912, -1912,   882, 15699, 23156,   954,   823, -1912,
   23156, 21078, -1912, -1912,   541, -1912,  1220, -1912,  1115,  1124,
    1131, -1912, -1912, -1912, -1912,   594,  1324,  1815, -1912, -1912,
   23156, -1912,  1819,   763, -1912, 10769, -1912, -1912, -1912, -1912,
   23156, 23156, -1912, -1912,   295,    83, -1912,   491, -1912, -1912,
    9728, -1912,   623, 15339, -1912, -1912, 18942, -1912, -1912, -1912,
    1833,    49,    49, -1912, -1912, -1912,  1828,  1832, -1912, -1912,
    1831, -1912,  1834,  1844,  1481,   982,  1829, -1912, -1912,  1220,
    1847, -1912, -1912,  1845, -1912, -1912, 23156, -1912, 18638, 23156,
    1220,  1850,  1386, -1912,  1388, -1912,  3967, -1912,  3967, -1912,
   -1912, -1912,  1848,  1849,  1854,  1399, 14856, 15017, -1912,  1852,
   -1912, -1912, -1912, -1912, -1912,  1401, 23156, -1912, -1912, -1912,
   -1912, -1912,   547,  1053,  1245,   591, -1912, -1912, -1912, -1912,
     623,   623, -1912, -1912, -1912,   649, -1912,  1161,  4053,   383,
   -1912,  5136, -1912,   623, -1912, -1912, -1912, -1912, -1912, -1912,
   15699,   132, 21972,  1935, 15699,  1661, 15500, -1912, -1912, -1912,
   -1912, 23156, -1912, 22046,  1937,  1837, 21155, 22120, 15699, 10428,
    1661,   652,  1109,  1839, 23156, -1912,  1867,   468, 15699, -1912,
   -1912,  1870, -1912, -1912,  1851,   763,   883,  1871,  1875,  1406,
    1190, 15699,  1882, 15699, 15699, 15699, 15699, -1912, -1912, -1912,
   -1912,  4317,  5265, -1912,  1481,  1481, -1912, -1912,  1878,  1880,
   -1912, -1912, -1912,  1887,  1879,    49,  1890, -1912,  1894, -1912,
   -1912, -1912, -1912,  1897, -1912, -1912, -1912,  1416,  1419, -1912,
   -1912, -1912, -1912, -1912, -1912,  1899, -1912, -1912, -1912, -1912,
   -1912,  1904,  1907,  1908,  1245, -1912,   623, -1912, -1912, -1912,
   -1912, -1912,  1895,  4712, -1912,  7472,   139, 11597, -1912, 15594,
   -1912,    17,  1192, 15699,  1988,   729,  1896,   237, 15699, 23156,
    1917,   652,  1109,  1898, 23600,  1905,   375,  2001, -1912, 22194,
   22268, 23156,  1661,  1906, 11761, -1912, -1912, -1912, 20066, -1912,
    1923,  1909,   155, 15699, -1912, 23156, 21602, -1912, -1912, 23156,
     142, -1912, -1912,   142, -1912, -1912,  1938,  1939, -1912, -1912,
   -1912,    49,  1481, -1912, -1912, -1912, -1912, -1912, 15339,  1936,
     807,  1162, -1912, -1912,  1053, -1912, -1912,   144, -1912,   301,
   -1912, -1912, -1912,  1942, 12479, -1912, -1912, 15699, -1912,    35,
   -1912, 15699, 23156,  1943, 22342, -1912, -1912, 22416, 22490, 23156,
    1917,  1661, 22564, 22638, 15699,  1929,   426,  1940,   444, -1912,
   -1912,  1952, 12479, 20066, -1912,  5068, 19762,  1191,  1946, -1912,
    2002,  1957,   919,  1956, -1912,  2035, -1912,  1195,  1221,   386,
     408, -1912, -1912,  1481,  1961, -1912, 18108, -1912, -1912,  1359,
   -1912, 23156, -1912, 23156, -1912, -1912,  1501, 12641, -1912, -1912,
   15699, -1912, -1912,  1661, -1912, -1912,  1661,  1953,   475,  1958,
     519, -1912, -1912,  1661, -1912,  1661, -1912,  1967, 22712, 22786,
   22860, -1912,  1501, -1912,  1947,  3568,  4329, -1912, -1912, -1912,
     155,  1968, 23156,  1955,   155,   155, 15699, -1912, -1912, 15699,
    2053, 15699,  2059,  1986, -1912,  1989, -1912, -1912, 15594, -1912,
    1501, -1912, -1912,  1992, 22934, 23008, 23082, -1912, -1912,  1661,
   -1912,  1661, -1912,  1661, -1912,  1947, 23156,  1993,  4329,  1981,
     763,  1995, -1912,   930, -1912, -1912, -1912, 15699, -1912, 15699,
   -1912, -1912, -1912,  9089,  1990, 15594, -1912, -1912,  1661, -1912,
    1661, -1912,  1661,  1999,  2007, -1912,   594,   763,  2003, -1912,
    1976,   763, -1912, -1912, -1912, -1912, -1912,  9602, -1912,   594,
   -1912, -1912,  1422, 23156, -1912,  1222, -1912,   763,  1191,  2009,
    1979, -1912, -1912,  1244, -1912, -1912,  1994,  1191, -1912, -1912
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   482,     0,     2,   482,   499,   500,   501,   502,   503,
     504,   505,   506,   507,   488,   490,   489,   491,     0,     0,
       0,   508,   510,   531,   511,   532,   514,   515,   529,   530,
     509,   527,   528,   512,   513,   516,   517,   518,   519,   520,
     521,   522,   523,   524,   525,   526,   533,   534,   843,   536,
     609,   610,   613,   615,   611,   617,     0,     0,     0,   482,
       0,     0,    17,   580,   586,     9,    10,    11,    12,    13,
      14,    15,    16,   800,   103,     0,    20,     0,     2,   101,
     102,    18,    19,   859,   482,   801,   422,     0,   425,   724,
     427,   436,     0,   426,   456,   457,     0,     0,     0,     0,
     563,   484,   486,   492,   482,   494,   497,   548,   535,   466,
     541,   546,   468,   558,   467,   573,   577,   583,   562,   589,
     601,   843,   606,   607,   590,   665,   428,   429,     3,   808,
     821,   487,     0,     0,   843,   881,   843,   482,   898,   899,
     900,   482,     0,  1085,  1086,     0,     1,   482,    17,     0,
     482,   445,   446,     0,   563,   492,   476,   477,   478,   811,
       0,   612,   614,   616,   618,     0,   482,     0,   844,   845,
     608,   537,   717,   718,   716,   777,   772,   762,     0,     0,
     809,     0,     0,   499,   802,   806,   807,   803,   804,   805,
     482,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     581,   584,   482,   482,     0,  1087,   563,   888,   906,  1091,
    1084,  1082,  1089,   421,     0,   165,   730,   164,     0,   430,
       0,     0,     0,     0,     0,     0,     0,   420,   975,   976,
       0,     0,   455,   841,   843,   841,   862,   843,   843,   465,
     482,   843,   841,   919,   843,   843,   464,   843,   938,   843,
     916,     0,   556,   557,     0,     0,   482,   482,     2,   482,
     437,   485,   495,   549,     0,   578,     0,   824,   482,     0,
       0,   724,   438,   563,   542,   559,   574,     0,   824,   482,
       0,   498,   543,   550,   551,   469,   560,   471,   472,   470,
     565,   575,   579,     0,   593,     0,   794,   482,     2,   822,
     880,   882,   482,     0,   482,     0,     0,   563,   482,   494,
       2,  1095,   563,  1098,   841,   841,     3,     0,   563,     0,
       0,   448,   843,   836,   838,   837,   839,     2,   482,     0,
     798,     0,   758,   760,   759,   761,     0,     0,   754,     0,
     744,     0,   753,   764,     0,   843,   843,     2,   482,  1106,
     483,   482,   473,   541,   474,   566,   475,   573,   570,   591,
     843,   592,     0,   705,   482,   706,  1060,  1061,   482,   707,
     709,   580,   586,   666,     0,   668,   669,   666,   846,     0,
     775,   763,     0,   850,    22,     0,    21,     0,     0,     0,
       0,     0,     0,     0,    24,    26,     4,     8,     5,     6,
       7,     0,     0,   482,     2,     0,   104,   105,   106,   107,
      88,    25,    89,    43,    87,   108,     0,     0,   123,   125,
     129,   132,   135,   140,   143,   145,   147,   149,   151,   153,
     156,     0,    27,     0,   587,     2,   108,   482,   157,   769,
     720,   577,   722,   768,     0,   719,   723,     0,     0,     0,
       0,     0,     0,     0,   860,   886,   843,   896,   904,   908,
     914,     2,  1093,   482,  1096,     2,   101,   482,     3,   704,
       0,  1106,     0,   483,   541,   566,   573,     3,     3,   686,
     690,   700,   706,   707,     2,   889,   907,  1083,     2,     2,
      24,     0,     2,   730,    25,     0,   728,   731,  1104,     0,
       0,   737,   726,   725,     0,     0,   826,     2,     2,   449,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   865,   922,   945,   843,     0,   461,
       2,   861,   869,  1003,   724,   863,   864,     0,   824,   482,
     918,   926,   724,   920,   921,     0,   937,   939,     0,   451,
     482,   482,   547,   483,     0,   563,   482,  1088,  1092,  1090,
     564,   798,     0,   824,   841,     0,   431,   439,   496,     0,
     824,   482,   798,     0,   824,   773,   544,   545,   561,   576,
     582,   585,   580,   586,   604,   605,     0,   774,   693,   714,
     483,     0,   694,   697,   696,     0,   203,   414,   823,     0,
     412,   465,   464,   563,     0,   433,     2,   434,   795,   453,
       0,     0,     0,   482,   482,   841,   482,   798,     0,     2,
       0,   757,   756,   755,   750,   493,     0,   748,   765,   539,
       0,     0,   482,   482,  1062,   483,   479,   480,   481,  1066,
    1057,  1058,  1064,   482,     2,   102,     0,  1022,  1036,  1106,
    1018,   843,   843,  1027,  1034,   712,   482,   571,   708,   483,
     567,   568,   572,     0,   843,  1072,   483,  1077,  1069,   482,
    1074,     0,   675,   667,   674,  1104,     0,   666,   666,     0,
       0,   482,     0,   858,   857,   853,   855,   856,   854,     0,
     848,   851,     0,    23,   482,    95,     0,   482,   482,    90,
     482,    97,     0,    33,    37,    38,    34,    35,    36,   482,
      93,    94,   482,   482,   482,     2,   104,   105,     0,     0,
     183,     0,     0,   607,     0,  1082,     0,     0,     0,     0,
       0,     0,     0,     0,    56,     0,    62,    63,    67,     0,
       0,    67,     0,    91,    92,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   482,   166,
     167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
     164,     0,   162,   163,   482,   987,   721,   984,   843,   843,
     992,   588,   482,   887,   843,   897,   905,   909,   915,   482,
     890,   892,   894,   482,   910,   912,     0,  1094,  1097,   482,
       0,     0,   482,   102,  1022,   843,  1106,   957,   843,   843,
    1106,   843,   972,   843,   843,     3,   708,     0,     0,  1106,
    1106,   482,   482,     0,     2,   739,     0,  1104,   736,  1105,
       0,   732,     0,     2,   735,   738,   180,   179,     0,     2,
     482,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   843,   874,   878,   917,   843,   931,
     935,   943,   843,   953,   866,   923,   946,     0,     0,     0,
     999,     0,   459,   827,     0,     0,   460,   828,   452,     0,
       0,     0,     0,   450,     2,   829,     0,   435,     2,   798,
       0,   824,     2,   830,     0,     0,     0,     0,   619,   883,
     482,   901,     0,     0,   482,   415,   413,   101,     3,   482,
       0,     3,   799,     2,   752,   482,   482,   746,   745,   746,
     540,   538,   668,   666,     0,   843,   843,  1068,   482,  1073,
     483,   482,  1059,     0,     0,     0,     0,  1037,     0,   843,
    1107,  1023,  1024,   713,  1020,  1021,  1035,  1063,  1067,  1065,
     569,   604,     0,  1071,  1076,   671,   666,     0,   676,  1104,
       0,   666,   778,   776,     0,     0,   850,    67,   810,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   482,     0,   122,   121,     0,   118,   117,    28,     0,
      29,     0,     0,     0,     0,     3,    67,     0,    52,     0,
      53,    60,     0,    59,    71,     0,    68,    69,    72,    55,
       0,    54,    58,     0,     0,    51,   124,   126,   127,   128,
     130,   131,   133,   134,   138,   139,   136,   137,   141,   142,
     144,   146,   148,   150,   152,     0,     0,   424,     0,     0,
      30,     3,   730,   158,     0,     0,     0,     0,   988,   989,
     985,   986,   771,   770,   482,   891,   893,   895,   482,   911,
     913,     0,     0,  1013,  1012,   482,     0,     0,     0,     0,
       0,   843,  1023,   960,   977,   482,   955,   963,   710,   958,
     959,   711,   482,   970,   980,   973,   974,     0,     3,  1106,
     443,     2,  1099,     2,   701,   702,   680,     3,     3,     3,
       3,   724,     0,   156,     0,     3,     3,     0,   733,     0,
     727,     0,   843,     0,   482,     3,   447,   454,   843,   875,
     879,   843,   932,   936,   944,   843,   954,   482,   867,   870,
     872,   482,   924,   927,   929,   482,   947,   949,   951,   841,
       0,   462,  1000,     3,  1004,  1005,     3,   832,   940,   553,
     552,   555,   554,     2,   799,   833,   780,   482,     2,   831,
       0,   799,   834,   619,   619,   619,   482,   695,   699,   698,
     715,     0,   418,     0,     0,   482,     0,   338,     0,     0,
       0,     0,     0,   185,     0,   333,   334,     0,     0,   387,
     386,     0,   160,   160,   393,   580,   586,   200,   482,     0,
     186,     0,   211,   187,   188,   482,   205,   189,   190,   191,
     192,     0,   193,   194,   339,     0,   353,   195,   359,   361,
     367,   196,   197,   198,     0,   199,   207,   563,   482,     0,
     209,     0,     0,     0,     3,     0,   812,   799,   787,   788,
       0,     3,   783,     3,     3,     0,   482,   762,   762,  1104,
     666,   666,   666,  1070,  1075,     2,   101,   482,     3,   578,
       3,   483,  1031,   843,  1030,  1033,   482,     3,  1019,  1025,
     666,     0,   843,   843,     0,     0,   651,     0,   670,     0,
     666,  1104,     2,   847,   849,     0,    96,   482,   482,     0,
     100,    98,   482,     0,   112,     0,     0,     0,   116,   120,
     119,   184,     0,     0,     0,   730,   109,   177,     0,     0,
      46,    47,    85,     0,    85,    85,     0,    73,    75,    49,
       0,     0,    45,     0,    48,   155,     0,     0,     0,     0,
    1104,   996,   843,   995,   998,   990,     0,     0,   884,   902,
       0,   843,   966,   969,   843,     0,   843,   843,   961,   978,
       0,     0,  1100,     0,   703,   482,   482,     0,     0,     0,
       0,   432,     3,     0,     0,     0,     0,   729,   734,     3,
     825,   182,   181,     3,     0,   482,   868,   871,   873,   482,
     925,   928,   930,   482,   948,   950,   952,     0,     0,     0,
     724,   843,  1011,  1010,     0,     0,     0,     0,     0,     3,
     799,   835,     0,   482,   482,   482,   482,   482,   482,   482,
     602,   632,     3,     0,   633,   563,   620,     0,     0,     0,
     416,    67,     0,     0,   324,   325,   208,   210,     0,     0,
       0,   482,   482,   320,     0,   318,     0,     0,     0,   730,
       0,     0,     0,     0,     0,   161,     0,     0,   394,     0,
       0,     0,     0,     3,   215,     0,   206,     0,   315,     0,
       0,     0,   338,   338,   344,   343,   338,   355,   354,   338,
     338,     0,   563,   843,     0,  1015,  1014,     0,     2,     0,
     790,     2,   785,     0,   786,     0,   766,   747,   751,   749,
       0,  1104,     0,     0,   482,     0,     0,     0,     3,   482,
    1026,  1028,  1029,     0,     0,   101,     0,     3,  1104,   666,
     660,   666,   676,   676,   730,   677,   652,  1104,     0,   779,
     482,   852,  1016,     0,     0,     0,    39,     0,   113,   115,
     114,   111,   110,   730,  1104,     0,    66,    82,     0,    76,
      83,    84,    61,     0,     0,     0,    70,    57,     0,   154,
     423,    31,     0,   482,   991,   993,   994,   885,   903,   843,
     482,   962,   964,   965,   482,   979,   981,     0,   956,   971,
     967,   982,  1101,     3,   688,   687,   691,  1103,     2,     2,
    1102,     0,     3,   840,   740,   741,     0,     0,   843,     0,
       0,     0,   876,   933,   941,   463,   842,     0,  1006,     0,
    1007,  1008,  1002,   816,     2,     0,   818,   602,   602,   602,
     633,   640,   607,     0,   646,   633,     0,   482,   594,   631,
     627,     0,     0,     0,     0,   634,   636,   843,   648,   648,
     648,     0,   628,   644,   419,     0,   328,   329,   326,   327,
       0,     0,   338,   225,     0,     0,   227,   427,   226,   563,
     482,   306,   305,   307,     0,   338,   185,   265,     0,   258,
       0,   185,   321,   319,     0,   313,  1104,   322,     0,     0,
       0,   375,   376,   377,   378,     0,   368,     0,   369,   330,
       0,   331,     0,     0,   358,   482,   216,   204,   317,   316,
       0,     0,   347,   357,     0,   338,   360,     0,   362,   385,
       0,   417,   843,   482,   814,   767,   482,     2,     2,   658,
       0,   666,   666,  1078,  1079,  1080,     0,     0,     3,     3,
       0,  1039,     0,     0,  1104,   666,     0,   673,   672,  1104,
       0,   655,     3,     0,  1017,    99,     0,    32,   482,     0,
    1104,     0,     0,    86,     0,    74,     0,    80,     0,    78,
      44,   159,     0,     0,     0,     0,   482,   482,   743,     0,
     440,   442,   877,   934,   942,     0,     0,   782,   820,   598,
     600,   596,     0,     0,  1046,     0,   641,  1051,   643,  1043,
     843,   843,   626,   647,   630,     0,   629,     0,     0,     0,
     650,     0,   622,   843,   621,   637,   649,   638,   639,   645,
     338,     0,     0,   246,   338,   228,   563,   311,   309,   312,
     308,     0,   310,     0,   254,     0,   185,     0,   338,   482,
     266,     0,   291,     0,     0,   314,     0,     0,   338,   337,
     379,     0,   370,     2,     0,     0,     0,     0,   340,     0,
       0,   338,     0,   338,   338,   338,   338,   202,   201,   441,
     784,     0,     0,   659,  1104,  1104,  1081,  1032,     0,     0,
    1038,  1040,   656,     0,     0,   666,     0,   654,     2,    50,
      42,    40,    41,     0,    64,   178,    77,     0,     0,   997,
     968,   983,   444,     2,   685,     3,   684,   742,  1001,  1009,
     624,     0,     0,     0,  1047,  1048,   843,   625,  1044,  1045,
     623,   603,     0,     0,   336,     0,     0,     0,   239,   338,
     217,     0,     0,   338,   248,   263,   274,   268,   338,   185,
     303,     0,   278,     0,     0,   269,   267,   256,   259,     0,
       0,   185,   292,     0,     0,   220,   335,     2,   482,   332,
       0,     0,   395,   338,   345,     0,    67,   356,   349,     0,
     350,   348,   363,   364,   789,   791,     0,     0,  1041,  1042,
     657,   666,  1104,   678,   781,    65,    81,    79,   482,     0,
     843,  1054,  1056,  1049,     0,   635,   234,   229,   232,     0,
     231,   238,   237,     0,   482,   241,   240,   338,   250,     0,
     247,   338,     0,     0,     0,   255,   260,     0,     0,   185,
     304,   279,     0,     0,   338,     0,   294,   295,   293,   262,
     323,     0,   482,   482,     3,   380,   483,   384,     0,   388,
       0,     0,     0,   396,   397,   223,   341,     0,     0,     0,
       0,   662,   664,  1104,     0,   689,   482,  1050,  1052,  1053,
     642,     0,   236,     0,   235,   219,   242,   482,   408,   251,
     338,   252,   249,   264,   277,   275,   271,   283,   281,   282,
     280,   261,   276,   272,   273,   270,   257,     0,     0,     0,
       0,   222,   242,     3,   373,     0,  1046,   381,   382,   383,
     395,     0,     0,     0,   395,     0,   338,   346,   342,   338,
       0,   338,     0,     0,   663,     0,   230,   233,   338,     3,
     243,   409,   253,     0,     0,     0,     0,   302,   300,   297,
     301,   298,   299,   296,     3,   373,     0,     0,  1047,     0,
       0,     0,   389,     0,   398,   224,   351,   338,   365,   338,
     661,  1055,   212,     0,     0,   338,   290,   288,   285,   289,
     286,   287,   284,     0,     0,   374,     0,   401,     0,   399,
       0,   401,   352,   366,   214,   213,   218,     0,   221,     0,
     371,   402,     0,     0,   390,     0,   372,     0,     0,     0,
       0,   403,   404,     0,   400,   391,     0,     0,   392,   405
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1912,  6635,  5357, -1912,    -1,   121,  1838,   -38, -1912,  -333,
   -1912,   416, -1912,  -735, -1912,   837,  -998, -1121, -1912,   262,
    5926,  1840, -1912,  1664, -1912,  1451,   430,   840,   848,   705,
     849,  1413,  1414,  1412,  1417,  1420, -1912,   311,  -146,  8105,
     972, -1912,  1744, -1912, -1912,  -699,  4888, -1099,  6596, -1912,
     256, -1912,   969,    40, -1912, -1912, -1912,   487,   134, -1912,
   -1643, -1911,   354,   117, -1912, -1912, -1912,   364, -1592, -1912,
   -1239, -1912, -1912, -1912, -1912,  -435, -1170, -1912,   488, -1198,
     490, -1912, -1912, -1912, -1912, -1912,   149, -1179, -1912, -1912,
   -1912,    76,   514,   516,   180, -1912, -1912, -1912, -1912, -1445,
   -1912,   110,    45, -1912,   185, -1912,  -159, -1912, -1912, -1912,
     968,  -706,  -837, -1337, -1912,    30, -1255,   537,  4403,  -807,
    -713, -1912,  -107, -1912, -1912,     8, -1912,  -118,  3578,    56,
    -245,  2518,   314,  -679,    11,   438,   407,  2136,  2717, -1912,
    2161, -1912,   382,  4246, -1912,  2101, -1912,    94, -1912, -1912,
    1954,   579,  4865,  3706,   -56,  1950,   -19, -1912, -1912, -1912,
   -1912, -1912,  -202,  5740,  5203, -1912,  -386,   292, -1912,  -702,
     302, -1912,   232,   800, -1912,   -28,  -165, -1912, -1912, -1912,
    -346,  6260,  -540,  1255,   113,  -750,  -657,   493,   273, -1912,
   -1322,  -158,   126,  1951,   991,  6949,  -182,  -527,  -252,  -200,
    -406,  1385, -1912,  1730,  -243,  1298,  1608, -1912, -1912, -1912,
   -1912,   380,  -167,   -81,  -912, -1912,   293, -1912, -1912, -1130,
     515, -1912, -1912, -1912,  2233,  -800,  -488,  -805,   -34, -1912,
   -1912, -1912, -1912, -1912, -1912,   636,  -860,  -210, -1799,  -164,
    7361,   -70,  2757, -1912,  1262, -1912,   545,   -50,  -226,  -197,
    -188,     1,   -74,   -68,   -62,   460,   -12,    -7,    14,  -184,
     -17,  -155,  -124,  -123,   249,  -119,  -115,  -102,  -752,  -751,
    -726,  -715,  -749,   -91,  -704, -1912, -1912,  -697,  1453,  1455,
    1460,  1987, -1912,   626,  6899, -1912,  -631,  -586,  -526,  -517,
    -789, -1912, -1734, -1748, -1744, -1731,  -636,  -100,  -319, -1912,
   -1912,   -31,   218,   -64, -1912,  7645,  1012,  1223,  -172
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1209,   224,   410,   411,    82,    83,   412,   386,   413,
    1535,  1536,   414,  1015,  1016,  1017,  1326,  1327,  1328,  1547,
     436,   416,   417,   418,   718,   719,   419,   420,   421,   422,
     423,   424,   425,   426,   427,   428,   429,   438,  1114,   720,
    1456,   781,   218,   783,   432,   848,  1210,  1211,  1212,  1213,
    1214,  1215,  1216,  2143,  1217,  1218,  1463,  1654,  1988,  1989,
    1918,  1919,  1920,  2109,  2110,  1219,  1668,  1669,  1670,  1822,
    1823,  1220,  1221,  1222,  1223,  1224,  1225,  1849,  1853,  1480,
    1472,  1226,  1227,  1479,  1473,  1228,  1229,  1230,  1231,  1232,
    1686,  2127,  1687,  1688,  2024,  1233,  1234,  1235,  1459,  2032,
    2033,  2034,  2172,  2183,  2056,  2057,   303,   304,   914,   915,
    1181,    85,    86,    87,    88,    89,    90,   469,    92,    93,
      94,    95,    96,   232,   233,   306,   285,   471,    98,   472,
      99,   590,   101,   102,   155,   351,   309,   106,   107,   170,
     108,   931,   352,   156,   111,   256,   112,   157,   264,   354,
     355,   356,   158,   433,   117,   118,   358,   119,   586,   907,
     905,   906,  1628,   359,   360,   122,   123,  1176,  1426,  1634,
    1635,  1785,  1786,  1427,  1623,  1805,  1636,   124,   678,  1735,
     674,   361,   675,   676,  1288,  1107,   477,   478,   943,   592,
     479,   480,   593,   594,   595,  1239,   442,   443,   219,   497,
     498,   499,   500,   501,   339,  1257,   340,   929,   927,   624,
     341,   380,   342,   343,   444,   126,   176,   177,   127,  1251,
    1252,  1253,  1254,     2,  1163,  1164,   616,  1246,   128,   330,
     331,   266,   277,   569,   129,   222,   130,   321,  1116,   897,
     531,   168,   131,   689,   690,   691,   132,   323,   236,   237,
     238,   324,   134,   135,   136,   137,   138,   139,   140,   241,
     325,   243,   244,   245,   326,   247,   248,   249,   816,   817,
     818,   819,   820,   250,   822,   823,   824,   786,   787,   788,
     789,   532,  1156,  1405,   141,  1743,   649,   650,   651,   652,
     653,   654,  1788,  1789,  1790,  1791,   639,   482,   366,   367,
     368,   445,   210,   143,   144,   145,   370,   840,   655
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      81,   194,   133,    81,   513,   192,  1024,   195,   362,    97,
     381,   552,   103,   196,  1255,   509,   952,   723,   503,   566,
     958,   201,  1331,   890,   892,   180,  1087,  1004,  1476,  1461,
     305,   679,  1094,   514,   151,  1901,   431,  1409,  1460,  1902,
     997,   549,   515,  1584,  1585,   667,   516,   235,   349,   670,
    1905,  1338,  1903,   209,  1240,    81,    81,   944,    81,   671,
     133,  1077,  1083,   197,  -792,  1084,   365,    97,   198,   529,
     103,   534,   729,   894,    81,   517,   377,   313,   542,  1834,
     242,   538,  1110,    81,   902,   207,  1078,   837,  1059,   199,
     146,    81,  1997,   513,  1450,   113,    81,  1079,   239,    81,
    1125,   267,  1171,    81,  1656,   278,   518,   519,  1080,   563,
    2060,   520,   528,   316,  1372,   521,  1990,   945,   448,   209,
     574,  1692,   514,  1247,   449,   149,   946,  -793,   522,   923,
     450,   515,   220,   305,   271,   516,   680,   969,   464,   682,
    1991,    81,   763,   385,    81,  2108,    81,    58,   133,    58,
     605,   607,    81,   113,   973,    97,   194,  1690,   103,    81,
     511,   305,   195,    58,   517,   606,    81,   220,   196,  1244,
    1983,  2108,   305,  1198,  1108,  1108,   159,   174,   174,   160,
     451,   524,    20,   952,   764,   452,  1657,  1657,   165,  1693,
      81,    81,   207,  1108,  1998,   518,   519,   599,   275,  2145,
     520,   221,  1549,  -824,   521,    81,   453,   485,  1236,  1915,
    1916,   672,  2061,   174,   525,   494,   673,   522,   197,   142,
      81,   724,   142,   198,    81,    81,   944,   166,   558,    81,
      81,   113,   207,   530,  1934,  1705,  1691,   581,  -824,   753,
     754,   113,  1295,   297,   199,   194,  1373,   530,   640,   610,
      81,   195,  1474,   297,  1990,   454,   235,   196,    81,   190,
     207,   628,  1108,   174,   213,  1063,   174,  -599,    81,    81,
     524,  1319,    81,   384,   570,  1475,  1996,   142,  2051,    81,
    1374,   174,   882,   755,   756,   854,   945,  1087,   375,   242,
     886,  2088,  1917,    81,    81,   946,    81,   558,  1992,   841,
     628,    81,  2052,   525,  1402,    81,  1704,   601,  -975,  1460,
    1707,  2030,  1374,   939,   855,  -975,  1310,   179,    81,    81,
    1279,   207,   142,   856,  1077,  1437,  1403,   857,    81,   481,
    1358,   657,   970,  1359,   113,    81,    81,  1901,   888,  1421,
      81,  1902,   174,  1554,   893,   181,   246,   510,   606,  1078,
     964,   182,  1905,  1022,  1903,  1240,   858,   190,  1656,  1408,
    1079,  1345,   113,   980,   293,   142,  1412,   640,  1410,  1422,
    1851,  1350,   646,   113,  1383,  1555,   187,   912,    81,   298,
     821,    81,   688,   109,   854,  1287,  1470,   859,   860,   299,
     174,   174,   861,  1259,  1983,   999,   862,   202,   113,   808,
    1742,   174,   275,  1852,   634,  2004,   209,  1477,   105,   863,
    1802,  1168,   901,   855,   261,   584,   174,  1803,   589,  1429,
     665,    58,   856,   149,   668,   283,   857,   290,  1827,   292,
    1478,  1291,  1755,  1757,  1759,   201,  1804,   448,  1430,   104,
    1657,   109,  1489,   449,  1584,  1585,    81,    -3,   485,   450,
     174,  1279,  1432,  1433,   657,   858,   227,   174,   174,  2053,
    2054,  2099,   174,  1423,   305,  1996,   105,  1108,   261,    81,
      81,   290,   292,   874,   225,   225,  1198,   953,   506,  1436,
     526,    81,    81,  2101,  1511,   226,   859,   860,   251,   944,
      81,   861,   494,   430,  1630,   862,   349,   104,  1198,   451,
     174,   539,  1996,   174,   452,   530,   875,   262,   863,  1236,
      81,  1284,   979,   190,   365,   982,   983,    58,   984,   109,
     261,  -976,    81,   634,   485,   453,  1461,   986,  -976,   109,
     988,   989,   990,   999,    58,  1460,   448,   454,    91,   530,
     936,   152,   449,  2013,   208,  1915,  1916,    81,   450,   945,
    1754,   298,   215,    81,   105,   602,   640,   240,   946,   598,
     268,   270,   874,   216,   279,  1576,  1855,  1825,  1434,   526,
     591,   657,  1833,   575,  1657,   308,  1421,  1421,  1421,   217,
     114,  1641,   922,  1471,   999,   104,  -476,   261,   587,   290,
     292,   174,  1939,  1940,  2078,   875,    91,   613,   293,  1852,
    1642,   530,   999,   174,   174,  1615,  1422,  1422,  1422,    81,
      58,    81,  2080,   485,   799,    81,   657,   133,   530,  1264,
     193,   261,   109,    58,    97,    81,   261,   103,  1944,    81,
      81,   958,   261,   999,  1558,  1429,  1793,   260,   114,  1118,
     657,   272,   234,  2114,  1088,  2129,  1340,   657,  1091,  2133,
     109,   208, -1105,  1641,  1711,  1794,  2045,  1104,  1105,   722,
    1474,   109,    81,   672,   261,   113,   486,  1149,   673,   662,
     161,   292,  1796,   162,   163,    81,   164,   999,   308,  1098,
     568,   625,   481,  1475,    91,   626,   109,  2116,   937,    58,
    1064,   208,  2007,  2008,   530,    58,  1645,   295,   322,   999,
    1423,  1423,  1423,  1085,  1694,  1803,   308,   644,   706,   765,
     113,   581,   957,   766,   262,  1797,   114,   308,  1835,   208,
    1501,   200,    64,   821,  1900,   963,   114,  1933,  1048,  1497,
      81,   298,    81,   571,    81,  1655,  1671,    58,    81,  1657,
    1518,    81,   308,    58,    58,   297,   298,   174,  1646,  1906,
    1527,   932,   935,  1679,   298,    58,   317,   262,  1887,   481,
    1888,   261,  2039,  1648,  1431,  2040,    81,  1657,  1907,  1092,
     322,  -813,   876,   644,   379,   512,   234,    14,    15,    16,
      17,    18,    63,    64,   961,  -653,    58,   261,    58,   662,
     292,   791,  -653,   883,    58,   792,   322,   174,  1817,  1818,
    1819,   887,  1657,   730,   706,    58,   337,  1803,   731,   382,
     567,    81,   885,    81,   621,    58,    58,  1137,   895,   114,
    1820,   530,   723,  1141,  1145,    81,  1910,   530,   530,   903,
    2006,    77,    81,   383,   142,  1385,    58,   261,   494,   530,
    1150,    81,  2019,   622,   623,   803,  1752,   114,  1334,   530,
      81,   322,  1008,   911,  1010,  1330,  1013,   912,   114,  1371,
    1021,   876,   261,  1025,   611,   322,  1389,   261,  1393,   261,
     530,  1283,   530,   349,  1509,   537,    81,   190,   644,    58,
     262,   384,   545,   114,  -718,  1563,   591,   999,  1050,   530,
     261,   365,   261,   261,   481,  1570,  1574,   732,  -480,   530,
     644,  1744,   733,   562,   261,   455,  2002,   486,   456,  1544,
    2071,    81,    81,   494,   573,  1502,  1503,   261,  1782,  1378,
     972,   657,    97,  1795,   626,   103,   261,  1364,    14,    15,
      16,    17,    18,   580,    64,   481,    14,    15,    16,    17,
      18,    14,    15,    16,    17,    18,   457,   174,   458,   261,
     722,   662,   292,   109,   174,  1248,   722,   481,   481,  2046,
     459,    81,   460,   530,  1314,   722,    14,    15,    16,    17,
      18,  1315,  1126,   261,   662,   688,   481,  1828,   484,  1734,
     261,  1546,  1829,   486,   722,  1400,   488,    58,  1330,  1655,
    1357,   821,  -477,   489,  1263,    58,   502,   262,   109,   508,
      58,   992,    14,    15,    16,    17,    18,   504,   113,   308,
     568,    81,   993,   994,   252,   253,   815,   254,   507,    81,
     842,   843,   255,   105,   844,    58,   724,   527,  1817,  1818,
    1819,  1513,  1622,  1180,   174,   174,  1178,  1951,    74,   746,
    1490,   528,  1952,  1676,   550,   481,   747,   748,    81,  -481,
    1820,   494,   629,   293,   104,    74,   853,  1054,   643,  1821,
    1068,    58,   644,   297,   530,   454,   568,   530,   234,    79,
     645,   941,  1071,  2093,    81,   643,  1072,   551,  2094,   644,
      81,    81,   646,  1282,  2160,    74,    79,   645,   974,  2161,
     381,   381,   626,   322,   975,   561,   212,   220,   976,   322,
    1817,  1818,  1819,   572,   539,   784,   867,   921,   530,   530,
     596,    81,   999,   600,  -478,   998,    79,    80,  1739,   999,
    1537,   632,  1820,  1671,    14,    15,    16,    17,    18,  1122,
     617,  1826,   142,  1123,    74,   664,   430,  1750,   677,    74,
    1157,  1525,   613,   142,   454,  1428,   530,  1113,  1605,   673,
     114,   692,  1165,    91,   784,   920,  1169,   322,   530,   643,
    1172,   681,   212,   644,   726,    79,    80,   640,   297,   539,
      79,   645,   530,   530,   884,  1736,  1027,  1028,  1029,  1532,
     349,  1864,  1865,    58,  1159,    74,   494,   693,   999,    81,
      81,    81,   696,    97,   697,   114,   103,  1248,   365,   896,
    1161,   698,   446,   613,   999,  1783,   900,   530,  1586,   530,
     904,  1085,   702,   454,   494,   644,    79,    80,  1329,   745,
      81,  2037,  1330,    97,   760,  1496,   103,  1592,  1593,   792,
    1531,  1747,   759,    81,  1330,  1748,    81,    81,   267,   278,
      81,   734,  1810,   735,   736,   737,  1330,   941,   761,    81,
     481,   762,   657,   793,   261,  1817,  1818,  1819,    14,    15,
      16,    17,    18,   749,   750,   261,  1811,   999,   271,  1837,
     999,   559,   738,   999,   261,   739,   740,  1820,  1838,   113,
     741,   742,  1123,   767,    81,  1839,  -186,   161,  2058,   999,
     162,   163,   794,   164,   795,   148,   109,   796,    81,    65,
      66,    67,    68,    69,    70,    71,    72,   797,   174,   113,
     798,   174,   174,   174,   494,  1911,  2058,    58,   461,   792,
      -3,   105,    81,   957,  1109,  1109,  -122,  -122,  -122,  -122,
    -122,  -122,   275,   825,  1658,  1972,   174,  1346,   751,   752,
     559,  1347,   174,  1109,  1957,  -479,  1999,   262,   999,  2097,
     999,  2111,  1238,  1330,    81,   589,  1335,  1076,  1360,   815,
     642,   -18,   174,   757,   758,  1361,  -121,  -121,  -121,  -121,
    -121,  -121,   261,   838,  1638,  2098,  2180,    74,   839,   999,
    2177,   852,  1428,  1428,  1428,   142,   349,  1624,  1428,  1874,
      14,    15,    16,    17,    18,   996,   261,  1783,  2186,   202,
     726,   530,  2187,   142,   365,   513,   174,  1639,    79,    80,
    1397,   849,  1109,   878,  1398,  1779,  1780,  1781,  1399,   864,
    1533,   880,   322,    81,   632,   726,  1411,    81,    81,  1001,
    1002,  2043,  1113,   142,   514,  1681,  1682,  1683,  1684,  1685,
    1435,  1100,  1101,   515,  1102,  1103,   151,   516,   494,   212,
     865,    91,   103,   103,  1317,  1123,   142,  1454,  1034,  1035,
    1036,  1037,  1249,   866,   722,  1413,  1414,  1415,   868,  1467,
     869,   494,   494,  1806,  1806,  1806,   517,  1332,  1333,   642,
     870,    81,   871,   570,   921,    14,    15,    16,    17,    18,
    1309,   481,   481,   114,   872,   148,   873,   172,   173,    65,
      66,    67,    68,    69,    70,    71,    72,   518,   519,   999,
    1336,   879,   520,  -157,  -157,   310,   521,  1470,  1471,  1102,
    1488,  1552,  1553,   494,  1557,  1553,  1561,  1553,   898,   522,
    1074,  1545,  1594,  1545,   899,   113,   113,  1170,  1074,  1607,
    1760,  1123,   494,  1885,  1123,  1886,  1553,    81,   909,   174,
     174,   261,    81,    81,    81,  1586,  1892,  1893,  1898,   999,
    1248,  1798,   446,   446,  1955,  1956,  1537,   109,  1468,  -597,
    1638,  -595,   854,  1976,  1553,  1638,  1977,  1553,  1915,  1916,
    2177,  2178,   913,   524,   908,   261,  1550,  1551,  1658,  1030,
    1031,   261,   105,   174,   174,   349,   910,   109,   430,  1032,
    1033,   855,   916,  1639,  2100,  2102,  1038,  1039,  1639,  1586,
     856,  1807,  1808,   365,   857,   924,   525,  1109,   926,    81,
    1076,   930,   105,  1238,    81,   947,  1356,   815,  1706,  1708,
      81,   949,    81,   646,   142,  1737,  1738,  1498,  1499,   966,
      81,   977,   971,   858,   262,  1000,   978,  1559,  1003,  1073,
    1006,   494,  1965,  1238,  1047,  1846,  1052,  1074,  1599,   142,
     142,  1081,  1600,  1128,   494,  1120,  1601,  1129,   959,   332,
     333,   334,   335,  1640,   859,   860,  1130,  1131,   568,   861,
    1132,  1133,  1134,   862,   430,   430,  1135,  1136,  1647,  1649,
     271,    58,  1151,   446,  1158,  1160,   863,   268,   279,  1162,
    -796,  1166,  1173,  1241,  1249,  1256,   103,  1174,   148,   494,
     172,   173,    65,    66,    67,    68,    69,    70,    71,    72,
    1175,  1260,    91,   148,  1272,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,  1273,  1274,  1709,  1275,   261,
    1286,  1287,  1293,  1290,  1296,  1292,  1248,  1300,   142,  1297,
    1299,   874,    91,  1301,   275,    81,  1302,    81,  1320,  1303,
     336,  1305,   174,  1306,   114,  1307,  1312,  1321,  1313,  1987,
    1337,  1355,    76,   174,   260,   272,  1341,  1342,   337,  1343,
    1344,  1348,  1727,  1349,   875,  1351,   174,  1362,  1352,   113,
    1353,   921,  1354,  -682,   114,   322,   261,    81,  -681,  1377,
      81,  1401,  -797,  1406,   446,  1438,  1441,  1950,  1442,   494,
    1451,  1452,  1453,   494,  1458,  2087,  -717,  1462,  1464,   999,
    1586,  1483,  1485,   109,   109,  1486,  1492,   494,  1526,  1494,
    1529,   174,  1519,  1543,  1545,  1567,  1762,   494,  1568,  1569,
     103,    84,  1580,  1763,   150,  1581,  1560,  1764,   105,   105,
     494,  1582,   494,   494,   494,   494,  1587,  1588,  1583,   513,
      81,    81,  1589,  1638,  1590,  1553,  1595,  1598,  1602,  1640,
    1603,  1604,  1612,  1613,  1640,  1616,  1651,  1627,   142,  1660,
    1660,  1431,   526,  1629,  1471,  1672,  1695,  1697,   514,   568,
    1700,  1701,   481,   481,  1198,  1673,  1639,   515,   967,    84,
    1577,   516,  1712,  1675,  1677,  2106,  1714,  1987,  1689,  1698,
    1699,  1715,    81,   142,  2031,   191,  1717,  2027,   494,  1718,
    1719,  1723,   494,   113,    84,  1721,  1722,   494,  1724,  1725,
     517,   174,  1731,  1741,   142,   174,  1746,   231,   446,  1745,
     259,  1840,   571,  1749,    84,  1753,  2131,   454,  1761,   174,
    1777,  1768,   494,   152,  1632,  1594,  1770,  1772,  1778,   174,
    1773,   518,   519,  1774,  1792,  1812,   520,  1843,  1814,  1637,
     521,  1845,   174,   261,   174,   174,   174,   174,    91,    91,
     221,   150,   174,   522,  1863,  1866,  1867,    84,  1870,  1875,
     150,  1871,  2027,   320,   328,  1872,   494,   921,  1877,  1879,
     494,  1884,  1889,  1890,  2025,   103,   348,   657,  1891,  1897,
    1923,   194,  1928,   494,  1929,   610,  1941,   195,  1943,   567,
     114,   114,  1947,   196,    81,  1953,    81,  2179,  1949,  1954,
     437,   191,   191,   103,  1959,  1968,   524,  1969,  1970,  1971,
     174,  1973,   150,   467,   174,  1974,   259,   142,  1975,   174,
     876,   530,  2031,  -683,   495,   699,  2031,  2031,  1980,   494,
    1117,  1981,  1982,  2001,  2003,   320,   262,  1249,   103,   525,
     231,   231,  -580,  2012,   174,  2009,  2014,   109,   874,  2025,
     743,   744,  2028,  2020,    81,    81,  2029,   207,   113,  2041,
    2042,   320,  2158,  2055,  1893,   494,  2064,  2077,   494,    84,
     494,   743,   105,  2081,   481,  2090,  2091,   494,  2079,  2092,
    2096,   875,  2104,   259,  2095,   261,   113,   363,   174,  2171,
    2117,  2113,   174,  2171,  2126,  2130,  2115,    81,  2137,   485,
     261,   743,  2132,  1660,  2139,   174,   494,  2140,   494,  2181,
    2157,  2166,   494,  2141,   494,  2146,   320,  2156,  2089,  2159,
    2168,   113,   328,  2174,   363,  2173,  2185,   475,   328,   320,
     320,  2169,  1640,  2184,  1881,  1637,   494,  1556,   150,   995,
    1637,  2188,  1040,  1042,  1041,  1457,  1799,    81,  1637,  1043,
     782,   174,  1847,  1044,  1466,  2167,    81,  2107,   348,   647,
     656,  1945,  1289,  1938,  1854,  1856,   260,   272,   188,  2124,
    1841,  2154,  1842,  2083,   348,  2134,  2175,  2082,   348,  1484,
     171,   109,   142,   288,   560,  1985,  2050,   174,   261,  1626,
     174,  1285,   174,   568,  1119,  1481,   430,  1258,   541,   174,
     845,  1860,    91,     3,   928,  1776,   105,  1055,  1294,  1056,
     142,   281,     0,   437,  1057,   282,     0,     0,   286,     0,
     291,     0,     0,  1249,     0,     0,     0,     0,   174,  1895,
     174,     0,     0,     0,   174,     0,   174,  1660,   446,     0,
       0,     0,     0,     0,   114,   142,     0,   437,     0,   959,
     785,     0,     0,     0,     0,     0,     0,   191,   174,   183,
       6,     7,     8,     9,    10,    11,    12,    13,     0,  2182,
       0,     0,   637,   150,     0,   660,     0,   467,  2189,     0,
       0,   814,     0,   656,     0,    19,     0,     0,   637,  2105,
       0,     0,   637,     0,     0,     0,     0,     0,     0,   835,
       0,   495,     0,     0,     0,   526,     0,     0,     0,     0,
     261,     0,    58,  1912,     0,     0,  1637,     0,     0,     0,
     280,   231,     0,   567,    48,    49,    50,    51,    52,    53,
      54,    55,   430,   231,   430,     0,    91,     0,     0,     0,
       0,     0,     0,     0,   148,     0,   109,   876,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   320,     0,
     437,   437,     0,     0,   320,     0,   348,     0,   281,     0,
       0,   105,    74,   430,   109,     0,     0,   322,   114,  1026,
       0,     0,  2170,     0,     0,     0,     0,     0,     0,     0,
       0,   637,    75,    76,     0,  2176,     0,   790,     0,   105,
       0,     0,  1660,    79,    80,     0,     0,  2155,     0,   109,
       0,     0,   261,   801,     0,   281,   804,     0,     0,     0,
     320,     0,   320,     0,   348,     0,    84,     0,  1637,     0,
    1660,     0,     0,     0,   105,     0,     0,     0,     0,     0,
       0,     0,   348,   467,     0,   656,     0,     0,     0,     0,
       0,     0,  1500,   647,   430,     0,     0,   647,   282,     0,
     661,     0,   291,     0,     0,  1660,   348,     0,     0,     0,
       0,     0,     0,   541,     0,     0,   656,   363,     0,   348,
     475,     0,     0,     0,  1528,     0,     0,     0,   683,   100,
       0,   150,   154,     0,     0,     0,     0,     0,     0,     0,
     707,    91,     0,     0,   437,     0,     0,   150,   150,     0,
     437,     0,     0,     0,     0,     0,     0,     0,     0,   437,
       0,     0,   150,   150,   150,     0,     0,     0,     0,    91,
       0,     0,     0,  1562,     0,   363,     0,     0,   475,     0,
     322,     0,     0,   114,     0,     0,     0,   100,     0,     0,
       0,     0,     0,     0,   363,     0,   637,   475,     0,     0,
       0,     0,     0,   684,    91,     0,     0,     0,     0,     0,
       0,   114,   206,     0,     0,     0,     0,     0,   467,   685,
     637,   686,   687,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   273,   637,   785,   785,   707,     0,     0,     0,
     611,   322,   437,     0,     0,     0,   114,   148,     0,   446,
       0,    65,    66,    67,    68,    69,    70,    71,    72,   467,
       0,  1304,   814,     0,   814,   307,  1308,     0,     0,   312,
       0,     0,     0,     0,     0,   100,     0,  1316,   318,     0,
       0,   348,   348,   322,     0,     0,     0,   495,     0,     0,
     835,     0,     0,     0,   350,  1355,    76,     0,     0,   148,
     348,   172,   173,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,   281,     0,     0,     0,   318,   447,
       0,     0,     0,     0,     0,     0,     0,   320,     0,   363,
     312,   473,   475,   148,  1720,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,  1733,     0,     0,     0,     0,     0,     0,     0,   523,
    1740,     0,     0,     0,   437,     0,     0,     0,   307,   348,
     363,     0,  1443,   475,     0,   150,   437,  1751,     0,   548,
       0,   790,   790,     0,   553,   555,     0,   206,   348,   189,
    1267,  1066,   363,   363,  1069,     0,   307,     0,     0,     0,
       0,   647,     0,     0,     0,     0,  1445,   307,     0,     0,
     576,   363,     0,     0,   578,   169,     0,     0,     0,   579,
       0,     0,     0,     0,     0,     0,     0,   263,     0,     0,
     555,     0,   307,     0,     0,     0,   603,     0,   284,   287,
       0,   467,   169,     0,     0,     0,     0,     0,   612,     0,
       0,     0,     0,     0,   541,     0,   318,     0,     0,   699,
    1325,  1139,     0,     0,     0,  1143,     0,     0,  1325,  1147,
       0,  1179,     0,     0,     0,     0,   635,     0,     0,   659,
     363,   263,     0,     0,     0,     0,     0,     0,   169,     0,
       0,     0,   666,     0,     0,     0,   666,  1325,     0,     0,
     495,   169,   637,   169,     0,   660,     0,     0,   785,  1836,
     148,     0,   371,   372,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,   814,     0,     0,     0,     0,
       0,   318,   814,   263,     0,   378,     0,     0,     0,     0,
     148,   743,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,   363,     0,     0,   475,     0,     0,     0,   378,
    1325,    77,     0,     0,     0,   318,   373,  1873,     0,     0,
       0,     0,  1876,   374,   348,     0,    19,     0,     0,  1538,
    1539,  1540,     0,  1883,     0,     0,  1541,  1542,     0,     0,
       0,   312,     0,     0,     0,   635,   933,   169,     0,     0,
     263,   169,     0,   934,   169,   169,     0,     0,   169,     0,
       0,   169,   169,     0,   169,     0,   169,   150,     0,    52,
      53,    54,    55,     0,     0,     0,   150,     0,     0,     0,
       0,     0,     0,     0,   263,   437,     0,     0,     0,   263,
       0,     0,   148,     0,     0,   263,    65,    66,    67,    68,
      69,    70,    71,    72,  1011,   790,     0,     0,   437,     0,
       0,     0,   148,     0,     0,   437,    65,    66,    67,    68,
      69,    70,    71,    72,  1019,     0,     0,   263,   318,   318,
       0,     0,     0,     0,   473,   363,   169,   259,    84,   169,
       0,     0,     0,     0,  1012,     0,     0,  1966,  1967,   307,
     320,     0,     0,     0,     0,     0,   150,     0,     0,     0,
       0,     0,   169,   169,  1020,     0,     0,   467,     0,     0,
       0,   708,     0,     0,     0,  1387,     0,   169,  1391,   148,
       0,     0,  1395,    65,    66,    67,    68,    69,    70,    71,
      72,  1322,   350,     0,   100,  1323,     0,  1324,   467,     0,
       0,     0,   150,     0,     0,     0,     0,     0,     0,     0,
     666,   940,     0,   495,     0,     0,     0,     0,     0,     0,
       0,  1325,     0,     0,     0,   951,     0,     0,    76,     0,
       0,  1548,     0,     0,   635,     0,     0,     0,     0,   960,
       0,     0,     0,     0,     0,     0,     0,   666,     0,     0,
     263,     0,     0,     0,     0,  2044,     0,     0,     0,   318,
       0,     0,     0,     0,     0,   348,   348,   708,     0,     0,
       0,     0,   318,   169,     0,   318,   318,     0,   318,     0,
       0,   637,     0,     0,     0,     0,     0,   318,     0,     0,
     318,   318,   318,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,   363,
       0,     0,   475,   150,   150,   150,   150,     0,   150,   150,
       0,     0,     0,     0,  1633,   328,  2103,     0,   378,   148,
       0,     0,   263,    65,    66,    67,    68,    69,    70,    71,
      72,   437,   437,     0,   169,     0,   473,   495,     0,  1280,
       0,     0,     0,   263,     0,     0,  1281,     0,     0,     0,
       0,     0,     0,  1058,     0,     0,     0,   263,     0,     0,
     318,     0,     0,     0,     0,     0,   363,   363,    76,   251,
     263,   834,   259,     0,     0,     0,     0,   940,     0,  1565,
       0,     0,  1082,     0,     0,     0,     0,     0,  1572,     0,
       0,     0,     0,     0,   467,     0,     0,     0,     0,   473,
     473,     0,   263,   183,     6,     7,     8,     9,    10,    11,
      12,    13,   495,     0,  1848,     0,     0,   378,   473,     0,
     150,     0,   647,     0,     0,     0,   263,     0,     0,     0,
       0,   495,     0,   263,     0,     0,  1325,     0,     0,     0,
       0,  1325,  1325,  1325,   148,     0,   172,   173,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   169,   169,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
     148,   169,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,  1237,     0,     0,     0,     0,   473,     0,     0,
       0,     0,     0,   154,   318,     0,     0,     0,    74,     0,
       0,     0,     0,  1447,     0,   363,   666,     0,   475,  1271,
    1633,  1784,     0,     0,     0,  1633,  1277,   437,  1631,    76,
       0,  1633,     0,  1633,    58,  1632,     0,     0,     0,    79,
      80,   148,     0,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,   328,
     150,     0,     0,     0,     0,     0,   148,     0,     0,   350,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    74,   437,     0,   619,     0,     0,
       0,     0,     0,     0,     0,   169,   169,     0,     0,     0,
       0,   169,     0,   348,    75,    76,   150,     0,     0,     0,
       0,     0,     0,     0,     0,    79,    80,     0,     0,     0,
     728,     0,   169,    77,   404,   169,   169,     0,   169,     0,
     169,   169,   153,     0,     0,     0,     0,     0,   150,     0,
       0,     0,     0,     0,  1325,     0,  1325,     0,  1154,     0,
       0,     0,     0,     0,     0,     0,   348,   348,  1787,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2036,
       0,   169,     0,  1784,  1784,   169,     0,     0,     0,   169,
       0,     0,     0,     0,     0,     0,     0,     0,  1633,     0,
      58,  1633,   473,   148,     0,   580,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   328,   263,     0,     0,
       0,     0,   205,     0,   363,     0,     0,     0,     0,   437,
       0,     0,   148,     0,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   154,     0,     0,     0,     0,
       0,     0,   169,   169,  1425,  1049,     0,     0,     0,     0,
      74,     0,   320,  1237,     0,     0,   169,   116,     0,     0,
     116,     0,     0,     0,     0,     0,     0,   363,   363,   311,
    2085,    76,     0,     0,   530,     0,   318,     0,   205,     0,
     148,    79,    80,  1237,    65,    66,    67,    68,    69,    70,
      71,    72,  1322,     0,  1784,     0,  1323,     0,  1324,     0,
       0,     0,     0,  1633,     0,     0,  1482,     0,     0,     0,
       0,     0,     0,     0,     0,   116,     0,     0,   440,     0,
    1787,  1787,     0,     0,   318,     0,     0,     0,     0,    76,
     462,     0,  1756,     0,     0,   635,     0,     0,   150,     0,
     116,     0,     0,   148,   553,     0,     0,    65,    66,    67,
      68,    69,    70,    71,    72,  1322,   265,     0,     0,  1323,
     116,  1324,     0,     0,     0,     0,   350,     0,   348,     0,
     318,     0,     0,     0,  1784,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   150,   554,     0,   557,   169,     0,
       0,     0,    76,   116,     0,  1758,     0,   116,     0,     0,
       0,     0,     0,   116,     0,     0,   116,     0,     0,     0,
     265,     0,   150,   150,     0,  2086,   328,     0,     0,     0,
       0,   344,   116,   376,     0,     0,     0,     0,     0,   169,
     153,    58,     0,   473,   473,   169,     0,     0,   169,     0,
       0,  1787,   169,     0,     0,     0,   441,   150,     0,     0,
       0,     0,   637,     0,     0,     0,   557,     0,   116,   441,
       0,     0,   265,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,  2086,  2086,     0,     0,   363,
       0,  1425,  1425,  1425,   154,   555,   318,   318,     0,     0,
       0,    74,     0,     0,     0,     0,   116,     0,     0,     0,
       0,     0,     0,     0,   263,     0,     0,     0,     0,  1659,
    1659,   230,    76,   116,     0,   116,     0,  2048,  2086,     0,
       0,  1787,    79,    80,   116,     0,     0,   637,     0,   265,
       0,   440,     0,     0,     0,   116,     0,   148,   263,   582,
     583,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     585,     0,     0,   116,     0,     0,     0,     0,   116,     0,
     116,     0,  1787,   265,   116,   205,     0,     0,   265,     0,
       0,     0,   350,     0,   265,     0,    58,     0,     0,     0,
     169,     0,     0,     0,   116,     0,     0,     0,    77,   169,
     169,   807,     0,     0,     0,     0,     0,     0,   154,     0,
       0,     0,     0,     0,   116,     0,   265,   116,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
     116,   148,  1787,  1787,   116,    65,    66,    67,    68,    69,
      70,    71,    72,  1322,     0,     0,    74,  1323,     0,  1324,
       0,     0,     0,    14,    15,    16,    17,    18,     0,   169,
       0,     0,     0,     0,     0,     0,   319,    76,   169,   441,
       0,   169,     0,   169,   169,  1787,     0,    79,    80,     0,
      76,     0,     0,  1610,     0,    58,     0,     0,   440,   440,
     148,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   263,   441,     0,   318,     0,     0,     0,     0,
       0,  1801,    58,     0,     0,     0,     0,   148,   169,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,   116,
       0,     0,     0,   441,     0,     0,     0,     0,  1816,   265,
       0,    77,     0,     0,   148,    74,   228,   229,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,   263,
       0,     0,     0,     0,     0,  1631,    76,     0,     0,     0,
       0,     0,    74,  1659,     0,     0,    79,    80,   148,     0,
     200,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,   350,   812,    76,   154,     0,   644,     0,     0,     0,
     169,     0,     0,    79,   813,   116,     0,   110,     0,     0,
       0,     0,     0,     0,     0,     0,   441,   441,     0,   440,
       0,   265,   116,     0,     0,     0,   318,    76,     0,     0,
     834,     0,   440,     0,     0,   440,   440,   116,   440,     0,
       0,     0,     0,     0,   473,   473,     0,   440,     0,     0,
     440,   440,   440,     0,     0,     0,   265,     0,     0,     0,
       0,     0,  1904,     0,     0,   110,     0,     0,     0,   265,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   116,
     116,     0,   116,     0,     0,     0,   169,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   376,     0,   116,   441,
       0,   265,    14,    15,    16,    17,    18,  1659,     0,   116,
     274,     0,     0,     0,     0,   169,     0,     0,     0,     0,
       0,     0,   116,     0,     0,   265,     0,     0,     0,   585,
     440,     0,   265,     0,     0,   116,   263,   965,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   116,   169,     0,
       0,     0,     0,   110,   169,     0,     0,     0,     0,     0,
     441,    58,     0,   116,   116,     0,   441,     0,     0,     0,
       0,     0,   353,     0,     0,   441,     0,     0,   116,   116,
     116,   148,     0,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,   474,
     769,   770,   771,   772,   773,   774,   775,   776,   777,   778,
     779,    74,     0,     0,   215,     0,  2026,     0,     0,   169,
       0,     0,     0,     0,   441,     0,     0,     0,     0,     0,
       0,  2085,    76,     0,     0,   530,   110,   204,     0,     0,
     116,   780,    79,    80,     0,     0,   473,     0,   441,     0,
       0,     0,     0,  1250,   440,   116,     0,     0,     0,   116,
       0,     0,  1659,     0,   110,   441,     0,     0,   116,     0,
       0,     0,     0,     0,     0,   110,     0,     0,   577,     0,
       0,     0,     0,   263,     0,     0,     0,   116,   116,     0,
    1659,  2026,     0,   353,     0,     0,     0,   169,   169,     0,
     110,     0,     0,   204,   274,   378,   116,     0,     0,     0,
     169,     0,     0,     0,     0,     0,     0,     0,     0,   204,
       0,     0,     0,     0,     0,  1659,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     0,  1155,     0,     0,
       0,     0,     0,   204,   636,     0,     0,   274,     0,     0,
       0,     0,     0,     0,  2128,     0,   470,     0,     0,     0,
     636,     0,     0,     0,   636,     0,   116,     0,     0,     0,
     441,     0,     0,     0,     0,   116,     0,     0,     0,     0,
       0,   116,   441,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,   116,     0,  1269,   441,   148,     0,
     172,   173,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,   204,   169,     0,     0,     0,   148,     0,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     148,     0,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,    74,   484,   441,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,   636,     0,   230,    76,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    79,    80,   812,    76,
       0,   204,   644,     0,     0,     0,     0,   169,     0,    79,
     813,     0,     0,   263,     0,  1250,     0,     0,     0,     0,
       0,   204,   646,   148,  1424,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
     116,     0,     0,     0,   116,     0,     0,     0,     0,     0,
       0,   116,     0,     0,     0,   353,   440,     0,     0,     0,
       0,   116,     0,     0,     0,     0,     0,     0,   116,     0,
       0,   488,   474,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,   148,   110,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
     116,     0,     0,     0,   440,     0,     0,     0,     0,     0,
     204,     0,   169,   116,    74,     0,     0,   116,     0,     0,
       0,   116,     0,     0,     0,     0,     0,     0,     0,   353,
     474,     0,   110,     0,  1631,    76,   115,     0,     0,    58,
     204,  1632,     0,   116,     0,    79,    80,     0,   636,   474,
     440,     0,   116,     0,     0,     0,     0,     0,     0,   353,
       0,   441,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   148,   636,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   441,   636,     0,     0,     0,     0,
       0,   441,     0,     0,   115,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   265,   116,     0,     0,     0,     0,   319,
      76,     0,     0,   204,   204,     0,     0,     0,     0,   470,
      79,    80,   116,     0,     0,     0,     0,     0,     0,   276,
       0,     0,     0,   441,     0,     0,     0,  1269,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
    1522,  1424,  1424,  1424,   153,  1620,  1621,  1625,     0,     0,
       0,     0,   115,   116,   441,     0,    74,     0,   116,     0,
       0,     0,   115,     0,   474,     0,     0,   204,     0,     0,
       0,     0,     0,     0,     0,     0,   230,    76,     0,     0,
     353,   357,     0,     0,     0,     0,   470,    79,    80,     0,
       0,     0,     0,     0,     0,   353,     0,     0,     0,   353,
       0,     0,     0,     0,     0,   474,     0,     0,   353,   204,
       0,     0,     0,     0,     0,     0,     0,     0,   476,     0,
       0,   116,   116,     0,     0,     0,     0,   353,   353,     0,
       0,     0,     0,     0,   204,     0,     0,     0,     0,     0,
       0,   116,     0,     0,     0,   116,   353,     0,     0,   116,
       0,     0,     0,     0,     0,   115,     0,     0,  1250,     0,
       0,     0,  1611,     0,     0,     0,     0,     0,     0,   116,
     116,   116,   116,   116,   116,   116,     0,     0,     0,     0,
       0,   265,     0,   115,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   115,     0,     0,   441,   441,    14,
      15,    16,    17,    18,     0,     0,   353,     0,     0,     0,
     110,     0,   357,     0,     0,   353,     0,     0,     0,   115,
       0,   470,   148,   276,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,   636,     0,     0,   274,   265,     0,
       0,     0,     0,     0,     0,   204,     0,     0,     0,     0,
      74,     0,     0,     0,   121,   440,     0,   121,    58,     0,
     441,     0,   470,   638,     0,   116,   276,     0,     0,     0,
    2085,    76,     0,     0,   530,     0,     0,     0,     0,   638,
       0,    79,    80,   638,   470,   470,   116,   474,     0,     0,
     148,     0,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,     0,   470,     0,     0,     0,     0,     0,     0,
       0,     0,   121,     0,     0,     0,     0,     0,    74,   116,
       0,     0,     0,     0,     0,     0,   116,     0,     0,     0,
     116,     0,     0,     0,     0,     0,     0,   121,  1631,    76,
       0,   721,     0,     0,  1250,     0,     0,     0,     0,    79,
      80,     0,     0,     0,     0,     0,     0,   121,     0,     0,
     353,     0,     0,     0,   353,   289,     0,     0,     0,     0,
       0,   353,   470,     0,     0,     0,   440,     0,     0,   204,
       0,   353,   638,   441,     0,     0,     0,     0,   353,     0,
     121,     0,     0,     0,   121,     0,     0,     0,     0,     0,
     121,     0,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   265,   116,     0,     0,   148,
     353,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,   353,     0,     0,     0,   353,     0,     0,
       0,   353,     0,   121,   204,     0,     0,    74,     0,     0,
       0,   441,     0,     0,   357,   121,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   319,    76,   116,
       0,   476,   116,     0,     0,     0,     0,     0,    79,    80,
       0,   110,     0,     0,     0,     0,   115,     0,   889,   891,
       0,     0,     0,   121,     0,   214,     0,     0,     0,     0,
       0,     0,     0,     0,   116,     0,     0,     0,     0,     0,
     121,   110,   121,     0,     0,     0,     0,   121,     0,     0,
       0,   121,   116,   116,     0,     0,     0,     0,   357,   476,
       0,   115,   121,     0,   274,   296,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   638,   476,     0,
       0,     0,     0,     0,     0,   121,     0,   121,   357,     0,
       0,   121,     0,   636,     0,     0,     0,     0,     0,     0,
       0,   638,   265,     0,     0,     0,     0,   470,     0,     0,
       0,   121,     0,     0,   638,   441,     0,     0,     0,     0,
       0,     0,     0,   353,   474,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,   721,    46,     0,    47,     0,     0,   721,     0,
       0,     0,     0,     0,     0,     0,     0,   721,     0,     0,
       0,     0,     0,     0,    58,     0,   121,     0,     0,     0,
       0,   353,   353,     0,     0,     0,   721,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   353,     0,   476,     0,   353,     0,     0,     0,   353,
     121,     0,     0,     0,     0,     0,     0,     0,     0,   357,
       0,     0,  1046,     0,   116,     0,     0,     0,     0,   204,
       0,     0,     0,     0,   357,     0,   121,     0,   357,     0,
     204,     0,     0,   608,   476,     0,     0,   357,     0,     0,
       0,     0,     0,     0,   116,     0,     0,   110,   110,     0,
       0,     0,     0,     0,     0,     0,   357,   357,     0,     0,
     116,   204,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   357,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   116,   116,
       0,     0,   265,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,   120,     0,     0,     0,     0,     0,
     474,     0,   116,   121,   121,   353,     0,     0,     0,     0,
       0,     0,     0,   116,     0,     0,     0,     0,   470,   470,
       0,     0,     0,     0,   121,   357,     0,     0,     0,   115,
       0,     0,     0,     0,   357,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,   638,     0,     0,   276,     0,     0,   353,
       0,     0,     0,     0,     0,     0,   353,     0,     0,   121,
     353,     0,     0,   810,   120,   811,     0,     0,     0,     0,
       0,     0,     0,     0,   827,   828,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   476,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,   120,     0,     0,   121,     0,     0,   120,     0,     0,
     120,     0,     0,     0,     0,     0,     0,   121,     0,     0,
     121,   121,     0,   121,     0,     0,   274,   204,     0,     0,
       0,     0,   121,     0,     0,   121,   121,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   357,
     120,     0,     0,   357,     0,     0,     0,     0,     0,     0,
     357,   110,   120,     0,     0,     0,     0,     0,     0,     0,
     357,     0,     0,     0,     0,     0,     0,   357,     0,   353,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,     0,     0,     0,     0,     0,     0,     0,   357,
       0,     0,     0,     0,     0,   121,     0,   120,     0,   120,
       0,     0,   357,     0,   120,     0,   357,     0,   120,     0,
     357,     0,   353,   353,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     204,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,   120,     0,     0,     0,   120,     0,
     115,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
     115,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   721,     0,     0,     0,
       0,     0,     0,   276,     0,     0,     0,     0,   415,     0,
       0,     0,     0,     0,     0,     0,   204,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   121,   121,
       0,     0,   638,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   357,   476,     0,     0,     0,     0,     0,   470,
     470,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,  1097,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   636,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   353,     0,     0,     0,     0,     0,
     357,   357,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     357,     0,     0,     0,   357,     0,     0,     0,   357,     0,
       0,   125,     0,     0,   125,     0,     0,     0,   110,   636,
       0,     0,     0,     0,  1242,  1243,     0,     0,  1245,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,   120,   353,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,   115,   115,     0,     0,
       0,   120,     0,   695,     0,     0,     0,   415,   701,   125,
       0,     0,     0,     0,     0,     0,  1650,   710,   711,  1653,
    1667,     0,     0,     0,     0,  1674,     0,     0,     0,  1678,
       0,  1680,   415,   415,   125,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,  1318,   415,   125,     0,     0,     0,     0,   476,
     121,     0,     0,     0,   357,     0,     0,     0,     0,   121,
       0,   470,     0,     0,     0,     0,     0,     0,   121,     0,
       0,     0,     0,   415,     0,     0,     0,   125,     0,     0,
       0,   125,     0,     0,     0,     0,     0,   125,  1339,     0,
     125,   121,     0,     0,     0,     0,     0,     0,   121,     0,
       0,   120,     0,     0,     0,     0,     0,     0,   357,     0,
       0,     0,     0,     0,   120,   357,     0,   120,   120,   357,
     120,   121,     0,     0,     0,     0,     0,     0,     0,   120,
     125,     0,   120,   120,   120,  1363,     0,     0,     0,   121,
       0,     0,   125,     0,  1367,  1368,  1369,  1370,     0,     0,
       0,     0,  1375,  1376,     0,     0,     0,     0,     0,     0,
       0,     0,  1384,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1775,     0,     0,     0,     0,
     125,     0,     0,     0,     0,   121,     0,     0,     0,     0,
    1404,     0,     0,  1407,     0,     0,     0,   125,     0,   125,
       0,     0,     0,     0,   125,   276,     0,     0,   125,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,   125,
       0,     0,     0,  1815,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1830,  1832,
     115,     0,   125,     0,   125,     0,     0,     0,   125,     0,
       0,     0,  1465,     0,     0,     0,     0,     0,   357,     0,
       0,     0,     0,  1653,     0,     0,     0,     0,   125,  1850,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1487,     0,     0,     0,     0,     0,     0,  1491,     0,
    1493,  1495,     0,     0,     0,     0,   121,   121,   121,   121,
     121,   121,   121,  1505,     0,  1506,     0,  1507,     0,     0,
       0,   357,   357,     0,  1516,     1,     0,     0,   147,     0,
       0,     0,     0,     0,   121,   121,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,   125,     0,   120,   120,     0,     0,     0,
       0,   415,   415,   415,   415,   415,   415,   415,   415,   415,
     415,   415,   415,   415,   415,   415,   415,   415,   415,   415,
       0,     0,     0,     0,   115,     0,     0,   125,     0,     0,
    1922,     0,     0,     0,     0,     0,     0,     0,     0,  1925,
       0,  1927,     0,   203,  1932,  1936,     0,  1667,     0,     0,
       0,     0,  1942,   125,     0,     0,     0,     0,     0,  1591,
       0,     0,     0,   121,     0,     0,  1596,     0,     0,     0,
    1597,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   415,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,  1614,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -483,  -483,     0,  -483,    46,     0,
      47,     0,  -483,     0,     0,     0,     0,     0,     0,     0,
     125,   125,     0,   638,     0,     0,     0,     0,     0,    58,
    1696,     0,  2011,     0,     0,     0,     0,  2016,  2018,     0,
     121,   125,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   357,     0,     0,     0,  2038,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,   115,
       0,     0,     0,   121,     0,  1726,     0,     0,     0,     0,
       0,     0,  1730,     0,  1732,     0,   125,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   115,   638,     0,
    2063,     0,  2066,   556,     0,  2068,  2070,     0,   121,     0,
    2073,  2075,    77,     0,     0,     0,     0,   120,     0,     0,
       0,   357,     0,   415,     0,     0,   120,     0,   415,   121,
       0,     0,   115,     0,     0,   120,     0,     0,     0,   415,
       0,     0,     0,   597,     0,     0,     0,     0,     0,     0,
    1765,   125,     0,     0,     0,   604,     0,     0,   120,  1769,
       0,   121,     0,     0,   125,   120,     0,   125,   125,     0,
     125,     0,   614,     0,     0,     0,  2119,  2121,  2123,   125,
       0,   415,   125,   125,   125,     0,     0,     0,   120,     0,
       0,     0,   633,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,  2148,  2150,  2152,   175,   178,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   121,     0,     0,     0,     0,     0,     0,   727,
       0,   223,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   125,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
     768,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1868,  1869,     0,     0,     0,
       0,   314,     0,     0,   315,     0,   806,     0,     0,  1878,
     809,   463,   364,     0,     0,     0,     0,     0,     0,   338,
       0,   415,     0,     0,     0,     0,     0,     0,     0,   831,
       0,     0,     0,   832,   833,     0,     0,   836,     0,     0,
       0,     0,     0,   533,     0,     0,     0,     0,     0,     0,
     533,     0,   850,   851,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,   120,   120,   120,   120,   120,   120,
       0,     0,     0,     0,     0,   881,     0,     0,     0,     0,
     505,     0,     0,     0,   125,     0,     0,     0,     0,     0,
       0,   120,   120,     0,     0,   125,   125,     0,     0,     0,
       0,     0,     0,   415,     0,     0,     0,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   415,     0,     0,     0,     0,   564,   565,
       0,   533,     0,     0,     0,   121,     0,     0,     0,   175,
       0,   415,   415,   415,     0,     0,     0,     0,   415,   415,
       0,   919,     0,     0,   175,     0,     0,   364,   648,     0,
       0,     0,  1979,     0,   925,     0,     0,     0,     0,     0,
     121,     0,   415,     0,     0,     0,     0,   669,     0,     0,
     120,     0,     0,     0,     0,     0,     0,     0,   615,   948,
       0,     0,     0,     0,     0,   618,   620,     0,     0,     0,
     627,     0,     0,     0,     0,     0,     0,     0,     0,   415,
     415,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   338,     0,
       0,   338,     0,     0,     0,     0,     0,     0,     0,   533,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     991,     0,     0,     0,     0,   533,   802,     0,   533,   805,
       0,     0,     0,     0,     0,     0,   364,   120,     0,     0,
     648,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2084,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,     0,     0,     0,     0,     0,     0,     0,   167,
       0,   533,     0,     0,     0,   533,     0,     0,     0,   223,
       0,     0,     0,     0,     0,     0,     0,   125,     0,     0,
       0,   829,   830,     0,     0,   120,   125,     0,     0,     0,
    2125,     0,     0,     0,     0,   125,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2144,     0,   125,  1112,
       0,     0,     0,  1986,     0,   125,     0,     0,  1121,     0,
       0,  2153,   294,     0,  1124,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,   300,     0,   301,   125,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     533,     0,     0,   364,     0,     0,   125,     0,     0,     0,
       0,   387,     0,     0,   388,     0,   389,     0,   390,     1,
       0,   938,   364,  1167,     0,     0,     0,     1,     0,     0,
       0,     0,   648,     0,     0,   391,   648,     0,     0,     0,
       0,     0,     0,   956,     0,   364,     0,     0,     1,     0,
       0,     0,   125,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,   392,   393,   338,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,     0,   401,   402,   535,   536,
       0,     0,   540,     0,    74,   543,   544,     0,   546,     0,
     547,     0,     0,     0,     0,     0,  1298,     0,     0,     0,
       0,     0,     0,     0,   403,   968,   415,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   125,   125,   125,   125,   125,   125,   125,
       0,     0,     0,   533,   533,     0,     0,     0,     0,     0,
       0,     0,     0,   533,  1067,     0,   533,  1070,     0,     0,
       0,   125,   125,     0,     0,     0,   630,   631,   364,     0,
       0,   648,     0,   648,   648,     0,     0,     0,     0,     0,
     648,   663,     0,     0,     0,     0,     0,     0,     0,   211,
     364,   364,     0,     0,   120,     0,  1365,     0,  1366,     0,
       0,     0,     0,     0,     0,   269,     0,     0,     0,   364,
       0,     0,   533,     0,     0,     0,   533,     0,     0,     0,
       0,     0,   120,   533,  1140,     0,     0,   533,  1144,     0,
       0,   533,  1148,     0,     0,  1099,     0,     0,     0,  1152,
       0,     0,  1111,  1440,     0,     0,     0,     0,     0,     0,
     125,     0,     0,     0,     0,   211,     0,   120,   147,   329,
       0,     0,     0,     1,     0,     0,     0,     0,     0,     0,
       0,   369,     0,     0,     0,     0,     0,   800,   364,   533,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   211,     0,     0,     0,     0,
       0,   415,     0,     0,     0,     0,     0,     0,   483,     0,
     648,   487,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1182,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   415,     0,     0,     0,     0,     0,   125,   877,     0,
     364,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1504,     0,     0,     0,   211,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   269,     0,
     125,    14,    15,    16,    17,    18,    19,  1530,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,   125,     0,   533,     0,    46,
       0,    47,     0,   487,     0,    48,    49,    50,    51,    52,
      53,    54,    55,   211,   648,   648,   125,   415,     0,   415,
      58,   648,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   641,     0,   658,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   125,     0,
       0,     0,   954,   955,    63,    64,     0,     0,   415,     0,
       0,     0,     0,   364,     0,   962,     0,   533,  1388,     0,
     533,  1392,     0,     0,   533,  1396,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,   725,     0,
       0,     0,   415,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   987,    77,   981,     0,     0,     0,  1702,  1703,
       0,    79,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   211,     0,     0,     0,     0,     0,     0,   125,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   415,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   641,     0,     0,     0,     0,     0,   826,     0,
       0,     0,     0,  1713,     0,     0,  1716,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1444,
    1446,  1448,     0,     0,     0,     0,     0,     0,     0,  1060,
    1061,     0,     0,     0,     0,  1065,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,     0,     0,     0,
    1469,     0,   648,  1512,     0,     0,  1086,     0,     0,  1089,
    1090,     0,  1093,     0,  1095,  1096,     0,     0,     0,     0,
    1182,     0,     0,     0,     0,   211,   211,   364,     0,     0,
       0,   483,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1766,  1767,  1138,     0,     0,     0,  1142,
       0,     0,     0,  1146,  1523,     0,     0,     0,     0,     0,
       0,   533,  1566,     0,     0,     0,     0,     0,  1813,  1716,
     533,  1573,     0,   648,   125,     0,     0,     0,     0,   369,
       0,  1824,     0,     0,   364,   364,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   483,     0,
     942,     0,   125,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   439,  1261,  1262,     0,     0,
       0,   641,     0,     0,     0,     0,  1858,     0,   468,     0,
    1278,     0,     0,     0,     0,     0,     0,   125,     0,     0,
       0,   496,     0,   496,     0,     0,   211,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   725,
       0,     0,   725,   725,     0,   725,     0,     0,     0,     0,
       0,     0,  1861,  1862,   725,     0,     0,   725,   725,   725,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1643,  1644,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,  1914,     0,     0,     0,
    1924,     0,     0,   483,     0,     0,     0,     0,     0,     0,
       0,     0,   609,     0,  1937,     0,     0,     0,     0,     0,
       0,   648,     0,     0,  1946,     0,     0,   211,     0,     0,
       0,     0,  1278,     0,     0,     0,     0,  1958,     0,  1960,
    1961,  1962,  1963,     0,   483,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   483,   483,  1948,     0,
       0,     0,     0,  1380,     0,     0,     0,     0,     0,  1386,
       0,     0,  1390,     0,     0,   483,  1394,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1716,     0,  1995,     0,     0,     0,  2000,
     533,     0,     0,     0,  2005,     0,     0,     0,  1978,     0,
       0,     0,     0,     0,     0,     0,   533,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2035,
       0,     0,  1994,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   483,     0,     0,     0,     0,     0,
       0,   211,     0,     0,     0,     0,     0,     0,     0,  2022,
       0,     0,  2023,     0,     0,   826,     0,     0,     0,     0,
    1809,     0,     0,  2059,     0,     0,     0,  2062,   496,     0,
       0,     0,     0,     0,   496,     0,     0,     0,     0,   847,
    2076,     0,   364,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1510,     0,   369,     0,     0,     0,
       0,     0,     0,  1520,  1521,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2112,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   364,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   533,   533,     0,     0,     0,     0,     0,     0,
       0,     0,  2135,     0,     0,  2136,     0,  2138,   533,     0,
       0,     0,     0,  1564,  2142,     0,     0,     0,     0,   918,
       0,     0,  1571,     0,     0,  1575,     0,  1578,  1579,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2162,     0,  2163,     0,     0,   468,  2165,
       0,  2142,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   950,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1606,  2165,    14,    15,    16,    17,    18,   483,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -482,  -482,
       0,  -482,    46,   533,    47,     0,  -482,   985,     0,     0,
    1964,   533,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   725,     0,    58,     0,     0,     0,     0,     0,     0,
       0,   847,  1005,     0,     0,  1007,     0,  1009,     0,     0,
       0,     0,     0,  1018,  1710,  1023,  1018,     0,     0,     0,
       0,     0,     0,   725,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1051,     0,     0,     0,   364,     0,   533,
    2049,     0,   269,   533,     0,     0,  1053,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1062,     0,     0,
       0,   211,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   468,   641,     0,  1051,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   533,     0,     0,     0,     0,     0,
    1575,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1115,     0,   369,   496,     0,     0,   725,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1127,     0,  1771,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   533,   533,  1153,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     483,   483,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   533,     0,     0,
       0,   439,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1268,  1270,     0,     0,     0,
       0,     0,     0,   468,     0,     0,     0,     0,   725,   725,
     725,     0,     0,   725,   725,     0,     0,     0,     0,     0,
     487,     0,     0,  1859,     0,     0,     0,     0,     0,     0,
       0,     0,  1018,     0,     0,     0,     0,     0,     0,     0,
    2164,     0,     0,     0,     0,     0,  1051,     0,     0,     0,
       0,     0,     0,     0,  1311,     0,     0,  1439,     0,     0,
       0,  1018,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   269,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,     0,
       0,   388,     0,   389,     0,   390,     0,     0,     0,   369,
       0,  1908,  1909,     0,     0,     0,     0,   496,     0,     0,
    1184,     0,   391,    -2,  1913,  1186,  -244,  -244,  1187,  1188,
    1189,  1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,  1198,
    -338,  -338,  1199,  1200,  1201,  1202,  1203,     0,  1204,     0,
     392,   393,     0,   490,   395,  1205,  1206,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,  1207,   398,
     399,   400,     0,   401,   402,     0,     0,     0,     0,     0,
       0,    74,     0,     0,   496,     0,  1379,  1276,  1382,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
    -244,  1208,     0,     0,    77,   404,     0,     0,     0,   298,
       0,   405,    79,    80,   406,   407,   408,   409,     0,     0,
       0,     0,     0,     0,     0,     0,  -185,  1984,     0,     0,
       0,   387,   211,     0,   388,     0,   389,     0,   390,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,   391,     0,     0,     0,     0,
       0,     0,     0,     0,   269,     0,     0,  1455,  1455,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,  2047,   398,   399,   400,     0,   401,   402,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,   369,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   403,     0,  1508,    77,   404,     0,
       0,     0,  1517,     0,   405,   466,    80,   406,   407,   408,
     409,     0,     0,   725,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   468,     0,     0,     0,     0,     0,     0,
       0,   483,   483,     0,     0,     0,     0,     0,     0,     0,
     496,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1018,     0,     0,   847,     0,
       0,     0,     0,     0,   183,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,     0,    19,     0,
      20,   269,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,   252,   253,     0,
     254,    46,     0,    47,     0,   255,     0,     0,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
    1608,  1609,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1018,     0,     0,     0,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,   496,     0,    20,   847,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   725,  -458,     0,     0,     0,     0,     0,
       0,     0,     0,  2164,     0,     0,     0,     0,    58,  1005,
       0,     0,     0,     0,     0,     0,     0,  -458,  1728,  1729,
    1439,     0,     0,   483,     0,     0,     0,     0,     0,   496,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     184,     0,   185,   186,     0,     0,     0,     0,   496,     0,
     847,   387,     0,     0,   388,     0,   389,     0,   390,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   725,     0,
       0,   487,     0,  1184,     0,   391,    -2,     0,  1186,  -245,
    -245,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,
    1196,  1197,  1198,  -338,  -338,  1199,  1200,  1201,  1202,  1203,
       0,  1204,     0,   392,   393,     0,   490,   395,  1205,  1206,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,  1207,   398,   399,   400,     0,   401,   402,     0,  1857,
       0,     0,   439,     0,    74,     0,     0,  1800,     0,     0,
       0,     0,     0,     0,     0,     0,  1439,     0,     0,     0,
       0,     0,     0,  -245,  1208,     0,     0,    77,   404,     0,
       0,     0,   298,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   387,     0,  -185,
     388,     0,   389,     0,   390,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1844,     0,     0,     0,  1184,
       0,   391,    -2,     0,  1186,     0,     0,  1187,  1188,  1189,
    1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,  1198,  -338,
    -338,  1199,  1200,  1201,  1202,  1203,     0,  1204,     0,   392,
     393,     0,   490,   395,  1205,  1206,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,  1207,   398,   399,
     400,  1880,   401,   402,  1882,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1208,  1899,     0,    77,   404,     0,     0,     0,   298,     0,
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
    1195,  1196,  1197,  1198,  -338,  -338,  1199,  1200,  1201,  1202,
    1203,     0,  1204,     0,   392,   393,    61,   490,   395,  1205,
    1206,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,  1207,   398,   399,   400,     0,   401,   402,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -3,  1208,     0,     0,    77,   435,
       0,  1018,     0,   298,     0,   405,    79,    80,   406,   407,
     408,   409,     0,     0,     0,     0,     0,     0,     0,     0,
    -185,     4,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,  1183,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   387,     0,    46,
     388,    47,   389,     0,   390,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1184,
      58,  1185,    -2,     0,  1186,     0,     0,  1187,  1188,  1189,
    1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,  1198,  -338,
    -338,  1199,  1200,  1201,  1202,  1203,     0,  1204,     0,   392,
     393,    61,   490,   395,  1205,  1206,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,  1207,   398,   399,
     400,     0,   401,   402,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1208,     0,     0,    77,   435,     0,     0,     0,   298,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,     0,     0,  -185,     4,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   387,     0,    46,   388,    47,   389,     0,   390,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   391,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   392,   393,    61,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,     0,   401,   402,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1661,
    1662,  1663,     0,     0,     0,   403,  1664,  1665,    77,   435,
       0,     0,     0,     0,     0,   405,    79,    80,   406,   407,
     408,   409,     0,     0,     0,     0,     0,     0,     0,     0,
    1666,     4,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   387,     0,    46,
     388,    47,   389,     0,   390,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   391,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
     393,    61,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,     0,   401,   402,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1661,  1662,  1663,     0,     0,     0,
     403,  1664,     0,    77,   435,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,     0,     0,  1666,     4,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   387,     0,    46,   388,    47,   389,     0,   390,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   391,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   392,   393,    61,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,     0,   401,   402,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   403,     0,  1652,    77,   435,
       0,     0,     0,     0,     0,   405,    79,    80,   406,   407,
     408,   409,     4,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   387,     0,
      46,   388,    47,   389,     0,   390,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   391,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     392,   393,    61,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,     0,   401,   402,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   403,     0,     0,    77,   435,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   387,     0,    46,   388,    47,   389,     0,
     390,   345,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   391,     0,     0,
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
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,     0,   401,   402,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   403,     0,     0,    77,  1265,     0,     0,     0,     0,
       0,   405,  1266,    80,   406,   407,   408,   409,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   387,     0,    46,   388,    47,   389,     0,
     390,   345,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,     0,   401,   402,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   403,     0,     0,    77,
     404,     0,     0,     0,     0,     0,   405,    79,    80,   406,
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
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,     0,   401,   402,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   403,     0,     0,    77,   465,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,  1993,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,
      -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,    -2,
       0,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
      -2,    -2,  2021,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,     0,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,
      -2,     0,  1514,    -2,     0,     0,     0,     0,    -2,    -2,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,   387,     0,     0,   388,
       0,   389,     0,   390,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,     0,     0,    58,
     391,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,     0,     0,   392,   393,
       0,   394,   395,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   396,   397,   384,     0,   398,   399,   400,
       0,   401,   402,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   403,
       0,     0,    77,   404,     0,     0,     0,     0,     0,   405,
    1515,    80,   406,   407,   408,   409,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,    59,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    61,    62,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,    76,     0,    77,    78,
       0,     0,     0,     0,     0,     0,    79,    80,   257,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -483,  -483,     0,  -483,    46,     0,    47,     0,
    -483,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   148,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,    76,     0,
      77,   258,     0,     0,     0,  -815,     0,     0,    79,    80,
     257,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -483,  -483,     0,  -483,    46,     0,
      47,     0,  -483,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   148,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
      76,     0,    77,   258,     0,     0,     0,     0,     0,     0,
      79,    80,     4,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,     0,     0,     0,     0,  -406,  -406,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -406,     0,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,    79,    80,     4,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,     0,     0,     0,     0,  -407,  -407,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -407,     0,     0,     0,    77,    78,     0,  1416,
       0,  1417,     0,     0,    79,    80,  1418,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1420,     0,     0,
       0,    77,   981,     0,  1416,     0,  1417,     0,     0,    79,
      80,  1418,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1617,     0,     0,     0,    77,   981,     0,  1416,
       0,  1417,     0,     0,    79,    80,  1418,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1618,     0,     0,
       0,    77,   981,     0,  1416,     0,  1417,     0,     0,    79,
      80,  1418,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1619,     0,     0,     0,    77,   981,     0,     0,
       0,     0,     0,     0,    79,    80,   257,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   257,     0,     0,     0,     0,     0,     0,    77,   258,
       0,    14,    15,    16,    17,    18,    79,    80,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -483,  -483,     0,  -483,    46,
       0,    47,     0,  -483,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   327,     0,     0,     0,     0,     0,
       0,    79,    80,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   345,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   148,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,   588,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1075,    76,  -692,    77,   644,     0,     0,     0,     0,
       0,     0,    79,    80,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -483,  -483,     0,
    -483,    46,     0,    47,     0,  -483,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   148,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,    76,     0,    77,   258,     0,     0,     0,
    -819,     0,     0,    79,    80,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -483,  -483,
       0,  -483,    46,     0,    47,     0,  -483,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   148,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,    76,     0,    77,   258,     0,     0,
       0,     0,     0,     0,    79,    80,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   345,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,   588,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   643,     0,  -692,    77,   644,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,   588,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   784,     0,  -692,    77,   530,
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
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,  1106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -679,    77,
     347,     0,     0,     0,     0,     0,     0,    79,    80,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   345,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   346,
      77,   347,     0,     0,     0,     0,     0,     0,    79,    80,
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
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
    1894,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,  1896,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,   347,     0,     0,     0,     0,     0,     0,
      79,    80,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   345,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   327,     0,     0,     0,     0,     0,
       0,    79,    80,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   345,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,   347,     0,     0,     0,     0,
       0,     0,    79,    80,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -483,  -483,     0,
    -483,    46,     0,    47,     0,  -483,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,  1439,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   387,     0,     0,   388,     0,   389,     0,
     390,     0,     0,     0,     0,    77,   258,     0,     0,     0,
       0,     0,     0,    79,    80,  1184,     0,   391,    -2,     0,
    1186,  1915,  1916,  1187,  1188,  1189,  1190,  1191,  1192,  1193,
    1194,  1195,  1196,  1197,  1198,     0,     0,  1199,  1200,  1201,
    1202,  1203,     0,  1204,     0,   392,   393,     0,   490,   395,
    1205,  1206,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,  1207,   398,   399,   400,  1439,   401,   402,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1208,     0,   387,    77,
     404,   388,     0,   389,   298,   390,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,     0,
    1184,  -185,   391,    -2,     0,  1186,     0,     0,  1187,  1188,
    1189,  1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,  1198,
       0,     0,  1199,  1200,  1201,  1202,  1203,     0,  1204,     0,
     392,   393,     0,   490,   395,  1205,  1206,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,  1207,   398,
     399,   400,     0,   401,   402,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1208,     0,     0,    77,   404,     0,     0,     0,   298,
       0,   405,    79,    80,   406,   407,   408,   409,     0,     0,
       0,     0,     0,     0,     0,     0,  -185,   302,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -410,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,   302,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,    77,
      46,     0,    47,     0,  -410,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -411,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,   302,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,    77,    46,     0,    47,     0,  -411,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,    19,   712,    20,
     713,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   387,    77,
      46,   388,    47,   389,  -410,   390,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   391,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   714,     0,     0,     0,     0,  1198,
       0,  -338,     0,     0,     0,     0,     0,     0,     0,     0,
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,     0,   401,   402,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1208,     0,     0,    77,   715,     0,     0,     0,   298,
       0,   405,    79,    80,   716,   717,   408,   409,    14,    15,
      16,    17,    18,    19,   712,    20,   713,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   387,     0,    46,   388,    47,   389,
       0,   390,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   391,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     714,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,     0,   401,
     402,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   403,     0,     0,
      77,   715,     0,     0,     0,   298,     0,   405,    79,    80,
     716,   717,   408,   409,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     387,     0,    46,   388,    47,   389,     0,   390,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   391,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,     0,   401,   402,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,   434,    77,   435,     0,     0,
       0,     0,     0,   405,    79,    80,   406,   407,   408,   409,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   387,     0,    46,   388,
      47,   389,     0,   390,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     391,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,   392,   393,     0,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,     0,   401,   402,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   403,     0,     0,    77,   435,
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
     392,   393,     0,   394,   395,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   396,   397,   384,     0,   398,
     399,   400,     0,   401,   402,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   403,     0,     0,    77,   465,     0,     0,     0,     0,
       0,   405,    79,    80,   406,   407,   408,   409,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   387,     0,    46,   388,    47,   389,
       0,   390,   345,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   391,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,     0,   401,
     402,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   403,     0,     0,
      77,   404,     0,     0,     0,     0,     0,   405,    79,    80,
     406,   407,   408,   409,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   703,     0,   704,   705,   257,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -483,  -483,     0,  -483,    46,     0,    47,   -17,
    -483,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
      77,    46,     0,    47,     0,     0,     0,   345,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   148,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   588,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -692,    77,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   148,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,    76,     0,    77,    78,
       0,     0,     0,  -817,     0,     0,    79,    80,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   148,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,    76,     0,
      77,    78,     0,     0,     0,     0,     0,     0,    79,    80,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   148,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
      79,    80,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   345,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   588,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,  -692,    77,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   148,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,    76,     0,    77,
     327,     0,     0,     0,     0,     0,     0,    79,    80,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   345,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1177,
       0,     0,     0,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
      77,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   345,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,    77,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,  1534,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   981,     0,     0,     0,     0,     0,
       0,    79,    80,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,   310,     0,     0,     0,
       0,     0,     0,    79,    80,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,    78,     0,
       0,     0,     0,     0,     0,    79,    80,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
     461,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   345,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
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
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   310,     0,     0,     0,     0,     0,
       0,    79,    80,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   345,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,   461,     0,     0,     0,
       0,     0,     0,    79,    80,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -483,  -483,
       0,  -483,    46,     0,    47,     0,  -483,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,    58,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,   345,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    77,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   327,     0,
       0,     0,     0,     0,     0,    79,    80,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
     981,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   345,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,     0,     0,    14,    15,    16,    17,
      18,    79,    80,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,   327,
       0,    14,    15,    16,    17,    18,    79,    80,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -483,  -483,     0,  -483,    46,
       0,    47,     0,  -483,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
      58,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,     0,    63,    64,     0,     0,     0,
       0,    79,    80,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   387,     0,    46,   388,    47,   389,     0,   390,
       0,     0,     0,     0,    77,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   392,   393,     0,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,     0,   401,   402,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   403,     0,     0,    77,   404,
       0,     0,     0,     0,     0,   405,   466,    80,   406,   407,
     408,   409,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   387,     0,    46,   388,    47,   389,     0,   390,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,     0,   401,   402,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   403,     0,     0,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   148,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,    77,    47,     0,     0,     0,   345,    49,
      50,    51,    52,    53,    54,    55,     0,   387,     0,     0,
     388,     0,   389,    58,   390,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,     0,   401,   402,   387,     0,     0,   388,     0,   389,
      74,   390,     0,     0,     0,     0,    77,     0,     0,     0,
       0,     0,     0,     0,  1661,  1662,  1663,     0,   391,     0,
     403,  1831,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,  1930,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,     0,   401,
     402,   387,     0,     0,   388,     0,   389,    74,   390,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1661,  1662,  1663,     0,   391,     0,   403,  1931,     0,
      77,   404,     0,     0,     0,     0,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   490,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,    76,     0,   491,   492,     0,
       0,     0,   493,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,  1314,
       0,    77,   404,     0,     0,     0,  1315,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,   387,   401,   402,   388,     0,   389,     0,
     390,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,   403,     0,     0,    77,   404,     0,     0,     0,
     493,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,   387,   401,   402,
     388,     0,   389,     0,   390,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,   403,   846,     0,    77,
     404,     0,     0,     0,     0,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,   387,   401,   402,   388,     0,   389,     0,   390,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
     403,     0,     0,    77,   404,     0,     0,     0,   298,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,  1014,     0,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,     0,
       0,    77,   404,     0,     0,  1045,     0,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,   387,   401,   402,   388,     0,   389,     0,
     390,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,   403,  1381,     0,    77,   404,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,   387,   401,   402,
     388,     0,   389,     0,   390,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,   403,     0,     0,    77,
     404,     0,     0,     0,  1449,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,   387,   401,   402,   388,     0,   389,     0,   390,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
     403,     0,     0,    77,   404,     0,     0,     0,  1524,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,     0,  1921,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,  1926,
       0,    77,   404,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,   387,   401,   402,   388,     0,   389,     0,
     390,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,   403,  1935,     0,    77,   404,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,   387,   401,   402,
     388,     0,   389,     0,   390,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,   403,  2015,     0,    77,
     404,     0,     0,     0,     0,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,   387,   401,   402,   388,     0,   389,     0,   390,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
     403,  2017,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,  2065,     0,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,  2067,
       0,    77,   404,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,   387,   401,   402,   388,     0,   389,     0,
     390,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,   403,  2069,     0,    77,   404,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,   387,   401,   402,
     388,     0,   389,     0,   390,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,   403,  2072,     0,    77,
     404,     0,     0,     0,     0,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,   387,   401,   402,   388,     0,   389,     0,   390,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
     403,  2074,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,  2118,     0,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,  2120,
       0,    77,   404,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,   387,   401,   402,   388,     0,   389,     0,
     390,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,   403,  2122,     0,    77,   404,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,   387,   401,   402,
     388,     0,   389,     0,   390,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,   403,  2147,     0,    77,
     404,     0,     0,     0,     0,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,   387,   401,   402,   388,     0,   389,     0,   390,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
     403,  2149,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,  2151,     0,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,     0,
       0,    77,   404,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,   387,   401,   402,   388,     0,   389,     0,
     390,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,   694,     0,     0,    77,   404,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,   387,   401,   402,
     388,     0,   389,     0,   390,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,   700,     0,     0,    77,
     404,     0,     0,     0,     0,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,   387,   401,   402,   388,     0,   389,     0,   390,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
     709,     0,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,     0,     0,    77,   404,     0,
       0,     0,     0,     0,   405,   917,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,     0,
       0,    77,   404,     0,     0,     0,     0,     0,   405,   466,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,  2010,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,     0,   401,   402,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   403,     0,     0,    77,   404,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   703,
       0,   704,   705,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -483,  -483,     0,
    -483,    46,     0,    47,     0,  -483,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,    75,     1,     4,   230,    75,   741,    75,   166,     1,
     177,   256,     1,    75,   926,   225,   647,   403,   218,   271,
     656,    77,  1020,   550,   551,    59,   815,   726,  1226,  1208,
     137,   377,   821,   230,     4,  1783,   182,  1167,  1208,  1783,
     719,   251,   230,  1365,  1366,   364,   230,    97,   166,   368,
    1784,  1049,  1783,    84,   914,    56,    57,   643,    59,    10,
      59,   812,   814,    75,     0,   814,   166,    59,    75,   233,
      59,   235,   405,   561,    75,   230,   167,   141,   242,  1671,
      97,   240,   832,    84,   572,    84,   812,   493,   785,    75,
       0,    92,    75,   319,  1193,     1,    97,   812,    97,   100,
     850,   100,   902,   104,  1441,   104,   230,   230,   812,   268,
      75,   230,    99,   147,  1112,   230,  1915,   643,   192,   150,
     279,    84,   319,   923,   192,     4,   643,     0,   230,   617,
     192,   319,    89,   240,   104,   319,   379,   677,   202,   382,
       1,   142,   133,   181,   145,  2056,   147,    72,   147,    72,
     314,   315,   153,    59,   681,   147,   230,    98,   147,   160,
     230,   268,   230,    72,   319,   152,   167,    89,   230,   919,
    1904,  2082,   279,    90,   831,   832,   118,    56,    57,   152,
     192,   231,    20,   814,   175,   192,  1441,  1442,   152,   152,
     191,   192,   191,   850,   177,   319,   319,   304,   104,  2110,
     319,   158,  1323,   160,   319,   206,   192,   206,   914,    77,
      78,   162,   177,    92,   231,   216,   167,   319,   230,     1,
     221,   403,     4,   230,   225,   226,   812,   152,   259,   230,
     231,   137,   231,   156,  1826,   152,   177,   293,   160,   128,
     129,   147,   977,   152,   230,   319,   134,   156,   348,   319,
     251,   319,   110,   152,  2053,   154,   306,   319,   259,   152,
     259,   342,   919,   142,   177,   792,   145,   160,   269,   270,
     320,  1006,   273,   118,   273,   133,  1919,    59,   134,   280,
     168,   160,   534,   172,   173,   511,   812,  1076,   167,   306,
     542,  2025,   160,   294,   295,   812,   297,   328,   159,   499,
     381,   302,     1,   320,   146,   306,  1476,   306,   160,  1479,
    1480,   156,   168,   632,   511,   167,   995,   152,   319,   320,
     951,   320,   104,   511,  1075,  1185,   168,   511,   329,   203,
    1082,   350,   678,  1082,   240,   336,   337,  2085,   548,  1176,
     341,  2085,   221,   123,   554,   152,    97,   226,   152,  1075,
     669,   152,  2086,   739,  2085,  1215,   511,   152,  1695,  1164,
    1075,  1058,   268,   696,   155,   147,  1171,   467,  1168,  1176,
      75,  1075,   176,   279,  1124,   155,    62,   158,   379,   160,
     471,   382,   383,     1,   610,   176,    91,   511,   511,   133,
     269,   270,   511,   933,  2128,   158,   511,   157,   304,   463,
    1530,   280,   308,   108,   348,   168,   437,   109,     1,   511,
     151,   899,   571,   610,   100,   294,   295,   158,   297,   158,
     364,    72,   610,   302,   368,   111,   610,   113,  1667,   115,
     132,   971,  1553,  1554,  1555,   491,   177,   511,   177,     1,
    1695,    59,  1247,   511,  1766,  1767,   447,   158,   447,   511,
     329,  1082,    61,    62,   473,   610,   177,   336,   337,   158,
     159,    75,   341,  1176,   571,  2108,    59,  1124,   154,   470,
     471,   157,   158,   523,   158,   158,    90,   649,   222,  1185,
     231,   482,   483,    75,  1273,   158,   610,   610,     3,  1075,
     491,   610,   493,   182,   177,   610,   614,    59,    90,   511,
     379,   152,  2145,   382,   511,   156,   523,   100,   610,  1215,
     511,    10,   694,   152,   614,   697,   698,    72,   700,   137,
     206,   160,   523,   467,   523,   511,  1705,   709,   167,   147,
     712,   713,   714,   158,    72,  1705,   610,   154,     1,   156,
     631,     4,   610,   168,    84,    77,    78,   548,   610,  1075,
    1548,   160,   149,   554,   147,   306,   656,    97,  1075,   303,
     100,    69,   612,   160,   104,  1354,    75,  1666,   177,   320,
     297,   590,  1671,   280,  1829,   137,  1413,  1414,  1415,   176,
       1,   158,   616,    92,   158,   147,     3,   273,   295,   275,
     276,   470,  1831,  1832,   168,   612,    59,   152,   155,   108,
     177,   156,   158,   482,   483,  1410,  1413,  1414,  1415,   610,
      72,   612,   168,   612,   152,   616,   635,   616,   156,   938,
      75,   307,   240,    72,   616,   626,   312,   616,   160,   630,
     631,  1267,   318,   158,  1333,   158,   158,   100,    59,   839,
     659,   104,    97,   168,   816,  2090,  1052,   666,   820,  2094,
     268,   191,   151,   158,   177,   177,  1978,   829,   830,   403,
     110,   279,   663,   162,   350,   571,   206,   877,   167,   355,
      58,   357,   177,    61,    62,   676,    64,   158,   240,   825,
     273,   154,   556,   133,   147,   158,   304,   168,   632,    72,
     152,   231,  1931,  1932,   156,    72,  1431,   158,   153,   158,
    1413,  1414,  1415,   152,   154,   158,   268,   156,   394,   154,
     616,   767,   656,   158,   307,  1627,   137,   279,   177,   259,
    1260,   106,   107,   814,   177,   669,   147,  1826,   766,  1256,
     731,   160,   733,   273,   735,  1441,  1442,    72,   739,  1994,
    1280,   742,   304,    72,    72,   152,   160,   626,   177,   158,
    1290,   630,   631,  1452,   160,    72,   176,   350,  1756,   633,
    1758,   447,  1960,   177,   152,  1963,   767,  2022,   177,   152,
     225,   160,   523,   156,   152,   230,   231,    13,    14,    15,
      16,    17,   106,   107,   663,   160,    72,   473,    72,   475,
     476,   154,   167,   537,    72,   158,   251,   676,   146,   147,
     148,   545,  2057,   156,   490,    72,   174,   158,   161,   152,
     273,   812,   539,   814,   134,    72,    72,   152,   562,   240,
     168,   156,  1208,   152,   152,   826,   177,   156,   156,   573,
    1929,   155,   833,   152,   616,   152,    72,   523,   839,   156,
     878,   842,  1941,   163,   164,   152,  1545,   268,   151,   156,
     851,   306,   731,   154,   733,   158,   735,   158,   279,  1111,
     739,   612,   548,   742,   319,   320,   152,   553,   152,   555,
     156,   962,   156,   991,   152,   239,   877,   152,   156,    72,
     473,   118,   246,   304,   159,   152,   613,   158,   767,   156,
     576,   991,   578,   579,   768,   152,   152,   156,   134,   156,
     156,  1532,   161,   267,   590,   154,   177,   447,   154,  1315,
    2009,   912,   913,   914,   278,  1261,  1262,   603,  1620,  1119,
     154,   940,   914,  1625,   158,   914,   612,  1099,    13,    14,
      15,    16,    17,   106,   107,   809,    13,    14,    15,    16,
      17,    13,    14,    15,    16,    17,   154,   826,   154,   635,
     694,   637,   638,   571,   833,   925,   700,   831,   832,   152,
     154,   962,   154,   156,   153,   709,    13,    14,    15,    16,
      17,   160,   851,   659,   660,   976,   850,   154,   152,  1519,
     666,   151,   159,   523,   728,  1149,   152,    72,   158,  1695,
    1081,  1082,     3,   158,   938,    72,    22,   590,   616,   158,
      72,   154,    13,    14,    15,    16,    17,   152,   914,   571,
     603,  1012,   165,   166,    47,    48,   471,    50,   152,  1020,
     155,   156,    55,   616,   159,    72,  1208,   158,   146,   147,
     148,  1276,  1418,   912,   913,   914,   910,   154,   132,   163,
    1250,    99,   159,  1449,   152,   919,   170,   171,  1049,   134,
     168,  1052,   154,   155,   616,   132,   511,   784,   152,   177,
     152,    72,   156,   152,   156,   154,   659,   156,   523,   163,
     164,   633,   799,   154,  1075,   152,   803,   152,   159,   156,
    1081,  1082,   176,   962,   154,   132,   163,   164,   154,   159,
    1257,  1258,   158,   548,   154,   160,    84,    89,   158,   554,
     146,   147,   148,   160,   152,   152,   154,   614,   156,   156,
     151,  1112,   158,   177,     3,   154,   163,   164,  1524,   158,
    1302,   157,   168,  1829,    13,    14,    15,    16,    17,   154,
     160,   177,   914,   158,   132,   154,   825,  1543,   160,   132,
     884,  1287,   152,   925,   154,  1176,   156,   836,  1400,   167,
     571,   154,   896,   616,   152,   610,   900,   612,   156,   152,
     904,   176,   150,   156,   158,   163,   164,  1267,   152,   152,
     163,   164,   156,   156,   538,  1521,   746,   747,   748,  1297,
    1298,  1721,  1722,    72,   154,   132,  1187,   118,   158,  1190,
    1191,  1192,   152,  1185,   152,   616,  1185,  1167,  1298,   563,
     154,   152,   190,   152,   158,   152,   570,   156,  1366,   156,
     574,   152,   152,   154,  1215,   156,   163,   164,   154,   169,
    1221,  1956,   158,  1215,   162,   154,  1215,  1373,  1374,   158,
     154,   154,   164,  1234,   158,   158,  1237,  1238,  1237,  1238,
    1241,   121,   154,   123,   124,   125,   158,   809,   174,  1250,
    1124,   132,  1271,   154,   940,   146,   147,   148,    13,    14,
      15,    16,    17,   165,   166,   951,   154,   158,  1238,   154,
     158,   259,   152,   158,   960,   155,   156,   168,   154,  1185,
     160,   161,   158,   155,  1285,   154,   177,    58,  1994,   158,
      61,    62,   154,    64,   154,   104,   914,   154,  1299,   108,
     109,   110,   111,   112,   113,   114,   115,   154,  1187,  1215,
     154,  1190,  1191,  1192,  1315,   154,  2022,    72,   156,   158,
     157,   914,  1323,  1267,   831,   832,    13,    14,    15,    16,
      17,    18,  1238,   134,  1441,  1875,  1215,  1064,   126,   127,
     328,  1068,  1221,   850,   154,   134,   154,   940,   158,   154,
     158,  2057,   914,   158,  1355,  1234,  1045,   812,  1085,   814,
     348,   159,  1241,   130,   131,  1092,    13,    14,    15,    16,
      17,    18,  1058,   159,  1424,   154,   154,   132,   158,   158,
     158,   176,  1413,  1414,  1415,  1167,  1504,  1418,  1419,  1735,
      13,    14,    15,    16,    17,    18,  1082,   152,   154,   157,
     158,   156,   158,  1185,  1504,  1631,  1285,  1424,   163,   164,
    1137,   152,   919,   152,  1141,  1617,  1618,  1619,  1145,   154,
    1299,   157,   877,  1424,   157,   158,  1170,  1428,  1429,   163,
     164,  1971,  1121,  1215,  1631,   111,   112,   113,   114,   115,
    1184,   157,   158,  1631,   157,   158,  1416,  1631,  1449,   437,
     154,   914,  1441,  1442,   157,   158,  1238,  1201,   753,   754,
     755,   756,   925,   154,  1208,  1173,  1174,  1175,   154,    78,
     154,  1472,  1473,  1638,  1639,  1640,  1631,   157,   158,   467,
     154,  1482,   154,  1482,   991,    13,    14,    15,    16,    17,
      18,  1365,  1366,   914,   154,   104,   154,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,  1631,  1631,   158,
     159,   176,  1631,   157,   158,   156,  1631,    91,    92,   157,
     158,   157,   158,  1524,   157,   158,   157,   158,   160,  1631,
     157,   158,   157,   158,   160,  1441,  1442,   901,   157,   158,
     157,   158,  1543,   157,   158,   157,   158,  1548,   154,  1428,
    1429,  1237,  1553,  1554,  1555,  1713,   157,   158,   157,   158,
    1530,  1631,   550,   551,   158,   159,  1748,  1185,   177,   160,
    1620,   160,  1798,   157,   158,  1625,   157,   158,    77,    78,
     158,   159,    70,  1633,   160,  1271,  1324,  1325,  1695,   749,
     750,  1277,  1185,  1472,  1473,  1713,   158,  1215,  1287,   751,
     752,  1798,   177,  1620,  2039,  2040,   757,   758,  1625,  1767,
    1798,  1639,  1640,  1713,  1798,   157,  1633,  1124,   152,  1620,
    1075,    78,  1215,  1185,  1625,   157,  1081,  1082,  1479,  1480,
    1631,    18,  1633,   176,  1416,  1522,  1523,  1257,  1258,   158,
    1641,   152,   160,  1798,  1237,   154,   177,  1336,   154,   157,
     160,  1652,  1862,  1215,   177,  1693,   160,   157,  1385,  1441,
    1442,    18,  1389,   154,  1665,   151,  1393,   154,   656,    65,
      66,    67,    68,  1424,  1798,  1798,   154,   154,  1271,  1798,
     154,   154,   154,  1798,  1373,  1374,   154,   154,  1432,  1433,
    1660,    72,    22,   681,   154,   154,  1798,  1237,  1238,   154,
     151,   151,   160,    70,  1167,   176,  1695,   160,   104,  1710,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     160,   160,  1185,   104,   154,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   154,   154,  1481,   154,  1425,
     151,   176,   154,   160,   154,   160,  1716,   154,  1530,   158,
     158,  1801,  1215,   154,  1660,  1756,   158,  1758,   157,   154,
     156,   154,  1641,   154,  1185,   154,   154,   157,   154,  1915,
     154,   152,   153,  1652,  1237,  1238,   154,   154,   174,   154,
     154,   154,  1509,   154,  1801,   154,  1665,   157,   154,  1695,
     154,  1298,   154,   154,  1215,  1250,  1482,  1798,   154,   151,
    1801,   154,   151,   158,   792,   152,   152,  1845,   152,  1810,
     152,   152,   152,  1814,    14,  2025,   159,    74,   177,   158,
    1978,   159,   157,  1441,  1442,   157,   177,  1828,   151,   177,
     151,  1710,   160,   160,   158,   154,  1563,  1838,   154,   154,
    1829,     1,   154,  1570,     4,   154,   177,  1574,  1441,  1442,
    1851,   157,  1853,  1854,  1855,  1856,   154,   158,  1365,  2085,
    1861,  1862,   158,  1913,   154,   158,   157,   154,   154,  1620,
     154,   154,   157,   151,  1625,   151,    80,   152,  1660,  1441,
    1442,   152,  1633,   177,    92,   177,   152,   151,  2085,  1482,
     152,   152,  1766,  1767,    90,   177,  1913,  2085,   675,    59,
    1355,  2085,   154,   177,   177,  2051,   151,  2053,   177,   177,
     177,   151,  1913,  1695,  1952,    75,   158,  1948,  1919,   158,
     151,   157,  1923,  1829,    84,   160,   160,  1928,   157,   157,
    2085,  1810,   157,   151,  1716,  1814,   159,    97,   926,   154,
     100,  1685,  1482,   159,   104,   121,  2092,   154,   151,  1828,
     151,   157,  1953,  1416,   159,   157,   154,   154,   151,  1838,
     154,  2085,  2085,   154,   177,   152,  2085,   152,   154,  1424,
    2085,   152,  1851,  1659,  1853,  1854,  1855,  1856,  1441,  1442,
     158,   141,  1861,  2085,   151,   157,   154,   147,   157,   160,
     150,   157,  2023,   153,   154,   151,  1997,  1504,   151,   154,
    2001,   151,   154,   154,  1948,  1994,   166,  2026,   154,   157,
      75,  2085,    75,  2014,   177,  2085,   177,  2085,   151,  1482,
    1441,  1442,   152,  2085,  2025,   154,  2027,  2173,   177,   154,
     190,   191,   192,  2022,   152,   157,  2086,   157,   151,   160,
    1919,   151,   202,   203,  1923,   151,   206,  1829,   151,  1928,
    1801,   156,  2090,   154,   216,   391,  2094,  2095,   154,  2060,
     837,   154,   154,    75,   168,   225,  1659,  1530,  2057,  2086,
     230,   231,   155,   168,  1953,   177,    75,  1695,  2128,  2023,
     416,   417,   159,   177,  2085,  2086,   177,  2086,  1994,   151,
     151,   251,  2130,   151,   158,  2096,   153,   168,  2099,   259,
    2101,   437,  1695,   151,  1978,   159,   104,  2108,   168,   152,
      75,  2128,   151,   273,   158,  1801,  2022,   166,  1997,  2157,
     153,   168,  2001,  2161,   177,   157,   168,  2128,    75,  2128,
    1816,   467,   177,  1695,    75,  2014,  2137,   151,  2139,  2177,
     159,   151,  2143,   154,  2145,   153,   306,   154,  2027,   154,
     151,  2057,   312,   177,   203,   152,   177,   203,   318,   319,
     320,   154,  1913,   154,  1748,  1620,  2167,  1330,   328,   718,
    1625,   177,   759,   761,   760,  1203,  1631,  2178,  1633,   762,
     436,  2060,  1695,   763,  1215,  2145,  2187,  2053,   348,   349,
     350,  1837,   969,  1829,  1704,  1707,  1659,  1660,    62,  2082,
    1686,  2125,  1686,  2023,   364,  2095,  2161,  2022,   368,  1241,
      49,  1829,  1994,   112,   264,  1913,  1984,  2096,  1904,  1419,
    2099,   966,  2101,  1816,   839,  1234,  1915,   929,   241,  2108,
     500,  1716,  1695,     0,   626,  1609,  1829,   784,   976,   784,
    2022,   105,    -1,   403,   784,   109,    -1,    -1,   112,    -1,
     114,    -1,    -1,  1716,    -1,    -1,    -1,    -1,  2137,  1766,
    2139,    -1,    -1,    -1,  2143,    -1,  2145,  1829,  1256,    -1,
      -1,    -1,    -1,    -1,  1695,  2057,    -1,   437,    -1,  1267,
     440,    -1,    -1,    -1,    -1,    -1,    -1,   447,  2167,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,  2178,
      -1,    -1,   348,   463,    -1,   351,    -1,   467,  2187,    -1,
      -1,   471,    -1,   473,    -1,    18,    -1,    -1,   364,  2046,
      -1,    -1,   368,    -1,    -1,    -1,    -1,    -1,    -1,   491,
      -1,   493,    -1,    -1,    -1,  2086,    -1,    -1,    -1,    -1,
    2026,    -1,    72,  1798,    -1,    -1,  1801,    -1,    -1,    -1,
      65,   511,    -1,  1816,    57,    58,    59,    60,    61,    62,
      63,    64,  2051,   523,  2053,    -1,  1829,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,  1994,  2128,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,   548,    -1,
     550,   551,    -1,    -1,   554,    -1,   556,    -1,   262,    -1,
      -1,  1994,   132,  2092,  2022,    -1,    -1,  1862,  1829,   745,
      -1,    -1,  2156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   467,   152,   153,    -1,  2169,    -1,   440,    -1,  2022,
      -1,    -1,  1994,   163,   164,    -1,    -1,  2126,    -1,  2057,
      -1,    -1,  2128,   456,    -1,   309,   459,    -1,    -1,    -1,
     610,    -1,   612,    -1,   614,    -1,   616,    -1,  1913,    -1,
    2022,    -1,    -1,    -1,  2057,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   632,   633,    -1,   635,    -1,    -1,    -1,    -1,
      -1,    -1,  1259,   643,  2173,    -1,    -1,   647,   352,    -1,
     354,    -1,   356,    -1,    -1,  2057,   656,    -1,    -1,    -1,
      -1,    -1,    -1,   516,    -1,    -1,   666,   556,    -1,   669,
     556,    -1,    -1,    -1,  1291,    -1,    -1,    -1,    13,     1,
      -1,   681,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     394,  1994,    -1,    -1,   694,    -1,    -1,   697,   698,    -1,
     700,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   709,
      -1,    -1,   712,   713,   714,    -1,    -1,    -1,    -1,  2022,
      -1,    -1,    -1,  1340,    -1,   614,    -1,    -1,   614,    -1,
    2025,    -1,    -1,  1994,    -1,    -1,    -1,    59,    -1,    -1,
      -1,    -1,    -1,    -1,   633,    -1,   632,   633,    -1,    -1,
      -1,    -1,    -1,    88,  2057,    -1,    -1,    -1,    -1,    -1,
      -1,  2022,    84,    -1,    -1,    -1,    -1,    -1,   768,   104,
     656,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   104,   669,   784,   785,   490,    -1,    -1,    -1,
    2085,  2086,   792,    -1,    -1,    -1,  2057,   104,    -1,  1627,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   809,
      -1,   987,   812,    -1,   814,   137,   992,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,   147,    -1,  1003,   150,    -1,
      -1,   831,   832,  2128,    -1,    -1,    -1,   839,    -1,    -1,
     842,    -1,    -1,    -1,   166,   152,   153,    -1,    -1,   104,
     850,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,   568,    -1,    -1,    -1,   190,   191,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   877,    -1,   768,
     202,   203,   768,   104,  1501,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,  1518,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   231,
    1527,    -1,    -1,    -1,   914,    -1,    -1,    -1,   240,   919,
     809,    -1,   177,   809,    -1,   925,   926,  1544,    -1,   251,
      -1,   784,   785,    -1,   256,   257,    -1,   259,   938,    62,
     940,   794,   831,   832,   797,    -1,   268,    -1,    -1,    -1,
      -1,   951,    -1,    -1,    -1,    -1,   177,   279,    -1,    -1,
     282,   850,    -1,    -1,   286,    48,    -1,    -1,    -1,   291,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
     302,    -1,   304,    -1,    -1,    -1,   308,    -1,   111,   112,
      -1,   991,    75,    -1,    -1,    -1,    -1,    -1,   320,    -1,
      -1,    -1,    -1,    -1,   857,    -1,   328,    -1,    -1,  1185,
    1012,   864,    -1,    -1,    -1,   868,    -1,    -1,  1020,   872,
      -1,   910,    -1,    -1,    -1,    -1,   348,    -1,    -1,   351,
     919,   154,    -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,
      -1,    -1,   364,    -1,    -1,    -1,   368,  1049,    -1,    -1,
    1052,   134,   938,   136,    -1,   941,    -1,    -1,  1058,  1676,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,  1075,    -1,    -1,    -1,    -1,
      -1,   403,  1082,   206,    -1,   168,    -1,    -1,    -1,    -1,
     104,  1267,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   991,    -1,    -1,   991,    -1,    -1,    -1,   192,
    1112,   155,    -1,    -1,    -1,   437,   160,  1734,    -1,    -1,
      -1,    -1,  1739,   167,  1124,    -1,    18,    -1,    -1,  1305,
    1306,  1307,    -1,  1750,    -1,    -1,  1312,  1313,    -1,    -1,
      -1,   463,    -1,    -1,    -1,   467,   160,   230,    -1,    -1,
     273,   234,    -1,   167,   237,   238,    -1,    -1,   241,    -1,
      -1,   244,   245,    -1,   247,    -1,   249,  1167,    -1,    61,
      62,    63,    64,    -1,    -1,    -1,  1176,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   307,  1185,    -1,    -1,    -1,   312,
      -1,    -1,   104,    -1,    -1,   318,   108,   109,   110,   111,
     112,   113,   114,   115,   116,  1058,    -1,    -1,  1208,    -1,
      -1,    -1,   104,    -1,    -1,  1215,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,    -1,   350,   550,   551,
      -1,    -1,    -1,    -1,   556,  1124,   319,  1237,  1238,   322,
      -1,    -1,    -1,    -1,   156,    -1,    -1,  1864,  1865,   571,
    1250,    -1,    -1,    -1,    -1,    -1,  1256,    -1,    -1,    -1,
      -1,    -1,   345,   346,   156,    -1,    -1,  1267,    -1,    -1,
      -1,   394,    -1,    -1,    -1,  1128,    -1,   360,  1131,   104,
      -1,    -1,  1135,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   614,    -1,   616,   120,    -1,   122,  1298,    -1,
      -1,    -1,  1302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     632,   633,    -1,  1315,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1323,    -1,    -1,    -1,   647,    -1,    -1,   153,    -1,
      -1,   156,    -1,    -1,   656,    -1,    -1,    -1,    -1,   661,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   669,    -1,    -1,
     473,    -1,    -1,    -1,    -1,  1972,    -1,    -1,    -1,   681,
      -1,    -1,    -1,    -1,    -1,  1365,  1366,   490,    -1,    -1,
      -1,    -1,   694,   456,    -1,   697,   698,    -1,   700,    -1,
      -1,  1267,    -1,    -1,    -1,    -1,    -1,   709,    -1,    -1,
     712,   713,   714,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,  1298,
      -1,    -1,  1298,  1413,  1414,  1415,  1416,    -1,  1418,  1419,
      -1,    -1,    -1,    -1,  1424,  1425,  2043,    -1,   511,   104,
      -1,    -1,   555,   108,   109,   110,   111,   112,   113,   114,
     115,  1441,  1442,    -1,   527,    -1,   768,  1449,    -1,   160,
      -1,    -1,    -1,   576,    -1,    -1,   167,    -1,    -1,    -1,
      -1,    -1,    -1,   785,    -1,    -1,    -1,   590,    -1,    -1,
     792,    -1,    -1,    -1,    -1,    -1,  1365,  1366,   153,     3,
     603,   156,  1482,    -1,    -1,    -1,    -1,   809,    -1,  1342,
      -1,    -1,   814,    -1,    -1,    -1,    -1,    -1,  1351,    -1,
      -1,    -1,    -1,    -1,  1504,    -1,    -1,    -1,    -1,   831,
     832,    -1,   635,     4,     5,     6,     7,     8,     9,    10,
      11,    12,  1524,    -1,  1700,    -1,    -1,   610,   850,    -1,
    1530,    -1,  1532,    -1,    -1,    -1,   659,    -1,    -1,    -1,
      -1,  1543,    -1,   666,    -1,    -1,  1548,    -1,    -1,    -1,
      -1,  1553,  1554,  1555,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,   651,   652,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
     104,   664,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   914,    -1,    -1,    -1,    -1,   919,    -1,    -1,
      -1,    -1,    -1,   925,   926,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,   163,    -1,  1504,   938,    -1,  1504,   941,
    1620,  1621,    -1,    -1,    -1,  1625,   948,  1627,   152,   153,
      -1,  1631,    -1,  1633,    72,   159,    -1,    -1,    -1,   163,
     164,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,  1659,
    1660,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,   991,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   132,  1695,    -1,   160,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   788,   789,    -1,    -1,    -1,
      -1,   794,    -1,  1713,   152,   153,  1716,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,    -1,    -1,
     152,    -1,   815,   155,   156,   818,   819,    -1,   821,    -1,
     823,   824,     4,    -1,    -1,    -1,    -1,    -1,  1748,    -1,
      -1,    -1,    -1,    -1,  1756,    -1,  1758,    -1,   881,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1766,  1767,  1621,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1955,
      -1,   864,    -1,  1783,  1784,   868,    -1,    -1,    -1,   872,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1798,    -1,
      72,  1801,  1124,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,  1816,   940,    -1,    -1,
      -1,    -1,    84,    -1,  1713,    -1,    -1,    -1,    -1,  1829,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,  1167,    -1,    -1,    -1,    -1,
      -1,    -1,   935,   936,  1176,   156,    -1,    -1,    -1,    -1,
     132,    -1,  1862,  1185,    -1,    -1,   949,     1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,  1766,  1767,   141,
     152,   153,    -1,    -1,   156,    -1,  1208,    -1,   150,    -1,
     104,   163,   164,  1215,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,  1904,    -1,   120,    -1,   122,    -1,
      -1,    -1,    -1,  1913,    -1,    -1,  1238,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,   190,    -1,
    1783,  1784,    -1,    -1,  1256,    -1,    -1,    -1,    -1,   153,
     202,    -1,   156,    -1,    -1,  1267,    -1,    -1,  1948,    -1,
      84,    -1,    -1,   104,  1276,    -1,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   100,    -1,    -1,   120,
     104,   122,    -1,    -1,    -1,    -1,  1298,    -1,  1978,    -1,
    1302,    -1,    -1,    -1,  1984,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1994,   257,    -1,   259,  1081,    -1,
      -1,    -1,   153,   137,    -1,   156,    -1,   141,    -1,    -1,
      -1,    -1,    -1,   147,    -1,    -1,   150,    -1,    -1,    -1,
     154,    -1,  2022,  2023,    -1,  2025,  2026,    -1,    -1,    -1,
      -1,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,  1122,
     302,    72,    -1,  1365,  1366,  1128,    -1,    -1,  1131,    -1,
      -1,  1904,  1135,    -1,    -1,    -1,   190,  2057,    -1,    -1,
      -1,    -1,  1948,    -1,    -1,    -1,   328,    -1,   202,   203,
      -1,    -1,   206,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,  2085,  2086,    -1,    -1,  1978,
      -1,  1413,  1414,  1415,  1416,  1417,  1418,  1419,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,   240,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1237,    -1,    -1,    -1,    -1,  1441,
    1442,   152,   153,   257,    -1,   259,    -1,  1980,  2128,    -1,
      -1,  1984,   163,   164,   268,    -1,    -1,  2023,    -1,   273,
      -1,   403,    -1,    -1,    -1,   279,    -1,   104,  1271,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     294,    -1,    -1,   297,    -1,    -1,    -1,    -1,   302,    -1,
     304,    -1,  2025,   307,   308,   437,    -1,    -1,   312,    -1,
      -1,    -1,  1504,    -1,   318,    -1,    72,    -1,    -1,    -1,
    1273,    -1,    -1,    -1,   328,    -1,    -1,    -1,   155,  1282,
    1283,   463,    -1,    -1,    -1,    -1,    -1,    -1,  1530,    -1,
      -1,    -1,    -1,    -1,   348,    -1,   350,   351,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     364,   104,  2085,  2086,   368,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,    -1,   132,   120,    -1,   122,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,  1342,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,  1351,   403,
      -1,  1354,    -1,  1356,  1357,  2128,    -1,   163,   164,    -1,
     153,    -1,    -1,  1406,    -1,    72,    -1,    -1,   550,   551,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,  1425,   437,    -1,  1627,    -1,    -1,    -1,    -1,
      -1,  1633,    72,    -1,    -1,    -1,    -1,   104,  1401,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   463,
      -1,    -1,    -1,   467,    -1,    -1,    -1,    -1,  1660,   473,
      -1,   155,    -1,    -1,   104,   132,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,  1482,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   132,  1695,    -1,    -1,   163,   164,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,  1713,   152,   153,  1716,    -1,   156,    -1,    -1,    -1,
    1483,    -1,    -1,   163,   164,   539,    -1,     1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   550,   551,    -1,   681,
      -1,   555,   556,    -1,    -1,    -1,  1748,   153,    -1,    -1,
     156,    -1,   694,    -1,    -1,   697,   698,   571,   700,    -1,
      -1,    -1,    -1,    -1,  1766,  1767,    -1,   709,    -1,    -1,
     712,   713,   714,    -1,    -1,    -1,   590,    -1,    -1,    -1,
      -1,    -1,  1784,    -1,    -1,    59,    -1,    -1,    -1,   603,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   613,
     614,    -1,   616,    -1,    -1,    -1,  1569,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   630,    -1,   632,   633,
      -1,   635,    13,    14,    15,    16,    17,  1829,    -1,   643,
     104,    -1,    -1,    -1,    -1,  1598,    -1,    -1,    -1,    -1,
      -1,    -1,   656,    -1,    -1,   659,    -1,    -1,    -1,   663,
     792,    -1,   666,    -1,    -1,   669,  1659,   671,    -1,    -1,
      -1,    -1,    -1,   137,    -1,    -1,    -1,   681,  1631,    -1,
      -1,    -1,    -1,   147,  1637,    -1,    -1,    -1,    -1,    -1,
     694,    72,    -1,   697,   698,    -1,   700,    -1,    -1,    -1,
      -1,    -1,   166,    -1,    -1,   709,    -1,    -1,   712,   713,
     714,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,   203,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   132,    -1,    -1,   149,    -1,  1948,    -1,    -1,  1712,
      -1,    -1,    -1,    -1,   768,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,   156,   240,    84,    -1,    -1,
     784,   176,   163,   164,    -1,    -1,  1978,    -1,   792,    -1,
      -1,    -1,    -1,   925,   926,   799,    -1,    -1,    -1,   803,
      -1,    -1,  1994,    -1,   268,   809,    -1,    -1,   812,    -1,
      -1,    -1,    -1,    -1,    -1,   279,    -1,    -1,   282,    -1,
      -1,    -1,    -1,  1816,    -1,    -1,    -1,   831,   832,    -1,
    2022,  2023,    -1,   297,    -1,    -1,    -1,  1790,  1791,    -1,
     304,    -1,    -1,   150,   308,  1798,   850,    -1,    -1,    -1,
    1803,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,
      -1,    -1,    -1,    -1,    -1,  2057,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,   881,    -1,    -1,
      -1,    -1,    -1,   190,   348,    -1,    -1,   351,    -1,    -1,
      -1,    -1,    -1,    -1,  2086,    -1,   203,    -1,    -1,    -1,
     364,    -1,    -1,    -1,   368,    -1,   910,    -1,    -1,    -1,
     914,    -1,    -1,    -1,    -1,   919,    -1,    -1,    -1,    -1,
      -1,   925,   926,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   938,    -1,   940,   941,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,   259,  1906,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,   132,   152,   991,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,   467,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,   152,   153,
      -1,   328,   156,    -1,    -1,    -1,    -1,  1980,    -1,   163,
     164,    -1,    -1,  2026,    -1,  1167,    -1,    -1,    -1,    -1,
      -1,   348,   176,   104,  1176,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
    1064,    -1,    -1,    -1,  1068,    -1,    -1,    -1,    -1,    -1,
      -1,  1075,    -1,    -1,    -1,   539,  1208,    -1,    -1,    -1,
      -1,  1085,    -1,    -1,    -1,    -1,    -1,    -1,  1092,    -1,
      -1,   152,   556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,   104,   571,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
    1124,    -1,    -1,    -1,  1256,    -1,    -1,    -1,    -1,    -1,
     437,    -1,  2085,  1137,   132,    -1,    -1,  1141,    -1,    -1,
      -1,  1145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   613,
     614,    -1,   616,    -1,   152,   153,     1,    -1,    -1,    72,
     467,   159,    -1,  1167,    -1,   163,   164,    -1,   632,   633,
    1302,    -1,  1176,    -1,    -1,    -1,    -1,    -1,    -1,   643,
      -1,  1185,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   656,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,  1208,   669,    -1,    -1,    -1,    -1,
      -1,  1215,    -1,    -1,    59,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1237,  1238,    -1,    -1,    -1,    -1,   152,
     153,    -1,    -1,   550,   551,    -1,    -1,    -1,    -1,   556,
     163,   164,  1256,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,  1267,    -1,    -1,    -1,  1271,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
    1284,  1413,  1414,  1415,  1416,  1417,  1418,  1419,    -1,    -1,
      -1,    -1,   137,  1297,  1298,    -1,   132,    -1,  1302,    -1,
      -1,    -1,   147,    -1,   768,    -1,    -1,   614,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,
     784,   166,    -1,    -1,    -1,    -1,   633,   163,   164,    -1,
      -1,    -1,    -1,    -1,    -1,   799,    -1,    -1,    -1,   803,
      -1,    -1,    -1,    -1,    -1,   809,    -1,    -1,   812,   656,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   203,    -1,
      -1,  1365,  1366,    -1,    -1,    -1,    -1,   831,   832,    -1,
      -1,    -1,    -1,    -1,   681,    -1,    -1,    -1,    -1,    -1,
      -1,  1385,    -1,    -1,    -1,  1389,   850,    -1,    -1,  1393,
      -1,    -1,    -1,    -1,    -1,   240,    -1,    -1,  1530,    -1,
      -1,    -1,  1406,    -1,    -1,    -1,    -1,    -1,    -1,  1413,
    1414,  1415,  1416,  1417,  1418,  1419,    -1,    -1,    -1,    -1,
      -1,  1425,    -1,   268,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   279,    -1,    -1,  1441,  1442,    13,
      14,    15,    16,    17,    -1,    -1,   910,    -1,    -1,    -1,
     914,    -1,   297,    -1,    -1,   919,    -1,    -1,    -1,   304,
      -1,   768,   104,   308,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   938,    -1,    -1,   941,  1482,    -1,
      -1,    -1,    -1,    -1,    -1,   792,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,     1,  1627,    -1,     4,    72,    -1,
    1504,    -1,   809,   348,    -1,  1509,   351,    -1,    -1,    -1,
     152,   153,    -1,    -1,   156,    -1,    -1,    -1,    -1,   364,
      -1,   163,   164,   368,   831,   832,  1530,   991,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   850,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,   132,  1563,
      -1,    -1,    -1,    -1,    -1,    -1,  1570,    -1,    -1,    -1,
    1574,    -1,    -1,    -1,    -1,    -1,    -1,    84,   152,   153,
      -1,   403,    -1,    -1,  1716,    -1,    -1,    -1,    -1,   163,
     164,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
    1064,    -1,    -1,    -1,  1068,   112,    -1,    -1,    -1,    -1,
      -1,  1075,   919,    -1,    -1,    -1,  1748,    -1,    -1,   926,
      -1,  1085,   467,  1627,    -1,    -1,    -1,    -1,  1092,    -1,
     137,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
     147,    -1,    -1,   150,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1659,  1660,    -1,    -1,   104,
    1124,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,  1137,    -1,    -1,    -1,  1141,    -1,    -1,
      -1,  1145,    -1,   190,   991,    -1,    -1,   132,    -1,    -1,
      -1,  1695,    -1,    -1,   539,   202,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,  1713,
      -1,   556,  1716,    -1,    -1,    -1,    -1,    -1,   163,   164,
      -1,  1185,    -1,    -1,    -1,    -1,   571,    -1,   550,   551,
      -1,    -1,    -1,   240,    -1,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1748,    -1,    -1,    -1,    -1,    -1,
     257,  1215,   259,    -1,    -1,    -1,    -1,   264,    -1,    -1,
      -1,   268,  1766,  1767,    -1,    -1,    -1,    -1,   613,   614,
      -1,   616,   279,    -1,  1238,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   632,   633,    -1,
      -1,    -1,    -1,    -1,    -1,   302,    -1,   304,   643,    -1,
      -1,   308,    -1,  1267,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   656,  1816,    -1,    -1,    -1,    -1,  1124,    -1,    -1,
      -1,   328,    -1,    -1,   669,  1829,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1297,  1298,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,   694,    51,    -1,    53,    -1,    -1,   700,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   709,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,   403,    -1,    -1,    -1,
      -1,  1365,  1366,    -1,    -1,    -1,   728,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1385,    -1,   768,    -1,  1389,    -1,    -1,    -1,  1393,
     437,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   784,
      -1,    -1,   764,    -1,  1948,    -1,    -1,    -1,    -1,  1256,
      -1,    -1,    -1,    -1,   799,    -1,   463,    -1,   803,    -1,
    1267,    -1,    -1,   316,   809,    -1,    -1,   812,    -1,    -1,
      -1,    -1,    -1,    -1,  1978,    -1,    -1,  1441,  1442,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   831,   832,    -1,    -1,
    1994,  1298,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   850,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2022,  2023,
      -1,    -1,  2026,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
    1504,    -1,  2046,   550,   551,  1509,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2057,    -1,    -1,    -1,    -1,  1365,  1366,
      -1,    -1,    -1,    -1,   571,   910,    -1,    -1,    -1,   914,
      -1,    -1,    -1,    -1,   919,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,
      -1,    -1,    -1,   938,    -1,    -1,   941,    -1,    -1,  1563,
      -1,    -1,    -1,    -1,    -1,    -1,  1570,    -1,    -1,   616,
    1574,    -1,    -1,   466,    84,   468,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   477,   478,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   991,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,
      -1,   141,    -1,    -1,   681,    -1,    -1,   147,    -1,    -1,
     150,    -1,    -1,    -1,    -1,    -1,    -1,   694,    -1,    -1,
     697,   698,    -1,   700,    -1,    -1,  1660,  1504,    -1,    -1,
      -1,    -1,   709,    -1,    -1,   712,   713,   714,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1064,
     190,    -1,    -1,  1068,    -1,    -1,    -1,    -1,    -1,    -1,
    1075,  1695,   202,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1085,    -1,    -1,    -1,    -1,    -1,    -1,  1092,    -1,  1713,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     240,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1124,
      -1,    -1,    -1,    -1,    -1,   792,    -1,   257,    -1,   259,
      -1,    -1,  1137,    -1,   264,    -1,  1141,    -1,   268,    -1,
    1145,    -1,  1766,  1767,    -1,    -1,    -1,    -1,    -1,   279,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1627,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   302,    -1,   304,    -1,    -1,    -1,   308,    -1,
    1185,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,    -1,
      -1,    -1,    -1,    -1,    -1,  1829,    -1,    -1,    -1,    -1,
    1215,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1208,    -1,    -1,    -1,
      -1,    -1,    -1,  1238,    -1,    -1,    -1,    -1,   182,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1713,   914,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   925,   926,
      -1,    -1,  1267,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   403,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1297,  1298,    -1,    -1,    -1,    -1,    -1,  1766,
    1767,    -1,    -1,    -1,    -1,    -1,    -1,   437,    -1,    -1,
      -1,    -1,   825,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1948,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   463,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1978,    -1,    -1,    -1,    -1,    -1,
    1365,  1366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1994,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1385,    -1,    -1,    -1,  1389,    -1,    -1,    -1,  1393,    -1,
      -1,     1,    -1,    -1,     4,    -1,    -1,    -1,  2022,  2023,
      -1,    -1,    -1,    -1,   917,   918,    -1,    -1,   921,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     550,   551,  2046,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2057,    -1,    -1,  1441,  1442,    -1,    -1,
      -1,   571,    -1,   387,    -1,    -1,    -1,   391,   392,    59,
      -1,    -1,    -1,    -1,    -1,    -1,  1438,   401,   402,  1441,
    1442,    -1,    -1,    -1,    -1,  1447,    -1,    -1,    -1,  1451,
      -1,  1453,   416,   417,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   616,    -1,    -1,    -1,
      -1,    -1,  1005,   437,   104,    -1,    -1,    -1,    -1,  1504,
    1167,    -1,    -1,    -1,  1509,    -1,    -1,    -1,    -1,  1176,
      -1,  1978,    -1,    -1,    -1,    -1,    -1,    -1,  1185,    -1,
      -1,    -1,    -1,   467,    -1,    -1,    -1,   137,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,   147,  1051,    -1,
     150,  1208,    -1,    -1,    -1,    -1,    -1,    -1,  1215,    -1,
      -1,   681,    -1,    -1,    -1,    -1,    -1,    -1,  1563,    -1,
      -1,    -1,    -1,    -1,   694,  1570,    -1,   697,   698,  1574,
     700,  1238,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   709,
     190,    -1,   712,   713,   714,  1098,    -1,    -1,    -1,  1256,
      -1,    -1,   202,    -1,  1107,  1108,  1109,  1110,    -1,    -1,
      -1,    -1,  1115,  1116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1607,    -1,    -1,    -1,    -1,
     240,    -1,    -1,    -1,    -1,  1302,    -1,    -1,    -1,    -1,
    1153,    -1,    -1,  1156,    -1,    -1,    -1,   257,    -1,   259,
      -1,    -1,    -1,    -1,   264,  1660,    -1,    -1,   268,    -1,
      -1,    -1,   792,    -1,    -1,    -1,    -1,    -1,    -1,   279,
      -1,    -1,    -1,  1655,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1670,  1671,
    1695,    -1,   302,    -1,   304,    -1,    -1,    -1,   308,    -1,
      -1,    -1,  1215,    -1,    -1,    -1,    -1,    -1,  1713,    -1,
      -1,    -1,    -1,  1695,    -1,    -1,    -1,    -1,   328,  1701,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1244,    -1,    -1,    -1,    -1,    -1,    -1,  1251,    -1,
    1253,  1254,    -1,    -1,    -1,    -1,  1413,  1414,  1415,  1416,
    1417,  1418,  1419,  1266,    -1,  1268,    -1,  1270,    -1,    -1,
      -1,  1766,  1767,    -1,  1277,     0,    -1,    -1,     3,    -1,
      -1,    -1,    -1,    -1,  1441,  1442,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   914,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   403,    -1,   925,   926,    -1,    -1,    -1,
      -1,   745,   746,   747,   748,   749,   750,   751,   752,   753,
     754,   755,   756,   757,   758,   759,   760,   761,   762,   763,
      -1,    -1,    -1,    -1,  1829,    -1,    -1,   437,    -1,    -1,
    1812,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1821,
      -1,  1823,    -1,    78,  1826,  1827,    -1,  1829,    -1,    -1,
      -1,    -1,  1834,   463,    -1,    -1,    -1,    -1,    -1,  1372,
      -1,    -1,    -1,  1530,    -1,    -1,  1379,    -1,    -1,    -1,
    1383,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   825,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,  1409,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     550,   551,    -1,  1948,    -1,    -1,    -1,    -1,    -1,    72,
    1463,    -1,  1934,    -1,    -1,    -1,    -1,  1939,  1940,    -1,
    1627,   571,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1978,    -1,    -1,    -1,  1959,    -1,    -1,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,  1994,
      -1,    -1,    -1,  1660,    -1,  1508,    -1,    -1,    -1,    -1,
      -1,    -1,  1515,    -1,  1517,    -1,   616,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2022,  2023,    -1,
    2002,    -1,  2004,   258,    -1,  2007,  2008,    -1,  1695,    -1,
    2012,  2013,   155,    -1,    -1,    -1,    -1,  1167,    -1,    -1,
      -1,  2046,    -1,   987,    -1,    -1,  1176,    -1,   992,  1716,
      -1,    -1,  2057,    -1,    -1,  1185,    -1,    -1,    -1,  1003,
      -1,    -1,    -1,   298,    -1,    -1,    -1,    -1,    -1,    -1,
    1583,   681,    -1,    -1,    -1,   310,    -1,    -1,  1208,  1592,
      -1,  1748,    -1,    -1,   694,  1215,    -1,   697,   698,    -1,
     700,    -1,   327,    -1,    -1,    -1,  2078,  2079,  2080,   709,
      -1,  1045,   712,   713,   714,    -1,    -1,    -1,  1238,    -1,
      -1,    -1,   347,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1256,    -1,    -1,    -1,
      -1,    -1,  2114,  2115,  2116,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1829,    -1,    -1,    -1,    -1,    -1,    -1,   404,
      -1,    92,  1302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   792,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,
     435,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1728,  1729,    -1,    -1,    -1,
      -1,   142,    -1,    -1,   145,    -1,   461,    -1,    -1,  1742,
     465,   202,   203,    -1,    -1,    -1,    -1,    -1,    -1,   160,
      -1,  1185,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   484,
      -1,    -1,    -1,   488,   489,    -1,    -1,   492,    -1,    -1,
      -1,    -1,    -1,   234,    -1,    -1,    -1,    -1,    -1,    -1,
     241,    -1,   507,   508,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1413,  1414,  1415,  1416,  1417,  1418,  1419,
      -1,    -1,    -1,    -1,    -1,   530,    -1,    -1,    -1,    -1,
     221,    -1,    -1,    -1,   914,    -1,    -1,    -1,    -1,    -1,
      -1,  1441,  1442,    -1,    -1,   925,   926,    -1,    -1,    -1,
      -1,    -1,    -1,  1267,    -1,    -1,    -1,  1994,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1287,    -1,    -1,    -1,    -1,   269,   270,
      -1,   322,    -1,    -1,    -1,  2022,    -1,    -1,    -1,   280,
      -1,  1305,  1306,  1307,    -1,    -1,    -1,    -1,  1312,  1313,
      -1,   606,    -1,    -1,   295,    -1,    -1,   348,   349,    -1,
      -1,    -1,  1895,    -1,   619,    -1,    -1,    -1,    -1,    -1,
    2057,    -1,  1336,    -1,    -1,    -1,    -1,   368,    -1,    -1,
    1530,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   329,   644,
      -1,    -1,    -1,    -1,    -1,   336,   337,    -1,    -1,    -1,
     341,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1373,
    1374,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   379,    -1,
      -1,   382,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   440,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     715,    -1,    -1,    -1,    -1,   456,   457,    -1,   459,   460,
      -1,    -1,    -1,    -1,    -1,    -1,   467,  1627,    -1,    -1,
     471,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2024,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1660,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      -1,   512,    -1,    -1,    -1,   516,    -1,    -1,    -1,   470,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1167,    -1,    -1,
      -1,   482,   483,    -1,    -1,  1695,  1176,    -1,    -1,    -1,
    2083,    -1,    -1,    -1,    -1,  1185,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   556,  1716,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2109,    -1,  1208,   834,
      -1,    -1,    -1,     1,    -1,  1215,    -1,    -1,   843,    -1,
      -1,  2124,   121,    -1,   849,    -1,    -1,    -1,  1748,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,  1238,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     611,    -1,    -1,   614,    -1,    -1,  1256,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,   894,
      -1,   632,   633,   898,    -1,    -1,    -1,   902,    -1,    -1,
      -1,    -1,   643,    -1,    -1,    73,   647,    -1,    -1,    -1,
      -1,    -1,    -1,   654,    -1,   656,    -1,    -1,   923,    -1,
      -1,    -1,  1302,    -1,    -1,    -1,    -1,    -1,    -1,  1829,
      -1,    -1,    -1,   101,   102,   626,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,   237,   238,
      -1,    -1,   241,    -1,   132,   244,   245,    -1,   247,    -1,
     249,    -1,    -1,    -1,    -1,    -1,   981,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   676,  1700,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   768,    -1,    -1,
      -1,    -1,    -1,  1413,  1414,  1415,  1416,  1417,  1418,  1419,
      -1,    -1,    -1,   784,   785,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   794,   795,    -1,   797,   798,    -1,    -1,
      -1,  1441,  1442,    -1,    -1,    -1,   345,   346,   809,    -1,
      -1,   812,    -1,   814,   815,    -1,    -1,    -1,    -1,    -1,
     821,   360,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
     831,   832,    -1,    -1,  1994,    -1,  1101,    -1,  1103,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,   850,
      -1,    -1,   853,    -1,    -1,    -1,   857,    -1,    -1,    -1,
      -1,    -1,  2022,   864,   865,    -1,    -1,   868,   869,    -1,
      -1,   872,   873,    -1,    -1,   826,    -1,    -1,    -1,   880,
      -1,    -1,   833,  1187,    -1,    -1,    -1,    -1,    -1,    -1,
    1530,    -1,    -1,    -1,    -1,   150,    -1,  2057,  1163,   154,
      -1,    -1,    -1,  1168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   166,    -1,    -1,    -1,    -1,    -1,   456,   919,   920,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   190,    -1,    -1,    -1,    -1,
      -1,  1915,    -1,    -1,    -1,    -1,    -1,    -1,   203,    -1,
     951,   206,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   913,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1955,    -1,    -1,    -1,    -1,    -1,  1627,   527,    -1,
     991,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1265,    -1,    -1,    -1,   259,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   273,    -1,
    1660,    13,    14,    15,    16,    17,    18,  1292,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,  1695,    -1,  1058,    -1,    51,
      -1,    53,    -1,   318,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,   328,  1075,  1076,  1716,  2051,    -1,  2053,
      72,  1082,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   348,    -1,   350,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1748,    -1,
      -1,    -1,   651,   652,   106,   107,    -1,    -1,  2092,    -1,
      -1,    -1,    -1,  1124,    -1,   664,    -1,  1128,  1129,    -1,
    1131,  1132,    -1,    -1,  1135,  1136,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   403,    -1,
      -1,    -1,  2126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,   156,    -1,    -1,    -1,  1472,  1473,
      -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   437,    -1,    -1,    -1,    -1,    -1,    -1,  1829,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2173,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   467,    -1,    -1,    -1,    -1,    -1,   473,    -1,
      -1,    -1,    -1,  1488,    -1,    -1,  1491,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1190,
    1191,  1192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   788,
     789,    -1,    -1,    -1,    -1,   794,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1267,    -1,    -1,    -1,
    1221,    -1,  1273,  1274,    -1,    -1,   815,    -1,    -1,   818,
     819,    -1,   821,    -1,   823,   824,    -1,    -1,    -1,    -1,
    1241,    -1,    -1,    -1,    -1,   550,   551,  1298,    -1,    -1,
      -1,   556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1588,  1589,   864,    -1,    -1,    -1,   868,
      -1,    -1,    -1,   872,  1285,    -1,    -1,    -1,    -1,    -1,
      -1,  1342,  1343,    -1,    -1,    -1,    -1,    -1,  1652,  1614,
    1351,  1352,    -1,  1354,  1994,    -1,    -1,    -1,    -1,   614,
      -1,  1665,    -1,    -1,  1365,  1366,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   633,    -1,
     635,    -1,  2022,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   190,   935,   936,    -1,    -1,
      -1,   656,    -1,    -1,    -1,    -1,  1710,    -1,   203,    -1,
     949,    -1,    -1,    -1,    -1,    -1,    -1,  2057,    -1,    -1,
      -1,   216,    -1,   218,    -1,    -1,   681,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   694,
      -1,    -1,   697,   698,    -1,   700,    -1,    -1,    -1,    -1,
      -1,    -1,  1717,  1718,   709,    -1,    -1,   712,   713,   714,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1428,  1429,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1504,    -1,    -1,  1810,    -1,    -1,    -1,
    1814,    -1,    -1,   768,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   317,    -1,  1828,    -1,    -1,    -1,    -1,    -1,
      -1,  1532,    -1,    -1,  1838,    -1,    -1,   792,    -1,    -1,
      -1,    -1,  1081,    -1,    -1,    -1,    -1,  1851,    -1,  1853,
    1854,  1855,  1856,    -1,   809,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   831,   832,  1843,    -1,
      -1,    -1,    -1,  1122,    -1,    -1,    -1,    -1,    -1,  1128,
      -1,    -1,  1131,    -1,    -1,   850,  1135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1878,    -1,  1919,    -1,    -1,    -1,  1923,
    1621,    -1,    -1,    -1,  1928,    -1,    -1,    -1,  1893,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1637,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1953,
      -1,    -1,  1917,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   919,    -1,    -1,    -1,    -1,    -1,
      -1,   926,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1944,
      -1,    -1,  1947,    -1,    -1,   940,    -1,    -1,    -1,    -1,
    1641,    -1,    -1,  1997,    -1,    -1,    -1,  2001,   493,    -1,
      -1,    -1,    -1,    -1,   499,    -1,    -1,    -1,    -1,   504,
    2014,    -1,  1713,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1273,    -1,   991,    -1,    -1,    -1,
      -1,    -1,    -1,  1282,  1283,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2060,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1766,  1767,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1783,  1784,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2096,    -1,    -1,  2099,    -1,  2101,  1799,    -1,
      -1,    -1,    -1,  1342,  2108,    -1,    -1,    -1,    -1,   604,
      -1,    -1,  1351,    -1,    -1,  1354,    -1,  1356,  1357,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2137,    -1,  2139,    -1,    -1,   633,  2143,
      -1,  2145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   646,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1401,  2167,    13,    14,    15,    16,    17,  1124,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,  1904,    53,    -1,    55,   702,    -1,    -1,
    1861,  1912,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1176,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   726,   727,    -1,    -1,   730,    -1,   732,    -1,    -1,
      -1,    -1,    -1,   738,  1483,   740,   741,    -1,    -1,    -1,
      -1,    -1,    -1,  1208,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   768,    -1,    -1,    -1,  1978,    -1,  1980,
    1981,    -1,  1237,  1984,    -1,    -1,   781,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   792,    -1,    -1,
      -1,  1256,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   806,  1267,    -1,   809,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2025,    -1,    -1,    -1,    -1,    -1,
    1569,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   836,    -1,  1298,   839,    -1,    -1,  1302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   852,    -1,  1598,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2085,  2086,   881,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1365,  1366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2128,    -1,    -1,
      -1,   926,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   940,   941,    -1,    -1,    -1,
      -1,    -1,    -1,   948,    -1,    -1,    -1,    -1,  1413,  1414,
    1415,    -1,    -1,  1418,  1419,    -1,    -1,    -1,    -1,    -1,
    1425,    -1,    -1,  1712,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   977,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,   991,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   999,    -1,    -1,    18,    -1,    -1,
      -1,  1006,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1482,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,  1504,
      -1,  1790,  1791,    -1,    -1,    -1,    -1,  1052,    -1,    -1,
      71,    -1,    73,    74,  1803,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    -1,    99,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,  1119,    -1,  1121,     5,  1123,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
     151,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   177,  1906,    -1,    -1,
      -1,    49,  1627,    -1,    52,    -1,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1659,    -1,    -1,  1202,  1203,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,  1980,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,  1713,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,  1271,   155,   156,    -1,
      -1,    -1,  1277,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,  1748,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1298,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1766,  1767,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1315,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1330,    -1,    -1,  1333,    -1,
      -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      20,  1816,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
    1405,  1406,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1431,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,  1449,    -1,    20,  1452,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,  1948,   154,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    72,  1504,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,  1513,  1514,
      18,    -1,    -1,  1978,    -1,    -1,    -1,    -1,    -1,  1524,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,    -1,    -1,    -1,    -1,  1543,    -1,
    1545,    49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2023,    -1,
      -1,  2026,    -1,    71,    -1,    73,    74,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      -1,    99,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,    -1,   124,   125,    -1,     1,
      -1,    -1,  1627,    -1,   132,    -1,    -1,  1632,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,   151,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,   177,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1690,    -1,    -1,    -1,    71,
      -1,    73,    74,    -1,    76,    -1,    -1,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    -1,    99,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,  1746,   124,   125,  1749,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,  1776,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   177,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    71,    72,    73,    74,    -1,    76,
      -1,    -1,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    -1,    99,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,    -1,    -1,   155,   156,
      -1,  1956,    -1,   160,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     177,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    71,
      72,    73,    74,    -1,    76,    -1,    -1,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    -1,    99,    -1,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   177,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,
     147,   148,    -1,    -1,    -1,   152,   153,   154,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     177,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   146,   147,   148,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   177,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,   154,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    77,    78,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,     5,    72,    -1,    -1,    -1,    -1,    77,    78,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,    -1,   163,   164,
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
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    -1,    72,    -1,    -1,    -1,    -1,    77,    78,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,    -1,    -1,    -1,   155,   156,    -1,     3,
      -1,     5,    -1,    -1,   163,   164,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,
      -1,   155,   156,    -1,     3,    -1,     5,    -1,    -1,   163,
     164,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,    -1,    -1,    -1,   155,   156,    -1,     3,
      -1,     5,    -1,    -1,   163,   164,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,
      -1,   155,   156,    -1,     3,    -1,     5,    -1,    -1,   163,
     164,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,    13,    14,    15,    16,    17,   163,   164,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,   154,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
     160,    -1,    -1,   163,   164,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,   154,   155,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,     4,     5,     6,
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
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,   154,   155,   156,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     4,     5,
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
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
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
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    71,    -1,    73,    74,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    -1,    -1,    93,    94,    95,
      96,    97,    -1,    99,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,    18,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    49,   155,
     156,    52,    -1,    54,   160,    56,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,   177,    73,    74,    -1,    76,    -1,    -1,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      -1,    -1,    93,    94,    95,    96,    97,    -1,    99,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   177,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,   155,
      51,    -1,    53,    -1,   160,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,   107,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,   155,    51,    -1,    53,    -1,   160,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,   155,
      51,    52,    53,    54,   160,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    90,
      -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,
      -1,   162,   163,   164,   165,   166,   167,   168,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    -1,   124,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,
     165,   166,   167,   168,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,   154,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,
     163,   164,   165,   166,   167,   168,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    -1,   124,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,   159,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   107,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
     155,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   154,   155,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,   160,    -1,    -1,   163,   164,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,   154,   155,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
     155,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,   155,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    72,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,   106,   107,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,    13,    14,    15,    16,
      17,   163,   164,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,    13,    14,    15,    16,    17,   163,   164,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      72,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,   106,   107,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,   106,   107,    -1,    -1,    -1,
      -1,   163,   164,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,   155,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    49,    -1,    -1,
      52,    -1,    54,    72,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    49,    -1,    -1,    52,    -1,    54,
     132,    56,    -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   146,   147,   148,    -1,    73,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    -1,   124,
     125,    49,    -1,    -1,    52,    -1,    54,   132,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   146,   147,   148,    -1,    73,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,   159,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,    -1,   154,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   179,   401,   402,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    69,    72,    73,
      99,   103,   104,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   119,   132,   152,   153,   155,   156,   163,
     164,   182,   183,   184,   199,   289,   290,   291,   292,   293,
     294,   295,   296,   297,   298,   299,   300,   303,   306,   308,
     309,   310,   311,   312,   313,   314,   315,   316,   318,   320,
     321,   322,   324,   325,   329,   330,   331,   332,   333,   335,
     341,   342,   343,   344,   355,   359,   393,   396,   406,   412,
     414,   420,   424,   429,   430,   431,   432,   433,   434,   435,
     436,   462,   480,   481,   482,   483,     0,   179,   104,   183,
     199,   293,   295,   306,   309,   312,   321,   325,   330,   118,
     152,    58,    61,    62,    64,   152,   152,   418,   419,   420,
     317,   318,   106,   107,   183,   373,   394,   395,   373,   152,
     406,   152,   152,     4,   104,   106,   107,   310,   315,   316,
     152,   199,   419,   424,   430,   431,   432,   434,   435,   436,
     106,   332,   157,   179,   296,   306,   309,   429,   433,   479,
     480,   483,   484,   177,   180,   149,   160,   176,   220,   376,
      89,   158,   413,   373,   180,   158,   158,   177,   106,   107,
     152,   199,   301,   302,   424,   425,   426,   427,   428,   429,
     433,   437,   438,   439,   440,   441,   442,   443,   444,   445,
     451,     3,    47,    48,    50,    55,   323,     3,   156,   199,
     295,   310,   314,   316,   326,   331,   409,   429,   433,   483,
      69,   293,   295,   309,   321,   325,   330,   410,   429,   433,
      65,   315,   315,   310,   316,   304,   315,   316,   323,   342,
     310,   315,   310,   155,   418,   158,   180,   152,   160,   228,
     418,   418,     3,   284,   285,   300,   303,   309,   313,   314,
     156,   306,   309,   481,   373,   373,   406,   176,   309,   152,
     199,   415,   424,   425,   429,   438,   442,   156,   199,   483,
     407,   408,    65,    66,    67,    68,   156,   174,   373,   382,
     384,   388,   390,   391,   331,    57,   154,   156,   199,   305,
     309,   313,   320,   321,   327,   328,   329,   330,   334,   341,
     342,   359,   369,   371,   462,   475,   476,   477,   478,   483,
     484,   106,   107,   160,   167,   183,   331,   451,   420,   152,
     389,   390,   152,   152,   118,   185,   186,    49,    52,    54,
      56,    73,   101,   102,   104,   105,   116,   117,   120,   121,
     122,   124,   125,   152,   156,   162,   165,   166,   167,   168,
     181,   182,   185,   187,   190,   198,   199,   200,   201,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   222,   331,   154,   156,   198,   199,   215,   217,
     306,   331,   374,   375,   392,   479,   484,   309,   430,   431,
     432,   434,   435,   436,   154,   154,   154,   154,   154,   154,
     154,   156,   306,   462,   481,   156,   163,   199,   217,   295,
     296,   305,   307,   309,   321,   328,   330,   364,   365,   368,
     369,   370,   475,   483,   152,   429,   433,   483,   152,   158,
     104,   155,   156,   160,   182,   184,   217,   377,   378,   379,
     380,   381,    22,   377,   152,   373,   228,   152,   158,   415,
     183,   419,   424,   426,   427,   428,   437,   439,   440,   441,
     443,   444,   445,   309,   425,   438,   442,   158,    99,   417,
     156,   418,   459,   462,   417,   418,   418,   413,   284,   152,
     418,   459,   417,   418,   418,   413,   418,   418,   309,   415,
     152,   152,   308,   309,   306,   309,   179,   306,   479,   484,
     333,   160,   413,   284,   373,   373,   376,   295,   314,   411,
     429,   433,   160,   413,   284,   394,   309,   321,   309,   309,
     106,   332,   106,   107,   183,   331,   336,   394,   134,   183,
     309,   366,   367,   370,   371,   372,   151,   179,   228,   300,
     177,   429,   442,   309,   179,   417,   152,   417,   180,   217,
     419,   424,   309,   152,   179,   373,   404,   160,   373,   160,
     373,   134,   163,   164,   387,   154,   158,   373,   391,   154,
     418,   418,   157,   179,   307,   309,   321,   328,   330,   474,
     475,   483,   484,   152,   156,   164,   176,   199,   462,   464,
     465,   466,   467,   468,   469,   486,   199,   334,   483,   309,
     328,   315,   310,   418,   154,   307,   309,   476,   307,   462,
     476,    10,   162,   167,   358,   360,   361,   160,   356,   358,
     382,   176,   382,    13,    88,   104,   106,   107,   182,   421,
     422,   423,   154,   118,   152,   198,   152,   152,   152,   201,
     152,   198,   152,   104,   106,   107,   310,   315,   316,   152,
     198,   198,    19,    21,    85,   156,   165,   166,   202,   203,
     217,   224,   228,   344,   374,   483,   158,   179,   152,   187,
     156,   161,   156,   161,   121,   123,   124,   125,   152,   155,
     156,   160,   161,   201,   201,   169,   163,   170,   171,   165,
     166,   126,   127,   128,   129,   172,   173,   130,   131,   164,
     162,   174,   132,   133,   175,   154,   158,   155,   179,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     176,   219,   220,   221,   152,   199,   455,   456,   457,   458,
     459,   154,   158,   154,   154,   154,   154,   154,   154,   152,
     418,   459,   462,   152,   459,   462,   179,   306,   481,   179,
     180,   180,   152,   164,   199,   424,   446,   447,   448,   449,
     450,   451,   452,   453,   454,   134,   483,   180,   180,   373,
     373,   179,   179,   179,   156,   184,   179,   378,   159,   158,
     485,   377,   155,   156,   159,   381,   153,   217,   223,   152,
     179,   179,   176,   424,   426,   427,   428,   437,   439,   440,
     441,   443,   444,   445,   154,   154,   154,   154,   154,   154,
     154,   154,   154,   154,   425,   438,   442,   418,   152,   176,
     157,   179,   376,   228,   413,   366,   376,   228,   415,   224,
     375,   224,   375,   415,   404,   228,   413,   417,   160,   160,
     413,   284,   404,   228,   413,   338,   339,   337,   160,   154,
     158,   154,   158,    70,   286,   287,   177,   163,   217,   179,
     424,   365,   406,   404,   157,   179,   152,   386,   384,   385,
      78,   319,   183,   160,   167,   183,   451,   307,   462,   476,
     309,   313,   483,   366,   465,   466,   467,   157,   179,    18,
     217,   309,   464,   486,   418,   418,   462,   307,   474,   484,
     309,   183,   418,   307,   476,   331,   158,   485,   373,   360,
     358,   160,   154,   375,   154,   154,   158,   152,   177,   374,
     187,   156,   374,   374,   374,   217,   374,   154,   374,   374,
     374,   179,   154,   165,   166,   203,    18,   311,   154,   158,
     154,   163,   164,   154,   223,   217,   160,   217,   183,   217,
     183,   116,   156,   183,   153,   191,   192,   193,   217,   116,
     156,   183,   344,   217,   191,   183,   201,   204,   204,   204,
     205,   205,   206,   206,   207,   207,   207,   207,   208,   208,
     209,   210,   211,   212,   213,   159,   224,   177,   185,   156,
     183,   217,   160,   217,   366,   456,   457,   458,   309,   455,
     418,   418,   217,   375,   152,   418,   459,   462,   152,   459,
     462,   366,   366,   157,   157,   152,   424,   447,   448,   449,
     452,    18,   309,   446,   450,   152,   418,   468,   486,   418,
     418,   486,   152,   418,   468,   418,   418,   180,   216,   373,
     157,   158,   157,   158,   486,   486,   134,   363,   364,   365,
     363,   373,   179,   215,   216,   217,   416,   485,   377,   379,
     151,   179,   154,   158,   179,   363,   183,   217,   154,   154,
     154,   154,   154,   154,   154,   154,   154,   152,   418,   459,
     462,   152,   418,   459,   462,   152,   418,   459,   462,   415,
     185,    22,   462,   217,   316,   331,   460,   228,   154,   154,
     154,   154,   154,   402,   403,   228,   151,   179,   404,   228,
     413,   403,   228,   160,   160,   160,   345,   134,   370,   371,
     183,   288,   373,    18,    71,    73,    76,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    93,
      94,    95,    96,    97,    99,   106,   107,   119,   152,   179,
     224,   225,   226,   227,   228,   229,   230,   232,   233,   243,
     249,   250,   251,   252,   253,   254,   259,   260,   263,   264,
     265,   266,   267,   273,   274,   275,   289,   309,   313,   373,
     414,    70,   180,   180,   363,   180,   405,   403,   293,   295,
     306,   397,   398,   399,   400,   392,   176,   383,   383,   360,
     160,   418,   418,   307,   476,   156,   163,   199,   217,   331,
     217,   309,   154,   154,   154,   154,     5,   309,   418,   464,
     160,   167,   183,   451,    10,   361,   151,   176,   362,   485,
     160,   360,   160,   154,   422,   191,   154,   158,   179,   158,
     154,   154,   158,   154,   201,   154,   154,   154,   201,    18,
     311,   217,   154,   154,   153,   160,   201,   157,   180,   191,
     157,   157,   116,   120,   122,   184,   194,   195,   196,   154,
     158,   194,   157,   158,   151,   215,   159,   154,   194,   180,
     378,   154,   154,   154,   154,   455,   366,   366,   154,   154,
     452,   154,   154,   154,   154,   152,   424,   451,   446,   450,
     366,   366,   157,   180,   486,   179,   179,   180,   180,   180,
     180,   376,   194,   134,   168,   180,   180,   151,   377,   217,
     418,   153,   217,   363,   180,   152,   418,   459,   462,   152,
     418,   459,   462,   152,   418,   459,   462,   366,   366,   366,
     417,   154,   146,   168,   180,   461,   158,   180,   405,   397,
     403,   228,   405,   345,   345,   345,     3,     5,    10,    73,
     151,   290,   297,   298,   306,   309,   346,   351,   479,   158,
     177,   152,    61,    62,   177,   228,   289,   414,   152,    18,
     226,   152,   152,   177,   373,   177,   373,   163,   373,   160,
     225,   152,   152,   152,   228,   217,   218,   218,    14,   276,
     254,   265,    74,   234,   177,   180,   230,    78,   177,   373,
      91,    92,   258,   262,   110,   133,   257,   109,   132,   261,
     257,   372,   309,   159,   288,   157,   157,   180,   158,   405,
     415,   180,   177,   180,   177,   180,   154,   375,   389,   389,
     485,   360,   358,   358,   179,   180,   180,   180,   217,   152,
     418,   468,   462,   308,     5,   163,   180,   217,   360,   160,
     418,   418,   331,   373,   160,   216,   151,   360,   485,   151,
     179,   154,   305,   183,    78,   188,   189,   374,   201,   201,
     201,   201,   201,   160,   378,   158,   151,   197,   156,   195,
     197,   197,   157,   158,   123,   155,   193,   157,   223,   215,
     177,   157,   485,   152,   418,   459,   462,   154,   154,   154,
     152,   418,   459,   462,   152,   418,   468,   424,   418,   418,
     154,   154,   157,   365,   368,   368,   369,   154,   158,   158,
     154,   180,   216,   216,   157,   157,   180,   180,   154,   366,
     366,   366,   154,   154,   154,   376,   418,   158,   217,   217,
     316,   331,   157,   151,   180,   405,   151,   151,   151,   151,
     306,   306,   344,   352,   479,   306,   351,   152,   340,   177,
     177,   152,   159,   199,   347,   348,   354,   424,   425,   438,
     442,   158,   177,   373,   373,   191,   177,   228,   177,   228,
     224,    80,   154,   224,   235,   289,   291,   294,   300,   309,
     313,   146,   147,   148,   153,   154,   177,   224,   244,   245,
     246,   289,   177,   177,   224,   177,   378,   177,   224,   223,
     224,   111,   112,   113,   114,   115,   268,   270,   271,   177,
      98,   177,    84,   152,   154,   152,   180,   151,   177,   177,
     152,   152,   226,   226,   254,   152,   264,   254,   264,   228,
     418,   177,   154,   179,   151,   151,   179,   158,   158,   151,
     485,   160,   160,   157,   157,   157,   180,   366,   217,   217,
     180,   157,   180,   485,   360,   357,   358,   362,   362,   378,
     485,   151,   397,   463,   464,   154,   159,   154,   158,   159,
     378,   485,   223,   121,   194,   195,   156,   195,   156,   195,
     157,   151,   366,   366,   366,   180,   179,   179,   157,   180,
     154,   418,   154,   154,   154,   224,   461,   151,   151,   340,
     340,   340,   347,   152,   199,   349,   350,   459,   470,   471,
     472,   473,   177,   158,   177,   347,   177,   392,   419,   424,
     217,   309,   151,   158,   177,   353,   354,   353,   353,   373,
     154,   154,   152,   226,   154,   224,   309,   146,   147,   148,
     168,   177,   247,   248,   226,   225,   177,   248,   154,   159,
     224,   153,   224,   225,   246,   177,   485,   154,   154,   154,
     228,   270,   271,   152,   217,   152,   185,   235,   201,   255,
     224,    75,   108,   256,   258,    75,   256,     1,   226,   418,
     398,   179,   179,   151,   360,   360,   157,   154,   180,   180,
     157,   157,   151,   485,   358,   160,   485,   151,   180,   154,
     217,   189,   217,   485,   151,   157,   157,   194,   194,   154,
     154,   154,   157,   158,   134,   365,   134,   157,   157,   217,
     177,   471,   472,   473,   309,   470,   158,   177,   418,   418,
     177,   154,   424,   418,   226,    77,    78,   160,   238,   239,
     240,   154,   224,    75,   226,   224,   153,   224,    75,   177,
     106,   153,   224,   225,   246,   153,   224,   226,   245,   248,
     248,   177,   224,   151,   160,   240,   226,   152,   179,   177,
     185,   154,   159,   154,   154,   158,   159,   154,   226,   152,
     226,   226,   226,   226,   373,   415,   485,   485,   157,   157,
     151,   160,   360,   151,   151,   151,   157,   157,   179,   180,
     154,   154,   154,   470,   418,   348,     1,   216,   236,   237,
     416,     1,   159,     1,   179,   226,   238,    75,   177,   154,
     226,    75,   177,   168,   168,   226,   225,   248,   248,   177,
     106,   224,   168,   168,    75,   153,   224,   153,   224,   225,
     177,     1,   179,   179,   272,   307,   309,   479,   159,   177,
     156,   185,   277,   278,   279,   226,   201,   191,   224,   257,
     257,   151,   151,   360,   485,   368,   152,   418,   459,   462,
     350,   134,     1,   158,   159,   151,   282,   283,   289,   226,
      75,   177,   226,   224,   153,   153,   224,   153,   224,   153,
     224,   225,   153,   224,   153,   224,   226,   168,   168,   168,
     168,   151,   282,   272,   180,   152,   199,   415,   470,   183,
     159,   104,   152,   154,   159,   158,    75,   154,   154,    75,
     253,    75,   253,   485,   151,   366,   216,   236,   239,   241,
     242,   289,   226,   168,   168,   168,   168,   153,   153,   224,
     153,   224,   153,   224,   241,   180,   177,   269,   309,   277,
     157,   216,   177,   277,   279,   226,   226,    75,   226,    75,
     151,   154,   226,   231,   180,   239,   153,   153,   224,   153,
     224,   153,   224,   180,   269,   215,   154,   159,   185,   154,
     154,   159,   226,   226,     1,   226,   151,   231,   151,   154,
     228,   185,   280,   152,   177,   280,   228,   158,   159,   216,
     154,   185,   183,   281,   154,   177,   154,   158,   177,   183
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   178,   179,   180,   181,   181,   181,   181,   181,   182,
     182,   182,   182,   182,   182,   182,   182,   183,   183,   184,
     184,   185,   186,   186,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   188,
     188,   189,   189,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   191,   191,   192,
     192,   193,   193,   194,   194,   195,   195,   195,   195,   195,
     195,   195,   196,   196,   196,   197,   197,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   199,   199,   199,   200,   200,   200,   200,   201,   201,
     201,   201,   201,   201,   201,   201,   201,   202,   202,   202,
     202,   203,   203,   204,   204,   205,   205,   205,   205,   206,
     206,   206,   207,   207,   207,   208,   208,   208,   208,   208,
     209,   209,   209,   210,   210,   211,   211,   212,   212,   213,
     213,   214,   214,   215,   215,   215,   216,   217,   217,   217,
     218,   218,   219,   219,   220,   220,   221,   221,   221,   221,
     221,   221,   221,   221,   221,   221,   221,   222,   222,   223,
     223,   223,   223,   224,   224,   225,   225,   226,   226,   226,
     226,   226,   226,   226,   226,   226,   226,   226,   226,   226,
     226,   227,   227,   228,   228,   229,   229,   230,   230,   230,
     230,   230,   231,   231,   231,   232,   233,   233,   233,   233,
     233,   233,   233,   234,   234,   235,   235,   235,   235,   236,
     236,   236,   237,   237,   238,   238,   238,   238,   238,   239,
     239,   240,   241,   241,   242,   242,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   244,   244,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   246,   246,   246,   247,   247,
     248,   248,   248,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   249,   250,   250,   251,   252,   253,   254,   254,
     255,   255,   256,   257,   257,   258,   258,   259,   259,   259,
     259,   259,   259,   260,   261,   261,   262,   263,   263,   264,
     264,   265,   265,   265,   265,   265,   265,   266,   267,   267,
     267,   268,   268,   269,   269,   270,   270,   270,   270,   271,
     272,   272,   272,   272,   272,   273,   274,   274,   275,   275,
     275,   275,   275,   276,   276,   277,   277,   278,   278,   279,
     279,   280,   280,   280,   281,   281,   282,   282,   283,   283,
     284,   284,   285,   285,   286,   286,   287,   287,   288,   288,
     289,   289,   289,   290,   290,   291,   291,   291,   291,   291,
     292,   292,   292,   293,   293,   293,   294,   294,   294,   294,
     294,   295,   295,   296,   296,   297,   297,   297,   298,   298,
     298,   298,   298,   299,   299,   300,   300,   300,   300,   301,
     301,   301,   301,   301,   302,   302,   303,   303,   303,   303,
     304,   304,   304,   305,   305,   305,   306,   306,   306,   307,
     307,   307,   308,   308,   309,   309,   310,   310,   311,   311,
     311,   311,   311,   312,   313,   313,   313,   314,   314,   315,
     315,   315,   315,   315,   315,   315,   315,   315,   316,   316,
     316,   316,   316,   316,   316,   316,   316,   316,   316,   316,
     316,   316,   316,   316,   316,   316,   316,   316,   316,   316,
     316,   316,   316,   316,   316,   316,   317,   317,   318,   319,
     319,   320,   320,   320,   320,   320,   321,   321,   322,   322,
     322,   322,   323,   323,   323,   323,   323,   323,   324,   324,
     324,   324,   325,   326,   325,   325,   327,   327,   327,   327,
     328,   328,   328,   329,   329,   329,   329,   330,   330,   330,
     331,   331,   331,   331,   331,   331,   332,   332,   332,   333,
     333,   334,   334,   336,   335,   337,   335,   338,   335,   339,
     335,   335,   340,   340,   341,   341,   342,   342,   343,   343,
     343,   344,   344,   344,   344,   344,   344,   344,   344,   345,
     345,   346,   346,   346,   346,   346,   346,   346,   346,   346,
     346,   346,   346,   347,   347,   347,   348,   348,   348,   348,
     349,   349,   349,   350,   351,   351,   352,   352,   353,   353,
     354,   355,   355,   356,   355,   355,   355,   355,   355,   355,
     357,   355,   355,   355,   355,   355,   358,   358,   359,   359,
     360,   360,   360,   360,   361,   361,   362,   362,   362,   363,
     363,   363,   363,   363,   363,   363,   364,   364,   364,   364,
     365,   365,   366,   366,   366,   366,   367,   367,   367,   367,
     368,   368,   368,   368,   368,   369,   369,   369,   369,   369,
     370,   370,   371,   371,   372,   372,   373,   373,   373,   374,
     374,   374,   375,   375,   376,   376,   376,   376,   377,   377,
     378,   378,   378,   378,   378,   379,   379,   380,   380,   381,
     381,   381,   381,   381,   382,   382,   383,   383,   385,   384,
     386,   384,   384,   384,   387,   387,   387,   387,   388,   388,
     388,   388,   389,   389,   390,   390,   391,   391,   392,   392,
     392,   392,   393,   393,   393,   394,   394,   395,   395,   396,
     396,   396,   396,   397,   397,   398,   398,   399,   399,   399,
     400,   400,   401,   401,   402,   402,   403,   403,   404,   405,
     406,   406,   406,   406,   406,   406,   406,   406,   406,   406,
     406,   407,   406,   408,   406,   409,   406,   410,   406,   411,
     406,   412,   412,   412,   413,   413,   414,   414,   414,   414,
     414,   414,   414,   414,   414,   414,   415,   415,   415,   415,
     416,   417,   417,   418,   418,   419,   419,   420,   421,   421,
     422,   422,   422,   423,   423,   423,   423,   423,   423,   424,
     424,   425,   425,   425,   425,   426,   426,   426,   426,   427,
     427,   427,   427,   427,   427,   427,   428,   428,   428,   428,
     429,   429,   429,   430,   430,   430,   430,   430,   431,   431,
     431,   431,   432,   432,   432,   432,   432,   432,   433,   433,
     433,   434,   434,   434,   434,   434,   435,   435,   435,   435,
     436,   436,   436,   436,   436,   436,   437,   437,   438,   438,
     438,   438,   439,   439,   439,   439,   440,   440,   440,   440,
     440,   440,   440,   441,   441,   441,   441,   442,   442,   442,
     443,   443,   443,   443,   443,   444,   444,   444,   444,   445,
     445,   445,   445,   445,   445,   446,   446,   446,   446,   446,
     447,   447,   447,   448,   448,   448,   448,   449,   449,   449,
     450,   450,   450,   450,   450,   451,   451,   452,   452,   452,
     453,   453,   454,   454,   455,   455,   455,   456,   456,   456,
     456,   456,   457,   457,   457,   457,   458,   458,   458,   459,
     459,   459,   459,   459,   460,   460,   460,   460,   460,   460,
     461,   461,   462,   462,   462,   462,   463,   463,   464,   464,
     464,   464,   465,   465,   465,   465,   465,   466,   466,   466,
     466,   467,   467,   467,   468,   468,   468,   469,   469,   469,
     469,   469,   469,   470,   470,   470,   471,   471,   471,   471,
     471,   472,   472,   472,   472,   473,   473,   474,   474,   474,
     475,   475,   476,   476,   476,   476,   476,   476,   477,   477,
     477,   477,   477,   477,   477,   477,   477,   477,   478,   478,
     478,   478,   479,   479,   479,   480,   480,   481,   481,   481,
     481,   481,   481,   482,   482,   482,   482,   482,   482,   483,
     483,   483,   484,   484,   485,   485,   486,   486
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
       1,     4,     4,     2,     6,     1,     2,     1,     2,     1,
       2,     1,     1,     2,     2,     2,     3,     5,    10,     7,
       5,    10,     7,     5,     7,     1,     1,     1,     2,     1,
       3,     1,     1,     3,     2,     3,     3,     2,     2,     1,
       2,     2,     0,     1,     2,     3,     4,     6,     5,     7,
       6,     7,     7,     8,     4,     6,     5,     7,     1,     3,
       4,     5,     4,     3,     5,     1,     2,     3,     3,     3,
       5,     5,     5,     5,     3,     5,     5,     5,     3,     4,
       5,     5,     5,     5,     7,     7,     7,     7,     7,     7,
       7,     2,     3,     4,     4,     4,     6,     6,     6,     6,
       6,     6,     6,     3,     4,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     4,     2,     3,     3,     2,     3,
       2,     3,     3,     6,     2,     2,     3,     3,     3,     3,
       3,     3,     5,     1,     1,     5,     5,     4,     0,     1,
       1,     3,     4,     1,     1,     4,     6,     3,     5,     5,
       5,     8,     9,     1,     1,     1,     4,     3,     3,     1,
       3,     1,     3,     5,     5,     8,     9,     1,     3,     3,
       4,     8,     9,     0,     2,     1,     1,     1,     1,     2,
       1,     2,     2,     2,     1,     3,     1,     1,     6,     8,
      10,    12,    14,     0,     1,     0,     1,     1,     3,     4,
       7,     0,     1,     3,     1,     3,     0,     1,     1,     2,
       0,     1,     2,     3,     0,     1,     3,     4,     1,     3,
       2,     2,     1,     7,     5,     1,     1,     1,     1,     1,
       2,     3,     6,     3,     3,     4,     1,     2,     2,     3,
       8,     8,     8,     5,     9,     2,     2,     5,     3,     3,
       4,     3,     4,     4,     5,     2,     1,     1,     1,     3,
       3,     2,     4,     6,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     4,     1,     2,     3,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     5,     0,
       1,     1,     2,     2,     3,     3,     1,     3,     1,     2,
       2,     2,     4,     4,     4,     4,     1,     1,     1,     2,
       2,     3,     1,     0,     3,     2,     1,     2,     2,     3,
       1,     2,     2,     1,     2,     2,     3,     1,     2,     2,
       1,     2,     3,     1,     2,     3,     1,     3,     4,     1,
       1,     1,     1,     0,     7,     0,     8,     0,     8,     0,
       8,     1,     0,     3,     3,     3,     1,     1,     2,     1,
       1,     1,     2,     1,     2,     1,     2,     1,     2,     0,
       2,     3,     3,     4,     4,     4,     3,     2,     2,     3,
       3,     2,     1,     0,     1,     4,     1,     2,     2,     2,
       0,     1,     4,     1,     2,     3,     1,     2,     0,     1,
       2,     6,     7,     0,     9,     8,     9,    10,     8,     9,
       0,    13,    11,    12,    11,     1,     0,     1,     3,     3,
       3,     2,     5,     5,     1,     1,     0,     2,     5,     0,
       1,     1,     1,     5,     5,     5,     1,     5,     5,     9,
       1,     5,     0,     1,     1,     3,     1,     1,     3,     3,
       1,     3,     3,     4,     1,     1,     1,     1,     2,     1,
       3,     3,     2,     3,     1,     3,     1,     1,     1,     1,
       1,     2,     1,     1,     0,     2,     2,     4,     1,     4,
       0,     1,     2,     3,     4,     2,     2,     1,     2,     2,
       5,     5,     7,     6,     1,     3,     0,     2,     0,     5,
       0,     5,     3,     1,     0,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     5,     6,     1,     1,
       3,     3,     2,     3,     3,     2,     4,     1,     4,     7,
       5,    10,     8,     1,     4,     2,     2,     1,     1,     5,
       2,     5,     0,     1,     3,     4,     0,     1,     0,     0,
       1,     1,     2,     2,     2,     2,     2,     2,     1,     2,
       5,     0,     6,     0,     8,     0,     7,     0,     7,     0,
       8,     1,     2,     3,     0,     5,     3,     4,     4,     4,
       4,     5,     5,     5,     5,     6,     1,     1,     1,     1,
       3,     0,     5,     0,     1,     1,     2,     6,     1,     3,
       0,     1,     4,     1,     1,     1,     1,     1,     1,     1,
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
       6,     7,     6,     6,     0,     1,     0,     2
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
#line 607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7969 "Parser/parser.cc"
    break;

  case 3:
#line 611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7975 "Parser/parser.cc"
    break;

  case 4:
#line 618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 7981 "Parser/parser.cc"
    break;

  case 5:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7987 "Parser/parser.cc"
    break;

  case 6:
#line 620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7993 "Parser/parser.cc"
    break;

  case 7:
#line 621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7999 "Parser/parser.cc"
    break;

  case 8:
#line 622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8005 "Parser/parser.cc"
    break;

  case 20:
#line 644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8011 "Parser/parser.cc"
    break;

  case 21:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8017 "Parser/parser.cc"
    break;

  case 22:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8023 "Parser/parser.cc"
    break;

  case 23:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8033 "Parser/parser.cc"
    break;

  case 24:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8039 "Parser/parser.cc"
    break;

  case 25:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8045 "Parser/parser.cc"
    break;

  case 26:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8051 "Parser/parser.cc"
    break;

  case 28:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8057 "Parser/parser.cc"
    break;

  case 29:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8063 "Parser/parser.cc"
    break;

  case 30:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, (yyvsp[-2].decl), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8069 "Parser/parser.cc"
    break;

  case 31:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8075 "Parser/parser.cc"
    break;

  case 32:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8085 "Parser/parser.cc"
    break;

  case 33:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.expr) = nullptr; }
#line 8091 "Parser/parser.cc"
    break;

  case 34:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8097 "Parser/parser.cc"
    break;

  case 35:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8103 "Parser/parser.cc"
    break;

  case 36:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8109 "Parser/parser.cc"
    break;

  case 37:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8115 "Parser/parser.cc"
    break;

  case 38:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8121 "Parser/parser.cc"
    break;

  case 40:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8133 "Parser/parser.cc"
    break;

  case 41:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8142 "Parser/parser.cc"
    break;

  case 42:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8148 "Parser/parser.cc"
    break;

  case 44:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) )) ) ); }
#line 8154 "Parser/parser.cc"
    break;

  case 45:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8160 "Parser/parser.cc"
    break;

  case 46:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8166 "Parser/parser.cc"
    break;

  case 47:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8172 "Parser/parser.cc"
    break;

  case 48:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8182 "Parser/parser.cc"
    break;

  case 49:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8188 "Parser/parser.cc"
    break;

  case 50:
#line 754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 8195 "Parser/parser.cc"
    break;

  case 51:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8201 "Parser/parser.cc"
    break;

  case 52:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8207 "Parser/parser.cc"
    break;

  case 53:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8213 "Parser/parser.cc"
    break;

  case 54:
#line 763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8219 "Parser/parser.cc"
    break;

  case 55:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8225 "Parser/parser.cc"
    break;

  case 56:
#line 767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8231 "Parser/parser.cc"
    break;

  case 57:
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8237 "Parser/parser.cc"
    break;

  case 58:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8243 "Parser/parser.cc"
    break;

  case 59:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8249 "Parser/parser.cc"
    break;

  case 60:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8255 "Parser/parser.cc"
    break;

  case 61:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8261 "Parser/parser.cc"
    break;

  case 62:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8267 "Parser/parser.cc"
    break;

  case 63:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8273 "Parser/parser.cc"
    break;

  case 64:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8279 "Parser/parser.cc"
    break;

  case 65:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8285 "Parser/parser.cc"
    break;

  case 66:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8295 "Parser/parser.cc"
    break;

  case 67:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8301 "Parser/parser.cc"
    break;

  case 70:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8307 "Parser/parser.cc"
    break;

  case 71:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8313 "Parser/parser.cc"
    break;

  case 74:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8319 "Parser/parser.cc"
    break;

  case 76:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8325 "Parser/parser.cc"
    break;

  case 77:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8331 "Parser/parser.cc"
    break;

  case 78:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8337 "Parser/parser.cc"
    break;

  case 79:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8343 "Parser/parser.cc"
    break;

  case 80:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8349 "Parser/parser.cc"
    break;

  case 81:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8355 "Parser/parser.cc"
    break;

  case 82:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8361 "Parser/parser.cc"
    break;

  case 83:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8367 "Parser/parser.cc"
    break;

  case 84:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8375 "Parser/parser.cc"
    break;

  case 85:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8381 "Parser/parser.cc"
    break;

  case 86:
#line 849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8390 "Parser/parser.cc"
    break;

  case 89:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8396 "Parser/parser.cc"
    break;

  case 90:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8402 "Parser/parser.cc"
    break;

  case 91:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8422 "Parser/parser.cc"
    break;

  case 92:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8428 "Parser/parser.cc"
    break;

  case 93:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8434 "Parser/parser.cc"
    break;

  case 94:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8440 "Parser/parser.cc"
    break;

  case 95:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8446 "Parser/parser.cc"
    break;

  case 96:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8452 "Parser/parser.cc"
    break;

  case 97:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8458 "Parser/parser.cc"
    break;

  case 98:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8464 "Parser/parser.cc"
    break;

  case 99:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8470 "Parser/parser.cc"
    break;

  case 100:
#line 900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8479 "Parser/parser.cc"
    break;

  case 101:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 8485 "Parser/parser.cc"
    break;

  case 102:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 8491 "Parser/parser.cc"
    break;

  case 103:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 8497 "Parser/parser.cc"
    break;

  case 104:
#line 914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 8503 "Parser/parser.cc"
    break;

  case 105:
#line 915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 8509 "Parser/parser.cc"
    break;

  case 106:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 8515 "Parser/parser.cc"
    break;

  case 107:
#line 917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 8521 "Parser/parser.cc"
    break;

  case 109:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 8527 "Parser/parser.cc"
    break;

  case 110:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8533 "Parser/parser.cc"
    break;

  case 111:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8539 "Parser/parser.cc"
    break;

  case 112:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8545 "Parser/parser.cc"
    break;

  case 113:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8551 "Parser/parser.cc"
    break;

  case 114:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 8557 "Parser/parser.cc"
    break;

  case 115:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8563 "Parser/parser.cc"
    break;

  case 116:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8569 "Parser/parser.cc"
    break;

  case 124:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8575 "Parser/parser.cc"
    break;

  case 126:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8581 "Parser/parser.cc"
    break;

  case 127:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8587 "Parser/parser.cc"
    break;

  case 128:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8593 "Parser/parser.cc"
    break;

  case 130:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8599 "Parser/parser.cc"
    break;

  case 131:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8605 "Parser/parser.cc"
    break;

  case 133:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8611 "Parser/parser.cc"
    break;

  case 134:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8617 "Parser/parser.cc"
    break;

  case 136:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8623 "Parser/parser.cc"
    break;

  case 137:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8629 "Parser/parser.cc"
    break;

  case 138:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8635 "Parser/parser.cc"
    break;

  case 139:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8641 "Parser/parser.cc"
    break;

  case 141:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8647 "Parser/parser.cc"
    break;

  case 142:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8653 "Parser/parser.cc"
    break;

  case 144:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8659 "Parser/parser.cc"
    break;

  case 146:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8665 "Parser/parser.cc"
    break;

  case 148:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8671 "Parser/parser.cc"
    break;

  case 150:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 8677 "Parser/parser.cc"
    break;

  case 152:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 8683 "Parser/parser.cc"
    break;

  case 154:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8689 "Parser/parser.cc"
    break;

  case 155:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), (yyvsp[-3].expr)->clone(), (yyvsp[0].expr) ) ); }
#line 8695 "Parser/parser.cc"
    break;

  case 158:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 8707 "Parser/parser.cc"
    break;

  case 159:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8713 "Parser/parser.cc"
    break;

  case 160:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8719 "Parser/parser.cc"
    break;

  case 164:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 8725 "Parser/parser.cc"
    break;

  case 165:
#line 1077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 8731 "Parser/parser.cc"
    break;

  case 166:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 8737 "Parser/parser.cc"
    break;

  case 167:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 8743 "Parser/parser.cc"
    break;

  case 168:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 8749 "Parser/parser.cc"
    break;

  case 169:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 8755 "Parser/parser.cc"
    break;

  case 170:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 8761 "Parser/parser.cc"
    break;

  case 171:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 8767 "Parser/parser.cc"
    break;

  case 172:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 8773 "Parser/parser.cc"
    break;

  case 173:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 8779 "Parser/parser.cc"
    break;

  case 174:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 8785 "Parser/parser.cc"
    break;

  case 175:
#line 1090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 8791 "Parser/parser.cc"
    break;

  case 176:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 8797 "Parser/parser.cc"
    break;

  case 177:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].expr) ) ) ); }
#line 8803 "Parser/parser.cc"
    break;

  case 178:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) )); }
#line 8809 "Parser/parser.cc"
    break;

  case 180:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8815 "Parser/parser.cc"
    break;

  case 181:
#line 1112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8821 "Parser/parser.cc"
    break;

  case 182:
#line 1114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8827 "Parser/parser.cc"
    break;

  case 184:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8833 "Parser/parser.cc"
    break;

  case 185:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8839 "Parser/parser.cc"
    break;

  case 198:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 8845 "Parser/parser.cc"
    break;

  case 200:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 8851 "Parser/parser.cc"
    break;

  case 201:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8857 "Parser/parser.cc"
    break;

  case 202:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntx error, label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.stmt) = nullptr;
		}
#line 8868 "Parser/parser.cc"
    break;

  case 203:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 8874 "Parser/parser.cc"
    break;

  case 204:
#line 1170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 8880 "Parser/parser.cc"
    break;

  case 206:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8886 "Parser/parser.cc"
    break;

  case 207:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8892 "Parser/parser.cc"
    break;

  case 208:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8898 "Parser/parser.cc"
    break;

  case 209:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8904 "Parser/parser.cc"
    break;

  case 210:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8910 "Parser/parser.cc"
    break;

  case 213:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8916 "Parser/parser.cc"
    break;

  case 214:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 8922 "Parser/parser.cc"
    break;

  case 215:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 8928 "Parser/parser.cc"
    break;

  case 216:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8934 "Parser/parser.cc"
    break;

  case 217:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 8940 "Parser/parser.cc"
    break;

  case 218:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8954 "Parser/parser.cc"
    break;

  case 219:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 8960 "Parser/parser.cc"
    break;

  case 220:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 8966 "Parser/parser.cc"
    break;

  case 221:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8975 "Parser/parser.cc"
    break;

  case 222:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 8981 "Parser/parser.cc"
    break;

  case 223:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 8987 "Parser/parser.cc"
    break;

  case 224:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 8993 "Parser/parser.cc"
    break;

  case 225:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 8999 "Parser/parser.cc"
    break;

  case 226:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9005 "Parser/parser.cc"
    break;

  case 227:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9011 "Parser/parser.cc"
    break;

  case 228:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9017 "Parser/parser.cc"
    break;

  case 229:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9023 "Parser/parser.cc"
    break;

  case 230:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9029 "Parser/parser.cc"
    break;

  case 232:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9035 "Parser/parser.cc"
    break;

  case 233:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9041 "Parser/parser.cc"
    break;

  case 234:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9047 "Parser/parser.cc"
    break;

  case 235:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9053 "Parser/parser.cc"
    break;

  case 236:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9059 "Parser/parser.cc"
    break;

  case 237:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9065 "Parser/parser.cc"
    break;

  case 238:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9071 "Parser/parser.cc"
    break;

  case 240:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9077 "Parser/parser.cc"
    break;

  case 241:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9083 "Parser/parser.cc"
    break;

  case 242:
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9089 "Parser/parser.cc"
    break;

  case 244:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9095 "Parser/parser.cc"
    break;

  case 245:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9101 "Parser/parser.cc"
    break;

  case 246:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9107 "Parser/parser.cc"
    break;

  case 247:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9116 "Parser/parser.cc"
    break;

  case 248:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9122 "Parser/parser.cc"
    break;

  case 249:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9128 "Parser/parser.cc"
    break;

  case 250:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9134 "Parser/parser.cc"
    break;

  case 251:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9143 "Parser/parser.cc"
    break;

  case 252:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9149 "Parser/parser.cc"
    break;

  case 253:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9155 "Parser/parser.cc"
    break;

  case 254:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9161 "Parser/parser.cc"
    break;

  case 255:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9170 "Parser/parser.cc"
    break;

  case 256:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9176 "Parser/parser.cc"
    break;

  case 257:
#line 1336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9182 "Parser/parser.cc"
    break;

  case 259:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9201 "Parser/parser.cc"
    break;

  case 260:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9207 "Parser/parser.cc"
    break;

  case 261:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9216 "Parser/parser.cc"
    break;

  case 262:
#line 1371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9222 "Parser/parser.cc"
    break;

  case 263:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9228 "Parser/parser.cc"
    break;

  case 264:
#line 1376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9234 "Parser/parser.cc"
    break;

  case 265:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9240 "Parser/parser.cc"
    break;

  case 266:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9246 "Parser/parser.cc"
    break;

  case 267:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9252 "Parser/parser.cc"
    break;

  case 268:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9261 "Parser/parser.cc"
    break;

  case 269:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9270 "Parser/parser.cc"
    break;

  case 270:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9276 "Parser/parser.cc"
    break;

  case 271:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9285 "Parser/parser.cc"
    break;

  case 272:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9294 "Parser/parser.cc"
    break;

  case 273:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9300 "Parser/parser.cc"
    break;

  case 274:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9306 "Parser/parser.cc"
    break;

  case 275:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9312 "Parser/parser.cc"
    break;

  case 276:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9318 "Parser/parser.cc"
    break;

  case 277:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9324 "Parser/parser.cc"
    break;

  case 278:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9330 "Parser/parser.cc"
    break;

  case 279:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9336 "Parser/parser.cc"
    break;

  case 280:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9342 "Parser/parser.cc"
    break;

  case 281:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9351 "Parser/parser.cc"
    break;

  case 282:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9361 "Parser/parser.cc"
    break;

  case 283:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9367 "Parser/parser.cc"
    break;

  case 284:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9373 "Parser/parser.cc"
    break;

  case 285:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9382 "Parser/parser.cc"
    break;

  case 286:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9392 "Parser/parser.cc"
    break;

  case 287:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9398 "Parser/parser.cc"
    break;

  case 288:
#line 1455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9407 "Parser/parser.cc"
    break;

  case 289:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9417 "Parser/parser.cc"
    break;

  case 290:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9423 "Parser/parser.cc"
    break;

  case 291:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9429 "Parser/parser.cc"
    break;

  case 292:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9435 "Parser/parser.cc"
    break;

  case 293:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9441 "Parser/parser.cc"
    break;

  case 294:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9450 "Parser/parser.cc"
    break;

  case 295:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9460 "Parser/parser.cc"
    break;

  case 296:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9466 "Parser/parser.cc"
    break;

  case 297:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9475 "Parser/parser.cc"
    break;

  case 298:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9485 "Parser/parser.cc"
    break;

  case 299:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9491 "Parser/parser.cc"
    break;

  case 300:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9500 "Parser/parser.cc"
    break;

  case 301:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9510 "Parser/parser.cc"
    break;

  case 302:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9516 "Parser/parser.cc"
    break;

  case 303:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9525 "Parser/parser.cc"
    break;

  case 304:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 9536 "Parser/parser.cc"
    break;

  case 305:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9542 "Parser/parser.cc"
    break;

  case 306:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9548 "Parser/parser.cc"
    break;

  case 307:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9554 "Parser/parser.cc"
    break;

  case 308:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 9560 "Parser/parser.cc"
    break;

  case 309:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9566 "Parser/parser.cc"
    break;

  case 311:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9572 "Parser/parser.cc"
    break;

  case 312:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9578 "Parser/parser.cc"
    break;

  case 313:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 9584 "Parser/parser.cc"
    break;

  case 314:
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 9590 "Parser/parser.cc"
    break;

  case 315:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 9596 "Parser/parser.cc"
    break;

  case 316:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 9602 "Parser/parser.cc"
    break;

  case 317:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 9608 "Parser/parser.cc"
    break;

  case 318:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 9614 "Parser/parser.cc"
    break;

  case 319:
#line 1574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 9620 "Parser/parser.cc"
    break;

  case 320:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 9626 "Parser/parser.cc"
    break;

  case 321:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 9632 "Parser/parser.cc"
    break;

  case 322:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 9638 "Parser/parser.cc"
    break;

  case 323:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9644 "Parser/parser.cc"
    break;

  case 324:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 9650 "Parser/parser.cc"
    break;

  case 325:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 9656 "Parser/parser.cc"
    break;

  case 326:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 9662 "Parser/parser.cc"
    break;

  case 327:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 9668 "Parser/parser.cc"
    break;

  case 328:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 9674 "Parser/parser.cc"
    break;

  case 329:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 9680 "Parser/parser.cc"
    break;

  case 330:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 9686 "Parser/parser.cc"
    break;

  case 331:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 9692 "Parser/parser.cc"
    break;

  case 332:
#line 1603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9698 "Parser/parser.cc"
    break;

  case 335:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9704 "Parser/parser.cc"
    break;

  case 336:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 9713 "Parser/parser.cc"
    break;

  case 337:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9719 "Parser/parser.cc"
    break;

  case 338:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9725 "Parser/parser.cc"
    break;

  case 341:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9731 "Parser/parser.cc"
    break;

  case 342:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9737 "Parser/parser.cc"
    break;

  case 345:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9743 "Parser/parser.cc"
    break;

  case 346:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) )); }
#line 9749 "Parser/parser.cc"
    break;

  case 347:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9755 "Parser/parser.cc"
    break;

  case 348:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9761 "Parser/parser.cc"
    break;

  case 349:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9767 "Parser/parser.cc"
    break;

  case 350:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9773 "Parser/parser.cc"
    break;

  case 351:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9779 "Parser/parser.cc"
    break;

  case 352:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9785 "Parser/parser.cc"
    break;

  case 353:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 9791 "Parser/parser.cc"
    break;

  case 356:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9797 "Parser/parser.cc"
    break;

  case 357:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9803 "Parser/parser.cc"
    break;

  case 358:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 9809 "Parser/parser.cc"
    break;

  case 359:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9815 "Parser/parser.cc"
    break;

  case 360:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9821 "Parser/parser.cc"
    break;

  case 361:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9827 "Parser/parser.cc"
    break;

  case 362:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9833 "Parser/parser.cc"
    break;

  case 363:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9839 "Parser/parser.cc"
    break;

  case 364:
#line 1711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_timeout( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9845 "Parser/parser.cc"
    break;

  case 365:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wucn) = nullptr; }
#line 9851 "Parser/parser.cc"
    break;

  case 366:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-8].wucn),
				new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, 
					build_waituntil_timeout( yylloc, (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), 
					build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9860 "Parser/parser.cc"
    break;

  case 367:
#line 1725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );
			// $$ = new StatementNode( build_compound( yylloc, nullptr ) );
		}
#line 9869 "Parser/parser.cc"
    break;

  case 368:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 9875 "Parser/parser.cc"
    break;

  case 369:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 9881 "Parser/parser.cc"
    break;

  case 370:
#line 1737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 9887 "Parser/parser.cc"
    break;

  case 371:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9893 "Parser/parser.cc"
    break;

  case 372:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 9899 "Parser/parser.cc"
    break;

  case 373:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9905 "Parser/parser.cc"
    break;

  case 374:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9911 "Parser/parser.cc"
    break;

  case 375:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9917 "Parser/parser.cc"
    break;

  case 376:
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9923 "Parser/parser.cc"
    break;

  case 377:
#line 1756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 9929 "Parser/parser.cc"
    break;

  case 378:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 9935 "Parser/parser.cc"
    break;

  case 379:
#line 1761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 9941 "Parser/parser.cc"
    break;

  case 381:
#line 1768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9947 "Parser/parser.cc"
    break;

  case 382:
#line 1770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9953 "Parser/parser.cc"
    break;

  case 383:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9959 "Parser/parser.cc"
    break;

  case 388:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 9965 "Parser/parser.cc"
    break;

  case 389:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9971 "Parser/parser.cc"
    break;

  case 390:
#line 1791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9977 "Parser/parser.cc"
    break;

  case 391:
#line 1793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9983 "Parser/parser.cc"
    break;

  case 392:
#line 1795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 9989 "Parser/parser.cc"
    break;

  case 393:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 9995 "Parser/parser.cc"
    break;

  case 394:
#line 1802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10001 "Parser/parser.cc"
    break;

  case 395:
#line 1807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10007 "Parser/parser.cc"
    break;

  case 398:
#line 1814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 10013 "Parser/parser.cc"
    break;

  case 399:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10019 "Parser/parser.cc"
    break;

  case 400:
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10028 "Parser/parser.cc"
    break;

  case 401:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10034 "Parser/parser.cc"
    break;

  case 402:
#line 1831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10040 "Parser/parser.cc"
    break;

  case 403:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ) ); }
#line 10046 "Parser/parser.cc"
    break;

  case 404:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10055 "Parser/parser.cc"
    break;

  case 405:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10064 "Parser/parser.cc"
    break;

  case 406:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10070 "Parser/parser.cc"
    break;

  case 409:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10076 "Parser/parser.cc"
    break;

  case 410:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10082 "Parser/parser.cc"
    break;

  case 412:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10088 "Parser/parser.cc"
    break;

  case 413:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-1].decl) ); }
#line 10094 "Parser/parser.cc"
    break;

  case 420:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10105 "Parser/parser.cc"
    break;

  case 423:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10111 "Parser/parser.cc"
    break;

  case 424:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10117 "Parser/parser.cc"
    break;

  case 428:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10123 "Parser/parser.cc"
    break;

  case 430:
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10129 "Parser/parser.cc"
    break;

  case 431:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10135 "Parser/parser.cc"
    break;

  case 432:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10141 "Parser/parser.cc"
    break;

  case 433:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10147 "Parser/parser.cc"
    break;

  case 434:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10153 "Parser/parser.cc"
    break;

  case 435:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10159 "Parser/parser.cc"
    break;

  case 437:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10165 "Parser/parser.cc"
    break;

  case 438:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10171 "Parser/parser.cc"
    break;

  case 439:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10177 "Parser/parser.cc"
    break;

  case 440:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10188 "Parser/parser.cc"
    break;

  case 441:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10194 "Parser/parser.cc"
    break;

  case 442:
#line 1995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10200 "Parser/parser.cc"
    break;

  case 443:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10206 "Parser/parser.cc"
    break;

  case 444:
#line 2003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 10212 "Parser/parser.cc"
    break;

  case 445:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10221 "Parser/parser.cc"
    break;

  case 446:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10230 "Parser/parser.cc"
    break;

  case 447:
#line 2018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10239 "Parser/parser.cc"
    break;

  case 448:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10250 "Parser/parser.cc"
    break;

  case 449:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10259 "Parser/parser.cc"
    break;

  case 450:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10265 "Parser/parser.cc"
    break;

  case 451:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10271 "Parser/parser.cc"
    break;

  case 452:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10277 "Parser/parser.cc"
    break;

  case 453:
#line 2051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10285 "Parser/parser.cc"
    break;

  case 454:
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10293 "Parser/parser.cc"
    break;

  case 455:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10299 "Parser/parser.cc"
    break;

  case 458:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10314 "Parser/parser.cc"
    break;

  case 459:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10320 "Parser/parser.cc"
    break;

  case 460:
#line 2084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10326 "Parser/parser.cc"
    break;

  case 461:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10332 "Parser/parser.cc"
    break;

  case 462:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10338 "Parser/parser.cc"
    break;

  case 463:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10344 "Parser/parser.cc"
    break;

  case 469:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntax error, expecting ';' at end of ",
				(yyvsp[-1].decl)->type->enumeration.name ? "enum" : ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ),
				" declaration." ) );
			(yyval.decl) = nullptr;
		}
#line 10355 "Parser/parser.cc"
    break;

  case 482:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10361 "Parser/parser.cc"
    break;

  case 485:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10367 "Parser/parser.cc"
    break;

  case 488:
#line 2171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 10373 "Parser/parser.cc"
    break;

  case 489:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 10379 "Parser/parser.cc"
    break;

  case 490:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 10385 "Parser/parser.cc"
    break;

  case 491:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 10391 "Parser/parser.cc"
    break;

  case 492:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 10397 "Parser/parser.cc"
    break;

  case 493:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10403 "Parser/parser.cc"
    break;

  case 495:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10409 "Parser/parser.cc"
    break;

  case 496:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10415 "Parser/parser.cc"
    break;

  case 498:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10421 "Parser/parser.cc"
    break;

  case 499:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 10427 "Parser/parser.cc"
    break;

  case 500:
#line 2210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 10433 "Parser/parser.cc"
    break;

  case 501:
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10439 "Parser/parser.cc"
    break;

  case 502:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10445 "Parser/parser.cc"
    break;

  case 503:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10451 "Parser/parser.cc"
    break;

  case 504:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10457 "Parser/parser.cc"
    break;

  case 505:
#line 2221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10463 "Parser/parser.cc"
    break;

  case 506:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10469 "Parser/parser.cc"
    break;

  case 507:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10475 "Parser/parser.cc"
    break;

  case 508:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10481 "Parser/parser.cc"
    break;

  case 509:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10487 "Parser/parser.cc"
    break;

  case 510:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10493 "Parser/parser.cc"
    break;

  case 511:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10499 "Parser/parser.cc"
    break;

  case 512:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10505 "Parser/parser.cc"
    break;

  case 513:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10511 "Parser/parser.cc"
    break;

  case 514:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10517 "Parser/parser.cc"
    break;

  case 515:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10523 "Parser/parser.cc"
    break;

  case 516:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10529 "Parser/parser.cc"
    break;

  case 517:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10535 "Parser/parser.cc"
    break;

  case 518:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10541 "Parser/parser.cc"
    break;

  case 519:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10547 "Parser/parser.cc"
    break;

  case 520:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10553 "Parser/parser.cc"
    break;

  case 521:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10559 "Parser/parser.cc"
    break;

  case 522:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10565 "Parser/parser.cc"
    break;

  case 523:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10571 "Parser/parser.cc"
    break;

  case 524:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10577 "Parser/parser.cc"
    break;

  case 525:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10583 "Parser/parser.cc"
    break;

  case 526:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10589 "Parser/parser.cc"
    break;

  case 527:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10595 "Parser/parser.cc"
    break;

  case 528:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10601 "Parser/parser.cc"
    break;

  case 529:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10607 "Parser/parser.cc"
    break;

  case 530:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10613 "Parser/parser.cc"
    break;

  case 531:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10619 "Parser/parser.cc"
    break;

  case 532:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10625 "Parser/parser.cc"
    break;

  case 533:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10631 "Parser/parser.cc"
    break;

  case 534:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10637 "Parser/parser.cc"
    break;

  case 536:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10643 "Parser/parser.cc"
    break;

  case 538:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10649 "Parser/parser.cc"
    break;

  case 539:
#line 2300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10655 "Parser/parser.cc"
    break;

  case 540:
#line 2302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10661 "Parser/parser.cc"
    break;

  case 542:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10667 "Parser/parser.cc"
    break;

  case 543:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10673 "Parser/parser.cc"
    break;

  case 544:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10679 "Parser/parser.cc"
    break;

  case 545:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10685 "Parser/parser.cc"
    break;

  case 547:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10691 "Parser/parser.cc"
    break;

  case 549:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10697 "Parser/parser.cc"
    break;

  case 550:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10703 "Parser/parser.cc"
    break;

  case 551:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10709 "Parser/parser.cc"
    break;

  case 552:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10715 "Parser/parser.cc"
    break;

  case 553:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 10721 "Parser/parser.cc"
    break;

  case 554:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10727 "Parser/parser.cc"
    break;

  case 555:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 10733 "Parser/parser.cc"
    break;

  case 556:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10739 "Parser/parser.cc"
    break;

  case 557:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10745 "Parser/parser.cc"
    break;

  case 558:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10756 "Parser/parser.cc"
    break;

  case 559:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10762 "Parser/parser.cc"
    break;

  case 560:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10768 "Parser/parser.cc"
    break;

  case 561:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10774 "Parser/parser.cc"
    break;

  case 562:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10785 "Parser/parser.cc"
    break;

  case 563:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10791 "Parser/parser.cc"
    break;

  case 564:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10797 "Parser/parser.cc"
    break;

  case 565:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10806 "Parser/parser.cc"
    break;

  case 567:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10812 "Parser/parser.cc"
    break;

  case 568:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 569:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10824 "Parser/parser.cc"
    break;

  case 571:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10830 "Parser/parser.cc"
    break;

  case 572:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10836 "Parser/parser.cc"
    break;

  case 574:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10842 "Parser/parser.cc"
    break;

  case 575:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10848 "Parser/parser.cc"
    break;

  case 576:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10854 "Parser/parser.cc"
    break;

  case 578:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10860 "Parser/parser.cc"
    break;

  case 579:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10866 "Parser/parser.cc"
    break;

  case 580:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10872 "Parser/parser.cc"
    break;

  case 581:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10878 "Parser/parser.cc"
    break;

  case 582:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10884 "Parser/parser.cc"
    break;

  case 584:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 585:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 586:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10902 "Parser/parser.cc"
    break;

  case 587:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10908 "Parser/parser.cc"
    break;

  case 588:
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 10914 "Parser/parser.cc"
    break;

  case 589:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10925 "Parser/parser.cc"
    break;

  case 593:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10931 "Parser/parser.cc"
    break;

  case 594:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10937 "Parser/parser.cc"
    break;

  case 595:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 10946 "Parser/parser.cc"
    break;

  case 596:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10954 "Parser/parser.cc"
    break;

  case 597:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 10963 "Parser/parser.cc"
    break;

  case 598:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10972 "Parser/parser.cc"
    break;

  case 599:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 10981 "Parser/parser.cc"
    break;

  case 600:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10990 "Parser/parser.cc"
    break;

  case 602:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10996 "Parser/parser.cc"
    break;

  case 603:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11002 "Parser/parser.cc"
    break;

  case 604:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11012 "Parser/parser.cc"
    break;

  case 605:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11031 "Parser/parser.cc"
    break;

  case 608:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11037 "Parser/parser.cc"
    break;

  case 609:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11043 "Parser/parser.cc"
    break;

  case 610:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11049 "Parser/parser.cc"
    break;

  case 611:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11055 "Parser/parser.cc"
    break;

  case 612:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11061 "Parser/parser.cc"
    break;

  case 613:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11067 "Parser/parser.cc"
    break;

  case 614:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11076 "Parser/parser.cc"
    break;

  case 615:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11082 "Parser/parser.cc"
    break;

  case 616:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11091 "Parser/parser.cc"
    break;

  case 617:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11097 "Parser/parser.cc"
    break;

  case 618:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11106 "Parser/parser.cc"
    break;

  case 619:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11112 "Parser/parser.cc"
    break;

  case 620:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11118 "Parser/parser.cc"
    break;

  case 621:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11131 "Parser/parser.cc"
    break;

  case 622:
#line 2589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntax error, expecting ';' at end of previous declaration." ) );
			(yyval.decl) = nullptr;
		}
#line 11140 "Parser/parser.cc"
    break;

  case 623:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11146 "Parser/parser.cc"
    break;

  case 624:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11152 "Parser/parser.cc"
    break;

  case 625:
#line 2598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11165 "Parser/parser.cc"
    break;

  case 626:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11171 "Parser/parser.cc"
    break;

  case 629:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11177 "Parser/parser.cc"
    break;

  case 630:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11183 "Parser/parser.cc"
    break;

  case 633:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11189 "Parser/parser.cc"
    break;

  case 635:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11195 "Parser/parser.cc"
    break;

  case 636:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11201 "Parser/parser.cc"
    break;

  case 637:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11207 "Parser/parser.cc"
    break;

  case 638:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11213 "Parser/parser.cc"
    break;

  case 639:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11219 "Parser/parser.cc"
    break;

  case 640:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11225 "Parser/parser.cc"
    break;

  case 642:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11231 "Parser/parser.cc"
    break;

  case 644:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11237 "Parser/parser.cc"
    break;

  case 645:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11243 "Parser/parser.cc"
    break;

  case 647:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11249 "Parser/parser.cc"
    break;

  case 648:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11255 "Parser/parser.cc"
    break;

  case 650:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11261 "Parser/parser.cc"
    break;

  case 651:
#line 2681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11267 "Parser/parser.cc"
    break;

  case 652:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11273 "Parser/parser.cc"
    break;

  case 653:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11279 "Parser/parser.cc"
    break;

  case 654:
#line 2687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11285 "Parser/parser.cc"
    break;

  case 655:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11291 "Parser/parser.cc"
    break;

  case 656:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11302 "Parser/parser.cc"
    break;

  case 657:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11308 "Parser/parser.cc"
    break;

  case 658:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11316 "Parser/parser.cc"
    break;

  case 659:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11322 "Parser/parser.cc"
    break;

  case 660:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11333 "Parser/parser.cc"
    break;

  case 661:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11341 "Parser/parser.cc"
    break;

  case 662:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11349 "Parser/parser.cc"
    break;

  case 663:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11357 "Parser/parser.cc"
    break;

  case 664:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11365 "Parser/parser.cc"
    break;

  case 666:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11371 "Parser/parser.cc"
    break;

  case 667:
#line 2735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11377 "Parser/parser.cc"
    break;

  case 668:
#line 2740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11386 "Parser/parser.cc"
    break;

  case 669:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11395 "Parser/parser.cc"
    break;

  case 670:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11401 "Parser/parser.cc"
    break;

  case 671:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 11407 "Parser/parser.cc"
    break;

  case 672:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11413 "Parser/parser.cc"
    break;

  case 673:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11419 "Parser/parser.cc"
    break;

  case 675:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11425 "Parser/parser.cc"
    break;

  case 676:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11431 "Parser/parser.cc"
    break;

  case 677:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11437 "Parser/parser.cc"
    break;

  case 678:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11443 "Parser/parser.cc"
    break;

  case 679:
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11449 "Parser/parser.cc"
    break;

  case 680:
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11455 "Parser/parser.cc"
    break;

  case 683:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11461 "Parser/parser.cc"
    break;

  case 684:
#line 2787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11467 "Parser/parser.cc"
    break;

  case 685:
#line 2789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11473 "Parser/parser.cc"
    break;

  case 687:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11479 "Parser/parser.cc"
    break;

  case 688:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11485 "Parser/parser.cc"
    break;

  case 689:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11491 "Parser/parser.cc"
    break;

  case 691:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11497 "Parser/parser.cc"
    break;

  case 692:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11503 "Parser/parser.cc"
    break;

  case 693:
#line 2814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11509 "Parser/parser.cc"
    break;

  case 695:
#line 2817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11515 "Parser/parser.cc"
    break;

  case 698:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11521 "Parser/parser.cc"
    break;

  case 699:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11527 "Parser/parser.cc"
    break;

  case 701:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11533 "Parser/parser.cc"
    break;

  case 702:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11539 "Parser/parser.cc"
    break;

  case 703:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11545 "Parser/parser.cc"
    break;

  case 708:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11551 "Parser/parser.cc"
    break;

  case 710:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11557 "Parser/parser.cc"
    break;

  case 711:
#line 2859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11563 "Parser/parser.cc"
    break;

  case 712:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11569 "Parser/parser.cc"
    break;

  case 713:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11575 "Parser/parser.cc"
    break;

  case 714:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11581 "Parser/parser.cc"
    break;

  case 715:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11587 "Parser/parser.cc"
    break;

  case 721:
#line 2890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11593 "Parser/parser.cc"
    break;

  case 724:
#line 2900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11599 "Parser/parser.cc"
    break;

  case 725:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 11605 "Parser/parser.cc"
    break;

  case 726:
#line 2902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 11611 "Parser/parser.cc"
    break;

  case 727:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11617 "Parser/parser.cc"
    break;

  case 728:
#line 2907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11623 "Parser/parser.cc"
    break;

  case 729:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11629 "Parser/parser.cc"
    break;

  case 730:
#line 2913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11635 "Parser/parser.cc"
    break;

  case 732:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 11641 "Parser/parser.cc"
    break;

  case 733:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (InitializerNode *)( (yyvsp[-2].init)->set_last( (yyvsp[0].init) ) ); }
#line 11647 "Parser/parser.cc"
    break;

  case 734:
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (InitializerNode *)((yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) )); }
#line 11653 "Parser/parser.cc"
    break;

  case 736:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 11659 "Parser/parser.cc"
    break;

  case 738:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-1].expr)->set_last( (yyvsp[0].expr) )); }
#line 11665 "Parser/parser.cc"
    break;

  case 739:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 11671 "Parser/parser.cc"
    break;

  case 740:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11677 "Parser/parser.cc"
    break;

  case 741:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11683 "Parser/parser.cc"
    break;

  case 742:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 11689 "Parser/parser.cc"
    break;

  case 743:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11695 "Parser/parser.cc"
    break;

  case 745:
#line 2978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11701 "Parser/parser.cc"
    break;

  case 746:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11707 "Parser/parser.cc"
    break;

  case 747:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11713 "Parser/parser.cc"
    break;

  case 748:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11724 "Parser/parser.cc"
    break;

  case 749:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11730 "Parser/parser.cc"
    break;

  case 750:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 11736 "Parser/parser.cc"
    break;

  case 751:
#line 3001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11742 "Parser/parser.cc"
    break;

  case 752:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11751 "Parser/parser.cc"
    break;

  case 753:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11757 "Parser/parser.cc"
    break;

  case 754:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11763 "Parser/parser.cc"
    break;

  case 755:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11769 "Parser/parser.cc"
    break;

  case 756:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 11775 "Parser/parser.cc"
    break;

  case 757:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11781 "Parser/parser.cc"
    break;

  case 758:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11787 "Parser/parser.cc"
    break;

  case 759:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11793 "Parser/parser.cc"
    break;

  case 760:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 11799 "Parser/parser.cc"
    break;

  case 761:
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11805 "Parser/parser.cc"
    break;

  case 762:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11811 "Parser/parser.cc"
    break;

  case 765:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11817 "Parser/parser.cc"
    break;

  case 766:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11823 "Parser/parser.cc"
    break;

  case 767:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11829 "Parser/parser.cc"
    break;

  case 768:
#line 3059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11835 "Parser/parser.cc"
    break;

  case 770:
#line 3062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11841 "Parser/parser.cc"
    break;

  case 771:
#line 3064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 11847 "Parser/parser.cc"
    break;

  case 772:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11853 "Parser/parser.cc"
    break;

  case 773:
#line 3071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11859 "Parser/parser.cc"
    break;

  case 774:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11865 "Parser/parser.cc"
    break;

  case 775:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11871 "Parser/parser.cc"
    break;

  case 776:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11877 "Parser/parser.cc"
    break;

  case 777:
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11886 "Parser/parser.cc"
    break;

  case 778:
#line 3090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11895 "Parser/parser.cc"
    break;

  case 779:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11904 "Parser/parser.cc"
    break;

  case 780:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11910 "Parser/parser.cc"
    break;

  case 781:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11919 "Parser/parser.cc"
    break;

  case 782:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11925 "Parser/parser.cc"
    break;

  case 784:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11931 "Parser/parser.cc"
    break;

  case 789:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11937 "Parser/parser.cc"
    break;

  case 790:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11943 "Parser/parser.cc"
    break;

  case 791:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11949 "Parser/parser.cc"
    break;

  case 793:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11955 "Parser/parser.cc"
    break;

  case 794:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11961 "Parser/parser.cc"
    break;

  case 795:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11967 "Parser/parser.cc"
    break;

  case 796:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11973 "Parser/parser.cc"
    break;

  case 798:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11979 "Parser/parser.cc"
    break;

  case 799:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11985 "Parser/parser.cc"
    break;

  case 800:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 11991 "Parser/parser.cc"
    break;

  case 801:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12008 "Parser/parser.cc"
    break;

  case 802:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12014 "Parser/parser.cc"
    break;

  case 803:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12020 "Parser/parser.cc"
    break;

  case 804:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12026 "Parser/parser.cc"
    break;

  case 805:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12032 "Parser/parser.cc"
    break;

  case 806:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12038 "Parser/parser.cc"
    break;

  case 807:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12044 "Parser/parser.cc"
    break;

  case 809:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12053 "Parser/parser.cc"
    break;

  case 810:
#line 3202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12059 "Parser/parser.cc"
    break;

  case 811:
#line 3204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12068 "Parser/parser.cc"
    break;

  case 812:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12078 "Parser/parser.cc"
    break;

  case 813:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12087 "Parser/parser.cc"
    break;

  case 814:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12097 "Parser/parser.cc"
    break;

  case 815:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12108 "Parser/parser.cc"
    break;

  case 816:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12118 "Parser/parser.cc"
    break;

  case 817:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12129 "Parser/parser.cc"
    break;

  case 818:
#line 3247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12139 "Parser/parser.cc"
    break;

  case 819:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12150 "Parser/parser.cc"
    break;

  case 820:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12160 "Parser/parser.cc"
    break;

  case 822:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12166 "Parser/parser.cc"
    break;

  case 823:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12172 "Parser/parser.cc"
    break;

  case 824:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12178 "Parser/parser.cc"
    break;

  case 825:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12190 "Parser/parser.cc"
    break;

  case 826:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12201 "Parser/parser.cc"
    break;

  case 827:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12210 "Parser/parser.cc"
    break;

  case 828:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12219 "Parser/parser.cc"
    break;

  case 829:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12225 "Parser/parser.cc"
    break;

  case 830:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12231 "Parser/parser.cc"
    break;

  case 831:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12237 "Parser/parser.cc"
    break;

  case 832:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12246 "Parser/parser.cc"
    break;

  case 833:
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12252 "Parser/parser.cc"
    break;

  case 834:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12258 "Parser/parser.cc"
    break;

  case 835:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12264 "Parser/parser.cc"
    break;

  case 840:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12270 "Parser/parser.cc"
    break;

  case 841:
#line 3352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12276 "Parser/parser.cc"
    break;

  case 842:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12286 "Parser/parser.cc"
    break;

  case 843:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12292 "Parser/parser.cc"
    break;

  case 846:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12298 "Parser/parser.cc"
    break;

  case 847:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12304 "Parser/parser.cc"
    break;

  case 849:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12310 "Parser/parser.cc"
    break;

  case 850:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12316 "Parser/parser.cc"
    break;

  case 851:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12322 "Parser/parser.cc"
    break;

  case 852:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12328 "Parser/parser.cc"
    break;

  case 857:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12334 "Parser/parser.cc"
    break;

  case 858:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12340 "Parser/parser.cc"
    break;

  case 859:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12346 "Parser/parser.cc"
    break;

  case 860:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12352 "Parser/parser.cc"
    break;

  case 861:
#line 3443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12358 "Parser/parser.cc"
    break;

  case 863:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12364 "Parser/parser.cc"
    break;

  case 864:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12370 "Parser/parser.cc"
    break;

  case 865:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12376 "Parser/parser.cc"
    break;

  case 866:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12382 "Parser/parser.cc"
    break;

  case 867:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12388 "Parser/parser.cc"
    break;

  case 868:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12394 "Parser/parser.cc"
    break;

  case 869:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12400 "Parser/parser.cc"
    break;

  case 870:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12406 "Parser/parser.cc"
    break;

  case 871:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12412 "Parser/parser.cc"
    break;

  case 872:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12418 "Parser/parser.cc"
    break;

  case 873:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12424 "Parser/parser.cc"
    break;

  case 874:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12430 "Parser/parser.cc"
    break;

  case 875:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12436 "Parser/parser.cc"
    break;

  case 876:
#line 3481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12442 "Parser/parser.cc"
    break;

  case 877:
#line 3483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12448 "Parser/parser.cc"
    break;

  case 878:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12454 "Parser/parser.cc"
    break;

  case 879:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12460 "Parser/parser.cc"
    break;

  case 880:
#line 3496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12466 "Parser/parser.cc"
    break;

  case 882:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12472 "Parser/parser.cc"
    break;

  case 883:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12478 "Parser/parser.cc"
    break;

  case 884:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12484 "Parser/parser.cc"
    break;

  case 885:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12490 "Parser/parser.cc"
    break;

  case 886:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12496 "Parser/parser.cc"
    break;

  case 887:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12502 "Parser/parser.cc"
    break;

  case 888:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12508 "Parser/parser.cc"
    break;

  case 889:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12514 "Parser/parser.cc"
    break;

  case 890:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12520 "Parser/parser.cc"
    break;

  case 891:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12526 "Parser/parser.cc"
    break;

  case 892:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12532 "Parser/parser.cc"
    break;

  case 893:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12538 "Parser/parser.cc"
    break;

  case 894:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12544 "Parser/parser.cc"
    break;

  case 895:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12550 "Parser/parser.cc"
    break;

  case 896:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12556 "Parser/parser.cc"
    break;

  case 897:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12562 "Parser/parser.cc"
    break;

  case 901:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12568 "Parser/parser.cc"
    break;

  case 902:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12574 "Parser/parser.cc"
    break;

  case 903:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12580 "Parser/parser.cc"
    break;

  case 904:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12586 "Parser/parser.cc"
    break;

  case 905:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12592 "Parser/parser.cc"
    break;

  case 906:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12598 "Parser/parser.cc"
    break;

  case 907:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12604 "Parser/parser.cc"
    break;

  case 908:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12610 "Parser/parser.cc"
    break;

  case 909:
#line 3575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12616 "Parser/parser.cc"
    break;

  case 910:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12622 "Parser/parser.cc"
    break;

  case 911:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12628 "Parser/parser.cc"
    break;

  case 912:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12634 "Parser/parser.cc"
    break;

  case 913:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12640 "Parser/parser.cc"
    break;

  case 914:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12646 "Parser/parser.cc"
    break;

  case 915:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12652 "Parser/parser.cc"
    break;

  case 916:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 12661 "Parser/parser.cc"
    break;

  case 917:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12667 "Parser/parser.cc"
    break;

  case 918:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12673 "Parser/parser.cc"
    break;

  case 920:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12679 "Parser/parser.cc"
    break;

  case 921:
#line 3617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12685 "Parser/parser.cc"
    break;

  case 922:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12691 "Parser/parser.cc"
    break;

  case 923:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12697 "Parser/parser.cc"
    break;

  case 924:
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12703 "Parser/parser.cc"
    break;

  case 925:
#line 3628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12709 "Parser/parser.cc"
    break;

  case 926:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12715 "Parser/parser.cc"
    break;

  case 927:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12721 "Parser/parser.cc"
    break;

  case 928:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12727 "Parser/parser.cc"
    break;

  case 929:
#line 3639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12733 "Parser/parser.cc"
    break;

  case 930:
#line 3641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12739 "Parser/parser.cc"
    break;

  case 931:
#line 3643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12745 "Parser/parser.cc"
    break;

  case 932:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12751 "Parser/parser.cc"
    break;

  case 933:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12757 "Parser/parser.cc"
    break;

  case 934:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12763 "Parser/parser.cc"
    break;

  case 935:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12769 "Parser/parser.cc"
    break;

  case 936:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12775 "Parser/parser.cc"
    break;

  case 937:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12781 "Parser/parser.cc"
    break;

  case 939:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12787 "Parser/parser.cc"
    break;

  case 940:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12793 "Parser/parser.cc"
    break;

  case 941:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12799 "Parser/parser.cc"
    break;

  case 942:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12805 "Parser/parser.cc"
    break;

  case 943:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12811 "Parser/parser.cc"
    break;

  case 944:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12817 "Parser/parser.cc"
    break;

  case 945:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12823 "Parser/parser.cc"
    break;

  case 946:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12829 "Parser/parser.cc"
    break;

  case 947:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12835 "Parser/parser.cc"
    break;

  case 948:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12841 "Parser/parser.cc"
    break;

  case 949:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12847 "Parser/parser.cc"
    break;

  case 950:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12853 "Parser/parser.cc"
    break;

  case 951:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12859 "Parser/parser.cc"
    break;

  case 952:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12865 "Parser/parser.cc"
    break;

  case 953:
#line 3705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12871 "Parser/parser.cc"
    break;

  case 954:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12877 "Parser/parser.cc"
    break;

  case 955:
#line 3717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12883 "Parser/parser.cc"
    break;

  case 956:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12889 "Parser/parser.cc"
    break;

  case 958:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12895 "Parser/parser.cc"
    break;

  case 959:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12901 "Parser/parser.cc"
    break;

  case 960:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12907 "Parser/parser.cc"
    break;

  case 961:
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12913 "Parser/parser.cc"
    break;

  case 962:
#line 3733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12919 "Parser/parser.cc"
    break;

  case 963:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12925 "Parser/parser.cc"
    break;

  case 964:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12931 "Parser/parser.cc"
    break;

  case 965:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12937 "Parser/parser.cc"
    break;

  case 966:
#line 3744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12943 "Parser/parser.cc"
    break;

  case 967:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12949 "Parser/parser.cc"
    break;

  case 968:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12955 "Parser/parser.cc"
    break;

  case 969:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12961 "Parser/parser.cc"
    break;

  case 970:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12967 "Parser/parser.cc"
    break;

  case 971:
#line 3769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12973 "Parser/parser.cc"
    break;

  case 973:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12979 "Parser/parser.cc"
    break;

  case 974:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12985 "Parser/parser.cc"
    break;

  case 975:
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12991 "Parser/parser.cc"
    break;

  case 976:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12997 "Parser/parser.cc"
    break;

  case 977:
#line 3786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13003 "Parser/parser.cc"
    break;

  case 978:
#line 3788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13009 "Parser/parser.cc"
    break;

  case 979:
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13015 "Parser/parser.cc"
    break;

  case 980:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13021 "Parser/parser.cc"
    break;

  case 981:
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13027 "Parser/parser.cc"
    break;

  case 982:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13033 "Parser/parser.cc"
    break;

  case 983:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13039 "Parser/parser.cc"
    break;

  case 985:
#line 3822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13045 "Parser/parser.cc"
    break;

  case 986:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13051 "Parser/parser.cc"
    break;

  case 987:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13057 "Parser/parser.cc"
    break;

  case 988:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13063 "Parser/parser.cc"
    break;

  case 989:
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13069 "Parser/parser.cc"
    break;

  case 990:
#line 3835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13075 "Parser/parser.cc"
    break;

  case 991:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13081 "Parser/parser.cc"
    break;

  case 993:
#line 3843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13087 "Parser/parser.cc"
    break;

  case 994:
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13093 "Parser/parser.cc"
    break;

  case 995:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13099 "Parser/parser.cc"
    break;

  case 996:
#line 3852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13105 "Parser/parser.cc"
    break;

  case 997:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13111 "Parser/parser.cc"
    break;

  case 998:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13117 "Parser/parser.cc"
    break;

  case 999:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13123 "Parser/parser.cc"
    break;

  case 1000:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13129 "Parser/parser.cc"
    break;

  case 1001:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13135 "Parser/parser.cc"
    break;

  case 1002:
#line 3870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13141 "Parser/parser.cc"
    break;

  case 1004:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13147 "Parser/parser.cc"
    break;

  case 1005:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13153 "Parser/parser.cc"
    break;

  case 1007:
#line 3881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13159 "Parser/parser.cc"
    break;

  case 1008:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13165 "Parser/parser.cc"
    break;

  case 1010:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13171 "Parser/parser.cc"
    break;

  case 1011:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13177 "Parser/parser.cc"
    break;

  case 1012:
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13183 "Parser/parser.cc"
    break;

  case 1013:
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13189 "Parser/parser.cc"
    break;

  case 1014:
#line 3900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13195 "Parser/parser.cc"
    break;

  case 1015:
#line 3902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13201 "Parser/parser.cc"
    break;

  case 1016:
#line 3936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13207 "Parser/parser.cc"
    break;

  case 1019:
#line 3943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13213 "Parser/parser.cc"
    break;

  case 1020:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13219 "Parser/parser.cc"
    break;

  case 1021:
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13225 "Parser/parser.cc"
    break;

  case 1022:
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13231 "Parser/parser.cc"
    break;

  case 1023:
#line 3954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13237 "Parser/parser.cc"
    break;

  case 1024:
#line 3956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13243 "Parser/parser.cc"
    break;

  case 1025:
#line 3958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13249 "Parser/parser.cc"
    break;

  case 1026:
#line 3960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13255 "Parser/parser.cc"
    break;

  case 1028:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13261 "Parser/parser.cc"
    break;

  case 1029:
#line 3968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13267 "Parser/parser.cc"
    break;

  case 1030:
#line 3970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13273 "Parser/parser.cc"
    break;

  case 1031:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13279 "Parser/parser.cc"
    break;

  case 1032:
#line 3977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13285 "Parser/parser.cc"
    break;

  case 1033:
#line 3979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13291 "Parser/parser.cc"
    break;

  case 1035:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13297 "Parser/parser.cc"
    break;

  case 1037:
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13303 "Parser/parser.cc"
    break;

  case 1038:
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13309 "Parser/parser.cc"
    break;

  case 1039:
#line 4002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13315 "Parser/parser.cc"
    break;

  case 1040:
#line 4005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13321 "Parser/parser.cc"
    break;

  case 1041:
#line 4007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13327 "Parser/parser.cc"
    break;

  case 1042:
#line 4009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13333 "Parser/parser.cc"
    break;

  case 1044:
#line 4024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13339 "Parser/parser.cc"
    break;

  case 1045:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13345 "Parser/parser.cc"
    break;

  case 1046:
#line 4031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13351 "Parser/parser.cc"
    break;

  case 1047:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13357 "Parser/parser.cc"
    break;

  case 1048:
#line 4035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13363 "Parser/parser.cc"
    break;

  case 1049:
#line 4037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13369 "Parser/parser.cc"
    break;

  case 1050:
#line 4039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13375 "Parser/parser.cc"
    break;

  case 1052:
#line 4045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13381 "Parser/parser.cc"
    break;

  case 1053:
#line 4047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13387 "Parser/parser.cc"
    break;

  case 1054:
#line 4049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13393 "Parser/parser.cc"
    break;

  case 1055:
#line 4054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13399 "Parser/parser.cc"
    break;

  case 1056:
#line 4056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13405 "Parser/parser.cc"
    break;

  case 1059:
#line 4066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13411 "Parser/parser.cc"
    break;

  case 1062:
#line 4077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13417 "Parser/parser.cc"
    break;

  case 1063:
#line 4079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13423 "Parser/parser.cc"
    break;

  case 1064:
#line 4081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13429 "Parser/parser.cc"
    break;

  case 1065:
#line 4083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13435 "Parser/parser.cc"
    break;

  case 1066:
#line 4085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13441 "Parser/parser.cc"
    break;

  case 1067:
#line 4087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13447 "Parser/parser.cc"
    break;

  case 1068:
#line 4094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13453 "Parser/parser.cc"
    break;

  case 1069:
#line 4096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13459 "Parser/parser.cc"
    break;

  case 1070:
#line 4098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13465 "Parser/parser.cc"
    break;

  case 1071:
#line 4100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13471 "Parser/parser.cc"
    break;

  case 1072:
#line 4102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13477 "Parser/parser.cc"
    break;

  case 1073:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13483 "Parser/parser.cc"
    break;

  case 1074:
#line 4107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13489 "Parser/parser.cc"
    break;

  case 1075:
#line 4109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13495 "Parser/parser.cc"
    break;

  case 1076:
#line 4111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13501 "Parser/parser.cc"
    break;

  case 1077:
#line 4113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13507 "Parser/parser.cc"
    break;

  case 1078:
#line 4118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13513 "Parser/parser.cc"
    break;

  case 1079:
#line 4120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13519 "Parser/parser.cc"
    break;

  case 1080:
#line 4125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13525 "Parser/parser.cc"
    break;

  case 1081:
#line 4127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 13531 "Parser/parser.cc"
    break;

  case 1083:
#line 4154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13537 "Parser/parser.cc"
    break;

  case 1087:
#line 4165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13543 "Parser/parser.cc"
    break;

  case 1088:
#line 4167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13549 "Parser/parser.cc"
    break;

  case 1089:
#line 4169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13555 "Parser/parser.cc"
    break;

  case 1090:
#line 4171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13561 "Parser/parser.cc"
    break;

  case 1091:
#line 4173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13567 "Parser/parser.cc"
    break;

  case 1092:
#line 4175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13573 "Parser/parser.cc"
    break;

  case 1093:
#line 4182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13579 "Parser/parser.cc"
    break;

  case 1094:
#line 4184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13585 "Parser/parser.cc"
    break;

  case 1095:
#line 4186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13591 "Parser/parser.cc"
    break;

  case 1096:
#line 4188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13597 "Parser/parser.cc"
    break;

  case 1097:
#line 4190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13603 "Parser/parser.cc"
    break;

  case 1098:
#line 4192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13609 "Parser/parser.cc"
    break;

  case 1099:
#line 4197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13615 "Parser/parser.cc"
    break;

  case 1100:
#line 4199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13621 "Parser/parser.cc"
    break;

  case 1101:
#line 4201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13627 "Parser/parser.cc"
    break;

  case 1102:
#line 4208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13633 "Parser/parser.cc"
    break;

  case 1103:
#line 4210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13639 "Parser/parser.cc"
    break;

  case 1106:
#line 4234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13645 "Parser/parser.cc"
    break;

  case 1107:
#line 4236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13651 "Parser/parser.cc"
    break;


#line 13655 "Parser/parser.cc"

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
#line 4239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
