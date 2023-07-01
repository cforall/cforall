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
#define YYLAST   23439

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  178
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  309
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1107
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2242

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
    2706,  2705,  2716,  2720,  2724,  2728,  2733,  2734,  2739,  2741,
    2746,  2748,  2750,  2752,  2757,  2758,  2764,  2765,  2766,  2773,
    2774,  2776,  2777,  2778,  2780,  2782,  2789,  2790,  2792,  2794,
    2799,  2800,  2806,  2807,  2809,  2810,  2815,  2816,  2817,  2819,
    2827,  2828,  2830,  2833,  2835,  2839,  2840,  2841,  2843,  2845,
    2850,  2852,  2857,  2859,  2868,  2870,  2875,  2876,  2877,  2881,
    2882,  2883,  2888,  2889,  2894,  2895,  2896,  2897,  2901,  2902,
    2907,  2908,  2909,  2910,  2911,  2925,  2926,  2931,  2932,  2938,
    2940,  2943,  2945,  2947,  2970,  2971,  2977,  2978,  2984,  2983,
    2993,  2992,  2996,  3002,  3008,  3009,  3011,  3015,  3020,  3022,
    3024,  3026,  3032,  3033,  3037,  3038,  3043,  3045,  3052,  3054,
    3055,  3057,  3062,  3064,  3066,  3071,  3073,  3078,  3083,  3091,
    3096,  3098,  3103,  3108,  3109,  3114,  3115,  3119,  3120,  3121,
    3126,  3128,  3134,  3136,  3141,  3143,  3149,  3150,  3154,  3158,
    3162,  3164,  3177,  3179,  3181,  3183,  3185,  3187,  3189,  3190,
    3195,  3198,  3197,  3209,  3208,  3221,  3220,  3234,  3233,  3247,
    3246,  3262,  3268,  3270,  3276,  3277,  3288,  3295,  3300,  3306,
    3309,  3312,  3316,  3322,  3325,  3328,  3333,  3334,  3335,  3336,
    3340,  3346,  3347,  3357,  3358,  3362,  3363,  3368,  3373,  3374,
    3380,  3381,  3383,  3388,  3389,  3390,  3391,  3392,  3394,  3429,
    3431,  3436,  3438,  3439,  3441,  3446,  3448,  3450,  3452,  3457,
    3459,  3461,  3463,  3465,  3467,  3469,  3474,  3476,  3478,  3480,
    3489,  3491,  3492,  3497,  3499,  3501,  3503,  3505,  3510,  3512,
    3514,  3516,  3521,  3523,  3525,  3527,  3529,  3531,  3543,  3544,
    3545,  3549,  3551,  3553,  3555,  3557,  3562,  3564,  3566,  3568,
    3573,  3575,  3577,  3579,  3581,  3583,  3595,  3600,  3605,  3607,
    3608,  3610,  3615,  3617,  3619,  3621,  3626,  3628,  3630,  3632,
    3634,  3636,  3638,  3643,  3645,  3647,  3649,  3658,  3660,  3661,
    3666,  3668,  3670,  3672,  3674,  3679,  3681,  3683,  3685,  3690,
    3692,  3694,  3696,  3698,  3700,  3710,  3712,  3714,  3715,  3717,
    3722,  3724,  3726,  3731,  3733,  3735,  3737,  3742,  3744,  3746,
    3760,  3762,  3764,  3765,  3767,  3772,  3774,  3779,  3781,  3783,
    3788,  3790,  3795,  3797,  3814,  3815,  3817,  3822,  3824,  3826,
    3828,  3830,  3835,  3836,  3838,  3840,  3845,  3847,  3849,  3855,
    3857,  3860,  3863,  3865,  3869,  3871,  3873,  3874,  3876,  3878,
    3882,  3884,  3889,  3891,  3893,  3895,  3930,  3931,  3935,  3936,
    3938,  3940,  3945,  3947,  3949,  3951,  3953,  3958,  3959,  3961,
    3963,  3968,  3970,  3972,  3978,  3979,  3981,  3990,  3993,  3995,
    3998,  4000,  4002,  4016,  4017,  4019,  4024,  4026,  4028,  4030,
    4032,  4037,  4038,  4040,  4042,  4047,  4049,  4057,  4058,  4059,
    4064,  4065,  4070,  4072,  4074,  4076,  4078,  4080,  4087,  4089,
    4091,  4093,  4095,  4098,  4100,  4102,  4104,  4106,  4111,  4113,
    4115,  4120,  4146,  4147,  4149,  4153,  4154,  4158,  4160,  4162,
    4164,  4166,  4168,  4175,  4177,  4179,  4181,  4183,  4185,  4190,
    4192,  4194,  4201,  4203,  4221,  4223,  4228,  4229
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

#define YYPACT_NINF (-1824)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1106)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     150, 12716,   196,   241, 17568,   235, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,   210,   931,
     258, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,    46,   406,
   -1824, -1824, -1824, -1824, -1824, -1824,  5492,  5492,   287, 12716,
     315,   324, 23168, -1824,   338, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824, -1824, -1824,  4401, -1824,   786,   345, -1824,
   -1824, -1824, -1824, -1824, 17416, -1824, -1824,   336,   372,   586,
     260, -1824,  5492,   372,   372,   372,   378,  3705,   570,   924,
   12878, -1824, -1824,   537, 17264,  1499, -1824, -1824, -1824,  3739,
     625, 14197,  4365,  1254,  3739,  1333,   497, -1824, -1824, -1824,
   -1824,   596, -1824, -1824, -1824, -1824,   523, -1824, -1824, -1824,
   -1824, -1824,   553,   565,   596, -1824,   596,   663, -1824, -1824,
   -1824, 18418,  5492, -1824, -1824,  5492, -1824, 12716, -1824,   571,
   18570, -1824, -1824,  5279, 19991, -1824, -1824,  1085,  1085,   743,
    3334, -1824, -1824, -1824, -1824,   317, 14790,  3456,   596, -1824,
   -1824, -1824, -1824, -1824, -1824,   604, -1824,   681,   776,   779,
   -1824,   828, 22559, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
   16271,  3228,  4401,    31,   809,   824,   832,   834,   837,   857,
   -1824, -1824, 18722, 11727,   862, -1824,  9083, -1824, -1824, -1824,
   -1824,   893, -1824, -1824,   838, -1824,  6768,  1035,  9728, -1824,
     902,  5492,   565,   908,   911,   914,   919, -1824, -1824, -1824,
    3858,  3487,   960,   990,    58,   990, -1824,   596,   596,    78,
      88,   363,   990, -1824,   596,   596,    78,   596, -1824,   596,
   -1824,  4849, -1824, -1824,   973,   996,  1085, 20187, -1824, 17416,
   -1824, -1824,  3739, -1824,  2582,   497,  1007,  1067,    88,  5492,
    5492,   586, -1824, 14307, -1824,  1085,  1085,  1051,  1067,    88,
    5492, -1824,  9308, -1824, -1824, -1824,  1085, -1824, -1824, -1824,
   -1824,  1085, -1824,   842,  5093,  5492, -1824,  3000,  1028, -1824,
   -1824, -1824, 17165,   565,   142, -1824, -1824, 20136, -1824,   990,
      54, -1824, 22559, 19991,  4066,  4849, -1824,   415, -1824, -1824,
   -1824, -1824, -1824, 18570,  5492, -1824,  1057, -1824, -1824, -1824,
   -1824,  5492,  3919,   516,   423, -1824,  5492,   681, -1824,   844,
     596,   596,  1068, 18874,   762, 15273, 20689,  3739,  3739, -1824,
    3739,  1085,  3739,  1085, -1824, -1824,   596, -1824,  1054, -1824,
   19026, -1824, -1824, -1824, 19178,   893, -1824,   419,   735,   208,
    1075,   585,   497,  1079, -1824,  3334,  1052,   681,  3334,  2872,
   -1824,  1100,  1138, 22633,  1107,  1122,  1127, 22559, 22707,  1129,
   23272, -1824, -1824, -1824, -1824, -1824, -1824, 22781, 22781, 16115,
    1128,  5429, -1824, -1824, -1824, -1824,   295, -1824,   629, -1824,
    1645, -1824, 22559, 22559, -1824,  1139,   651,   866,   953,   608,
     979,  1124,  1159,  1123,  1192,    95, -1824,   759, -1824,  1204,
   -1824,   937,  3276, 16583, -1824, -1824,   787,  1204, -1824, -1824,
     815, -1824, -1824,  3228,  1214,  1232,  1240,  1266,  1269,  1273,
   -1824, -1824,   444,  1252, -1824,   853,  1252, -1824, -1824, 18418,
   -1824,   981,  1272, 16739, -1824, -1824,  5408,  5059,  1297, 15273,
    1300,   796,   951, -1824, -1824, -1824, -1824, -1824,  5492,  5459,
   -1824, -1824, -1824, -1824, -1824, -1824, 17060,  4342,  1128,  6768,
    1285,  1325, -1824, -1824,  1309,  9728,   802, -1824, -1824, -1824,
   20857,  1319, -1824, -1824, -1824, -1824, -1824,  3858,   894,  1335,
    1360,  1362,   899,  1372,  1374,  1385,  1396,  1398,  1400,  3487,
   -1824, -1824, -1824,   596,  1406,  1311,  1408, -1824, -1824,  1412,
     586, -1824, -1824,   565,  1067, -1824, -1824, -1824,   586, -1824,
   -1824,   565, -1824, -1824,  4849, -1824, 16583, 16583, -1824,  1085,
    5279, 10316, 15434, -1824, -1824, -1824, -1824, -1824,   565,  1067,
      54,  1413, -1824, -1824,  3739,  1415,  1067,    88, -1824,   565,
    1067, -1824, 23367, -1824,  1085,  1085, -1824, -1824,  1417,   699,
    1423,   497,  1426, -1824, 17729, -1824,   852, -1824,  1517, 20586,
   -1824,  5279,  7620, 20187, -1824, 17165, 22855, -1824, -1824, -1824,
   -1824, -1824,  4066,   909,  4849, -1824, 15434,   990, 12716, -1824,
    1432, -1824,  1438, -1824, -1824, -1824, -1824, -1824,  3334, -1824,
   -1824,  1513,  5291,  2963, 19178, 11727, -1824, 19330, -1824,  1085,
    1085, -1824, -1824,   893, -1824,   964,  1435,  1575, 22559,  1002,
    1412,  1420, -1824,   596,   596, -1824,  1252, -1824, 18874, -1824,
   -1824, 18015,  1085,  1085, -1824,  5291,   596, -1824, 19846, -1824,
   -1824, 19026, -1824,   317, -1824, -1824, -1824,  1440,  5492,   208,
    1079,  1444,   912, 18570,   949, -1824, -1824, -1824, -1824, -1824,
   -1824,   950, -1824,  1448,  1431, -1824, 16427, -1824,  5429, 19482,
   19482, -1824, 16427, -1824, 22559, -1824, -1824, -1824, -1824, -1824,
   -1824, 16427, -1824, -1824, 18114, 19482, 19482,   937,  1349,  1478,
     618,  1520, -1824,  1001,  1459,   998,  1460, -1824, 20857, 22559,
   20931,  1457, 22559,  3000, 22559,  3000, -1824,  3169, -1824, -1824,
   21005,  2614, 22559, 21005,  3000, -1824, -1824, 22559, 22559, 22559,
   22559, 22559, 22559, 22559, 22559, 22559, 22559, 22559, 22559, 22559,
   22559, 22559, 22559, 22559, 22559, 22559, 21079,  1442,   828,  4931,
   11727, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824,  1461, 22559, -1824, -1824,   787,  2211, -1824, -1824,
     596,   596, -1824, -1824, 16583, -1824,   504,  1252, -1824,  1024,
    1252, -1824, -1824, -1824,  1412, -1824, -1824,  1412, 22929, -1824,
   -1824, 11727,  1463,  1466,  4637,  1609,  2927,   522,  1420, -1824,
     596,   596,  1420,   544, -1824,   596,   596, 22559,  5492,  1016,
    1036,  1420,   216, 14629, 14629,  5492, -1824, -1824, 22559,  1309,
   -1824,  6768,  1479, -1824,  2140, -1824, -1824, -1824, -1824, -1824,
    1031, -1824, 14629,  3000,  5279,  3000,  1066,  1477,  1485,  1487,
    1074,  1488,  1489,  1491,  1493,  1494,  1495,   554,  1252, -1824,
   -1824,   564,  1252, -1824, -1824,   587,  1252, -1824, -1824, -1824,
    5279,   828,  1610,  1252, 20281, -1824, -1824,   565, 17729, -1824,
   -1824, -1824,  1073,  1497,  1093,  1501, -1824,  1505, -1824,   565,
   -1824,  1506, -1824,   565,  1067,  1505, -1824,   565,  1498,  1502,
    1503, -1824, -1824, 18015, -1824,  1510, -1824, -1824, -1824,  3000,
    5492, 10871,  1589,  1490, -1824, -1824, 19643, -1824,  1272, -1824,
   14629,  1120, -1824, -1824,  1505, -1824, 18570, 16583,  1492, -1824,
    1492, -1824, -1824, -1824,   208,  1509,   596,   596, -1824, 19026,
   -1824, 11892, 16895, -1824, 17729,  1523,  1526,  1527, -1824,  7798,
     596, -1824,  1002, -1824, -1824, -1824, -1824,  1412, -1824, -1824,
   -1824,  1085, -1824,  3575, -1824, -1824,   497,   111,  1532,  1508,
    1440,  1525,   208, -1824, -1824,  1528,  1533,  2872, 21005, -1824,
    1535,  1534,   345,  1536,  1539,  1542,  1541,  1543, 22559,  1549,
    1550,  1551, 11727, 22559, -1824, -1824,  1531, -1824, -1824, -1824,
   22559, -1824,  1553,  1555, 20783,  1042, -1824, 21005,  1529, -1824,
    1554, -1824, -1824,  3824, -1824, -1824,  1119, -1824, -1824, -1824,
   -1824,  3824, -1824, -1824,  1048,   695, -1824, -1824,  1139,  1139,
    1139,   651,   651,   866,   866,   953,   953,   953,   953,   608,
     608,   979,  1124,  1159,  1123,  1192, 22559,  1061, -1824,  1558,
    3824, -1824, -1824,  6768, -1824, 17729,  1561,  1562,  1563,  2211,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,  1412, -1824,
   -1824,  1412, 17729, 17729, -1824, -1824,  4637,   963,  1567,  1569,
    1571,  1572,  3068,  2927, -1824, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,  1570, -1824,
    1420, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,  1574,
    1576, -1824,   586,  3824,  1091,    29, -1824, -1824,  1578, -1824,
    9728, -1824, 22559,   596, 21153, 14629, -1824, -1824, -1824,  1514,
     599,  1252, -1824,   614,  1252, -1824, -1824,   621,  1252, -1824,
   -1824, -1824,  1412, -1824, -1824, -1824,  1412, -1824, -1824, -1824,
    1412,   990,  1577, -1824,  1412,   333, -1824,  1204,  1579, -1824,
   -1824, -1824, -1824, -1824, -1824,  1584, -1824, -1824, -1824, 18570,
    1505, -1824,   565, -1824, -1824, -1824, -1824, -1824, 13519,  1582,
    1583, -1824,   175, -1824,   473,   387, 11562,  1588, 15794,  1590,
    1591,  3207,  3426,  1919, 21227,  1592, -1824, -1824,  1593,  1594,
   -1824, -1824,   565, 22559, 22559,  1734,  1595,   712, -1824, 15959,
    1675,  1597,  1573, -1824, -1824, -1824, 10696, -1824, -1824, -1824,
   -1824, -1824,  3083, -1824, -1824, -1824,  1251,   177, -1824,   213,
   -1824,   177, -1824, -1824, -1824,  3000, -1824, -1824, 13040, 17416,
    1600, -1824,  5492, -1824,  1580,  1596,  1599, -1824,  1196, -1824,
   -1824, -1824, -1824,  5279, -1824, -1824,  1586,  1601,  1135, 18570,
     681,   681,  1440,   208,  1079,  1079, -1824, -1824,  1128,  1272,
   16739, -1824,  1204, -1824, 12057, -1824,   693,  1252, -1824,  1085,
    9412, -1824, -1824,   208,  1605,   596,   596,   317,  5492, -1824,
   21301, -1824,  1625,   208,  1440,  1630, -1824, -1824,  1141,   723,
   18015, 11727,  3000, -1824,   723, 18266,   723, -1824, 22559, 22559,
   22559, -1824, -1824, -1824, -1824, 22559, 22559,  1622,  6768, -1824,
   -1824,  1603,   731, -1824, -1824, -1824,  4124, -1824, -1824,  1239,
   -1824,   334, -1824, 21005,  1249, -1824, 20857, -1824, -1824, 22559,
    1606,  1256,  1260,  1309, -1824,   696,  1252, -1824, -1824, 17729,
   17729, -1824, -1824,  1631,   729,  1252, -1824,   760,  2682,   596,
     596, -1824, -1824, 17729, 17729, -1824,  1633, -1824, 15434, 15434,
    1634,  1629,  1635,  1638, -1824,  1636, 22559, 22559,  1283,  1639,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824,  1644, 22559, -1824,
   -1824, -1824,  1412, -1824, -1824, -1824,  1412, -1824, -1824, -1824,
    1412, 17729, 17729, 17729,   586,   596, -1824, -1824,  1294, 22559,
   20430,  1646,  1648,  1658, -1824, -1824, -1824,  1662, 13674, 13829,
   13984, 18570, 20187, 19482, 19482,  1664, -1824,  1637,  1642,  2585,
    9886, -1824,   466,  5492, -1824, -1824,  5492, -1824, 21005,    62,
     276, -1824, -1824, -1824, -1824, 22559,  1668,  1672, 11396, 11046,
   -1824,  1647, -1824,  1650, 22559,  1651,  6768,  1653, 22559, 20857,
   22559,  1259, -1824,  1654,    26, -1824,    90,  1729,   262,  1670,
   -1824, -1824,  1681, -1824,  1656, -1824,  1657,  1684,  1685, 15794,
   15794, -1824, -1824,  1750, -1824, -1824,   145,   145,   534, 14468,
     596,   472, -1824, -1824, -1824,  1687, -1824,  1692, -1824,  1693,
   -1824,  1690, -1824,  1691, -1824, -1824, -1824, -1824,  1701,  1440,
    1695,  1700, 12222,  1696,  1704,  1705, -1824,  1710, -1824, -1824,
   -1824,  1412, 22559, 22559,  1272,  1708, -1824,  1440,   208, -1824,
    1079,   251,  1508,  6768, -1824, -1824,  1440,  1717, -1824, 18570,
   -1824,  1019,  1715,  1711,  1142, -1824,  1713, -1824, -1824, -1824,
   -1824, -1824,  6768,  1309, 20857, -1824,  1754,  3824, -1824,  1754,
    1754, -1824,  3824,  4795,  5116, -1824, -1824,  1296, -1824, -1824,
   -1824,  1725,  1723, -1824, -1824, -1824,  1412, -1824, -1824,  1724,
    1726,   596, -1824, -1824, -1824,  1412, -1824, -1824, -1824,  1731,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824,  1730, -1824, -1824, -1824, -1824,  1735,  1732,
     596, -1824, 17729, 17729, 17729, -1824, -1824, -1824, -1824, -1824,
   22559, -1824,   333, -1824,  1204, -1824, -1824, -1824,  1737,  1740,
   -1824,  1664,  1664,  1664,  4622,  1032,  1716,   487, -1824,  4622,
     512, 16583, -1824, -1824, -1824,  4099, 22559,  4919,   375, -1824,
   -1824,    93,  1736,  1736,  1736,  5492, -1824, -1824, 17881, -1824,
    1151, -1824, -1824, -1824, -1824,  1153,  1742, 15794,  1597,  1745,
   22559,   336,  1738,   378, 14146, 18570, -1824, -1824, -1824,   836,
   15794, 22559,  1055,   640, -1824, 22559,  8087, -1824, -1824,   546,
   -1824,  1309, -1824,  1160,  1162,  1165, -1824, -1824, -1824, -1824,
     565,  1259,  1748, -1824, -1824, 22559, -1824,  1749,   828, -1824,
   11562, -1824, -1824, -1824, -1824, 22559, 22559, -1824, -1824,   421,
     145, -1824,   453, -1824, -1824, 10521, -1824,   596, 15434, -1824,
   -1824, 18570, -1824, -1824, -1824,  1751,   208,   208, -1824, -1824,
   -1824,  1746, -1824, 17729, -1824, -1824,  1747, -1824,  1753,  1755,
    1440,  1079,  1752, -1824, -1824,  1309,  1756, -1824, -1824,  1757,
   -1824, -1824, 22559, -1824, 18266, 22559,  1309,  1762,  1302, -1824,
    1305, -1824,  3824, -1824,  3824, -1824, -1824, -1824, -1824, 17729,
    1761,  1763, -1824, -1824, 17729, 17729,  1764,  1765,  1308, 14951,
   15112, -1824,  1767, -1824, -1824, -1824, -1824, -1824,  1766,  1771,
    1772,  1312, 22559, -1824, -1824, -1824, -1824, -1824,   551,  1032,
    2427,   576, -1824, -1824, -1824, -1824,   596,   596, -1824, -1824,
   -1824,   609, -1824,  1179,  4099,   716, -1824,  4919, -1824,   596,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, 15794,
     304, 21375,  1853, 15794,  1597, 15595, -1824, -1824, -1824, -1824,
   22559, -1824, 21449,  1856,  1768, 20706, 21523, 15794, 11221,  1597,
     467,  1136,  1775, 22559, -1824,  1787,   480, 15794, -1824, -1824,
    1789, -1824, -1824,  1776,   828,   745,  1785,  1788,  1315,  1180,
   15794,  1792, 15794, 15794, 15794, 15794, -1824, -1824, -1824, -1824,
    5492,  5279, -1824,  1440,  1440, -1824, -1824,  1797,  1799, -1824,
   -1824, -1824,  1798,  1800,   208,  1807, -1824,  1808, -1824, -1824,
   -1824, -1824,  1812, -1824, -1824, -1824,  1318,  1323, -1824, -1824,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,  1810, -1824,
   -1824,  1811,  1813,  1814, -1824, -1824, -1824, -1824, -1824, -1824,
    1815,  1816,  1817,  2427, -1824,   596, -1824, -1824, -1824, -1824,
   -1824,  1818,  4622, -1824,  6183,    96, 12390, -1824, 15689, -1824,
      80,  1186, 15794,  1897,   648,  1820,   380, 15794, 22559,  1823,
     467,  1136,  1804, 23003,  1821,   425,  1900, -1824, 21597, 21671,
   22559,  1597,  1809, 12554, -1824, -1824, -1824, 19694, -1824,  1825,
    1824,    57, 15794, -1824, 22559, 21005, -1824, -1824, 22559,   177,
   -1824, -1824,   177, -1824, -1824,  1834,  1841,  1843, -1824, -1824,
   -1824,   208,  1440, -1824, -1824, -1824, -1824, -1824,  1844,  1848,
    1849, 15434,  1837, -1824, -1824, -1824,   785,  1252, -1824, -1824,
    1032, -1824, -1824,   157, -1824,   181, -1824, -1824, -1824,  1855,
   13202, -1824, -1824, 15794, -1824,    82, -1824, 15794, 22559,  1851,
   21745, -1824, -1824, 21819, 21893, 22559,  1823,  1597, 21967, 22041,
   15794,  1839,   461,  1840,   469, -1824, -1824,  1862, 13202, 19694,
   -1824,  5152, 19330,  3000,  1857, -1824,  1910,  1865,   746,  1860,
   -1824,  1944, -1824,  1197,  1202,   417,   433, -1824, -1824, -1824,
    1440,  1869, -1824, -1824, -1824, -1824, -1824, -1824, -1824,  1412,
   -1824, 22559, -1824, 22559, -1824, -1824,  1405, 13364, -1824, -1824,
   15794, -1824, -1824,  1597, -1824, -1824,  1597,  1854,   549,  1867,
     709, -1824, -1824,  1597, -1824,  1597, -1824,  1868, 22115, 22189,
   22263, -1824,  1405, -1824,  1866,  3608,  3979, -1824, -1824, -1824,
      57,  1882, 22559,  1870,    57,    57, 15794, -1824, -1824, 15794,
    1970, 15794,  1976,  1902, -1824, 17729, -1824, -1824, 15689, -1824,
    1405, -1824, -1824,  1901, 22337, 22411, 22485, -1824, -1824,  1597,
   -1824,  1597, -1824,  1597, -1824,  1866, 22559,  1904,  3979,  1903,
     828,  1906, -1824,   761, -1824, -1824, -1824, 15794, -1824, 15794,
   -1824, -1824, -1824, 10160,  1912, 15689, -1824, -1824,  1597, -1824,
    1597, -1824,  1597,  1913,  1915, -1824,   565,   828,  1922, -1824,
    1894,   828, -1824, -1824,  1924, -1824, -1824, -1824, 10399, -1824,
     565, -1824, -1824,  1340, 22559, -1824,  1222, -1824, -1824,   828,
    3000,  1925,  1914, -1824, -1824,  1230, -1824, -1824,  1916,  3000,
   -1824, -1824
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
     821,   487,     0,     0,   843,   881,   843,     2,   898,   899,
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
       2,   843,   841,   919,   843,   843,   464,   843,   938,   843,
     916,     0,   556,   557,     0,     0,   482,   482,     2,   482,
     437,   485,   495,   549,     0,   578,     0,   824,     2,     0,
       0,   724,   438,   563,   542,   559,   574,     0,   824,     2,
       0,   498,   543,   550,   551,   469,   560,   471,   472,   470,
     565,   575,   579,     0,   593,     0,   794,     2,     2,   822,
     880,   882,   482,     0,     2,     2,  1095,   563,  1098,   841,
     841,     3,     0,   563,     0,     0,   448,   843,   836,   838,
     837,   839,     2,   482,     0,   798,     0,   758,   760,   759,
     761,     0,     0,   754,     0,   744,     0,   753,   764,     0,
     843,   843,     2,   482,  1106,   483,   482,   494,   473,   541,
     474,   566,   475,   573,   570,   591,   843,   592,     0,   705,
     482,   706,  1060,  1061,   482,   707,   709,   580,   586,   666,
       0,   668,   669,   666,   846,     0,   775,   763,     0,   850,
      22,     0,    21,     0,     0,     0,     0,     0,     0,     0,
      24,    26,     4,     8,     5,     6,     7,     0,     0,   482,
       2,     0,   104,   105,   106,   107,    88,    25,    89,    43,
      87,   108,     0,     0,   123,   125,   129,   132,   135,   140,
     143,   145,   147,   149,   151,   153,   156,     0,    27,     0,
     587,     2,   108,   482,   157,   769,   720,   577,   722,   768,
       0,   719,   723,     0,     0,     0,     0,     0,     0,     0,
     860,   886,   843,   896,   904,   908,   914,     2,  1093,   482,
    1096,     2,   101,   482,     3,   704,     0,  1106,     0,   483,
     541,   566,   573,     3,     3,   686,   690,   700,   706,   707,
       2,   889,   907,  1083,     2,     2,    24,     0,     2,   730,
      25,     0,   728,   731,  1104,     0,     0,   737,   726,   725,
       0,     0,   826,     2,     2,     2,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     865,   922,   945,   843,     0,   461,     2,   861,   869,  1003,
     724,   863,   864,     0,   824,     2,   918,   926,   724,   920,
     921,     0,   937,   939,     0,   451,   482,   482,   547,   483,
       0,   563,   482,  1088,  1092,  1090,   564,   798,     0,   824,
     841,     0,   431,   439,   496,     0,   824,     2,   798,     0,
     824,   773,   544,   545,   561,   576,   582,   585,   580,   586,
     604,   605,     0,   774,   482,   714,     0,   203,   414,   482,
       3,     0,   563,   482,   823,   482,     0,   433,     2,   434,
     795,   453,     0,     0,     0,     2,   482,   841,   482,   798,
       0,     2,     0,   757,   756,   755,   750,   493,     0,   748,
     765,   539,     0,     0,   482,   482,  1062,   483,   479,   480,
     481,  1066,  1057,  1058,  1064,     2,     2,   102,     0,  1022,
    1036,  1106,  1018,   843,   843,  1027,  1034,   712,   482,   571,
     708,   483,   567,   568,   572,     0,   843,  1072,   483,  1077,
    1069,   482,  1074,     0,   675,   667,   674,  1104,     0,   666,
     666,     0,     0,   482,     0,   858,   857,   853,   855,   856,
     854,     0,   848,   851,     0,    23,   482,    95,     0,   482,
     482,    90,   482,    97,     0,    33,    37,    38,    34,    35,
      36,   482,    93,    94,   482,   482,   482,     2,   104,   105,
       0,     0,   183,     0,     0,   607,     0,  1082,     0,     0,
       0,     0,     0,     0,     0,     0,    56,     0,    62,    63,
      67,     0,     0,    67,     0,    91,    92,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     482,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   164,     0,   162,   163,     2,   987,   721,   984,
     843,   843,   992,   588,   482,   887,   843,   897,   905,   909,
     915,     2,   890,   892,   894,     2,   910,   912,     0,  1094,
    1097,   482,     0,     0,     2,   102,  1022,   843,  1106,   957,
     843,   843,  1106,   843,   972,   843,   843,     3,   708,     0,
       0,  1106,  1106,   482,   482,     0,     2,   739,     0,  1104,
     736,  1105,     0,   732,     0,     2,   735,   738,   180,   179,
       0,     2,   482,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   843,   874,   878,
     917,   843,   931,   935,   943,   843,   953,   866,   923,   946,
       0,     0,     0,   999,     0,   459,   827,     0,   482,   460,
     828,   452,     0,     0,     0,     0,   450,     2,   829,     0,
     435,     2,   798,     0,   824,     2,   830,     0,     0,     0,
       0,   619,   693,   483,     3,     3,   697,   696,   901,     0,
       0,   482,   415,     0,   465,   464,   563,     3,   101,     3,
     482,     0,     3,   799,     2,   752,   482,   482,   746,   745,
     746,   540,   538,   668,   666,     0,   843,   843,  1068,   482,
    1073,   483,   482,  1059,   482,     0,     0,     0,  1037,     0,
     843,  1107,  1023,  1024,   713,  1020,  1021,  1035,  1063,  1067,
    1065,   569,   604,     0,  1071,  1076,   671,   666,     0,   676,
    1104,     0,   666,   778,   776,     0,     0,   850,    67,   810,
       0,     0,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   482,     0,   122,   121,     0,   118,   117,    28,
       0,    29,     0,     0,     0,     0,     3,    67,     0,    52,
       0,    53,    60,     0,    59,    71,     0,    68,    69,    72,
      55,     0,    54,    58,     0,     0,    51,   124,   126,   127,
     128,   130,   131,   133,   134,   138,   139,   136,   137,   141,
     142,   144,   146,   148,   150,   152,     0,     0,   424,     0,
       0,    30,     3,   730,   158,   482,     0,     0,     0,   988,
     989,   985,   986,   771,   770,     2,   891,   893,   895,     2,
     911,   913,   482,   482,  1013,  1012,     2,     0,     0,     0,
       0,     0,   843,  1023,   960,   977,     2,   955,   963,   710,
     958,   959,   711,     2,   970,   980,   973,   974,     0,     3,
    1106,   443,     2,  1099,     2,   701,   702,   680,     3,     3,
       3,     3,   724,     0,   156,     0,     3,     3,     0,   733,
       0,   727,     0,   843,     0,   482,     3,   447,   449,     0,
     843,   875,   879,   843,   932,   936,   944,   843,   954,     2,
     867,   870,   872,     2,   924,   927,   929,     2,   947,   949,
     951,   841,     0,   462,  1000,     3,  1004,  1005,     3,   832,
       3,   553,   552,   555,   554,     2,   799,   833,   780,   482,
       2,   831,     0,   799,   834,   619,   619,   619,   482,     0,
       0,   715,     0,   418,     0,     0,   482,     0,   338,     0,
       0,     0,     0,     0,   185,     0,   333,   334,     0,     0,
     387,   386,     0,   160,   160,   393,   580,   586,   200,   482,
       0,   186,     0,   211,   187,   188,   482,   205,   189,   190,
     191,   192,     0,   193,   194,   339,     0,   353,   195,   359,
     361,   367,   196,   197,   198,     0,   199,   207,   563,   482,
       0,   209,     0,   412,     0,     0,     0,     3,     0,   812,
     799,   787,   788,     0,     3,   783,     3,     3,     0,   482,
     762,   762,  1104,   666,   666,   666,  1070,  1075,     2,   101,
     482,     3,   578,     3,   483,     3,   843,  1030,  1033,   482,
       3,  1019,  1025,   666,     0,   843,   843,     0,     0,   651,
       0,   670,     0,   666,  1104,     2,   847,   849,     0,    96,
     482,   482,     0,   100,    98,   482,     0,   112,     0,     0,
       0,   116,   120,   119,   184,     0,     0,     0,   730,   109,
     177,     0,     0,    46,    47,    85,     0,    85,    85,     0,
      73,    75,    49,     0,     0,    45,     0,    48,   155,     0,
       0,     0,     0,  1104,     3,   843,   995,   998,   990,   482,
     482,     3,     3,     0,   843,   966,   969,   843,     0,   843,
     843,   961,   978,   482,   482,  1100,     0,   703,   482,   482,
       0,     0,     0,     0,   432,     3,     0,     0,     0,     0,
     729,   734,     3,   825,   182,   181,     3,     0,     0,     2,
     868,   871,   873,     2,   925,   928,   930,     2,   948,   950,
     952,   482,   482,   482,   724,   843,  1011,  1010,     0,     0,
       0,     0,     0,     0,     3,   799,   835,     0,   482,   482,
     482,   482,   482,   482,   482,   602,   632,     3,     3,   633,
     563,   620,     0,     0,   883,     2,     0,   416,    67,     0,
       0,   324,   325,   208,   210,     0,     0,     0,   482,   482,
     320,     0,   318,     0,     0,     0,   730,     0,     0,     0,
       0,     0,   161,     0,     0,   394,     0,     0,     0,     0,
       3,   215,     0,   206,     0,   315,     0,     0,     0,   338,
     338,   344,   343,   338,   355,   354,   338,   338,     0,   563,
     843,     0,   413,  1015,  1014,     0,     2,     0,   790,     2,
     785,     0,   786,     0,   766,   747,   751,   749,     0,  1104,
       0,     0,   482,     0,     0,     0,     3,     0,     2,  1026,
    1028,  1029,     0,     0,   101,     0,     3,  1104,   666,   660,
     666,   676,   676,   730,   677,   652,  1104,     0,   779,   482,
     852,  1016,     0,     0,     0,    39,     0,   113,   115,   114,
     111,   110,   730,  1104,     0,    66,    82,     0,    76,    83,
      84,    61,     0,     0,     0,    70,    57,     0,   154,   423,
      31,     0,     0,     2,   991,   993,   994,     3,     3,     0,
       0,   843,     2,   962,   964,   965,     2,   979,   981,     0,
     956,   971,     3,     3,  1101,     3,   688,   687,   691,  1103,
       2,     2,  1102,     0,     3,   840,   740,   741,     0,     0,
     843,   454,   482,   482,   482,     3,     3,     3,   463,   842,
       0,  1006,     0,  1007,  1008,  1002,   940,   816,     2,     0,
     818,   602,   602,   602,   633,   640,   607,     0,   646,   633,
       0,   482,   594,   631,   627,     0,     0,     0,     0,   634,
     636,   843,   648,   648,   648,     0,   628,   644,   482,   419,
       0,   328,   329,   326,   327,     0,     0,   338,   225,     0,
       0,   227,   427,   226,   563,   482,   306,   305,   307,     0,
     338,   185,   265,     0,   258,     0,   185,   321,   319,     0,
     313,  1104,   322,     0,     0,     0,   375,   376,   377,   378,
       0,   368,     0,   369,   330,     0,   331,     0,     0,   358,
     482,   216,   204,   317,   316,     0,     0,   347,   357,     0,
     338,   360,     0,   362,   385,     0,   417,   843,   482,   814,
     767,   482,     2,     2,   658,     0,   666,   666,  1078,  1079,
    1080,     0,  1031,   482,     3,     3,     0,  1039,     0,     0,
    1104,   666,     0,   673,   672,  1104,     0,   655,     3,     0,
    1017,    99,     0,    32,   482,     0,  1104,     0,     0,    86,
       0,    74,     0,    80,     0,    78,    44,   159,   996,   482,
       0,     0,   884,   902,   482,   482,     0,     0,     0,   482,
     482,   743,     0,   440,   442,     3,     3,     3,     0,     0,
       0,     0,     0,   782,   820,   598,   600,   596,     0,     0,
    1046,     0,   641,  1051,   643,  1043,   843,   843,   626,   647,
     630,     0,   629,     0,     0,     0,   650,     0,   622,   843,
     621,   637,   649,   638,   639,   645,   695,   699,   698,   338,
       0,     0,   246,   338,   228,   563,   311,   309,   312,   308,
       0,   310,     0,   254,     0,   185,     0,   338,   482,   266,
       0,   291,     0,     0,   314,     0,     0,   338,   337,   379,
       0,   370,     2,     0,     0,     0,     0,   340,     0,     0,
     338,     0,   338,   338,   338,   338,   202,   201,   441,   784,
       0,     0,   659,  1104,  1104,  1081,     3,     0,     0,  1038,
    1040,   656,     0,     0,   666,     0,   654,     2,    50,    42,
      40,    41,     0,    64,   178,    77,     0,     0,     3,   885,
     903,     3,     3,   967,   982,   444,     2,   685,     3,   684,
     742,     0,     0,     0,   876,   933,   941,  1001,  1009,   624,
       0,     0,     0,  1047,  1048,   843,   625,  1044,  1045,   623,
     603,     0,     0,   336,     0,     0,     0,   239,   338,   217,
       0,     0,   338,   248,   263,   274,   268,   338,   185,   303,
       0,   278,     0,     0,   269,   267,   256,   259,     0,     0,
     185,   292,     0,     0,   220,   335,     2,   482,   332,     0,
       0,   395,   338,   345,     0,    67,   356,   349,     0,   350,
     348,   363,   364,   789,   791,     0,     0,     0,  1041,  1042,
     657,   666,  1104,   678,   781,    65,    81,    79,     0,     0,
       0,   482,     0,   877,   934,   942,   843,  1054,  1056,  1049,
       0,   635,   234,   229,   232,     0,   231,   238,   237,     0,
     482,   241,   240,   338,   250,     0,   247,   338,     0,     0,
       0,   255,   260,     0,     0,   185,   304,   279,     0,     0,
     338,     0,   294,   295,   293,   262,   323,     0,   482,   482,
       3,   380,   483,   384,     0,   388,     0,     0,     0,   396,
     397,   223,   341,     0,     0,     0,     0,   662,   664,  1032,
    1104,     0,   997,   968,   983,   689,     2,  1050,  1052,  1053,
     642,     0,   236,     0,   235,   219,   242,   482,   408,   251,
     338,   252,   249,   264,   277,   275,   271,   283,   281,   282,
     280,   261,   276,   272,   273,   270,   257,     0,     0,     0,
       0,   222,   242,     3,   373,     0,  1046,   381,   382,   383,
     395,     0,     0,     0,   395,     0,   338,   346,   342,   338,
       0,   338,     0,     0,   663,   482,   230,   233,   338,     3,
     243,   409,   253,     0,     0,     0,     0,   302,   300,   297,
     301,   298,   299,   296,     3,   373,     0,     0,  1047,     0,
       0,     0,   389,     0,   398,   224,   351,   338,   365,   338,
     661,     3,   212,     0,     0,   338,   290,   288,   285,   289,
     286,   287,   284,     0,     0,   374,     0,   401,     0,   399,
       0,   401,   352,   366,     0,   214,   213,   218,     0,   221,
       0,   371,   402,     0,     0,   390,     0,  1055,   372,     0,
       0,     0,     0,   403,   404,     0,   400,   391,     0,     0,
     392,   405
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1824,  6714,  5543, -1824,    -1,   418,  2105,   -56, -1824,  -367,
   -1824,   319, -1824,  -717, -1824,   748,  -911, -1177, -1824,   173,
    2485,  1846, -1824,  -186, -1824,  1377,   286,   777,   816,   656,
     771,  1339,  1342,  1347,  1338,  1346, -1824,    22,   -46,  8672,
     891, -1824,  1679, -1824, -1824,  -681,  7698, -1164,  3136, -1824,
    1934, -1824,   886,   -83, -1824, -1824, -1824,   403,    12, -1824,
   -1750, -1727,   254,   -14, -1824, -1824, -1824,   263, -1569, -1824,
   -1401, -1824, -1824, -1824, -1824,  -555, -1140, -1824,   400, -1204,
     408, -1824, -1824, -1824, -1824, -1824,    83, -1176, -1824, -1824,
   -1824,   -44,   428,   432,    67, -1824, -1824, -1824, -1824,  -755,
   -1824,    -7,   -72, -1824,    72, -1824,  -179, -1824, -1824, -1824,
     904,  -513, -1092, -1360, -1824,     3, -1276,    64,  3998,  -989,
    -935, -1824,  -288, -1824, -1824,    69, -1824,  -154,   252,   139,
    -236,  3698,   484,  -639,     0,    74,   561,  1994,   350, -1824,
    2092, -1824,   153,  3706, -1824,  2035, -1824,   136, -1824, -1824,
    2154,   270,  4537,  2704,   -41,  1888,  -293, -1824, -1824, -1824,
   -1824, -1824,  -500,  5493,  5323, -1824,  -389,   202, -1824,  -694,
     211, -1824,   124,   738, -1824,   -82,  -262, -1824, -1824, -1824,
    -332,  6077,  -196,  1198,    47,  -684,  -557,  -560,  2113, -1824,
   -1320,  -162,   890,  -575,   932,  4518,  -165,  -480,  -254,  -197,
    -462,  1337, -1824,  1676,   -86,  1236,  1556, -1824, -1824, -1824,
   -1824,   321,  -169,  -153,  -914, -1824,   309, -1824, -1824, -1118,
     449, -1824, -1824, -1824,  2182,  -786,  -464, -1004,     4, -1824,
   -1824, -1824, -1824, -1824, -1824,   320,  -797,  -226, -1823,  -122,
    7187,   -69,  7225, -1824,  1207, -1824,   791,   128,  -219,  -212,
    -208,     1,   -70,   -60,   -49,  1598,   -36,   -13,     7,  -206,
     178,  -202,  -199,  -198,   343,  -195,  -188,  -187,  -728,  -720,
    -712,  -710,  -716,  -123,  -675, -1824, -1824,  -698,  1409,  1411,
    1421,  2693, -1824,   574,  7930, -1824,  -620,  -595,  -585,  -571,
    -726, -1824, -1765, -1762, -1755, -1738,  -619,     5,  -307, -1824,
   -1824,    -8,    76,   -68, -1824,  8385,  2091,  -629,  -146
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1210,   224,   406,   407,    82,    83,   408,   382,   409,
    1544,  1545,   410,  1016,  1017,  1018,  1329,  1330,  1331,  1556,
     432,   412,   413,   414,   710,   711,   415,   416,   417,   418,
     419,   420,   421,   422,   423,   424,   425,   434,  1115,   712,
    1463,   773,   218,   775,   428,   840,  1211,  1212,  1213,  1214,
    1215,  1216,  1217,  2193,  1218,  1219,  1470,  1669,  2034,  2035,
    1957,  1958,  1959,  2159,  2160,  1220,  1683,  1684,  1685,  1851,
    1852,  1221,  1222,  1223,  1224,  1225,  1226,  1878,  1882,  1487,
    1479,  1227,  1228,  1486,  1480,  1229,  1230,  1231,  1232,  1233,
    1701,  2177,  1702,  1703,  2070,  1234,  1235,  1236,  1466,  2078,
    2079,  2080,  2223,  2235,  2106,  2107,   303,   304,   911,   912,
    1182,    85,    86,    87,    88,    89,    90,   465,    92,    93,
      94,    95,    96,   232,   233,   591,   285,   467,   436,   468,
      99,   313,   101,   102,   155,   346,   347,   106,   107,   170,
     108,   932,   348,   156,   111,   256,   112,   157,   264,   350,
     351,   352,   158,   429,   117,   118,   354,   119,   582,   900,
     898,   899,  1642,   355,   356,   122,   123,  1178,  1431,  1648,
    1649,  1811,  1812,  1432,  1637,  1831,  1650,   124,   670,  1751,
     666,   357,   667,   668,  1291,  1108,   473,   474,   904,   905,
     475,   476,   906,   359,   586,  1240,   438,   439,   219,   493,
     494,   495,   496,   497,   334,  1260,   335,   930,   928,   616,
     336,   376,   337,   338,   440,   126,   176,   177,   127,  1254,
    1255,  1256,  1257,     2,  1165,  1166,   608,  1249,   128,   325,
     326,   266,   277,   565,   129,   222,   130,   316,  1117,   890,
     527,   168,   131,   681,   682,   683,   132,   318,   236,   237,
     238,   319,   134,   135,   136,   137,   138,   139,   140,   241,
     320,   243,   244,   245,   321,   247,   248,   249,   808,   809,
     810,   811,   812,   250,   814,   815,   816,   778,   779,   780,
     781,   528,  1158,  1409,   141,  1759,   641,   642,   643,   644,
     645,   646,  1814,  1815,  1816,  1817,   631,   478,   362,   363,
     364,   441,   210,   143,   144,   145,   366,   832,   647
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      81,   103,   133,    81,   358,   194,   192,   151,   377,   907,
     715,   509,   344,  1258,   590,   195,  1025,   562,   510,   953,
     548,   499,   511,  1483,   512,   545,   196,   829,   513,   959,
    1457,   514,   515,  1468,   721,   516,   201,  1005,   968,   197,
     945,   671,   517,   518,   373,  1944,   922,  1940,  1596,  1597,
     946,  1414,   649,   659,  1941,    81,    81,   662,    81,   103,
     133,   534,   198,   180,   947,    91,   883,   885,   152,  1467,
      97,  1942,   998,   308,    81,   104,   209,   142,  1084,  1060,
     142,  1088,   199,    81,  1078,   207,  1426,  1095,  1671,   559,
    1085,    81,  1079,   887,  1080,   509,    81,  2037,   239,    81,
     570,   267,   510,    81,   895,   278,   511,   271,   512,  1173,
    1334,   525,   513,   530,  1241,   514,   515,  1863,    58,   516,
     538,  1287,   444,    91,  1705,   381,   517,   518,    97,  1081,
      58,  2036,   445,   104,   460,   142,   427,   113,  1250,  1341,
    1111,    81,   209,   446,    81,   924,    81,   103,   133,  1558,
    -792,   311,    81,   524,   109,  2043,   447,  2110,  1126,    81,
     194,   507,  1413,  1376,   260,    58,    81,   220,   272,  1417,
     195,   361,  1672,  1672,  1707,   380,   649,  -410,  2029,   448,
     142,   196,  2102,   297,   620,   450,   953,   597,   599,  1427,
      81,    81,   207,   974,   197,   113,   146,  1377,   166,   449,
    1118,   691,  1375,  1706,   426,    81,   598,   481,  2042,   945,
     297,    91,   109,  2076,   526,   490,    97,   198,   663,   946,
      81,   104,   298,   142,   620,   235,   735,   736,   755,    81,
      81,  -411,   207,   947,   716,  1199,  1247,   199,  -824,  1661,
     275,  -793,  1708,  1428,   194,   602,  1497,   735,  -410,   526,
      81,   554,   577,    98,   195,  2038,   153,  2044,    81,  2111,
     207,  1298, -1105,  1110,  1110,   196,  1109,  1109,    81,    81,
     756,   114,    81,   664,   566,   242,   875,   735,   665,    81,
    2036,  1856,  1110,   113,   879,  1109,  1973,  1481,   847,   672,
    1322,  2101,   674,    81,    81,   848,    81,  1720,   833,   849,
     109,   850,  -411,   907,  1064,   851,  2138,   917,   852,   853,
    1482,    98,   854,    81,    81,   554,   207,   940,   881,   855,
     856,   981,  1484,    81,   886,  1377,  1426,  1426,  1426,   114,
      81,    81,  1282,  1436,   649,    81,   205,   563,   971,  2103,
    2104,  1292,  1023,  1719,   813,  1485,  1467,  1722,   632,   220,
    1671,  1088,  1437,   159,   965,  1361,  1078,  1313,   649,   520,
    1110,  1348,   160,  1109,  1079,   649,  1080,  1362,   598,   907,
     664,  1944,  1481,  1940,    81,   665,   593,    81,   680,  2158,
    1941,  1954,  1955,   847,  1415,  1771,  1773,  1775,   894,  1444,
     848,   800,   638,   306,   849,  1482,   850,  1942,  1237,    98,
     851,  1353,   205,   852,   853,  2158,   293,   854,  2042,   521,
     165,  1629,   189,  2029,   855,   856,  1709,   114,   221,  1241,
    -824,  1758,   149,    63,    64,   209,    20,  1290,  1170,  1427,
    1427,  1427,   922,  2195,  1672,    58,   298,   444,   113,   179,
     246,  1386,    81,   520,   481,  2042,   201,   445,  1439,  1440,
     263,   722,   344,  1663,   458,   109,   723,  1563,   446,  1978,
    1979,   284,   287,  1282,  1956,    81,    81,   181,   632,  1596,
    1597,   447,    77,   970,   174,   174,   182,    81,    81,  1406,
     907,   945,   626,  1428,  1428,  1428,    81,    58,   490,  1564,
     190,   946,  2149,   521,   448,   954,  1880,   907,   907,   657,
     937,  1407,   202,   660,   263,   947,    81,  1199,  2151,   550,
     174,   553,  1477,   213,   449,   535,    58,  1854,    81,   526,
     481,   980,  1862,  1199,   983,   984,  1828,   985,  1884,  1881,
      -3,   161,   444,  1829,   162,   163,   987,   164,  1000,   989,
     990,   991,   445,    81,  1468,  1478,   187,   298,  2050,    81,
    1520,  1027,  1830,   446,    98,   227,   263,  1954,  1955,   533,
     174,  1881,   105,   174,  1441,  1110,   541,   605,  1109,  2053,
    2054,   526,   114,   251,   522,   553,    58,   617,   174,  -975,
    1467,   618,  1672,  1000,   261,   371,  -975,   558,    81,   571,
      81,  1343,   914,  2059,    58,   283,   791,   290,   569,   292,
     526,    81,   626,    81,   583,   481,   270,    81,   103,   133,
     649,   361,   923,  1846,  1847,  1848,    58,    81,  1128,  1000,
     105,    81,    81,   263,  1655,  1438,    58,  1000,  -476,  2128,
    1436,  1588,  1267,  1508,  1119,  1849,    58,  2130,   261,   174,
    1983,   290,   292,  1656,  1151,  1819,  1770,   867,   649,  1726,
     613,   959,   293,   632,    81,  1567,  1065,   263,   522,    58,
     526,   262,  1089,   263,  1820,  1537,  1092,    81,    58,   593,
    1655,    58,    91,  1443,  1086,  1105,  1106,    97,   636,   614,
     615,   295,   104,   813,   142,   205,    58,   174,   174,  1822,
     261,  1972,   909,    58,   298,   263,  1093,   868,   174,   942,
     636,  2095,  1049,  1237,  1000,   297,  1139,  1000,   105,  1829,
     526,   799,   580,   174,  1571,   585,  1143,  2164,   577,   235,
     526,  1660,    81,  1864,    81,   298,    81,  1823,  1939,   275,
      81,   113,   867,    81,  1945,   215,   745,   746,  1262,  1147,
     700,   922,   174,   526,   113,  -653,   216,   312,   109,   174,
     174,  1389,  -653,  1946,   174,   526,   375,   261,    81,   290,
     292,   109,   217,   938,  1672,    58,  1393,  1829,    58,   242,
     526,  1099,   993,  1397,   907,   907,  1294,   526,  1694,  1505,
     747,   748,   868,   994,   995,   724,  1949,   958,   907,   907,
     725,   261,  1672,   174,  1857,  2085,   174,   261,  2086,  1858,
     964,    58,  1307,    81,  2052,    81,  1000,  1311,  1595,    14,
      15,    16,    17,    18,   738,  1152,  2065,    81,  1319,   263,
     715,   739,   740,  -410,    81,  2048,   907,   907,   907,   261,
     490,  1672,    58,    81,   564,   654,   700,   292,   344,   426,
    1286,   153,    81,    81,    81,  1518,  1337,    98,  1573,   636,
    1114,   190,   526,  1333,   877,   332,  1553,    58,  1374,  -599,
      98,  1916,   869,  1917,   190,   114,   193,  1000,    58,    81,
     450,  -718,   526,  1768,   698,   942,  1317,  2166,   114,   889,
    1735,  1582,  1555,  1318,   174,   526,   893,   190,   234,  1333,
     897,  2121,   200,    64,    74,  -976,   174,   174,  1749,  1990,
    2143,   263,  -976,  -813,  1991,  2144,   262,  1756,    81,    81,
     490,   103,  1586,   757,   635,  2210,   636,   758,   636,    74,
    2211,  1760,   263,  1381,  1767,    79,   637,   261,   378,  1251,
    -480,   379,  1510,  1511,   915,  1670,  1686,  2096,   638,   776,
    1808,   526,   263,   526,   317,  1821,   380,   869,   576,    64,
      79,    80,   922,   261,  1367,   654,   292,   834,   835,  1360,
     813,   836,    81,   451,    14,    15,    16,    17,    18,   783,
     698,   252,   253,   784,   254,    91,   680,   263,   452,   255,
      97,   649,  1846,  1847,  1848,  1239,   453,   142,   454,   161,
    1252,   455,   162,   163,  1691,   164,   485,   361,   621,   293,
     691,   263,   142,   261,  1849,   795,   908,   149,   263,   526,
     909,   456,    81,  1850,   480,    14,    15,    16,    17,    18,
      81,   508,   234,    58,  1028,  1029,  1030,  1498,   261,  1404,
     262,   741,   742,   261,  1636,   261,   174,   907,   907,   907,
     933,   936,   317,  1522,   716,   484,   297,   113,   450,    81,
     526,   535,   490,   860,   500,   526,   261,   498,   261,   261,
     503,   605,  1865,   450,   109,   526,   973,  1509,  1338,   504,
     618,  1755,   505,   962,    58,    81,   261,   506,  1266,   743,
     744,    81,    81,  1838,   735,  -481,   174,  1527,   261,   524,
    1766,   377,   377,   477,   202,   718,    74,  1536,    14,    15,
      16,    17,    18,   975,   976,   603,   317,   618,   977,   749,
     750,   261,    81,   654,   292,  1086,   635,   450,   523,   636,
     636,  1902,  1547,  1548,  1549,   546,  1905,    79,    80,  1550,
    1551,  1805,  1806,  1807,    74,   261,   654,  1912,   624,   718,
    1546,  1009,   261,  1011,  1114,  1014,  1541,   344,   547,  1022,
    1618,    74,  1026,   262,   635,   999,   220,    58,   636,  1000,
    1673,  1002,  1003,    98,    74,    79,   637,   557,   907,   105,
    1433,   635,  1251,  1101,  1102,   636,  1069,  1051,  1253,   587,
     526,   114,    79,   637,  1809,  1123,   103,   490,   526,  1124,
      81,    81,    81,  1103,  1104,    79,    80,  1670,  1752,  1320,
    1124,  1846,  1847,  1848,   907,  1335,  1336,  1598,   656,   907,
     907,   568,   564,  1000,  1172,   490,   103,   609,   297,  1000,
    1339,    81,   526,  1849,  1156,   624,   535,  1161,   673,  1928,
     526,  1000,  1855,  1252,    81,   669,   174,    81,    81,   267,
     278,    81,   271,   174,  1534,   142,   665,  1163,  -157,  -157,
      91,  1000,    81,   263,   684,    97,   685,  -477,   807,   688,
    1239,  1127,   142,  1129,  2005,  2006,   263,    14,    15,    16,
      17,    18,   605,  1332,   689,   632,   526,  1333,  2083,   690,
      91,   694,  1846,  1847,  1848,    97,   718,    81,   751,  1504,
    1239,   263,   142,   784,  1000,  1540,  1763,   753,   846,  1333,
    1764,    81,   260,   272,  1849,  1839,   361,  1840,   737,  1333,
     234,  1000,   426,  -186,  1866,   142,  1867,   490,  1000,  1868,
    1124,   752,   113,  1000,   754,    81,    58,  1181,   174,   174,
    1604,  1605,  1750,  1950,  1996,   317,  -478,   784,  1000,   109,
    2045,   317,  1477,  1478,  1000,  1686,    14,    15,    16,    17,
      18,  2147,   113,  1103,  1496,  1333,  2148,    81,   344,   759,
    1000,  1568,  -122,  -122,  -122,  -122,  -122,  -122,   785,   109,
    1696,  1697,  1698,  1699,  1700,   275,  2232,  1418,  1419,  1420,
    2229,  1285,   317,  2091,  2238,  2179,   786,   261,  2239,  2183,
    1832,  1832,  1832,   921,   787,   317,  1561,  1562,   426,   426,
     261,  1035,  1036,  1037,  1038,    58,  1566,  1562,   457,   958,
    1433,  1433,  1433,  1570,  1562,  1638,  1433,  1075,  1554,  1903,
     788,  1253,  1673,   789,   151,   261,   509,   790,    81,    -3,
    1429,   817,    81,   510,  -479,    81,   261,   511,    98,   512,
    1606,  1554,   477,   513,   -18,   261,   514,   515,   103,   103,
     516,  1075,  1620,  1776,  1124,   490,   114,   517,   518,  1914,
    1124,  2153,  1915,  1562,   262,  1925,  1926,   831,    98,  1937,
    1000,   841,   105,  1994,  1995,  2016,  1562,   564,   490,   490,
    2017,  1562,  1954,  1955,   830,   152,   114,   872,    81,   857,
     566,  -121,  -121,  -121,  -121,  -121,  -121,   142,  2229,  2230,
    1559,  1560,   262,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    91,    91,   858,   477,   859,   361,  1031,  1032,
    1039,  1040,  1675,  1675,   142,   142,   861,  2108,   862,  1877,
    2150,  2152,   490,    14,    15,    16,    17,    18,   997,   863,
    1893,  1894,  1251,   261,    14,    15,    16,    17,    18,  1312,
     864,   490,   865,   563,   866,  2108,    81,  1652,   871,  1033,
    1034,    81,    81,    81,   280,   873,  1598,   261,   305,  1721,
    1723,  1833,  1834,   891,   344,   892,  1824,  -597,  1753,  1754,
     907,  1506,  1507,  -595,   113,   113,   901,   910,   263,   925,
     927,   931,   948,   950,  2161,  1077,   638,   807,   967,  1546,
     978,   109,   109,  1252,   972,   847,   174,  1653,   979,   174,
     174,   174,   848,  1001,  1004,   142,   849,  1007,   850,  1048,
    1074,  1053,   851,  1075,   263,   852,   853,  1082,  1598,   854,
    1121,  1130,  1153,    81,   174,   317,   855,   856,    81,  1131,
     174,  1132,  1133,  1134,    81,  1135,    81,  1136,  1137,  1138,
     477,  1162,  1875,   585,    81,  1164,  -796,  1168,  1175,  1242,
     174,   317,  1176,  1177,  -694,  2004,   490,  1243,  1259,  1263,
    1429,  1429,  1429,   153,  1634,  1635,  1639,  1276,   271,   490,
    1277,  1278,   208,  1289,  1290,  1293,  1323,  1296,  1295,  1299,
    1388,   477,  1300,  1303,  1302,   240,  1304,  1306,   268,  1305,
      98,    98,   279,  1308,  1309,  1310,   174,  1315,  2012,  1316,
     103,  1324,  1340,   477,   477,  1345,  1346,  1347,   114,   114,
    1542,  1354,   261,  1355,   490,  1356,  1357,  1365,  -682,  1380,
    -681,  1405,   477,   361,  1251,  -797,  1434,  1410,   260,   272,
    1445,  1435,  1448,  1449,  1458,  1459,  1460,   105,  1465,  1469,
    1471,   142,  1666,  1493,  -717,  1000,  1494,  1492,   261,  1490,
    1623,  1554,  1652,  1500,   261,  1528,   726,  1652,   727,   728,
     729,    81,  1654,    81,    91,   520,  1535,   105,  1502,   649,
     263,  1538,  1552,  1569,  1675,  1581,   142,  1600,  1599,   208,
    1594,  1253,  1602,  1601,  1562,  1252,  1607,   730,  1610,   262,
     731,   732,  1626,  1625,   482,   733,   734,   142,  2082,  1627,
     477,   275,  1653,  1630,  1643,  2090,  1641,  1653,  1989,  1644,
    1438,  1478,  1710,    81,  1687,   521,    81,  1688,  1690,   208,
    1692,  1704,  1712,  1713,  1714,   564,  1715,  1716,   490,   263,
    1199,  1727,   490,  1729,  1730,  2137,   113,    84,  1732,  1733,
     150,   174,  1734,  1738,   174,  1736,   490,   208,   103,  1598,
    1737,  1739,  1740,   109,  1742,  1747,   490,  1077,  1757,  1761,
    1762,   567,  1765,  1359,   807,  1769,  1777,  1778,  1782,   490,
    1783,   490,   490,   490,   490,   450,  1793,  1791,  1803,    81,
      81,  1804,  1606,  1818,  1841,  1646,   221,   174,   174,  1843,
    1872,  1874,  1892,  1895,  1899,    84,  1901,  1906,  2033,   563,
    1900,  1908,  1904,  1913,   261,  1919,   509,  1920,  1923,  1924,
    1934,   191,    91,   510,  1930,  1935,  1936,   511,  1962,   512,
      84,  1967,  1675,   513,   142,  2077,   514,   515,  1982,  1992,
     516,  1986,  1993,   231,  1998,  1968,   259,   517,   518,  2010,
      84,    81,  1980,  1988,  2008,   867,  2009,   490,  2013,  2014,
    2011,   490,    98,  2015,  -683,  2023,   490,  2024,  2025,  2026,
    2027,  2028,  2047,   261,   526,  2060,   426,  1654,  -580,  2073,
     114,  2055,  1654,  1253,  2074,  2087,  2066,   150,  2049,  2058,
     522,   490,  2088,    84,   113,  1926,   150,  2089,  2092,   315,
     323,  2075,  2093,  2094,  2114,   868,  2105,  2127,  2129,   105,
     105,   109,   343,  2131,  2141,   477,  2140,  2142,  2145,  2146,
    2154,  2167,  2163,   148,   263,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,  2165,   433,   191,   191,  2180,
     103,   482,   490,  2176,   317,  2187,   490,  2182,   150,   463,
     564,  2189,   259,  2190,  2196,  2156,   188,  2033,  2206,   490,
    2209,  2073,  2207,  2217,  2219,   194,   602,   299,   103,  2220,
      81,  2225,    81,   174,  2224,   195,   231,   231,  2227,  2236,
    1652,  1565,  1454,  1910,  2077,   174,   196,   996,  2077,  2077,
    1041,  2237,  1044,  2240,  1042,  1464,  2181,   315,   174,   281,
    1043,  1045,  1473,   282,    91,    84,   286,   103,   291,   490,
      98,   774,  2218,  1876,  1675,  2157,   142,   482,  2174,   259,
    1984,  1977,  1885,   426,  2208,   426,  2071,  1883,   114,  1870,
    1653,  2204,    91,  1871,    81,    81,  2133,   207,  2184,  2226,
    2132,   171,  1675,   174,   142,   490,  1491,   288,   490,  1589,
     490,  2222,   556,   323,  2100,  2222,   502,   490,   261,   323,
     315,   315,  1640,  2031,   426,  1288,  1261,  1488,  1120,   150,
     869,    91,   837,  2233,   929,   212,   113,    81,  2231,   481,
    1889,  1675,     3,   142,  1297,  1056,   490,  1057,   490,   343,
     639,   648,   490,   109,   490,   263,  1802,  1058,  2205,     0,
       0,     0,     0,     0,   113,     0,   343,     0,  2071,     0,
     343,     0,     0,     0,     0,     0,     0,   490,     0,     0,
    1651,   109,     0,     0,    14,    15,    16,    17,    18,    81,
       0,     0,     0,     0,     0,   262,     0,   594,    81,     0,
       0,   212,     0,   113,   148,   433,   426,     0,    65,    66,
      67,    68,    69,    70,    71,    72,   281,   174,   477,   477,
     109,   174,     0,     0,   520,     0,     0,     0,     0,     0,
       0,   105,     0,     0,     0,   174,     0,     0,     0,   433,
       0,   442,   777,    58,     0,   174,     0,     0,     0,   191,
       0,     0,    98,    76,     0,  1654,   826,     0,   174,     0,
     174,   174,   174,   174,     0,   150,   867,     0,   174,   463,
     114,   261,     0,   806,   521,   648,     0,     0,     0,     0,
      98,   491,     0,     0,     0,     0,     0,     0,     0,   261,
       0,     0,     0,   714,     0,     0,     0,     0,   114,     0,
       0,   281,   282,    74,   653,     0,   291,     0,     0,     0,
     555,     0,     0,   231,     0,     0,   868,   471,     0,    98,
       0,     0,     0,   776,     0,   231,     0,   526,     0,     0,
       0,     0,     0,     0,    79,    80,   174,   114,     0,     0,
     174,     0,     0,     0,   699,   174,     0,     0,     0,     0,
     315,     0,   433,   433,     0,     0,   315,     0,   343,     0,
       0,     0,     0,     0,     0,     0,   564,     0,     0,     0,
     174,     0,     0,     0,   555,     0,     0,     0,     0,   105,
       0,     0,   263,     0,     0,  1651,     0,   261,     0,     0,
    1651,     0,     0,     0,   634,     0,  1825,   315,  1651,     0,
      14,    15,    16,    17,    18,     0,     0,     0,   315,     0,
     315,     0,   343,     0,    84,     0,     0,     0,     0,     0,
       0,   174,     0,     0,     0,   174,     0,   876,     0,     0,
     343,   463,     0,   648,     0,   880,     0,     0,   174,   522,
     699,   639,     0,     0,     0,   639,     0,     0,     0,     0,
       0,  2139,   888,     0,   343,     0,     0,   629,     0,    58,
     652,     0,     0,   896,   648,     0,     0,   343,     0,     0,
       0,     0,     0,     0,   629,     0,     0,     0,   629,   150,
       0,   869,     0,     0,   212,     0,     0,     0,   174,     0,
       0,     0,   433,     0,     0,   150,   150,     0,   433,     0,
       0,     0,     0,     0,     0,     0,     0,   433,  1837,     0,
     150,   150,   150,     0,   634,     0,   261,     0,   281,    74,
       0,     0,     0,     0,   174,     0,     0,   174,     0,   174,
       0,     0,     0,     0,     0,     0,   174,     0,     0,  1809,
       0,     0,     0,   526,     0,     0,     0,     0,   251,     0,
      79,    80,   827,     0,   491,     0,     0,     0,     0,     0,
      19,   105,     0,     0,     0,   174,   463,   174,     0,     0,
       0,   174,     0,   174,     0,  1951,     0,   629,  1651,     0,
     714,     0,   777,   777,     0,     0,   714,     0,     0,   105,
     433,     0,    19,     0,     0,   714,   174,   442,   442,    48,
      49,    50,    51,    52,    53,    54,    55,   463,  2234,     0,
     806,     0,   806,     0,   714,     0,     0,  2241,     0,     0,
       0,     0,   261,     0,     0,     0,     0,   411,   105,   343,
     343,     0,     0,     0,     0,    52,    53,    54,    55,   477,
     477,     0,   317,     0,     0,     0,     0,     0,   343,   148,
     315,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,   116,   471,     0,   116,     0,
       0,     0,     0,     0,     0,     0,   315,    74,   148,     0,
       0,     0,    65,    66,    67,    68,    69,    70,    71,    72,
    1020,     0,     0,     0,     0,     0,     0,  1645,    76,   960,
       0,     0,     0,  1651,  1646,     0,     0,     0,    79,    80,
       0,     0,     0,     0,     0,     0,     0,   433,     0,     0,
     471,     0,     0,   116,   442,     0,   343,     0,     0,     0,
    1021,     0,   150,   433,     0,     0,     0,     0,   629,   471,
       0,     0,     0,     0,     0,   343,   148,  1270,   116,     0,
      65,    66,    67,    68,    69,    70,    71,    72,   639,     0,
       0,     0,   629,     0,   265,     0,     0,     0,   116,     0,
       0,  1159,     0,     0,     0,   629,     0,     0,     0,     0,
       0,     0,     0,  1167,     0,     0,     0,  1171,     0,     0,
       0,  1174,     0,     0,  1358,    76,   268,   279,   463,     0,
       0,     0,     0,     0,     0,   116,     0,     0,     0,     0,
       0,   116,     0,     0,   116,     0,     0,     0,   265,     0,
       0,     0,   317,     0,     0,     0,     0,     0,   687,   339,
     116,   372,   411,   693,     0,   442,     0,     0,     0,     0,
       0,     0,   702,   703,     0,   675,     0,     0,     0,     0,
       0,     0,     0,     0,   437,     0,     0,   411,   411,     0,
       0,     0,     0,     0,     0,   777,   116,   437,     0,     0,
     265,   477,     0,     0,   471,     0,     0,     0,   411,     0,
       0,     0,   806,     0,     0,     0,   603,   317,     0,   806,
       0,     0,     0,     0,   537,     0,   491,     0,     0,   827,
      14,    15,    16,    17,    18,     0,     0,     0,   411,     0,
       0,     0,     0,     0,     0,   471,     0,     0,     0,     0,
     676,   116,     0,   116,     0,     0,     0,     0,     0,   317,
       0,   343,     0,     0,     0,     0,   677,   265,   678,   679,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,  1160,     0,     0,     0,     0,     0,     0,   581,    58,
       0,     0,     0,     0,     0,     0,   116,     0,     0,     0,
       0,   265,     0,     0,     0,   150,     0,   265,   442,     0,
       0,     0,     0,     0,   150,     0,     0,   116,     0,     0,
       0,   148,   433,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,   116,     0,   265,
     116,     0,     0,     0,     0,   433,     0,  1275,     0,    74,
       0,     0,   433,     0,   116,     0,     0,   148,   116,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,   804,
      76,     0,     0,   636,   259,    84,     0,   567,     0,     0,
      79,   805,     0,   629,     0,     0,   652,     0,     0,   315,
       0,     0,     0,   437,   148,   150,  1416,     0,    65,    66,
      67,    68,    69,    70,    71,    72,   463,     0,  1328,  1442,
       0,     0,     0,   934,     0,     0,  1328,     0,     0,   782,
     935,     0,     0,     0,     0,     0,  1461,   437,     0,     0,
      58,     0,     0,   714,     0,   793,   471,   463,   796,     0,
       0,   150,     0,     0,     0,  1328,     0,     0,   491,     0,
       0,  1474,     0,   116,     0,     0,     0,   437,  1344,     0,
       0,     0,   148,   265,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,     0,  1351,  1352,   148,     0,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,   537,     0,     0,     0,     0,
       0,     0,     0,     0,   343,   343,     0,     0,  1328,     0,
    1358,    76,   411,   411,   411,   411,   411,   411,   411,   411,
     411,   411,   411,   411,   411,   411,   411,   411,   411,   411,
     411,    14,    15,    16,    17,    18,     0,     0,     0,     0,
     437,   437,     0,     0,     0,   265,   116,     0,     0,     0,
    1475,     0,     0,     0,   150,   150,   150,   150,     0,   150,
     150,     0,     0,   148,     0,  1647,   323,    65,    66,    67,
      68,    69,    70,    71,    72,  1012,     0,     0,   116,     0,
       0,     0,     0,   116,   433,   433,   265,   116,     0,   116,
      58,     0,   411,     0,     0,     0,     0,     0,     0,     0,
     116,   148,   116,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,  1013,   372,     0,   116,   437,
       0,   265,   148,     0,     0,   259,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
     442,     0,   116,     0,     0,   265,     0,     0,   463,   581,
      74,   960,   265,     0,     0,   116,     0,   966,     0,     0,
       0,     0,     0,  1662,  1664,     0,     0,   116,     0,     0,
      75,    76,     0,     0,  1450,   150,     0,   639,     0,     0,
     437,    79,    80,   116,   116,     0,   437,     0,     0,   327,
     328,   329,   330,     0,     0,   437,     0,     0,   116,   116,
     116,   761,   762,   763,   764,   765,   766,   767,   768,   769,
     770,   771,  1724,   491,   629,   215,     0,     0,     0,     0,
       0,  1328,     0,     0,     0,     0,     0,     0,   148,     0,
     172,   173,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,   772,     0,     0,   471,     0,     0,     0,     0,
       0,     0,  1577,  1578,   437,     0,     0,     0,     0,   782,
     782,     0,     0,   411,     0,     0,  1592,  1593,   411,  1067,
    1647,  1810,  1070,     0,     0,  1647,     0,   433,   437,   411,
     331,  1647,     0,  1647,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,   437,     0,     0,   332,     0,
       0,     0,     0,     0,  1615,  1616,  1617,     0,     0,     0,
     323,   150,     0,     0,     0,     0,     0,   116,   116,     0,
     148,   411,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,     0,   537,     0,     0,   116,     0,     0,     0,
    1141,     0,     0,     0,  1145,     0,   433,     0,  1149,    58,
     148,   491,   367,   368,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,   343,     0,     0,   150,  1157,     0,
       0,     0,   116,     0,     0,     0,     0,     0,     0,     0,
       0,   148,     0,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,  1452,     0,     0,     0,   265,     0,     0,
     150,    77,     0,     0,     0,   437,   369,     0,     0,    74,
     265,     0,     0,   370,   116,     0,     0,     0,     0,     0,
     116,   437,     0,     0,  1869,   343,   343,     0,   491,   230,
      76,     0,     0,   116,     0,  1272,   437,     0,   116,     0,
      79,    80,     0,     0,     0,  1810,  1810,   491,     0,     0,
       0,     0,  1328,     0,     0,     0,   471,  1328,  1328,  1328,
    1647,   411,     0,  1647,     0,     0,     0,     0,     0,   148,
      58,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,   323,     0,     0,     0,     0,   437,     0,     0,   100,
       0,     0,   154,     0,   433,     0,     0,   110,     0,     0,
       0,     0,   148,     0,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,     0,  1795,  1796,  1797,     0,     0,
       0,     0,   442,     0,     0,  1283,     0,   315,     0,     0,
      74,     0,  1284,   183,     6,     7,     8,     9,    10,    11,
      12,    13,   782,     0,     0,   411,     0,   100,     0,   116,
    2135,    76,     0,     0,   526,   110,     0,     0,     0,     0,
       0,    79,    80,     0,     0,   411,   116,   116,     0,     0,
       0,     0,   206,     0,     0,     0,     0,     0,     0,  1810,
       0,     0,     0,   411,   411,   411,     0,     0,  1647,     0,
     411,   411,   273,     0,     0,     0,     0,     0,     0,   148,
     274,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,  1391,   411,     0,  1395,     0,     0,   116,
    1399,     0,     0,   150,     0,     0,     0,    74,     0,   307,
       0,     0,     0,     0,     0,   100,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,  1896,   230,    76,     0,
       0,   411,   411,     0,   345,     0,     0,   343,    79,    80,
       0,     0,   349,   116,     0,     0,  1810,  1328,     0,  1328,
       0,     0,   116,     0,     0,     0,   150,     0,     0,   443,
     437,     0,  1918,     0,     0,     0,     0,  1921,  1922,     0,
     307,   469,     0,     0,     0,     0,     0,     0,     0,   470,
       0,     0,     0,   437,   150,   150,     0,  2136,   323,     0,
     437,     0,     0,     0,     0,     0,     0,     0,   148,   519,
      58,     0,    65,    66,    67,    68,    69,    70,    71,    72,
    1325,     0,   265,   116,  1326,     0,  1327,     0,     0,   544,
       0,     0,     0,   150,   549,   551,     0,   206,     0,     0,
       0,     0,   148,   116,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,   437,     0,     0,    76,  1272,     0,
     572,  2136,  2136,     0,   574,     0,     0,     0,   573,   575,
      74,  1531,    14,    15,    16,    17,    18,     0,     0,     0,
     592,     0,     0,     0,   116,   437,     0,     0,   110,   116,
     230,    76,     0,   604,     0,     0,     0,     0,     0,     0,
       0,    79,    80,   148,  2136,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,  1575,     0,
       0,   627,     0,     0,   651,     0,     0,  1584,     0,   628,
       0,    58,   274,   116,   116,     0,     0,     0,   658,     0,
       0,     0,   658,     0,     0,     0,   628,   116,   116,     0,
     628,     0,   116,   116,     0,     0,     0,     0,     0,   611,
       0,     0,   204,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   116,   116,   116,     0,     0,
       0,    74,     0,     0,  1624,     0,     0,     0,     0,     0,
       0,     0,   116,   116,   116,   116,   116,   116,   116,     0,
       0,  2135,    76,     0,   265,   526,     0,     0,    58,     0,
    2221,   629,    79,    80,     0,     0,     0,     0,   204,     0,
       0,     0,   437,   437,  2228,     0,     0,   307,     0,     0,
       0,   627,     0,     0,   204,     0,     0,     0,     0,   628,
     148,    58,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,   204,     0,
       0,     0,     0,   265,     0,     0,     0,     0,    74,     0,
     411,   466,     0,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   437,     0,   314,    76,
       0,     0,     0,   629,     0,     0,     0,     0,   148,    79,
      80,    74,    65,    66,    67,    68,    69,    70,    71,    72,
    1325,     0,     0,   116,  1326,     0,  1327,     0,     0,     0,
     469,  1645,    76,     0,     0,     0,     0,   204,   470,     0,
       0,     0,    79,    80,     0,     0,     0,     0,  2191,     0,
       0,     0,     0,     0,     0,     0,     0,    76,     0,     0,
    1557,     0,   903,     0,     0,     0,     0,   551,     0,     0,
     349,   916,     0,   592,     0,     0,     0,     0,     0,   274,
       0,   110,     0,     0,   345,     0,   100,     0,     0,     0,
       0,     0,   470,     0,   110,     0,   116,   116,   116,     0,
       0,   204,   658,   941,  1447,     0,     0,     0,  1813,     0,
     628,   470,     0,     0,     0,     0,     0,   952,     0,     0,
       0,   204,     0,     0,     0,   437,   627,     0,     0,     0,
       0,   961,     0,     0,   628,     0,     0,     0,     0,   658,
       0,     0,   116,     0,     0,     0,     0,   628,     0,   183,
       6,     7,     8,     9,    10,    11,    12,    13,   265,   116,
       0,     0,     0,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,   252,   253,   437,   254,    46,     0,    47,     0,
     255,     0,     0,    49,    50,    51,    52,    53,    54,    55,
       0,   204,   116,     0,     0,   116,     0,     0,     0,   411,
       0,     0,     0,     0,     0,     0,   148,   116,   200,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   469,     0,
       0,   204,     0,     0,     0,     0,   470,     0,   116,     0,
       0,     0,     0,    58,     0,  1059,     0,     0,     0,   411,
       0,     0,     0,   116,     0,     0,     0,     0,   116,   116,
       0,     0,     0,   116,   116,    76,     0,     0,   826,   941,
       0,     0,  1813,  1813,  1083,   148,     0,   470,     0,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,  -458,
       0,   469,   469,     0,     0,     0,     0,     0,     0,   349,
     349,     0,     0,    74,     0,     0,     0,     0,   115,     0,
     469,     0,  -458,     0,   204,   204,     0,     0,   349,   265,
     466,     0,     0,    75,    76,     0,     0,     0,     0,     0,
       0,     0,   437,     0,    79,    80,     0,     0,     0,     0,
       0,     0,     0,     0,   175,   178,   903,     0,     0,     0,
       0,     0,     0,     0,   349,     0,   411,     0,   411,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,     0,     0,     0,   204,     0,     0,     0,     0,  1238,
     223,     0,     0,     0,     0,  1717,  1718,   110,   469,     0,
       0,     0,     0,   466,   154,     0,   349,   411,     0,     0,
       0,     0,     0,     0,     0,     0,  1813,   658,     0,     0,
    1274,   276,   903,     0,     0,   628,   204,  1280,   274,     0,
     349,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     309,   411,     0,   310,     0,     0,     0,     0,     0,     0,
       0,   204,     0,     0,     0,     0,     0,     0,   333,     0,
       0,     0,     0,     0,   115,     0,     0,     0,     0,     0,
     345,   116,     0,     0,     0,     0,     0,     0,   470,     0,
       0,     0,     0,   353,     0,     0,     0,     0,     0,   411,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2098,
       0,     0,     0,  1813,     0,   116,   148,     0,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   501,
     472,   148,     0,     0,   116,    65,    66,    67,    68,    69,
      70,    71,    72,   903,    74,     0,     0,     0,   466,     0,
       0,   349,     0,     0,  1813,     0,     0,     0,     0,    74,
     903,   903,   116,   116,  1645,    76,   265,     0,   349,   349,
       0,  1646,   204,     0,     0,    79,    80,   560,   561,  1076,
      76,     0,     0,   636,     0,     0,     0,     0,   175,   466,
      79,    80,     0,  1842,     0,     0,     0,     0,     0,     0,
       0,   116,     0,   175,     0,     0,  1853,     0,     0,     0,
       0,   466,   466,   469,     0,     0,     0,     0,  1813,  1813,
       0,   349,     0,     0,     0,     0,     0,     0,     0,   115,
     466,     0,   607,     0,     0,     0,     0,     0,     0,   610,
     612,     0,     0,     0,   619,     0,     0,     0,     0,   116,
       0,  1887,    14,    15,    16,    17,    18,   154,     0,     0,
       0,  1813,     0,     0,     0,     0,  1430,     0,     0,     0,
     630,     0,     0,   276,  1238,     0,     0,     0,     0,     0,
       0,     0,   110,   333,     0,     0,   333,   630,     0,   148,
       0,   630,     0,    65,    66,    67,    68,    69,    70,    71,
      72,  1325,     0,     0,  1238,  1326,     0,  1327,   466,     0,
       0,    58,   110,     0,     0,   204,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,  1489,     0,     0,
       0,     0,     0,     0,     0,   274,     0,     0,    76,     0,
       0,  1772,     0,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,   627,     0,
       0,     0,     0,     0,     0,  1953,   628,   549,     0,  1963,
       0,    74,     0,     0,   223,     0,     0,     0,     0,     0,
     204,    58,     0,  1976,     0,     0,   821,   822,   903,   345,
     630,   314,    76,  1985,     0,     0,   349,   470,     0,     0,
       0,     0,    79,    80,     0,     0,  1997,     0,  1999,  2000,
    2001,  2002,     0,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,   148,     0,   576,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   903,   903,     0,
       0,    74,     0,     0,     0,   349,   349,     0,     0,     0,
       0,   903,   903,     0,     0,     0,   469,   469,     0,   349,
     349,  1645,    76,     0,   349,   349,     0,     0,     0,     0,
       0,     0,    79,    80,     0,     0,     0,  1050,     0,   472,
       0,     0,     0,     0,  2041,     0,     0,     0,  2046,   903,
     903,   903,     0,  2051,     0,     0,     0,   349,   349,   349,
       0,     0,     0,     0,     0,     0,  1430,  1430,  1430,   154,
     551,   353,     0,   466,     0,     0,     0,     0,  2081,     0,
     276,     0,   115,     0,     0,     0,   333,     0,     0,     0,
       0,     0,     0,   472,     0,   115,  1674,  1674,     0,     0,
       0,     0,     0,     0,   110,   110,     0,     0,     0,     0,
       0,   630,   472,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,  2109,
       0,     0,     0,  2112,     0,   630,   969,     0,     0,     0,
       0,    74,     0,     0,     0,     0,  2126,   148,   630,   578,
     579,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     345,   804,    76,     0,     0,   636,     0,     0,   470,     0,
     148,     0,    79,   805,    65,    66,    67,    68,    69,    70,
      71,    72,  1325,     0,     0,   638,  1326,   154,  1327,     0,
       0,     0,     0,     0,     0,     0,  2162,     0,    77,     0,
       0,     0,     0,     0,     0,     0,   148,   204,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,   204,    76,
       0,     0,  1774,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2185,     0,    74,  2186,     0,  2188,     0,     0,
       0,     0,     0,     0,  2192,     0,     0,   472,     0,   204,
       0,     0,     0,     0,  2135,    76,     0,     0,   526,     0,
     903,   903,   903,     0,     0,    79,    80,     0,   349,   349,
     349,     0,     0,  2212,   121,  2213,     0,   121,     0,  2216,
       0,  2192,     0,     0,     0,     0,  1100,     0,   472,     0,
       0,     0,     0,  1112,     0,  1827,     0,     0,     0,     0,
       0,     0,     0,     0,  2216,     0,   903,     0,     0,     0,
     353,   353,     0,     0,   349,     0,   466,   466,     0,     0,
       0,     0,     0,  1845,     0,     0,     0,     0,     0,   353,
       0,   274,   121,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,   148,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   121,  1674,     0,
       0,    74,     0,     0,     0,   353,   110,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   345,   121,  1183,   154,
       0,   314,    76,     0,   349,   289,     0,     0,     0,     0,
       0,   903,    79,    80,     0,     0,    77,     0,   115,   349,
       0,     0,     0,     0,     0,     0,     0,   353,     0,     0,
       0,     0,     0,     0,   121,     0,     0,     0,     0,     0,
     121,     0,     0,   121,     0,     0,   630,   903,     0,   276,
       0,   353,   903,   903,     0,   349,     0,   469,   469,     0,
     349,   349,     0,     0,   120,   349,   349,   120,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1943,     0,
     204,     0,   148,   121,   172,   173,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   121,     0,     0,     0,   472,
       0,   389,     0,   390,   391,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,  1674,     0,     0,     0,
     480,     0,     0,   148,   110,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,   120,     0,     0,
     121,   720,   121,     0,    77,   400,     0,   121,     0,     0,
       0,     0,   353,     0,     0,     0,   148,   120,   172,   173,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   353,
     353,   484,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   121,     0,     0,     0,     0,
       0,   214,     0,     0,   120,     0,     0,   225,   226,   204,
     120,     0,     0,   120,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   353,     0,     0,     0,     0,     0,     0,     0,
       0,   296,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,  2072,     0,     0,     0,     0,
       0,     0,     0,   628,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1451,
    1453,  1455,     0,     0,     0,     0,     0,     0,     0,   469,
       0,     0,   121,   115,     0,     0,   204,   349,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1674,     0,
    1476,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     120,     0,   120,   115,     0,     0,   121,   120,     0,     0,
    1183,     0,     0,     0,     0,     0,  1674,  2072,     0,     0,
       0,     0,     0,     0,   110,   628,   276,     0,     0,     0,
       0,     0,   121,     0,     0,     0,     0,   466,   466,     0,
       0,     0,     0,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1674,  1532,   630,     0,     0,
       0,     0,     0,   110,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2178,     0,     0,   353,   472,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   903,   600,     0,     0,     0,     0,     0,
       0,   349,     0,     0,     0,     0,     0,     0,     0,   121,
     121,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   353,   353,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
     353,   353,     0,     0,     0,   353,   353,     0,     0,     0,
       0,     0,   121,     0,     0,     0,   121,     0,   121,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,   121,     0,     0,     0,     0,     0,     0,   353,   353,
     353,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1657,   120,     0,  1659,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,   115,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,   802,     0,   803,     0,   121,
       0,     0,   121,   121,     0,   121,   819,   820,     0,   466,
       0,     0,     0,     0,   121,     0,     0,   121,   121,   121,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
     120,     0,     0,     0,     0,     0,     0,     0,     0,   472,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   125,     0,
       0,   125,   120,     0,     0,     0,   120,     0,   120,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   913,     0,     0,   125,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   353,
     353,   353,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   125,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,  1835,     0,     0,     0,     0,     0,   120,
       0,   125,   120,   120,  2032,   120,     0,     0,     0,     0,
       0,     0,     0,     0,   120,   353,     0,   120,   120,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   276,     0,     0,     0,     0,     0,   125,     0,
       0,     0,     0,     0,   125,     0,     0,   125,     0,     0,
       0,     0,   383,     0,   121,   384,     0,   385,     0,   386,
       0,     0,     0,     0,     0,     0,     0,   115,     0,   121,
     121,     0,     0,     0,     0,     0,   387,     0,     0,     0,
       0,     0,     0,     0,     0,   353,     0,   125,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,   125,
     353,     0,     0,     0,   388,   389,     0,   390,   391,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   392,
     393,   380,     0,   394,   395,   396,     0,   397,   398,     0,
       0,     0,     0,     0,     0,    74,   353,     0,     0,     0,
       0,   353,   353,     0,     0,     0,   353,   353,     0,     0,
       0,     0,     0,     0,   125,   399,   125,     0,    77,   400,
       0,   125,     0,     0,     0,   401,    79,    80,   402,   403,
     404,   405,     0,     0,     0,     0,     0,     0,     0,     0,
    1098,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   125,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,     0,
     125,     0,     0,     0,   120,     0,     0,     0,  2003,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
     120,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1179,  1180,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1244,  1245,  1246,     0,     0,  1248,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   125,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   121,     0,     0,     0,     0,     0,     0,     0,
       0,   121,     0,     0,     0,     0,     0,     0,     0,   121,
     125,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   630,     0,     0,     0,     0,     0,
       0,     0,   121,     0,     0,     0,   125,     0,     0,   121,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1321,
       0,     0,     0,     0,     0,     0,     0,     0,   353,     0,
       0,     0,   121,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,     0,   121,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1342,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,   630,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   125,   125,     0,     0,     0,   121,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1366,     0,   115,     0,     0,     0,     0,     0,
       0,  1370,  1371,  1372,  1373,     0,     0,     0,     0,  1378,
    1379,     0,   120,     0,     0,     0,   125,     0,     0,  1387,
     125,   120,   125,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,   125,     0,     0,     0,     0,
       0,     0,   353,     0,     0,     0,     0,     0,  1408,     0,
       0,  1411,   120,  1412,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     1,     0,     0,   147,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,   121,   121,   121,   121,   121,   121,   121,     0,     0,
     125,     0,   120,     0,     0,     0,     0,     0,     0,  1472,
       0,     0,     0,   125,     0,     0,   125,   125,     0,   125,
       0,   121,   121,     0,     0,     0,     0,     0,   125,     0,
       0,   125,   125,   125,     0,     0,     0,     0,     0,     0,
    1495,     0,   203,     0,     0,     0,     0,  1499,   120,  1501,
    1503,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1513,     0,  1514,     0,  1515,   383,  1517,     0,
     384,     0,   385,  1525,   386,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   302,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   125,   121,     0,     0,     0,     0,     0,     0,   388,
     389,     0,   486,   391,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   392,   393,   380,  1572,   394,   395,
     396,     0,   397,   398,  1579,  1580,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,   120,   120,   120,   120,   120,   120,  1603,     0,
     399,    76,     0,   487,   488,  1608,     0,     0,   489,  1609,
     401,    79,    80,   402,   403,   404,   405,     0,     0,     0,
       0,   120,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   302,     0,     0,  1628,     0,     0,
       0,     0,     0,     0,   121,     0,     0,     0,     0,     0,
       0,   225,   552,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   302,     0,     0,     0,     0,     0,   125,     0,
       0,     0,     0,   302,     0,     0,     0,     0,   121,     0,
       0,     0,     0,   125,   125,     0,     0,     0,     0,     0,
       0,   584,   588,  1711,     0,     0,     0,     0,   595,   596,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,   121,     0,     0,   606,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   121,     0,   625,     0,     0,  1741,
       0,     0,     0,     0,     0,     0,     0,  1746,     0,  1748,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   719,     0,     0,     0,     0,     0,
    1780,  1781,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,  1786,  1787,     0,  1788,     0,
       0,     0,     0,     0,     0,   760,     0,  1792,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1798,  1799,
    1800,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,   798,     0,     0,     0,   801,     0,     0,     0,     0,
       0,   121,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   823,     0,     0,     0,   824,   825,
       0,     0,   828,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   842,   843,   844,
     845,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   167,     0,     0,     0,     0,
     874,     0,     0,     0,     0,     0,   125,     0,     0,   878,
       0,     0,     0,     0,     0,   125,     0,   120,     0,     0,
       0,     0,     0,   125,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   169,     0,     0,     0,     0,     0,     0,
       0,   302,     0,     0,     0,     0,   125,  1897,  1898,     0,
       0,     0,     0,   125,     0,     0,     0,     0,     0,     0,
     169,  1907,     0,     0,     0,     0,     0,     0,   294,     0,
       0,     0,   920,     0,     0,     0,   125,     0,     0,   584,
       0,   300,     0,   301,     0,   926,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   125,     0,  1931,  1932,
    1933,     0,     0,     0,     0,     0,   169,     0,     0,   944,
     949,   120,     0,     0,     0,     0,     0,     0,     0,   169,
       0,   169,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   125,     0,     0,     0,     0,     0,     0,     0,
       0,   121,     0,   374,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,     0,     0,
       0,   992,     0,     0,   531,   532,     0,     0,   536,     0,
     121,   539,   540,     0,   542,     0,   543,     0,     0,  2007,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   169,     0,     0,     0,   169,
       0,  2018,   169,   169,  2019,  2020,   169,     0,     0,   169,
     169,  2022,   169,     0,   169,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1055,     0,     0,     0,     0,   125,   125,   125,   125,   125,
     125,   125,     0,     0,     0,  1072,     0,     0,     0,  1073,
       0,     0,     0,     0,     0,     0,     0,     0,   944,     0,
       0,     0,     0,     0,     0,   125,   125,   622,   623,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,   169,
    1113,     0,   169,   655,     0,     0,     0,     0,     0,  1122,
       0,     0,     0,     0,     0,  1125,     0,     0,     0,     0,
       0,   120,     0,     0,     0,   169,   169,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   169,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     1,     0,     0,     0,  1169,     0,     0,     0,     1,
       0,     0,     0,  2134,     0,     0,   125,     0,     0,     0,
       0,     0,     0,   257,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     1,   792,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -483,  -483,     0,
    -483,    46,     0,    47,     0,  -483,  2175,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,  1301,     0,     0,     0,
       0,     0,  2194,     0,     0,     0,     0,     0,     0,     0,
     870,     0,     0,     0,     0,     0,     0,  2203,   125,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,   374,     0,  2214,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   169,     0,
       0,     0,   125,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,     0,     0,     0,  1349,
       0,     0,     0,  1350,     0,     0,     0,   125,     0,     0,
     944,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1363,     0,     0,  1279,     0,     0,     0,  1364,   125,     0,
       0,    14,    15,    16,    17,    18,  1368,     0,  1369,     0,
       0,     0,     0,     0,     0,     0,     0,   374,     0,     0,
     955,   956,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   125,     0,   963,     0,     0,     0,   383,     0,     0,
     384,     0,   385,  1401,   386,     0,     0,  1402,     0,     0,
       0,  1403,     0,     0,     0,     0,     0,     0,   169,   169,
      58,   387,     0,     0,     0,     0,     0,     0,     0,   147,
       0,   169,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   388,
     389,     0,   390,   391,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   392,   393,   380,     0,   394,   395,
     396,     0,   397,   398,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,   125,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     399,     0,     0,    77,   400,     0,     0,     0,     0,     0,
     401,   462,    80,   402,   403,   404,   405,  1061,  1062,     0,
       0,     0,     0,  1066,     0,     0,     0,     0,     0,     0,
       0,     0,  1512,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1087,     0,     0,  1090,  1091,     0,
    1094,     0,  1096,  1097,     0,   169,   169,     0,     0,  1539,
       0,   169,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   169,     0,     0,   169,   169,     0,   169,     0,
     169,   169,     0,     0,  1140,     0,     0,     0,  1144,     0,
       0,     0,  1148,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   169,     0,     0,     0,   169,     0,     0,     0,
     169,     0,     0,     0,     0,     0,   360,   713,     0,     0,
       0,     0,     0,  1612,     0,     0,     0,  1613,     0,     0,
       0,  1614,     0,     0,     0,     0,     0,   125,     0,     0,
       0,     0,     0,  1264,  1265,     0,     0,     0,     0,     0,
       0,     0,   459,   360,     0,     0,   383,  1281,     0,   384,
       0,   385,     0,   386,     0,   125,     0,     0,     0,  1658,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,   169,   169,     0,   529,     0,     0,     0,     0,     0,
       0,   529,     0,     0,     0,   169,     0,     0,     0,     0,
       0,     0,     0,     0,   125,     0,     0,     0,   388,   389,
       0,   390,   391,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   392,   393,   380,     0,   394,   395,   396,
    1728,   397,   398,  1731,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1743,  1676,  1677,  1678,     0,     0,     0,   399,
    1860,     0,    77,   400,   882,   884,     0,   529,     0,   401,
      79,    80,   402,   403,   404,   405,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1281,
       0,     0,     0,   360,   640,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1779,     0,     0,
       0,     0,     0,     0,   661,     0,  1784,     0,     0,     0,
    1785,     0,     0,     0,     0,     0,     0,   169,     0,     0,
    1383,     0,     0,     0,  1789,  1790,     0,  1390,     0,     0,
    1394,     0,     0,     0,  1398,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1731,     0,     0,     0,     0,     0,   169,     0,
       0,     0,     0,     0,     0,   169,     0,     0,   169,     0,
       0,     0,   169,     0,     0,     0,   529,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   529,   794,   713,   529,   797,     0,     0,     0,
     713,     0,     0,   360,     0,     0,     0,   640,     0,   713,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   713,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   529,     0,
       0,     0,   529,     0,     0,     0,  1890,  1891,     0,     0,
       0,     0,     0,     0,  1047,     0,     0,     0,     0,     0,
       0,     0,     0,  1519,     0,     0,     0,     0,     0,   211,
       0,     0,  1529,  1530,     0,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,   269,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   169,     0,     0,     0,     0,     0,     0,     0,     0,
     169,   169,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1574,   529,     0,   211,   360,     0,     0,   324,
       0,  1583,     0,     0,  1587,     0,  1590,  1591,     0,     0,
       0,   365,     0,     0,   939,   360,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   640,     0,     0,     0,   640,
     169,     0,     0,     0,     0,   211,   957,     0,   360,   169,
       0,     0,   169,     0,   169,   169,  1987,     0,   479,     0,
       0,   483,  1619,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1731,     0,     0,     0,     0,     0,     0,     0,     0,
     169,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2021,     0,     0,     0,   211,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   269,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2040,     0,     0,     0,     0,     0,     0,  1725,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,     0,     0,     0,  2068,   483,     0,
    2069,     0,     0,     0,     0,     0,   529,   529,   211,     0,
       0,     0,     0,     0,     0,   169,   529,  1068,     0,   529,
    1071,     0,     0,     0,     0,     0,     0,     0,   633,     0,
     650,   360,     0,     0,   640,     0,   640,   640,     0,     0,
       0,     0,     0,   640,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   360,   360,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1587,     0,
       0,     0,   360,     0,     0,     0,   529,     0,     0,     0,
     529,     0,     0,     0,   717,     0,     0,   529,  1142,     0,
       0,   529,  1146,     0,     0,   529,  1150,  1794,     0,     0,
       0,     0,     0,  1154,     0,     0,   169,     0,     0,     0,
    2155,     0,     0,     0,     0,     0,     0,     0,   211,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   169,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   633,     0,
     360,   529,     0,     0,   818,     0,     0,     0,     0,     0,
       0,     0,   435,     0,     0,     0,     0,     0,     0,     0,
     169,     0,     0,     0,     0,   464,   169,     0,     0,     0,
       0,     0,   640,     0,     0,     0,     0,     0,   492,     0,
     492,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   713,     0,     0,
       0,     0,     0,     0,  1888,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,     0,     0,     0,     0,
       0,   211,   211,     0,     0,     0,     0,   479,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   169,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   601,     0,     0,     0,     0,   529,
       0,   365,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1947,  1948,     0,   640,   640,     0,     0,
     479,     0,   943,   640,     0,     0,  1952,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   633,     0,     0,     0,     0,     0,     0,
       0,   169,   169,     0,     0,     0,     0,     0,     0,   374,
       0,     0,     0,     0,   169,   360,     0,     0,   211,     0,
     529,  1392,     0,   529,  1396,     0,     0,   529,  1400,     0,
       0,   717,     0,     0,   717,   717,     0,   717,     0,     0,
       0,     0,     0,     0,     0,     0,   717,     0,     0,   717,
     717,   717,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -483,  -483,  2030,  -483,    46,     0,    47,     0,  -483,     0,
       0,     0,     0,  1665,     0,   479,  1668,  1682,     0,     0,
       0,     0,  1689,     0,     0,    58,  1693,     0,  1695,     0,
       0,   492,     0,     0,     0,     0,     0,   492,     0,   211,
     169,     0,   839,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   479,   148,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     360,     0,     0,     0,     0,     0,   640,  1521,   479,   479,
       0,     0,     0,  2097,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   479,     0,     0,
       0,   360,     0,     0,     0,    75,    76,     0,    77,   322,
       0,     0,     0,     0,     0,     0,    79,    80,     0,     0,
       0,   169,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   919,     0,
       0,     0,     0,     0,     0,   529,  1576,     0,     0,     0,
       0,     0,     0,     0,   529,  1585,     0,   640,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   464,   360,   360,
       0,     0,     0,     0,     0,   479,     0,     0,     0,     0,
     951,     0,   211,     0,     0,     0,     0,     0,  1801,     0,
       0,    14,    15,    16,    17,    18,   818,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -482,  -482,     0,  -482,    46,
     169,    47,     0,  -482,     0,     0,   986,     0,  1844,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,     0,
      58,     0,     0,  1859,  1861,     0,     0,     0,     0,     0,
     839,  1006,     0,     0,  1008,     0,  1010,     0,     0,     0,
       0,     0,  1019,     0,  1024,  1019,     0,     0,  1668,     0,
       0,     0,     0,     0,  1879,     0,     0,  1523,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,  1052,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,  1054,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1063,     0,     0,     0,
       0,   383,     0,     0,   384,     0,   385,     0,   386,     0,
     464,   640,     0,  1052,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,   387,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1116,     0,     0,   492,     0,     0,     0,     0,     0,     0,
     479,     0,     0,   388,   389,     0,   390,   391,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   392,   393,
     380,     0,   394,   395,   396,     0,   397,   398,     0,  1961,
       0,     0,     0,     0,    74,     0,  1155,     0,  1964,     0,
    1966,     0,     0,  1971,  1975,     0,  1682,     0,     0,     0,
       0,  1981,     0,   717,   399,   529,     0,    77,   400,     0,
       0,     0,     0,     0,   401,  1524,    80,   402,   403,   404,
     405,   529,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   717,     0,     0,     0,     0,   435,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1271,  1273,     0,     0,     0,     0,     0,
       0,   464,     0,   269,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   211,     0,     0,     0,     0,     0,
    1019,     0,     0,     0,     0,   633,     0,     0,   360,     0,
       0,     0,     0,     0,  1052,     0,     0,     0,     0,     0,
       0,  2057,  1314,     0,     0,     0,  2062,  2064,     0,  1019,
       0,     0,     0,     0,     0,     0,   365,     0,     0,     0,
     717,     0,     0,     0,     0,     0,  2084,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   360,
     360,     0,     0,     0,     0,   492,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   529,
     529,     0,     0,     0,     0,     0,  2113,     0,  2116,     0,
       0,  2118,  2120,   479,   479,   529,  2123,  2125,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   383,     0,     0,
     384,     0,   385,     0,   386,     0,     0,     0,     0,     0,
       0,     0,   492,     0,  1382,     0,  1385,     0,     0,     0,
       0,   387,     0,   717,   717,   717,     0,     0,   717,   717,
       0,     0,     0,     0,     0,   483,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2169,  2171,  2173,   388,
     389,     0,   390,   391,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   392,   393,   380,     0,   394,   395,
     396,     0,   397,   398,     0,     0,     0,     0,     0,     0,
      74,     0,  2198,  2200,  2202,     0,     0,     0,     0,     0,
       0,     0,     0,   529,   269,  1462,  1462,     0,     0,     0,
     399,   529,     0,    77,   400,     0,     0,     0,   489,   257,
     401,    79,    80,   402,   403,   404,   405,   365,     0,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -483,  -483,     0,  -483,    46,     0,    47,
       0,  -483,     0,     0,     0,     0,  1516,     0,     0,     0,
       0,   360,  1526,     0,     0,     0,   529,  2099,    58,     0,
     529,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   464,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     492,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,   529,     0,     0,     0,  1019,     0,     0,   839,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,   211,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   322,     0,     0,     0,     0,     0,     0,    79,
      80,     0,     0,     0,     0,     0,     0,     0,     0,   269,
    1611,     0,     0,     0,     0,   529,   529,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1621,  1622,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   529,     0,
    1019,     0,     0,   365,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   492,     0,
       0,   839,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   717,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2215,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   479,   479,     0,     0,  1446,     0,
       0,     0,     0,     0,  1006,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1744,  1745,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   492,     0,     0,     0,   383,
       0,     0,   384,     0,   385,     0,   386,     0,     0,     0,
       0,     0,     0,     0,   492,     0,   839,     0,     0,     0,
     269,  1185,     0,   387,    -2,     0,  1187,  -244,  -244,  1188,
    1189,  1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,  1198,
    1199,  -338,  -338,  1200,  1201,  1202,  1203,  1204,     0,  1205,
       0,   388,   389,     0,   486,   391,  1206,  1207,    65,    66,
      67,    68,    69,    70,    71,    72,   392,   393,   380,  1208,
     394,   395,   396,     0,   397,   398,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -244,  1209,   435,     0,    77,   400,     0,  1826,     0,
     298,     0,   401,    79,    80,   402,   403,   404,   405,    14,
      15,    16,    17,    18,     0,     0,    20,  -185,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -483,  -483,     0,  -483,    46,     0,    47,
       0,  -483,   717,     0,     0,     0,     0,  1873,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2215,     0,     0,     0,     0,     0,   479,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1446,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1909,     0,     0,  1911,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   383,     0,
       0,   384,     0,   385,   717,   386,     0,   483,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1185,    77,   387,    -2,  1938,  1187,  -245,  -245,  1188,  1189,
    1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,  1198,  1199,
    -338,  -338,  1200,  1201,  1202,  1203,  1204,     0,  1205,     0,
     388,   389,     0,   486,   391,  1206,  1207,    65,    66,    67,
      68,    69,    70,    71,    72,   392,   393,   380,  1208,   394,
     395,   396,  1886,   397,   398,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,  1446,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -245,  1209,     0,     0,    77,   400,     0,     0,     0,   298,
       0,   401,    79,    80,   402,   403,   404,   405,     0,     0,
     383,     0,     0,   384,     0,   385,  -185,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1185,     0,   387,    -2,     0,  1187,     0,     0,
    1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,
    1198,  1199,  -338,  -338,  1200,  1201,  1202,  1203,  1204,     0,
    1205,     0,   388,   389,     0,   486,   391,  1206,  1207,    65,
      66,    67,    68,    69,    70,    71,    72,   392,   393,   380,
    1208,   394,   395,   396,     0,   397,   398,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1019,     0,     0,
       0,     0,     0,  1209,     0,     0,    77,   400,     0,     0,
       0,   298,     0,   401,    79,    80,   402,   403,   404,   405,
       0,     0,     0,     0,     0,     0,     0,     0,  -185,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,  1184,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   383,     0,    46,   384,    47,
     385,     0,   386,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,  1185,    58,  1186,
      -2,     0,  1187,     0,     0,  1188,  1189,  1190,  1191,  1192,
    1193,  1194,  1195,  1196,  1197,  1198,  1199,  -338,  -338,  1200,
    1201,  1202,  1203,  1204,     0,  1205,     0,   388,   389,    61,
     486,   391,  1206,  1207,    65,    66,    67,    68,    69,    70,
      71,    72,   392,   393,   380,  1208,   394,   395,   396,     0,
     397,   398,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -3,  1209,     0,
       0,    77,   431,     0,     0,     0,   298,     0,   401,    79,
      80,   402,   403,   404,   405,     0,     0,     0,     0,     0,
       0,     0,     0,  -185,     4,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,  1184,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     383,     0,    46,   384,    47,   385,     0,   386,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,  1185,    58,  1186,    -2,     0,  1187,     0,     0,
    1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,
    1198,  1199,  -338,  -338,  1200,  1201,  1202,  1203,  1204,     0,
    1205,     0,   388,   389,    61,   486,   391,  1206,  1207,    65,
      66,    67,    68,    69,    70,    71,    72,   392,   393,   380,
    1208,   394,   395,   396,     0,   397,   398,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1209,     0,     0,    77,   431,     0,     0,
       0,   298,     0,   401,    79,    80,   402,   403,   404,   405,
       0,     0,     0,     0,     0,     0,     0,     0,  -185,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   383,     0,    46,   384,    47,
     385,     0,   386,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,   387,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   388,   389,    61,
     390,   391,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   392,   393,   380,     0,   394,   395,   396,     0,
     397,   398,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1676,  1677,  1678,     0,     0,     0,   399,  1679,
    1680,    77,   431,     0,     0,     0,     0,     0,   401,    79,
      80,   402,   403,   404,   405,     0,     0,     0,     0,     0,
       0,     0,     0,  1681,     4,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     383,     0,    46,   384,    47,   385,     0,   386,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,   387,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   388,   389,    61,   390,   391,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   392,   393,   380,
       0,   394,   395,   396,     0,   397,   398,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1676,  1677,  1678,
       0,     0,     0,   399,  1679,     0,    77,   431,     0,     0,
       0,     0,     0,   401,    79,    80,   402,   403,   404,   405,
       0,     0,     0,     0,     0,     0,     0,     0,  1681,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   383,     0,    46,   384,    47,
     385,     0,   386,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,   387,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   388,   389,    61,
     390,   391,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   392,   393,   380,     0,   394,   395,   396,     0,
     397,   398,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   399,     0,
    1667,    77,   431,     0,     0,     0,     0,     0,   401,    79,
      80,   402,   403,   404,   405,     4,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   383,     0,    46,   384,    47,   385,     0,   386,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,   387,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   388,   389,    61,   390,   391,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   392,   393,
     380,     0,   394,   395,   396,     0,   397,   398,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   399,     0,     0,    77,   431,     0,
       0,     0,     0,     0,   401,    79,    80,   402,   403,   404,
     405,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   383,     0,    46,   384,
      47,   385,     0,   386,   340,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     387,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   388,   389,
       0,   390,   391,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   392,   393,   380,     0,   394,   395,   396,
       0,   397,   398,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   399,
       0,     0,    77,   461,     0,     0,     0,     0,     0,   401,
     462,    80,   402,   403,   404,   405,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   383,     0,    46,   384,    47,   385,     0,   386,   340,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   387,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   388,   389,     0,   390,   391,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   392,   393,
     380,     0,   394,   395,   396,     0,   397,   398,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   399,     0,     0,    77,  1268,     0,
       0,     0,     0,     0,   401,  1269,    80,   402,   403,   404,
     405,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   383,     0,    46,   384,
      47,   385,     0,   386,   340,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     387,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   388,   389,
       0,   390,   391,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   392,   393,   380,     0,   394,   395,   396,
       0,   397,   398,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   399,
       0,     0,    77,   400,     0,     0,     0,     0,     0,   401,
      79,    80,   402,   403,   404,   405,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   383,     0,    46,   384,    47,   385,     0,   386,   340,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   387,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   388,   389,     0,   390,   391,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   392,   393,
     380,     0,   394,   395,   396,     0,   397,   398,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   399,     0,     0,    77,   461,     0,
       0,     0,     0,     0,   401,    79,    80,   402,   403,   404,
     405,  2039,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,    -2,     0,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,
       0,     0,    -2,     0,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,    -2,    -2,  2067,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,     0,     0,    -2,     0,     0,    -2,     0,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,    -2,    -2,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,    59,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    61,
      62,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,    76,
       0,    77,    78,     0,     0,     0,     0,     0,     0,    79,
      80,   257,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -483,  -483,     0,  -483,    46,
       0,    47,     0,  -483,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   148,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,    76,     0,    77,   258,     0,     0,     0,  -815,     0,
       0,    79,    80,   257,   183,     6,     7,     8,     9,    10,
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
       0,     0,     0,    79,    80,     4,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,     0,     0,     0,     0,  -406,
    -406,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -406,     0,     0,     0,    77,    78,     0,
       0,     0,     0,     0,     0,    79,    80,     4,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,     0,     0,     0,
       0,  -407,  -407,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -407,     0,     0,     0,    77,
      78,     0,  1421,     0,  1422,     0,     0,    79,    80,  1423,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,  1424,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1425,     0,     0,     0,    77,   982,     0,  1421,     0,  1422,
       0,     0,    79,    80,  1423,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,  1424,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1631,     0,     0,     0,    77,
     982,     0,  1421,     0,  1422,     0,     0,    79,    80,  1423,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,  1424,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1632,     0,     0,     0,    77,   982,     0,  1421,     0,  1422,
       0,     0,    79,    80,  1423,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,  1424,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1633,     0,     0,     0,    77,
     982,     0,     0,     0,     0,     0,     0,    79,    80,   257,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -483,  -483,     0,  -483,    46,     0,    47,
       0,  -483,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   258,     0,     0,     0,     0,     0,     0,    79,
      80,   183,     6,     7,     8,     9,    10,    11,    12,    13,
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
      76,     0,    77,   258,     0,     0,     0,  -819,     0,     0,
      79,    80,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -483,  -483,     0,  -483,    46,
       0,    47,     0,  -483,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   148,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,    76,     0,    77,   258,     0,     0,     0,     0,     0,
       0,    79,    80,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   340,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,  1107,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -679,    77,   342,     0,     0,     0,     0,
       0,     0,    79,    80,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   340,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   341,    77,   342,     0,     0,     0,
       0,     0,     0,    79,    80,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   340,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,  1927,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    77,   342,     0,     0,
       0,     0,     0,     0,    79,    80,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   340,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,  1929,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   342,     0,
       0,     0,     0,     0,     0,    79,    80,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     340,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,   322,
       0,     0,     0,     0,     0,     0,    79,    80,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   340,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
     342,     0,     0,     0,     0,     0,     0,    79,    80,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -483,  -483,     0,  -483,    46,     0,    47,     0,
    -483,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,  1446,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   383,     0,
       0,   384,     0,   385,     0,   386,     0,     0,     0,     0,
      77,   258,     0,     0,     0,     0,     0,     0,    79,    80,
    1185,     0,   387,    -2,     0,  1187,  1954,  1955,  1188,  1189,
    1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,  1198,  1199,
       0,     0,  1200,  1201,  1202,  1203,  1204,     0,  1205,     0,
     388,   389,     0,   486,   391,  1206,  1207,    65,    66,    67,
      68,    69,    70,    71,    72,   392,   393,   380,  1208,   394,
     395,   396,  1446,   397,   398,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1209,     0,   383,    77,   400,   384,     0,   385,   298,
     386,   401,    79,    80,   402,   403,   404,   405,     0,     0,
       0,     0,     0,     0,     0,  1185,  -185,   387,    -2,     0,
    1187,     0,     0,  1188,  1189,  1190,  1191,  1192,  1193,  1194,
    1195,  1196,  1197,  1198,  1199,     0,     0,  1200,  1201,  1202,
    1203,  1204,     0,  1205,     0,   388,   389,     0,   486,   391,
    1206,  1207,    65,    66,    67,    68,    69,    70,    71,    72,
     392,   393,   380,  1208,   394,   395,   396,     0,   397,   398,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1209,     0,     0,    77,
     400,     0,     0,     0,   298,     0,   401,    79,    80,   402,
     403,   404,   405,     0,     0,     0,     0,     0,     0,     0,
       0,  -185,    14,    15,    16,    17,    18,    19,   704,    20,
     705,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   383,     0,
      46,   384,    47,   385,     0,   386,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   387,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   706,     0,     0,     0,     0,  1199,
       0,  -338,     0,     0,     0,     0,     0,     0,     0,     0,
     388,   389,     0,   390,   391,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   392,   393,   380,     0,   394,
     395,   396,     0,   397,   398,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1209,     0,     0,    77,   707,     0,     0,     0,   298,
       0,   401,    79,    80,   708,   709,   404,   405,    14,    15,
      16,    17,    18,    19,   704,    20,   705,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   383,     0,    46,   384,    47,   385,
       0,   386,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   387,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     706,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   388,   389,     0,   390,
     391,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   392,   393,   380,     0,   394,   395,   396,     0,   397,
     398,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   399,     0,     0,
      77,   707,     0,     0,     0,   298,     0,   401,    79,    80,
     708,   709,   404,   405,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     383,     0,    46,   384,    47,   385,     0,   386,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   387,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   388,   389,     0,   390,   391,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   392,   393,   380,
       0,   394,   395,   396,     0,   397,   398,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   399,     0,   430,    77,   431,     0,     0,
       0,     0,     0,   401,    79,    80,   402,   403,   404,   405,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   383,     0,    46,   384,
      47,   385,     0,   386,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     387,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   388,   389,
       0,   390,   391,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   392,   393,   380,     0,   394,   395,   396,
       0,   397,   398,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   399,
       0,     0,    77,   707,     0,     0,     0,   298,     0,   401,
      79,    80,   402,   403,   404,   405,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   383,     0,    46,   384,    47,   385,     0,   386,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   387,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   388,   389,     0,   390,   391,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   392,
     393,   380,     0,   394,   395,   396,     0,   397,   398,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   399,     0,     0,    77,   431,
       0,     0,     0,     0,     0,   401,    79,    80,   402,   403,
     404,   405,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   383,     0,
      46,   384,    47,   385,     0,   386,   340,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   387,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     388,   389,     0,   390,   391,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   392,   393,   380,     0,   394,
     395,   396,     0,   397,   398,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   399,     0,     0,    77,   461,     0,     0,     0,     0,
       0,   401,    79,    80,   402,   403,   404,   405,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   383,     0,    46,   384,    47,   385,
       0,   386,   340,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   387,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   388,   389,     0,   390,
     391,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   392,   393,   380,     0,   394,   395,   396,     0,   397,
     398,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   399,     0,     0,
      77,   400,     0,     0,     0,     0,     0,   401,    79,    80,
     402,   403,   404,   405,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   695,     0,   696,   697,   589,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,   -17,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
      77,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   148,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,    76,     0,    77,
      78,     0,     0,     0,  -817,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,    76,
       0,    77,    78,     0,     0,     0,     0,     0,     0,    79,
      80,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   148,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,    79,    80,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   340,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   902,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -692,    77,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   340,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1836,     0,     0,     0,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,    77,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   340,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
      77,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   988,    77,
     982,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,  1543,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   982,     0,     0,     0,     0,     0,     0,    79,
      80,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   305,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,    77,    78,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,    77,   457,     0,
       0,     0,     0,     0,     0,    79,    80,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   340,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
     342,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   340,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   305,     0,     0,     0,     0,     0,     0,    79,
      80,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   340,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   457,     0,     0,     0,     0,     0,
       0,    79,    80,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   340,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,   322,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,    77,   982,     0,
       0,     0,     0,     0,     0,    79,    80,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,    58,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,   340,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
     982,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   340,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,     0,     0,    14,    15,    16,    17,    18,    79,
      80,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -483,  -483,
       0,  -483,    46,     0,    47,     0,  -483,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    77,   322,     0,    14,
      15,    16,    17,    18,    79,    80,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -483,  -483,     0,  -483,    46,     0,    47,
       0,  -483,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,     0,    63,    64,     0,     0,     0,     0,    79,
      80,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     383,     0,    46,   384,    47,   385,     0,   386,     0,     0,
       0,     0,    77,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   388,   389,     0,   390,   391,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   392,   393,   380,
       0,   394,   395,   396,     0,   397,   398,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   399,     0,     0,    77,   400,     0,     0,
       0,     0,     0,   401,   462,    80,   402,   403,   404,   405,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   383,
       0,    46,   384,    47,   385,     0,   386,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   387,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   388,   389,     0,   390,   391,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   392,   393,   380,     0,
     394,   395,   396,     0,   397,   398,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   399,     0,     0,    77,   400,     0,     0,     0,
       0,     0,   401,    79,    80,   402,   403,   404,   405,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    77,    47,     0,     0,     0,   340,    49,    50,    51,
      52,    53,    54,    55,     0,   383,     0,     0,   384,     0,
     385,    58,   386,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   387,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   388,   389,     0,
     390,   391,  1969,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   392,   393,   380,     0,   394,   395,   396,     0,
     397,   398,   383,     0,     0,   384,     0,   385,    74,   386,
       0,     0,     0,     0,    77,     0,     0,     0,     0,     0,
       0,     0,  1676,  1677,  1678,     0,   387,     0,   399,  1970,
       0,    77,   400,     0,     0,     0,     0,     0,   401,    79,
      80,   402,   403,   404,   405,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   388,   389,     0,   390,   391,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   392,
     393,   380,     0,   394,   395,   396,   383,   397,   398,   384,
       0,   385,     0,   386,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,     0,     0,   399,  1317,     0,    77,   400,
       0,     0,     0,  1318,     0,   401,    79,    80,   402,   403,
     404,   405,     0,     0,     0,     0,     0,     0,   388,   389,
       0,   390,   391,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   392,   393,   380,     0,   394,   395,   396,
     383,   397,   398,   384,     0,   385,     0,   386,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,     0,     0,     0,     0,   399,
     838,     0,    77,   400,     0,     0,     0,     0,     0,   401,
      79,    80,   402,   403,   404,   405,     0,     0,     0,     0,
       0,     0,   388,   389,     0,   390,   391,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   392,   393,   380,
       0,   394,   395,   396,   383,   397,   398,   384,     0,   385,
       0,   386,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,     0,
       0,     0,     0,   399,     0,     0,    77,   400,     0,     0,
       0,   298,     0,   401,    79,    80,   402,   403,   404,   405,
       0,     0,     0,     0,     0,     0,   388,   389,     0,   390,
     391,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   392,   393,   380,     0,   394,   395,   396,   383,   397,
     398,   384,     0,   385,     0,   386,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,     0,     0,     0,     0,   399,  1015,     0,
      77,   400,     0,     0,     0,     0,     0,   401,    79,    80,
     402,   403,   404,   405,     0,     0,     0,     0,     0,     0,
     388,   389,     0,   390,   391,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   392,   393,   380,     0,   394,
     395,   396,   383,   397,   398,   384,     0,   385,     0,   386,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   387,     0,     0,     0,
       0,   399,     0,     0,    77,   400,     0,     0,  1046,     0,
       0,   401,    79,    80,   402,   403,   404,   405,     0,     0,
       0,     0,     0,     0,   388,   389,     0,   390,   391,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   392,
     393,   380,     0,   394,   395,   396,   383,   397,   398,   384,
       0,   385,     0,   386,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,     0,     0,   399,  1384,     0,    77,   400,
       0,     0,     0,     0,     0,   401,    79,    80,   402,   403,
     404,   405,     0,     0,     0,     0,     0,     0,   388,   389,
       0,   390,   391,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   392,   393,   380,     0,   394,   395,   396,
     383,   397,   398,   384,     0,   385,     0,   386,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,     0,     0,     0,     0,   399,
       0,     0,    77,   400,     0,     0,     0,  1456,     0,   401,
      79,    80,   402,   403,   404,   405,     0,     0,     0,     0,
       0,     0,   388,   389,     0,   390,   391,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   392,   393,   380,
       0,   394,   395,   396,   383,   397,   398,   384,     0,   385,
       0,   386,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,     0,
       0,     0,     0,   399,     0,     0,    77,   400,     0,     0,
       0,  1533,     0,   401,    79,    80,   402,   403,   404,   405,
       0,     0,     0,     0,     0,     0,   388,   389,     0,   390,
     391,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   392,   393,   380,     0,   394,   395,   396,   383,   397,
     398,   384,     0,   385,     0,   386,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,     0,     0,     0,     0,   399,     0,  1960,
      77,   400,     0,     0,     0,     0,     0,   401,    79,    80,
     402,   403,   404,   405,     0,     0,     0,     0,     0,     0,
     388,   389,     0,   390,   391,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   392,   393,   380,     0,   394,
     395,   396,   383,   397,   398,   384,     0,   385,     0,   386,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   387,     0,     0,     0,
       0,   399,  1965,     0,    77,   400,     0,     0,     0,     0,
       0,   401,    79,    80,   402,   403,   404,   405,     0,     0,
       0,     0,     0,     0,   388,   389,     0,   390,   391,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   392,
     393,   380,     0,   394,   395,   396,   383,   397,   398,   384,
       0,   385,     0,   386,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,     0,     0,   399,  1974,     0,    77,   400,
       0,     0,     0,     0,     0,   401,    79,    80,   402,   403,
     404,   405,     0,     0,     0,     0,     0,     0,   388,   389,
       0,   390,   391,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   392,   393,   380,     0,   394,   395,   396,
     383,   397,   398,   384,     0,   385,     0,   386,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,     0,     0,     0,     0,   399,
    2061,     0,    77,   400,     0,     0,     0,     0,     0,   401,
      79,    80,   402,   403,   404,   405,     0,     0,     0,     0,
       0,     0,   388,   389,     0,   390,   391,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   392,   393,   380,
       0,   394,   395,   396,   383,   397,   398,   384,     0,   385,
       0,   386,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,     0,
       0,     0,     0,   399,  2063,     0,    77,   400,     0,     0,
       0,     0,     0,   401,    79,    80,   402,   403,   404,   405,
       0,     0,     0,     0,     0,     0,   388,   389,     0,   390,
     391,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   392,   393,   380,     0,   394,   395,   396,   383,   397,
     398,   384,     0,   385,     0,   386,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,     0,     0,     0,     0,   399,  2115,     0,
      77,   400,     0,     0,     0,     0,     0,   401,    79,    80,
     402,   403,   404,   405,     0,     0,     0,     0,     0,     0,
     388,   389,     0,   390,   391,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   392,   393,   380,     0,   394,
     395,   396,   383,   397,   398,   384,     0,   385,     0,   386,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   387,     0,     0,     0,
       0,   399,  2117,     0,    77,   400,     0,     0,     0,     0,
       0,   401,    79,    80,   402,   403,   404,   405,     0,     0,
       0,     0,     0,     0,   388,   389,     0,   390,   391,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   392,
     393,   380,     0,   394,   395,   396,   383,   397,   398,   384,
       0,   385,     0,   386,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,     0,     0,   399,  2119,     0,    77,   400,
       0,     0,     0,     0,     0,   401,    79,    80,   402,   403,
     404,   405,     0,     0,     0,     0,     0,     0,   388,   389,
       0,   390,   391,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   392,   393,   380,     0,   394,   395,   396,
     383,   397,   398,   384,     0,   385,     0,   386,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,     0,     0,     0,     0,   399,
    2122,     0,    77,   400,     0,     0,     0,     0,     0,   401,
      79,    80,   402,   403,   404,   405,     0,     0,     0,     0,
       0,     0,   388,   389,     0,   390,   391,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   392,   393,   380,
       0,   394,   395,   396,   383,   397,   398,   384,     0,   385,
       0,   386,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,     0,
       0,     0,     0,   399,  2124,     0,    77,   400,     0,     0,
       0,     0,     0,   401,    79,    80,   402,   403,   404,   405,
       0,     0,     0,     0,     0,     0,   388,   389,     0,   390,
     391,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   392,   393,   380,     0,   394,   395,   396,   383,   397,
     398,   384,     0,   385,     0,   386,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,     0,     0,     0,     0,   399,  2168,     0,
      77,   400,     0,     0,     0,     0,     0,   401,    79,    80,
     402,   403,   404,   405,     0,     0,     0,     0,     0,     0,
     388,   389,     0,   390,   391,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   392,   393,   380,     0,   394,
     395,   396,   383,   397,   398,   384,     0,   385,     0,   386,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   387,     0,     0,     0,
       0,   399,  2170,     0,    77,   400,     0,     0,     0,     0,
       0,   401,    79,    80,   402,   403,   404,   405,     0,     0,
       0,     0,     0,     0,   388,   389,     0,   390,   391,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   392,
     393,   380,     0,   394,   395,   396,   383,   397,   398,   384,
       0,   385,     0,   386,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,     0,     0,   399,  2172,     0,    77,   400,
       0,     0,     0,     0,     0,   401,    79,    80,   402,   403,
     404,   405,     0,     0,     0,     0,     0,     0,   388,   389,
       0,   390,   391,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   392,   393,   380,     0,   394,   395,   396,
     383,   397,   398,   384,     0,   385,     0,   386,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,     0,     0,     0,     0,   399,
    2197,     0,    77,   400,     0,     0,     0,     0,     0,   401,
      79,    80,   402,   403,   404,   405,     0,     0,     0,     0,
       0,     0,   388,   389,     0,   390,   391,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   392,   393,   380,
       0,   394,   395,   396,   383,   397,   398,   384,     0,   385,
       0,   386,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,     0,
       0,     0,     0,   399,  2199,     0,    77,   400,     0,     0,
       0,     0,     0,   401,    79,    80,   402,   403,   404,   405,
       0,     0,     0,     0,     0,     0,   388,   389,     0,   390,
     391,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   392,   393,   380,     0,   394,   395,   396,   383,   397,
     398,   384,     0,   385,     0,   386,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,     0,     0,     0,     0,   399,  2201,     0,
      77,   400,     0,     0,     0,     0,     0,   401,    79,    80,
     402,   403,   404,   405,     0,     0,     0,     0,     0,     0,
     388,   389,     0,   390,   391,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   392,   393,   380,     0,   394,
     395,   396,   383,   397,   398,   384,     0,   385,     0,   386,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   387,     0,     0,     0,
       0,   399,     0,     0,    77,   400,     0,     0,     0,     0,
       0,   401,    79,    80,   402,   403,   404,   405,     0,     0,
       0,     0,     0,     0,   388,   389,     0,   390,   391,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   392,
     393,   380,     0,   394,   395,   396,   383,   397,   398,   384,
       0,   385,     0,   386,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,     0,     0,   686,     0,     0,    77,   400,
       0,     0,     0,     0,     0,   401,    79,    80,   402,   403,
     404,   405,     0,     0,     0,     0,     0,     0,   388,   389,
       0,   390,   391,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   392,   393,   380,     0,   394,   395,   396,
     383,   397,   398,   384,     0,   385,     0,   386,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,     0,     0,     0,     0,   692,
       0,     0,    77,   400,     0,     0,     0,     0,     0,   401,
      79,    80,   402,   403,   404,   405,     0,     0,     0,     0,
       0,     0,   388,   389,     0,   390,   391,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   392,   393,   380,
       0,   394,   395,   396,   383,   397,   398,   384,     0,   385,
       0,   386,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,     0,
       0,     0,     0,   701,     0,     0,    77,   400,     0,     0,
       0,     0,     0,   401,    79,    80,   402,   403,   404,   405,
       0,     0,     0,     0,     0,     0,   388,   389,     0,   390,
     391,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   392,   393,   380,     0,   394,   395,   396,   383,   397,
     398,   384,     0,   385,     0,   386,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,     0,     0,     0,     0,   399,     0,     0,
      77,   400,     0,     0,     0,     0,     0,   401,   918,    80,
     402,   403,   404,   405,     0,     0,     0,     0,     0,     0,
     388,   389,     0,   390,   391,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   392,   393,   380,     0,   394,
     395,   396,   383,   397,   398,   384,     0,   385,     0,   386,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   387,     0,     0,     0,
       0,   399,     0,     0,    77,   400,     0,     0,     0,     0,
       0,   401,   462,    80,   402,   403,   404,   405,     0,     0,
       0,     0,     0,     0,   388,   389,     0,   390,   391,  2056,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   392,
     393,   380,     0,   394,   395,   396,     0,   397,   398,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   399,     0,     0,    77,   400,
       0,     0,     0,     0,     0,   401,    79,    80,   402,   403,
     404,   405,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   184,     0,   185,   186,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   695,     0,   696,   697,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -483,  -483,     0,  -483,    46,     0,
      47,     0,  -483,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,     1,     1,     4,   166,    75,    75,     4,   177,   584,
     399,   230,   166,   927,   302,    75,   733,   271,   230,   639,
     256,   218,   230,  1227,   230,   251,    75,   489,   230,   648,
    1194,   230,   230,  1209,   401,   230,    77,   718,   667,    75,
     635,   373,   230,   230,   167,  1810,   606,  1809,  1368,  1369,
     635,  1169,   345,   360,  1809,    56,    57,   364,    59,    59,
      59,   240,    75,    59,   635,     1,   546,   547,     4,  1209,
       1,  1809,   711,   141,    75,     1,    84,     1,   806,   777,
       4,   807,    75,    84,   804,    84,  1178,   813,  1448,   268,
     806,    92,   804,   557,   804,   314,    97,     1,    97,   100,
     279,   100,   314,   104,   568,   104,   314,   104,   314,   895,
    1021,   233,   314,   235,   911,   314,   314,  1686,    72,   314,
     242,    10,   192,    59,    98,   181,   314,   314,    59,   804,
      72,  1954,   192,    59,   202,    59,   182,     1,   924,  1050,
     824,   142,   150,   192,   145,   609,   147,   147,   147,  1326,
       0,   147,   153,    99,     1,    75,   192,    75,   842,   160,
     230,   230,  1166,   134,   100,    72,   167,    89,   104,  1173,
     230,   166,  1448,  1449,    84,   118,   469,    89,  1943,   192,
     104,   230,     1,   152,   337,   154,   806,   309,   310,  1178,
     191,   192,   191,   673,   230,    59,     0,   168,   152,   192,
     829,   387,  1113,   177,   182,   206,   152,   206,  1958,   804,
     152,   147,    59,   156,   156,   216,   147,   230,    10,   804,
     221,   147,   160,   147,   377,    97,   412,   413,   133,   230,
     231,    89,   231,   804,   399,    90,   920,   230,   160,   177,
     104,     0,   152,  1178,   314,   314,  1250,   433,   160,   156,
     251,   259,   293,     1,   314,   159,     4,   177,   259,   177,
     259,   978,   151,   823,   824,   314,   823,   824,   269,   270,
     175,     1,   273,   162,   273,    97,   530,   463,   167,   280,
    2103,  1682,   842,   147,   538,   842,  1855,   110,   507,   375,
    1007,   134,   378,   294,   295,   507,   297,   152,   495,   507,
     147,   507,   160,   878,   784,   507,  2071,   595,   507,   507,
     133,    59,   507,   314,   315,   323,   315,   624,   544,   507,
     507,   688,   109,   324,   550,   168,  1418,  1419,  1420,    59,
     331,   332,   952,   158,   627,   336,    84,   273,   670,   158,
     159,   970,   731,  1483,   467,   132,  1486,  1487,   343,    89,
    1710,  1077,   177,   118,   661,  1083,  1076,   996,   651,   231,
     920,  1059,   152,   920,  1076,   658,  1076,  1083,   152,   944,
     162,  2136,   110,  2135,   375,   167,   302,   378,   379,  2106,
    2135,    77,    78,   602,  1170,  1562,  1563,  1564,   567,  1186,
     602,   459,   176,   141,   602,   133,   602,  2135,   911,   147,
     602,  1076,   150,   602,   602,  2132,   155,   602,  2158,   231,
     152,  1415,    62,  2178,   602,   602,   154,   147,   158,  1216,
     160,  1539,     4,   106,   107,   433,    20,   176,   892,  1418,
    1419,  1420,   992,  2160,  1710,    72,   160,   507,   302,   152,
      97,  1125,   443,   315,   443,  2195,   487,   507,    61,    62,
     100,   156,   606,   177,   202,   302,   161,   123,   507,  1860,
    1861,   111,   112,  1083,   160,   466,   467,   152,   463,  1789,
    1790,   507,   155,   669,    56,    57,   152,   478,   479,   146,
    1055,  1076,   343,  1418,  1419,  1420,   487,    72,   489,   155,
     152,  1076,    75,   315,   507,   641,    75,  1072,  1073,   360,
     623,   168,   157,   364,   154,  1076,   507,    90,    75,   257,
      92,   259,    91,   177,   507,   152,    72,  1681,   519,   156,
     519,   686,  1686,    90,   689,   690,   151,   692,    75,   108,
     158,    58,   602,   158,    61,    62,   701,    64,   158,   704,
     705,   706,   602,   544,  1720,    92,    62,   160,   168,   550,
    1276,   737,   177,   602,   302,   177,   206,    77,    78,   239,
     142,   108,     1,   145,   177,  1125,   246,   152,  1125,  1970,
    1971,   156,   302,     3,   231,   323,    72,   154,   160,   160,
    1720,   158,  1858,   158,   100,   167,   167,   267,   589,   280,
     591,  1053,   591,   168,    72,   111,   152,   113,   278,   115,
     156,   602,   463,   604,   295,   604,    69,   608,   608,   608,
     903,   606,   608,   146,   147,   148,    72,   618,   844,   158,
      59,   622,   623,   273,   158,   152,    72,   158,     3,   168,
     158,  1357,   939,  1262,   831,   168,    72,   168,   154,   221,
     160,   157,   158,   177,   870,   158,  1557,   519,   941,   177,
     134,  1270,   155,   648,   655,  1336,   152,   307,   315,    72,
     156,   100,   808,   313,   177,  1294,   812,   668,    72,   595,
     158,    72,   608,  1186,   152,   821,   822,   608,   156,   163,
     164,   158,   608,   806,   608,   433,    72,   269,   270,   177,
     206,  1855,   158,    72,   160,   345,   152,   519,   280,   625,
     156,  2021,   758,  1216,   158,   152,   152,   158,   147,   158,
     156,   459,   294,   295,  1343,   297,   152,   168,   759,   591,
     156,  1438,   723,   177,   725,   160,   727,  1641,   177,   593,
     731,   595,   604,   734,   158,   149,   128,   129,   934,   152,
     390,  1301,   324,   156,   608,   160,   160,   176,   595,   331,
     332,   152,   167,   177,   336,   156,   152,   273,   759,   275,
     276,   608,   176,   624,  2040,    72,   152,   158,    72,   591,
     156,   817,   154,   152,  1349,  1350,   972,   156,  1459,  1259,
     172,   173,   604,   165,   166,   156,   177,   648,  1363,  1364,
     161,   307,  2068,   375,   154,  1999,   378,   313,  2002,   159,
     661,    72,   988,   804,  1968,   806,   158,   993,  1368,    13,
      14,    15,    16,    17,   163,   871,  1980,   818,  1004,   469,
    1209,   170,   171,   160,   825,   177,  1401,  1402,  1403,   345,
     831,  2107,    72,   834,   273,   351,   486,   353,   992,   817,
     963,   589,   843,   844,   845,   152,   151,   595,   152,   156,
     828,   152,   156,   158,   534,   174,  1318,    72,  1112,   160,
     608,  1772,   519,  1774,   152,   595,    75,   158,    72,   870,
     154,   159,   156,  1554,   390,   801,   153,   168,   608,   559,
    1509,   152,   151,   160,   466,   156,   566,   152,    97,   158,
     570,  2055,   106,   107,   132,   160,   478,   479,  1527,   154,
     154,   551,   167,   160,   159,   159,   345,  1536,   909,   910,
     911,   911,   152,   154,   152,   154,   156,   158,   156,   132,
     159,  1541,   572,  1120,  1553,   163,   164,   443,   152,   926,
     134,   152,  1264,  1265,   591,  1448,  1449,   152,   176,   152,
    1634,   156,   592,   156,   153,  1639,   118,   604,   106,   107,
     163,   164,  1512,   469,  1100,   471,   472,   155,   156,  1082,
    1083,   159,   963,   154,    13,    14,    15,    16,    17,   154,
     486,    47,    48,   158,    50,   911,   977,   627,   154,    55,
     911,  1274,   146,   147,   148,   911,   154,   911,   154,    58,
     926,   154,    61,    62,  1456,    64,   158,   992,   154,   155,
    1186,   651,   926,   519,   168,   152,   154,   589,   658,   156,
     158,   154,  1013,   177,   152,    13,    14,    15,    16,    17,
    1021,   230,   231,    72,   738,   739,   740,  1253,   544,  1151,
     469,   165,   166,   549,  1423,   551,   618,  1612,  1613,  1614,
     622,   623,   251,  1279,  1209,   152,   152,   911,   154,  1050,
     156,   152,  1053,   154,   152,   156,   572,    22,   574,   575,
     152,   152,  1691,   154,   911,   156,   154,  1263,  1046,   158,
     158,  1533,   158,   655,    72,  1076,   592,   158,   939,   126,
     127,  1082,  1083,  1658,  1270,   134,   668,  1283,   604,    99,
    1552,  1260,  1261,   203,   157,   158,   132,  1293,    13,    14,
      15,    16,    17,   154,   154,   314,   315,   158,   158,   130,
     131,   627,  1113,   629,   630,   152,   152,   154,   158,   156,
     156,  1750,  1308,  1309,  1310,   152,  1755,   163,   164,  1315,
    1316,  1631,  1632,  1633,   132,   651,   652,  1766,   157,   158,
    1305,   723,   658,   725,  1122,   727,  1300,  1301,   152,   731,
    1404,   132,   734,   592,   152,   154,    89,    72,   156,   158,
    1448,   163,   164,   911,   132,   163,   164,   160,  1743,   608,
    1178,   152,  1169,   157,   158,   156,   152,   759,   926,   151,
     156,   911,   163,   164,   152,   154,  1186,  1188,   156,   158,
    1191,  1192,  1193,   157,   158,   163,   164,  1710,  1530,   157,
     158,   146,   147,   148,  1779,   157,   158,  1369,   154,  1784,
    1785,   160,   651,   158,   894,  1216,  1216,   160,   152,   158,
     159,  1222,   156,   168,   874,   157,   152,   154,   176,  1789,
     156,   158,   177,  1169,  1235,   160,   818,  1238,  1239,  1238,
    1239,  1242,  1239,   825,  1290,  1169,   167,   154,   157,   158,
    1186,   158,  1253,   903,   154,  1186,   118,     3,   467,   152,
    1186,   843,  1186,   845,  1893,  1894,   916,    13,    14,    15,
      16,    17,   152,   154,   152,  1270,   156,   158,  1995,   152,
    1216,   152,   146,   147,   148,  1216,   158,  1288,   164,   154,
    1216,   941,  1216,   158,   158,   154,   154,   174,   507,   158,
     158,  1302,  1238,  1239,   168,   154,  1301,   154,   169,   158,
     519,   158,  1290,   177,   154,  1239,   154,  1318,   158,   154,
     158,   162,  1186,   158,   132,  1326,    72,   909,   910,   911,
    1376,  1377,  1528,   154,   154,   544,     3,   158,   158,  1186,
     154,   550,    91,    92,   158,  1858,    13,    14,    15,    16,
      17,   154,  1216,   157,   158,   158,   154,  1358,  1512,   155,
     158,  1339,    13,    14,    15,    16,    17,    18,   154,  1216,
     111,   112,   113,   114,   115,  1239,   154,  1175,  1176,  1177,
     158,   963,   591,  2012,   154,  2140,   154,   903,   158,  2144,
    1652,  1653,  1654,   602,   154,   604,   157,   158,  1376,  1377,
     916,   745,   746,   747,   748,    72,   157,   158,   156,  1270,
    1418,  1419,  1420,   157,   158,  1423,  1424,   157,   158,  1751,
     154,  1169,  1710,   154,  1421,   941,  1645,   154,  1429,   157,
    1178,   134,  1433,  1645,   134,  1436,   952,  1645,  1186,  1645,
     157,   158,   552,  1645,   159,   961,  1645,  1645,  1448,  1449,
    1645,   157,   158,   157,   158,  1456,  1186,  1645,  1645,   157,
     158,  2090,   157,   158,   903,   157,   158,   158,  1216,   157,
     158,   152,   911,   158,   159,   157,   158,   916,  1479,  1480,
     157,   158,    77,    78,   159,  1421,  1216,   176,  1489,   154,
    1489,    13,    14,    15,    16,    17,    18,  1421,   158,   159,
    1327,  1328,   941,     4,     5,     6,     7,     8,     9,    10,
      11,    12,  1448,  1449,   154,   625,   154,  1512,   741,   742,
     749,   750,  1448,  1449,  1448,  1449,   154,  2040,   154,  1715,
    2085,  2086,  1533,    13,    14,    15,    16,    17,    18,   154,
    1736,  1737,  1539,  1059,    13,    14,    15,    16,    17,    18,
     154,  1552,   154,  1489,   154,  2068,  1557,  1429,   152,   743,
     744,  1562,  1563,  1564,    65,   157,  1728,  1083,   156,  1486,
    1487,  1653,  1654,   160,  1728,   160,  1645,   160,  1531,  1532,
    2155,  1260,  1261,   160,  1448,  1449,   160,    70,  1238,   157,
     152,    78,   157,    18,  2107,   804,   176,   806,   158,  1764,
     152,  1448,  1449,  1539,   160,  1824,  1188,  1429,   177,  1191,
    1192,  1193,  1824,   154,   154,  1539,  1824,   160,  1824,   177,
     157,   160,  1824,   157,  1274,  1824,  1824,    18,  1790,  1824,
     151,   154,    22,  1634,  1216,   844,  1824,  1824,  1639,   154,
    1222,   154,   154,   154,  1645,   154,  1647,   154,   154,   154,
     760,   154,  1708,  1235,  1655,   154,   151,   151,   160,    70,
    1242,   870,   160,   160,   154,  1891,  1667,   177,   176,   160,
    1418,  1419,  1420,  1421,  1422,  1423,  1424,   154,  1675,  1680,
     154,   154,    84,   151,   176,   160,   157,   154,   160,   154,
     176,   801,   158,   154,   158,    97,   154,   154,   100,   158,
    1448,  1449,   104,   154,   154,   154,  1288,   154,  1904,   154,
    1710,   157,   154,   823,   824,   154,   154,   154,  1448,  1449,
    1302,   154,  1238,   154,  1725,   154,   154,   157,   154,   151,
     154,   154,   842,  1728,  1731,   151,   154,   158,  1674,  1675,
     152,   158,   152,   152,   152,   152,   152,  1186,    14,    74,
     177,  1675,    80,   157,   159,   158,   157,   177,  1274,   159,
    1410,   158,  1634,   177,  1280,   160,   121,  1639,   123,   124,
     125,  1772,  1429,  1774,  1710,  1647,   151,  1216,   177,  2072,
    1430,   151,   160,   177,  1710,   154,  1710,   158,   154,   191,
     157,  1539,   154,   158,   158,  1731,   157,   152,   154,  1238,
     155,   156,   154,   157,   206,   160,   161,  1731,  1994,   151,
     920,  1675,  1634,   151,   177,  2011,   152,  1639,  1874,   177,
     152,    92,   152,  1824,   177,  1647,  1827,   177,   177,   231,
     177,   177,   151,   177,   177,  1274,   152,   152,  1839,  1489,
      90,   154,  1843,   151,   151,  2071,  1710,     1,   158,   158,
       4,  1433,   151,   157,  1436,   160,  1857,   259,  1858,  2021,
     160,   157,   157,  1710,   154,   157,  1867,  1076,   151,   154,
     159,   273,   159,  1082,  1083,   121,   151,   154,   154,  1880,
     154,  1882,  1883,  1884,  1885,   154,   154,   157,   151,  1890,
    1891,   151,   157,   177,   152,   159,   158,  1479,  1480,   154,
     152,   152,   151,   157,   157,    59,   151,   151,  1954,  1845,
     157,   154,   160,   151,  1430,   154,  2135,   154,   154,   154,
     154,    75,  1858,  2135,   157,   154,   154,  2135,    75,  2135,
      84,    75,  1858,  2135,  1858,  1991,  2135,  2135,   151,   154,
    2135,   152,   154,    97,   152,   177,   100,  2135,  2135,   151,
     104,  1952,   177,   177,   157,  1827,   157,  1958,   151,   151,
     160,  1962,  1710,   151,   154,   154,  1967,   154,   154,   154,
     154,   154,    75,  1489,   156,    75,  1954,  1634,   155,  1987,
    1710,   177,  1639,  1731,   159,   151,   177,   141,   168,   168,
    1647,  1992,   151,   147,  1858,   158,   150,   154,   154,   153,
     154,   177,   154,   154,   153,  1827,   151,   168,   168,  1448,
    1449,  1858,   166,   151,   104,  1125,   159,   152,   158,    75,
     151,   153,   168,   104,  1674,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   168,   190,   191,   192,   157,
    2040,   443,  2043,   177,  1253,    75,  2047,   177,   202,   203,
    1489,    75,   206,   151,   153,  2101,    62,  2103,   154,  2060,
     154,  2069,   159,   151,   151,  2135,  2135,   133,  2068,   154,
    2071,   177,  2073,  1655,   152,  2135,   230,   231,   154,   154,
    1952,  1333,   163,  1764,  2140,  1667,  2135,   710,  2144,  2145,
     751,   177,   754,   177,   752,  1204,  2142,   251,  1680,   105,
     753,   755,  1216,   109,  2040,   259,   112,  2107,   114,  2110,
    1858,   432,  2195,  1710,  2040,  2103,  2040,   519,  2132,   273,
    1866,  1858,  1722,  2101,  2180,  2103,  1987,  1719,  1858,  1701,
    1952,  2175,  2068,  1701,  2135,  2136,  2069,  2136,  2145,  2211,
    2068,    49,  2068,  1725,  2068,  2146,  1242,   112,  2149,  1358,
    2151,  2207,   264,   307,  2030,  2211,   222,  2158,  1674,   313,
     314,   315,  1424,  1952,  2142,   967,   930,  1235,   831,   323,
    1827,  2107,   496,  2229,   618,    84,  2040,  2178,  2224,  2178,
    1731,  2107,     0,  2107,   977,   776,  2187,   776,  2189,   343,
     344,   345,  2193,  2040,  2195,  1845,  1622,   776,  2176,    -1,
      -1,    -1,    -1,    -1,  2068,    -1,   360,    -1,  2069,    -1,
     364,    -1,    -1,    -1,    -1,    -1,    -1,  2218,    -1,    -1,
    1429,  2068,    -1,    -1,    13,    14,    15,    16,    17,  2230,
      -1,    -1,    -1,    -1,    -1,  1674,    -1,   303,  2239,    -1,
      -1,   150,    -1,  2107,   104,   399,  2224,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   262,  1839,  1368,  1369,
    2107,  1843,    -1,    -1,  2136,    -1,    -1,    -1,    -1,    -1,
      -1,  1710,    -1,    -1,    -1,  1857,    -1,    -1,    -1,   433,
      -1,   190,   436,    72,    -1,  1867,    -1,    -1,    -1,   443,
      -1,    -1,  2040,   153,    -1,  1952,   156,    -1,  1880,    -1,
    1882,  1883,  1884,  1885,    -1,   459,  2178,    -1,  1890,   463,
    2040,  1827,    -1,   467,  2136,   469,    -1,    -1,    -1,    -1,
    2068,   216,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1845,
      -1,    -1,    -1,   399,    -1,    -1,    -1,    -1,  2068,    -1,
      -1,   347,   348,   132,   350,    -1,   352,    -1,    -1,    -1,
     259,    -1,    -1,   507,    -1,    -1,  2178,   203,    -1,  2107,
      -1,    -1,    -1,   152,    -1,   519,    -1,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,  1958,  2107,    -1,    -1,
    1962,    -1,    -1,    -1,   390,  1967,    -1,    -1,    -1,    -1,
     544,    -1,   546,   547,    -1,    -1,   550,    -1,   552,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1845,    -1,    -1,    -1,
    1992,    -1,    -1,    -1,   323,    -1,    -1,    -1,    -1,  1858,
      -1,    -1,  2072,    -1,    -1,  1634,    -1,  1943,    -1,    -1,
    1639,    -1,    -1,    -1,   343,    -1,  1645,   591,  1647,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,   602,    -1,
     604,    -1,   606,    -1,   608,    -1,    -1,    -1,    -1,    -1,
      -1,  2043,    -1,    -1,    -1,  2047,    -1,   533,    -1,    -1,
     624,   625,    -1,   627,    -1,   541,    -1,    -1,  2060,  2136,
     486,   635,    -1,    -1,    -1,   639,    -1,    -1,    -1,    -1,
      -1,  2073,   558,    -1,   648,    -1,    -1,   343,    -1,    72,
     346,    -1,    -1,   569,   658,    -1,    -1,   661,    -1,    -1,
      -1,    -1,    -1,    -1,   360,    -1,    -1,    -1,   364,   673,
      -1,  2178,    -1,    -1,   433,    -1,    -1,    -1,  2110,    -1,
      -1,    -1,   686,    -1,    -1,   689,   690,    -1,   692,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   701,  1658,    -1,
     704,   705,   706,    -1,   463,    -1,  2072,    -1,   564,   132,
      -1,    -1,    -1,    -1,  2146,    -1,    -1,  2149,    -1,  2151,
      -1,    -1,    -1,    -1,    -1,    -1,  2158,    -1,    -1,   152,
      -1,    -1,    -1,   156,    -1,    -1,    -1,    -1,     3,    -1,
     163,   164,   487,    -1,   489,    -1,    -1,    -1,    -1,    -1,
      18,  2040,    -1,    -1,    -1,  2187,   760,  2189,    -1,    -1,
      -1,  2193,    -1,  2195,    -1,  1824,    -1,   463,  1827,    -1,
     686,    -1,   776,   777,    -1,    -1,   692,    -1,    -1,  2068,
     784,    -1,    18,    -1,    -1,   701,  2218,   546,   547,    57,
      58,    59,    60,    61,    62,    63,    64,   801,  2230,    -1,
     804,    -1,   806,    -1,   720,    -1,    -1,  2239,    -1,    -1,
      -1,    -1,  2178,    -1,    -1,    -1,    -1,   182,  2107,   823,
     824,    -1,    -1,    -1,    -1,    61,    62,    63,    64,  1789,
    1790,    -1,  1891,    -1,    -1,    -1,    -1,    -1,   842,   104,
     844,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,     1,   552,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   870,   132,   104,    -1,
      -1,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   648,
      -1,    -1,    -1,  1952,   159,    -1,    -1,    -1,   163,   164,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   911,    -1,    -1,
     606,    -1,    -1,    59,   673,    -1,   920,    -1,    -1,    -1,
     156,    -1,   926,   927,    -1,    -1,    -1,    -1,   624,   625,
      -1,    -1,    -1,    -1,    -1,   939,   104,   941,    84,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   952,    -1,
      -1,    -1,   648,    -1,   100,    -1,    -1,    -1,   104,    -1,
      -1,   877,    -1,    -1,    -1,   661,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   889,    -1,    -1,    -1,   893,    -1,    -1,
      -1,   897,    -1,    -1,   152,   153,  1238,  1239,   992,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,   147,    -1,    -1,   150,    -1,    -1,    -1,   154,    -1,
      -1,    -1,  2071,    -1,    -1,    -1,    -1,    -1,   383,   165,
     166,   167,   387,   388,    -1,   784,    -1,    -1,    -1,    -1,
      -1,    -1,   397,   398,    -1,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   190,    -1,    -1,   412,   413,    -1,
      -1,    -1,    -1,    -1,    -1,  1059,   202,   203,    -1,    -1,
     206,  2021,    -1,    -1,   760,    -1,    -1,    -1,   433,    -1,
      -1,    -1,  1076,    -1,    -1,    -1,  2135,  2136,    -1,  1083,
      -1,    -1,    -1,    -1,   241,    -1,   831,    -1,    -1,   834,
      13,    14,    15,    16,    17,    -1,    -1,    -1,   463,    -1,
      -1,    -1,    -1,    -1,    -1,   801,    -1,    -1,    -1,    -1,
      88,   257,    -1,   259,    -1,    -1,    -1,    -1,    -1,  2178,
      -1,  1125,    -1,    -1,    -1,    -1,   104,   273,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,   878,    -1,    -1,    -1,    -1,    -1,    -1,   294,    72,
      -1,    -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,
      -1,   307,    -1,    -1,    -1,  1169,    -1,   313,   927,    -1,
      -1,    -1,    -1,    -1,  1178,    -1,    -1,   323,    -1,    -1,
      -1,   104,  1186,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,   343,    -1,   345,
     346,    -1,    -1,    -1,    -1,  1209,    -1,   944,    -1,   132,
      -1,    -1,  1216,    -1,   360,    -1,    -1,   104,   364,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   152,
     153,    -1,    -1,   156,  1238,  1239,    -1,  1489,    -1,    -1,
     163,   164,    -1,   939,    -1,    -1,   942,    -1,    -1,  1253,
      -1,    -1,    -1,   399,   104,  1259,  1172,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,  1270,    -1,  1013,  1185,
      -1,    -1,    -1,   160,    -1,    -1,  1021,    -1,    -1,   436,
     167,    -1,    -1,    -1,    -1,    -1,  1202,   433,    -1,    -1,
      72,    -1,    -1,  1209,    -1,   452,   992,  1301,   455,    -1,
      -1,  1305,    -1,    -1,    -1,  1050,    -1,    -1,  1053,    -1,
      -1,    78,    -1,   459,    -1,    -1,    -1,   463,  1055,    -1,
      -1,    -1,   104,   469,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,  1072,  1073,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,   512,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1368,  1369,    -1,    -1,  1113,    -1,
     152,   153,   737,   738,   739,   740,   741,   742,   743,   744,
     745,   746,   747,   748,   749,   750,   751,   752,   753,   754,
     755,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
     546,   547,    -1,    -1,    -1,   551,   552,    -1,    -1,    -1,
     177,    -1,    -1,    -1,  1418,  1419,  1420,  1421,    -1,  1423,
    1424,    -1,    -1,   104,    -1,  1429,  1430,   108,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,    -1,   584,    -1,
      -1,    -1,    -1,   589,  1448,  1449,   592,   593,    -1,   595,
      72,    -1,   817,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     606,   104,   608,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,   156,   622,    -1,   624,   625,
      -1,   627,   104,    -1,    -1,  1489,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
    1259,    -1,   648,    -1,    -1,   651,    -1,    -1,  1512,   655,
     132,  1270,   658,    -1,    -1,   661,    -1,   663,    -1,    -1,
      -1,    -1,    -1,  1439,  1440,    -1,    -1,   673,    -1,    -1,
     152,   153,    -1,    -1,   177,  1539,    -1,  1541,    -1,    -1,
     686,   163,   164,   689,   690,    -1,   692,    -1,    -1,    65,
      66,    67,    68,    -1,    -1,   701,    -1,    -1,   704,   705,
     706,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,  1488,  1318,  1270,   149,    -1,    -1,    -1,    -1,
      -1,  1326,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,   176,    -1,    -1,  1301,    -1,    -1,    -1,    -1,
      -1,    -1,  1349,  1350,   760,    -1,    -1,    -1,    -1,   776,
     777,    -1,    -1,   988,    -1,    -1,  1363,  1364,   993,   786,
    1634,  1635,   789,    -1,    -1,  1639,    -1,  1641,   784,  1004,
     156,  1645,    -1,  1647,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,   801,    -1,    -1,   174,    -1,
      -1,    -1,    -1,    -1,  1401,  1402,  1403,    -1,    -1,    -1,
    1674,  1675,    -1,    -1,    -1,    -1,    -1,   823,   824,    -1,
     104,  1046,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   850,    -1,    -1,   842,    -1,    -1,    -1,
     857,    -1,    -1,    -1,   861,    -1,  1710,    -1,   865,    72,
     104,  1456,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,  1728,    -1,    -1,  1731,   874,    -1,
      -1,    -1,   878,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   177,    -1,    -1,    -1,   903,    -1,    -1,
    1764,   155,    -1,    -1,    -1,   911,   160,    -1,    -1,   132,
     916,    -1,    -1,   167,   920,    -1,    -1,    -1,    -1,    -1,
     926,   927,    -1,    -1,  1700,  1789,  1790,    -1,  1533,   152,
     153,    -1,    -1,   939,    -1,   941,   942,    -1,   944,    -1,
     163,   164,    -1,    -1,    -1,  1809,  1810,  1552,    -1,    -1,
      -1,    -1,  1557,    -1,    -1,    -1,  1512,  1562,  1563,  1564,
    1824,  1186,    -1,  1827,    -1,    -1,    -1,    -1,    -1,   104,
      72,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,  1845,    -1,    -1,    -1,    -1,   992,    -1,    -1,     1,
      -1,    -1,     4,    -1,  1858,    -1,    -1,     1,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,  1612,  1613,  1614,    -1,    -1,
      -1,    -1,  1641,    -1,    -1,   160,    -1,  1891,    -1,    -1,
     132,    -1,   167,     4,     5,     6,     7,     8,     9,    10,
      11,    12,  1059,    -1,    -1,  1270,    -1,    59,    -1,  1055,
     152,   153,    -1,    -1,   156,    59,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,    -1,  1290,  1072,  1073,    -1,    -1,
      -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,    -1,  1943,
      -1,    -1,    -1,  1308,  1309,  1310,    -1,    -1,  1952,    -1,
    1315,  1316,   104,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     104,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,  1130,  1339,    -1,  1133,    -1,    -1,  1125,
    1137,    -1,    -1,  1987,    -1,    -1,    -1,   132,    -1,   141,
      -1,    -1,    -1,    -1,    -1,   147,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   147,    -1,    -1,  1743,   152,   153,    -1,
      -1,  1376,  1377,    -1,   166,    -1,    -1,  2021,   163,   164,
      -1,    -1,   166,  1169,    -1,    -1,  2030,  1772,    -1,  1774,
      -1,    -1,  1178,    -1,    -1,    -1,  2040,    -1,    -1,   191,
    1186,    -1,  1779,    -1,    -1,    -1,    -1,  1784,  1785,    -1,
     202,   203,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   203,
      -1,    -1,    -1,  1209,  2068,  2069,    -1,  2071,  2072,    -1,
    1216,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   231,
      72,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,  1238,  1239,   120,    -1,   122,    -1,    -1,   251,
      -1,    -1,    -1,  2107,   256,   257,    -1,   259,    -1,    -1,
      -1,    -1,   104,  1259,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,  1270,    -1,    -1,   153,  1274,    -1,
     282,  2135,  2136,    -1,   286,    -1,    -1,    -1,   282,   291,
     132,  1287,    13,    14,    15,    16,    17,    -1,    -1,    -1,
     302,    -1,    -1,    -1,  1300,  1301,    -1,    -1,   302,  1305,
     152,   153,    -1,   315,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,   104,  2178,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,  1345,    -1,
      -1,   343,    -1,    -1,   346,    -1,    -1,  1354,    -1,   343,
      -1,    72,   346,  1349,  1350,    -1,    -1,    -1,   360,    -1,
      -1,    -1,   364,    -1,    -1,    -1,   360,  1363,  1364,    -1,
     364,    -1,  1368,  1369,    -1,    -1,    -1,    -1,    -1,   160,
      -1,    -1,    84,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1401,  1402,  1403,    -1,    -1,
      -1,   132,    -1,    -1,  1410,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1418,  1419,  1420,  1421,  1422,  1423,  1424,    -1,
      -1,   152,   153,    -1,  1430,   156,    -1,    -1,    72,    -1,
    2206,  1987,   163,   164,    -1,    -1,    -1,    -1,   150,    -1,
      -1,    -1,  1448,  1449,  2220,    -1,    -1,   459,    -1,    -1,
      -1,   463,    -1,    -1,   166,    -1,    -1,    -1,    -1,   463,
     104,    72,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,   190,    -1,
      -1,    -1,    -1,  1489,    -1,    -1,    -1,    -1,   132,    -1,
    1715,   203,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,  1512,    -1,   152,   153,
      -1,    -1,    -1,  2069,    -1,    -1,    -1,    -1,   104,   163,
     164,   132,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,    -1,  1539,   120,    -1,   122,    -1,    -1,    -1,
     552,   152,   153,    -1,    -1,    -1,    -1,   259,   552,    -1,
      -1,    -1,   163,   164,    -1,    -1,    -1,    -1,  2155,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,    -1,    -1,
     156,    -1,   584,    -1,    -1,    -1,    -1,   589,    -1,    -1,
     584,   593,    -1,   595,    -1,    -1,    -1,    -1,    -1,   593,
      -1,   595,    -1,    -1,   606,    -1,   608,    -1,    -1,    -1,
      -1,    -1,   606,    -1,   608,    -1,  1612,  1613,  1614,    -1,
      -1,   323,   624,   625,  1188,    -1,    -1,    -1,  1635,    -1,
     624,   625,    -1,    -1,    -1,    -1,    -1,   639,    -1,    -1,
      -1,   343,    -1,    -1,    -1,  1641,   648,    -1,    -1,    -1,
      -1,   653,    -1,    -1,   648,    -1,    -1,    -1,    -1,   661,
      -1,    -1,  1658,    -1,    -1,    -1,    -1,   661,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,  1674,  1675,
      -1,    -1,    -1,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,  1710,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    58,    59,    60,    61,    62,    63,    64,
      -1,   433,  1728,    -1,    -1,  1731,    -1,    -1,    -1,  1954,
      -1,    -1,    -1,    -1,    -1,    -1,   104,  1743,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   760,    -1,
      -1,   463,    -1,    -1,    -1,    -1,   760,    -1,  1764,    -1,
      -1,    -1,    -1,    72,    -1,   777,    -1,    -1,    -1,  1994,
      -1,    -1,    -1,  1779,    -1,    -1,    -1,    -1,  1784,  1785,
      -1,    -1,    -1,  1789,  1790,   153,    -1,    -1,   156,   801,
      -1,    -1,  1809,  1810,   806,   104,    -1,   801,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,   154,
      -1,   823,   824,    -1,    -1,    -1,    -1,    -1,    -1,   823,
     824,    -1,    -1,   132,    -1,    -1,    -1,    -1,     1,    -1,
     842,    -1,   177,    -1,   546,   547,    -1,    -1,   842,  1845,
     552,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1858,    -1,   163,   164,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    56,    57,   878,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   878,    -1,  2101,    -1,  2103,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   606,    -1,    -1,    -1,    -1,   911,
      92,    -1,    -1,    -1,    -1,  1479,  1480,   911,   920,    -1,
      -1,    -1,    -1,   625,   926,    -1,   920,  2142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1943,   939,    -1,    -1,
     942,   104,   944,    -1,    -1,   939,   648,   949,   942,    -1,
     944,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,  2176,    -1,   145,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   673,    -1,    -1,    -1,    -1,    -1,    -1,   160,    -1,
      -1,    -1,    -1,    -1,   147,    -1,    -1,    -1,    -1,    -1,
     992,  1987,    -1,    -1,    -1,    -1,    -1,    -1,   992,    -1,
      -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,    -1,  2224,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2026,
      -1,    -1,    -1,  2030,    -1,  2021,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   221,
     203,   104,    -1,    -1,  2040,   108,   109,   110,   111,   112,
     113,   114,   115,  1055,   132,    -1,    -1,    -1,   760,    -1,
      -1,  1055,    -1,    -1,  2071,    -1,    -1,    -1,    -1,   132,
    1072,  1073,  2068,  2069,   152,   153,  2072,    -1,  1072,  1073,
      -1,   159,   784,    -1,    -1,   163,   164,   269,   270,   152,
     153,    -1,    -1,   156,    -1,    -1,    -1,    -1,   280,   801,
     163,   164,    -1,  1667,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2107,    -1,   295,    -1,    -1,  1680,    -1,    -1,    -1,
      -1,   823,   824,  1125,    -1,    -1,    -1,    -1,  2135,  2136,
      -1,  1125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,
     842,    -1,   324,    -1,    -1,    -1,    -1,    -1,    -1,   331,
     332,    -1,    -1,    -1,   336,    -1,    -1,    -1,    -1,  2155,
      -1,  1725,    13,    14,    15,    16,    17,  1169,    -1,    -1,
      -1,  2178,    -1,    -1,    -1,    -1,  1178,    -1,    -1,    -1,
     343,    -1,    -1,   346,  1186,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1186,   375,    -1,    -1,   378,   360,    -1,   104,
      -1,   364,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,    -1,  1216,   120,    -1,   122,   920,    -1,
      -1,    72,  1216,    -1,    -1,   927,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,  1239,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1239,    -1,    -1,   153,    -1,
      -1,   156,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,  1270,    -1,
      -1,    -1,    -1,    -1,    -1,  1839,  1270,  1279,    -1,  1843,
      -1,   132,    -1,    -1,   466,    -1,    -1,    -1,    -1,    -1,
     992,    72,    -1,  1857,    -1,    -1,   478,   479,  1300,  1301,
     463,   152,   153,  1867,    -1,    -1,  1300,  1301,    -1,    -1,
      -1,    -1,   163,   164,    -1,    -1,  1880,    -1,  1882,  1883,
    1884,  1885,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,  1349,  1350,    -1,
      -1,   132,    -1,    -1,    -1,  1349,  1350,    -1,    -1,    -1,
      -1,  1363,  1364,    -1,    -1,    -1,  1368,  1369,    -1,  1363,
    1364,   152,   153,    -1,  1368,  1369,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,    -1,    -1,    -1,   156,    -1,   552,
      -1,    -1,    -1,    -1,  1958,    -1,    -1,    -1,  1962,  1401,
    1402,  1403,    -1,  1967,    -1,    -1,    -1,  1401,  1402,  1403,
      -1,    -1,    -1,    -1,    -1,    -1,  1418,  1419,  1420,  1421,
    1422,   584,    -1,  1125,    -1,    -1,    -1,    -1,  1992,    -1,
     593,    -1,   595,    -1,    -1,    -1,   618,    -1,    -1,    -1,
      -1,    -1,    -1,   606,    -1,   608,  1448,  1449,    -1,    -1,
      -1,    -1,    -1,    -1,  1448,  1449,    -1,    -1,    -1,    -1,
      -1,   624,   625,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,  2043,
      -1,    -1,    -1,  2047,    -1,   648,   668,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,  2060,   104,   661,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
    1512,   152,   153,    -1,    -1,   156,    -1,    -1,  1512,    -1,
     104,    -1,   163,   164,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,    -1,   176,   120,  1539,   122,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2110,    -1,   155,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,  1259,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,  1270,   153,
      -1,    -1,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2146,    -1,   132,  2149,    -1,  2151,    -1,    -1,
      -1,    -1,    -1,    -1,  2158,    -1,    -1,   760,    -1,  1301,
      -1,    -1,    -1,    -1,   152,   153,    -1,    -1,   156,    -1,
    1612,  1613,  1614,    -1,    -1,   163,   164,    -1,  1612,  1613,
    1614,    -1,    -1,  2187,     1,  2189,    -1,     4,    -1,  2193,
      -1,  2195,    -1,    -1,    -1,    -1,   818,    -1,   801,    -1,
      -1,    -1,    -1,   825,    -1,  1647,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2218,    -1,  1658,    -1,    -1,    -1,
     823,   824,    -1,    -1,  1658,    -1,  1368,  1369,    -1,    -1,
      -1,    -1,    -1,  1675,    -1,    -1,    -1,    -1,    -1,   842,
      -1,  1675,    59,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    84,  1710,    -1,
      -1,   132,    -1,    -1,    -1,   878,  1710,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1728,   104,   910,  1731,
      -1,   152,   153,    -1,  1728,   112,    -1,    -1,    -1,    -1,
      -1,  1743,   163,   164,    -1,    -1,   155,    -1,   911,  1743,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   920,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
     147,    -1,    -1,   150,    -1,    -1,   939,  1779,    -1,   942,
      -1,   944,  1784,  1785,    -1,  1779,    -1,  1789,  1790,    -1,
    1784,  1785,    -1,    -1,     1,  1789,  1790,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1810,    -1,
    1512,    -1,   104,   190,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   202,    -1,    -1,    -1,   992,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,  1858,    -1,    -1,    -1,
     152,    -1,    -1,   104,  1858,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    84,    -1,    -1,
     257,   152,   259,    -1,   155,   156,    -1,   264,    -1,    -1,
      -1,    -1,  1055,    -1,    -1,    -1,   104,   104,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,  1072,
    1073,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,   141,    -1,    -1,    94,    95,  1641,
     147,    -1,    -1,   150,    -1,    -1,   323,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   190,    -1,  1987,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1987,    -1,   202,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1191,
    1192,  1193,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2021,
      -1,    -1,   399,  1186,    -1,    -1,  1728,  2021,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2040,    -1,
    1222,    -1,    -1,    -1,    -1,    -1,  2040,    -1,    -1,    -1,
     257,    -1,   259,  1216,    -1,    -1,   433,   264,    -1,    -1,
    1242,    -1,    -1,    -1,    -1,    -1,  2068,  2069,    -1,    -1,
      -1,    -1,    -1,    -1,  2068,  2069,  1239,    -1,    -1,    -1,
      -1,    -1,   459,    -1,    -1,    -1,    -1,  1789,  1790,    -1,
      -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2107,  1288,  1270,    -1,    -1,
      -1,    -1,    -1,  2107,    -1,    -1,   323,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2136,    -1,    -1,  1300,  1301,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2155,   311,    -1,    -1,    -1,    -1,    -1,
      -1,  2155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   546,
     547,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1349,  1350,    -1,    -1,
      -1,    -1,   399,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1363,  1364,    -1,    -1,    -1,  1368,  1369,    -1,    -1,    -1,
      -1,    -1,   589,    -1,    -1,    -1,   593,    -1,   595,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   433,    -1,    -1,    -1,
      -1,   608,    -1,    -1,    -1,    -1,    -1,    -1,  1401,  1402,
    1403,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1433,   459,    -1,  1436,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1448,  1449,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   673,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   462,    -1,   464,    -1,   686,
      -1,    -1,   689,   690,    -1,   692,   473,   474,    -1,  2021,
      -1,    -1,    -1,    -1,   701,    -1,    -1,   704,   705,   706,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   546,
     547,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1512,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
      -1,     4,   589,    -1,    -1,    -1,   593,    -1,   595,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   608,    -1,    -1,    -1,    -1,    -1,   784,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   590,    -1,    -1,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1612,
    1613,  1614,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    -1,    -1,    -1,    -1,   673,    -1,    -1,    -1,
      -1,    -1,    -1,  1655,    -1,    -1,    -1,    -1,    -1,   686,
      -1,   104,   689,   690,     1,   692,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   701,  1658,    -1,   704,   705,   706,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1675,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,   147,    -1,    -1,   150,    -1,    -1,
      -1,    -1,    49,    -1,   911,    52,    -1,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1710,    -1,   926,
     927,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1728,    -1,   190,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   784,    -1,   202,
    1743,    -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,  1779,    -1,    -1,    -1,
      -1,  1784,  1785,    -1,    -1,    -1,  1789,  1790,    -1,    -1,
      -1,    -1,    -1,    -1,   257,   152,   259,    -1,   155,   156,
      -1,   264,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     817,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1858,    -1,    -1,    -1,    -1,
     323,    -1,    -1,    -1,   911,    -1,    -1,    -1,  1890,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   926,
     927,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   904,   905,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     917,   918,   919,    -1,    -1,   922,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   399,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1178,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1186,
     433,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1987,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1209,    -1,    -1,    -1,   459,    -1,    -1,  1216,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1006,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2021,    -1,
      -1,    -1,  1239,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2040,    -1,    -1,
      -1,    -1,  1259,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1052,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2068,  2069,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   546,   547,    -1,    -1,    -1,  1305,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1099,    -1,  2107,    -1,    -1,    -1,    -1,    -1,
      -1,  1108,  1109,  1110,  1111,    -1,    -1,    -1,    -1,  1116,
    1117,    -1,  1169,    -1,    -1,    -1,   589,    -1,    -1,  1126,
     593,  1178,   595,    -1,    -1,    -1,    -1,    -1,    -1,  1186,
      -1,    -1,    -1,    -1,    -1,   608,    -1,    -1,    -1,    -1,
      -1,    -1,  2155,    -1,    -1,    -1,    -1,    -1,  1155,    -1,
      -1,  1158,  1209,  1160,    -1,    -1,    -1,    -1,    -1,  1216,
      -1,    -1,    -1,    -1,     0,    -1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1239,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1418,  1419,  1420,  1421,  1422,  1423,  1424,    -1,    -1,
     673,    -1,  1259,    -1,    -1,    -1,    -1,    -1,    -1,  1216,
      -1,    -1,    -1,   686,    -1,    -1,   689,   690,    -1,   692,
      -1,  1448,  1449,    -1,    -1,    -1,    -1,    -1,   701,    -1,
      -1,   704,   705,   706,    -1,    -1,    -1,    -1,    -1,    -1,
    1247,    -1,    78,    -1,    -1,    -1,    -1,  1254,  1305,  1256,
    1257,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1269,    -1,  1271,    -1,  1273,    49,  1275,    -1,
      52,    -1,    54,  1280,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   784,  1539,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,  1344,   120,   121,
     122,    -1,   124,   125,  1351,  1352,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1418,  1419,  1420,  1421,  1422,  1423,  1424,  1375,    -1,
     152,   153,    -1,   155,   156,  1382,    -1,    -1,   160,  1386,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,  1448,  1449,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   240,    -1,    -1,  1414,    -1,    -1,
      -1,    -1,    -1,    -1,  1641,    -1,    -1,    -1,    -1,    -1,
      -1,  1428,   258,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   268,    -1,    -1,    -1,    -1,    -1,   911,    -1,
      -1,    -1,    -1,   279,    -1,    -1,    -1,    -1,  1675,    -1,
      -1,    -1,    -1,   926,   927,    -1,    -1,    -1,    -1,    -1,
      -1,   297,   298,  1470,    -1,    -1,    -1,    -1,   304,   305,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1539,  1710,    -1,    -1,   322,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1731,    -1,   342,    -1,    -1,  1516,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1524,    -1,  1526,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1764,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   400,    -1,    -1,    -1,    -1,    -1,
    1577,  1578,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1641,  1592,  1593,    -1,  1595,    -1,
      -1,    -1,    -1,    -1,    -1,   431,    -1,  1604,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1615,  1616,
    1617,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1675,    -1,
      -1,   457,    -1,    -1,    -1,   461,    -1,    -1,    -1,    -1,
      -1,  1858,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   480,    -1,    -1,    -1,   484,   485,
      -1,    -1,   488,  1710,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   503,   504,   505,
     506,    -1,    -1,    -1,  1731,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,    -1,
     526,    -1,    -1,    -1,    -1,    -1,  1169,    -1,    -1,   535,
      -1,    -1,    -1,    -1,    -1,  1178,    -1,  1764,    -1,    -1,
      -1,    -1,    -1,  1186,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   567,    -1,    -1,    -1,    -1,  1209,  1744,  1745,    -1,
      -1,    -1,    -1,  1216,    -1,    -1,    -1,    -1,    -1,    -1,
      75,  1758,    -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,
      -1,    -1,   598,    -1,    -1,    -1,  1239,    -1,    -1,   605,
      -1,   134,    -1,   136,    -1,   611,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1259,    -1,  1795,  1796,
    1797,    -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,   635,
     636,  1858,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,  2040,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1305,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2068,    -1,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,
      -1,   707,    -1,    -1,   237,   238,    -1,    -1,   241,    -1,
    2107,   244,   245,    -1,   247,    -1,   249,    -1,    -1,  1896,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   230,    -1,    -1,    -1,   234,
      -1,  1918,   237,   238,  1921,  1922,   241,    -1,    -1,   244,
     245,  1928,   247,    -1,   249,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     776,    -1,    -1,    -1,    -1,  1418,  1419,  1420,  1421,  1422,
    1423,  1424,    -1,    -1,    -1,   791,    -1,    -1,    -1,   795,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   804,    -1,
      -1,    -1,    -1,    -1,    -1,  1448,  1449,   340,   341,    -1,
      -1,    -1,    -1,  2040,    -1,    -1,    -1,    -1,    -1,   314,
     826,    -1,   317,   356,    -1,    -1,    -1,    -1,    -1,   835,
      -1,    -1,    -1,    -1,    -1,   841,    -1,    -1,    -1,    -1,
      -1,  2068,    -1,    -1,    -1,   340,   341,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   356,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2107,   887,    -1,    -1,    -1,   891,    -1,    -1,    -1,   895,
      -1,    -1,    -1,  2070,    -1,    -1,  1539,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,   924,   452,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,  2133,   452,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,   982,    -1,    -1,    -1,
      -1,    -1,  2159,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     523,    -1,    -1,    -1,    -1,    -1,    -1,  2174,  1641,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,   507,    -1,  2191,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   523,    -1,
      -1,    -1,  1675,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,  1065,
      -1,    -1,    -1,  1069,    -1,    -1,    -1,  1710,    -1,    -1,
    1076,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1086,    -1,    -1,     5,    -1,    -1,    -1,  1093,  1731,    -1,
      -1,    13,    14,    15,    16,    17,  1102,    -1,  1104,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   602,    -1,    -1,
     643,   644,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1764,    -1,   656,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,  1139,    56,    -1,    -1,  1143,    -1,    -1,
      -1,  1147,    -1,    -1,    -1,    -1,    -1,    -1,   643,   644,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1165,
      -1,   656,    -1,    -1,  1170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,  1858,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,   780,   781,    -1,
      -1,    -1,    -1,   786,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1268,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   807,    -1,    -1,   810,   811,    -1,
     813,    -1,   815,   816,    -1,   780,   781,    -1,    -1,  1295,
      -1,   786,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   807,    -1,    -1,   810,   811,    -1,   813,    -1,
     815,   816,    -1,    -1,   857,    -1,    -1,    -1,   861,    -1,
      -1,    -1,   865,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   857,    -1,    -1,    -1,   861,    -1,    -1,    -1,
     865,    -1,    -1,    -1,    -1,    -1,   166,   399,    -1,    -1,
      -1,    -1,    -1,  1389,    -1,    -1,    -1,  1393,    -1,    -1,
      -1,  1397,    -1,    -1,    -1,    -1,    -1,  2040,    -1,    -1,
      -1,    -1,    -1,   936,   937,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   202,   203,    -1,    -1,    49,   950,    -1,    52,
      -1,    54,    -1,    56,    -1,  2068,    -1,    -1,    -1,  1435,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,   936,   937,    -1,   234,    -1,    -1,    -1,    -1,    -1,
      -1,   241,    -1,    -1,    -1,   950,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2107,    -1,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
    1496,   124,   125,  1499,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1518,   146,   147,   148,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,   546,   547,    -1,   317,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1082,
      -1,    -1,    -1,   343,   344,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1573,    -1,    -1,
      -1,    -1,    -1,    -1,   364,    -1,  1582,    -1,    -1,    -1,
    1586,    -1,    -1,    -1,    -1,    -1,    -1,  1082,    -1,    -1,
    1123,    -1,    -1,    -1,  1600,  1601,    -1,  1130,    -1,    -1,
    1133,    -1,    -1,    -1,  1137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1628,    -1,    -1,    -1,    -1,    -1,  1123,    -1,
      -1,    -1,    -1,    -1,    -1,  1130,    -1,    -1,  1133,    -1,
      -1,    -1,  1137,    -1,    -1,    -1,   436,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   452,   453,   686,   455,   456,    -1,    -1,    -1,
     692,    -1,    -1,   463,    -1,    -1,    -1,   467,    -1,   701,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   720,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   508,    -1,
      -1,    -1,   512,    -1,    -1,    -1,  1732,  1733,    -1,    -1,
      -1,    -1,    -1,    -1,   756,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1276,    -1,    -1,    -1,    -1,    -1,    84,
      -1,    -1,  1285,  1286,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   552,    -1,    -1,   100,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1276,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1285,  1286,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1345,   603,    -1,   150,   606,    -1,    -1,   154,
      -1,  1354,    -1,    -1,  1357,    -1,  1359,  1360,    -1,    -1,
      -1,   166,    -1,    -1,   624,   625,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   635,    -1,    -1,    -1,   639,
    1345,    -1,    -1,    -1,    -1,   190,   646,    -1,   648,  1354,
      -1,    -1,  1357,    -1,  1359,  1360,  1872,    -1,   203,    -1,
      -1,   206,  1405,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1907,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1405,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1926,    -1,    -1,    -1,   259,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   273,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1956,    -1,    -1,    -1,    -1,    -1,    -1,  1490,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     760,    -1,    -1,    -1,    -1,    -1,    -1,  1983,   313,    -1,
    1986,    -1,    -1,    -1,    -1,    -1,   776,   777,   323,    -1,
      -1,    -1,    -1,    -1,    -1,  1490,   786,   787,    -1,   789,
     790,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   343,    -1,
     345,   801,    -1,    -1,   804,    -1,   806,   807,    -1,    -1,
      -1,    -1,    -1,   813,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   823,   824,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1581,    -1,
      -1,    -1,   842,    -1,    -1,    -1,   846,    -1,    -1,    -1,
     850,    -1,    -1,    -1,   399,    -1,    -1,   857,   858,    -1,
      -1,   861,   862,    -1,    -1,   865,   866,  1610,    -1,    -1,
      -1,    -1,    -1,   873,    -1,    -1,  1581,    -1,    -1,    -1,
    2096,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   433,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1610,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   463,    -1,
     920,   921,    -1,    -1,   469,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   190,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1645,    -1,    -1,    -1,    -1,   203,  1651,    -1,    -1,    -1,
      -1,    -1,   952,    -1,    -1,    -1,    -1,    -1,   216,    -1,
     218,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1209,    -1,    -1,
      -1,    -1,    -1,    -1,  1727,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   992,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   546,   547,    -1,    -1,    -1,    -1,   552,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1727,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,  1059,
      -1,   606,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1816,  1817,    -1,  1076,  1077,    -1,    -1,
     625,    -1,   627,  1083,    -1,    -1,  1829,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   648,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1816,  1817,    -1,    -1,    -1,    -1,    -1,    -1,  1824,
      -1,    -1,    -1,    -1,  1829,  1125,    -1,    -1,   673,    -1,
    1130,  1131,    -1,  1133,  1134,    -1,    -1,  1137,  1138,    -1,
      -1,   686,    -1,    -1,   689,   690,    -1,   692,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   701,    -1,    -1,   704,
     705,   706,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,  1945,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,  1445,    -1,   760,  1448,  1449,    -1,    -1,
      -1,    -1,  1454,    -1,    -1,    72,  1458,    -1,  1460,    -1,
      -1,   489,    -1,    -1,    -1,    -1,    -1,   495,    -1,   784,
    1945,    -1,   500,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   801,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
    1270,    -1,    -1,    -1,    -1,    -1,  1276,  1277,   823,   824,
      -1,    -1,    -1,  2026,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   842,    -1,    -1,
      -1,  1301,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,    -1,
      -1,  2026,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   596,    -1,
      -1,    -1,    -1,    -1,    -1,  1345,  1346,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1354,  1355,    -1,  1357,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   625,  1368,  1369,
      -1,    -1,    -1,    -1,    -1,   920,    -1,    -1,    -1,    -1,
     638,    -1,   927,    -1,    -1,    -1,    -1,    -1,  1620,    -1,
      -1,    13,    14,    15,    16,    17,   941,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
    2135,    53,    -1,    55,    -1,    -1,   694,    -1,  1670,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   992,    -1,    -1,
      72,    -1,    -1,  1685,  1686,    -1,    -1,    -1,    -1,    -1,
     718,   719,    -1,    -1,   722,    -1,   724,    -1,    -1,    -1,
      -1,    -1,   730,    -1,   732,   733,    -1,    -1,  1710,    -1,
      -1,    -1,    -1,    -1,  1716,    -1,    -1,     5,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,   760,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1512,    -1,    -1,   773,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   784,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,
     798,  1541,    -1,   801,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     828,    -1,    -1,   831,    -1,    -1,    -1,    -1,    -1,    -1,
    1125,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,    -1,  1841,
      -1,    -1,    -1,    -1,   132,    -1,   874,    -1,  1850,    -1,
    1852,    -1,    -1,  1855,  1856,    -1,  1858,    -1,    -1,    -1,
      -1,  1863,    -1,  1178,   152,  1635,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,  1651,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1209,    -1,    -1,    -1,    -1,   927,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   941,   942,    -1,    -1,    -1,    -1,    -1,
      -1,   949,    -1,  1238,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1259,    -1,    -1,    -1,    -1,    -1,
     978,    -1,    -1,    -1,    -1,  1270,    -1,    -1,  1728,    -1,
      -1,    -1,    -1,    -1,   992,    -1,    -1,    -1,    -1,    -1,
      -1,  1973,  1000,    -1,    -1,    -1,  1978,  1979,    -1,  1007,
      -1,    -1,    -1,    -1,    -1,    -1,  1301,    -1,    -1,    -1,
    1305,    -1,    -1,    -1,    -1,    -1,  1998,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1789,
    1790,    -1,    -1,    -1,    -1,  1053,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1809,
    1810,    -1,    -1,    -1,    -1,    -1,  2048,    -1,  2050,    -1,
      -1,  2053,  2054,  1368,  1369,  1825,  2058,  2059,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1120,    -1,  1122,    -1,  1124,    -1,    -1,    -1,
      -1,    73,    -1,  1418,  1419,  1420,    -1,    -1,  1423,  1424,
      -1,    -1,    -1,    -1,    -1,  1430,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2128,  2129,  2130,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,  2164,  2165,  2166,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1943,  1489,  1203,  1204,    -1,    -1,    -1,
     152,  1951,    -1,   155,   156,    -1,    -1,    -1,   160,     3,
     162,   163,   164,   165,   166,   167,   168,  1512,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,  1274,    -1,    -1,    -1,
      -1,  2021,  1280,    -1,    -1,    -1,  2026,  2027,    72,    -1,
    2030,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1301,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1318,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2071,    -1,    -1,    -1,  1333,    -1,    -1,  1336,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1641,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1674,
    1388,    -1,    -1,    -1,    -1,  2135,  2136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1409,  1410,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2178,    -1,
    1438,    -1,    -1,  1728,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1456,    -1,
      -1,  1459,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1764,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1789,  1790,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,  1512,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1522,  1523,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1533,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1552,    -1,  1554,    -1,    -1,    -1,
    1845,    71,    -1,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    -1,    99,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,   152,  1641,    -1,   155,   156,    -1,  1646,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    13,
      14,    15,    16,    17,    -1,    -1,    20,   177,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,  1987,    -1,    -1,    -1,    -1,  1705,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,  2021,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1762,    -1,    -1,  1765,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,  2069,    56,    -1,  2072,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,   155,    73,    74,  1802,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    -1,    99,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,     1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      49,    -1,    -1,    52,    -1,    54,   177,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    73,    74,    -1,    76,    -1,    -1,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    -1,
      99,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1995,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,   160,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    71,    72,    73,
      74,    -1,    76,    -1,    -1,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    -1,    99,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   177,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    71,    72,    73,    74,    -1,    76,    -1,    -1,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    -1,
      99,    -1,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,   160,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   146,   147,   148,    -1,    -1,    -1,   152,   153,
     154,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   177,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,   147,   148,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
     154,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    77,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
      -1,   163,   164,     3,     4,     5,     6,     7,     8,     9,
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
      -1,    -1,    -1,   163,   164,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    -1,    72,    -1,    -1,    -1,    -1,    77,
      78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    77,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,   155,
     156,    -1,     3,    -1,     5,    -1,    -1,   163,   164,    10,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,    -1,    -1,    -1,   155,   156,    -1,     3,    -1,     5,
      -1,    -1,   163,   164,    10,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,   155,
     156,    -1,     3,    -1,     5,    -1,    -1,   163,   164,    10,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,    -1,    -1,    -1,   155,   156,    -1,     3,    -1,     5,
      -1,    -1,   163,   164,    10,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,    72,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
     153,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,    -1,
     163,   164,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
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
      -1,   132,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
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
      -1,    -1,    -1,    -1,   154,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
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
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
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
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
      71,    -1,    73,    74,    -1,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      -1,    -1,    93,    94,    95,    96,    97,    -1,    99,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,    18,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    49,   155,   156,    52,    -1,    54,   160,
      56,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,   177,    73,    74,    -1,
      76,    -1,    -1,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    -1,    -1,    93,    94,    95,
      96,    97,    -1,    99,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   177,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
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
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,   159,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
     155,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,   160,    -1,    -1,   163,   164,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
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
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,   155,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,   155,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
     155,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   163,   164,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    72,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   106,
     107,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
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
      -1,   155,    -1,    -1,    13,    14,    15,    16,    17,   163,
     164,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    13,
      14,    15,    16,    17,   163,   164,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    72,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,   106,   107,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,   106,   107,    -1,    -1,    -1,    -1,   163,
     164,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    -1,    -1,
      -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,   155,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    49,    -1,    -1,    52,    -1,
      54,    72,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    49,    -1,    -1,    52,    -1,    54,   132,    56,
      -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   146,   147,   148,    -1,    73,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,   160,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,   159,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,   160,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,   154,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72
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
      89,   158,   413,   373,   180,   180,   180,   177,   106,   107,
     152,   199,   301,   302,   424,   425,   426,   427,   428,   429,
     433,   437,   438,   439,   440,   441,   442,   443,   444,   445,
     451,     3,    47,    48,    50,    55,   323,     3,   156,   199,
     295,   310,   314,   316,   326,   331,   409,   429,   433,   483,
      69,   293,   295,   309,   321,   325,   330,   410,   429,   433,
      65,   315,   315,   310,   316,   304,   315,   316,   323,   342,
     310,   315,   310,   155,   418,   158,   180,   152,   160,   228,
     418,   418,   179,   284,   285,   156,   306,   309,   481,   373,
     373,   406,   176,   309,   152,   199,   415,   424,   425,   429,
     438,   442,   156,   199,   483,   407,   408,    65,    66,    67,
      68,   156,   174,   373,   382,   384,   388,   390,   391,   331,
      57,   154,   156,   199,   305,   309,   313,   314,   320,   321,
     327,   328,   329,   330,   334,   341,   342,   359,   369,   371,
     462,   475,   476,   477,   478,   483,   484,   106,   107,   160,
     167,   183,   331,   451,   420,   152,   389,   390,   152,   152,
     118,   185,   186,    49,    52,    54,    56,    73,   101,   102,
     104,   105,   116,   117,   120,   121,   122,   124,   125,   152,
     156,   162,   165,   166,   167,   168,   181,   182,   185,   187,
     190,   198,   199,   200,   201,   204,   205,   206,   207,   208,
     209,   210,   211,   212,   213,   214,   215,   216,   222,   331,
     154,   156,   198,   199,   215,   217,   306,   331,   374,   375,
     392,   479,   484,   309,   430,   431,   432,   434,   435,   436,
     154,   154,   154,   154,   154,   154,   154,   156,   306,   462,
     481,   156,   163,   199,   217,   295,   296,   305,   307,   309,
     321,   328,   330,   364,   365,   368,   369,   370,   475,   483,
     152,   429,   433,   483,   152,   158,   104,   155,   156,   160,
     182,   184,   217,   377,   378,   379,   380,   381,    22,   377,
     152,   373,   228,   152,   158,   158,   158,   419,   424,   426,
     427,   428,   437,   439,   440,   441,   443,   444,   445,   309,
     425,   438,   442,   158,    99,   417,   156,   418,   459,   462,
     417,   418,   418,   413,   284,   152,   418,   459,   417,   418,
     418,   413,   418,   418,   309,   415,   152,   152,   308,   309,
     306,   309,   179,   306,   479,   484,   333,   160,   413,   284,
     373,   373,   376,   295,   314,   411,   429,   433,   160,   413,
     284,   394,   309,   321,   309,   309,   106,   332,   106,   107,
     183,   331,   336,   394,   179,   183,   372,   151,   179,     3,
     300,   303,   309,   313,   228,   179,   179,   417,   152,   417,
     180,   217,   419,   424,   309,   152,   179,   373,   404,   160,
     373,   160,   373,   134,   163,   164,   387,   154,   158,   373,
     391,   154,   418,   418,   157,   179,   307,   309,   321,   328,
     330,   474,   475,   483,   484,   152,   156,   164,   176,   199,
     462,   464,   465,   466,   467,   468,   469,   486,   199,   334,
     483,   309,   328,   315,   310,   418,   154,   307,   309,   476,
     307,   462,   476,    10,   162,   167,   358,   360,   361,   160,
     356,   358,   382,   176,   382,    13,    88,   104,   106,   107,
     182,   421,   422,   423,   154,   118,   152,   198,   152,   152,
     152,   201,   152,   198,   152,   104,   106,   107,   310,   315,
     316,   152,   198,   198,    19,    21,    85,   156,   165,   166,
     202,   203,   217,   224,   228,   344,   374,   483,   158,   179,
     152,   187,   156,   161,   156,   161,   121,   123,   124,   125,
     152,   155,   156,   160,   161,   201,   201,   169,   163,   170,
     171,   165,   166,   126,   127,   128,   129,   172,   173,   130,
     131,   164,   162,   174,   132,   133,   175,   154,   158,   155,
     179,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   176,   219,   220,   221,   152,   199,   455,   456,
     457,   458,   459,   154,   158,   154,   154,   154,   154,   154,
     154,   152,   418,   459,   462,   152,   459,   462,   179,   306,
     481,   179,   180,   180,   152,   164,   199,   424,   446,   447,
     448,   449,   450,   451,   452,   453,   454,   134,   483,   180,
     180,   373,   373,   179,   179,   179,   156,   184,   179,   378,
     159,   158,   485,   377,   155,   156,   159,   381,   153,   217,
     223,   152,   179,   179,   179,   179,   424,   426,   427,   428,
     437,   439,   440,   441,   443,   444,   445,   154,   154,   154,
     154,   154,   154,   154,   154,   154,   154,   425,   438,   442,
     418,   152,   176,   157,   179,   376,   228,   413,   179,   376,
     228,   415,   224,   375,   224,   375,   415,   404,   228,   413,
     417,   160,   160,   413,   284,   404,   228,   413,   338,   339,
     337,   160,   134,   309,   366,   367,   370,   371,   154,   158,
      70,   286,   287,   180,   429,   442,   309,   300,   163,   217,
     179,   424,   365,   406,   404,   157,   179,   152,   386,   384,
     385,    78,   319,   183,   160,   167,   183,   451,   307,   462,
     476,   309,   313,   483,   179,   465,   466,   467,   157,   179,
      18,   217,   309,   464,   486,   418,   418,   462,   307,   474,
     484,   309,   183,   418,   307,   476,   331,   158,   485,   373,
     360,   358,   160,   154,   375,   154,   154,   158,   152,   177,
     374,   187,   156,   374,   374,   374,   217,   374,   154,   374,
     374,   374,   179,   154,   165,   166,   203,    18,   311,   154,
     158,   154,   163,   164,   154,   223,   217,   160,   217,   183,
     217,   183,   116,   156,   183,   153,   191,   192,   193,   217,
     116,   156,   183,   344,   217,   191,   183,   201,   204,   204,
     204,   205,   205,   206,   206,   207,   207,   207,   207,   208,
     208,   209,   210,   211,   212,   213,   159,   224,   177,   185,
     156,   183,   217,   160,   217,   179,   456,   457,   458,   309,
     455,   418,   418,   217,   375,   152,   418,   459,   462,   152,
     459,   462,   179,   179,   157,   157,   152,   424,   447,   448,
     449,   452,    18,   309,   446,   450,   152,   418,   468,   486,
     418,   418,   486,   152,   418,   468,   418,   418,   180,   216,
     373,   157,   158,   157,   158,   486,   486,   134,   363,   364,
     365,   363,   373,   179,   215,   216,   217,   416,   485,   377,
     379,   151,   179,   154,   158,   179,   363,   183,   415,   183,
     154,   154,   154,   154,   154,   154,   154,   154,   154,   152,
     418,   459,   462,   152,   418,   459,   462,   152,   418,   459,
     462,   415,   185,    22,   462,   217,   316,   331,   460,   228,
     366,   154,   154,   154,   154,   402,   403,   228,   151,   179,
     404,   228,   413,   403,   228,   160,   160,   160,   345,   180,
     180,   183,   288,   373,    18,    71,    73,    76,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      93,    94,    95,    96,    97,    99,   106,   107,   119,   152,
     179,   224,   225,   226,   227,   228,   229,   230,   232,   233,
     243,   249,   250,   251,   252,   253,   254,   259,   260,   263,
     264,   265,   266,   267,   273,   274,   275,   289,   309,   313,
     373,   414,    70,   177,   180,   180,   180,   363,   180,   405,
     403,   293,   295,   306,   397,   398,   399,   400,   392,   176,
     383,   383,   360,   160,   418,   418,   307,   476,   156,   163,
     199,   217,   331,   217,   309,   366,   154,   154,   154,     5,
     309,   418,   464,   160,   167,   183,   451,    10,   361,   151,
     176,   362,   485,   160,   360,   160,   154,   422,   191,   154,
     158,   179,   158,   154,   154,   158,   154,   201,   154,   154,
     154,   201,    18,   311,   217,   154,   154,   153,   160,   201,
     157,   180,   191,   157,   157,   116,   120,   122,   184,   194,
     195,   196,   154,   158,   194,   157,   158,   151,   215,   159,
     154,   194,   180,   378,   366,   154,   154,   154,   455,   179,
     179,   366,   366,   452,   154,   154,   154,   154,   152,   424,
     451,   446,   450,   179,   179,   157,   180,   486,   179,   179,
     180,   180,   180,   180,   376,   194,   134,   168,   180,   180,
     151,   377,   217,   418,   153,   217,   363,   180,   176,   152,
     418,   459,   462,   152,   418,   459,   462,   152,   418,   459,
     462,   179,   179,   179,   417,   154,   146,   168,   180,   461,
     158,   180,   180,   405,   397,   403,   228,   405,   345,   345,
     345,     3,     5,    10,    73,   151,   290,   297,   298,   306,
     309,   346,   351,   479,   154,   158,   158,   177,   152,    61,
      62,   177,   228,   289,   414,   152,    18,   226,   152,   152,
     177,   373,   177,   373,   163,   373,   160,   225,   152,   152,
     152,   228,   217,   218,   218,    14,   276,   254,   265,    74,
     234,   177,   180,   230,    78,   177,   373,    91,    92,   258,
     262,   110,   133,   257,   109,   132,   261,   257,   372,   309,
     159,   288,   177,   157,   157,   180,   158,   405,   415,   180,
     177,   180,   177,   180,   154,   375,   389,   389,   485,   360,
     358,   358,   179,   180,   180,   180,   217,   180,   152,   418,
     468,   462,   308,     5,   163,   180,   217,   360,   160,   418,
     418,   331,   373,   160,   216,   151,   360,   485,   151,   179,
     154,   305,   183,    78,   188,   189,   374,   201,   201,   201,
     201,   201,   160,   378,   158,   151,   197,   156,   195,   197,
     197,   157,   158,   123,   155,   193,   157,   223,   215,   177,
     157,   485,   180,   152,   418,   459,   462,   366,   366,   180,
     180,   154,   152,   418,   459,   462,   152,   418,   468,   424,
     418,   418,   366,   366,   157,   365,   368,   368,   369,   154,
     158,   158,   154,   180,   216,   216,   157,   157,   180,   180,
     154,   217,   179,   179,   179,   366,   366,   366,   376,   418,
     158,   217,   217,   316,   331,   157,   154,   151,   180,   405,
     151,   151,   151,   151,   306,   306,   344,   352,   479,   306,
     351,   152,   340,   177,   177,   152,   159,   199,   347,   348,
     354,   424,   425,   438,   442,   158,   177,   373,   179,   373,
     191,   177,   228,   177,   228,   224,    80,   154,   224,   235,
     289,   291,   294,   300,   309,   313,   146,   147,   148,   153,
     154,   177,   224,   244,   245,   246,   289,   177,   177,   224,
     177,   378,   177,   224,   223,   224,   111,   112,   113,   114,
     115,   268,   270,   271,   177,    98,   177,    84,   152,   154,
     152,   180,   151,   177,   177,   152,   152,   226,   226,   254,
     152,   264,   254,   264,   228,   418,   177,   154,   179,   151,
     151,   179,   158,   158,   151,   485,   160,   160,   157,   157,
     157,   180,   154,   179,   217,   217,   180,   157,   180,   485,
     360,   357,   358,   362,   362,   378,   485,   151,   397,   463,
     464,   154,   159,   154,   158,   159,   378,   485,   223,   121,
     194,   195,   156,   195,   156,   195,   157,   151,   154,   179,
     180,   180,   154,   154,   179,   179,   180,   180,   180,   179,
     179,   157,   180,   154,   418,   366,   366,   366,   180,   180,
     180,   224,   461,   151,   151,   340,   340,   340,   347,   152,
     199,   349,   350,   459,   470,   471,   472,   473,   177,   158,
     177,   347,   177,   392,   419,   424,   217,   309,   151,   158,
     177,   353,   354,   353,   353,   373,   134,   370,   371,   154,
     154,   152,   226,   154,   224,   309,   146,   147,   148,   168,
     177,   247,   248,   226,   225,   177,   248,   154,   159,   224,
     153,   224,   225,   246,   177,   485,   154,   154,   154,   228,
     270,   271,   152,   217,   152,   185,   235,   201,   255,   224,
      75,   108,   256,   258,    75,   256,     1,   226,   418,   398,
     179,   179,   151,   360,   360,   157,   366,   180,   180,   157,
     157,   151,   485,   358,   160,   485,   151,   180,   154,   217,
     189,   217,   485,   151,   157,   157,   194,   194,   366,   154,
     154,   366,   366,   154,   154,   157,   158,   134,   365,   134,
     157,   180,   180,   180,   154,   154,   154,   157,   217,   177,
     471,   472,   473,   309,   470,   158,   177,   418,   418,   177,
     154,   424,   418,   226,    77,    78,   160,   238,   239,   240,
     154,   224,    75,   226,   224,   153,   224,    75,   177,   106,
     153,   224,   225,   246,   153,   224,   226,   245,   248,   248,
     177,   224,   151,   160,   240,   226,   152,   179,   177,   185,
     154,   159,   154,   154,   158,   159,   154,   226,   152,   226,
     226,   226,   226,   373,   415,   485,   485,   180,   157,   157,
     151,   160,   360,   151,   151,   151,   157,   157,   180,   180,
     180,   179,   180,   154,   154,   154,   154,   154,   154,   470,
     418,   348,     1,   216,   236,   237,   416,     1,   159,     1,
     179,   226,   238,    75,   177,   154,   226,    75,   177,   168,
     168,   226,   225,   248,   248,   177,   106,   224,   168,   168,
      75,   153,   224,   153,   224,   225,   177,     1,   179,   179,
     272,   307,   309,   479,   159,   177,   156,   185,   277,   278,
     279,   226,   201,   191,   224,   257,   257,   151,   151,   154,
     360,   485,   154,   154,   154,   368,   152,   418,   459,   462,
     350,   134,     1,   158,   159,   151,   282,   283,   289,   226,
      75,   177,   226,   224,   153,   153,   224,   153,   224,   153,
     224,   225,   153,   224,   153,   224,   226,   168,   168,   168,
     168,   151,   282,   272,   180,   152,   199,   415,   470,   183,
     159,   104,   152,   154,   159,   158,    75,   154,   154,    75,
     253,    75,   253,   485,   151,   179,   216,   236,   239,   241,
     242,   289,   226,   168,   168,   168,   168,   153,   153,   224,
     153,   224,   153,   224,   241,   180,   177,   269,   309,   277,
     157,   216,   177,   277,   279,   226,   226,    75,   226,    75,
     151,   366,   226,   231,   180,   239,   153,   153,   224,   153,
     224,   153,   224,   180,   269,   215,   154,   159,   185,   154,
     154,   159,   226,   226,   180,     1,   226,   151,   231,   151,
     154,   228,   185,   280,   152,   177,   280,   154,   228,   158,
     159,   216,   154,   185,   183,   281,   154,   177,   154,   158,
     177,   183
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
       0,     1,     4,     5,     0,     1,     3,     4,     1,     3,
       2,     2,     1,     7,     5,     1,     1,     1,     1,     1,
       2,     3,     6,     3,     3,     4,     1,     2,     2,     3,
       8,     8,     8,     5,     9,     2,     2,     5,     3,     5,
       4,     3,     4,     4,     7,     2,     1,     1,     1,     3,
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
       1,     5,     0,     1,     1,     5,     1,     1,     5,     5,
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
       4,     5,     4,     5,     3,     4,     8,     9,     3,     4,
       2,     1,     2,     6,     8,     9,     3,     4,     2,     3,
       4,     5,     4,     5,     4,     5,     3,     4,     1,     1,
       1,     4,     8,     9,     3,     4,     2,     3,     3,     4,
       4,     5,     4,     5,     3,     4,     1,     3,     2,     1,
       2,     2,     2,     3,     4,     5,     2,     4,     5,     4,
       5,     3,     4,     8,     9,     3,     4,     2,     1,     2,
       6,     8,     9,     3,     4,     2,     3,     4,     5,     4,
       5,     4,     5,     3,     4,     2,     4,     1,     2,     2,
       2,     3,     4,     2,     4,     4,     3,     6,     8,     3,
       2,     4,     1,     2,     2,     1,     1,     2,     3,     4,
       2,     4,     6,     8,     1,     2,     2,     1,     2,     2,
       3,     4,     1,     4,     4,     3,     5,     8,     3,     2,
       3,     7,     5,     1,     1,     1,     3,     3,     3,     5,
       1,     1,     5,     5,     6,     6,     0,     1,     1,     3,
       2,     2,     1,     2,     2,     3,     4,     1,     4,     4,
       3,     5,     8,     3,     1,     2,     1,     2,     6,     5,
       6,     7,     7,     1,     2,     2,     1,     2,     2,     3,
       4,     1,     4,     4,     3,     8,     3,     1,     1,     2,
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
#line 7887 "Parser/parser.cc"
    break;

  case 3:
#line 611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7893 "Parser/parser.cc"
    break;

  case 4:
#line 618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 7899 "Parser/parser.cc"
    break;

  case 5:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7905 "Parser/parser.cc"
    break;

  case 6:
#line 620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7911 "Parser/parser.cc"
    break;

  case 7:
#line 621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7917 "Parser/parser.cc"
    break;

  case 8:
#line 622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 7923 "Parser/parser.cc"
    break;

  case 20:
#line 644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7929 "Parser/parser.cc"
    break;

  case 21:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 7935 "Parser/parser.cc"
    break;

  case 22:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7941 "Parser/parser.cc"
    break;

  case 23:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7951 "Parser/parser.cc"
    break;

  case 24:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 7957 "Parser/parser.cc"
    break;

  case 25:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 7963 "Parser/parser.cc"
    break;

  case 26:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 7969 "Parser/parser.cc"
    break;

  case 28:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 7975 "Parser/parser.cc"
    break;

  case 29:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 7981 "Parser/parser.cc"
    break;

  case 30:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, (yyvsp[-2].decl), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 7987 "Parser/parser.cc"
    break;

  case 31:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 7993 "Parser/parser.cc"
    break;

  case 32:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8003 "Parser/parser.cc"
    break;

  case 33:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.expr) = nullptr; }
#line 8009 "Parser/parser.cc"
    break;

  case 34:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8015 "Parser/parser.cc"
    break;

  case 35:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8021 "Parser/parser.cc"
    break;

  case 36:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8027 "Parser/parser.cc"
    break;

  case 37:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8033 "Parser/parser.cc"
    break;

  case 38:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8039 "Parser/parser.cc"
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
#line 8051 "Parser/parser.cc"
    break;

  case 41:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8060 "Parser/parser.cc"
    break;

  case 42:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8066 "Parser/parser.cc"
    break;

  case 44:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) )) ) ); }
#line 8072 "Parser/parser.cc"
    break;

  case 45:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8078 "Parser/parser.cc"
    break;

  case 46:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8084 "Parser/parser.cc"
    break;

  case 47:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8090 "Parser/parser.cc"
    break;

  case 48:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8100 "Parser/parser.cc"
    break;

  case 49:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8106 "Parser/parser.cc"
    break;

  case 50:
#line 754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 8113 "Parser/parser.cc"
    break;

  case 51:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8119 "Parser/parser.cc"
    break;

  case 52:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8125 "Parser/parser.cc"
    break;

  case 53:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8131 "Parser/parser.cc"
    break;

  case 54:
#line 763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8137 "Parser/parser.cc"
    break;

  case 55:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8143 "Parser/parser.cc"
    break;

  case 56:
#line 767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8149 "Parser/parser.cc"
    break;

  case 57:
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8155 "Parser/parser.cc"
    break;

  case 58:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8161 "Parser/parser.cc"
    break;

  case 59:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8167 "Parser/parser.cc"
    break;

  case 60:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8173 "Parser/parser.cc"
    break;

  case 61:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8179 "Parser/parser.cc"
    break;

  case 62:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8185 "Parser/parser.cc"
    break;

  case 63:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8191 "Parser/parser.cc"
    break;

  case 64:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8197 "Parser/parser.cc"
    break;

  case 65:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8203 "Parser/parser.cc"
    break;

  case 66:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8213 "Parser/parser.cc"
    break;

  case 67:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8219 "Parser/parser.cc"
    break;

  case 70:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8225 "Parser/parser.cc"
    break;

  case 71:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8231 "Parser/parser.cc"
    break;

  case 74:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8237 "Parser/parser.cc"
    break;

  case 76:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8243 "Parser/parser.cc"
    break;

  case 77:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8249 "Parser/parser.cc"
    break;

  case 78:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8255 "Parser/parser.cc"
    break;

  case 79:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8261 "Parser/parser.cc"
    break;

  case 80:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8267 "Parser/parser.cc"
    break;

  case 81:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8273 "Parser/parser.cc"
    break;

  case 82:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8279 "Parser/parser.cc"
    break;

  case 83:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8285 "Parser/parser.cc"
    break;

  case 84:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8293 "Parser/parser.cc"
    break;

  case 85:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8299 "Parser/parser.cc"
    break;

  case 86:
#line 849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8308 "Parser/parser.cc"
    break;

  case 89:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8314 "Parser/parser.cc"
    break;

  case 90:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8320 "Parser/parser.cc"
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
#line 8340 "Parser/parser.cc"
    break;

  case 92:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8346 "Parser/parser.cc"
    break;

  case 93:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8352 "Parser/parser.cc"
    break;

  case 94:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8358 "Parser/parser.cc"
    break;

  case 95:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8364 "Parser/parser.cc"
    break;

  case 96:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8370 "Parser/parser.cc"
    break;

  case 97:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8376 "Parser/parser.cc"
    break;

  case 98:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8382 "Parser/parser.cc"
    break;

  case 99:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8388 "Parser/parser.cc"
    break;

  case 100:
#line 900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8397 "Parser/parser.cc"
    break;

  case 101:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 8403 "Parser/parser.cc"
    break;

  case 102:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 8409 "Parser/parser.cc"
    break;

  case 103:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 8415 "Parser/parser.cc"
    break;

  case 104:
#line 914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 8421 "Parser/parser.cc"
    break;

  case 105:
#line 915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 8427 "Parser/parser.cc"
    break;

  case 106:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 8433 "Parser/parser.cc"
    break;

  case 107:
#line 917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 8439 "Parser/parser.cc"
    break;

  case 109:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 8445 "Parser/parser.cc"
    break;

  case 110:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8451 "Parser/parser.cc"
    break;

  case 111:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8457 "Parser/parser.cc"
    break;

  case 112:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8463 "Parser/parser.cc"
    break;

  case 113:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8469 "Parser/parser.cc"
    break;

  case 114:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 8475 "Parser/parser.cc"
    break;

  case 115:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8481 "Parser/parser.cc"
    break;

  case 116:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8487 "Parser/parser.cc"
    break;

  case 124:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8493 "Parser/parser.cc"
    break;

  case 126:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8499 "Parser/parser.cc"
    break;

  case 127:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8505 "Parser/parser.cc"
    break;

  case 128:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8511 "Parser/parser.cc"
    break;

  case 130:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8517 "Parser/parser.cc"
    break;

  case 131:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8523 "Parser/parser.cc"
    break;

  case 133:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8529 "Parser/parser.cc"
    break;

  case 134:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8535 "Parser/parser.cc"
    break;

  case 136:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8541 "Parser/parser.cc"
    break;

  case 137:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8547 "Parser/parser.cc"
    break;

  case 138:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8553 "Parser/parser.cc"
    break;

  case 139:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8559 "Parser/parser.cc"
    break;

  case 141:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8565 "Parser/parser.cc"
    break;

  case 142:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8571 "Parser/parser.cc"
    break;

  case 144:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8577 "Parser/parser.cc"
    break;

  case 146:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8583 "Parser/parser.cc"
    break;

  case 148:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8589 "Parser/parser.cc"
    break;

  case 150:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 8595 "Parser/parser.cc"
    break;

  case 152:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 8601 "Parser/parser.cc"
    break;

  case 154:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8607 "Parser/parser.cc"
    break;

  case 155:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), (yyvsp[-3].expr)->clone(), (yyvsp[0].expr) ) ); }
#line 8613 "Parser/parser.cc"
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
#line 8625 "Parser/parser.cc"
    break;

  case 159:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8631 "Parser/parser.cc"
    break;

  case 160:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8637 "Parser/parser.cc"
    break;

  case 164:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 8643 "Parser/parser.cc"
    break;

  case 165:
#line 1077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 8649 "Parser/parser.cc"
    break;

  case 166:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 8655 "Parser/parser.cc"
    break;

  case 167:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 8661 "Parser/parser.cc"
    break;

  case 168:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 8667 "Parser/parser.cc"
    break;

  case 169:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 8673 "Parser/parser.cc"
    break;

  case 170:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 8679 "Parser/parser.cc"
    break;

  case 171:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 8685 "Parser/parser.cc"
    break;

  case 172:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 8691 "Parser/parser.cc"
    break;

  case 173:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 8697 "Parser/parser.cc"
    break;

  case 174:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 8703 "Parser/parser.cc"
    break;

  case 175:
#line 1090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 8709 "Parser/parser.cc"
    break;

  case 176:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 8715 "Parser/parser.cc"
    break;

  case 177:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].expr) ) ) ); }
#line 8721 "Parser/parser.cc"
    break;

  case 178:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) )); }
#line 8727 "Parser/parser.cc"
    break;

  case 180:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8733 "Parser/parser.cc"
    break;

  case 181:
#line 1112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8739 "Parser/parser.cc"
    break;

  case 182:
#line 1114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8745 "Parser/parser.cc"
    break;

  case 184:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8751 "Parser/parser.cc"
    break;

  case 185:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8757 "Parser/parser.cc"
    break;

  case 198:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 8763 "Parser/parser.cc"
    break;

  case 200:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 8769 "Parser/parser.cc"
    break;

  case 201:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8775 "Parser/parser.cc"
    break;

  case 202:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntx error, label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.stmt) = nullptr;
		}
#line 8786 "Parser/parser.cc"
    break;

  case 203:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 8792 "Parser/parser.cc"
    break;

  case 204:
#line 1170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 8798 "Parser/parser.cc"
    break;

  case 206:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8804 "Parser/parser.cc"
    break;

  case 207:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8810 "Parser/parser.cc"
    break;

  case 208:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8816 "Parser/parser.cc"
    break;

  case 209:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8822 "Parser/parser.cc"
    break;

  case 210:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8828 "Parser/parser.cc"
    break;

  case 213:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8834 "Parser/parser.cc"
    break;

  case 214:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 8840 "Parser/parser.cc"
    break;

  case 215:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 8846 "Parser/parser.cc"
    break;

  case 216:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8852 "Parser/parser.cc"
    break;

  case 217:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 8858 "Parser/parser.cc"
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
#line 8872 "Parser/parser.cc"
    break;

  case 219:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 8878 "Parser/parser.cc"
    break;

  case 220:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 8884 "Parser/parser.cc"
    break;

  case 221:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8893 "Parser/parser.cc"
    break;

  case 222:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 8899 "Parser/parser.cc"
    break;

  case 223:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 8905 "Parser/parser.cc"
    break;

  case 224:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 8911 "Parser/parser.cc"
    break;

  case 225:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 8917 "Parser/parser.cc"
    break;

  case 226:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8923 "Parser/parser.cc"
    break;

  case 227:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8929 "Parser/parser.cc"
    break;

  case 228:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 8935 "Parser/parser.cc"
    break;

  case 229:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 8941 "Parser/parser.cc"
    break;

  case 230:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8947 "Parser/parser.cc"
    break;

  case 232:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 8953 "Parser/parser.cc"
    break;

  case 233:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 8959 "Parser/parser.cc"
    break;

  case 234:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 8965 "Parser/parser.cc"
    break;

  case 235:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 8971 "Parser/parser.cc"
    break;

  case 236:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 8977 "Parser/parser.cc"
    break;

  case 237:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 8983 "Parser/parser.cc"
    break;

  case 238:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 8989 "Parser/parser.cc"
    break;

  case 240:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 8995 "Parser/parser.cc"
    break;

  case 241:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9001 "Parser/parser.cc"
    break;

  case 242:
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9007 "Parser/parser.cc"
    break;

  case 244:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9013 "Parser/parser.cc"
    break;

  case 245:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9019 "Parser/parser.cc"
    break;

  case 246:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9025 "Parser/parser.cc"
    break;

  case 247:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9034 "Parser/parser.cc"
    break;

  case 248:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9040 "Parser/parser.cc"
    break;

  case 249:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9046 "Parser/parser.cc"
    break;

  case 250:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9052 "Parser/parser.cc"
    break;

  case 251:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9061 "Parser/parser.cc"
    break;

  case 252:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9067 "Parser/parser.cc"
    break;

  case 253:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9073 "Parser/parser.cc"
    break;

  case 254:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9079 "Parser/parser.cc"
    break;

  case 255:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9088 "Parser/parser.cc"
    break;

  case 256:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9094 "Parser/parser.cc"
    break;

  case 257:
#line 1336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9100 "Parser/parser.cc"
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
#line 9119 "Parser/parser.cc"
    break;

  case 260:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9125 "Parser/parser.cc"
    break;

  case 261:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9134 "Parser/parser.cc"
    break;

  case 262:
#line 1371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9140 "Parser/parser.cc"
    break;

  case 263:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9146 "Parser/parser.cc"
    break;

  case 264:
#line 1376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9152 "Parser/parser.cc"
    break;

  case 265:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9158 "Parser/parser.cc"
    break;

  case 266:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9164 "Parser/parser.cc"
    break;

  case 267:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9170 "Parser/parser.cc"
    break;

  case 268:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9179 "Parser/parser.cc"
    break;

  case 269:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9188 "Parser/parser.cc"
    break;

  case 270:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9194 "Parser/parser.cc"
    break;

  case 271:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9203 "Parser/parser.cc"
    break;

  case 272:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9212 "Parser/parser.cc"
    break;

  case 273:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9218 "Parser/parser.cc"
    break;

  case 274:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9224 "Parser/parser.cc"
    break;

  case 275:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9230 "Parser/parser.cc"
    break;

  case 276:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9236 "Parser/parser.cc"
    break;

  case 277:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9242 "Parser/parser.cc"
    break;

  case 278:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9248 "Parser/parser.cc"
    break;

  case 279:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9254 "Parser/parser.cc"
    break;

  case 280:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9260 "Parser/parser.cc"
    break;

  case 281:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9269 "Parser/parser.cc"
    break;

  case 282:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9279 "Parser/parser.cc"
    break;

  case 283:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9285 "Parser/parser.cc"
    break;

  case 284:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9291 "Parser/parser.cc"
    break;

  case 285:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9300 "Parser/parser.cc"
    break;

  case 286:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9310 "Parser/parser.cc"
    break;

  case 287:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9316 "Parser/parser.cc"
    break;

  case 288:
#line 1455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9325 "Parser/parser.cc"
    break;

  case 289:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9335 "Parser/parser.cc"
    break;

  case 290:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9341 "Parser/parser.cc"
    break;

  case 291:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9347 "Parser/parser.cc"
    break;

  case 292:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9353 "Parser/parser.cc"
    break;

  case 293:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9359 "Parser/parser.cc"
    break;

  case 294:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9368 "Parser/parser.cc"
    break;

  case 295:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9378 "Parser/parser.cc"
    break;

  case 296:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9384 "Parser/parser.cc"
    break;

  case 297:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9393 "Parser/parser.cc"
    break;

  case 298:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9403 "Parser/parser.cc"
    break;

  case 299:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9409 "Parser/parser.cc"
    break;

  case 300:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9418 "Parser/parser.cc"
    break;

  case 301:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9428 "Parser/parser.cc"
    break;

  case 302:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9434 "Parser/parser.cc"
    break;

  case 303:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9443 "Parser/parser.cc"
    break;

  case 304:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 9454 "Parser/parser.cc"
    break;

  case 305:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9460 "Parser/parser.cc"
    break;

  case 306:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9466 "Parser/parser.cc"
    break;

  case 307:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9472 "Parser/parser.cc"
    break;

  case 308:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 9478 "Parser/parser.cc"
    break;

  case 309:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9484 "Parser/parser.cc"
    break;

  case 311:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9490 "Parser/parser.cc"
    break;

  case 312:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9496 "Parser/parser.cc"
    break;

  case 313:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 9502 "Parser/parser.cc"
    break;

  case 314:
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 9508 "Parser/parser.cc"
    break;

  case 315:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 9514 "Parser/parser.cc"
    break;

  case 316:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 9520 "Parser/parser.cc"
    break;

  case 317:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 9526 "Parser/parser.cc"
    break;

  case 318:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 9532 "Parser/parser.cc"
    break;

  case 319:
#line 1574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 9538 "Parser/parser.cc"
    break;

  case 320:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 9544 "Parser/parser.cc"
    break;

  case 321:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 9550 "Parser/parser.cc"
    break;

  case 322:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 9556 "Parser/parser.cc"
    break;

  case 323:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9562 "Parser/parser.cc"
    break;

  case 324:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 9568 "Parser/parser.cc"
    break;

  case 325:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 9574 "Parser/parser.cc"
    break;

  case 326:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 9580 "Parser/parser.cc"
    break;

  case 327:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 9586 "Parser/parser.cc"
    break;

  case 328:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 9592 "Parser/parser.cc"
    break;

  case 329:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 9598 "Parser/parser.cc"
    break;

  case 330:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 9604 "Parser/parser.cc"
    break;

  case 331:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 9610 "Parser/parser.cc"
    break;

  case 332:
#line 1603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9616 "Parser/parser.cc"
    break;

  case 335:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9622 "Parser/parser.cc"
    break;

  case 336:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 9631 "Parser/parser.cc"
    break;

  case 337:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9637 "Parser/parser.cc"
    break;

  case 338:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9643 "Parser/parser.cc"
    break;

  case 341:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9649 "Parser/parser.cc"
    break;

  case 342:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9655 "Parser/parser.cc"
    break;

  case 345:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9661 "Parser/parser.cc"
    break;

  case 346:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) )); }
#line 9667 "Parser/parser.cc"
    break;

  case 347:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9673 "Parser/parser.cc"
    break;

  case 348:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9679 "Parser/parser.cc"
    break;

  case 349:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 350:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9691 "Parser/parser.cc"
    break;

  case 351:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9697 "Parser/parser.cc"
    break;

  case 352:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9703 "Parser/parser.cc"
    break;

  case 353:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 9709 "Parser/parser.cc"
    break;

  case 356:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9715 "Parser/parser.cc"
    break;

  case 357:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9721 "Parser/parser.cc"
    break;

  case 358:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 9727 "Parser/parser.cc"
    break;

  case 359:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9733 "Parser/parser.cc"
    break;

  case 360:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9739 "Parser/parser.cc"
    break;

  case 361:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9745 "Parser/parser.cc"
    break;

  case 362:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9751 "Parser/parser.cc"
    break;

  case 363:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9757 "Parser/parser.cc"
    break;

  case 364:
#line 1711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_timeout( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9763 "Parser/parser.cc"
    break;

  case 365:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wucn) = nullptr; }
#line 9769 "Parser/parser.cc"
    break;

  case 366:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-8].wucn),
                new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, 
                    build_waituntil_timeout( yylloc, (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), 
                    build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9778 "Parser/parser.cc"
    break;

  case 367:
#line 1725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
            (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );
            // $$ = new StatementNode( build_compound( yylloc, nullptr ) );
        }
#line 9787 "Parser/parser.cc"
    break;

  case 368:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 9793 "Parser/parser.cc"
    break;

  case 369:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 9799 "Parser/parser.cc"
    break;

  case 370:
#line 1737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 9805 "Parser/parser.cc"
    break;

  case 371:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9811 "Parser/parser.cc"
    break;

  case 372:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 9817 "Parser/parser.cc"
    break;

  case 373:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9823 "Parser/parser.cc"
    break;

  case 374:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9829 "Parser/parser.cc"
    break;

  case 375:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9835 "Parser/parser.cc"
    break;

  case 376:
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9841 "Parser/parser.cc"
    break;

  case 377:
#line 1756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 9847 "Parser/parser.cc"
    break;

  case 378:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 9853 "Parser/parser.cc"
    break;

  case 379:
#line 1761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 9859 "Parser/parser.cc"
    break;

  case 381:
#line 1768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9865 "Parser/parser.cc"
    break;

  case 382:
#line 1770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9871 "Parser/parser.cc"
    break;

  case 383:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9877 "Parser/parser.cc"
    break;

  case 388:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 9883 "Parser/parser.cc"
    break;

  case 389:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9889 "Parser/parser.cc"
    break;

  case 390:
#line 1791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9895 "Parser/parser.cc"
    break;

  case 391:
#line 1793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9901 "Parser/parser.cc"
    break;

  case 392:
#line 1795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 9907 "Parser/parser.cc"
    break;

  case 393:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 9913 "Parser/parser.cc"
    break;

  case 394:
#line 1802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 9919 "Parser/parser.cc"
    break;

  case 395:
#line 1807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9925 "Parser/parser.cc"
    break;

  case 398:
#line 1814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 9931 "Parser/parser.cc"
    break;

  case 399:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 9937 "Parser/parser.cc"
    break;

  case 400:
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 9946 "Parser/parser.cc"
    break;

  case 401:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9952 "Parser/parser.cc"
    break;

  case 402:
#line 1831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 9958 "Parser/parser.cc"
    break;

  case 403:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ) ); }
#line 9964 "Parser/parser.cc"
    break;

  case 404:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9973 "Parser/parser.cc"
    break;

  case 405:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9982 "Parser/parser.cc"
    break;

  case 406:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9988 "Parser/parser.cc"
    break;

  case 409:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9994 "Parser/parser.cc"
    break;

  case 410:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10000 "Parser/parser.cc"
    break;

  case 412:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10006 "Parser/parser.cc"
    break;

  case 413:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 10012 "Parser/parser.cc"
    break;

  case 420:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10023 "Parser/parser.cc"
    break;

  case 423:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10029 "Parser/parser.cc"
    break;

  case 424:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10035 "Parser/parser.cc"
    break;

  case 428:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10041 "Parser/parser.cc"
    break;

  case 430:
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10047 "Parser/parser.cc"
    break;

  case 431:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10053 "Parser/parser.cc"
    break;

  case 432:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10059 "Parser/parser.cc"
    break;

  case 433:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10065 "Parser/parser.cc"
    break;

  case 434:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10071 "Parser/parser.cc"
    break;

  case 435:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10077 "Parser/parser.cc"
    break;

  case 437:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10083 "Parser/parser.cc"
    break;

  case 438:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10089 "Parser/parser.cc"
    break;

  case 439:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10095 "Parser/parser.cc"
    break;

  case 440:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10106 "Parser/parser.cc"
    break;

  case 441:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10112 "Parser/parser.cc"
    break;

  case 442:
#line 1995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10118 "Parser/parser.cc"
    break;

  case 443:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10124 "Parser/parser.cc"
    break;

  case 444:
#line 2003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 10130 "Parser/parser.cc"
    break;

  case 445:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10139 "Parser/parser.cc"
    break;

  case 446:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10148 "Parser/parser.cc"
    break;

  case 447:
#line 2018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10157 "Parser/parser.cc"
    break;

  case 448:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10168 "Parser/parser.cc"
    break;

  case 449:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10177 "Parser/parser.cc"
    break;

  case 450:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10183 "Parser/parser.cc"
    break;

  case 451:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10189 "Parser/parser.cc"
    break;

  case 452:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10195 "Parser/parser.cc"
    break;

  case 453:
#line 2051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10203 "Parser/parser.cc"
    break;

  case 454:
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10211 "Parser/parser.cc"
    break;

  case 455:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10217 "Parser/parser.cc"
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
#line 10232 "Parser/parser.cc"
    break;

  case 459:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10238 "Parser/parser.cc"
    break;

  case 460:
#line 2084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10244 "Parser/parser.cc"
    break;

  case 461:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10250 "Parser/parser.cc"
    break;

  case 462:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10256 "Parser/parser.cc"
    break;

  case 463:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10262 "Parser/parser.cc"
    break;

  case 469:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntax error, expecting ';' at end of ",
				(yyvsp[-1].decl)->type->enumeration.name ? "enum" : ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ),
				" declaration." ) );
			(yyval.decl) = nullptr;
		}
#line 10273 "Parser/parser.cc"
    break;

  case 482:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10279 "Parser/parser.cc"
    break;

  case 485:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10285 "Parser/parser.cc"
    break;

  case 488:
#line 2171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 10291 "Parser/parser.cc"
    break;

  case 489:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 10297 "Parser/parser.cc"
    break;

  case 490:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 10303 "Parser/parser.cc"
    break;

  case 491:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 10309 "Parser/parser.cc"
    break;

  case 492:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 10315 "Parser/parser.cc"
    break;

  case 493:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10321 "Parser/parser.cc"
    break;

  case 495:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10327 "Parser/parser.cc"
    break;

  case 496:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10333 "Parser/parser.cc"
    break;

  case 498:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10339 "Parser/parser.cc"
    break;

  case 499:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 10345 "Parser/parser.cc"
    break;

  case 500:
#line 2210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 10351 "Parser/parser.cc"
    break;

  case 501:
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10357 "Parser/parser.cc"
    break;

  case 502:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10363 "Parser/parser.cc"
    break;

  case 503:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10369 "Parser/parser.cc"
    break;

  case 504:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10375 "Parser/parser.cc"
    break;

  case 505:
#line 2221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10381 "Parser/parser.cc"
    break;

  case 506:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10387 "Parser/parser.cc"
    break;

  case 507:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10393 "Parser/parser.cc"
    break;

  case 508:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10399 "Parser/parser.cc"
    break;

  case 509:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10405 "Parser/parser.cc"
    break;

  case 510:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10411 "Parser/parser.cc"
    break;

  case 511:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10417 "Parser/parser.cc"
    break;

  case 512:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10423 "Parser/parser.cc"
    break;

  case 513:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10429 "Parser/parser.cc"
    break;

  case 514:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10435 "Parser/parser.cc"
    break;

  case 515:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10441 "Parser/parser.cc"
    break;

  case 516:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10447 "Parser/parser.cc"
    break;

  case 517:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10453 "Parser/parser.cc"
    break;

  case 518:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10459 "Parser/parser.cc"
    break;

  case 519:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10465 "Parser/parser.cc"
    break;

  case 520:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10471 "Parser/parser.cc"
    break;

  case 521:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10477 "Parser/parser.cc"
    break;

  case 522:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10483 "Parser/parser.cc"
    break;

  case 523:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10489 "Parser/parser.cc"
    break;

  case 524:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10495 "Parser/parser.cc"
    break;

  case 525:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10501 "Parser/parser.cc"
    break;

  case 526:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10507 "Parser/parser.cc"
    break;

  case 527:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10513 "Parser/parser.cc"
    break;

  case 528:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10519 "Parser/parser.cc"
    break;

  case 529:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10525 "Parser/parser.cc"
    break;

  case 530:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10531 "Parser/parser.cc"
    break;

  case 531:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10537 "Parser/parser.cc"
    break;

  case 532:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10543 "Parser/parser.cc"
    break;

  case 533:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10549 "Parser/parser.cc"
    break;

  case 534:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10555 "Parser/parser.cc"
    break;

  case 536:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10561 "Parser/parser.cc"
    break;

  case 538:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10567 "Parser/parser.cc"
    break;

  case 539:
#line 2300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10573 "Parser/parser.cc"
    break;

  case 540:
#line 2302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10579 "Parser/parser.cc"
    break;

  case 542:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10585 "Parser/parser.cc"
    break;

  case 543:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10591 "Parser/parser.cc"
    break;

  case 544:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10597 "Parser/parser.cc"
    break;

  case 545:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10603 "Parser/parser.cc"
    break;

  case 547:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10609 "Parser/parser.cc"
    break;

  case 549:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10615 "Parser/parser.cc"
    break;

  case 550:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10621 "Parser/parser.cc"
    break;

  case 551:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10627 "Parser/parser.cc"
    break;

  case 552:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10633 "Parser/parser.cc"
    break;

  case 553:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 10639 "Parser/parser.cc"
    break;

  case 554:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10645 "Parser/parser.cc"
    break;

  case 555:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 10651 "Parser/parser.cc"
    break;

  case 556:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10657 "Parser/parser.cc"
    break;

  case 557:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10663 "Parser/parser.cc"
    break;

  case 558:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10674 "Parser/parser.cc"
    break;

  case 559:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10680 "Parser/parser.cc"
    break;

  case 560:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10686 "Parser/parser.cc"
    break;

  case 561:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10692 "Parser/parser.cc"
    break;

  case 562:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10703 "Parser/parser.cc"
    break;

  case 563:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10709 "Parser/parser.cc"
    break;

  case 564:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10715 "Parser/parser.cc"
    break;

  case 565:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10724 "Parser/parser.cc"
    break;

  case 567:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10730 "Parser/parser.cc"
    break;

  case 568:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10736 "Parser/parser.cc"
    break;

  case 569:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10742 "Parser/parser.cc"
    break;

  case 571:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10748 "Parser/parser.cc"
    break;

  case 572:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10754 "Parser/parser.cc"
    break;

  case 574:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10760 "Parser/parser.cc"
    break;

  case 575:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10766 "Parser/parser.cc"
    break;

  case 576:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10772 "Parser/parser.cc"
    break;

  case 578:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10778 "Parser/parser.cc"
    break;

  case 579:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10784 "Parser/parser.cc"
    break;

  case 580:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10790 "Parser/parser.cc"
    break;

  case 581:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10796 "Parser/parser.cc"
    break;

  case 582:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10802 "Parser/parser.cc"
    break;

  case 584:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10808 "Parser/parser.cc"
    break;

  case 585:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10814 "Parser/parser.cc"
    break;

  case 586:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10820 "Parser/parser.cc"
    break;

  case 587:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10826 "Parser/parser.cc"
    break;

  case 588:
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 10832 "Parser/parser.cc"
    break;

  case 589:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10843 "Parser/parser.cc"
    break;

  case 593:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10849 "Parser/parser.cc"
    break;

  case 594:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10855 "Parser/parser.cc"
    break;

  case 595:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10864 "Parser/parser.cc"
    break;

  case 596:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10872 "Parser/parser.cc"
    break;

  case 597:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10881 "Parser/parser.cc"
    break;

  case 598:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10890 "Parser/parser.cc"
    break;

  case 599:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10899 "Parser/parser.cc"
    break;

  case 600:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10908 "Parser/parser.cc"
    break;

  case 602:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10914 "Parser/parser.cc"
    break;

  case 603:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10920 "Parser/parser.cc"
    break;

  case 604:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10930 "Parser/parser.cc"
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
#line 10949 "Parser/parser.cc"
    break;

  case 608:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 10955 "Parser/parser.cc"
    break;

  case 609:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 10961 "Parser/parser.cc"
    break;

  case 610:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 10967 "Parser/parser.cc"
    break;

  case 611:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 10973 "Parser/parser.cc"
    break;

  case 612:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 10979 "Parser/parser.cc"
    break;

  case 613:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 10985 "Parser/parser.cc"
    break;

  case 614:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 10994 "Parser/parser.cc"
    break;

  case 615:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11000 "Parser/parser.cc"
    break;

  case 616:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11009 "Parser/parser.cc"
    break;

  case 617:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11015 "Parser/parser.cc"
    break;

  case 618:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11024 "Parser/parser.cc"
    break;

  case 619:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11030 "Parser/parser.cc"
    break;

  case 620:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11036 "Parser/parser.cc"
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
#line 11049 "Parser/parser.cc"
    break;

  case 622:
#line 2589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntax error, expecting ';' at end of previous declaration." ) );
			(yyval.decl) = nullptr;
		}
#line 11058 "Parser/parser.cc"
    break;

  case 623:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11064 "Parser/parser.cc"
    break;

  case 624:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11070 "Parser/parser.cc"
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
#line 11083 "Parser/parser.cc"
    break;

  case 626:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11089 "Parser/parser.cc"
    break;

  case 629:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11095 "Parser/parser.cc"
    break;

  case 630:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11101 "Parser/parser.cc"
    break;

  case 633:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11107 "Parser/parser.cc"
    break;

  case 635:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11113 "Parser/parser.cc"
    break;

  case 636:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11119 "Parser/parser.cc"
    break;

  case 637:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11125 "Parser/parser.cc"
    break;

  case 638:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11131 "Parser/parser.cc"
    break;

  case 639:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11137 "Parser/parser.cc"
    break;

  case 640:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11143 "Parser/parser.cc"
    break;

  case 642:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11149 "Parser/parser.cc"
    break;

  case 644:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11155 "Parser/parser.cc"
    break;

  case 645:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11161 "Parser/parser.cc"
    break;

  case 647:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11167 "Parser/parser.cc"
    break;

  case 648:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11173 "Parser/parser.cc"
    break;

  case 650:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11179 "Parser/parser.cc"
    break;

  case 651:
#line 2681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11185 "Parser/parser.cc"
    break;

  case 652:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11191 "Parser/parser.cc"
    break;

  case 653:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 11197 "Parser/parser.cc"
    break;

  case 654:
#line 2687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11203 "Parser/parser.cc"
    break;

  case 655:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11209 "Parser/parser.cc"
    break;

  case 656:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11220 "Parser/parser.cc"
    break;

  case 657:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11226 "Parser/parser.cc"
    break;

  case 658:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11234 "Parser/parser.cc"
    break;

  case 659:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11240 "Parser/parser.cc"
    break;

  case 660:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 11251 "Parser/parser.cc"
    break;

  case 661:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11259 "Parser/parser.cc"
    break;

  case 662:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11267 "Parser/parser.cc"
    break;

  case 663:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11275 "Parser/parser.cc"
    break;

  case 664:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11283 "Parser/parser.cc"
    break;

  case 666:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11289 "Parser/parser.cc"
    break;

  case 667:
#line 2735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11295 "Parser/parser.cc"
    break;

  case 668:
#line 2740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11301 "Parser/parser.cc"
    break;

  case 669:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11307 "Parser/parser.cc"
    break;

  case 670:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11313 "Parser/parser.cc"
    break;

  case 671:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 11319 "Parser/parser.cc"
    break;

  case 672:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11325 "Parser/parser.cc"
    break;

  case 673:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11331 "Parser/parser.cc"
    break;

  case 675:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11337 "Parser/parser.cc"
    break;

  case 676:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11343 "Parser/parser.cc"
    break;

  case 677:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11349 "Parser/parser.cc"
    break;

  case 678:
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11355 "Parser/parser.cc"
    break;

  case 679:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11361 "Parser/parser.cc"
    break;

  case 680:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11367 "Parser/parser.cc"
    break;

  case 683:
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11373 "Parser/parser.cc"
    break;

  case 684:
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11379 "Parser/parser.cc"
    break;

  case 685:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11385 "Parser/parser.cc"
    break;

  case 687:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11391 "Parser/parser.cc"
    break;

  case 688:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11397 "Parser/parser.cc"
    break;

  case 689:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11403 "Parser/parser.cc"
    break;

  case 691:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11409 "Parser/parser.cc"
    break;

  case 692:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11415 "Parser/parser.cc"
    break;

  case 693:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11421 "Parser/parser.cc"
    break;

  case 695:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11427 "Parser/parser.cc"
    break;

  case 698:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11433 "Parser/parser.cc"
    break;

  case 699:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11439 "Parser/parser.cc"
    break;

  case 701:
#line 2829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11445 "Parser/parser.cc"
    break;

  case 702:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11451 "Parser/parser.cc"
    break;

  case 703:
#line 2834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11457 "Parser/parser.cc"
    break;

  case 708:
#line 2844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11463 "Parser/parser.cc"
    break;

  case 710:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11469 "Parser/parser.cc"
    break;

  case 711:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11475 "Parser/parser.cc"
    break;

  case 712:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11481 "Parser/parser.cc"
    break;

  case 713:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11487 "Parser/parser.cc"
    break;

  case 714:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11493 "Parser/parser.cc"
    break;

  case 715:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11499 "Parser/parser.cc"
    break;

  case 721:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11505 "Parser/parser.cc"
    break;

  case 724:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11511 "Parser/parser.cc"
    break;

  case 725:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 11517 "Parser/parser.cc"
    break;

  case 726:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 11523 "Parser/parser.cc"
    break;

  case 727:
#line 2897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11529 "Parser/parser.cc"
    break;

  case 728:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11535 "Parser/parser.cc"
    break;

  case 729:
#line 2902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11541 "Parser/parser.cc"
    break;

  case 730:
#line 2907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11547 "Parser/parser.cc"
    break;

  case 732:
#line 2909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 11553 "Parser/parser.cc"
    break;

  case 733:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (InitializerNode *)( (yyvsp[-2].init)->set_last( (yyvsp[0].init) ) ); }
#line 11559 "Parser/parser.cc"
    break;

  case 734:
#line 2911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (InitializerNode *)((yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) )); }
#line 11565 "Parser/parser.cc"
    break;

  case 736:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 11571 "Parser/parser.cc"
    break;

  case 738:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-1].expr)->set_last( (yyvsp[0].expr) )); }
#line 11577 "Parser/parser.cc"
    break;

  case 739:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 11583 "Parser/parser.cc"
    break;

  case 740:
#line 2942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11589 "Parser/parser.cc"
    break;

  case 741:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11595 "Parser/parser.cc"
    break;

  case 742:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 11601 "Parser/parser.cc"
    break;

  case 743:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11607 "Parser/parser.cc"
    break;

  case 745:
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11613 "Parser/parser.cc"
    break;

  case 746:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11619 "Parser/parser.cc"
    break;

  case 747:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11625 "Parser/parser.cc"
    break;

  case 748:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11636 "Parser/parser.cc"
    break;

  case 749:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11642 "Parser/parser.cc"
    break;

  case 750:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11648 "Parser/parser.cc"
    break;

  case 751:
#line 2995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11654 "Parser/parser.cc"
    break;

  case 752:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11663 "Parser/parser.cc"
    break;

  case 753:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11669 "Parser/parser.cc"
    break;

  case 754:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11675 "Parser/parser.cc"
    break;

  case 755:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11681 "Parser/parser.cc"
    break;

  case 756:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 11687 "Parser/parser.cc"
    break;

  case 757:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11693 "Parser/parser.cc"
    break;

  case 758:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11699 "Parser/parser.cc"
    break;

  case 759:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11705 "Parser/parser.cc"
    break;

  case 760:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 11711 "Parser/parser.cc"
    break;

  case 761:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11717 "Parser/parser.cc"
    break;

  case 762:
#line 3032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11723 "Parser/parser.cc"
    break;

  case 765:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11729 "Parser/parser.cc"
    break;

  case 766:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11735 "Parser/parser.cc"
    break;

  case 767:
#line 3046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11741 "Parser/parser.cc"
    break;

  case 768:
#line 3053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11747 "Parser/parser.cc"
    break;

  case 770:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11753 "Parser/parser.cc"
    break;

  case 771:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 11759 "Parser/parser.cc"
    break;

  case 772:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11765 "Parser/parser.cc"
    break;

  case 773:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11771 "Parser/parser.cc"
    break;

  case 774:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11777 "Parser/parser.cc"
    break;

  case 775:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11783 "Parser/parser.cc"
    break;

  case 776:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11789 "Parser/parser.cc"
    break;

  case 777:
#line 3079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11798 "Parser/parser.cc"
    break;

  case 778:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11807 "Parser/parser.cc"
    break;

  case 779:
#line 3092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11816 "Parser/parser.cc"
    break;

  case 780:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11822 "Parser/parser.cc"
    break;

  case 781:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11831 "Parser/parser.cc"
    break;

  case 782:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11837 "Parser/parser.cc"
    break;

  case 784:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11843 "Parser/parser.cc"
    break;

  case 789:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11849 "Parser/parser.cc"
    break;

  case 790:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11855 "Parser/parser.cc"
    break;

  case 791:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11861 "Parser/parser.cc"
    break;

  case 793:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11867 "Parser/parser.cc"
    break;

  case 794:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11873 "Parser/parser.cc"
    break;

  case 795:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11879 "Parser/parser.cc"
    break;

  case 796:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11885 "Parser/parser.cc"
    break;

  case 798:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11891 "Parser/parser.cc"
    break;

  case 799:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11897 "Parser/parser.cc"
    break;

  case 800:
#line 3163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 11903 "Parser/parser.cc"
    break;

  case 801:
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11920 "Parser/parser.cc"
    break;

  case 802:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11926 "Parser/parser.cc"
    break;

  case 803:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11932 "Parser/parser.cc"
    break;

  case 804:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11938 "Parser/parser.cc"
    break;

  case 805:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11944 "Parser/parser.cc"
    break;

  case 806:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11950 "Parser/parser.cc"
    break;

  case 807:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11956 "Parser/parser.cc"
    break;

  case 809:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11965 "Parser/parser.cc"
    break;

  case 810:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 11971 "Parser/parser.cc"
    break;

  case 811:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11980 "Parser/parser.cc"
    break;

  case 812:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11990 "Parser/parser.cc"
    break;

  case 813:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11999 "Parser/parser.cc"
    break;

  case 814:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12009 "Parser/parser.cc"
    break;

  case 815:
#line 3221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12020 "Parser/parser.cc"
    break;

  case 816:
#line 3228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12030 "Parser/parser.cc"
    break;

  case 817:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12041 "Parser/parser.cc"
    break;

  case 818:
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12051 "Parser/parser.cc"
    break;

  case 819:
#line 3247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12062 "Parser/parser.cc"
    break;

  case 820:
#line 3254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12072 "Parser/parser.cc"
    break;

  case 822:
#line 3269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12078 "Parser/parser.cc"
    break;

  case 823:
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12084 "Parser/parser.cc"
    break;

  case 824:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12090 "Parser/parser.cc"
    break;

  case 825:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12102 "Parser/parser.cc"
    break;

  case 826:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12113 "Parser/parser.cc"
    break;

  case 827:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12122 "Parser/parser.cc"
    break;

  case 828:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12131 "Parser/parser.cc"
    break;

  case 829:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12137 "Parser/parser.cc"
    break;

  case 830:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12143 "Parser/parser.cc"
    break;

  case 831:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12149 "Parser/parser.cc"
    break;

  case 832:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12158 "Parser/parser.cc"
    break;

  case 833:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12164 "Parser/parser.cc"
    break;

  case 834:
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12170 "Parser/parser.cc"
    break;

  case 835:
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12176 "Parser/parser.cc"
    break;

  case 840:
#line 3341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12182 "Parser/parser.cc"
    break;

  case 841:
#line 3346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12188 "Parser/parser.cc"
    break;

  case 842:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12198 "Parser/parser.cc"
    break;

  case 843:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12204 "Parser/parser.cc"
    break;

  case 846:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12210 "Parser/parser.cc"
    break;

  case 847:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12216 "Parser/parser.cc"
    break;

  case 849:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12222 "Parser/parser.cc"
    break;

  case 850:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12228 "Parser/parser.cc"
    break;

  case 851:
#line 3382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12234 "Parser/parser.cc"
    break;

  case 852:
#line 3384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12240 "Parser/parser.cc"
    break;

  case 857:
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12246 "Parser/parser.cc"
    break;

  case 858:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12252 "Parser/parser.cc"
    break;

  case 859:
#line 3430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12258 "Parser/parser.cc"
    break;

  case 860:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12264 "Parser/parser.cc"
    break;

  case 861:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12270 "Parser/parser.cc"
    break;

  case 863:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12276 "Parser/parser.cc"
    break;

  case 864:
#line 3442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12282 "Parser/parser.cc"
    break;

  case 865:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12288 "Parser/parser.cc"
    break;

  case 866:
#line 3449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12294 "Parser/parser.cc"
    break;

  case 867:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12300 "Parser/parser.cc"
    break;

  case 868:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12306 "Parser/parser.cc"
    break;

  case 869:
#line 3458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12312 "Parser/parser.cc"
    break;

  case 870:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12318 "Parser/parser.cc"
    break;

  case 871:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12324 "Parser/parser.cc"
    break;

  case 872:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12330 "Parser/parser.cc"
    break;

  case 873:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12336 "Parser/parser.cc"
    break;

  case 874:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12342 "Parser/parser.cc"
    break;

  case 875:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12348 "Parser/parser.cc"
    break;

  case 876:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12354 "Parser/parser.cc"
    break;

  case 877:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12360 "Parser/parser.cc"
    break;

  case 878:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12366 "Parser/parser.cc"
    break;

  case 879:
#line 3481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12372 "Parser/parser.cc"
    break;

  case 880:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12378 "Parser/parser.cc"
    break;

  case 882:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12384 "Parser/parser.cc"
    break;

  case 883:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12390 "Parser/parser.cc"
    break;

  case 884:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12396 "Parser/parser.cc"
    break;

  case 885:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12402 "Parser/parser.cc"
    break;

  case 886:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12408 "Parser/parser.cc"
    break;

  case 887:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12414 "Parser/parser.cc"
    break;

  case 888:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12420 "Parser/parser.cc"
    break;

  case 889:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12426 "Parser/parser.cc"
    break;

  case 890:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12432 "Parser/parser.cc"
    break;

  case 891:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12438 "Parser/parser.cc"
    break;

  case 892:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12444 "Parser/parser.cc"
    break;

  case 893:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12450 "Parser/parser.cc"
    break;

  case 894:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12456 "Parser/parser.cc"
    break;

  case 895:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12462 "Parser/parser.cc"
    break;

  case 896:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12468 "Parser/parser.cc"
    break;

  case 897:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12474 "Parser/parser.cc"
    break;

  case 901:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12480 "Parser/parser.cc"
    break;

  case 902:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12486 "Parser/parser.cc"
    break;

  case 903:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12492 "Parser/parser.cc"
    break;

  case 904:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12498 "Parser/parser.cc"
    break;

  case 905:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12504 "Parser/parser.cc"
    break;

  case 906:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12510 "Parser/parser.cc"
    break;

  case 907:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12516 "Parser/parser.cc"
    break;

  case 908:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12522 "Parser/parser.cc"
    break;

  case 909:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12528 "Parser/parser.cc"
    break;

  case 910:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12534 "Parser/parser.cc"
    break;

  case 911:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12540 "Parser/parser.cc"
    break;

  case 912:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12546 "Parser/parser.cc"
    break;

  case 913:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12552 "Parser/parser.cc"
    break;

  case 914:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12558 "Parser/parser.cc"
    break;

  case 915:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12564 "Parser/parser.cc"
    break;

  case 916:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12573 "Parser/parser.cc"
    break;

  case 917:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12579 "Parser/parser.cc"
    break;

  case 918:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12585 "Parser/parser.cc"
    break;

  case 920:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12591 "Parser/parser.cc"
    break;

  case 921:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12597 "Parser/parser.cc"
    break;

  case 922:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12603 "Parser/parser.cc"
    break;

  case 923:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12609 "Parser/parser.cc"
    break;

  case 924:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12615 "Parser/parser.cc"
    break;

  case 925:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12621 "Parser/parser.cc"
    break;

  case 926:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12627 "Parser/parser.cc"
    break;

  case 927:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12633 "Parser/parser.cc"
    break;

  case 928:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12639 "Parser/parser.cc"
    break;

  case 929:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12645 "Parser/parser.cc"
    break;

  case 930:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12651 "Parser/parser.cc"
    break;

  case 931:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12657 "Parser/parser.cc"
    break;

  case 932:
#line 3639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12663 "Parser/parser.cc"
    break;

  case 933:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12669 "Parser/parser.cc"
    break;

  case 934:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12675 "Parser/parser.cc"
    break;

  case 935:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12681 "Parser/parser.cc"
    break;

  case 936:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12687 "Parser/parser.cc"
    break;

  case 937:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12693 "Parser/parser.cc"
    break;

  case 939:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12699 "Parser/parser.cc"
    break;

  case 940:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12705 "Parser/parser.cc"
    break;

  case 941:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12711 "Parser/parser.cc"
    break;

  case 942:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12717 "Parser/parser.cc"
    break;

  case 943:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12723 "Parser/parser.cc"
    break;

  case 944:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12729 "Parser/parser.cc"
    break;

  case 945:
#line 3680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12735 "Parser/parser.cc"
    break;

  case 946:
#line 3682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12741 "Parser/parser.cc"
    break;

  case 947:
#line 3684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12747 "Parser/parser.cc"
    break;

  case 948:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12753 "Parser/parser.cc"
    break;

  case 949:
#line 3691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12759 "Parser/parser.cc"
    break;

  case 950:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12765 "Parser/parser.cc"
    break;

  case 951:
#line 3695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12771 "Parser/parser.cc"
    break;

  case 952:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12777 "Parser/parser.cc"
    break;

  case 953:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12783 "Parser/parser.cc"
    break;

  case 954:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12789 "Parser/parser.cc"
    break;

  case 955:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12795 "Parser/parser.cc"
    break;

  case 956:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12801 "Parser/parser.cc"
    break;

  case 958:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12807 "Parser/parser.cc"
    break;

  case 959:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12813 "Parser/parser.cc"
    break;

  case 960:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12819 "Parser/parser.cc"
    break;

  case 961:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12825 "Parser/parser.cc"
    break;

  case 962:
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12831 "Parser/parser.cc"
    break;

  case 963:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12837 "Parser/parser.cc"
    break;

  case 964:
#line 3734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12843 "Parser/parser.cc"
    break;

  case 965:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12849 "Parser/parser.cc"
    break;

  case 966:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12855 "Parser/parser.cc"
    break;

  case 967:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12861 "Parser/parser.cc"
    break;

  case 968:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12867 "Parser/parser.cc"
    break;

  case 969:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12873 "Parser/parser.cc"
    break;

  case 970:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12879 "Parser/parser.cc"
    break;

  case 971:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12885 "Parser/parser.cc"
    break;

  case 973:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12891 "Parser/parser.cc"
    break;

  case 974:
#line 3768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12897 "Parser/parser.cc"
    break;

  case 975:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12903 "Parser/parser.cc"
    break;

  case 976:
#line 3775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12909 "Parser/parser.cc"
    break;

  case 977:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12915 "Parser/parser.cc"
    break;

  case 978:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12921 "Parser/parser.cc"
    break;

  case 979:
#line 3784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12927 "Parser/parser.cc"
    break;

  case 980:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12933 "Parser/parser.cc"
    break;

  case 981:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12939 "Parser/parser.cc"
    break;

  case 982:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12945 "Parser/parser.cc"
    break;

  case 983:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12951 "Parser/parser.cc"
    break;

  case 985:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12957 "Parser/parser.cc"
    break;

  case 986:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12963 "Parser/parser.cc"
    break;

  case 987:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 12969 "Parser/parser.cc"
    break;

  case 988:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 12975 "Parser/parser.cc"
    break;

  case 989:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12981 "Parser/parser.cc"
    break;

  case 990:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12987 "Parser/parser.cc"
    break;

  case 991:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12993 "Parser/parser.cc"
    break;

  case 993:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12999 "Parser/parser.cc"
    break;

  case 994:
#line 3839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13005 "Parser/parser.cc"
    break;

  case 995:
#line 3841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13011 "Parser/parser.cc"
    break;

  case 996:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 13017 "Parser/parser.cc"
    break;

  case 997:
#line 3848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13023 "Parser/parser.cc"
    break;

  case 998:
#line 3850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13029 "Parser/parser.cc"
    break;

  case 999:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13035 "Parser/parser.cc"
    break;

  case 1000:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13041 "Parser/parser.cc"
    break;

  case 1001:
#line 3861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13047 "Parser/parser.cc"
    break;

  case 1002:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13053 "Parser/parser.cc"
    break;

  case 1004:
#line 3870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13059 "Parser/parser.cc"
    break;

  case 1005:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13065 "Parser/parser.cc"
    break;

  case 1007:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13071 "Parser/parser.cc"
    break;

  case 1008:
#line 3877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13077 "Parser/parser.cc"
    break;

  case 1010:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13083 "Parser/parser.cc"
    break;

  case 1011:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13089 "Parser/parser.cc"
    break;

  case 1012:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13095 "Parser/parser.cc"
    break;

  case 1013:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13101 "Parser/parser.cc"
    break;

  case 1014:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13107 "Parser/parser.cc"
    break;

  case 1015:
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13113 "Parser/parser.cc"
    break;

  case 1016:
#line 3930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13119 "Parser/parser.cc"
    break;

  case 1019:
#line 3937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13125 "Parser/parser.cc"
    break;

  case 1020:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13131 "Parser/parser.cc"
    break;

  case 1021:
#line 3941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13137 "Parser/parser.cc"
    break;

  case 1022:
#line 3946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13143 "Parser/parser.cc"
    break;

  case 1023:
#line 3948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13149 "Parser/parser.cc"
    break;

  case 1024:
#line 3950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13155 "Parser/parser.cc"
    break;

  case 1025:
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13161 "Parser/parser.cc"
    break;

  case 1026:
#line 3954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13167 "Parser/parser.cc"
    break;

  case 1028:
#line 3960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13173 "Parser/parser.cc"
    break;

  case 1029:
#line 3962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13179 "Parser/parser.cc"
    break;

  case 1030:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13185 "Parser/parser.cc"
    break;

  case 1031:
#line 3969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 13191 "Parser/parser.cc"
    break;

  case 1032:
#line 3971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13197 "Parser/parser.cc"
    break;

  case 1033:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13203 "Parser/parser.cc"
    break;

  case 1035:
#line 3980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13209 "Parser/parser.cc"
    break;

  case 1037:
#line 3991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13215 "Parser/parser.cc"
    break;

  case 1038:
#line 3994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13221 "Parser/parser.cc"
    break;

  case 1039:
#line 3996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13227 "Parser/parser.cc"
    break;

  case 1040:
#line 3999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13233 "Parser/parser.cc"
    break;

  case 1041:
#line 4001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13239 "Parser/parser.cc"
    break;

  case 1042:
#line 4003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13245 "Parser/parser.cc"
    break;

  case 1044:
#line 4018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13251 "Parser/parser.cc"
    break;

  case 1045:
#line 4020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13257 "Parser/parser.cc"
    break;

  case 1046:
#line 4025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13263 "Parser/parser.cc"
    break;

  case 1047:
#line 4027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13269 "Parser/parser.cc"
    break;

  case 1048:
#line 4029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13275 "Parser/parser.cc"
    break;

  case 1049:
#line 4031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13281 "Parser/parser.cc"
    break;

  case 1050:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13287 "Parser/parser.cc"
    break;

  case 1052:
#line 4039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13293 "Parser/parser.cc"
    break;

  case 1053:
#line 4041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13299 "Parser/parser.cc"
    break;

  case 1054:
#line 4043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13305 "Parser/parser.cc"
    break;

  case 1055:
#line 4048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13311 "Parser/parser.cc"
    break;

  case 1056:
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13317 "Parser/parser.cc"
    break;

  case 1059:
#line 4060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13323 "Parser/parser.cc"
    break;

  case 1062:
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13329 "Parser/parser.cc"
    break;

  case 1063:
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13335 "Parser/parser.cc"
    break;

  case 1064:
#line 4075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13341 "Parser/parser.cc"
    break;

  case 1065:
#line 4077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13347 "Parser/parser.cc"
    break;

  case 1066:
#line 4079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13353 "Parser/parser.cc"
    break;

  case 1067:
#line 4081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13359 "Parser/parser.cc"
    break;

  case 1068:
#line 4088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13365 "Parser/parser.cc"
    break;

  case 1069:
#line 4090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13371 "Parser/parser.cc"
    break;

  case 1070:
#line 4092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13377 "Parser/parser.cc"
    break;

  case 1071:
#line 4094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13383 "Parser/parser.cc"
    break;

  case 1072:
#line 4096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13389 "Parser/parser.cc"
    break;

  case 1073:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13395 "Parser/parser.cc"
    break;

  case 1074:
#line 4101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13401 "Parser/parser.cc"
    break;

  case 1075:
#line 4103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13407 "Parser/parser.cc"
    break;

  case 1076:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13413 "Parser/parser.cc"
    break;

  case 1077:
#line 4107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13419 "Parser/parser.cc"
    break;

  case 1078:
#line 4112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13425 "Parser/parser.cc"
    break;

  case 1079:
#line 4114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13431 "Parser/parser.cc"
    break;

  case 1080:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13437 "Parser/parser.cc"
    break;

  case 1081:
#line 4121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 13443 "Parser/parser.cc"
    break;

  case 1083:
#line 4148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13449 "Parser/parser.cc"
    break;

  case 1087:
#line 4159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13455 "Parser/parser.cc"
    break;

  case 1088:
#line 4161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13461 "Parser/parser.cc"
    break;

  case 1089:
#line 4163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13467 "Parser/parser.cc"
    break;

  case 1090:
#line 4165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13473 "Parser/parser.cc"
    break;

  case 1091:
#line 4167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13479 "Parser/parser.cc"
    break;

  case 1092:
#line 4169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13485 "Parser/parser.cc"
    break;

  case 1093:
#line 4176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13491 "Parser/parser.cc"
    break;

  case 1094:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13497 "Parser/parser.cc"
    break;

  case 1095:
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13503 "Parser/parser.cc"
    break;

  case 1096:
#line 4182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13509 "Parser/parser.cc"
    break;

  case 1097:
#line 4184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13515 "Parser/parser.cc"
    break;

  case 1098:
#line 4186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13521 "Parser/parser.cc"
    break;

  case 1099:
#line 4191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13527 "Parser/parser.cc"
    break;

  case 1100:
#line 4193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13533 "Parser/parser.cc"
    break;

  case 1101:
#line 4195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13539 "Parser/parser.cc"
    break;

  case 1102:
#line 4202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13545 "Parser/parser.cc"
    break;

  case 1103:
#line 4204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13551 "Parser/parser.cc"
    break;

  case 1106:
#line 4228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13557 "Parser/parser.cc"
    break;

  case 1107:
#line 4230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13563 "Parser/parser.cc"
    break;


#line 13567 "Parser/parser.cc"

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
#line 4233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
