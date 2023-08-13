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
#define YYLAST   24303

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  178
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  309
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1104
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2182

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
    1699,  1704,  1706,  1708,  1713,  1718,  1720,  1722,  1727,  1729,
    1735,  1736,  1740,  1741,  1742,  1743,  1747,  1752,  1753,  1755,
    1757,  1759,  1763,  1767,  1768,  1772,  1774,  1776,  1778,  1780,
    1786,  1787,  1793,  1794,  1798,  1799,  1804,  1806,  1815,  1816,
    1818,  1823,  1828,  1839,  1840,  1844,  1845,  1851,  1852,  1856,
    1858,  1862,  1864,  1868,  1869,  1873,  1874,  1878,  1885,  1886,
    1890,  1892,  1907,  1908,  1909,  1910,  1912,  1916,  1918,  1922,
    1929,  1931,  1933,  1938,  1939,  1941,  1943,  1945,  1977,  1980,
    1985,  1987,  1993,  1998,  2003,  2014,  2021,  2026,  2028,  2030,
    2036,  2040,  2047,  2049,  2050,  2051,  2067,  2069,  2072,  2074,
    2077,  2082,  2083,  2087,  2088,  2089,  2090,  2100,  2101,  2102,
    2111,  2112,  2113,  2117,  2118,  2119,  2128,  2129,  2130,  2135,
    2136,  2145,  2146,  2151,  2152,  2156,  2158,  2160,  2162,  2164,
    2169,  2174,  2175,  2177,  2187,  2188,  2193,  2195,  2197,  2199,
    2201,  2203,  2206,  2208,  2210,  2215,  2217,  2219,  2221,  2223,
    2225,  2227,  2229,  2231,  2233,  2235,  2237,  2239,  2241,  2243,
    2245,  2247,  2249,  2251,  2253,  2255,  2257,  2259,  2261,  2263,
    2265,  2267,  2269,  2274,  2275,  2279,  2286,  2287,  2293,  2294,
    2296,  2298,  2300,  2305,  2307,  2312,  2313,  2315,  2317,  2322,
    2324,  2326,  2328,  2330,  2332,  2337,  2344,  2346,  2348,  2353,
    2361,  2360,  2364,  2372,  2373,  2375,  2377,  2382,  2383,  2385,
    2390,  2391,  2393,  2395,  2400,  2401,  2403,  2408,  2410,  2412,
    2414,  2415,  2417,  2422,  2424,  2426,  2431,  2438,  2442,  2443,
    2448,  2447,  2452,  2451,  2461,  2460,  2471,  2470,  2480,  2485,
    2486,  2491,  2497,  2515,  2516,  2520,  2522,  2524,  2530,  2532,
    2534,  2536,  2541,  2543,  2548,  2550,  2559,  2560,  2565,  2574,
    2579,  2581,  2583,  2592,  2594,  2595,  2596,  2598,  2600,  2601,
    2606,  2607,  2608,  2613,  2615,  2618,  2621,  2628,  2629,  2630,
    2636,  2641,  2643,  2649,  2650,  2656,  2657,  2661,  2666,  2668,
    2671,  2670,  2674,  2676,  2683,  2685,  2689,  2692,  2691,  2702,
    2706,  2710,  2714,  2719,  2720,  2725,  2730,  2738,  2740,  2742,
    2744,  2749,  2750,  2756,  2757,  2758,  2765,  2766,  2768,  2769,
    2770,  2772,  2774,  2781,  2782,  2784,  2786,  2791,  2792,  2798,
    2799,  2801,  2802,  2807,  2808,  2809,  2811,  2819,  2820,  2822,
    2825,  2827,  2831,  2832,  2833,  2835,  2837,  2842,  2844,  2849,
    2851,  2860,  2862,  2867,  2868,  2869,  2873,  2874,  2875,  2880,
    2881,  2886,  2887,  2888,  2889,  2893,  2894,  2899,  2900,  2901,
    2902,  2903,  2917,  2918,  2923,  2924,  2930,  2932,  2935,  2937,
    2939,  2962,  2963,  2969,  2970,  2976,  2975,  2985,  2984,  2988,
    2994,  3000,  3001,  3003,  3007,  3012,  3014,  3016,  3018,  3024,
    3025,  3029,  3030,  3035,  3037,  3044,  3046,  3047,  3049,  3054,
    3056,  3058,  3063,  3065,  3070,  3075,  3083,  3088,  3090,  3095,
    3100,  3101,  3106,  3107,  3111,  3112,  3113,  3118,  3120,  3126,
    3128,  3133,  3135,  3141,  3142,  3146,  3150,  3154,  3156,  3169,
    3171,  3173,  3175,  3177,  3179,  3181,  3182,  3187,  3190,  3189,
    3201,  3200,  3213,  3212,  3226,  3225,  3239,  3238,  3254,  3260,
    3262,  3268,  3269,  3280,  3287,  3292,  3298,  3301,  3304,  3308,
    3314,  3317,  3320,  3325,  3326,  3327,  3328,  3332,  3338,  3339,
    3349,  3350,  3354,  3355,  3360,  3365,  3366,  3372,  3373,  3375,
    3380,  3381,  3382,  3383,  3384,  3386,  3421,  3423,  3428,  3430,
    3431,  3433,  3438,  3440,  3442,  3444,  3449,  3451,  3453,  3455,
    3457,  3459,  3461,  3466,  3468,  3470,  3472,  3481,  3483,  3484,
    3489,  3491,  3493,  3495,  3497,  3502,  3504,  3506,  3508,  3513,
    3515,  3517,  3519,  3521,  3523,  3535,  3536,  3537,  3541,  3543,
    3545,  3547,  3549,  3554,  3556,  3558,  3560,  3565,  3567,  3569,
    3571,  3573,  3575,  3587,  3592,  3597,  3599,  3600,  3602,  3607,
    3609,  3611,  3613,  3618,  3620,  3622,  3624,  3626,  3628,  3630,
    3635,  3637,  3639,  3641,  3650,  3652,  3653,  3658,  3660,  3662,
    3664,  3666,  3671,  3673,  3675,  3677,  3682,  3684,  3686,  3688,
    3690,  3692,  3702,  3704,  3706,  3707,  3709,  3714,  3716,  3718,
    3723,  3725,  3727,  3729,  3734,  3736,  3738,  3752,  3754,  3756,
    3757,  3759,  3764,  3766,  3771,  3773,  3775,  3780,  3782,  3787,
    3789,  3806,  3807,  3809,  3814,  3816,  3818,  3820,  3822,  3827,
    3828,  3830,  3832,  3837,  3839,  3841,  3847,  3849,  3852,  3855,
    3857,  3861,  3863,  3865,  3866,  3868,  3870,  3874,  3876,  3881,
    3883,  3885,  3887,  3922,  3923,  3927,  3928,  3930,  3932,  3937,
    3939,  3941,  3943,  3945,  3950,  3951,  3953,  3955,  3960,  3962,
    3964,  3970,  3971,  3973,  3982,  3985,  3987,  3990,  3992,  3994,
    4008,  4009,  4011,  4016,  4018,  4020,  4022,  4024,  4029,  4030,
    4032,  4034,  4039,  4041,  4049,  4050,  4051,  4056,  4057,  4062,
    4064,  4066,  4068,  4070,  4072,  4079,  4081,  4083,  4085,  4087,
    4090,  4092,  4094,  4096,  4098,  4103,  4105,  4107,  4112,  4138,
    4139,  4141,  4145,  4146,  4150,  4152,  4154,  4156,  4158,  4160,
    4167,  4169,  4171,  4173,  4175,  4177,  4182,  4184,  4186,  4193,
    4195,  4213,  4215,  4220,  4221
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

#define YYPACT_NINF (-1931)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1103)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     133, 12300,   153,   178, 18004,    90, -1931, -1931, -1931, -1931,
   -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931,   -50,   834,
     121, -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931,
   -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931,
   -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931,   135,   292,
   -1931, -1931, -1931, -1931, -1931, -1931,  5033,  5033,   228, 12300,
     321,   375, 23981, -1931,   482, -1931, -1931, -1931, -1931, -1931,
   -1931, -1931, -1931, -1931, -1931,  3110, -1931,   817,   350, -1931,
   -1931, -1931, -1931, -1931, 17852, -1931, -1931,   368,   507,   272,
     503, -1931,  5033,   507,   516,   532,   598,  5543,   707,   898,
   12462, -1931, -1931,   715, 17700,  1246, -1931, -1931, -1931,  2662,
     789,  9970,  9644,  1081,  2662,  1111,   645, -1931, -1931, -1931,
   -1931,   737, -1931, -1931, -1931, -1931,   671, -1931, -1931, -1931,
   -1931, -1931,   685,   694,   737, -1931,   737,  8977, -1931, -1931,
   -1931, 18999,  5033, -1931, -1931,  5033, -1931, 12300, -1931,   681,
   19151, -1931, -1931,  5671, 20572, -1931, -1931,  1027,  1027,   719,
    3087, -1931, -1931, -1931, -1931,   514, 14857,  4236,   737, -1931,
   -1931, -1931, -1931, -1931, -1931,   741, -1931,   784,   831,   836,
   -1931,   749, 23372, -1931, -1931, -1931, -1931, -1931, -1931, -1931,
   16558,  4161,  3110,   814,   845,   864,   874,   881,   893,   901,
   -1931, -1931, 19303, 11311,   934, -1931, 18301, -1931, -1931, -1931,
   -1931,   948, -1931, -1931,   957, -1931,  9371,  1084, 21596, -1931,
     979,  5033,   694,   989,   993,  5671,  2328, -1931, -1931, -1931,
    3228,  3350,  1008,  1079,    42,  1079, -1931,   737,   737,    11,
   16036,   454,  1079, -1931,   737,   737,    11,   737, -1931,   737,
   -1931,  3629, -1931, -1931,  1035,  1087,  1027, 20768, -1931, 17852,
   -1931, -1931,  2662, -1931,  1378,   645,  1088,  1213, 16036,  5033,
    5033,   272, -1931, 14052, -1931,  1027,  1027,  1100,  1213, 16036,
    5033, -1931, 24180, -1931, -1931, -1931,  1027, -1931, -1931, -1931,
   -1931,  1027, -1931,  1016,  3998,  5033, -1931, 17557,  1049, -1931,
   -1931, -1931, 21167,   694, 16141,  1142,  5671, 17452, 20768,  2662,
   -1931, -1931, 20717, -1931,  1079,    37, -1931, 23372, 20572,  4385,
    3629, -1931,   467, -1931, -1931, -1931, -1931, -1931, 19151,  5033,
   -1931,  1105, -1931, -1931, -1931, -1931,  5033,  4417,   573,    78,
   -1931,  5033,   784, -1931,   778,   737,   737,  1169, 19455,   846,
   15340, 21270,  2662, -1931,  2662,  1027,  2662,  1027, -1931, -1931,
     737, -1931,  1182, -1931, 19607, -1931, -1931, -1931, 19759,   948,
   -1931,   476,   711,   363,  1179,   661,   645,  1186, -1931,  3087,
    1180,   784,  3087,  1654, -1931,  1206,  1244, 23446,  1225,  1240,
    1253, 23372, 23520,  1260, 24085, -1931, -1931, -1931, -1931, -1931,
   -1931, 23594, 23594, 16402,  1207,  4670, -1931, -1931, -1931, -1931,
     423, -1931,   484, -1931,  1012, -1931, 23372, 23372, -1931,  1262,
     626,  1063,  1167,   755,  1174,  1258,  1271,  1273,  1313,   244,
   -1931,   596, -1931,  1306, -1931,  1152,  3091, 16870, -1931, -1931,
     774,  1306, -1931, -1931,   859, -1931, -1931,  4161,  1300,  1310,
    1322,  1335,  1343,  1352, -1931, -1931,   495,  1356, -1931,   456,
    1356, -1931, -1931, 18999, -1931,  1160,  1334, 17026, -1931, -1931,
    3338,  5087,  1382, 15340,  1388,   584,   683, -1931, -1931, -1931,
   -1931, -1931,  5033,  5118, -1931, -1931, -1931, -1931, -1931, -1931,
   17347,  4971,  1207,  9371,  1367,  1375, -1931, -1931,  1381, 21596,
     826, -1931, -1931, -1931, 21670,  1394, -1931, -1931, -1931, -1931,
    1380,  3228,   835,  1397,  1405,  1407,   841,  1414,  1416,  1418,
    1420,  1422,  1423,  3350, -1931, -1931, -1931,   737,  1426,  1415,
    1433, -1931, -1931,  1436,   272, -1931, -1931,   694,  1213, 18165,
   -1931, -1931,   272, -1931, -1931,   694, -1931, -1931,  3629, -1931,
   16870, 16870, -1931,  1027,  5671, 21358, 15501, -1931, -1931, -1931,
   -1931, -1931,   694,  1213,    37,  1434, -1931, -1931,  2662,  1439,
    1213, 16036, -1931,   694,  1213, -1931, 24231, -1931,  1027,  1027,
   -1931, -1931,  1440,   398,  1441,   645,  1442, -1931, -1931, -1931,
   18596,  1443,  1438, -1931, -1931,   866, -1931,  1533, -1931,  1429,
   -1931, -1931, -1931, 19920, 23668, -1931, -1931, -1931, -1931, -1931,
    4385,   964,  3629, 18165, 15501,  1079, 12300, -1931,  1450, -1931,
    1456, -1931, -1931, -1931, -1931, -1931,  3087, -1931, -1931,  1535,
    4815,  2785, 19759, 11311, -1931, 19971, -1931,  1027,  1027, -1931,
   -1931,   948, -1931, 14374,  1452,  1596, 23372,   902,  1436,  1444,
   -1931,   737,   737, -1931,  1356, -1931, 19455, -1931, -1931, 18596,
    1027,  1027, -1931,  4815,   737, -1931, 20427, -1931, -1931, 19607,
   -1931,   514, -1931, -1931, -1931,  1457,  5033,   363,  1186,  1458,
     867, 19151,   875, -1931, -1931, -1931, -1931, -1931, -1931,   895,
   -1931,  1464,  1445, -1931, 16714, -1931,  4670, 20123, 20123, -1931,
   16714, -1931, 23372, -1931, -1931, -1931, -1931, -1931, -1931, 16714,
   -1931, -1931, 18695, 20123, 20123,  1152,  1055,  1229,   677,  1402,
   -1931,   921,  1467,  1161,  1473, -1931, 21670, 23372, 21744,  1474,
   23372,  2328, 23372,  2328, -1931,  2447, -1931, -1931, 21818,  2645,
   23372, 21818,  2328, -1931, -1931, 23372, 23372, 23372, 23372, 23372,
   23372, 23372, 23372, 23372, 23372, 23372, 23372, 23372, 23372, 23372,
   23372, 23372, 23372, 23372, 21892,  1459,   749,  4605, 11311, -1931,
   -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931,
    1475, 23372, -1931, -1931, 14535,  1569, -1931, -1931,   737,   737,
   -1931, -1931, 16870, -1931,   496,  1356, -1931,   900,  1356, 18165,
   -1931, -1931,  1436, 18165, -1931,  1436, 23742, -1931, -1931, 11311,
    1476,  1482, 13891,  1628,  3143,   533,  1444, -1931,   737,   737,
    1444,   612, -1931,   737,   737, 23372,  5033,  1171,  1176,  1444,
     319, 14696, 14696,  5033, -1931, -1931, 23372,  1381, -1931,  9371,
    1493, -1931,  2506, -1931, -1931, -1931, -1931, -1931,   924, -1931,
   14696,  2328, 23372,   931,  1494,  1496,  1498,   937,  1500,  1501,
    1505,  1506,  1507,  1512,   615,  1356, -1931, -1931,   621,  1356,
   -1931, -1931,   642,  1356, -1931, -1931, -1931,  5671,   749,  1657,
    1356, 20862, -1931, -1931,   694,  1526, -1931, -1931, -1931,   955,
    1527,   976,  1529, -1931,  1537, -1931,   694, -1931,  1541, -1931,
     694,  1213,  1537, -1931,   694,  1534,  1538,  1540, -1931, -1931,
   18462, -1931,  2328,  5033, 10455,  1625, -1931,  1334, -1931, 14696,
     986, -1931, -1931,  1537, -1931, 19151, 16870,  1528, -1931,  1528,
   -1931, -1931, -1931,   363,  1542,   737,   737, -1931, 19607, -1931,
   11476, 17182, -1931,  1549,  1554,  1556,  1558, -1931,  8581,   737,
   -1931,   902, -1931, -1931, -1931, -1931,  1436, -1931, -1931, -1931,
    1027, -1931,  3617, -1931, -1931,   645,   249,  1567,  1543,  1457,
    1560,   363, -1931, -1931,  1564,  1573,  1654, 21818, -1931,  1574,
    1571,   350,  1572,  1577,  1582,  1580,  1587, 23372,  1589,  1591,
    1593, 11311, 23372, -1931, -1931,  1778, -1931, -1931, -1931, 23372,
   -1931,  1594,  1595, 21522,  1194, -1931, 21818,  1597, -1931,  1599,
   -1931, -1931,  2580, -1931, -1931,  1000, -1931, -1931, -1931, -1931,
    2580, -1931, -1931,  1201,   575, -1931, -1931,  1262,  1262,  1262,
     626,   626,  1063,  1063,  1167,  1167,  1167,  1167,   755,   755,
    1174,  1258,  1271,  1273,  1313, 23372,  1209, -1931,  1598,  2580,
   -1931, -1931,  9371, -1931,  1605,  1616,  1618,  1619,  1569, -1931,
   -1931, -1931, -1931, -1931, 18165, -1931, -1931,  1436, 18165, -1931,
    1436,  1620,  1621, -1931, -1931, 13891,  1005,  1622,  1624,  1630,
    1631,  2874,  3143, -1931, -1931, 18165, -1931, -1931, -1931, -1931,
   -1931, -1931, 18165, -1931, -1931, -1931, -1931,  1623, -1931,  1444,
   -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931,  1632,  1633,
   -1931,   272,  2580,  1218,    72, -1931, -1931,  1602, -1931, 21596,
   -1931, 23372,   737, 21966, 14696, -1931, -1931, -1931,   647,  1356,
   -1931,   688,  1356, -1931, -1931,   693,  1356, 18165, -1931, -1931,
    1436, 18165, -1931, -1931,  1436, 18165, -1931, -1931,  1436,  1079,
    1635, -1931,  1436,   365, -1931,  1306,  1640, -1931, -1931, -1931,
   -1931, -1931, -1931,  1648, -1931, -1931, -1931, 19151,  1537, -1931,
     694, -1931, -1931, -1931, -1931, -1931, 13103, -1931, -1931, -1931,
   -1931,   285, -1931,   606,   392, 11146,  1649, 15861,  1650,  1651,
    1276,  2366,  3697, 22040,  1653, -1931, -1931,  1655,  1656, -1931,
   -1931,   694, 23372, 23372,  1792,  1652,   729, -1931, 16246,  1736,
    1658,  1637, -1931, -1931, -1931, 10280, -1931, -1931, -1931, -1931,
   -1931,  2773, -1931, -1931, -1931,  1312,   192, -1931,   343, -1931,
     192, -1931, -1931, -1931,  2328, -1931, -1931, 12624, 17852,  1661,
   -1931,  5033,  1660,  1664, -1931,  1250, -1931, -1931, -1931, -1931,
    5671, -1931, -1931,  1638,  1647,  1021, 19151,   784,   784,  1457,
     363,  1186,  1186, -1931, -1931,  1207,  1334, 17026, -1931,  1306,
   -1931, 11641, -1931,   700,  1356, -1931,  1027,  9233, -1931, -1931,
     363,  1665,   737,   737,   514,  5033, -1931, 22114, -1931,  1675,
     363,  1457,  1677, -1931, -1931,  1022,   782, 18596, 11311,  2328,
   -1931,   782, 18847,   782, -1931, 23372, 23372, 23372, -1931, -1931,
   -1931, -1931, 23372, 23372,  1669,  9371, -1931, -1931,  1672,   805,
   -1931, -1931, -1931,  2185, -1931, -1931,  1272, -1931,   -12, -1931,
   21818,  1294, -1931, 21670, -1931, -1931, 23372,  1659,  1299,  1301,
    1381, -1931,   710,  1356, -1931, -1931,  1678,  1679, -1931, -1931,
    1681,   716,  1356, -1931,   730,  1561,   737,   737, -1931, -1931,
    1685,  1686, -1931,  1674, -1931, 15501, 15501,  1687,  1688,  1689,
    1690, -1931,  1693, 23372, 23372,  1316,  1691, -1931, -1931, -1931,
   -1931, -1931, -1931, -1931,  1701, 18165, -1931, -1931,  1436, 18165,
   -1931, -1931,  1436, 18165, -1931, -1931,  1436,  1707,  1709,  1710,
     272,   737, -1931, -1931,  1321, 23372, 21011,  1705,  1694, -1931,
   -1931, -1931,  1718, 13258, 13413, 13568, 19151, 20768, 20123, 20123,
    1719, -1931,  1695,   414,  1168,  6639, -1931,   449,  5033,  5033,
   -1931, 21818,   229,   355, -1931, -1931, -1931, -1931, 23372,  1721,
    1732, 10980, 10630, -1931,  1699, -1931,  1712, 23372,  1713,  9371,
    1716, 23372, 21670, 23372,  1230, -1931,  1720,    22, -1931,     6,
    1795,   295,  1744, -1931, -1931,  1747, -1931,  1725, -1931,  1726,
    1752,  1755, 15861, 15861, -1931, -1931,  1780, -1931, -1931,    50,
      50,   878, 14213,   737,   473, -1931, -1931,  1754, -1931,  1759,
   -1931,  1763, -1931,  1758, -1931,  1760, -1931, -1931, -1931, -1931,
    1769,  1457,  1762,  1770, 11806,  1766,  1777,  1779, -1931, 18165,
   -1931, -1931,  1436, 23372, 23372,  1334,  1781, -1931,  1457,   363,
   -1931,  1186,     7,  1543,  9371, -1931, -1931,  1457,  1784, -1931,
   19151, -1931,   788,  1785,  1783,  1034, -1931,  1786, -1931, -1931,
   -1931, -1931, -1931,  9371,  1381, 21670, -1931,  1819,  2580, -1931,
    1819,  1819, -1931,  2580,  4917,  5383, -1931, -1931,  1327, -1931,
   -1931, -1931,  1796, 18165, -1931, -1931,  1436, -1931, -1931,   737,
   18165, -1931, -1931,  1436, 18165, -1931, -1931,  1789, -1931, -1931,
   -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931, -1931,
   -1931,  1793, -1931, -1931, -1931, -1931,  1797,  1799,   737,  1801,
    1802,  1805, -1931, -1931, -1931, -1931, -1931, 23372, -1931,   365,
   -1931,  1306, -1931, -1931,  1798,  1810, -1931,  1719,  1719,  1719,
    5194,   840,  1787,   528, -1931,  5194,   566, 16870, -1931, -1931,
   -1931,  4532, 23372,  4135,   257, -1931, -1931,    17,  1804,  1804,
    1804,  5033, -1931, -1931, -1931,  1040, -1931, -1931, -1931, -1931,
    1041,  1814, 15861,  1658,  1813, 23372,   368,  1816,   598, 13730,
   19151, -1931, -1931, -1931,   997, 15861, 23372,  1139,   664, -1931,
   23372, 21368, -1931, -1931,   593, -1931,  1381, -1931,  1048,  1059,
    1064, -1931, -1931, -1931, -1931,   694,  1230,  1820, -1931, -1931,
   23372, -1931,  1823,   749, -1931, 11146, -1931, -1931, -1931, -1931,
   23372, 23372, -1931, -1931,   569,    50, -1931,   543, -1931, -1931,
   10095, -1931,   737, 15501, -1931, -1931, 19151, -1931, -1931, -1931,
    1817,   363,   363, -1931, -1931, -1931,  1824,  1828, -1931, -1931,
    1826, -1931,  1827,  1834,  1457,  1186,  1811, -1931, -1931,  1381,
    1837, -1931, -1931,  1842, -1931, -1931, 23372, -1931, 18847, 23372,
    1381,  1839,  1337, -1931,  1342, -1931,  2580, -1931,  2580, -1931,
   -1931, -1931,  1843,  1845,  1846,  1345, 15018, 15179, -1931,  1850,
   -1931, -1931, -1931, -1931, -1931,  1347, 23372, -1931, -1931, -1931,
   -1931, -1931,   599,   840,  2197,   601, -1931, -1931, -1931, -1931,
     737,   737, -1931, -1931, -1931,   633, -1931,  1067,  4532,   947,
   -1931,  4135, -1931,   737, -1931, -1931, -1931, -1931, -1931, -1931,
   15861,   102, 22188,  1927, 15861,  1658, 15662, -1931, -1931, -1931,
   -1931, 23372, -1931, 22262,  1935,  1835, 21445, 22336, 15861, 10805,
    1658,   753,  1319,  1838, 23372, -1931,  1862,   320, 15861, -1931,
   -1931,  1864, -1931, -1931,  1840,   749,   665,  1865,  1874,  1311,
    1072, 15861,  1866, 15861, 15861, 15861, -1931, -1931, -1931, -1931,
    5033,  5671, -1931,  1457,  1457, -1931, -1931,  1875,  1876, -1931,
   -1931, -1931,  1879,  1871,   363,  1883, -1931,  1884, -1931, -1931,
   -1931, -1931,  1885, -1931, -1931, -1931,  1351,  1353, -1931, -1931,
   -1931, -1931, -1931, -1931,  1886, -1931, -1931, -1931, -1931, -1931,
    1887,  1888,  1889,  2197, -1931,   737, -1931, -1931, -1931, -1931,
   -1931,  1882,  5194, -1931,  5452,    69, 11974, -1931, 15756, -1931,
      26,  1077, 15861,  1970,   637,  1878,   279, 15861, 23372,  1892,
     753,  1319,  1872, 23816,  1891,   351,  1975, -1931, 22410, 22484,
   23372,  1658,  1880, 12138, -1931, -1931, -1931, 20275, -1931,  1893,
    1890,   129, 15861, -1931, 23372, 21818, -1931, -1931, 23372,   192,
   -1931, -1931, -1931, -1931,  1900,  1902, -1931, -1931, -1931,   363,
    1457, -1931, -1931, -1931, -1931, -1931, 15501,  1897,   733,  1356,
   -1931, -1931,   840, -1931, -1931,   296, -1931,   106, -1931, -1931,
   -1931,  1910, 12786, -1931, -1931, 15861, -1931,    41, -1931, 15861,
   23372,  1913, 22558, -1931, -1931, 22632, 22706, 23372,  1892,  1658,
   22780, 22854, 15861,  1903,   446,  1906,   474, -1931, -1931,  1919,
   12786, 20275, -1931,  5291, 19971,  2328,  1921, -1931,  1979,  1932,
     692,  1933, -1931,  2015, -1931,  1080,  1115,   393, -1931, -1931,
    1457,  1942, -1931, 18165, -1931, -1931,  1436, -1931, 23372, -1931,
   23372, -1931, -1931,  1437, 12948, -1931, -1931, 15861, -1931, -1931,
    1658, -1931, -1931,  1658,  1929,   555,  1931,   595, -1931, -1931,
    1658, -1931,  1658, -1931,  1948, 22928, 23002, 23076, -1931,  1437,
   -1931,  1928,  3725,  3463, -1931, -1931, -1931,   129,  1952, 23372,
    1936,   129,   129, 15861, -1931, -1931, 15861,  2037,  1963, -1931,
    1962, -1931, -1931, 15756, -1931,  1437, -1931, -1931,  1965, 23150,
   23224, 23298, -1931, -1931,  1658, -1931,  1658, -1931,  1658, -1931,
    1928, 23372,  1966,  3463,  1964,   749,  1971, -1931,   800, -1931,
   -1931, -1931, 15861, -1931, -1931, -1931,  9800,  1978, 15756, -1931,
   -1931,  1658, -1931,  1658, -1931,  1658,  1982,  1972, -1931,   694,
     749,  1990, -1931,  1967,   749, -1931, -1931, -1931, -1931,  9973,
   -1931,   694, -1931, -1931,  1362, 23372, -1931,  1138, -1931,   749,
    2328,  1991,  1973, -1931, -1931,  1145, -1931, -1931,  1974,  2328,
   -1931, -1931
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   479,     0,     2,   479,   496,   497,   498,   499,   500,
     501,   502,   503,   504,   485,   487,   486,   488,     0,     0,
       0,   505,   507,   528,   508,   529,   511,   512,   526,   527,
     506,   524,   525,   509,   510,   513,   514,   515,   516,   517,
     518,   519,   520,   521,   522,   523,   530,   531,   840,   533,
     606,   607,   610,   612,   608,   614,     0,     0,     0,   479,
       0,     0,    17,   577,   583,     9,    10,    11,    12,    13,
      14,    15,    16,   797,   103,     0,    20,     0,     2,   101,
     102,    18,    19,   856,   479,   798,   419,     0,   422,   721,
     424,   433,     0,   423,   453,   454,     0,     0,     0,     0,
     560,   481,   483,   489,   479,   491,   494,   545,   532,   463,
     538,   543,   465,   555,   464,   570,   574,   580,   559,   586,
     598,   840,   603,   604,   587,   662,   425,   426,     3,   805,
     818,   484,     0,     0,   840,   878,   840,   479,   895,   896,
     897,   479,     0,  1082,  1083,     0,     1,   479,    17,     0,
     479,   442,   443,     0,   560,   489,   473,   474,   475,   808,
       0,   609,   611,   613,   615,     0,   479,     0,   841,   842,
     605,   534,   714,   715,   713,   774,   769,   759,     0,     0,
     806,     0,     0,   496,   799,   803,   804,   800,   801,   802,
     479,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     578,   581,   479,   479,     0,  1084,   560,   885,   903,  1088,
    1081,  1079,  1086,   418,     0,   165,   727,   164,     0,   427,
       0,     0,     0,     0,     0,     0,     0,   417,   972,   973,
       0,     0,   452,   838,   840,   838,   859,   840,   840,   462,
     479,   840,   838,   916,   840,   840,   461,   840,   935,   840,
     913,     0,   553,   554,     0,     0,   479,   479,     2,   479,
     434,   482,   492,   546,     0,   575,     0,   821,   479,     0,
       0,   721,   435,   560,   539,   556,   571,     0,   821,   479,
       0,   495,   540,   547,   548,   466,   557,   468,   469,   467,
     562,   572,   576,     0,   590,     0,   791,   479,     2,   819,
     877,   879,   479,     0,   479,     0,     0,   560,   479,   491,
       2,  1092,   560,  1095,   838,   838,     3,     0,   560,     0,
       0,   445,   840,   833,   835,   834,   836,     2,   479,     0,
     795,     0,   755,   757,   756,   758,     0,     0,   751,     0,
     741,     0,   750,   761,     0,   840,   840,     2,   479,  1103,
     480,   479,   470,   538,   471,   563,   472,   570,   567,   588,
     840,   589,     0,   702,   479,   703,  1057,  1058,   479,   704,
     706,   577,   583,   663,     0,   665,   666,   663,   843,     0,
     772,   760,     0,   847,    22,     0,    21,     0,     0,     0,
       0,     0,     0,     0,    24,    26,     4,     8,     5,     6,
       7,     0,     0,   479,     2,     0,   104,   105,   106,   107,
      88,    25,    89,    43,    87,   108,     0,     0,   123,   125,
     129,   132,   135,   140,   143,   145,   147,   149,   151,   153,
     156,     0,    27,     0,   584,     2,   108,   479,   157,   766,
     717,   574,   719,   765,     0,   716,   720,     0,     0,     0,
       0,     0,     0,     0,   857,   883,   840,   893,   901,   905,
     911,     2,  1090,   479,  1093,     2,   101,   479,     3,   701,
       0,  1103,     0,   480,   538,   563,   570,     3,     3,   683,
     687,   697,   703,   704,     2,   886,   904,  1080,     2,     2,
      24,     0,     2,   727,    25,     0,   725,   728,  1101,     0,
       0,   734,   723,   722,     0,     0,   823,     2,     2,   446,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   862,   919,   942,   840,     0,   458,
       2,   858,   866,  1000,   721,   860,   861,     0,   821,   479,
     915,   923,   721,   917,   918,     0,   934,   936,     0,   448,
     479,   479,   544,   480,     0,   560,   479,  1085,  1089,  1087,
     561,   795,     0,   821,   838,     0,   428,   436,   493,     0,
     821,   479,   795,     0,   821,   770,   541,   542,   558,   573,
     579,   582,   577,   583,   601,   602,     0,   771,   690,   711,
     480,     0,   691,   694,   693,     0,   203,   411,   820,     0,
     409,   462,   461,   560,     0,   430,     2,   431,   792,   450,
       0,     0,     0,   479,   479,   838,   479,   795,     0,     2,
       0,   754,   753,   752,   747,   490,     0,   745,   762,   536,
       0,     0,   479,   479,  1059,   480,   476,   477,   478,  1063,
    1054,  1055,  1061,   479,     2,   102,     0,  1019,  1033,  1103,
    1015,   840,   840,  1024,  1031,   709,   479,   568,   705,   480,
     564,   565,   569,     0,   840,  1069,   480,  1074,  1066,   479,
    1071,     0,   672,   664,   671,  1101,     0,   663,   663,     0,
       0,   479,     0,   855,   854,   850,   852,   853,   851,     0,
     845,   848,     0,    23,   479,    95,     0,   479,   479,    90,
     479,    97,     0,    33,    37,    38,    34,    35,    36,   479,
      93,    94,   479,   479,   479,     2,   104,   105,     0,     0,
     183,     0,     0,   604,     0,  1079,     0,     0,     0,     0,
       0,     0,     0,     0,    56,     0,    62,    63,    67,     0,
       0,    67,     0,    91,    92,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   479,   166,
     167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
     164,     0,   162,   163,   479,   984,   718,   981,   840,   840,
     989,   585,   479,   884,   840,   894,   902,   906,   912,   479,
     887,   889,   891,   479,   907,   909,     0,  1091,  1094,   479,
       0,     0,   479,   102,  1019,   840,  1103,   954,   840,   840,
    1103,   840,   969,   840,   840,     3,   705,     0,     0,  1103,
    1103,   479,   479,     0,     2,   736,     0,  1101,   733,  1102,
       0,   729,     0,     2,   732,   735,   180,   179,     0,     2,
     479,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   840,   871,   875,   914,   840,   928,
     932,   940,   840,   950,   863,   920,   943,     0,     0,     0,
     996,     0,   456,   824,     0,     0,   457,   825,   449,     0,
       0,     0,     0,   447,     2,   826,     0,   432,     2,   795,
       0,   821,     2,   827,     0,     0,     0,     0,   616,   880,
     479,   898,     0,     0,   479,   412,   410,   101,     3,   479,
       0,     3,   796,     2,   749,   479,   479,   743,   742,   743,
     537,   535,   665,   663,     0,   840,   840,  1065,   479,  1070,
     480,   479,  1056,     0,     0,     0,     0,  1034,     0,   840,
    1104,  1020,  1021,   710,  1017,  1018,  1032,  1060,  1064,  1062,
     566,   601,     0,  1068,  1073,   668,   663,     0,   673,  1101,
       0,   663,   775,   773,     0,     0,   847,    67,   807,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   479,     0,   122,   121,     0,   118,   117,    28,     0,
      29,     0,     0,     0,     0,     3,    67,     0,    52,     0,
      53,    60,     0,    59,    71,     0,    68,    69,    72,    55,
       0,    54,    58,     0,     0,    51,   124,   126,   127,   128,
     130,   131,   133,   134,   138,   139,   136,   137,   141,   142,
     144,   146,   148,   150,   152,     0,     0,   421,     0,     0,
      30,     3,   727,   158,     0,     0,     0,     0,   985,   986,
     982,   983,   768,   767,   479,   888,   890,   892,   479,   908,
     910,     0,     0,  1010,  1009,   479,     0,     0,     0,     0,
       0,   840,  1020,   957,   974,   479,   952,   960,   707,   955,
     956,   708,   479,   967,   977,   970,   971,     0,     3,  1103,
     440,     2,  1096,     2,   698,   699,   677,     3,     3,     3,
       3,   721,     0,   156,     0,     3,     3,     0,   730,     0,
     724,     0,   840,     0,   479,     3,   444,   451,   840,   872,
     876,   840,   929,   933,   941,   840,   951,   479,   864,   867,
     869,   479,   921,   924,   926,   479,   944,   946,   948,   838,
       0,   459,   997,     3,  1001,  1002,     3,   829,   937,   550,
     549,   552,   551,     2,   796,   830,   777,   479,     2,   828,
       0,   796,   831,   616,   616,   616,   479,   692,   696,   695,
     712,     0,   415,     0,     0,   479,     0,   338,     0,     0,
       0,     0,     0,   185,     0,   333,   334,     0,     0,   384,
     383,     0,   160,   160,   390,   577,   583,   200,   479,     0,
     186,     0,   211,   187,   188,   479,   205,   189,   190,   191,
     192,     0,   193,   194,   339,     0,   353,   195,   359,   361,
     364,   196,   197,   198,     0,   199,   207,   560,   479,     0,
     209,     0,     0,     0,     3,     0,   809,   796,   784,   785,
       0,     3,   780,     3,     3,     0,   479,   759,   759,  1101,
     663,   663,   663,  1067,  1072,     2,   101,   479,     3,   575,
       3,   480,  1028,   840,  1027,  1030,   479,     3,  1016,  1022,
     663,     0,   840,   840,     0,     0,   648,     0,   667,     0,
     663,  1101,     2,   844,   846,     0,    96,   479,   479,     0,
     100,    98,   479,     0,   112,     0,     0,     0,   116,   120,
     119,   184,     0,     0,     0,   727,   109,   177,     0,     0,
      46,    47,    85,     0,    85,    85,     0,    73,    75,    49,
       0,     0,    45,     0,    48,   155,     0,     0,     0,     0,
    1101,   993,   840,   992,   995,   987,     0,     0,   881,   899,
       0,   840,   963,   966,   840,     0,   840,   840,   958,   975,
       0,     0,  1097,     0,   700,   479,   479,     0,     0,     0,
       0,   429,     3,     0,     0,     0,     0,   726,   731,     3,
     822,   182,   181,     3,     0,   479,   865,   868,   870,   479,
     922,   925,   927,   479,   945,   947,   949,     0,     0,     0,
     721,   840,  1008,  1007,     0,     0,     0,     0,     0,     3,
     796,   832,     0,   479,   479,   479,   479,   479,   479,   479,
     599,   629,     3,     0,   630,   560,   617,     0,     0,     0,
     413,    67,     0,     0,   324,   325,   208,   210,     0,     0,
       0,   479,   479,   320,     0,   318,     0,     0,     0,   727,
       0,     0,     0,     0,     0,   161,     0,     0,   391,     0,
       0,     0,     0,     3,   215,     0,   206,     0,   315,     0,
       0,     0,   338,   338,   344,   343,   338,   355,   354,   338,
     338,     0,   560,   840,     0,  1012,  1011,     0,     2,     0,
     787,     2,   782,     0,   783,     0,   763,   744,   748,   746,
       0,  1101,     0,     0,   479,     0,     0,     0,     3,   479,
    1023,  1025,  1026,     0,     0,   101,     0,     3,  1101,   663,
     657,   663,   673,   673,   727,   674,   649,  1101,     0,   776,
     479,   849,  1013,     0,     0,     0,    39,     0,   113,   115,
     114,   111,   110,   727,  1101,     0,    66,    82,     0,    76,
      83,    84,    61,     0,     0,     0,    70,    57,     0,   154,
     420,    31,     0,   479,   988,   990,   991,   882,   900,   840,
     479,   959,   961,   962,   479,   976,   978,     0,   953,   968,
     964,   979,  1098,     3,   685,   684,   688,  1100,     2,     2,
    1099,     0,     3,   837,   737,   738,     0,     0,   840,     0,
       0,     0,   873,   930,   938,   460,   839,     0,  1003,     0,
    1004,  1005,   999,   813,     2,     0,   815,   599,   599,   599,
     630,   637,   604,     0,   643,   630,     0,   479,   591,   628,
     624,     0,     0,     0,     0,   631,   633,   840,   645,   645,
     645,     0,   625,   641,   416,     0,   328,   329,   326,   327,
       0,     0,   338,   225,     0,     0,   227,   424,   226,   560,
     479,   306,   305,   307,     0,   338,   185,   265,     0,   258,
       0,   185,   321,   319,     0,   313,  1101,   322,     0,     0,
       0,   372,   373,   374,   375,     0,   365,     0,   366,   330,
       0,   331,     0,     0,   358,   479,   216,   204,   317,   316,
       0,     0,   347,   357,     0,   338,   360,     0,   362,   382,
       0,   414,   840,   479,   811,   764,   479,     2,     2,   655,
       0,   663,   663,  1075,  1076,  1077,     0,     0,     3,     3,
       0,  1036,     0,     0,  1101,   663,     0,   670,   669,  1101,
       0,   652,     3,     0,  1014,    99,     0,    32,   479,     0,
    1101,     0,     0,    86,     0,    74,     0,    80,     0,    78,
      44,   159,     0,     0,     0,     0,   479,   479,   740,     0,
     437,   439,   874,   931,   939,     0,     0,   779,   817,   595,
     597,   593,     0,     0,  1043,     0,   638,  1048,   640,  1040,
     840,   840,   623,   644,   627,     0,   626,     0,     0,     0,
     647,     0,   619,   840,   618,   634,   646,   635,   636,   642,
     338,     0,     0,   246,   338,   228,   560,   311,   309,   312,
     308,     0,   310,     0,   254,     0,   185,     0,   338,   479,
     266,     0,   291,     0,     0,   314,     0,     0,   338,   337,
     376,     0,   367,     2,     0,     0,     0,     0,   340,     0,
       0,   338,     0,   338,   338,   338,   202,   201,   438,   781,
       0,     0,   656,  1101,  1101,  1078,  1029,     0,     0,  1035,
    1037,   653,     0,     0,   663,     0,   651,     2,    50,    42,
      40,    41,     0,    64,   178,    77,     0,     0,   994,   965,
     980,   441,     2,   682,     3,   681,   739,   998,  1006,   621,
       0,     0,     0,  1044,  1045,   840,   622,  1041,  1042,   620,
     600,     0,     0,   336,     0,     0,     0,   239,   338,   217,
       0,     0,   338,   248,   263,   274,   268,   338,   185,   303,
       0,   278,     0,     0,   269,   267,   256,   259,     0,     0,
     185,   292,     0,     0,   220,   335,     2,   479,   332,     0,
       0,   392,   338,   345,     0,    67,   356,   349,     0,   350,
     348,   363,   786,   788,     0,     0,  1038,  1039,   654,   663,
    1101,   675,   778,    65,    81,    79,   479,     0,   840,  1051,
    1053,  1046,     0,   632,   234,   229,   232,     0,   231,   238,
     237,     0,   479,   241,   240,   338,   250,     0,   247,   338,
       0,     0,     0,   255,   260,     0,     0,   185,   304,   279,
       0,     0,   338,     0,   294,   295,   293,   262,   323,     0,
     479,   479,     3,   377,   480,   381,     0,   385,     0,     0,
       0,   393,   394,   223,   341,     0,     0,     0,   659,   661,
    1101,     0,   686,   479,  1047,  1049,  1050,   639,     0,   236,
       0,   235,   219,   242,   479,   405,   251,   338,   252,   249,
     264,   277,   275,   271,   283,   281,   282,   280,   261,   276,
     272,   273,   270,   257,     0,     0,     0,     0,   222,   242,
       3,   370,     0,  1043,   378,   379,   380,   392,     0,     0,
       0,   392,     0,   338,   346,   342,   338,     0,     0,   660,
       0,   230,   233,   338,     3,   243,   406,   253,     0,     0,
       0,     0,   302,   300,   297,   301,   298,   299,   296,     3,
     370,     0,     0,  1044,     0,     0,     0,   386,     0,   395,
     224,   351,   338,   658,  1052,   212,     0,     0,   338,   290,
     288,   285,   289,   286,   287,   284,     0,     0,   371,     0,
     398,     0,   396,     0,   398,   352,   214,   213,   218,     0,
     221,     0,   368,   399,     0,     0,   387,     0,   369,     0,
       0,     0,     0,   400,   401,     0,   397,   388,     0,     0,
     389,   402
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1931,  5865,  3740, -1931,    -1,   234,  1626,  -163, -1931,  -364,
   -1931,   405, -1931,  -728, -1931,   824,  -952,  -972, -1931,   200,
    3977,  1873, -1931,   410, -1931,  1447,   260,   787,   780,   617,
     783,  1398,  1399,  1401,  1406,  1404, -1931,   214,  -170,  8327,
     967, -1931,  1724, -1931, -1931,  -706,  7502, -1120,  2959, -1931,
    1836, -1931,   956,    34, -1931, -1931, -1931,   478,   124, -1931,
   -1832, -1930,   338,    97, -1931, -1931, -1931,   352, -1550, -1931,
   -1272, -1931, -1931, -1931, -1931,   143, -1066, -1931, -1931, -1211,
     480, -1931, -1931, -1931, -1931, -1931,    65, -1192, -1931, -1931,
   -1931,    74,   509,   510,   179, -1931, -1931, -1931, -1931,  -906,
   -1931,   107,    48, -1931,   183, -1931,   -75, -1931, -1931, -1931,
     963,  -680,  -928, -1328, -1931,    30, -1049,   163,  3335,  -872,
    -859, -1931,   -27, -1931, -1931,     8, -1931,  -158,  6301,    56,
    -249,  2774,   629,  -673,    86,   298,    76,  2144,  1531, -1931,
    2156, -1931,   441,  4304, -1931,  2095, -1931,   123, -1931, -1931,
      39,   443,  5070,  3767,   -40,  1944,  -346, -1931, -1931, -1931,
   -1931, -1931,  -710,  5856,  5690, -1931,  -386,  -112, -1931,  -663,
     303, -1931,   236,   801, -1931,   -91,  -429, -1931, -1931, -1931,
    -371,  6173,  -559,  1259,    40,  -631,  -636,  -334,  1708, -1931,
   -1327,  -142,   -16,  1332,   985,  7230,   161,  -500,  -231,  -185,
    -479,  1385, -1931,  1727,   112,  1297,  1604, -1931, -1931, -1931,
   -1931,   309,  -167,  -227,  -899, -1931,   281, -1931, -1931, -1131,
     512, -1931, -1931, -1931,  2231,  -830,  -466,  -889,   -30, -1931,
   -1931, -1931, -1931, -1931, -1931,   211,  -869,  -204, -1838,  -210,
    7793,   -74,  6383, -1931,  1256, -1931,  2293,    64,  -225,  -188,
    -187,    25,   -73,   -53,   -45,   -22,   -44,    51,   117,  -181,
      66,  -169,  -151,  -149,   157,  -145,  -102,   -93,  -749,  -741,
    -714,  -704,  -745,  -132,  -700, -1931, -1931,  -737,  1451,  1454,
    1460,  2075, -1931,   625,  7139, -1931,  -603,  -591,  -555,  -551,
    -761, -1931, -1549, -1730, -1726, -1717,  -645,  -138,  -305, -1931,
   -1931,   -20,   196,  -122, -1931,  7543,   942,  1104,  -461
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1209,   224,   410,   411,    82,    83,   412,   386,   413,
    1535,  1536,   414,  1015,  1016,  1017,  1326,  1327,  1328,  1547,
     436,   416,   417,   418,   718,   719,   419,   420,   421,   422,
     423,   424,   425,   426,   427,   428,   429,   438,  1114,   720,
    1456,   781,   218,   783,   432,   848,  1210,  1211,  1212,  1213,
    1214,  1215,  1216,  2136,  1217,  1218,  1463,  1654,  1986,  1987,
    1917,  1918,  1919,  2104,  2105,  1219,  1668,  1669,  1670,  1822,
    1823,  1220,  1221,  1222,  1223,  1224,  1225,  1849,  1853,  1480,
    1472,  1226,  1227,  1479,  1473,  1228,  1229,  1230,  1231,  1232,
    1686,  2122,  1687,  1688,  2022,  1233,  1234,  1235,  1459,  2030,
    2031,  2032,  2164,  2175,  2053,  2054,   303,   304,   914,   915,
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
      81,   192,   194,    81,   657,   513,   679,   552,   349,    97,
     381,   958,   431,  1024,   837,  1476,  1461,   723,   385,   313,
    1004,   509,   195,   529,   362,   534,   133,  1255,   365,   180,
     196,   197,   542,   503,   151,   377,  1409,   201,  1584,  1585,
     566,   729,   514,   515,   952,  1240,   997,   549,  1059,   516,
     890,   892,   944,  1900,  1087,    81,    81,  1901,    81,   667,
    1094,   517,   208,   670,   209,  1083,  1902,    97,  1331,  1084,
    1989,  1077,  1171,  1450,    81,   240,  1988,   105,   268,   518,
     464,   519,   279,    81,   133,   520,  1994,   103,   945,    58,
    1692,    81,   946,  1247,   513,   894,    81,  1338,  1078,    81,
     220,  1995,   160,    81,   605,   607,   902,  2049,  1079,   207,
     305,  1554,  1080,  1656,    58,   628,  2057,   316,   969,   448,
    1690,  1834,   239,  2103,   113,   267,   198,   657,   521,   278,
     209,   514,   515,  -789,   271,   105,   528,   522,   516,   449,
    1198,    81,  1460,  1555,    81,   103,    81,   450,   451,  2103,
     517,   923,    81,   146,   628,    97,   511,   194,  1693,    81,
    1372,   235,   293,   242,    91,   538,    81,   152,   518,   208,
     519,  -821,   133,   530,   520,  2138,   262,   195,  -790,  1914,
    1915,   973,   113,  1287,   486,   196,   197,   481,   953,   606,
      81,    81,   199,   563,   297,  1108,  1108,   142,   530,  1691,
     142,  1110,  1705,  1996,   574,    81,  1373,    58,   159,   208,
     640,   952,  1988,   305,  1108,   494,   207,   521,  2058,  1125,
      81,   944,    91,   105,    81,    81,   522,   275,  1990,    81,
      81,   485,   625,   103,  1236,  1904,   626,   208,   149,   558,
    1374,   305,   475,   452,   657,   610,   194,   384,  1421,  1295,
      81,   571,   305,   581,   246,   142,   207,   945,    81,  1284,
     113,   946,  1916,   260,  2050,  2051,   195,   272,    81,    81,
     113,  1994,    81,   165,   196,  1408,  1933,   599,  1319,    81,
     921,   198,  1412,  1108,   207,  2028,   854,   166,  1244,   657,
     174,   174,  1063,    81,    81,   524,    81,   525,   570,   104,
     142,    81,  1474,   882,  1422,    81,  1994,   970,   558,   453,
      91,   886,    20,   657,   841,  1087,  1437,  1423,    81,    81,
     657,  1345,  1310,   855,   856,  1475,   174,   939,    81,   640,
     857,   601,   980,  1358,  1077,    81,    81,  1359,  1410,   821,
      81,   808,   858,   142,   888,   207,  1240,   199,  1279,   568,
     893,  1549,  1900,  1022,  1981,  1088,  1901,   104,  1489,  1091,
     859,  1078,   860,   113,   964,  1902,   861,  1656,  1104,  1105,
     235,  1079,   242,   671,  1259,  1350,   174,   763,    81,   174,
     179,    81,   688,   262,   524,   854,   525,   637,   526,   298,
     660,   113,  1657,  1657,   174,  1827,   430,  1914,  1915,  1742,
   -1102,   375,   113,   637,   634,  1474,  1646,   637,  1802,   862,
    1704,   672,  1291,  1460,  1707,  1803,   673,   209,   863,   764,
     665,   215,   855,   856,   668,   486,   262,   113,  1475,   857,
    2048,   275,   216,  1168,  1804,   308,   567,   999,   448,  1584,
    1585,   858,   109,  1429,   114,   104,    81,  2002,   217,  1694,
     537,   201,  1477,  1432,  1433,   174,   349,   545,   449,   859,
     510,   860,  1430,   602,  1374,   861,   450,   451,  2096,    81,
      81,   606,   485,   181,  2085,  1478,   365,   526,   562,  1279,
    1943,    81,    81,  1198,   944,  1421,  1421,  1421,  1108,   573,
      81,   680,   494,  1383,   682,   646,   901,  1109,  1109,   936,
     109,   486,   114,   174,   174,  1436,   637,   202,   862,   999,
      81,  1402,  1511,  1461,   174,   298,  1109,   863,   640,  2011,
     945,  1615,    81,   634,   946,   672,    58,   182,   584,   174,
     673,   589,  1648,  1403,  1904,  1236,   149,   448,   308,    58,
     481,  1422,  1422,  1422,   305,   213,  1825,    81,   485,   262,
     190,  1833,   298,    81,  1423,  1423,  1423,   449,  -596,  1938,
    1939,   575,   452,   174,   724,   450,   308,    58,    58,  1434,
     174,   174,   225,  1340,  1981,   174,   587,   308,   109,   730,
     114,  1755,  1757,  1759,   731,  1109,   922,   874,   109,   875,
     114,  1630,   220,  1576,   657,   475,  1754,    14,    15,    16,
      17,    18,   308,  1048,   999,    58,   539,  1641,   803,    81,
     530,    81,   530,   174,  2075,    81,   174,   481,  1855,   613,
      63,    64,   958,   530,    97,    81,  1642,  1558,   453,    81,
      81,  1429,   999,  1264,   190,  1471,  -972,   485,  1364,  1460,
     732,   133,  2077,  -972,  1851,   733,  1657,   799,  1064,  2042,
    1711,   530,   530,   475,  1118,  1098,    58,   921,  2005,  2006,
    1470,   221,    81,  -821,   161,    -3,   262,   162,   163,    77,
     164,   637,   475,  1149,   225,    81,   874,  1852,   875,   568,
     876,   109,   821,   114,    58,  1085,  1793,    58,   937,   644,
     226,   187,   105,    58,   113,   637,    14,    15,    16,    17,
      18,  1501,   103,  1645,   174,  1794,  1932,   621,   637,   109,
     251,   114,   957,   999,    58,  1150,   174,   174,  -477,    58,
     109,  1518,   114,  2109,  1641,   963,  1334,   581,  1797,   261,
      81,  1527,    81,  1330,    81,   568,   622,   623,    81,   113,
     283,    81,   290,  1796,   292,   109,  1679,   114,  2037,   884,
     765,   999,   481,   999,   766,    58,  1497,  1803,  1431,  1905,
      58,  1655,  1671,  2111,  1092,    58,    81,  1137,   644,   876,
    1835,   530,    58,  1141,   896,   227,  1899,   530,  1906,    91,
    1657,   900,    58,   261,   270,   904,   290,   292,    58,   746,
    1109,  1803,  -473,   481,  1145,   999,   747,   748,   530,  1385,
     293,   699,    58,   530,  1886,    58,  1887,   475,  2004,    58,
    1909,    81,   142,    81,  2000,   481,   481,  -478,  1828,  1950,
    2017,  -650,   723,  1829,  1951,    81,   743,   744,  -650,   295,
    1283,   992,    81,   349,   481,   261,  1544,   297,   494,  1752,
    1389,    81,   993,   994,   530,  1393,  2090,   743,   475,   530,
      81,  2091,  1509,   365,   298,   979,   644,   317,   982,   983,
     174,   984,  1563,   190,   932,   935,   530,   384,  1570,   308,
     986,  -973,   530,   988,   989,   990,    81,   743,  -973,  -810,
    1371,   190,  1574,   753,   754,  2043,   644,  2068,  -715,   530,
    1502,  1503,   161,   379,  1178,   162,   163,   961,   164,  1817,
    1818,  1819,   261,   481,   290,   292,    74,  1779,  1780,  1781,
     174,    81,    81,   494,   104,    14,    15,    16,    17,    18,
      74,  1820,    97,   200,    64,   657,   784,   755,   756,  1744,
     530,   941,   629,   293,  1378,  1314,   261,    79,    80,  1400,
     643,   261,  1315,  1657,   644,   252,   253,   261,   254,  1357,
     821,    79,   645,   255,  2153,  1248,  1546,  1782,   337,  2154,
    1734,    81,  1795,  1330,   921,  1008,   297,  1010,   454,  1013,
    1676,  1657,    74,  1021,    58,   688,  1025,   637,    74,   261,
     660,   842,   843,   382,   662,   844,   292,   297,   383,   454,
     105,   530,  1783,   539,  1263,   867,   530,   530,   643,   455,
     103,  1050,   644,    79,    80,  1657,  1027,  1028,  1029,    79,
     645,    81,   109,   791,   114,  1655,   262,   792,   456,    81,
     911,   972,   646,   706,   912,   626,   212,  1513,   457,   974,
     475,  1583,  1622,   626,    74,   458,   912,   113,   298,   430,
      14,    15,    16,    17,    18,  1739,  1490,   459,    81,   975,
    1113,   494,  1068,   976,   643,   460,   530,   109,   644,   114,
     174,  1413,  1414,  1415,  1750,    79,   645,   174,  -122,  -122,
    -122,  -122,  -122,  -122,    81,   998,   261,    91,  1122,   999,
      81,    81,  1123,   297,  -474,  1126,   484,   530,  1249,   539,
     381,   381,   212,   530,    14,    15,    16,    17,    18,    58,
     488,   454,   261,   530,   662,   292,   502,   941,   481,  1159,
     142,    81,  1170,   999,  -475,   489,   613,  1525,   454,   706,
     530,   142,   580,    64,    14,    15,    16,    17,    18,   640,
    1161,   504,   446,   734,   999,   735,   736,   737,   613,  1532,
     349,   507,   530,  1817,  1818,  1819,  1180,   174,   174,  1671,
    1736,   508,   261,    58,  1329,  1026,  1428,  1085,  1330,   454,
     365,   644,  1863,  1864,   738,  1820,   527,   739,   740,  1605,
     921,   251,   741,   742,  1821,  1496,  1531,   261,   528,   792,
    1330,  2124,   261,    58,   261,  2128,   494,   550,  1747,    81,
      81,    81,  1748,    97,  1810,  1811,  1282,  1248,  1330,   999,
     596,   559,  1837,  1592,  1593,   261,   999,   261,   261,  1806,
    1806,  1806,  1238,  1838,   494,   268,   279,  1123,  1839,   261,
      81,  1910,   999,    97,  1586,   792,  1956,  2035,   749,   750,
     999,  1997,   261,    81,  2094,   999,    81,    81,  1330,   551,
      81,   261,  -121,  -121,  -121,  -121,  -121,  -121,   561,    81,
     183,     6,     7,     8,     9,    10,    11,    12,    13,  1335,
     572,   105,   267,   278,   261,   617,   662,   292,   271,  2095,
     559,   103,   148,   999,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,    81,  1817,  1818,  1819,   261,   662,
     642,   105,  2172,   751,   752,   261,  2169,   999,    81,  2178,
      74,   103,   220,  2179,   757,   758,   637,  1820,   113,   202,
     726,   280,  2055,   262,   494,  1970,  1826,   632,   726,   600,
    1631,    76,    81,   957,  1001,  1002,   632,  1632,  1100,  1101,
    1249,    79,    80,  1102,  1103,  1113,   664,   475,   113,   677,
    2055,  1681,  1682,  1683,  1684,  1685,   349,   568,    91,   481,
     481,  1317,  1123,   673,    81,   109,   681,   114,  1332,  1333,
     692,   275,   693,   142,  1873,   726,   365,   999,  1336,   724,
    1034,  1035,  1036,  1037,  2106,  -157,  -157,   696,    91,   212,
     148,   142,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,   697,  1428,  1428,  1428,    19,  1304,  1624,  1428,
     260,   272,  1308,  1470,  1471,   698,   513,  1102,  1488,   642,
    2040,   142,   702,  1316,  1658,    14,    15,    16,    17,    18,
     996,   174,   759,    81,   174,   174,   174,    81,    81,  1552,
    1553,   745,  1894,   760,   142,    48,    49,    50,    51,    52,
      53,    54,    55,   514,   515,   762,   151,   761,   494,   174,
     516,  1557,  1553,  1443,   793,   174,  1561,  1553,  1074,  1545,
     571,   767,   517,  1537,   794,  1817,  1818,  1819,   589,  1954,
    1955,   494,   494,  1594,  1545,   174,   795,   999,  1074,  1607,
     518,    81,   519,  1238,  1760,  1123,   520,  1820,  1638,   796,
    1639,    -3,   446,   446,  1884,  1123,  -186,   797,   363,  1885,
    1553,   430,  1891,  1892,  1897,   999,   798,   570,  1974,  1553,
    1975,  1553,   461,  1238,  1914,  1915,   825,   105,   105,   174,
    2169,  2170,  -476,   494,  1550,  1551,   -18,   103,   103,   521,
    1846,  1032,  1033,  1533,   838,   363,  1030,  1031,   522,   839,
    1038,  1039,   494,   475,  1706,  1708,   849,    81,  1807,  1808,
    1559,   864,    81,    81,    81,   349,   852,  1798,   568,   865,
    1248,   866,  1737,  1738,   113,   113,  1498,  1499,   868,   261,
     869,  1586,   870,   854,   871,   365,   872,   873,   878,   152,
     261,  1640,    14,    15,    16,    17,    18,   430,   430,   261,
     880,   879,   310,   189,   898,   699,   910,   909,   959,   899,
    -594,  -592,   908,   913,    91,    91,   916,   924,   926,   947,
     855,   856,   142,   930,   949,   966,   977,   857,   971,    81,
     646,  1000,   978,   446,    81,  1586,   109,  1003,   114,   858,
      81,   263,    81,  1073,  1006,  1052,  1047,   142,   142,  1074,
      81,    58,   284,   287,  1120,   567,  1081,   859,  1128,   860,
    1129,   494,  1130,   861,  1131,  1132,   109,  1963,   114,  1133,
    1134,  1135,   174,   174,   494,   148,  1136,   683,  1658,    65,
      66,    67,    68,    69,    70,    71,    72,   743,   657,  1151,
    1158,  1160,  1949,  1162,  1638,   263,  1639,   261,  -793,  1638,
     271,  1639,  1166,  1249,  1173,  1241,   862,   524,  1174,   525,
    1175,    74,  1260,  1272,  1256,   863,   174,   174,  1273,   494,
    1274,   261,  1275,  1355,    76,  1538,  1539,  1540,  1286,  1287,
    1290,   784,  1541,  1542,  1292,   530,   142,  1293,  1296,  1297,
    1299,  1300,    79,    80,   446,   262,  1301,   263,  1302,  1660,
    1660,  1303,   684,  1305,  1985,  1306,  1248,  1307,  1312,  1313,
     481,   481,  1337,  1377,  1320,    81,  1321,    81,   685,  1341,
     686,   687,    65,    66,    67,    68,    69,    70,    71,    72,
    1342,   105,  1343,  1344,  1348,  1349,  1351,  1640,  1352,   967,
    1362,   103,  1640,   275,  1353,  1354,  -679,  -678,  2029,  1401,
     526,    14,    15,    16,    17,    18,  1309,    81,  1406,  -794,
      81,  1438,  1441,  1442,   263,  1451,  1458,  1452,  1453,   494,
    1462,  -714,  1651,   494,  1464,  1492,   999,  1485,   113,  2084,
    1483,  1486,   260,   272,  1494,  1519,  1526,   494,  1529,  1543,
    1545,  1582,  1567,  1568,  1586,  1569,  1560,   494,   263,  1580,
    1581,  1587,   495,   263,  1590,  1613,  1588,  1589,  1595,   263,
     494,  1553,   494,   494,   494,  1598,   142,   513,    91,    81,
      81,  1602,  1612,  1603,  1604,   874,   261,   875,   446,  1616,
    1198,  1627,  1629,  1431,    84,   174,  1672,   150,  2101,  1249,
    1985,   263,   109,   109,   114,   114,   174,  1471,   363,  1673,
    1675,   142,   568,  1677,   514,   515,  1695,  1689,  1697,   174,
     261,   516,  1698,  1699,  1700,   105,   261,  1701,  1712,  1537,
    1714,    81,   142,   517,  1715,   103,  1717,   494,  1718,  2126,
    1719,   494,  1721,  1723,  2029,   708,   494,  2025,  2029,  2029,
    1722,   518,    84,   519,  1724,  1741,  1725,   520,  1731,  1745,
    1753,  1117,  1746,   454,   174,  1749,   363,  1761,   191,  1777,
    1768,   494,   113,  1770,  1594,  1772,  1773,    84,   876,  1774,
     481,  1778,  2151,  1632,  1792,   363,  1812,  1814,  1862,   299,
     231,  1874,  1843,   259,   221,  1845,  1638,    84,  1639,   567,
     521,  1865,  1866,  1869,  1870,  1871,   637,  2163,  1876,   522,
    1883,  2163,    91,  1660,   494,  2171,  1878,  1888,   494,  1889,
    1890,  2025,  1922,  2023,   263,   591,  2173,  1896,   610,   194,
    1927,   494,  1928,  1942,   150,  1940,  1946,  1948,  1958,  1952,
      84,   708,    81,   150,    81,   142,   320,   328,  1953,   195,
    1968,  1969,  1966,  1967,  1971,  1972,  1973,   196,   530,   348,
    -680,  1978,  1979,  1980,   174,  1999,  2001,  -577,   174,  2007,
    2012,  2038,  2026,  2039,   261,  1892,   494,  2018,   506,  2010,
     637,  2052,   174,   437,   191,   191,  2061,  2027,   105,  1640,
    2078,  2074,   174,  1289,  2076,   150,   467,  2023,   103,   259,
    2087,    81,    81,  2088,  2089,   174,   263,   174,   174,   174,
    2093,  2092,   494,  2099,   174,   494,   105,  2108,   320,  2110,
     363,  2112,   494,   231,   231,  2121,   103,   263,   207,  2125,
    1848,   261,  2132,  2127,  2133,   113,  2134,   835,  2139,   495,
    2149,   263,    81,  2150,   320,  2152,  2161,  1660,   430,  2158,
     105,   494,    84,  2160,   263,   494,   109,   494,   114,   598,
     103,   363,  2165,   113,  2166,  2176,   259,   524,   485,   525,
    2177,  2180,   174,  1880,  1556,    91,   174,  1040,   494,  1041,
     782,   174,  1042,   363,   363,   995,   263,  1044,  1043,    81,
    1457,  1466,  2159,  1847,  2102,  1944,  2119,   113,    81,   320,
    2097,  1937,   363,    91,  1854,   328,   174,   874,   142,   875,
     263,   328,   320,   320,  2147,  1841,  1842,   263,   446,  2129,
    2080,   150,  2167,  2079,  1484,   171,   188,   288,   560,   959,
      14,    15,    16,    17,    18,  1983,   142,    91,  2047,  1481,
    1626,   348,   647,   656,  1119,  1285,  1258,   845,  1859,   174,
     928,     3,  1294,   174,  1776,  1055,     0,   348,  1056,   722,
     526,   348,  1179,     0,  1057,     0,   174,   885,     0,   281,
     142,   363,     0,   282,     0,     0,   286,     0,   291,  2086,
       0,     0,   430,     0,   430,     0,     0,     0,     0,    58,
     109,     0,   114,     0,     0,     0,   437,     0,     0,     0,
     876,     0,     0,     0,     0,     0,     0,     0,   261,   148,
    1660,   174,     0,    65,    66,    67,    68,    69,    70,    71,
      72,  1322,     0,   430,     0,  1323,     0,  1324,     0,     0,
     437,     0,     0,   785,     0,     0,   541,     0,  1660,     0,
     191,   591,     0,   363,     0,     0,     0,   174,     0,    74,
     174,     0,     0,     0,     0,  2148,   150,   174,    76,     0,
     467,  1548,     0,     0,   814,     0,   656,     0,     0,  1783,
       0,     0,  1660,   530,     0,     0,     0,     0,     0,     0,
      79,    80,     0,  1500,  2034,     0,   174,     0,   193,     0,
     174,     0,   174,   883,     0,     0,     0,     0,     0,   430,
       0,   887,     0,     0,   231,     0,     0,     0,     0,     0,
     234,     0,     0,   174,     0,  1528,   231,     0,   895,     0,
       0,     0,     0,     0,  2174,     0,   281,     0,     0,   903,
       0,     0,  1154,  2181,     0,     0,     0,     0,     0,     0,
       0,   320,     0,   437,   437,     0,     0,   320,     0,   348,
     261,     0,   148,   109,     0,   114,    65,    66,    67,    68,
      69,    70,    71,    72,  1562,   261,   322,     0,     0,     0,
       0,     0,     0,   281,     0,     0,   363,     0,     0,     0,
       0,   109,     0,   114,     0,   495,     0,     0,   835,     0,
     148,   263,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,     0,   320,     0,   320,     0,   348,     0,    84,
       0,     0,  1054,     0,     0,   109,   282,   114,   661,     0,
     291,     0,     0,     0,     0,   348,   467,  1071,   656,     0,
       0,  1072,     0,     0,     0,   790,   647,     0,   322,     0,
     647,     0,     0,   512,   234,     0,     0,     0,     0,   348,
     722,   801,   261,     0,   804,     0,   722,     0,   707,   656,
       0,     0,   348,  1445,   322,   722,     0,     0,     0,     0,
       0,   148,     0,     0,   150,    65,    66,    67,    68,    69,
      70,    71,    72,  1011,   722,     0,     0,   437,     0,   446,
     150,   150,     0,   437,     0,     0,     0,     0,     0,     0,
       0,     0,   437,     0,     0,   150,   150,   150,     0,     0,
       0,   541,     0,     0,     0,     0,     0,     0,     0,   322,
       0,     0,     0,  1012,     0,  1720,     0,     0,     0,     0,
     148,     0,   611,   322,    65,    66,    67,    68,    69,    70,
      71,    72,  1733,     0,     0,     0,     0,     0,     0,     0,
     363,  1740,     0,     0,   707,     0,     0,     0,  1325,     0,
       0,   467,     0,     0,     0,     0,  1325,     0,  1751,     0,
       0,     0,     0,   261,     0,     0,     0,   785,   785,    76,
       0,     0,   834,    19,     0,   437,   183,     6,     7,     8,
       9,    10,    11,    12,    13,  1325,     0,     0,   495,     0,
       0,     0,   467,     0,   148,   814,     0,   814,    65,    66,
      67,    68,    69,    70,    71,    72,  1322,   363,   363,     0,
    1323,     0,  1324,     0,   348,   348,    52,    53,    54,    55,
       0,     0,   281,     0,     0,     0,     0,     0,     0,     0,
    1157,     0,     0,   348,     0,     0,     0,     0,     0,     0,
       0,     0,  1165,    76,     0,     0,  1169,     0,  1325,     0,
    1172,     0,     0,     0,     0,     0,     0,     0,     0,   148,
     320,     0,   261,    65,    66,    67,    68,    69,    70,    71,
      72,  1019,     0,     0,   815,     0,     0,     0,   263,     0,
       0,     0,  1346,     0,     0,   100,  1347,     0,   154,     0,
    1836,     0,     0,     0,     0,     0,     0,   437,     0,     0,
       0,     0,   348,  1360,     0,     0,     0,     0,   150,   437,
    1361,  1020,   263,     0,   853,     0,     0,     0,     0,     0,
       0,   348,     0,  1267,     0,     0,   234,     0,     0,     0,
       0,     0,     0,     0,   647,     0,     0,     0,     0,     0,
       0,     0,     0,   100,     0,     0,   363,     0,  1872,     0,
       0,   322,     0,  1875,     0,  1397,     0,   322,     0,  1398,
       0,  1467,     0,  1399,  1882,     0,     0,     0,   206,   790,
     790,     0,     0,     0,   467,     0,     0,     0,     0,  1066,
       0,     0,  1069,     0,     0,     0,     0,   148,   273,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,   148,
       0,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,   920,     0,   322,     0,     0,     0,     0,
       0,   307,     0,     0,     0,   312,     0,     0,     0,     0,
       0,   100,     0,     0,   318,     0,     0,     0,     0,     0,
       0,   785,   541,     0,     0,     0,     0,  1610,     0,  1139,
     350,   495,     0,  1143,     0,   933,    58,  1147,   814,  1325,
    1468,     0,   934,     0,     0,   814,   263,     0,     0,     0,
       0,     0,     0,     0,   318,   447,     0,  1964,  1965,     0,
       0,     0,     0,     0,     0,     0,   312,   473,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,   348,     0,     0,
       0,     0,     0,     0,     0,   523,  1411,     0,     0,     0,
       0,     0,     0,   263,   307,     0,     0,     0,     0,     0,
    1435,     0,     0,     0,     0,   548,  1355,    76,     0,     0,
     553,   555,     0,   206,     0,     0,     0,  1454,     0,     0,
     150,     0,   307,     0,   722,   363,     0,     0,     0,   150,
       0,     0,     0,   307,     0,     0,   576,     0,   437,     0,
     578,     0,     0,     0,     0,   579,     0,     0,     0,     0,
       0,     0,     0,     0,  2041,   495,   555,     0,   307,     0,
       0,   437,   603,     0,     0,     0,     0,     0,   437,     0,
       0,     0,     0,  1599,   612,     0,     0,  1600,   363,   363,
       0,  1601,   318,     0,     0,  1076,     0,   815,     0,     0,
     259,    84,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   635,   320,     0,   659,     0,     0,     0,   150,
       0,     0,     0,   790,     0,     0,     0,     0,   666,     0,
     467,     0,   666,     0,  2098,     0,     0,     0,     0,     0,
     495,     0,   332,   333,   334,   335,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,   495,
     322,   467,     0,     0,  1325,   150,     0,   318,     0,  1325,
    1325,  1325,    58,     0,     0,     0,     0,     0,     0,     0,
     263,   148,     0,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,  1387,     0,     0,  1391,     0,     0,     0,
    1395,   318,     0,     0,   148,    58,     0,  1727,    65,    66,
      67,    68,    69,    70,    71,    72,   769,   770,   771,   772,
     773,   774,   775,   776,   777,   778,   779,   312,   348,   348,
     215,   635,    74,   336,     0,     0,     0,   148,     0,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,   337,    75,    76,     0,     0,     0,   780,  1647,  1649,
       0,  1762,     0,    79,    80,    74,     0,     0,  1763,     0,
       0,     0,  1764,     0,     0,     0,   150,   150,   150,   150,
       0,   150,   150,     0,     0,   812,    76,  1633,   328,   644,
      58,     0,     0,     0,     0,     0,    79,   813,   363,     0,
       0,     0,     0,     0,   437,   437,     0,  1709,     0,     0,
       0,     0,     0,     0,   318,   318,     0,     0,     0,     0,
     473,     0,   148,     0,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   307,     0,   263,     0,     0,
       0,     0,     0,     0,     0,   259,     0,     0,     0,     0,
      74,     0,     0,    14,    15,    16,    17,    18,  1076,     0,
       0,     0,     0,     0,  1356,   815,     0,   467,     0,     0,
     230,    76,  1325,     0,  1325,     0,     0,     0,   350,     0,
     100,    79,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,   647,   666,   940,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1565,     0,   204,
       0,   951,    58,     0,     0,     0,  1572,     0,     0,     0,
     635,     0,     0,     0,     0,   960,     0,     0,     0,     0,
       0,     0,   148,   666,   172,   173,    65,    66,    67,    68,
      69,    70,    71,    72,   148,   318,   228,   229,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   318,     0,
       0,   318,   318,     0,   318,     0,    14,    15,    16,    17,
      18,     0,    74,   318,     0,   204,   318,   318,   318,     0,
     484,     0,     0,  1633,  1784,     0,     0,     0,  1633,     0,
     437,   204,   230,    76,  1633,     0,  1633,     0,     0,     0,
       0,     0,     0,    79,    80,     0,     0,     0,     0,     0,
       0,  1840,     0,     0,     0,   204,     0,     0,     0,     0,
       0,     0,   328,   150,     0,    58,     0,     0,   470,     0,
       0,     0,   473,   322,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   263,     0,     0,     0,  1058,
       0,     0,     0,     0,     0,     0,   318,   148,   437,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,   940,     0,     0,   348,     0,  1082,   150,
       0,     0,     0,     0,   204,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   473,   473,     0,     0,     0,
       0,     0,     0,     0,     0,  2082,    76,     0,     0,   530,
       0,   150,     0,     0,   473,     0,    79,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   348,
     348,     0,    14,    15,    16,    17,    18,     0,  1577,     0,
       0,     0,     0,     0,     0,     0,  1784,  1784,     0,     0,
       0,     0,     0,   204,     0,     0,     0,     0,     0,     0,
       0,  1633,     0,     0,  1633,     0,     0,     0,     0,     0,
       0,     0,     0,   204,     0,     0,     0,     0,  1237,   328,
       0,     0,     0,   473,     0,     0,  1787,     0,     0,   154,
     318,    58,   437,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   666,     0,     0,  1271,     0,  1637,     0,     0,
       0,   148,  1277,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,   148,   320,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,  2100,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,   350,     0,     0,   116,     0,
       0,   116,   204,     0,     0,     0,  1784,  1280,     0,     0,
       0,   319,    76,     0,  1281,  1633,     0,     0,     0,     0,
       0,     0,    79,    80,     0,     0,     0,    58,     0,     0,
       0,   148,   204,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
     150,     0,     0,     0,     0,     0,   116,     0,   214,   148,
       0,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,   348,
       0,   116,     0,     0,     0,  1784,     0,    74,  1787,  1787,
    1447,     0,     0,     0,     0,   150,     0,   265,   296,     0,
       0,   116,     0,     0,     0,     0,     0,  2082,    76,     0,
       0,   530,     0,     0,     0,   204,   204,     0,    79,    80,
       0,   470,     0,   150,   150,     0,  2083,   328,   473,     0,
       0,     0,     0,     0,   116,     0,     0,     0,   116,     0,
       0,     0,     0,  1637,   116,     0,     0,   116,  1637,     0,
       0,   265,     0,     0,  1799,     0,  1637,   150,     0,     0,
       0,     0,   344,   116,   376,     0,     0,     0,     0,     0,
       0,   154,     0,     0,     0,     0,     0,     0,     0,   204,
    1425,     0,     0,     0,     0,  2083,  2083,   441,     0,  1237,
       0,     0,     0,     0,     0,     0,     0,     0,   470,   116,
     441,     0,     0,   265,     0,     0,     0,     0,  1787,     0,
       0,     0,   318,     0,     0,  2162,     0,     0,     0,  1237,
       0,   204,     0,     0,     0,     0,  2083,  2168,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   116,     0,     0,
       0,     0,  1482,     0,     0,     0,   204,     0,     0,     0,
       0,     0,     0,     0,   116,     0,   116,     0,     0,     0,
     318,     0,     0,     0,     0,   116,     0,     0,     0,     0,
     265,   635,     0,     0,     0,     0,   116,     0,     0,     0,
     553,     0,     0,  2045,     0,     0,   608,  1787,     0,     0,
       0,   585,     0,     0,   116,     0,     0,     0,     0,   116,
       0,   116,   350,     0,   265,   116,   318,     0,     0,   265,
       0,     0,     0,     0,     0,   265,     0,     0,     0,     0,
       0,  1911,     0,     0,  1637,   116,     0,     0,  1787,     0,
       0,     0,   148,   470,   582,   583,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   116,     0,   265,   116,     0,
       0,     0,     0,     0,     0,     0,     0,   204,     0,     0,
       0,   116,     0,     0,     0,   116,     0,     0,     0,   473,
     473,     0,     0,     0,   470,     0,  1440,     0,    14,    15,
      16,    17,    18,    77,   322,     0,     0,  1787,  1787,   415,
       0,     0,     0,     0,     0,     0,   470,   470,     0,     0,
     441,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,     0,     0,     0,     0,   470,     0,  1425,  1425,  1425,
     154,   555,   318,   318,     0,     0,     0,     0,  1787,     0,
       0,     0,     0,     0,   441,  1637,   810,    58,   811,     0,
       0,     0,     0,     0,     0,  1659,  1659,   827,   828,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     116,     0,     0,    58,   441,     0,     0,     0,     0,   148,
     265,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,   470,     0,     0,     0,     0,     0,
       0,   204,     0,     0,     0,   148,     0,    74,     0,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   350,     0,
       0,     0,     0,     0,     0,     0,     0,  1631,    76,     0,
       0,     0,     0,    74,     0,     0,     0,     0,    79,    80,
       0,     0,     0,     0,   154,   110,   116,     0,     0,     0,
       0,     0,     0,    75,    76,     0,   322,   441,   441,     0,
       0,     0,   265,   116,    79,    80,   204,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   116,     0,
     148,     0,   371,   372,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,   265,     0,     0,
       0,     0,     0,   110,   695,     0,     0,     0,   415,   701,
     265,     0,     0,     0,     0,   611,   322,     0,   710,   711,
     116,   116,     0,   116,     0,     0,     0,     0,     0,     0,
       0,    77,     0,   415,   415,     0,   373,   376,     0,   116,
     441,   318,   265,   374,     0,     0,     0,  1801,   274,     0,
     116,     0,     0,     0,   415,     0,   322,     0,     0,     0,
       0,     0,     0,   116,     0,     0,   265,     0,     0,     0,
     585,  1702,  1703,   265,  1816,     0,   116,     0,   965,     0,
       0,   110,     0,     0,   415,     0,     0,     0,   116,     0,
       0,   110,     0,     0,     0,     0,     0,    58,     0,   470,
       0,   441,     0,     0,   116,   116,     0,   441,     0,  1659,
     353,     0,     0,     0,     0,     0,   441,     0,     0,   116,
     116,   116,     0,     0,     0,     0,     0,   350,     0,   148,
     154,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,   474,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,   148,   318,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,   441,     0,   319,    76,     0,
     473,   473,     0,     0,   110,     0,     0,     0,    79,    80,
       0,   116,     0,     0,     0,     0,     0,     0,  1903,   441,
       0,     0,     0,     0,     0,  1097,   116,     0,     0,     0,
     116,     0,   110,     0,     0,     0,   441,   619,     0,   116,
       0,     0,     0,   110,     0,     0,   577,     0,     0,     0,
       0,   204,     0,     0,     0,     0,     0,     0,   116,   116,
       0,   353,   204,  1659,    58,     0,     0,     0,   110,     0,
       0,  1813,   274,     0,     0,     0,     0,   116,     0,     0,
       0,     0,     0,     0,  1824,     0,     0,     0,     0,     0,
       0,     0,     0,   204,     0,     0,   148,     0,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,  1155,     0,
       0,     0,   636,     0,     0,   274,     0,  1242,  1243,     0,
       0,  1245,     0,     0,    74,     0,     0,     0,   636,  1857,
       0,     0,   636,     0,     0,     0,     0,   116,     0,     0,
       0,   441,     0,     0,  1631,    76,   116,     0,     0,     0,
       0,     0,   116,   441,     0,    79,    80,     0,     0,     0,
     470,   470,     0,     0,     0,   116,     0,  1269,   441,   148,
       0,   580,    64,    65,    66,    67,    68,    69,    70,    71,
      72,  2024,   415,   415,   415,   415,   415,   415,   415,   415,
     415,   415,   415,   415,   415,   415,   415,   415,   415,   415,
     415,     0,     0,     0,     0,  1318,     0,     0,     0,     0,
     473,     0,     0,     0,     0,     0,     0,     0,   441,     0,
       0,  1049,     0,     0,     0,     0,  1659,     0,     0,  1913,
       0,   636,   393,  1923,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,  1936,     0,     0,
       0,  1339,     0,     0,  1659,  2024,     0,  1945,     0,     0,
       0,     0,   415,     0,     0,     0,     0,     0,     0,     0,
    1957,     0,  1959,  1960,  1961,     0,     0,     0,     0,     0,
       0,     0,   728,     0,     0,    77,   404,     0,  1659,     0,
       0,   116,     0,     0,     0,   116,     0,     0,  1363,   204,
       0,     0,   116,   353,     0,     0,     0,  1367,  1368,  1369,
    1370,     0,   116,     0,     0,  1375,  1376,  2123,     0,   116,
     474,     0,     0,     0,     0,  1384,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,  1993,     0,     0,
       0,  1998,     0,     0,     0,     0,  2003,     0,     0,     0,
       0,   116,     0,  1404,     0,     0,  1407,     0,     0,     0,
       0,     0,     0,     0,   116,     0,     0,     0,   116,     0,
       0,  2033,   116,     0,     0,     0,     0,   353,   474,   148,
     110,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,   116,     0,   636,   474,     0,     0,
       0,     0,     0,   116,     0,     0,     0,   353,     0,     0,
       0,     0,   441,     0,  2056,  1465,     0,     0,  2059,     0,
     636,     0,   204,     0,   415,     0,     0,     0,     0,   415,
      77,  2073,     0,   636,     0,   441,     0,     0,     0,     0,
     415,     0,   441,     0,  1487,     0,     0,     0,     0,     0,
       0,  1491,     0,  1493,  1495,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   265,   116,  1505,     0,  1506,     0,
    1507,     0,     0,     0,     0,     0,  2107,  1516,     0,     0,
       0,   148,   415,   116,     0,    65,    66,    67,    68,    69,
      70,    71,    72,  1322,   441,     0,     0,  1323,  1269,  1324,
       0,     0,     0,     0,     0,     0,     0,     0,   204,     0,
       0,  1522,  2130,     0,     0,  2131,     0,     0,     0,     0,
       0,     0,  2135,     0,   116,   441,     0,     0,     0,   116,
      76,   115,   474,  1756,     0,   148,     0,   200,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   353,     0,
       0,  2155,     0,     0,     0,  2157,     0,  2135,     0,     0,
       0,   470,   470,   353,     0,     0,     0,   353,     0,     0,
       0,     0,  1591,   474,     0,     0,   353,     0,  2157,  1596,
       0,     0,     0,  1597,    76,     0,     0,   834,     0,   115,
       0,     0,   116,   116,     0,   353,   353,   148,     0,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,  1614,
       0,     0,   116,     0,   353,     0,   116,     0,     0,     0,
     116,     0,   415,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1611,   276,     0,     0,     0,     0,     0,
     116,   116,   116,   116,   116,   116,   116,     0,     0,     0,
       0,   148,   265,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,  1696,     0,     0,     0,   115,   441,   441,
       0,     0,     0,     0,   353,     0,     0,   115,   110,    74,
       0,     0,   148,   353,   172,   173,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,   357,     0,     0,   812,
      76,     0,   636,   644,   415,   274,     0,     0,  1726,   265,
      79,   813,     0,     0,     0,  1730,     0,  1732,     0,     0,
       0,     0,     0,   646,   415,     0,     0,     0,     0,     0,
     488,   441,     0,   476,     0,     0,   116,     0,     0,     0,
       0,     0,   415,   415,   415,     0,     0,     0,     0,   415,
     415,     0,     0,     0,     0,   474,     0,   116,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
     115,   470,     0,   415,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1765,     0,     0,    74,     0,     0,     0,
     116,     0,  1769,     0,     0,     0,     0,   116,   115,     0,
       0,   116,     0,     0,     0,     0,  1631,    76,     0,   115,
     415,   415,     0,  1632,     0,     0,     0,    79,    80,     0,
       0,     0,     0,     0,     0,     0,     0,   357,   353,     0,
       0,     0,   353,     0,   115,     0,     0,     0,   276,   353,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   353,
       0,     0,     0,     0,   441,   148,   353,   228,   229,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   638,     0,
       0,   276,     0,    74,     0,     0,   265,   116,   353,     0,
       0,     0,     0,     0,   638,     0,     0,     0,   638,     0,
       0,   353,     0,  2082,    76,   353,     0,   530,     0,   353,
       0,     0,     0,  1984,    79,    80,     0,     0,     0,     0,
       0,     0,   441,     0,     0,     0,     0,     0,  1867,  1868,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     116,     0,  1877,   116,     0,     0,     0,   148,     0,   110,
       0,    65,    66,    67,    68,    69,    70,    71,    72,  1322,
       0,   387,     0,  1323,   388,  1324,   389,     0,   390,     0,
       0,     0,     0,     0,     0,   116,     0,     0,     0,   110,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
       0,     0,     0,   116,   116,     0,    76,   638,     0,  1758,
       0,     0,   274,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,   636,   398,   399,   400,     0,   401,   402,     0,     0,
       0,     0,     0,   265,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   441,     0,     0,     0,
       0,   353,   474,     0,   403,     0,     0,    77,   404,   357,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,   476,     0,     0,     0,
       0,     0,     0,     0,  1977,     0,     0,     0,     0,     0,
       0,   115,     0,     0,     0,     0,     0,   148,     0,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   353,
     353,     0,     0,     0,     0,    74,     0,   415,     0,     0,
       0,     0,     0,   357,   476,     0,   115,     0,     0,   353,
       0,   121,     0,   353,   121,   230,    76,   353,     0,     0,
       0,     0,   638,   476,     0,     0,    79,    80,     0,     0,
       0,     0,     0,   357,   116,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   638,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   638,
       0,     0,     0,   116,     0,   110,   110,     0,     0,   121,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   116,
       0,     0,  2081,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   121,   148,     0,   228,   229,    65,
      66,    67,    68,    69,    70,    71,    72,   116,   116,     0,
       0,   265,     0,     0,   121,     0,     0,     0,     0,     0,
       0,     0,   289,    74,     0,     0,     0,     0,   474,     0,
     116,     0,     0,   353,     0,     0,     0,     0,     0,     0,
    2120,   116,     0,   319,    76,     0,     0,   121,     0,     0,
       0,   121,     0,     0,    79,    80,     0,   121,   476,     0,
     121,     0,     0,     0,  2137,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   357,     0,     0,   120,     0,  2146,
     120,     0,     0,     0,     0,     1,     0,   353,   147,   357,
       0,     0,     0,   357,   353,     0,     0,     0,   353,   476,
     121,     0,   357,     0,     0,     0,     0,     0,     0,     0,
       0,   415,   121,     0,     0,     0,     0,     0,     0,     0,
       0,   357,   357,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,     0,     0,     0,     0,
     357,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     121,   415,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,     0,   203,     0,     0,     0,   121,     0,   121,
       0,     0,     0,     0,   121,     0,     0,     0,   121,     0,
     120,     0,     0,     0,   274,     0,     0,     0,     0,   121,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     357,     0,     0,     0,   115,     0,     0,     0,     0,   357,
       0,     0,   121,   120,   121,     0,     0,   120,   121,   110,
       0,     0,     0,   120,     0,     0,   120,     0,   638,     0,
       0,   276,     0,     0,     0,     0,     0,   353,   121,     0,
       0,     0,     0,     0,     0,   415,     0,   415,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,   476,     0,     0,     0,     0,   415,     0,     0,     0,
     353,   353,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   121,     0,     0,   120,     0,   415,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,   120,     0,     0,     0,     0,
     120,     0,     0,   556,   120,     0,     0,   121,     0,     0,
       0,     0,     0,   110,   357,   120,     0,     0,   357,     0,
       0,     0,   415,     0,     0,   357,     0,     0,     0,     0,
       0,     0,     0,   121,     0,   357,     0,     0,   120,     0,
     120,     0,   357,   597,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   125,   604,     0,   125,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,   614,     0,   357,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   357,     0,     0,
       0,   357,   633,     0,     0,   357,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   125,     0,     0,     0,     0,     0,     0,     0,
     121,   121,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   636,     0,     0,     0,   115,     0,   125,     0,   120,
       0,   121,     0,     0,     0,     0,     0,     0,     0,   727,
       0,     0,     0,     0,     0,     0,     0,   125,     0,     0,
     353,     0,     0,     0,     0,   115,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,   110,     0,     0,     0,
     768,     0,     0,     0,     0,   153,   121,     0,   276,     0,
     125,     0,     0,     0,   125,     0,     0,     0,     0,   120,
     125,     0,     0,   125,   110,   636,   806,     0,     0,     0,
     809,     0,     0,     0,     0,     0,     0,   638,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   353,     0,   831,
       0,     0,     0,   832,   833,     0,     0,   836,   110,     0,
       0,     0,     0,   125,     0,     0,     0,   357,   476,     0,
       0,   121,   850,   851,     0,   125,     0,     0,     0,     0,
       0,     0,     0,     0,   121,   205,     0,   121,   121,     0,
     121,     0,     0,     0,     0,   881,     0,     0,     0,   121,
       0,     0,   121,   121,   121,     0,   120,   120,     0,     0,
       0,     0,     0,   125,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
     125,   169,   125,     0,     0,   357,   357,   125,     0,     0,
       0,   125,   311,     0,     0,     0,     0,     0,     0,     0,
       0,   205,   125,     0,     0,   357,     0,     0,   169,   357,
       0,     0,     0,   357,     0,     0,     0,     0,     0,     0,
       0,   919,   120,     0,     0,   125,     0,   125,     0,     0,
       0,   125,   121,     0,   925,     0,     0,     0,     0,     0,
       0,   440,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   125,     0,   462,   169,     0,     0,     0,     0,   948,
       0,   115,   115,     0,     0,     0,     0,   169,     0,   169,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,   378,     0,   120,   120,     0,   120,     0,   554,     0,
     557,     0,     0,     0,     0,   120,     0,     0,   120,   120,
     120,     0,     0,     0,   476,   378,   125,     0,     0,   357,
     991,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   153,   121,     0,     0,     0,     0,     0,
     125,     0,     0,   169,     0,   121,   121,   169,     0,     0,
     169,   169,     0,     0,   169,     0,     0,   169,   169,   557,
     169,     0,   169,   357,     0,     0,   125,     0,     0,     0,
     357,     0,   257,     0,   357,     0,     0,     0,   120,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -480,  -480,     0,  -480,
      46,     0,    47,     0,  -480,     0,     0,     0,     0,  1112,
       0,     0,   169,     0,   440,   169,     0,     0,  1121,     0,
       0,    58,     0,     0,  1124,     0,     0,     0,     0,     0,
       0,     0,     0,   125,   125,     0,     0,     0,   169,   169,
     276,     0,     0,     0,     0,     0,     0,     0,   205,     0,
       0,     0,     0,   169,   125,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     1,
       0,     0,     0,  1167,   807,   115,     0,     1,     0,     0,
     120,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,   120,   357,     0,     0,     0,     0,     1,   125,
       0,     0,     0,     0,    77,   327,     0,     0,     0,     0,
       0,     0,    79,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   357,   357,     0,   169,
       0,     0,     0,     0,     0,     0,  1298,     0,     0,     0,
       0,   440,   440,     0,   125,     0,     0,   121,     0,     0,
       0,     0,     0,     0,     0,     0,   121,   125,     0,     0,
     125,   125,     0,   125,     0,   121,     0,     0,     0,     0,
       0,     0,   125,     0,     0,   125,   125,   125,     0,     0,
       0,     0,     0,     0,   378,     0,     0,     0,   121,   115,
       0,     0,     0,     0,     0,   121,     0,     0,     0,     0,
     169,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   125,  1365,     0,  1366,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   440,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   121,   378,     0,   440,     0,     0,   440,   440,
       0,   440,     0,     0,     0,     0,     0,     0,     0,     0,
     440,     0,     0,   440,   440,   440,     0,   638,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,   147,     0,
       0,     0,   120,     1,   169,   169,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,   357,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   115,     0,   120,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   125,     0,     0,
     115,   638,     0,   440,   120,     0,     0,     0,   125,   125,
       0,     0,     0,   121,   121,   121,   121,   121,   121,   121,
       0,     0,   120,   357,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   115,     0,     0,     0,     0,     0,
    1504,   121,   121,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1530,   120,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   169,   169,     0,     0,     0,     0,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   169,     0,
       0,   169,   169,     0,   169,     0,   169,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     121,     0,     0,     0,     0,     0,  1250,   440,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   169,     0,     0,
       0,   169,     0,     0,     0,   169,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
     120,   120,   120,   120,   120,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   175,   178,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,   120,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   121,   169,   169,
       0,     0,   223,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   169,     0,     0,     0,     0,     0,     0,     0,
     125,   463,   364,     0,     0,     0,     0,     0,     0,   125,
     121,     0,     0,  1713,     0,     0,  1716,     0,   125,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   314,   533,     0,   315,     0,     0,     0,     0,
     533,   125,     0,     0,     0,   121,   120,     0,   125,     0,
     338,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,   125,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   125,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   505,     0,  1766,  1767,     0,     0,     0,     0,     0,
       0,   533,     0,     0,   169,     0,     0,     0,  1250,     0,
       0,     0,     0,     0,     0,   125,     0,  1424,     0,  1716,
       0,     0,     0,   120,     0,     0,     0,   364,   648,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   564,
     565,     0,     0,     0,     0,   169,     0,   669,     0,   440,
     175,   169,     0,     0,   169,     0,   120,     0,   169,   121,
       0,     0,     0,     0,     0,   175,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,   440,     0,   615,
       0,     0,     0,     0,     0,     0,   618,   620,     0,     0,
       0,   627,   120,     0,     0,     0,     0,     0,     0,   533,
       0,     0,  1860,  1861,     0,     0,   125,   125,   125,   125,
     125,   125,   125,     0,     0,   533,   802,     0,   533,   805,
       0,     0,     0,   440,   120,     0,   364,     0,     0,   338,
     648,     0,   338,     0,   125,   125,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   211,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   269,     0,     0,     0,     0,     0,     0,
       0,   533,     0,     0,     0,   533,   169,     0,     0,     0,
       0,     0,     0,     0,     0,   169,   169,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   121,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,   211,     0,   364,     0,   329,     0,     0,
     223,     0,     0,   125,     0,     0,     0,     0,  1947,   369,
     121,     0,   829,   830,  1424,  1424,  1424,   153,  1620,  1621,
    1625,     0,     0,     0,     0,   169,     0,     0,     0,     0,
       0,     0,     0,   211,   169,     0,     0,   169,     0,   169,
     169,     0,  1716,     0,   121,     0,   483,     0,     0,   487,
     533,     0,     0,   364,     0,     0,     0,  1976,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   938,   364,     0,     0,     0,     0,     0,     0,     0,
       0,  1992,   648,     0,   169,     0,   648,     0,     0,     0,
       0,     0,     0,   956,     0,   364,     0,     0,     0,     0,
     125,     0,   211,     0,     0,     0,     0,     0,  2020,     0,
       0,  2021,     0,     0,     0,     0,   269,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1250,     0,   125,     0,     0,     0,     0,     0,     0,
       0,   167,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,     0,   338,     0,     0,     0,
       0,   487,     0,     0,     0,     0,   169,     0,   125,     0,
       0,   211,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   125,
       0,   641,     0,   658,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   721,   968,   364,     0,     0,
     120,     0,     0,     0,   294,     0,     0,     0,     0,     0,
       0,   125,     0,   533,   533,     0,     0,   300,   440,   301,
       0,     0,     0,   533,  1067,     0,   533,  1070,     0,     0,
       0,     0,     0,     0,     0,     0,   725,     0,   364,     0,
       0,   648,   169,   648,   648,     0,     0,     0,     0,     0,
     648,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,   364,     0,     0,     0,     0,     0,     0,     0,     0,
     211,   169,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,   533,     0,     0,     0,   533,     0,     0,     0,
       0,     0,   125,   533,  1140,     0,     0,   533,  1144,     0,
     641,   533,  1148,     0,   169,     0,   826,  1250,     0,  1152,
     169,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     535,   536,     0,     0,   540,     0,     0,   543,   544,     0,
     546,     0,   547,     0,     0,     0,     0,     0,     0,   440,
       0,     0,   889,   891,     0,     0,  1099,     0,   364,   533,
       0,     0,     0,  1111,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     648,     0,     0,   211,   211,   169,     0,     0,     0,   483,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,     0,     0,     0,     0,     0,     0,     0,   630,   631,
       0,     0,     0,  1182,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   663,     0,     0,     0,   369,     0,     0,
       0,     0,     0,     0,     0,   125,     0,     0,     0,     0,
       0,     0,     0,   169,   169,     0,   483,     0,   942,     0,
       0,   378,     0,     0,     0,     0,   169,     0,     0,     0,
       0,     0,     0,   125,     0,     0,   721,   533,     0,   641,
       0,     0,   721,     0,     0,     0,     0,     0,     0,     0,
       0,   721,     0,     0,   648,   648,     0,     0,     0,     0,
       0,   648,     0,     0,   211,     0,     0,   125,     0,     0,
     721,     0,     0,     0,     0,     0,     0,   725,     0,     0,
     725,   725,     0,   725,     0,     0,     0,     0,     0,   800,
       0,     0,   725,     0,     0,   725,   725,   725,     0,     0,
       0,     0,     0,   364,     0,     0,  1046,   533,  1388,     0,
     533,  1392,     0,     0,   533,  1396,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   169,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   483,     0,     0,     0,     0,     0,     0,     0,     0,
     877,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   211,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   483,     0,     0,     0,     0,     0,     0,     0,
       0,   169,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   483,   483,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   483,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,     0,     0,     0,
       0,     0,   648,  1512,     0,     0,     0,     0,     0,     0,
    1444,  1446,  1448,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,     0,   954,   955,     0,     0,     0,     0,
       0,  1469,     0,     0,     0,     0,     0,   962,     0,     0,
       0,     0,   483,     0,     0,   169,     0,     0,     0,   211,
       0,  1182,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   533,  1566,   826,     0,     0,     0,     0,     0,     0,
     533,  1573,     0,   648,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,   364,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1523,     0,   439,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     468,     0,     0,     0,   369,     0,     0,     0,     0,     0,
       0,     0,     0,   496,     0,   496,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1060,  1061,     0,     0,     0,  1276,  1065,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1086,     0,
       0,  1089,  1090,     0,  1093,     0,  1095,  1096,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,   388,     0,   389,     0,   390,     0,     0,
       0,     0,     0,   364,   609,     0,     0,     0,     0,     0,
       0,     0,     0,    58,   391,     0,     0,  1138,  1643,  1644,
       0,  1142,     0,     0,     0,  1146,     0,   483,     0,     0,
       0,   648,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,     0,   401,   402,     0,     0,     0,
     721,     0,     0,    74,     0,     0,     0,     0,     0,   725,
       0,     0,     0,     0,     0,     0,     0,     0,  1261,  1262,
       0,     0,     0,   403,     0,     0,    77,   404,     0,     0,
       0,     0,  1278,   405,   466,    80,   406,   407,   408,   409,
       0,   725,     0,     0,     0,     0,     0,     0,     0,     0,
     533,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   533,     0,     0,     0,
     269,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   211,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     641,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     496,     0,     0,     0,     0,     0,   496,     0,     0,     0,
       0,   847,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   369,     0,     0,     0,   725,     0,     0,     0,     0,
       0,     0,   364,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1809,     0,     0,  1278,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   364,     0,   483,   483,
       0,     0,     0,     0,     0,  1380,     0,     0,     0,     0,
       0,  1386,   533,   533,  1390,     0,     0,     0,  1394,     0,
       0,   918,     0,     0,     0,     0,     0,     0,   533,     0,
    1650,     0,     0,  1653,  1667,     0,     0,     0,     0,  1674,
       0,     0,     0,  1678,     0,  1680,   725,   725,   725,     0,
     468,   725,   725,     0,     0,     0,     0,     0,   487,     0,
       0,     0,     0,   950,     0,     0,     0,     0,     0,     0,
     302,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,   269,     0,     0,    46,   985,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,   533,     0,     0,     0,     0,   369,     0,    58,
     533,     0,     0,   847,  1005,     0,     0,  1007,     0,  1009,
       0,     0,     0,     0,     0,  1018,  1510,  1023,  1018,     0,
       0,     0,     0,     0,     0,  1520,  1521,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
    1962,     0,     0,     0,     0,  1051,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1053,  1775,
       0,     0,     0,     0,     0,   364,     0,   533,  2046,  1062,
       0,   533,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,   468,     0,  1564,  1051,  -407,     0,     0,
       0,     0,     0,     0,  1571,     0,     0,  1575,     0,  1578,
    1579,     0,     0,     0,     0,     0,     0,  1815,     0,     0,
       0,     0,   533,  1115,     0,     0,   496,     0,     0,     0,
     211,     0,  1830,  1832,     0,     0,     0,     0,     0,  1127,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1606,     0,     0,  1653,     0,     0,
       0,     0,   269,  1850,     0,     0,     0,     0,  1153,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   533,   533,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1514,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,   439,     0,     0,   369,     0,     0,     0,
       0,     0,   533,     0,     0,     0,     0,  1268,  1270,     0,
       0,     0,     0,     0,     0,   468,  1710,     0,     0,     0,
       0,     0,   387,     0,     0,   388,     0,   389,     0,   390,
       0,   725,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1018,    58,   391,     0,     0,   483,
     483,     0,     0,     0,  1921,     0,     0,     0,  1051,     0,
       0,     0,     0,  1924,     0,  1926,  1311,     0,  1931,  1935,
       0,  1667,     0,  1018,   392,   393,  1941,   394,   395,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,     0,   398,   399,   400,     0,   401,   402,   269,
       0,     0,  1575,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   496,
       0,     0,     0,     0,     0,   403,     0,     0,    77,   404,
       0,  1771,     0,     0,     0,   405,  1515,    80,   406,   407,
     408,   409,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,   388,     0,   389,     0,   390,     0,     0,
       0,     0,     0,     0,     0,  2009,     0,     0,     0,     0,
    2014,  2016,     0,     0,   391,     0,   496,     0,  1379,     0,
    1382,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2036,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   490,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
     725,   398,   399,   400,     0,   401,   402,     0,     0,     0,
       0,     0,  2060,    74,  2063,  1858,     0,  2065,  2067,     0,
       0,     0,  2070,  2072,     0,     0,     0,     0,     0,   483,
       0,     0,     0,   403,    76,     0,   491,   492,     0,  1455,
    1455,   493,     0,   405,    79,    80,   406,   407,   408,   409,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   725,     0,     0,   487,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2114,  2116,  2118,
       0,     0,     0,  1907,  1908,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1912,     0,  1508,     0,
       0,     0,     0,     0,  1517,     0,     0,     0,     0,     0,
       0,  2141,  2143,  2145,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   468,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   496,     0,     0,     0,     0,     0,   183,     6,
       7,     8,     9,    10,    11,    12,    13,  1018,     0,     0,
     847,     0,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,   252,   253,     0,   254,    46,     0,    47,  1982,   255,
       0,     0,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1608,  1609,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1018,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2044,     0,     0,     0,     0,   496,     0,     0,   847,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -455,     0,
       0,  2156,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1439,     0,
       0,  -455,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1005,     0,     0,     0,     0,     0,     0,     0,     0,
    1728,  1729,     0,     0,     0,     0,     0,     0,     0,   387,
       0,   496,   388,     0,   389,     0,   390,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     496,  1184,   847,   391,    -2,     0,  1186,  -244,  -244,  1187,
    1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,
    1198,  -338,  -338,  1199,  1200,  1201,  1202,  1203,     0,  1204,
       0,   392,   393,     0,   490,   395,  1205,  1206,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,  1207,
     398,   399,   400,     0,   401,   402,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -244,  1208,     0,   439,    77,   404,     0,     0,  1800,
     298,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,  2156,     0,     0,  -185,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
      20,  1439,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  1844,     0,     0,
       0,    46,   387,    47,     0,   388,     0,   389,     0,   390,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,  1184,     0,   391,    -2,     0,  1186,
    -245,  -245,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,
    1195,  1196,  1197,  1198,  -338,  -338,  1199,  1200,  1201,  1202,
    1203,     0,  1204,  1879,   392,   393,  1881,   490,   395,  1205,
    1206,    65,    66,    67,    68,    69,    70,    71,    72,   396,
     397,   384,  1207,   398,   399,   400,  1856,   401,   402,     0,
       0,     0,     0,  1898,     0,    74,     0,     0,     0,     0,
       0,     0,     0,  1439,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -245,  1208,     0,     0,    77,   404,
       0,     0,     0,   298,     0,   405,    79,    80,   406,   407,
     408,   409,     0,     0,   387,     0,     0,   388,     0,   389,
    -185,   390,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1184,     0,   391,    -2,
       0,  1186,     0,     0,  1187,  1188,  1189,  1190,  1191,  1192,
    1193,  1194,  1195,  1196,  1197,  1198,  -338,  -338,  1199,  1200,
    1201,  1202,  1203,     0,  1204,     0,   392,   393,     0,   490,
     395,  1205,  1206,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,  1207,   398,   399,   400,     0,   401,
     402,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1208,     0,     0,
      77,   404,     0,     0,     0,   298,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,  -185,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1018,     4,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,  1183,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   387,
       0,    46,   388,    47,   389,     0,   390,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,  1184,    58,  1185,    -2,     0,  1186,     0,     0,  1187,
    1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,  1196,  1197,
    1198,  -338,  -338,  1199,  1200,  1201,  1202,  1203,     0,  1204,
       0,   392,   393,    61,   490,   395,  1205,  1206,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,  1207,
     398,   399,   400,     0,   401,   402,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -3,  1208,     0,     0,    77,   435,     0,     0,     0,
     298,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,     0,     0,  -185,     4,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,  1183,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   387,     0,    46,   388,    47,   389,
       0,   390,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,  1184,    58,  1185,    -2,
       0,  1186,     0,     0,  1187,  1188,  1189,  1190,  1191,  1192,
    1193,  1194,  1195,  1196,  1197,  1198,  -338,  -338,  1199,  1200,
    1201,  1202,  1203,     0,  1204,     0,   392,   393,    61,   490,
     395,  1205,  1206,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,  1207,   398,   399,   400,     0,   401,
     402,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1208,     0,     0,
      77,   435,     0,     0,     0,   298,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,  -185,     4,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   387,
       0,    46,   388,    47,   389,     0,   390,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,   391,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   392,   393,    61,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,     0,   401,   402,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1661,  1662,  1663,     0,
       0,     0,   403,  1664,  1665,    77,   435,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,     0,     0,  1666,     4,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   387,     0,    46,   388,    47,   389,
       0,   390,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,   391,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,    61,   394,
     395,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,     0,   401,
     402,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1661,  1662,  1663,     0,     0,     0,   403,  1664,     0,
      77,   435,     0,     0,     0,     0,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,  1666,     4,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   387,
       0,    46,   388,    47,   389,     0,   390,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,   391,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   392,   393,    61,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,     0,   401,   402,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   403,     0,  1652,    77,   435,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   387,     0,    46,   388,    47,
     389,     0,   390,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,   391,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,   393,    61,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,     0,
     401,   402,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   403,     0,
       0,    77,   435,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     387,     0,    46,   388,    47,   389,     0,   390,   345,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   391,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,     0,   401,   402,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,     0,    77,   465,     0,     0,
       0,     0,     0,   405,   466,    80,   406,   407,   408,   409,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   387,     0,    46,   388,    47,
     389,     0,   390,   345,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   391,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,     0,
     401,   402,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   403,     0,
       0,    77,  1265,     0,     0,     0,     0,     0,   405,  1266,
      80,   406,   407,   408,   409,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     387,     0,    46,   388,    47,   389,     0,   390,   345,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   391,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,     0,   401,   402,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,     0,    77,   404,     0,     0,
       0,     0,     0,   405,    79,    80,   406,   407,   408,   409,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   387,     0,    46,   388,    47,
     389,     0,   390,   345,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   391,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,     0,
     401,   402,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   403,     0,
       0,    77,   465,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,  1991,     0,    -2,    -2,    -2,
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
      -2,     0,     0,     0,     0,     0,     0,    -2,    -2,  2019,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,
       0,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,
      -2,     0,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,    -2,    -2,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,    59,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    61,    62,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,    76,     0,    77,    78,     0,     0,     0,
       0,     0,     0,    79,    80,   257,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -480,
    -480,     0,  -480,    46,     0,    47,     0,  -480,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   148,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,    76,     0,    77,   258,     0,
       0,     0,  -812,     0,     0,    79,    80,   257,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -480,  -480,     0,  -480,    46,     0,    47,     0,  -480,
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
     258,     0,     0,     0,     0,     0,     0,    79,    80,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,     0,
       0,     0,     0,  -403,  -403,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -403,     0,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,    79,
      80,     4,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,     0,     0,     0,     0,  -404,  -404,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -404,
       0,     0,     0,    77,    78,     0,  1416,     0,  1417,     0,
       0,    79,    80,  1418,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1420,     0,     0,     0,    77,   981,
       0,  1416,     0,  1417,     0,     0,    79,    80,  1418,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1419,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1617,
       0,     0,     0,    77,   981,     0,  1416,     0,  1417,     0,
       0,    79,    80,  1418,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1618,     0,     0,     0,    77,   981,
       0,  1416,     0,  1417,     0,     0,    79,    80,  1418,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1419,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1619,
       0,     0,     0,    77,   981,     0,     0,     0,     0,     0,
       0,    79,    80,   257,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -480,  -480,     0,
    -480,    46,     0,    47,     0,  -480,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,   258,     0,     0,     0,
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
       0,     0,     0,     0,     0,   148,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,   588,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1075,    76,  -689,    77,   644,     0,     0,
       0,     0,     0,     0,    79,    80,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -480,
    -480,     0,  -480,    46,     0,    47,     0,  -480,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   148,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,    76,     0,    77,   258,     0,
       0,     0,  -816,     0,     0,    79,    80,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -480,  -480,     0,  -480,    46,     0,    47,     0,  -480,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   148,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,    76,     0,    77,   258,
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
       0,     0,     0,     0,     0,     0,    74,     0,   588,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   643,     0,  -689,    77,
     644,     0,     0,     0,     0,     0,     0,    79,    80,   183,
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
       0,     0,     0,     0,     0,     0,     0,    74,     0,   588,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   784,     0,  -689,
      77,   530,     0,     0,     0,     0,     0,     0,    79,    80,
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
    1106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -676,    77,   347,     0,     0,     0,     0,     0,     0,    79,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   346,    77,   347,     0,     0,     0,     0,     0,     0,
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
      74,     0,  1893,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   347,     0,     0,     0,     0,     0,
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
       0,    74,     0,  1895,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,   327,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    77,   347,     0,     0,
       0,     0,     0,     0,    79,    80,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -480,
    -480,     0,  -480,    46,     0,    47,     0,  -480,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,  1439,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,     0,     0,   388,     0,
     389,     0,   390,     0,     0,     0,     0,    77,   258,     0,
       0,     0,     0,     0,     0,    79,    80,  1184,     0,   391,
      -2,     0,  1186,  1914,  1915,  1187,  1188,  1189,  1190,  1191,
    1192,  1193,  1194,  1195,  1196,  1197,  1198,     0,     0,  1199,
    1200,  1201,  1202,  1203,     0,  1204,     0,   392,   393,     0,
     490,   395,  1205,  1206,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,  1207,   398,   399,   400,  1439,
     401,   402,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1208,     0,
     387,    77,   404,   388,     0,   389,   298,   390,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,     0,  1184,  -185,   391,    -2,     0,  1186,     0,     0,
    1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,  1196,
    1197,  1198,     0,     0,  1199,  1200,  1201,  1202,  1203,     0,
    1204,     0,   392,   393,     0,   490,   395,  1205,  1206,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
    1207,   398,   399,   400,     0,   401,   402,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1208,     0,     0,    77,   404,     0,     0,
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
       0,     0,     0,     0,     0,  -407,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,   302,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,    77,    46,     0,    47,     0,  -407,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -408,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,    19,   712,    20,   713,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   387,    77,    46,   388,    47,
     389,  -408,   390,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   391,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   714,     0,     0,     0,     0,  1198,     0,  -338,     0,
       0,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,     0,
     401,   402,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1208,     0,
       0,    77,   715,     0,     0,     0,   298,     0,   405,    79,
      80,   716,   717,   408,   409,    14,    15,    16,    17,    18,
      19,   712,    20,   713,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   387,     0,    46,   388,    47,   389,     0,   390,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   391,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   714,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,     0,   401,   402,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   403,     0,     0,    77,   715,     0,
       0,     0,   298,     0,   405,    79,    80,   716,   717,   408,
     409,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   387,     0,    46,
     388,    47,   389,     0,   390,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   391,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,     0,   401,   402,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     403,     0,   434,    77,   435,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   387,     0,    46,   388,    47,   389,     0,
     390,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,     0,   401,   402,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   403,     0,     0,    77,
     715,     0,     0,     0,   298,     0,   405,    79,    80,   406,
     407,   408,   409,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   387,
       0,    46,   388,    47,   389,     0,   390,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   391,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,     0,   401,   402,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   403,     0,     0,    77,   435,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   387,     0,    46,   388,    47,
     389,     0,   390,   345,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   391,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,     0,
     401,   402,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   403,     0,
       0,    77,   465,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   387,     0,    46,   388,    47,   389,     0,   390,   345,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   391,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,     0,   401,   402,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   403,     0,     0,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   703,     0,   704,   705,   257,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -480,
    -480,     0,  -480,    46,     0,    47,   -17,  -480,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,    77,    46,     0,
      47,     0,     0,     0,   345,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   148,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   588,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -689,    77,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   148,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,    76,     0,    77,    78,     0,     0,     0,
    -814,     0,     0,    79,    80,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   148,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,    76,     0,    77,    78,     0,
       0,     0,     0,     0,     0,    79,    80,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   148,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,    79,    80,   183,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,   588,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,  -689,
      77,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -480,  -480,
       0,  -480,    46,     0,    47,     0,  -480,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   148,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,    76,     0,    77,   327,     0,     0,
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
       0,     0,     0,     0,     0,     0,  1177,     0,     0,     0,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,    77,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   345,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,    77,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   987,
      77,   981,     0,     0,     0,     0,     0,     0,    79,    80,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,  1534,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,   981,     0,     0,     0,     0,     0,     0,
      79,    80,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,   310,     0,     0,     0,     0,
       0,     0,    79,    80,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,    79,    80,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,   461,
       0,     0,     0,     0,     0,     0,    79,    80,    14,    15,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,   347,     0,     0,     0,     0,     0,     0,    79,    80,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,   310,     0,     0,     0,     0,     0,     0,
      79,    80,    14,    15,    16,    17,    18,    19,     0,    20,
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
       0,     0,     0,     0,    77,   461,     0,     0,     0,     0,
       0,     0,    79,    80,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -480,  -480,     0,
    -480,    46,     0,    47,     0,  -480,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,    58,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,   345,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    77,   327,     0,     0,
       0,     0,     0,     0,    79,    80,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,   981,
       0,     0,     0,     0,     0,     0,    79,    80,    14,    15,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,   981,     0,     0,     0,     0,     0,     0,    79,    80,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,     0,     0,    14,    15,    16,    17,    18,
      79,    80,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -480,
    -480,     0,  -480,    46,     0,    47,     0,  -480,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   327,     0,
      14,    15,    16,    17,    18,    79,    80,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -480,  -480,     0,  -480,    46,     0,
      47,     0,  -480,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,    58,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,     0,    63,    64,     0,     0,     0,     0,
      79,    80,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   387,     0,    46,   388,    47,   389,     0,   390,     0,
       0,     0,     0,    77,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,     0,   401,   402,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   403,     0,     0,    77,   404,     0,
       0,     0,     0,     0,   405,   466,    80,   406,   407,   408,
     409,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     387,     0,    46,   388,    47,   389,     0,   390,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   392,   393,     0,   394,   395,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   396,   397,   384,
       0,   398,   399,   400,     0,   401,   402,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,     0,    77,   404,     0,     0,
       0,     0,     0,   405,    79,    80,   406,   407,   408,   409,
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
      70,    71,    72,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,    77,    47,     0,     0,     0,   345,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    63,    64,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -480,  -480,     0,  -480,    46,
       0,    47,     0,  -480,     0,     0,     0,   387,     0,     0,
     388,     0,   389,     0,   390,    77,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,     0,   401,   402,   387,     0,     0,   388,     0,   389,
      74,   390,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,  1661,  1662,  1663,     0,   391,     0,
     403,  1831,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   392,   393,     0,   394,
     395,  1929,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   396,   397,   384,     0,   398,   399,   400,     0,   401,
     402,   387,     0,     0,   388,     0,   389,    74,   390,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1661,  1662,  1663,     0,   391,     0,   403,  1930,     0,
      77,   404,     0,     0,     0,     0,     0,   405,    79,    80,
     406,   407,   408,   409,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,  1314,     0,    77,   404,     0,
       0,     0,  1315,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,     0,
       0,    77,   404,     0,     0,     0,   493,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,   387,   401,   402,   388,     0,   389,     0,
     390,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,   403,   846,     0,    77,   404,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,   387,   401,   402,
     388,     0,   389,     0,   390,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,   403,     0,     0,    77,
     404,     0,     0,     0,   298,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,   387,   401,   402,   388,     0,   389,     0,   390,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
     403,  1014,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,     0,     0,    77,   404,     0,
       0,  1045,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,  1381,
       0,    77,   404,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,   387,   401,   402,   388,     0,   389,     0,
     390,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,   403,     0,     0,    77,   404,     0,     0,     0,
    1449,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,   387,   401,   402,
     388,     0,   389,     0,   390,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,   403,     0,     0,    77,
     404,     0,     0,     0,  1524,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,   387,   401,   402,   388,     0,   389,     0,   390,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
     403,     0,  1920,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,  1925,     0,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,  1934,
       0,    77,   404,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,   387,   401,   402,   388,     0,   389,     0,
     390,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,   403,  2013,     0,    77,   404,     0,     0,     0,
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
     403,  2062,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,  2064,     0,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,  2066,
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
       0,   391,     0,     0,     0,     0,   403,  2071,     0,    77,
     404,     0,     0,     0,     0,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,   387,   401,   402,   388,     0,   389,     0,   390,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
     403,  2113,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,  2115,     0,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   403,  2117,
       0,    77,   404,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,   387,   401,   402,   388,     0,   389,     0,
     390,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,   403,  2140,     0,    77,   404,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,   387,   401,   402,
     388,     0,   389,     0,   390,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,   403,  2142,     0,    77,
     404,     0,     0,     0,     0,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,   387,   401,   402,   388,     0,   389,     0,   390,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
     403,  2144,     0,    77,   404,     0,     0,     0,     0,     0,
     405,    79,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,     0,     0,    77,   404,     0,
       0,     0,     0,     0,   405,    79,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,   387,
     401,   402,   388,     0,   389,     0,   390,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,   694,     0,
       0,    77,   404,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,     0,     0,     0,     0,     0,
       0,   392,   393,     0,   394,   395,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   396,   397,   384,     0,
     398,   399,   400,   387,   401,   402,   388,     0,   389,     0,
     390,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
       0,     0,   700,     0,     0,    77,   404,     0,     0,     0,
       0,     0,   405,    79,    80,   406,   407,   408,   409,     0,
       0,     0,     0,     0,     0,   392,   393,     0,   394,   395,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     396,   397,   384,     0,   398,   399,   400,   387,   401,   402,
     388,     0,   389,     0,   390,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,   709,     0,     0,    77,
     404,     0,     0,     0,     0,     0,   405,    79,    80,   406,
     407,   408,   409,     0,     0,     0,     0,     0,     0,   392,
     393,     0,   394,   395,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   396,   397,   384,     0,   398,   399,
     400,   387,   401,   402,   388,     0,   389,     0,   390,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,     0,     0,
     403,     0,     0,    77,   404,     0,     0,     0,     0,     0,
     405,   917,    80,   406,   407,   408,   409,     0,     0,     0,
       0,     0,     0,   392,   393,     0,   394,   395,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   396,   397,
     384,     0,   398,   399,   400,   387,   401,   402,   388,     0,
     389,     0,   390,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,   403,     0,     0,    77,   404,     0,
       0,     0,     0,     0,   405,   466,    80,   406,   407,   408,
     409,     0,     0,     0,     0,     0,     0,   392,   393,     0,
     394,   395,  2008,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   396,   397,   384,     0,   398,   399,   400,     0,
     401,   402,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   403,     0,
       0,    77,   404,     0,     0,     0,     0,     0,   405,    79,
      80,   406,   407,   408,   409,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   184,     0,   185,   186,   183,
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
      39,    40,    41,    42,    43,    44,    45,  -479,  -479,     0,
    -479,    46,     0,    47,     0,  -479,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,    20,    58,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -480,  -480,
       0,  -480,    46,     0,    47,     0,  -480,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,    75,    75,     4,   350,   230,   377,   256,   166,     1,
     177,   656,   182,   741,   493,  1226,  1208,   403,   181,   141,
     726,   225,    75,   233,   166,   235,     1,   926,   166,    59,
      75,    75,   242,   218,     4,   167,  1167,    77,  1365,  1366,
     271,   405,   230,   230,   647,   914,   719,   251,   785,   230,
     550,   551,   643,  1783,   815,    56,    57,  1783,    59,   364,
     821,   230,    84,   368,    84,   814,  1783,    59,  1020,   814,
       1,   812,   902,  1193,    75,    97,  1914,     1,   100,   230,
     202,   230,   104,    84,    59,   230,  1918,     1,   643,    72,
      84,    92,   643,   923,   319,   561,    97,  1049,   812,   100,
      89,    75,   152,   104,   314,   315,   572,     1,   812,    84,
     137,   123,   812,  1441,    72,   342,    75,   147,   677,   192,
      98,  1671,    97,  2053,     1,   100,    75,   473,   230,   104,
     150,   319,   319,     0,   104,    59,    99,   230,   319,   192,
      90,   142,  1208,   155,   145,    59,   147,   192,   192,  2079,
     319,   617,   153,     0,   381,   147,   230,   230,   152,   160,
    1112,    97,   155,    97,     1,   240,   167,     4,   319,   191,
     319,   160,   147,   156,   319,  2105,   100,   230,     0,    77,
      78,   681,    59,   176,   206,   230,   230,   203,   649,   152,
     191,   192,    75,   268,   152,   831,   832,     1,   156,   177,
       4,   832,   152,   177,   279,   206,   134,    72,   118,   231,
     348,   814,  2050,   240,   850,   216,   191,   319,   177,   850,
     221,   812,    59,   147,   225,   226,   319,   104,   159,   230,
     231,   206,   154,   147,   914,  1784,   158,   259,     4,   259,
     168,   268,   203,   192,   590,   319,   319,   118,  1176,   977,
     251,   273,   279,   293,    97,    59,   231,   812,   259,    10,
     137,   812,   160,   100,   158,   159,   319,   104,   269,   270,
     147,  2103,   273,   152,   319,  1164,  1826,   304,  1006,   280,
     614,   230,  1171,   919,   259,   156,   511,   152,   919,   635,
      56,    57,   792,   294,   295,   231,   297,   231,   273,     1,
     104,   302,   110,   534,  1176,   306,  2138,   678,   328,   192,
     147,   542,    20,   659,   499,  1076,  1185,  1176,   319,   320,
     666,  1058,   995,   511,   511,   133,    92,   632,   329,   467,
     511,   306,   696,  1082,  1075,   336,   337,  1082,  1168,   471,
     341,   463,   511,   147,   548,   320,  1215,   230,   951,   273,
     554,  1323,  2082,   739,  1903,   816,  2082,    59,  1247,   820,
     511,  1075,   511,   240,   669,  2082,   511,  1695,   829,   830,
     306,  1075,   306,    10,   933,  1075,   142,   133,   379,   145,
     152,   382,   383,   307,   320,   610,   320,   348,   231,   160,
     351,   268,  1441,  1442,   160,  1667,   182,    77,    78,  1530,
     151,   167,   279,   364,   348,   110,   177,   368,   151,   511,
    1476,   162,   971,  1479,  1480,   158,   167,   437,   511,   175,
     364,   149,   610,   610,   368,   447,   350,   304,   133,   610,
     134,   308,   160,   899,   177,   137,   273,   158,   511,  1766,
    1767,   610,     1,   158,     1,   147,   447,   168,   176,   154,
     239,   491,   109,    61,    62,   221,   614,   246,   511,   610,
     226,   610,   177,   306,   168,   610,   511,   511,    75,   470,
     471,   152,   447,   152,  2023,   132,   614,   320,   267,  1082,
     160,   482,   483,    90,  1075,  1413,  1414,  1415,  1124,   278,
     491,   379,   493,  1124,   382,   176,   571,   831,   832,   631,
      59,   523,    59,   269,   270,  1185,   467,   157,   610,   158,
     511,   146,  1273,  1705,   280,   160,   850,   610,   656,   168,
    1075,  1410,   523,   467,  1075,   162,    72,   152,   294,   295,
     167,   297,   177,   168,  2083,  1215,   302,   610,   240,    72,
     556,  1413,  1414,  1415,   571,   177,  1666,   548,   523,   473,
     152,  1671,   160,   554,  1413,  1414,  1415,   610,   160,  1831,
    1832,   280,   511,   329,   403,   610,   268,    72,    72,   177,
     336,   337,   158,  1052,  2123,   341,   295,   279,   137,   156,
     137,  1553,  1554,  1555,   161,   919,   616,   523,   147,   523,
     147,   177,    89,  1354,   940,   556,  1548,    13,    14,    15,
      16,    17,   304,   766,   158,    72,   152,   158,   152,   610,
     156,   612,   156,   379,   168,   616,   382,   633,    75,   152,
     106,   107,  1267,   156,   616,   626,   177,  1333,   511,   630,
     631,   158,   158,   938,   152,    92,   160,   612,  1099,  1705,
     156,   616,   168,   167,    75,   161,  1695,   152,   152,  1976,
     177,   156,   156,   614,   839,   825,    72,   991,  1930,  1931,
      91,   158,   663,   160,    58,   158,   590,    61,    62,   155,
      64,   632,   633,   877,   158,   676,   612,   108,   612,   603,
     523,   240,   814,   240,    72,   152,   158,    72,   632,   156,
     158,    62,   616,    72,   571,   656,    13,    14,    15,    16,
      17,  1260,   616,  1431,   470,   177,  1826,   134,   669,   268,
       3,   268,   656,   158,    72,   878,   482,   483,   134,    72,
     279,  1280,   279,   168,   158,   669,   151,   767,  1627,   100,
     731,  1290,   733,   158,   735,   659,   163,   164,   739,   616,
     111,   742,   113,   177,   115,   304,  1452,   304,  1959,   538,
     154,   158,   768,   158,   158,    72,  1256,   158,   152,   158,
      72,  1441,  1442,   168,   152,    72,   767,   152,   156,   612,
     177,   156,    72,   152,   563,   177,   177,   156,   177,   616,
    1829,   570,    72,   154,    69,   574,   157,   158,    72,   163,
    1124,   158,     3,   809,   152,   158,   170,   171,   156,   152,
     155,   391,    72,   156,  1756,    72,  1758,   768,  1928,    72,
     177,   812,   616,   814,   177,   831,   832,   134,   154,   154,
    1940,   160,  1208,   159,   159,   826,   416,   417,   167,   158,
     962,   154,   833,   991,   850,   206,  1315,   152,   839,  1545,
     152,   842,   165,   166,   156,   152,   154,   437,   809,   156,
     851,   159,   152,   991,   160,   694,   156,   176,   697,   698,
     626,   700,   152,   152,   630,   631,   156,   118,   152,   571,
     709,   160,   156,   712,   713,   714,   877,   467,   167,   160,
    1111,   152,   152,   128,   129,   152,   156,  2007,   159,   156,
    1261,  1262,    58,   152,   910,    61,    62,   663,    64,   146,
     147,   148,   273,   919,   275,   276,   132,  1617,  1618,  1619,
     676,   912,   913,   914,   616,    13,    14,    15,    16,    17,
     132,   168,   914,   106,   107,  1271,   152,   172,   173,  1532,
     156,   633,   154,   155,  1119,   153,   307,   163,   164,  1149,
     152,   312,   160,  1992,   156,    47,    48,   318,    50,  1081,
    1082,   163,   164,    55,   154,   925,   151,  1620,   174,   159,
    1519,   962,  1625,   158,  1298,   731,   152,   733,   154,   735,
    1449,  2020,   132,   739,    72,   976,   742,   938,   132,   350,
     941,   155,   156,   152,   355,   159,   357,   152,   152,   154,
     914,   156,   152,   152,   938,   154,   156,   156,   152,   154,
     914,   767,   156,   163,   164,  2054,   746,   747,   748,   163,
     164,  1012,   571,   154,   571,  1695,   940,   158,   154,  1020,
     154,   154,   176,   394,   158,   158,    84,  1276,   154,   154,
     991,  1365,  1418,   158,   132,   154,   158,   914,   160,   825,
      13,    14,    15,    16,    17,  1524,  1250,   154,  1049,   154,
     836,  1052,   152,   158,   152,   154,   156,   616,   156,   616,
     826,  1173,  1174,  1175,  1543,   163,   164,   833,    13,    14,
      15,    16,    17,    18,  1075,   154,   447,   914,   154,   158,
    1081,  1082,   158,   152,     3,   851,   152,   156,   925,   152,
    1257,  1258,   150,   156,    13,    14,    15,    16,    17,    72,
     152,   154,   473,   156,   475,   476,    22,   809,  1124,   154,
     914,  1112,   901,   158,     3,   158,   152,  1287,   154,   490,
     156,   925,   106,   107,    13,    14,    15,    16,    17,  1267,
     154,   152,   190,   121,   158,   123,   124,   125,   152,  1297,
    1298,   152,   156,   146,   147,   148,   912,   913,   914,  1829,
    1521,   158,   523,    72,   154,   745,  1176,   152,   158,   154,
    1298,   156,  1721,  1722,   152,   168,   158,   155,   156,  1400,
    1504,     3,   160,   161,   177,   154,   154,   548,    99,   158,
     158,  2087,   553,    72,   555,  2091,  1187,   152,   154,  1190,
    1191,  1192,   158,  1185,   154,   154,   962,  1167,   158,   158,
     151,   259,   154,  1373,  1374,   576,   158,   578,   579,  1638,
    1639,  1640,   914,   154,  1215,  1237,  1238,   158,   154,   590,
    1221,   154,   158,  1215,  1366,   158,   154,  1955,   165,   166,
     158,   154,   603,  1234,   154,   158,  1237,  1238,   158,   152,
    1241,   612,    13,    14,    15,    16,    17,    18,   160,  1250,
       4,     5,     6,     7,     8,     9,    10,    11,    12,  1045,
     160,  1185,  1237,  1238,   635,   160,   637,   638,  1238,   154,
     328,  1185,   104,   158,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,  1285,   146,   147,   148,   659,   660,
     348,  1215,   154,   126,   127,   666,   158,   158,  1299,   154,
     132,  1215,    89,   158,   130,   131,  1267,   168,  1185,   157,
     158,    65,  1992,  1237,  1315,  1874,   177,   157,   158,   177,
     152,   153,  1323,  1267,   163,   164,   157,   159,   157,   158,
    1167,   163,   164,   157,   158,  1121,   154,  1298,  1215,   160,
    2020,   111,   112,   113,   114,   115,  1504,  1271,  1185,  1365,
    1366,   157,   158,   167,  1355,   914,   176,   914,   157,   158,
     154,  1238,   118,  1167,  1735,   158,  1504,   158,   159,  1208,
     753,   754,   755,   756,  2054,   157,   158,   152,  1215,   437,
     104,  1185,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   152,  1413,  1414,  1415,    18,   987,  1418,  1419,
    1237,  1238,   992,    91,    92,   152,  1631,   157,   158,   467,
    1969,  1215,   152,  1003,  1441,    13,    14,    15,    16,    17,
      18,  1187,   164,  1424,  1190,  1191,  1192,  1428,  1429,   157,
     158,   169,  1766,   162,  1238,    57,    58,    59,    60,    61,
      62,    63,    64,  1631,  1631,   132,  1416,   174,  1449,  1215,
    1631,   157,   158,   177,   154,  1221,   157,   158,   157,   158,
    1482,   155,  1631,  1302,   154,   146,   147,   148,  1234,   158,
     159,  1472,  1473,   157,   158,  1241,   154,   158,   157,   158,
    1631,  1482,  1631,  1185,   157,   158,  1631,   168,  1424,   154,
    1424,   157,   550,   551,   157,   158,   177,   154,   166,   157,
     158,  1287,   157,   158,   157,   158,   154,  1482,   157,   158,
     157,   158,   156,  1215,    77,    78,   134,  1441,  1442,  1285,
     158,   159,   134,  1524,  1324,  1325,   159,  1441,  1442,  1631,
    1693,   751,   752,  1299,   159,   203,   749,   750,  1631,   158,
     757,   758,  1543,  1504,  1479,  1480,   152,  1548,  1639,  1640,
    1336,   154,  1553,  1554,  1555,  1713,   176,  1631,  1482,   154,
    1530,   154,  1522,  1523,  1441,  1442,  1257,  1258,   154,   940,
     154,  1713,   154,  1798,   154,  1713,   154,   154,   152,  1416,
     951,  1424,    13,    14,    15,    16,    17,  1373,  1374,   960,
     157,   176,   156,    62,   160,  1185,   158,   154,   656,   160,
     160,   160,   160,    70,  1441,  1442,   177,   157,   152,   157,
    1798,  1798,  1416,    78,    18,   158,   152,  1798,   160,  1620,
     176,   154,   177,   681,  1625,  1767,  1185,   154,  1185,  1798,
    1631,   100,  1633,   157,   160,   160,   177,  1441,  1442,   157,
    1641,    72,   111,   112,   151,  1482,    18,  1798,   154,  1798,
     154,  1652,   154,  1798,   154,   154,  1215,  1861,  1215,   154,
     154,   154,  1428,  1429,  1665,   104,   154,    13,  1695,   108,
     109,   110,   111,   112,   113,   114,   115,  1267,  2024,    22,
     154,   154,  1845,   154,  1620,   154,  1620,  1058,   151,  1625,
    1660,  1625,   151,  1530,   160,    70,  1798,  1633,   160,  1633,
     160,   132,   160,   154,   176,  1798,  1472,  1473,   154,  1710,
     154,  1082,   154,   152,   153,  1305,  1306,  1307,   151,   176,
     160,   152,  1312,  1313,   160,   156,  1530,   154,   154,   158,
     158,   154,   163,   164,   792,  1659,   154,   206,   158,  1441,
    1442,   154,    88,   154,  1914,   154,  1716,   154,   154,   154,
    1766,  1767,   154,   151,   157,  1756,   157,  1758,   104,   154,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     154,  1695,   154,   154,   154,   154,   154,  1620,   154,   675,
     157,  1695,  1625,  1660,   154,   154,   154,   154,  1951,   154,
    1633,    13,    14,    15,    16,    17,    18,  1798,   158,   151,
    1801,   152,   152,   152,   273,   152,    14,   152,   152,  1810,
      74,   159,    80,  1814,   177,   177,   158,   157,  1695,  2023,
     159,   157,  1659,  1660,   177,   160,   151,  1828,   151,   160,
     158,   157,   154,   154,  1976,   154,   177,  1838,   307,   154,
     154,   154,   216,   312,   154,   151,   158,   158,   157,   318,
    1851,   158,  1853,  1854,  1855,   154,  1660,  2082,  1695,  1860,
    1861,   154,   157,   154,   154,  1801,  1237,  1801,   926,   151,
      90,   152,   177,   152,     1,  1641,   177,     4,  2048,  1716,
    2050,   350,  1441,  1442,  1441,  1442,  1652,    92,   556,   177,
     177,  1695,  1816,   177,  2082,  2082,   152,   177,   151,  1665,
    1271,  2082,   177,   177,   152,  1829,  1277,   152,   154,  1748,
     151,  1912,  1716,  2082,   151,  1829,   158,  1918,   158,  2089,
     151,  1922,   160,   157,  2087,   394,  1927,  1947,  2091,  2092,
     160,  2082,    59,  2082,   157,   151,   157,  2082,   157,   154,
     121,   837,   159,   154,  1710,   159,   614,   151,    75,   151,
     157,  1952,  1829,   154,   157,   154,   154,    84,  1801,   154,
    1976,   151,  2125,   159,   177,   633,   152,   154,   151,   133,
      97,   160,   152,   100,   158,   152,  1912,   104,  1912,  1816,
    2082,   157,   154,   157,   157,   151,  1947,  2150,   151,  2082,
     151,  2154,  1829,  1695,  1995,  2165,   154,   154,  1999,   154,
     154,  2021,    75,  1947,   473,   297,  2169,   157,  2082,  2082,
      75,  2012,   177,   151,   141,   177,   152,   177,   152,   154,
     147,   490,  2023,   150,  2025,  1829,   153,   154,   154,  2082,
     151,   160,   157,   157,   151,   151,   151,  2082,   156,   166,
     154,   154,   154,   154,  1810,    75,   168,   155,  1814,   177,
      75,   151,   159,   151,  1425,   158,  2057,   177,   222,   168,
    2021,   151,  1828,   190,   191,   192,   153,   177,  1992,  1912,
     151,   168,  1838,   969,   168,   202,   203,  2021,  1992,   206,
     159,  2082,  2083,   104,   152,  1851,   555,  1853,  1854,  1855,
      75,   158,  2093,   151,  1860,  2096,  2020,   168,   225,   168,
     768,   153,  2103,   230,   231,   177,  2020,   576,  2083,   157,
    1700,  1482,    75,   177,   151,  1992,   154,   491,   153,   493,
     154,   590,  2123,   159,   251,   154,   154,  1829,  1914,   151,
    2054,  2132,   259,   151,   603,  2136,  1695,  2138,  1695,   303,
    2054,   809,   152,  2020,   177,   154,   273,  2083,  2123,  2083,
     177,   177,  1918,  1748,  1330,  1992,  1922,   759,  2159,   760,
     436,  1927,   761,   831,   832,   718,   635,   763,   762,  2170,
    1203,  1215,  2138,  1695,  2050,  1837,  2079,  2054,  2179,   306,
    2037,  1829,   850,  2020,  1704,   312,  1952,  2123,  1992,  2123,
     659,   318,   319,   320,  2120,  1686,  1686,   666,  1256,  2092,
    2021,   328,  2154,  2020,  1241,    49,    62,   112,   264,  1267,
      13,    14,    15,    16,    17,  1912,  2020,  2054,  1982,  1234,
    1419,   348,   349,   350,   839,   966,   929,   500,  1716,  1995,
     626,     0,   976,  1999,  1609,   784,    -1,   364,   784,   403,
    2083,   368,   910,    -1,   784,    -1,  2012,   539,    -1,   105,
    2054,   919,    -1,   109,    -1,    -1,   112,    -1,   114,  2025,
      -1,    -1,  2048,    -1,  2050,    -1,    -1,    -1,    -1,    72,
    1829,    -1,  1829,    -1,    -1,    -1,   403,    -1,    -1,    -1,
    2123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1659,   104,
    1992,  2057,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,  2089,    -1,   120,    -1,   122,    -1,    -1,
     437,    -1,    -1,   440,    -1,    -1,   241,    -1,  2020,    -1,
     447,   613,    -1,   991,    -1,    -1,    -1,  2093,    -1,   132,
    2096,    -1,    -1,    -1,    -1,  2121,   463,  2103,   153,    -1,
     467,   156,    -1,    -1,   471,    -1,   473,    -1,    -1,   152,
      -1,    -1,  2054,   156,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,    -1,  1259,  1954,    -1,  2132,    -1,    75,    -1,
    2136,    -1,  2138,   537,    -1,    -1,    -1,    -1,    -1,  2165,
      -1,   545,    -1,    -1,   511,    -1,    -1,    -1,    -1,    -1,
      97,    -1,    -1,  2159,    -1,  1291,   523,    -1,   562,    -1,
      -1,    -1,    -1,    -1,  2170,    -1,   262,    -1,    -1,   573,
      -1,    -1,   881,  2179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   548,    -1,   550,   551,    -1,    -1,   554,    -1,   556,
    1801,    -1,   104,  1992,    -1,  1992,   108,   109,   110,   111,
     112,   113,   114,   115,  1340,  1816,   153,    -1,    -1,    -1,
      -1,    -1,    -1,   309,    -1,    -1,  1124,    -1,    -1,    -1,
      -1,  2020,    -1,  2020,    -1,   839,    -1,    -1,   842,    -1,
     104,   940,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   610,    -1,   612,    -1,   614,    -1,   616,
      -1,    -1,   784,    -1,    -1,  2054,   352,  2054,   354,    -1,
     356,    -1,    -1,    -1,    -1,   632,   633,   799,   635,    -1,
      -1,   803,    -1,    -1,    -1,   440,   643,    -1,   225,    -1,
     647,    -1,    -1,   230,   231,    -1,    -1,    -1,    -1,   656,
     694,   456,  1903,    -1,   459,    -1,   700,    -1,   394,   666,
      -1,    -1,   669,   177,   251,   709,    -1,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   681,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   728,    -1,    -1,   694,    -1,  1627,
     697,   698,    -1,   700,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   709,    -1,    -1,   712,   713,   714,    -1,    -1,
      -1,   516,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   306,
      -1,    -1,    -1,   156,    -1,  1501,    -1,    -1,    -1,    -1,
     104,    -1,   319,   320,   108,   109,   110,   111,   112,   113,
     114,   115,  1518,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1298,  1527,    -1,    -1,   490,    -1,    -1,    -1,  1012,    -1,
      -1,   768,    -1,    -1,    -1,    -1,  1020,    -1,  1544,    -1,
      -1,    -1,    -1,  2024,    -1,    -1,    -1,   784,   785,   153,
      -1,    -1,   156,    18,    -1,   792,     4,     5,     6,     7,
       8,     9,    10,    11,    12,  1049,    -1,    -1,  1052,    -1,
      -1,    -1,   809,    -1,   104,   812,    -1,   814,   108,   109,
     110,   111,   112,   113,   114,   115,   116,  1365,  1366,    -1,
     120,    -1,   122,    -1,   831,   832,    61,    62,    63,    64,
      -1,    -1,   568,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     884,    -1,    -1,   850,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   896,   153,    -1,    -1,   900,    -1,  1112,    -1,
     904,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     877,    -1,  2123,   108,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,    -1,   471,    -1,    -1,    -1,  1237,    -1,
      -1,    -1,  1064,    -1,    -1,     1,  1068,    -1,     4,    -1,
    1676,    -1,    -1,    -1,    -1,    -1,    -1,   914,    -1,    -1,
      -1,    -1,   919,  1085,    -1,    -1,    -1,    -1,   925,   926,
    1092,   156,  1271,    -1,   511,    -1,    -1,    -1,    -1,    -1,
      -1,   938,    -1,   940,    -1,    -1,   523,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   951,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    59,    -1,    -1,  1504,    -1,  1734,    -1,
      -1,   548,    -1,  1739,    -1,  1137,    -1,   554,    -1,  1141,
      -1,    78,    -1,  1145,  1750,    -1,    -1,    -1,    84,   784,
     785,    -1,    -1,    -1,   991,    -1,    -1,    -1,    -1,   794,
      -1,    -1,   797,    -1,    -1,    -1,    -1,   104,   104,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,   610,    -1,   612,    -1,    -1,    -1,    -1,
      -1,   137,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,   147,    -1,    -1,   150,    -1,    -1,    -1,    -1,    -1,
      -1,  1058,   857,    -1,    -1,    -1,    -1,  1406,    -1,   864,
     166,  1315,    -1,   868,    -1,   160,    72,   872,  1075,  1323,
     177,    -1,   167,    -1,    -1,  1082,  1425,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   190,   191,    -1,  1863,  1864,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   202,   203,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1124,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   231,  1170,    -1,    -1,    -1,
      -1,    -1,    -1,  1482,   240,    -1,    -1,    -1,    -1,    -1,
    1184,    -1,    -1,    -1,    -1,   251,   152,   153,    -1,    -1,
     256,   257,    -1,   259,    -1,    -1,    -1,  1201,    -1,    -1,
    1167,    -1,   268,    -1,  1208,  1713,    -1,    -1,    -1,  1176,
      -1,    -1,    -1,   279,    -1,    -1,   282,    -1,  1185,    -1,
     286,    -1,    -1,    -1,    -1,   291,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1970,  1449,   302,    -1,   304,    -1,
      -1,  1208,   308,    -1,    -1,    -1,    -1,    -1,  1215,    -1,
      -1,    -1,    -1,  1385,   320,    -1,    -1,  1389,  1766,  1767,
      -1,  1393,   328,    -1,    -1,   812,    -1,   814,    -1,    -1,
    1237,  1238,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   348,  1250,    -1,   351,    -1,    -1,    -1,  1256,
      -1,    -1,    -1,  1058,    -1,    -1,    -1,    -1,   364,    -1,
    1267,    -1,   368,    -1,  2040,    -1,    -1,    -1,    -1,    -1,
    1524,    -1,    65,    66,    67,    68,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1543,
     877,  1298,    -1,    -1,  1548,  1302,    -1,   403,    -1,  1553,
    1554,  1555,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1659,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,  1128,    -1,    -1,  1131,    -1,    -1,    -1,
    1135,   437,    -1,    -1,   104,    72,    -1,  1509,   108,   109,
     110,   111,   112,   113,   114,   115,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   463,  1365,  1366,
     149,   467,   132,   156,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,   174,   152,   153,    -1,    -1,    -1,   176,  1432,  1433,
      -1,  1563,    -1,   163,   164,   132,    -1,    -1,  1570,    -1,
      -1,    -1,  1574,    -1,    -1,    -1,  1413,  1414,  1415,  1416,
      -1,  1418,  1419,    -1,    -1,   152,   153,  1424,  1425,   156,
      72,    -1,    -1,    -1,    -1,    -1,   163,   164,  1976,    -1,
      -1,    -1,    -1,    -1,  1441,  1442,    -1,  1481,    -1,    -1,
      -1,    -1,    -1,    -1,   550,   551,    -1,    -1,    -1,    -1,
     556,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   571,    -1,  1816,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1482,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    13,    14,    15,    16,    17,  1075,    -1,
      -1,    -1,    -1,    -1,  1081,  1082,    -1,  1504,    -1,    -1,
     152,   153,  1756,    -1,  1758,    -1,    -1,    -1,   614,    -1,
     616,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1530,    -1,  1532,   632,   633,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1342,    -1,    84,
      -1,   647,    72,    -1,    -1,    -1,  1351,    -1,    -1,    -1,
     656,    -1,    -1,    -1,    -1,   661,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   669,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   104,   681,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,   694,    -1,
      -1,   697,   698,    -1,   700,    -1,    13,    14,    15,    16,
      17,    -1,   132,   709,    -1,   150,   712,   713,   714,    -1,
     152,    -1,    -1,  1620,  1621,    -1,    -1,    -1,  1625,    -1,
    1627,   166,   152,   153,  1631,    -1,  1633,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    -1,    -1,    -1,    -1,    -1,
      -1,  1685,    -1,    -1,    -1,   190,    -1,    -1,    -1,    -1,
      -1,    -1,  1659,  1660,    -1,    72,    -1,    -1,   203,    -1,
      -1,    -1,   768,  1250,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2024,    -1,    -1,    -1,   785,
      -1,    -1,    -1,    -1,    -1,    -1,   792,   104,  1695,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,   809,    -1,    -1,  1713,    -1,   814,  1716,
      -1,    -1,    -1,    -1,   259,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   831,   832,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,   156,
      -1,  1748,    -1,    -1,   850,    -1,   163,   164,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1766,
    1767,    -1,    13,    14,    15,    16,    17,    -1,  1355,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1783,  1784,    -1,    -1,
      -1,    -1,    -1,   328,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1798,    -1,    -1,  1801,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   348,    -1,    -1,    -1,    -1,   914,  1816,
      -1,    -1,    -1,   919,    -1,    -1,  1621,    -1,    -1,   925,
     926,    72,  1829,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   938,    -1,    -1,   941,    -1,  1424,    -1,    -1,
      -1,   104,   948,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   104,  1861,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,  2043,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,   991,    -1,    -1,     1,    -1,
      -1,     4,   437,    -1,    -1,    -1,  1903,   160,    -1,    -1,
      -1,   152,   153,    -1,   167,  1912,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,    -1,    -1,    -1,    72,    -1,    -1,
      -1,   104,   467,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1947,    -1,    -1,    -1,    -1,    -1,    59,    -1,    88,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1976,
      -1,    84,    -1,    -1,    -1,  1982,    -1,   132,  1783,  1784,
     163,    -1,    -1,    -1,    -1,  1992,    -1,   100,   128,    -1,
      -1,   104,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,   156,    -1,    -1,    -1,   550,   551,    -1,   163,   164,
      -1,   556,    -1,  2020,  2021,    -1,  2023,  2024,  1124,    -1,
      -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,  1620,   147,    -1,    -1,   150,  1625,    -1,
      -1,   154,    -1,    -1,  1631,    -1,  1633,  2054,    -1,    -1,
      -1,    -1,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,  1167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   614,
    1176,    -1,    -1,    -1,    -1,  2082,  2083,   190,    -1,  1185,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   633,   202,
     203,    -1,    -1,   206,    -1,    -1,    -1,    -1,  1903,    -1,
      -1,    -1,  1208,    -1,    -1,  2149,    -1,    -1,    -1,  1215,
      -1,   656,    -1,    -1,    -1,    -1,  2123,  2161,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   240,    -1,    -1,
      -1,    -1,  1238,    -1,    -1,    -1,   681,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   257,    -1,   259,    -1,    -1,    -1,
    1256,    -1,    -1,    -1,    -1,   268,    -1,    -1,    -1,    -1,
     273,  1267,    -1,    -1,    -1,    -1,   279,    -1,    -1,    -1,
    1276,    -1,    -1,  1978,    -1,    -1,   316,  1982,    -1,    -1,
      -1,   294,    -1,    -1,   297,    -1,    -1,    -1,    -1,   302,
      -1,   304,  1298,    -1,   307,   308,  1302,    -1,    -1,   312,
      -1,    -1,    -1,    -1,    -1,   318,    -1,    -1,    -1,    -1,
      -1,  1798,    -1,    -1,  1801,   328,    -1,    -1,  2023,    -1,
      -1,    -1,   104,   768,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   348,    -1,   350,   351,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   792,    -1,    -1,
      -1,   364,    -1,    -1,    -1,   368,    -1,    -1,    -1,  1365,
    1366,    -1,    -1,    -1,   809,    -1,  1187,    -1,    13,    14,
      15,    16,    17,   155,  1861,    -1,    -1,  2082,  2083,   182,
      -1,    -1,    -1,    -1,    -1,    -1,   831,   832,    -1,    -1,
     403,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,   850,    -1,  1413,  1414,  1415,
    1416,  1417,  1418,  1419,    -1,    -1,    -1,    -1,  2123,    -1,
      -1,    -1,    -1,    -1,   437,  1912,   466,    72,   468,    -1,
      -1,    -1,    -1,    -1,    -1,  1441,  1442,   477,   478,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     463,    -1,    -1,    72,   467,    -1,    -1,    -1,    -1,   104,
     473,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,   919,    -1,    -1,    -1,    -1,    -1,
      -1,   926,    -1,    -1,    -1,   104,    -1,   132,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,  1504,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,   163,   164,
      -1,    -1,    -1,    -1,  1530,     1,   539,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,  2023,   550,   551,    -1,
      -1,    -1,   555,   556,   163,   164,   991,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   571,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,   590,    -1,    -1,
      -1,    -1,    -1,    59,   387,    -1,    -1,    -1,   391,   392,
     603,    -1,    -1,    -1,    -1,  2082,  2083,    -1,   401,   402,
     613,   614,    -1,   616,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,   416,   417,    -1,   160,   630,    -1,   632,
     633,  1627,   635,   167,    -1,    -1,    -1,  1633,   104,    -1,
     643,    -1,    -1,    -1,   437,    -1,  2123,    -1,    -1,    -1,
      -1,    -1,    -1,   656,    -1,    -1,   659,    -1,    -1,    -1,
     663,  1472,  1473,   666,  1660,    -1,   669,    -1,   671,    -1,
      -1,   137,    -1,    -1,   467,    -1,    -1,    -1,   681,    -1,
      -1,   147,    -1,    -1,    -1,    -1,    -1,    72,    -1,  1124,
      -1,   694,    -1,    -1,   697,   698,    -1,   700,    -1,  1695,
     166,    -1,    -1,    -1,    -1,    -1,   709,    -1,    -1,   712,
     713,   714,    -1,    -1,    -1,    -1,    -1,  1713,    -1,   104,
    1716,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,   203,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,   104,  1748,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,   768,    -1,   152,   153,    -1,
    1766,  1767,    -1,    -1,   240,    -1,    -1,    -1,   163,   164,
      -1,   784,    -1,    -1,    -1,    -1,    -1,    -1,  1784,   792,
      -1,    -1,    -1,    -1,    -1,   825,   799,    -1,    -1,    -1,
     803,    -1,   268,    -1,    -1,    -1,   809,   160,    -1,   812,
      -1,    -1,    -1,   279,    -1,    -1,   282,    -1,    -1,    -1,
      -1,  1256,    -1,    -1,    -1,    -1,    -1,    -1,   831,   832,
      -1,   297,  1267,  1829,    72,    -1,    -1,    -1,   304,    -1,
      -1,  1652,   308,    -1,    -1,    -1,    -1,   850,    -1,    -1,
      -1,    -1,    -1,    -1,  1665,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1298,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   881,    -1,
      -1,    -1,   348,    -1,    -1,   351,    -1,   917,   918,    -1,
      -1,   921,    -1,    -1,   132,    -1,    -1,    -1,   364,  1710,
      -1,    -1,   368,    -1,    -1,    -1,    -1,   910,    -1,    -1,
      -1,   914,    -1,    -1,   152,   153,   919,    -1,    -1,    -1,
      -1,    -1,   925,   926,    -1,   163,   164,    -1,    -1,    -1,
    1365,  1366,    -1,    -1,    -1,   938,    -1,   940,   941,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,  1947,   745,   746,   747,   748,   749,   750,   751,   752,
     753,   754,   755,   756,   757,   758,   759,   760,   761,   762,
     763,    -1,    -1,    -1,    -1,  1005,    -1,    -1,    -1,    -1,
    1976,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   991,    -1,
      -1,   156,    -1,    -1,    -1,    -1,  1992,    -1,    -1,  1810,
      -1,   467,   102,  1814,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,  1828,    -1,    -1,
      -1,  1051,    -1,    -1,  2020,  2021,    -1,  1838,    -1,    -1,
      -1,    -1,   825,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1851,    -1,  1853,  1854,  1855,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,  2054,    -1,
      -1,  1064,    -1,    -1,    -1,  1068,    -1,    -1,  1098,  1504,
      -1,    -1,  1075,   539,    -1,    -1,    -1,  1107,  1108,  1109,
    1110,    -1,  1085,    -1,    -1,  1115,  1116,  2083,    -1,  1092,
     556,    -1,    -1,    -1,    -1,  1125,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   571,    -1,  1918,    -1,    -1,
      -1,  1922,    -1,    -1,    -1,    -1,  1927,    -1,    -1,    -1,
      -1,  1124,    -1,  1153,    -1,    -1,  1156,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1137,    -1,    -1,    -1,  1141,    -1,
      -1,  1952,  1145,    -1,    -1,    -1,    -1,   613,   614,   104,
     616,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,  1167,    -1,   632,   633,    -1,    -1,
      -1,    -1,    -1,  1176,    -1,    -1,    -1,   643,    -1,    -1,
      -1,    -1,  1185,    -1,  1995,  1215,    -1,    -1,  1999,    -1,
     656,    -1,  1627,    -1,   987,    -1,    -1,    -1,    -1,   992,
     155,  2012,    -1,   669,    -1,  1208,    -1,    -1,    -1,    -1,
    1003,    -1,  1215,    -1,  1244,    -1,    -1,    -1,    -1,    -1,
      -1,  1251,    -1,  1253,  1254,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1237,  1238,  1266,    -1,  1268,    -1,
    1270,    -1,    -1,    -1,    -1,    -1,  2057,  1277,    -1,    -1,
      -1,   104,  1045,  1256,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,  1267,    -1,    -1,   120,  1271,   122,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1713,    -1,
      -1,  1284,  2093,    -1,    -1,  2096,    -1,    -1,    -1,    -1,
      -1,    -1,  2103,    -1,  1297,  1298,    -1,    -1,    -1,  1302,
     153,     1,   768,   156,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   784,    -1,
      -1,  2132,    -1,    -1,    -1,  2136,    -1,  2138,    -1,    -1,
      -1,  1766,  1767,   799,    -1,    -1,    -1,   803,    -1,    -1,
      -1,    -1,  1372,   809,    -1,    -1,   812,    -1,  2159,  1379,
      -1,    -1,    -1,  1383,   153,    -1,    -1,   156,    -1,    59,
      -1,    -1,  1365,  1366,    -1,   831,   832,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,  1409,
      -1,    -1,  1385,    -1,   850,    -1,  1389,    -1,    -1,    -1,
    1393,    -1,  1185,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1406,   104,    -1,    -1,    -1,    -1,    -1,
    1413,  1414,  1415,  1416,  1417,  1418,  1419,    -1,    -1,    -1,
      -1,   104,  1425,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,  1463,    -1,    -1,    -1,   137,  1441,  1442,
      -1,    -1,    -1,    -1,   910,    -1,    -1,   147,   914,   132,
      -1,    -1,   104,   919,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,   166,    -1,    -1,   152,
     153,    -1,   938,   156,  1267,   941,    -1,    -1,  1508,  1482,
     163,   164,    -1,    -1,    -1,  1515,    -1,  1517,    -1,    -1,
      -1,    -1,    -1,   176,  1287,    -1,    -1,    -1,    -1,    -1,
     152,  1504,    -1,   203,    -1,    -1,  1509,    -1,    -1,    -1,
      -1,    -1,  1305,  1306,  1307,    -1,    -1,    -1,    -1,  1312,
    1313,    -1,    -1,    -1,    -1,   991,    -1,  1530,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     240,  1976,    -1,  1336,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1583,    -1,    -1,   132,    -1,    -1,    -1,
    1563,    -1,  1592,    -1,    -1,    -1,    -1,  1570,   268,    -1,
      -1,  1574,    -1,    -1,    -1,    -1,   152,   153,    -1,   279,
    1373,  1374,    -1,   159,    -1,    -1,    -1,   163,   164,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   297,  1064,    -1,
      -1,    -1,  1068,    -1,   304,    -1,    -1,    -1,   308,  1075,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1085,
      -1,    -1,    -1,    -1,  1627,   104,  1092,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   348,    -1,
      -1,   351,    -1,   132,    -1,    -1,  1659,  1660,  1124,    -1,
      -1,    -1,    -1,    -1,   364,    -1,    -1,    -1,   368,    -1,
      -1,  1137,    -1,   152,   153,  1141,    -1,   156,    -1,  1145,
      -1,    -1,    -1,     1,   163,   164,    -1,    -1,    -1,    -1,
      -1,    -1,  1695,    -1,    -1,    -1,    -1,    -1,  1728,  1729,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1713,    -1,  1742,  1716,    -1,    -1,    -1,   104,    -1,  1185,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,    49,    -1,   120,    52,   122,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,  1748,    -1,    -1,    -1,  1215,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1766,  1767,    -1,   153,   467,    -1,   156,
      -1,    -1,  1238,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,  1267,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,  1816,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1829,    -1,    -1,    -1,
      -1,  1297,  1298,    -1,   152,    -1,    -1,   155,   156,   539,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,   556,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1894,    -1,    -1,    -1,    -1,    -1,
      -1,   571,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1365,
    1366,    -1,    -1,    -1,    -1,   132,    -1,  1700,    -1,    -1,
      -1,    -1,    -1,   613,   614,    -1,   616,    -1,    -1,  1385,
      -1,     1,    -1,  1389,     4,   152,   153,  1393,    -1,    -1,
      -1,    -1,   632,   633,    -1,    -1,   163,   164,    -1,    -1,
      -1,    -1,    -1,   643,  1947,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   656,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   669,
      -1,    -1,    -1,  1976,    -1,  1441,  1442,    -1,    -1,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1992,
      -1,    -1,  2022,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,  2020,  2021,    -1,
      -1,  2024,    -1,    -1,   104,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,   132,    -1,    -1,    -1,    -1,  1504,    -1,
    2043,    -1,    -1,  1509,    -1,    -1,    -1,    -1,    -1,    -1,
    2080,  2054,    -1,   152,   153,    -1,    -1,   137,    -1,    -1,
      -1,   141,    -1,    -1,   163,   164,    -1,   147,   768,    -1,
     150,    -1,    -1,    -1,  2104,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   784,    -1,    -1,     1,    -1,  2119,
       4,    -1,    -1,    -1,    -1,     0,    -1,  1563,     3,   799,
      -1,    -1,    -1,   803,  1570,    -1,    -1,    -1,  1574,   809,
     190,    -1,   812,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1914,   202,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   831,   832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,    -1,    -1,
     850,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     240,  1954,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    -1,    -1,    78,    -1,    -1,    -1,   257,    -1,   259,
      -1,    -1,    -1,    -1,   264,    -1,    -1,    -1,   268,    -1,
     104,    -1,    -1,    -1,  1660,    -1,    -1,    -1,    -1,   279,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     910,    -1,    -1,    -1,   914,    -1,    -1,    -1,    -1,   919,
      -1,    -1,   302,   137,   304,    -1,    -1,   141,   308,  1695,
      -1,    -1,    -1,   147,    -1,    -1,   150,    -1,   938,    -1,
      -1,   941,    -1,    -1,    -1,    -1,    -1,  1713,   328,    -1,
      -1,    -1,    -1,    -1,    -1,  2048,    -1,  2050,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   190,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   202,    -1,
      -1,   991,    -1,    -1,    -1,    -1,  2089,    -1,    -1,    -1,
    1766,  1767,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   403,    -1,    -1,   240,    -1,  2121,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   257,    -1,   259,    -1,    -1,    -1,    -1,
     264,    -1,    -1,   258,   268,    -1,    -1,   437,    -1,    -1,
      -1,    -1,    -1,  1829,  1064,   279,    -1,    -1,  1068,    -1,
      -1,    -1,  2165,    -1,    -1,  1075,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   463,    -1,  1085,    -1,    -1,   302,    -1,
     304,    -1,  1092,   298,   308,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,   310,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,   328,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   327,    -1,  1124,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1137,    -1,    -1,
      -1,  1141,   347,    -1,    -1,  1145,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     550,   551,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1947,    -1,    -1,    -1,  1185,    -1,    84,    -1,   403,
      -1,   571,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   404,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
    1976,    -1,    -1,    -1,    -1,  1215,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   437,    -1,    -1,  1992,    -1,    -1,    -1,
     435,    -1,    -1,    -1,    -1,     4,   616,    -1,  1238,    -1,
     137,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,   463,
     147,    -1,    -1,   150,  2020,  2021,   461,    -1,    -1,    -1,
     465,    -1,    -1,    -1,    -1,    -1,    -1,  1267,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2043,    -1,   484,
      -1,    -1,    -1,   488,   489,    -1,    -1,   492,  2054,    -1,
      -1,    -1,    -1,   190,    -1,    -1,    -1,  1297,  1298,    -1,
      -1,   681,   507,   508,    -1,   202,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   694,    84,    -1,   697,   698,    -1,
     700,    -1,    -1,    -1,    -1,   530,    -1,    -1,    -1,   709,
      -1,    -1,   712,   713,   714,    -1,   550,   551,    -1,    -1,
      -1,    -1,    -1,   240,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   571,    -1,    -1,
     257,    48,   259,    -1,    -1,  1365,  1366,   264,    -1,    -1,
      -1,   268,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   150,   279,    -1,    -1,  1385,    -1,    -1,    75,  1389,
      -1,    -1,    -1,  1393,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   606,   616,    -1,    -1,   302,    -1,   304,    -1,    -1,
      -1,   308,   792,    -1,   619,    -1,    -1,    -1,    -1,    -1,
      -1,   190,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   328,    -1,   202,   121,    -1,    -1,    -1,    -1,   644,
      -1,  1441,  1442,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   681,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     694,   168,    -1,   697,   698,    -1,   700,    -1,   257,    -1,
     259,    -1,    -1,    -1,    -1,   709,    -1,    -1,   712,   713,
     714,    -1,    -1,    -1,  1504,   192,   403,    -1,    -1,  1509,
     715,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   302,   914,    -1,    -1,    -1,    -1,    -1,
     437,    -1,    -1,   230,    -1,   925,   926,   234,    -1,    -1,
     237,   238,    -1,    -1,   241,    -1,    -1,   244,   245,   328,
     247,    -1,   249,  1563,    -1,    -1,   463,    -1,    -1,    -1,
    1570,    -1,     3,    -1,  1574,    -1,    -1,    -1,   792,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,   834,
      -1,    -1,   319,    -1,   403,   322,    -1,    -1,   843,    -1,
      -1,    72,    -1,    -1,   849,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   550,   551,    -1,    -1,    -1,   345,   346,
    1660,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   437,    -1,
      -1,    -1,    -1,   360,   571,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   894,
      -1,    -1,    -1,   898,   463,  1695,    -1,   902,    -1,    -1,
     914,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   925,   926,  1713,    -1,    -1,    -1,    -1,   923,   616,
      -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1766,  1767,    -1,   456,
      -1,    -1,    -1,    -1,    -1,    -1,   981,    -1,    -1,    -1,
      -1,   550,   551,    -1,   681,    -1,    -1,  1167,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1176,   694,    -1,    -1,
     697,   698,    -1,   700,    -1,  1185,    -1,    -1,    -1,    -1,
      -1,    -1,   709,    -1,    -1,   712,   713,   714,    -1,    -1,
      -1,    -1,    -1,    -1,   511,    -1,    -1,    -1,  1208,  1829,
      -1,    -1,    -1,    -1,    -1,  1215,    -1,    -1,    -1,    -1,
     527,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1238,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1256,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   792,  1101,    -1,  1103,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   681,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1302,   610,    -1,   694,    -1,    -1,   697,   698,
      -1,   700,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     709,    -1,    -1,   712,   713,   714,    -1,  1947,    -1,    -1,
      -1,    -1,    -1,  1167,    -1,    -1,    -1,    -1,  1163,    -1,
      -1,    -1,  1176,  1168,   651,   652,    -1,    -1,    -1,    -1,
      -1,  1185,    -1,    -1,    -1,    -1,  1976,   664,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1992,    -1,  1208,    -1,    -1,    -1,    -1,    -1,
      -1,  1215,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   914,    -1,    -1,
    2020,  2021,    -1,   792,  1238,    -1,    -1,    -1,   925,   926,
      -1,    -1,    -1,  1413,  1414,  1415,  1416,  1417,  1418,  1419,
      -1,    -1,  1256,  2043,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2054,    -1,    -1,    -1,    -1,    -1,
    1265,  1441,  1442,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1292,  1302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   788,   789,    -1,    -1,    -1,    -1,   794,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   815,    -1,
      -1,   818,   819,    -1,   821,    -1,   823,   824,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1530,    -1,    -1,    -1,    -1,    -1,   925,   926,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   864,    -1,    -1,
      -1,   868,    -1,    -1,    -1,   872,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1413,
    1414,  1415,  1416,  1417,  1418,  1419,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    56,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1441,  1442,    -1,
      -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1627,   935,   936,
      -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   949,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1167,   202,   203,    -1,    -1,    -1,    -1,    -1,    -1,  1176,
    1660,    -1,    -1,  1488,    -1,    -1,  1491,    -1,  1185,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,   234,    -1,   145,    -1,    -1,    -1,    -1,
     241,  1208,    -1,    -1,    -1,  1695,  1530,    -1,  1215,    -1,
     160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1716,    -1,    -1,    -1,
      -1,  1238,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1256,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1748,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   221,    -1,  1588,  1589,    -1,    -1,    -1,    -1,    -1,
      -1,   322,    -1,    -1,  1081,    -1,    -1,    -1,  1167,    -1,
      -1,    -1,    -1,    -1,    -1,  1302,    -1,  1176,    -1,  1614,
      -1,    -1,    -1,  1627,    -1,    -1,    -1,   348,   349,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   269,
     270,    -1,    -1,    -1,    -1,  1122,    -1,   368,    -1,  1208,
     280,  1128,    -1,    -1,  1131,    -1,  1660,    -1,  1135,  1829,
      -1,    -1,    -1,    -1,    -1,   295,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1695,    -1,    -1,    -1,    -1,    -1,  1256,    -1,   329,
      -1,    -1,    -1,    -1,    -1,    -1,   336,   337,    -1,    -1,
      -1,   341,  1716,    -1,    -1,    -1,    -1,    -1,    -1,   440,
      -1,    -1,  1717,  1718,    -1,    -1,  1413,  1414,  1415,  1416,
    1417,  1418,  1419,    -1,    -1,   456,   457,    -1,   459,   460,
      -1,    -1,    -1,  1302,  1748,    -1,   467,    -1,    -1,   379,
     471,    -1,   382,    -1,  1441,  1442,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   512,    -1,    -1,    -1,   516,  1273,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1282,  1283,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1992,    -1,    -1,  1829,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,   556,    -1,   154,    -1,    -1,
     470,    -1,    -1,  1530,    -1,    -1,    -1,    -1,  1843,   166,
    2020,    -1,   482,   483,  1413,  1414,  1415,  1416,  1417,  1418,
    1419,    -1,    -1,    -1,    -1,  1342,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   190,  1351,    -1,    -1,  1354,    -1,  1356,
    1357,    -1,  1877,    -1,  2054,    -1,   203,    -1,    -1,   206,
     611,    -1,    -1,   614,    -1,    -1,    -1,  1892,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   632,   633,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1916,   643,    -1,  1401,    -1,   647,    -1,    -1,    -1,
      -1,    -1,    -1,   654,    -1,   656,    -1,    -1,    -1,    -1,
    1627,    -1,   259,    -1,    -1,    -1,    -1,    -1,  1943,    -1,
      -1,  1946,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1530,    -1,  1660,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,    -1,    -1,    -1,    -1,  1992,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   626,    -1,    -1,    -1,
      -1,   318,    -1,    -1,    -1,    -1,  1483,    -1,  1695,    -1,
      -1,   328,    -1,    -1,    -1,    -1,  2020,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1716,
      -1,   348,    -1,   350,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   403,   676,   768,    -1,    -1,
    2054,    -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,
      -1,  1748,    -1,   784,   785,    -1,    -1,   134,  1627,   136,
      -1,    -1,    -1,   794,   795,    -1,   797,   798,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   403,    -1,   809,    -1,
      -1,   812,  1569,   814,   815,    -1,    -1,    -1,    -1,    -1,
     821,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     831,   832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     437,  1598,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   850,
      -1,    -1,   853,    -1,    -1,    -1,   857,    -1,    -1,    -1,
      -1,    -1,  1829,   864,   865,    -1,    -1,   868,   869,    -1,
     467,   872,   873,    -1,  1631,    -1,   473,  1716,    -1,   880,
    1637,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     237,   238,    -1,    -1,   241,    -1,    -1,   244,   245,    -1,
     247,    -1,   249,    -1,    -1,    -1,    -1,    -1,    -1,  1748,
      -1,    -1,   550,   551,    -1,    -1,   826,    -1,   919,   920,
      -1,    -1,    -1,   833,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     951,    -1,    -1,   550,   551,  1712,    -1,    -1,    -1,   556,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     991,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   345,   346,
      -1,    -1,    -1,   913,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   360,    -1,    -1,    -1,   614,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1992,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1790,  1791,    -1,   633,    -1,   635,    -1,
      -1,  1798,    -1,    -1,    -1,    -1,  1803,    -1,    -1,    -1,
      -1,    -1,    -1,  2020,    -1,    -1,   694,  1058,    -1,   656,
      -1,    -1,   700,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   709,    -1,    -1,  1075,  1076,    -1,    -1,    -1,    -1,
      -1,  1082,    -1,    -1,   681,    -1,    -1,  2054,    -1,    -1,
     728,    -1,    -1,    -1,    -1,    -1,    -1,   694,    -1,    -1,
     697,   698,    -1,   700,    -1,    -1,    -1,    -1,    -1,   456,
      -1,    -1,   709,    -1,    -1,   712,   713,   714,    -1,    -1,
      -1,    -1,    -1,  1124,    -1,    -1,   764,  1128,  1129,    -1,
    1131,  1132,    -1,    -1,  1135,  1136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1905,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   768,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     527,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   792,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   809,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1978,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   831,   832,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   850,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1267,    -1,    -1,    -1,
      -1,    -1,  1273,  1274,    -1,    -1,    -1,    -1,    -1,    -1,
    1190,  1191,  1192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1298,    -1,    -1,
      -1,    -1,    -1,    -1,   651,   652,    -1,    -1,    -1,    -1,
      -1,  1221,    -1,    -1,    -1,    -1,    -1,   664,    -1,    -1,
      -1,    -1,   919,    -1,    -1,  2082,    -1,    -1,    -1,   926,
      -1,  1241,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1342,  1343,   940,    -1,    -1,    -1,    -1,    -1,    -1,
    1351,  1352,    -1,  1354,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1365,  1366,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1285,    -1,   190,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     203,    -1,    -1,    -1,   991,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   216,    -1,   218,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   788,   789,    -1,    -1,    -1,     5,   794,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   815,    -1,
      -1,   818,   819,    -1,   821,    -1,   823,   824,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,
      -1,    -1,    -1,  1504,   317,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,   864,  1428,  1429,
      -1,   868,    -1,    -1,    -1,   872,    -1,  1124,    -1,    -1,
      -1,  1532,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
    1208,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,  1176,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   935,   936,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,   949,   162,   163,   164,   165,   166,   167,   168,
      -1,  1208,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1621,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1637,    -1,    -1,    -1,
    1237,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1256,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1267,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     493,    -1,    -1,    -1,    -1,    -1,   499,    -1,    -1,    -1,
      -1,   504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1298,    -1,    -1,    -1,  1302,    -1,    -1,    -1,    -1,
      -1,    -1,  1713,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1641,    -1,    -1,  1081,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1766,  1767,    -1,  1365,  1366,
      -1,    -1,    -1,    -1,    -1,  1122,    -1,    -1,    -1,    -1,
      -1,  1128,  1783,  1784,  1131,    -1,    -1,    -1,  1135,    -1,
      -1,   604,    -1,    -1,    -1,    -1,    -1,    -1,  1799,    -1,
    1438,    -1,    -1,  1441,  1442,    -1,    -1,    -1,    -1,  1447,
      -1,    -1,    -1,  1451,    -1,  1453,  1413,  1414,  1415,    -1,
     633,  1418,  1419,    -1,    -1,    -1,    -1,    -1,  1425,    -1,
      -1,    -1,    -1,   646,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,  1482,    -1,    -1,    51,   702,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,  1903,    -1,    -1,    -1,    -1,  1504,    -1,    72,
    1911,    -1,    -1,   726,   727,    -1,    -1,   730,    -1,   732,
      -1,    -1,    -1,    -1,    -1,   738,  1273,   740,   741,    -1,
      -1,    -1,    -1,    -1,    -1,  1282,  1283,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
    1860,    -1,    -1,    -1,    -1,   768,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   781,  1607,
      -1,    -1,    -1,    -1,    -1,  1976,    -1,  1978,  1979,   792,
      -1,  1982,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   806,    -1,  1342,   809,   160,    -1,    -1,
      -1,    -1,    -1,    -1,  1351,    -1,    -1,  1354,    -1,  1356,
    1357,    -1,    -1,    -1,    -1,    -1,    -1,  1655,    -1,    -1,
      -1,    -1,  2023,   836,    -1,    -1,   839,    -1,    -1,    -1,
    1627,    -1,  1670,  1671,    -1,    -1,    -1,    -1,    -1,   852,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1401,    -1,    -1,  1695,    -1,    -1,
      -1,    -1,  1659,  1701,    -1,    -1,    -1,    -1,   881,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2082,  2083,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,   926,    -1,    -1,  1713,    -1,    -1,    -1,
      -1,    -1,  2123,    -1,    -1,    -1,    -1,   940,   941,    -1,
      -1,    -1,    -1,    -1,    -1,   948,  1483,    -1,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,
      -1,  1748,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   977,    72,    73,    -1,    -1,  1766,
    1767,    -1,    -1,    -1,  1812,    -1,    -1,    -1,   991,    -1,
      -1,    -1,    -1,  1821,    -1,  1823,   999,    -1,  1826,  1827,
      -1,  1829,    -1,  1006,   101,   102,  1834,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,  1816,
      -1,    -1,  1569,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1052,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,  1598,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1933,    -1,    -1,    -1,    -1,
    1938,  1939,    -1,    -1,    73,    -1,  1119,    -1,  1121,    -1,
    1123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1958,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
    1947,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,  2000,   132,  2002,  1712,    -1,  2005,  2006,    -1,
      -1,    -1,  2010,  2011,    -1,    -1,    -1,    -1,    -1,  1976,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,  1202,
    1203,   160,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2021,    -1,    -1,  2024,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2075,  2076,  2077,
      -1,    -1,    -1,  1790,  1791,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1803,    -1,  1271,    -1,
      -1,    -1,    -1,    -1,  1277,    -1,    -1,    -1,    -1,    -1,
      -1,  2109,  2110,  2111,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1298,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1315,    -1,    -1,    -1,    -1,    -1,     4,     5,
       6,     7,     8,     9,    10,    11,    12,  1330,    -1,    -1,
    1333,    -1,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,  1905,    55,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1405,  1406,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1431,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1978,    -1,    -1,    -1,    -1,  1449,    -1,    -1,  1452,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,   177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1513,  1514,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      -1,  1524,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1543,    71,  1545,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    -1,    99,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,   152,    -1,  1627,   155,   156,    -1,    -1,  1632,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,   177,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      20,    18,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,  1690,    -1,    -1,
      -1,    51,    49,    53,    -1,    52,    -1,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    71,    -1,    73,    74,    -1,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    -1,    99,  1746,   101,   102,  1749,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,     1,   124,   125,    -1,
      -1,    -1,    -1,  1776,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
     177,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    73,    74,
      -1,    76,    -1,    -1,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    -1,    99,    -1,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,    -1,   124,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1955,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    71,    72,    73,    74,    -1,    76,    -1,    -1,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    -1,    99,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    69,    -1,    71,    72,    73,    74,
      -1,    76,    -1,    -1,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    -1,    99,    -1,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,    -1,   124,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   177,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   146,   147,   148,    -1,
      -1,    -1,   152,   153,   154,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    -1,   124,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   146,   147,   148,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   177,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,   154,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,     3,
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
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,     1,    -1,     3,     4,     5,
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
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,   119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,    -1,   163,   164,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
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
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    77,    78,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,   155,   156,    -1,     3,    -1,     5,    -1,
      -1,   163,   164,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,   155,   156,
      -1,     3,    -1,     5,    -1,    -1,   163,   164,    10,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,   155,   156,    -1,     3,    -1,     5,    -1,
      -1,   163,   164,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,   155,   156,
      -1,     3,    -1,     5,    -1,    -1,   163,   164,    10,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,   154,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,    -1,   163,   164,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
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
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,   154,   155,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,   154,
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
     154,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
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
     132,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,   132,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
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
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    71,    -1,    73,
      74,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    -1,    -1,    93,
      94,    95,    96,    97,    -1,    99,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,    18,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      49,   155,   156,    52,    -1,    54,   160,    56,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,   177,    73,    74,    -1,    76,    -1,    -1,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    -1,    -1,    93,    94,    95,    96,    97,    -1,
      99,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
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
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,   107,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,   155,    51,    -1,    53,    -1,   160,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,   155,    51,    52,    53,
      54,   160,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    -1,    -1,    -1,    -1,    90,    -1,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,
     164,   165,   166,   167,   168,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
     168,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,   154,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    13,    14,    15,
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
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
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
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    13,    14,    15,    16,    17,
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
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,   159,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,   155,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,   155,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
     160,    -1,    -1,   163,   164,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,   154,
     155,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
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
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,   155,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,   155,    57,    58,    59,    60,    61,    62,    63,    64,
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
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,    13,    14,    15,    16,    17,    18,    -1,    20,
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
      -1,    -1,   163,   164,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    13,    14,    15,    16,
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
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    13,    14,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,    13,    14,    15,    16,    17,    18,    -1,    20,
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
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    72,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,   106,   107,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    13,    14,    15,    16,
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
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    13,    14,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,    13,    14,    15,    16,    17,
     163,   164,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      13,    14,    15,    16,    17,   163,   164,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    72,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,   106,   107,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,   106,   107,    -1,    -1,    -1,    -1,
     163,   164,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    -1,
      -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,    -1,
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
     168,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     113,   114,   115,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,   155,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,   106,   107,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,   155,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    49,    -1,    -1,    52,    -1,    54,
     132,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   146,   147,   148,    -1,    73,    -1,
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
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,
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
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,   159,    -1,    -1,   162,   163,   164,   165,   166,   167,
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
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
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
     152,    -1,   154,   155,   156,    -1,    -1,    -1,    -1,    -1,
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
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,     4,
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
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,    72,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72
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
     224,    75,   108,   256,   258,    75,     1,   226,   418,   398,
     179,   179,   151,   360,   360,   157,   154,   180,   180,   157,
     157,   151,   485,   358,   160,   485,   151,   180,   154,   217,
     189,   217,   485,   151,   157,   157,   194,   194,   154,   154,
     154,   157,   158,   134,   365,   134,   157,   157,   217,   177,
     471,   472,   473,   309,   470,   158,   177,   418,   418,   177,
     154,   424,   418,   226,    77,    78,   160,   238,   239,   240,
     154,   224,    75,   226,   224,   153,   224,    75,   177,   106,
     153,   224,   225,   246,   153,   224,   226,   245,   248,   248,
     177,   224,   151,   160,   240,   226,   152,   179,   177,   185,
     154,   159,   154,   154,   158,   159,   154,   226,   152,   226,
     226,   226,   373,   415,   485,   485,   157,   157,   151,   160,
     360,   151,   151,   151,   157,   157,   179,   180,   154,   154,
     154,   470,   418,   348,     1,   216,   236,   237,   416,     1,
     159,     1,   179,   226,   238,    75,   177,   154,   226,    75,
     177,   168,   168,   226,   225,   248,   248,   177,   106,   224,
     168,   168,    75,   153,   224,   153,   224,   225,   177,     1,
     179,   179,   272,   307,   309,   479,   159,   177,   156,   185,
     277,   278,   279,   226,   201,   191,   224,   257,   151,   151,
     360,   485,   368,   152,   418,   459,   462,   350,   134,     1,
     158,   159,   151,   282,   283,   289,   226,    75,   177,   226,
     224,   153,   153,   224,   153,   224,   153,   224,   225,   153,
     224,   153,   224,   226,   168,   168,   168,   168,   151,   282,
     272,   180,   152,   199,   415,   470,   183,   159,   104,   152,
     154,   159,   158,    75,   154,   154,    75,   253,   485,   151,
     366,   216,   236,   239,   241,   242,   289,   226,   168,   168,
     168,   168,   153,   153,   224,   153,   224,   153,   224,   241,
     180,   177,   269,   309,   277,   157,   216,   177,   277,   279,
     226,   226,    75,   151,   154,   226,   231,   180,   239,   153,
     153,   224,   153,   224,   153,   224,   180,   269,   215,   154,
     159,   185,   154,   154,   159,   226,     1,   226,   151,   231,
     151,   154,   228,   185,   280,   152,   177,   280,   228,   158,
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
     264,   265,   265,   265,   266,   267,   267,   267,   268,   268,
     269,   269,   270,   270,   270,   270,   271,   272,   272,   272,
     272,   272,   273,   274,   274,   275,   275,   275,   275,   275,
     276,   276,   277,   277,   278,   278,   279,   279,   280,   280,
     280,   281,   281,   282,   282,   283,   283,   284,   284,   285,
     285,   286,   286,   287,   287,   288,   288,   289,   289,   289,
     290,   290,   291,   291,   291,   291,   291,   292,   292,   292,
     293,   293,   293,   294,   294,   294,   294,   294,   295,   295,
     296,   296,   297,   297,   297,   298,   298,   298,   298,   298,
     299,   299,   300,   300,   300,   300,   301,   301,   301,   301,
     301,   302,   302,   303,   303,   303,   303,   304,   304,   304,
     305,   305,   305,   306,   306,   306,   307,   307,   307,   308,
     308,   309,   309,   310,   310,   311,   311,   311,   311,   311,
     312,   313,   313,   313,   314,   314,   315,   315,   315,   315,
     315,   315,   315,   315,   315,   316,   316,   316,   316,   316,
     316,   316,   316,   316,   316,   316,   316,   316,   316,   316,
     316,   316,   316,   316,   316,   316,   316,   316,   316,   316,
     316,   316,   316,   317,   317,   318,   319,   319,   320,   320,
     320,   320,   320,   321,   321,   322,   322,   322,   322,   323,
     323,   323,   323,   323,   323,   324,   324,   324,   324,   325,
     326,   325,   325,   327,   327,   327,   327,   328,   328,   328,
     329,   329,   329,   329,   330,   330,   330,   331,   331,   331,
     331,   331,   331,   332,   332,   332,   333,   333,   334,   334,
     336,   335,   337,   335,   338,   335,   339,   335,   335,   340,
     340,   341,   341,   342,   342,   343,   343,   343,   344,   344,
     344,   344,   344,   344,   344,   344,   345,   345,   346,   346,
     346,   346,   346,   346,   346,   346,   346,   346,   346,   346,
     347,   347,   347,   348,   348,   348,   348,   349,   349,   349,
     350,   351,   351,   352,   352,   353,   353,   354,   355,   355,
     356,   355,   355,   355,   355,   355,   355,   357,   355,   355,
     355,   355,   355,   358,   358,   359,   359,   360,   360,   360,
     360,   361,   361,   362,   362,   362,   363,   363,   363,   363,
     363,   363,   363,   364,   364,   364,   364,   365,   365,   366,
     366,   366,   366,   367,   367,   367,   367,   368,   368,   368,
     368,   368,   369,   369,   369,   369,   369,   370,   370,   371,
     371,   372,   372,   373,   373,   373,   374,   374,   374,   375,
     375,   376,   376,   376,   376,   377,   377,   378,   378,   378,
     378,   378,   379,   379,   380,   380,   381,   381,   381,   381,
     381,   382,   382,   383,   383,   385,   384,   386,   384,   384,
     384,   387,   387,   387,   387,   388,   388,   388,   388,   389,
     389,   390,   390,   391,   391,   392,   392,   392,   392,   393,
     393,   393,   394,   394,   395,   395,   396,   396,   396,   396,
     397,   397,   398,   398,   399,   399,   399,   400,   400,   401,
     401,   402,   402,   403,   403,   404,   405,   406,   406,   406,
     406,   406,   406,   406,   406,   406,   406,   406,   407,   406,
     408,   406,   409,   406,   410,   406,   411,   406,   412,   412,
     412,   413,   413,   414,   414,   414,   414,   414,   414,   414,
     414,   414,   414,   415,   415,   415,   415,   416,   417,   417,
     418,   418,   419,   419,   420,   421,   421,   422,   422,   422,
     423,   423,   423,   423,   423,   423,   424,   424,   425,   425,
     425,   425,   426,   426,   426,   426,   427,   427,   427,   427,
     427,   427,   427,   428,   428,   428,   428,   429,   429,   429,
     430,   430,   430,   430,   430,   431,   431,   431,   431,   432,
     432,   432,   432,   432,   432,   433,   433,   433,   434,   434,
     434,   434,   434,   435,   435,   435,   435,   436,   436,   436,
     436,   436,   436,   437,   437,   438,   438,   438,   438,   439,
     439,   439,   439,   440,   440,   440,   440,   440,   440,   440,
     441,   441,   441,   441,   442,   442,   442,   443,   443,   443,
     443,   443,   444,   444,   444,   444,   445,   445,   445,   445,
     445,   445,   446,   446,   446,   446,   446,   447,   447,   447,
     448,   448,   448,   448,   449,   449,   449,   450,   450,   450,
     450,   450,   451,   451,   452,   452,   452,   453,   453,   454,
     454,   455,   455,   455,   456,   456,   456,   456,   456,   457,
     457,   457,   457,   458,   458,   458,   459,   459,   459,   459,
     459,   460,   460,   460,   460,   460,   460,   461,   461,   462,
     462,   462,   462,   463,   463,   464,   464,   464,   464,   465,
     465,   465,   465,   465,   466,   466,   466,   466,   467,   467,
     467,   468,   468,   468,   469,   469,   469,   469,   469,   469,
     470,   470,   470,   471,   471,   471,   471,   471,   472,   472,
     472,   472,   473,   473,   474,   474,   474,   475,   475,   476,
     476,   476,   476,   476,   476,   477,   477,   477,   477,   477,
     477,   477,   477,   477,   477,   478,   478,   478,   478,   479,
     479,   479,   480,   480,   481,   481,   481,   481,   481,   481,
     482,   482,   482,   482,   482,   482,   483,   483,   483,   484,
     484,   485,   485,   486,   486
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
       3,     1,     3,     5,     1,     3,     3,     4,     8,     9,
       0,     2,     1,     1,     1,     1,     2,     1,     2,     2,
       2,     1,     3,     1,     1,     6,     8,    10,    12,    14,
       0,     1,     0,     1,     1,     3,     4,     7,     0,     1,
       3,     1,     3,     0,     1,     1,     2,     0,     1,     2,
       3,     0,     1,     3,     4,     1,     3,     2,     2,     1,
       7,     5,     1,     1,     1,     1,     1,     2,     3,     6,
       3,     3,     4,     1,     2,     2,     3,     8,     8,     8,
       5,     9,     2,     2,     5,     3,     3,     4,     3,     4,
       4,     5,     2,     1,     1,     1,     3,     3,     2,     4,
       6,     1,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       4,     1,     2,     3,     1,     2,     1,     1,     1,     1,
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
       1,     2,     3,     1,     2,     0,     1,     2,     6,     7,
       0,     9,     8,     9,    10,     8,     9,     0,    13,    11,
      12,    11,     1,     0,     1,     3,     3,     3,     2,     5,
       5,     1,     1,     0,     2,     5,     0,     1,     1,     1,
       5,     5,     5,     1,     5,     5,     9,     1,     5,     0,
       1,     1,     3,     1,     1,     3,     3,     1,     3,     3,
       4,     1,     1,     1,     1,     2,     1,     3,     3,     2,
       3,     1,     3,     1,     1,     1,     1,     1,     2,     1,
       1,     0,     2,     2,     4,     1,     4,     0,     1,     2,
       3,     4,     2,     2,     1,     2,     2,     5,     5,     7,
       6,     1,     3,     0,     2,     0,     5,     0,     5,     3,
       1,     0,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     2,     5,     6,     1,     1,     3,     3,     2,
       3,     3,     2,     4,     1,     4,     7,     5,    10,     8,
       1,     4,     2,     2,     1,     1,     5,     2,     5,     0,
       1,     3,     4,     0,     1,     0,     0,     1,     1,     2,
       2,     2,     2,     2,     2,     1,     2,     5,     0,     6,
       0,     8,     0,     7,     0,     7,     0,     8,     1,     2,
       3,     0,     5,     3,     4,     4,     4,     4,     5,     5,
       5,     5,     6,     1,     1,     1,     1,     3,     0,     5,
       0,     1,     1,     2,     6,     1,     3,     0,     1,     4,
       1,     1,     1,     1,     1,     1,     1,     3,     2,     1,
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
       4,     4,     3,     3,     6,     3,     2,     3,     7,     5,
       1,     1,     1,     3,     3,     3,     5,     1,     1,     5,
       5,     6,     6,     0,     1,     1,     3,     2,     2,     1,
       2,     2,     3,     4,     1,     4,     4,     3,     3,     6,
       3,     1,     2,     1,     2,     6,     5,     6,     7,     7,
       1,     2,     2,     1,     2,     2,     3,     4,     1,     4,
       4,     3,     6,     3,     1,     1,     2,     1,     1,     2,
       3,     2,     3,     2,     3,     3,     2,     4,     3,     2,
       3,     2,     4,     3,     2,     6,     6,     6,     7,     1,
       2,     1,     1,     1,     2,     3,     2,     3,     2,     3,
       3,     4,     2,     3,     4,     2,     5,     6,     7,     6,
       6,     0,     1,     0,     2
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
#line 8043 "Parser/parser.cc"
    break;

  case 3:
#line 611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8049 "Parser/parser.cc"
    break;

  case 4:
#line 618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8055 "Parser/parser.cc"
    break;

  case 5:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8061 "Parser/parser.cc"
    break;

  case 6:
#line 620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8067 "Parser/parser.cc"
    break;

  case 7:
#line 621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8073 "Parser/parser.cc"
    break;

  case 8:
#line 622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8079 "Parser/parser.cc"
    break;

  case 20:
#line 644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8085 "Parser/parser.cc"
    break;

  case 21:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8091 "Parser/parser.cc"
    break;

  case 22:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8097 "Parser/parser.cc"
    break;

  case 23:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8107 "Parser/parser.cc"
    break;

  case 24:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8113 "Parser/parser.cc"
    break;

  case 25:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8119 "Parser/parser.cc"
    break;

  case 26:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8125 "Parser/parser.cc"
    break;

  case 28:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8131 "Parser/parser.cc"
    break;

  case 29:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8137 "Parser/parser.cc"
    break;

  case 30:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, (yyvsp[-2].decl), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8143 "Parser/parser.cc"
    break;

  case 31:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8149 "Parser/parser.cc"
    break;

  case 32:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8159 "Parser/parser.cc"
    break;

  case 33:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.expr) = nullptr; }
#line 8165 "Parser/parser.cc"
    break;

  case 34:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8171 "Parser/parser.cc"
    break;

  case 35:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8177 "Parser/parser.cc"
    break;

  case 36:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8183 "Parser/parser.cc"
    break;

  case 37:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8189 "Parser/parser.cc"
    break;

  case 38:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8195 "Parser/parser.cc"
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
#line 8207 "Parser/parser.cc"
    break;

  case 41:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8216 "Parser/parser.cc"
    break;

  case 42:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8222 "Parser/parser.cc"
    break;

  case 44:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) )) ) ); }
#line 8228 "Parser/parser.cc"
    break;

  case 45:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8234 "Parser/parser.cc"
    break;

  case 46:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8240 "Parser/parser.cc"
    break;

  case 47:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8246 "Parser/parser.cc"
    break;

  case 48:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8256 "Parser/parser.cc"
    break;

  case 49:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8262 "Parser/parser.cc"
    break;

  case 50:
#line 754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 8269 "Parser/parser.cc"
    break;

  case 51:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8275 "Parser/parser.cc"
    break;

  case 52:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8281 "Parser/parser.cc"
    break;

  case 53:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8287 "Parser/parser.cc"
    break;

  case 54:
#line 763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8293 "Parser/parser.cc"
    break;

  case 55:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8299 "Parser/parser.cc"
    break;

  case 56:
#line 767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8305 "Parser/parser.cc"
    break;

  case 57:
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8311 "Parser/parser.cc"
    break;

  case 58:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8317 "Parser/parser.cc"
    break;

  case 59:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8323 "Parser/parser.cc"
    break;

  case 60:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8329 "Parser/parser.cc"
    break;

  case 61:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8335 "Parser/parser.cc"
    break;

  case 62:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8341 "Parser/parser.cc"
    break;

  case 63:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8347 "Parser/parser.cc"
    break;

  case 64:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8353 "Parser/parser.cc"
    break;

  case 65:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8359 "Parser/parser.cc"
    break;

  case 66:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8369 "Parser/parser.cc"
    break;

  case 67:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8375 "Parser/parser.cc"
    break;

  case 70:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8381 "Parser/parser.cc"
    break;

  case 71:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8387 "Parser/parser.cc"
    break;

  case 74:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8393 "Parser/parser.cc"
    break;

  case 76:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8399 "Parser/parser.cc"
    break;

  case 77:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8405 "Parser/parser.cc"
    break;

  case 78:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8411 "Parser/parser.cc"
    break;

  case 79:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8417 "Parser/parser.cc"
    break;

  case 80:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8423 "Parser/parser.cc"
    break;

  case 81:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8429 "Parser/parser.cc"
    break;

  case 82:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8435 "Parser/parser.cc"
    break;

  case 83:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8441 "Parser/parser.cc"
    break;

  case 84:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8449 "Parser/parser.cc"
    break;

  case 85:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8455 "Parser/parser.cc"
    break;

  case 86:
#line 849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8464 "Parser/parser.cc"
    break;

  case 89:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8470 "Parser/parser.cc"
    break;

  case 90:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8476 "Parser/parser.cc"
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
#line 8496 "Parser/parser.cc"
    break;

  case 92:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8502 "Parser/parser.cc"
    break;

  case 93:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8508 "Parser/parser.cc"
    break;

  case 94:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8514 "Parser/parser.cc"
    break;

  case 95:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8520 "Parser/parser.cc"
    break;

  case 96:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8526 "Parser/parser.cc"
    break;

  case 97:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8532 "Parser/parser.cc"
    break;

  case 98:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8538 "Parser/parser.cc"
    break;

  case 99:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8544 "Parser/parser.cc"
    break;

  case 100:
#line 900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8553 "Parser/parser.cc"
    break;

  case 101:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 8559 "Parser/parser.cc"
    break;

  case 102:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 8565 "Parser/parser.cc"
    break;

  case 103:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 8571 "Parser/parser.cc"
    break;

  case 104:
#line 914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 8577 "Parser/parser.cc"
    break;

  case 105:
#line 915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 8583 "Parser/parser.cc"
    break;

  case 106:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 8589 "Parser/parser.cc"
    break;

  case 107:
#line 917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 8595 "Parser/parser.cc"
    break;

  case 109:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 8601 "Parser/parser.cc"
    break;

  case 110:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8607 "Parser/parser.cc"
    break;

  case 111:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8613 "Parser/parser.cc"
    break;

  case 112:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8619 "Parser/parser.cc"
    break;

  case 113:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8625 "Parser/parser.cc"
    break;

  case 114:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 8631 "Parser/parser.cc"
    break;

  case 115:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8637 "Parser/parser.cc"
    break;

  case 116:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8643 "Parser/parser.cc"
    break;

  case 124:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8649 "Parser/parser.cc"
    break;

  case 126:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8655 "Parser/parser.cc"
    break;

  case 127:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8661 "Parser/parser.cc"
    break;

  case 128:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8667 "Parser/parser.cc"
    break;

  case 130:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8673 "Parser/parser.cc"
    break;

  case 131:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8679 "Parser/parser.cc"
    break;

  case 133:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8685 "Parser/parser.cc"
    break;

  case 134:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8691 "Parser/parser.cc"
    break;

  case 136:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8697 "Parser/parser.cc"
    break;

  case 137:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8703 "Parser/parser.cc"
    break;

  case 138:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8709 "Parser/parser.cc"
    break;

  case 139:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8715 "Parser/parser.cc"
    break;

  case 141:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8721 "Parser/parser.cc"
    break;

  case 142:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8727 "Parser/parser.cc"
    break;

  case 144:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8733 "Parser/parser.cc"
    break;

  case 146:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8739 "Parser/parser.cc"
    break;

  case 148:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8745 "Parser/parser.cc"
    break;

  case 150:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 8751 "Parser/parser.cc"
    break;

  case 152:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 8757 "Parser/parser.cc"
    break;

  case 154:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8763 "Parser/parser.cc"
    break;

  case 155:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), (yyvsp[-3].expr)->clone(), (yyvsp[0].expr) ) ); }
#line 8769 "Parser/parser.cc"
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
#line 8781 "Parser/parser.cc"
    break;

  case 159:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8787 "Parser/parser.cc"
    break;

  case 160:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8793 "Parser/parser.cc"
    break;

  case 164:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 8799 "Parser/parser.cc"
    break;

  case 165:
#line 1077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 8805 "Parser/parser.cc"
    break;

  case 166:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 8811 "Parser/parser.cc"
    break;

  case 167:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 8817 "Parser/parser.cc"
    break;

  case 168:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 8823 "Parser/parser.cc"
    break;

  case 169:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 8829 "Parser/parser.cc"
    break;

  case 170:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 8835 "Parser/parser.cc"
    break;

  case 171:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 8841 "Parser/parser.cc"
    break;

  case 172:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 8847 "Parser/parser.cc"
    break;

  case 173:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 8853 "Parser/parser.cc"
    break;

  case 174:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 8859 "Parser/parser.cc"
    break;

  case 175:
#line 1090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 8865 "Parser/parser.cc"
    break;

  case 176:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 8871 "Parser/parser.cc"
    break;

  case 177:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].expr) ) ) ); }
#line 8877 "Parser/parser.cc"
    break;

  case 178:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) )); }
#line 8883 "Parser/parser.cc"
    break;

  case 180:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8889 "Parser/parser.cc"
    break;

  case 181:
#line 1112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8895 "Parser/parser.cc"
    break;

  case 182:
#line 1114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8901 "Parser/parser.cc"
    break;

  case 184:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8907 "Parser/parser.cc"
    break;

  case 185:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8913 "Parser/parser.cc"
    break;

  case 198:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 8919 "Parser/parser.cc"
    break;

  case 200:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 8925 "Parser/parser.cc"
    break;

  case 201:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8931 "Parser/parser.cc"
    break;

  case 202:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntx error, label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.stmt) = nullptr;
		}
#line 8942 "Parser/parser.cc"
    break;

  case 203:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 8948 "Parser/parser.cc"
    break;

  case 204:
#line 1170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 8954 "Parser/parser.cc"
    break;

  case 206:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8960 "Parser/parser.cc"
    break;

  case 207:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8966 "Parser/parser.cc"
    break;

  case 208:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8972 "Parser/parser.cc"
    break;

  case 209:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8978 "Parser/parser.cc"
    break;

  case 210:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8984 "Parser/parser.cc"
    break;

  case 213:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8990 "Parser/parser.cc"
    break;

  case 214:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 8996 "Parser/parser.cc"
    break;

  case 215:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9002 "Parser/parser.cc"
    break;

  case 216:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9008 "Parser/parser.cc"
    break;

  case 217:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9014 "Parser/parser.cc"
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
#line 9028 "Parser/parser.cc"
    break;

  case 219:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9034 "Parser/parser.cc"
    break;

  case 220:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9040 "Parser/parser.cc"
    break;

  case 221:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 9049 "Parser/parser.cc"
    break;

  case 222:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9055 "Parser/parser.cc"
    break;

  case 223:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9061 "Parser/parser.cc"
    break;

  case 224:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9067 "Parser/parser.cc"
    break;

  case 225:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9073 "Parser/parser.cc"
    break;

  case 226:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9079 "Parser/parser.cc"
    break;

  case 227:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9085 "Parser/parser.cc"
    break;

  case 228:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9091 "Parser/parser.cc"
    break;

  case 229:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9097 "Parser/parser.cc"
    break;

  case 230:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9103 "Parser/parser.cc"
    break;

  case 232:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9109 "Parser/parser.cc"
    break;

  case 233:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9115 "Parser/parser.cc"
    break;

  case 234:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9121 "Parser/parser.cc"
    break;

  case 235:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9127 "Parser/parser.cc"
    break;

  case 236:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9133 "Parser/parser.cc"
    break;

  case 237:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9139 "Parser/parser.cc"
    break;

  case 238:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9145 "Parser/parser.cc"
    break;

  case 240:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9151 "Parser/parser.cc"
    break;

  case 241:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9157 "Parser/parser.cc"
    break;

  case 242:
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9163 "Parser/parser.cc"
    break;

  case 244:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9169 "Parser/parser.cc"
    break;

  case 245:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9175 "Parser/parser.cc"
    break;

  case 246:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9181 "Parser/parser.cc"
    break;

  case 247:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9190 "Parser/parser.cc"
    break;

  case 248:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9196 "Parser/parser.cc"
    break;

  case 249:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9202 "Parser/parser.cc"
    break;

  case 250:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9208 "Parser/parser.cc"
    break;

  case 251:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9217 "Parser/parser.cc"
    break;

  case 252:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9223 "Parser/parser.cc"
    break;

  case 253:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9229 "Parser/parser.cc"
    break;

  case 254:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9235 "Parser/parser.cc"
    break;

  case 255:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9244 "Parser/parser.cc"
    break;

  case 256:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9250 "Parser/parser.cc"
    break;

  case 257:
#line 1336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9256 "Parser/parser.cc"
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
#line 9275 "Parser/parser.cc"
    break;

  case 260:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9281 "Parser/parser.cc"
    break;

  case 261:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9290 "Parser/parser.cc"
    break;

  case 262:
#line 1371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9296 "Parser/parser.cc"
    break;

  case 263:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9302 "Parser/parser.cc"
    break;

  case 264:
#line 1376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9308 "Parser/parser.cc"
    break;

  case 265:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9314 "Parser/parser.cc"
    break;

  case 266:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9320 "Parser/parser.cc"
    break;

  case 267:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9326 "Parser/parser.cc"
    break;

  case 268:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9335 "Parser/parser.cc"
    break;

  case 269:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9344 "Parser/parser.cc"
    break;

  case 270:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9350 "Parser/parser.cc"
    break;

  case 271:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9359 "Parser/parser.cc"
    break;

  case 272:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9368 "Parser/parser.cc"
    break;

  case 273:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9374 "Parser/parser.cc"
    break;

  case 274:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9380 "Parser/parser.cc"
    break;

  case 275:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9386 "Parser/parser.cc"
    break;

  case 276:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9392 "Parser/parser.cc"
    break;

  case 277:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9398 "Parser/parser.cc"
    break;

  case 278:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9404 "Parser/parser.cc"
    break;

  case 279:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9410 "Parser/parser.cc"
    break;

  case 280:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9416 "Parser/parser.cc"
    break;

  case 281:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9425 "Parser/parser.cc"
    break;

  case 282:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9435 "Parser/parser.cc"
    break;

  case 283:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9441 "Parser/parser.cc"
    break;

  case 284:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9447 "Parser/parser.cc"
    break;

  case 285:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9456 "Parser/parser.cc"
    break;

  case 286:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9466 "Parser/parser.cc"
    break;

  case 287:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9472 "Parser/parser.cc"
    break;

  case 288:
#line 1455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9481 "Parser/parser.cc"
    break;

  case 289:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9491 "Parser/parser.cc"
    break;

  case 290:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9497 "Parser/parser.cc"
    break;

  case 291:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9503 "Parser/parser.cc"
    break;

  case 292:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9509 "Parser/parser.cc"
    break;

  case 293:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9515 "Parser/parser.cc"
    break;

  case 294:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9524 "Parser/parser.cc"
    break;

  case 295:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9534 "Parser/parser.cc"
    break;

  case 296:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9540 "Parser/parser.cc"
    break;

  case 297:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9549 "Parser/parser.cc"
    break;

  case 298:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9559 "Parser/parser.cc"
    break;

  case 299:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9565 "Parser/parser.cc"
    break;

  case 300:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9574 "Parser/parser.cc"
    break;

  case 301:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9584 "Parser/parser.cc"
    break;

  case 302:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9590 "Parser/parser.cc"
    break;

  case 303:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9599 "Parser/parser.cc"
    break;

  case 304:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 9610 "Parser/parser.cc"
    break;

  case 305:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9616 "Parser/parser.cc"
    break;

  case 306:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9622 "Parser/parser.cc"
    break;

  case 307:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9628 "Parser/parser.cc"
    break;

  case 308:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 9634 "Parser/parser.cc"
    break;

  case 309:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9640 "Parser/parser.cc"
    break;

  case 311:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9646 "Parser/parser.cc"
    break;

  case 312:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9652 "Parser/parser.cc"
    break;

  case 313:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 9658 "Parser/parser.cc"
    break;

  case 314:
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 9664 "Parser/parser.cc"
    break;

  case 315:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 9670 "Parser/parser.cc"
    break;

  case 316:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 9676 "Parser/parser.cc"
    break;

  case 317:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 9682 "Parser/parser.cc"
    break;

  case 318:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 9688 "Parser/parser.cc"
    break;

  case 319:
#line 1574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 9694 "Parser/parser.cc"
    break;

  case 320:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 9700 "Parser/parser.cc"
    break;

  case 321:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 9706 "Parser/parser.cc"
    break;

  case 322:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 9712 "Parser/parser.cc"
    break;

  case 323:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9718 "Parser/parser.cc"
    break;

  case 324:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 9724 "Parser/parser.cc"
    break;

  case 325:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 9730 "Parser/parser.cc"
    break;

  case 326:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 9736 "Parser/parser.cc"
    break;

  case 327:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 9742 "Parser/parser.cc"
    break;

  case 328:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 9748 "Parser/parser.cc"
    break;

  case 329:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 9754 "Parser/parser.cc"
    break;

  case 330:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 9760 "Parser/parser.cc"
    break;

  case 331:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 9766 "Parser/parser.cc"
    break;

  case 332:
#line 1603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9772 "Parser/parser.cc"
    break;

  case 335:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9778 "Parser/parser.cc"
    break;

  case 336:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 9787 "Parser/parser.cc"
    break;

  case 337:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9793 "Parser/parser.cc"
    break;

  case 338:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9799 "Parser/parser.cc"
    break;

  case 341:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9805 "Parser/parser.cc"
    break;

  case 342:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9811 "Parser/parser.cc"
    break;

  case 345:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9817 "Parser/parser.cc"
    break;

  case 346:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) )); }
#line 9823 "Parser/parser.cc"
    break;

  case 347:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9829 "Parser/parser.cc"
    break;

  case 348:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9835 "Parser/parser.cc"
    break;

  case 349:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9841 "Parser/parser.cc"
    break;

  case 350:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9847 "Parser/parser.cc"
    break;

  case 351:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9853 "Parser/parser.cc"
    break;

  case 352:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9859 "Parser/parser.cc"
    break;

  case 353:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 9865 "Parser/parser.cc"
    break;

  case 356:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9871 "Parser/parser.cc"
    break;

  case 357:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9877 "Parser/parser.cc"
    break;

  case 358:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 9883 "Parser/parser.cc"
    break;

  case 359:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9889 "Parser/parser.cc"
    break;

  case 360:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9895 "Parser/parser.cc"
    break;

  case 361:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9901 "Parser/parser.cc"
    break;

  case 362:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9907 "Parser/parser.cc"
    break;

  case 363:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9913 "Parser/parser.cc"
    break;

  case 364:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 9919 "Parser/parser.cc"
    break;

  case 365:
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 9925 "Parser/parser.cc"
    break;

  case 366:
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 9931 "Parser/parser.cc"
    break;

  case 367:
#line 1723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 9937 "Parser/parser.cc"
    break;

  case 368:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9943 "Parser/parser.cc"
    break;

  case 369:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 9949 "Parser/parser.cc"
    break;

  case 370:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9955 "Parser/parser.cc"
    break;

  case 371:
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9961 "Parser/parser.cc"
    break;

  case 372:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9967 "Parser/parser.cc"
    break;

  case 373:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9973 "Parser/parser.cc"
    break;

  case 374:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 9979 "Parser/parser.cc"
    break;

  case 375:
#line 1743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 9985 "Parser/parser.cc"
    break;

  case 376:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 9991 "Parser/parser.cc"
    break;

  case 378:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9997 "Parser/parser.cc"
    break;

  case 379:
#line 1756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10003 "Parser/parser.cc"
    break;

  case 380:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10009 "Parser/parser.cc"
    break;

  case 385:
#line 1773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10015 "Parser/parser.cc"
    break;

  case 386:
#line 1775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10021 "Parser/parser.cc"
    break;

  case 387:
#line 1777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10027 "Parser/parser.cc"
    break;

  case 388:
#line 1779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10033 "Parser/parser.cc"
    break;

  case 389:
#line 1781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10039 "Parser/parser.cc"
    break;

  case 390:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10045 "Parser/parser.cc"
    break;

  case 391:
#line 1788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10051 "Parser/parser.cc"
    break;

  case 392:
#line 1793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10057 "Parser/parser.cc"
    break;

  case 395:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 10063 "Parser/parser.cc"
    break;

  case 396:
#line 1805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10069 "Parser/parser.cc"
    break;

  case 397:
#line 1807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10078 "Parser/parser.cc"
    break;

  case 398:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10084 "Parser/parser.cc"
    break;

  case 399:
#line 1817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10090 "Parser/parser.cc"
    break;

  case 400:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ) ); }
#line 10096 "Parser/parser.cc"
    break;

  case 401:
#line 1824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10105 "Parser/parser.cc"
    break;

  case 402:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10114 "Parser/parser.cc"
    break;

  case 403:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10120 "Parser/parser.cc"
    break;

  case 406:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10126 "Parser/parser.cc"
    break;

  case 407:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10132 "Parser/parser.cc"
    break;

  case 409:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10138 "Parser/parser.cc"
    break;

  case 410:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-1].decl) ); }
#line 10144 "Parser/parser.cc"
    break;

  case 417:
#line 1879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10155 "Parser/parser.cc"
    break;

  case 420:
#line 1891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10161 "Parser/parser.cc"
    break;

  case 421:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10167 "Parser/parser.cc"
    break;

  case 425:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10173 "Parser/parser.cc"
    break;

  case 427:
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 428:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10185 "Parser/parser.cc"
    break;

  case 429:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10191 "Parser/parser.cc"
    break;

  case 430:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10197 "Parser/parser.cc"
    break;

  case 431:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10203 "Parser/parser.cc"
    break;

  case 432:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10209 "Parser/parser.cc"
    break;

  case 434:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10215 "Parser/parser.cc"
    break;

  case 435:
#line 1942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10221 "Parser/parser.cc"
    break;

  case 436:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10227 "Parser/parser.cc"
    break;

  case 437:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10238 "Parser/parser.cc"
    break;

  case 438:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10244 "Parser/parser.cc"
    break;

  case 439:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10250 "Parser/parser.cc"
    break;

  case 440:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10256 "Parser/parser.cc"
    break;

  case 441:
#line 1989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 10262 "Parser/parser.cc"
    break;

  case 442:
#line 1994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10271 "Parser/parser.cc"
    break;

  case 443:
#line 1999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10280 "Parser/parser.cc"
    break;

  case 444:
#line 2004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10289 "Parser/parser.cc"
    break;

  case 445:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10300 "Parser/parser.cc"
    break;

  case 446:
#line 2022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10309 "Parser/parser.cc"
    break;

  case 447:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10315 "Parser/parser.cc"
    break;

  case 448:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10321 "Parser/parser.cc"
    break;

  case 449:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10327 "Parser/parser.cc"
    break;

  case 450:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10335 "Parser/parser.cc"
    break;

  case 451:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10343 "Parser/parser.cc"
    break;

  case 452:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10349 "Parser/parser.cc"
    break;

  case 455:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10364 "Parser/parser.cc"
    break;

  case 456:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10370 "Parser/parser.cc"
    break;

  case 457:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10376 "Parser/parser.cc"
    break;

  case 458:
#line 2073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10382 "Parser/parser.cc"
    break;

  case 459:
#line 2075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10388 "Parser/parser.cc"
    break;

  case 460:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10394 "Parser/parser.cc"
    break;

  case 466:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntax error, expecting ';' at end of ",
				(yyvsp[-1].decl)->type->enumeration.name ? "enum" : ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ),
				" declaration." ) );
			(yyval.decl) = nullptr;
		}
#line 10405 "Parser/parser.cc"
    break;

  case 479:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10411 "Parser/parser.cc"
    break;

  case 482:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10417 "Parser/parser.cc"
    break;

  case 485:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 10423 "Parser/parser.cc"
    break;

  case 486:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 10429 "Parser/parser.cc"
    break;

  case 487:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 10435 "Parser/parser.cc"
    break;

  case 488:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 10441 "Parser/parser.cc"
    break;

  case 489:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 10447 "Parser/parser.cc"
    break;

  case 490:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10453 "Parser/parser.cc"
    break;

  case 492:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10459 "Parser/parser.cc"
    break;

  case 493:
#line 2178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10465 "Parser/parser.cc"
    break;

  case 495:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10471 "Parser/parser.cc"
    break;

  case 496:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 10477 "Parser/parser.cc"
    break;

  case 497:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 10483 "Parser/parser.cc"
    break;

  case 498:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10489 "Parser/parser.cc"
    break;

  case 499:
#line 2200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10495 "Parser/parser.cc"
    break;

  case 500:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10501 "Parser/parser.cc"
    break;

  case 501:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10507 "Parser/parser.cc"
    break;

  case 502:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10513 "Parser/parser.cc"
    break;

  case 503:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10519 "Parser/parser.cc"
    break;

  case 504:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10525 "Parser/parser.cc"
    break;

  case 505:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10531 "Parser/parser.cc"
    break;

  case 506:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10537 "Parser/parser.cc"
    break;

  case 507:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10543 "Parser/parser.cc"
    break;

  case 508:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10549 "Parser/parser.cc"
    break;

  case 509:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10555 "Parser/parser.cc"
    break;

  case 510:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10561 "Parser/parser.cc"
    break;

  case 511:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10567 "Parser/parser.cc"
    break;

  case 512:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10573 "Parser/parser.cc"
    break;

  case 513:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10579 "Parser/parser.cc"
    break;

  case 514:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10585 "Parser/parser.cc"
    break;

  case 515:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10591 "Parser/parser.cc"
    break;

  case 516:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10597 "Parser/parser.cc"
    break;

  case 517:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10603 "Parser/parser.cc"
    break;

  case 518:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10609 "Parser/parser.cc"
    break;

  case 519:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10615 "Parser/parser.cc"
    break;

  case 520:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10621 "Parser/parser.cc"
    break;

  case 521:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10627 "Parser/parser.cc"
    break;

  case 522:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10633 "Parser/parser.cc"
    break;

  case 523:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10639 "Parser/parser.cc"
    break;

  case 524:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10645 "Parser/parser.cc"
    break;

  case 525:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10651 "Parser/parser.cc"
    break;

  case 526:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10657 "Parser/parser.cc"
    break;

  case 527:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10663 "Parser/parser.cc"
    break;

  case 528:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10669 "Parser/parser.cc"
    break;

  case 529:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10675 "Parser/parser.cc"
    break;

  case 530:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10681 "Parser/parser.cc"
    break;

  case 531:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10687 "Parser/parser.cc"
    break;

  case 533:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10693 "Parser/parser.cc"
    break;

  case 535:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10699 "Parser/parser.cc"
    break;

  case 536:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10705 "Parser/parser.cc"
    break;

  case 537:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10711 "Parser/parser.cc"
    break;

  case 539:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10717 "Parser/parser.cc"
    break;

  case 540:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10723 "Parser/parser.cc"
    break;

  case 541:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10729 "Parser/parser.cc"
    break;

  case 542:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10735 "Parser/parser.cc"
    break;

  case 544:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10741 "Parser/parser.cc"
    break;

  case 546:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10747 "Parser/parser.cc"
    break;

  case 547:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10753 "Parser/parser.cc"
    break;

  case 548:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10759 "Parser/parser.cc"
    break;

  case 549:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10765 "Parser/parser.cc"
    break;

  case 550:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 10771 "Parser/parser.cc"
    break;

  case 551:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10777 "Parser/parser.cc"
    break;

  case 552:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 10783 "Parser/parser.cc"
    break;

  case 553:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10789 "Parser/parser.cc"
    break;

  case 554:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10795 "Parser/parser.cc"
    break;

  case 555:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10806 "Parser/parser.cc"
    break;

  case 556:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10812 "Parser/parser.cc"
    break;

  case 557:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 558:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10824 "Parser/parser.cc"
    break;

  case 559:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10835 "Parser/parser.cc"
    break;

  case 560:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10841 "Parser/parser.cc"
    break;

  case 561:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10847 "Parser/parser.cc"
    break;

  case 562:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10856 "Parser/parser.cc"
    break;

  case 564:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10862 "Parser/parser.cc"
    break;

  case 565:
#line 2376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10868 "Parser/parser.cc"
    break;

  case 566:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10874 "Parser/parser.cc"
    break;

  case 568:
#line 2384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10880 "Parser/parser.cc"
    break;

  case 569:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10886 "Parser/parser.cc"
    break;

  case 571:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10892 "Parser/parser.cc"
    break;

  case 572:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10898 "Parser/parser.cc"
    break;

  case 573:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10904 "Parser/parser.cc"
    break;

  case 575:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10910 "Parser/parser.cc"
    break;

  case 576:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10916 "Parser/parser.cc"
    break;

  case 577:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10922 "Parser/parser.cc"
    break;

  case 578:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10928 "Parser/parser.cc"
    break;

  case 579:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10934 "Parser/parser.cc"
    break;

  case 581:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10940 "Parser/parser.cc"
    break;

  case 582:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10946 "Parser/parser.cc"
    break;

  case 583:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10952 "Parser/parser.cc"
    break;

  case 584:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10958 "Parser/parser.cc"
    break;

  case 585:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 10964 "Parser/parser.cc"
    break;

  case 586:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10975 "Parser/parser.cc"
    break;

  case 590:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10981 "Parser/parser.cc"
    break;

  case 591:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10987 "Parser/parser.cc"
    break;

  case 592:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 10996 "Parser/parser.cc"
    break;

  case 593:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11004 "Parser/parser.cc"
    break;

  case 594:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11013 "Parser/parser.cc"
    break;

  case 595:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11022 "Parser/parser.cc"
    break;

  case 596:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11031 "Parser/parser.cc"
    break;

  case 597:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11040 "Parser/parser.cc"
    break;

  case 599:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11046 "Parser/parser.cc"
    break;

  case 600:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11052 "Parser/parser.cc"
    break;

  case 601:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11062 "Parser/parser.cc"
    break;

  case 602:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11081 "Parser/parser.cc"
    break;

  case 605:
#line 2521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11087 "Parser/parser.cc"
    break;

  case 606:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11093 "Parser/parser.cc"
    break;

  case 607:
#line 2525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11099 "Parser/parser.cc"
    break;

  case 608:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11105 "Parser/parser.cc"
    break;

  case 609:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11111 "Parser/parser.cc"
    break;

  case 610:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11117 "Parser/parser.cc"
    break;

  case 611:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11126 "Parser/parser.cc"
    break;

  case 612:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11132 "Parser/parser.cc"
    break;

  case 613:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11141 "Parser/parser.cc"
    break;

  case 614:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11147 "Parser/parser.cc"
    break;

  case 615:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11156 "Parser/parser.cc"
    break;

  case 616:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11162 "Parser/parser.cc"
    break;

  case 617:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11168 "Parser/parser.cc"
    break;

  case 618:
#line 2566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11181 "Parser/parser.cc"
    break;

  case 619:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntax error, expecting ';' at end of previous declaration." ) );
			(yyval.decl) = nullptr;
		}
#line 11190 "Parser/parser.cc"
    break;

  case 620:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11196 "Parser/parser.cc"
    break;

  case 621:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11202 "Parser/parser.cc"
    break;

  case 622:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11215 "Parser/parser.cc"
    break;

  case 623:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11221 "Parser/parser.cc"
    break;

  case 626:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11227 "Parser/parser.cc"
    break;

  case 627:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11233 "Parser/parser.cc"
    break;

  case 630:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11239 "Parser/parser.cc"
    break;

  case 632:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11245 "Parser/parser.cc"
    break;

  case 633:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11251 "Parser/parser.cc"
    break;

  case 634:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11257 "Parser/parser.cc"
    break;

  case 635:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11263 "Parser/parser.cc"
    break;

  case 636:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11269 "Parser/parser.cc"
    break;

  case 637:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11275 "Parser/parser.cc"
    break;

  case 639:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11281 "Parser/parser.cc"
    break;

  case 641:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11287 "Parser/parser.cc"
    break;

  case 642:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11293 "Parser/parser.cc"
    break;

  case 644:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11299 "Parser/parser.cc"
    break;

  case 645:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11305 "Parser/parser.cc"
    break;

  case 647:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11311 "Parser/parser.cc"
    break;

  case 648:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11317 "Parser/parser.cc"
    break;

  case 649:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11323 "Parser/parser.cc"
    break;

  case 650:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11329 "Parser/parser.cc"
    break;

  case 651:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11335 "Parser/parser.cc"
    break;

  case 652:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11341 "Parser/parser.cc"
    break;

  case 653:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11352 "Parser/parser.cc"
    break;

  case 654:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11358 "Parser/parser.cc"
    break;

  case 655:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11366 "Parser/parser.cc"
    break;

  case 656:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding '!' the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11372 "Parser/parser.cc"
    break;

  case 657:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11383 "Parser/parser.cc"
    break;

  case 658:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11391 "Parser/parser.cc"
    break;

  case 659:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11399 "Parser/parser.cc"
    break;

  case 660:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11407 "Parser/parser.cc"
    break;

  case 661:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11415 "Parser/parser.cc"
    break;

  case 663:
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11421 "Parser/parser.cc"
    break;

  case 664:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11427 "Parser/parser.cc"
    break;

  case 665:
#line 2726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11436 "Parser/parser.cc"
    break;

  case 666:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11445 "Parser/parser.cc"
    break;

  case 667:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11451 "Parser/parser.cc"
    break;

  case 668:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 11457 "Parser/parser.cc"
    break;

  case 669:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11463 "Parser/parser.cc"
    break;

  case 670:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11469 "Parser/parser.cc"
    break;

  case 672:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11475 "Parser/parser.cc"
    break;

  case 673:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11481 "Parser/parser.cc"
    break;

  case 674:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11487 "Parser/parser.cc"
    break;

  case 675:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11493 "Parser/parser.cc"
    break;

  case 676:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11499 "Parser/parser.cc"
    break;

  case 677:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11505 "Parser/parser.cc"
    break;

  case 680:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11511 "Parser/parser.cc"
    break;

  case 681:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11517 "Parser/parser.cc"
    break;

  case 682:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11523 "Parser/parser.cc"
    break;

  case 684:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11529 "Parser/parser.cc"
    break;

  case 685:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11535 "Parser/parser.cc"
    break;

  case 686:
#line 2787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11541 "Parser/parser.cc"
    break;

  case 688:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11547 "Parser/parser.cc"
    break;

  case 689:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11553 "Parser/parser.cc"
    break;

  case 690:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11559 "Parser/parser.cc"
    break;

  case 692:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11565 "Parser/parser.cc"
    break;

  case 695:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11571 "Parser/parser.cc"
    break;

  case 696:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11577 "Parser/parser.cc"
    break;

  case 698:
#line 2821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11583 "Parser/parser.cc"
    break;

  case 699:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11589 "Parser/parser.cc"
    break;

  case 700:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11595 "Parser/parser.cc"
    break;

  case 705:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11601 "Parser/parser.cc"
    break;

  case 707:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11607 "Parser/parser.cc"
    break;

  case 708:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11613 "Parser/parser.cc"
    break;

  case 709:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11619 "Parser/parser.cc"
    break;

  case 710:
#line 2852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11625 "Parser/parser.cc"
    break;

  case 711:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11631 "Parser/parser.cc"
    break;

  case 712:
#line 2863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11637 "Parser/parser.cc"
    break;

  case 718:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11643 "Parser/parser.cc"
    break;

  case 721:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11649 "Parser/parser.cc"
    break;

  case 722:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 11655 "Parser/parser.cc"
    break;

  case 723:
#line 2888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 11661 "Parser/parser.cc"
    break;

  case 724:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11667 "Parser/parser.cc"
    break;

  case 725:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11673 "Parser/parser.cc"
    break;

  case 726:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11679 "Parser/parser.cc"
    break;

  case 727:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11685 "Parser/parser.cc"
    break;

  case 729:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 11691 "Parser/parser.cc"
    break;

  case 730:
#line 2902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (InitializerNode *)( (yyvsp[-2].init)->set_last( (yyvsp[0].init) ) ); }
#line 11697 "Parser/parser.cc"
    break;

  case 731:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (InitializerNode *)((yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) )); }
#line 11703 "Parser/parser.cc"
    break;

  case 733:
#line 2919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 11709 "Parser/parser.cc"
    break;

  case 735:
#line 2925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-1].expr)->set_last( (yyvsp[0].expr) )); }
#line 11715 "Parser/parser.cc"
    break;

  case 736:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 11721 "Parser/parser.cc"
    break;

  case 737:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11727 "Parser/parser.cc"
    break;

  case 738:
#line 2936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11733 "Parser/parser.cc"
    break;

  case 739:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 11739 "Parser/parser.cc"
    break;

  case 740:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11745 "Parser/parser.cc"
    break;

  case 742:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11751 "Parser/parser.cc"
    break;

  case 743:
#line 2969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11757 "Parser/parser.cc"
    break;

  case 744:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11763 "Parser/parser.cc"
    break;

  case 745:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11774 "Parser/parser.cc"
    break;

  case 746:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11780 "Parser/parser.cc"
    break;

  case 747:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 11786 "Parser/parser.cc"
    break;

  case 748:
#line 2987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11792 "Parser/parser.cc"
    break;

  case 749:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11801 "Parser/parser.cc"
    break;

  case 750:
#line 2995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11807 "Parser/parser.cc"
    break;

  case 751:
#line 3000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11813 "Parser/parser.cc"
    break;

  case 752:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11819 "Parser/parser.cc"
    break;

  case 753:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 11825 "Parser/parser.cc"
    break;

  case 754:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11831 "Parser/parser.cc"
    break;

  case 755:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11837 "Parser/parser.cc"
    break;

  case 756:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11843 "Parser/parser.cc"
    break;

  case 757:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 11849 "Parser/parser.cc"
    break;

  case 758:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11855 "Parser/parser.cc"
    break;

  case 759:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11861 "Parser/parser.cc"
    break;

  case 762:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11867 "Parser/parser.cc"
    break;

  case 763:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11873 "Parser/parser.cc"
    break;

  case 764:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11879 "Parser/parser.cc"
    break;

  case 765:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11885 "Parser/parser.cc"
    break;

  case 767:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11891 "Parser/parser.cc"
    break;

  case 768:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 11897 "Parser/parser.cc"
    break;

  case 769:
#line 3055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11903 "Parser/parser.cc"
    break;

  case 770:
#line 3057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11909 "Parser/parser.cc"
    break;

  case 771:
#line 3059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11915 "Parser/parser.cc"
    break;

  case 772:
#line 3064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11921 "Parser/parser.cc"
    break;

  case 773:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11927 "Parser/parser.cc"
    break;

  case 774:
#line 3071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11936 "Parser/parser.cc"
    break;

  case 775:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11945 "Parser/parser.cc"
    break;

  case 776:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11954 "Parser/parser.cc"
    break;

  case 777:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11960 "Parser/parser.cc"
    break;

  case 778:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11969 "Parser/parser.cc"
    break;

  case 779:
#line 3096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11975 "Parser/parser.cc"
    break;

  case 781:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11981 "Parser/parser.cc"
    break;

  case 786:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11987 "Parser/parser.cc"
    break;

  case 787:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11993 "Parser/parser.cc"
    break;

  case 788:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11999 "Parser/parser.cc"
    break;

  case 790:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 12005 "Parser/parser.cc"
    break;

  case 791:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12011 "Parser/parser.cc"
    break;

  case 792:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12017 "Parser/parser.cc"
    break;

  case 793:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12023 "Parser/parser.cc"
    break;

  case 795:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12029 "Parser/parser.cc"
    break;

  case 796:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12035 "Parser/parser.cc"
    break;

  case 797:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12041 "Parser/parser.cc"
    break;

  case 798:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12058 "Parser/parser.cc"
    break;

  case 799:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12064 "Parser/parser.cc"
    break;

  case 800:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12070 "Parser/parser.cc"
    break;

  case 801:
#line 3174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12076 "Parser/parser.cc"
    break;

  case 802:
#line 3176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12082 "Parser/parser.cc"
    break;

  case 803:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12088 "Parser/parser.cc"
    break;

  case 804:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12094 "Parser/parser.cc"
    break;

  case 806:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12103 "Parser/parser.cc"
    break;

  case 807:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12109 "Parser/parser.cc"
    break;

  case 808:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12118 "Parser/parser.cc"
    break;

  case 809:
#line 3195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12128 "Parser/parser.cc"
    break;

  case 810:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12137 "Parser/parser.cc"
    break;

  case 811:
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12147 "Parser/parser.cc"
    break;

  case 812:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12158 "Parser/parser.cc"
    break;

  case 813:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12168 "Parser/parser.cc"
    break;

  case 814:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12179 "Parser/parser.cc"
    break;

  case 815:
#line 3233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12189 "Parser/parser.cc"
    break;

  case 816:
#line 3239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12200 "Parser/parser.cc"
    break;

  case 817:
#line 3246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12210 "Parser/parser.cc"
    break;

  case 819:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12216 "Parser/parser.cc"
    break;

  case 820:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12222 "Parser/parser.cc"
    break;

  case 821:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12228 "Parser/parser.cc"
    break;

  case 822:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12240 "Parser/parser.cc"
    break;

  case 823:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12251 "Parser/parser.cc"
    break;

  case 824:
#line 3288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12260 "Parser/parser.cc"
    break;

  case 825:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12269 "Parser/parser.cc"
    break;

  case 826:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12275 "Parser/parser.cc"
    break;

  case 827:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12281 "Parser/parser.cc"
    break;

  case 828:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12287 "Parser/parser.cc"
    break;

  case 829:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12296 "Parser/parser.cc"
    break;

  case 830:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12302 "Parser/parser.cc"
    break;

  case 831:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12308 "Parser/parser.cc"
    break;

  case 832:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12314 "Parser/parser.cc"
    break;

  case 837:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12320 "Parser/parser.cc"
    break;

  case 838:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12326 "Parser/parser.cc"
    break;

  case 839:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12336 "Parser/parser.cc"
    break;

  case 840:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12342 "Parser/parser.cc"
    break;

  case 843:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12348 "Parser/parser.cc"
    break;

  case 844:
#line 3361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12354 "Parser/parser.cc"
    break;

  case 846:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12360 "Parser/parser.cc"
    break;

  case 847:
#line 3372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12366 "Parser/parser.cc"
    break;

  case 848:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12372 "Parser/parser.cc"
    break;

  case 849:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12378 "Parser/parser.cc"
    break;

  case 854:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12384 "Parser/parser.cc"
    break;

  case 855:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12390 "Parser/parser.cc"
    break;

  case 856:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12396 "Parser/parser.cc"
    break;

  case 857:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12402 "Parser/parser.cc"
    break;

  case 858:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12408 "Parser/parser.cc"
    break;

  case 860:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12414 "Parser/parser.cc"
    break;

  case 861:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12420 "Parser/parser.cc"
    break;

  case 862:
#line 3439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12426 "Parser/parser.cc"
    break;

  case 863:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12432 "Parser/parser.cc"
    break;

  case 864:
#line 3443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12438 "Parser/parser.cc"
    break;

  case 865:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12444 "Parser/parser.cc"
    break;

  case 866:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12450 "Parser/parser.cc"
    break;

  case 867:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12456 "Parser/parser.cc"
    break;

  case 868:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12462 "Parser/parser.cc"
    break;

  case 869:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12468 "Parser/parser.cc"
    break;

  case 870:
#line 3458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12474 "Parser/parser.cc"
    break;

  case 871:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12480 "Parser/parser.cc"
    break;

  case 872:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12486 "Parser/parser.cc"
    break;

  case 873:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12492 "Parser/parser.cc"
    break;

  case 874:
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12498 "Parser/parser.cc"
    break;

  case 875:
#line 3471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12504 "Parser/parser.cc"
    break;

  case 876:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12510 "Parser/parser.cc"
    break;

  case 877:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12516 "Parser/parser.cc"
    break;

  case 879:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12522 "Parser/parser.cc"
    break;

  case 880:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12528 "Parser/parser.cc"
    break;

  case 881:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12534 "Parser/parser.cc"
    break;

  case 882:
#line 3494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12540 "Parser/parser.cc"
    break;

  case 883:
#line 3496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12546 "Parser/parser.cc"
    break;

  case 884:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12552 "Parser/parser.cc"
    break;

  case 885:
#line 3503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12558 "Parser/parser.cc"
    break;

  case 886:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12564 "Parser/parser.cc"
    break;

  case 887:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12570 "Parser/parser.cc"
    break;

  case 888:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12576 "Parser/parser.cc"
    break;

  case 889:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12582 "Parser/parser.cc"
    break;

  case 890:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12588 "Parser/parser.cc"
    break;

  case 891:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12594 "Parser/parser.cc"
    break;

  case 892:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12600 "Parser/parser.cc"
    break;

  case 893:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12606 "Parser/parser.cc"
    break;

  case 894:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12612 "Parser/parser.cc"
    break;

  case 898:
#line 3542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12618 "Parser/parser.cc"
    break;

  case 899:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12624 "Parser/parser.cc"
    break;

  case 900:
#line 3546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12630 "Parser/parser.cc"
    break;

  case 901:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12636 "Parser/parser.cc"
    break;

  case 902:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12642 "Parser/parser.cc"
    break;

  case 903:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12648 "Parser/parser.cc"
    break;

  case 904:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12654 "Parser/parser.cc"
    break;

  case 905:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12660 "Parser/parser.cc"
    break;

  case 906:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12666 "Parser/parser.cc"
    break;

  case 907:
#line 3566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12672 "Parser/parser.cc"
    break;

  case 908:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12678 "Parser/parser.cc"
    break;

  case 909:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12684 "Parser/parser.cc"
    break;

  case 910:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12690 "Parser/parser.cc"
    break;

  case 911:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12696 "Parser/parser.cc"
    break;

  case 912:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12702 "Parser/parser.cc"
    break;

  case 913:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 12711 "Parser/parser.cc"
    break;

  case 914:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12717 "Parser/parser.cc"
    break;

  case 915:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12723 "Parser/parser.cc"
    break;

  case 917:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12729 "Parser/parser.cc"
    break;

  case 918:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12735 "Parser/parser.cc"
    break;

  case 919:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12741 "Parser/parser.cc"
    break;

  case 920:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12747 "Parser/parser.cc"
    break;

  case 921:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12753 "Parser/parser.cc"
    break;

  case 922:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12759 "Parser/parser.cc"
    break;

  case 923:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12765 "Parser/parser.cc"
    break;

  case 924:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12771 "Parser/parser.cc"
    break;

  case 925:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12777 "Parser/parser.cc"
    break;

  case 926:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12783 "Parser/parser.cc"
    break;

  case 927:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12789 "Parser/parser.cc"
    break;

  case 928:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12795 "Parser/parser.cc"
    break;

  case 929:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12801 "Parser/parser.cc"
    break;

  case 930:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12807 "Parser/parser.cc"
    break;

  case 931:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12813 "Parser/parser.cc"
    break;

  case 932:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12819 "Parser/parser.cc"
    break;

  case 933:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12825 "Parser/parser.cc"
    break;

  case 934:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12831 "Parser/parser.cc"
    break;

  case 936:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12837 "Parser/parser.cc"
    break;

  case 937:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12843 "Parser/parser.cc"
    break;

  case 938:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12849 "Parser/parser.cc"
    break;

  case 939:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12855 "Parser/parser.cc"
    break;

  case 940:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12861 "Parser/parser.cc"
    break;

  case 941:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12867 "Parser/parser.cc"
    break;

  case 942:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12873 "Parser/parser.cc"
    break;

  case 943:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12879 "Parser/parser.cc"
    break;

  case 944:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12885 "Parser/parser.cc"
    break;

  case 945:
#line 3678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12891 "Parser/parser.cc"
    break;

  case 946:
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12897 "Parser/parser.cc"
    break;

  case 947:
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12903 "Parser/parser.cc"
    break;

  case 948:
#line 3687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12909 "Parser/parser.cc"
    break;

  case 949:
#line 3689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12915 "Parser/parser.cc"
    break;

  case 950:
#line 3691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12921 "Parser/parser.cc"
    break;

  case 951:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12927 "Parser/parser.cc"
    break;

  case 952:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12933 "Parser/parser.cc"
    break;

  case 953:
#line 3705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12939 "Parser/parser.cc"
    break;

  case 955:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12945 "Parser/parser.cc"
    break;

  case 956:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12951 "Parser/parser.cc"
    break;

  case 957:
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12957 "Parser/parser.cc"
    break;

  case 958:
#line 3717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12963 "Parser/parser.cc"
    break;

  case 959:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12969 "Parser/parser.cc"
    break;

  case 960:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12975 "Parser/parser.cc"
    break;

  case 961:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12981 "Parser/parser.cc"
    break;

  case 962:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12987 "Parser/parser.cc"
    break;

  case 963:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12993 "Parser/parser.cc"
    break;

  case 964:
#line 3735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12999 "Parser/parser.cc"
    break;

  case 965:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13005 "Parser/parser.cc"
    break;

  case 966:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13011 "Parser/parser.cc"
    break;

  case 967:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13017 "Parser/parser.cc"
    break;

  case 968:
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13023 "Parser/parser.cc"
    break;

  case 970:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13029 "Parser/parser.cc"
    break;

  case 971:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13035 "Parser/parser.cc"
    break;

  case 972:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13041 "Parser/parser.cc"
    break;

  case 973:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13047 "Parser/parser.cc"
    break;

  case 974:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13053 "Parser/parser.cc"
    break;

  case 975:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13059 "Parser/parser.cc"
    break;

  case 976:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13065 "Parser/parser.cc"
    break;

  case 977:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13071 "Parser/parser.cc"
    break;

  case 978:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13077 "Parser/parser.cc"
    break;

  case 979:
#line 3788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13083 "Parser/parser.cc"
    break;

  case 980:
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13089 "Parser/parser.cc"
    break;

  case 982:
#line 3808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13095 "Parser/parser.cc"
    break;

  case 983:
#line 3810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13101 "Parser/parser.cc"
    break;

  case 984:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13107 "Parser/parser.cc"
    break;

  case 985:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13113 "Parser/parser.cc"
    break;

  case 986:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13119 "Parser/parser.cc"
    break;

  case 987:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13125 "Parser/parser.cc"
    break;

  case 988:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13131 "Parser/parser.cc"
    break;

  case 990:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13137 "Parser/parser.cc"
    break;

  case 991:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13143 "Parser/parser.cc"
    break;

  case 992:
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13149 "Parser/parser.cc"
    break;

  case 993:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13155 "Parser/parser.cc"
    break;

  case 994:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13161 "Parser/parser.cc"
    break;

  case 995:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13167 "Parser/parser.cc"
    break;

  case 996:
#line 3848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13173 "Parser/parser.cc"
    break;

  case 997:
#line 3850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13179 "Parser/parser.cc"
    break;

  case 998:
#line 3853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13185 "Parser/parser.cc"
    break;

  case 999:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13191 "Parser/parser.cc"
    break;

  case 1001:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13197 "Parser/parser.cc"
    break;

  case 1002:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13203 "Parser/parser.cc"
    break;

  case 1004:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13209 "Parser/parser.cc"
    break;

  case 1005:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13215 "Parser/parser.cc"
    break;

  case 1007:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13221 "Parser/parser.cc"
    break;

  case 1008:
#line 3877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13227 "Parser/parser.cc"
    break;

  case 1009:
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13233 "Parser/parser.cc"
    break;

  case 1010:
#line 3884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13239 "Parser/parser.cc"
    break;

  case 1011:
#line 3886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13245 "Parser/parser.cc"
    break;

  case 1012:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13251 "Parser/parser.cc"
    break;

  case 1013:
#line 3922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13257 "Parser/parser.cc"
    break;

  case 1016:
#line 3929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13263 "Parser/parser.cc"
    break;

  case 1017:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13269 "Parser/parser.cc"
    break;

  case 1018:
#line 3933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13275 "Parser/parser.cc"
    break;

  case 1019:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13281 "Parser/parser.cc"
    break;

  case 1020:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13287 "Parser/parser.cc"
    break;

  case 1021:
#line 3942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13293 "Parser/parser.cc"
    break;

  case 1022:
#line 3944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13299 "Parser/parser.cc"
    break;

  case 1023:
#line 3946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13305 "Parser/parser.cc"
    break;

  case 1025:
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13311 "Parser/parser.cc"
    break;

  case 1026:
#line 3954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13317 "Parser/parser.cc"
    break;

  case 1027:
#line 3956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13323 "Parser/parser.cc"
    break;

  case 1028:
#line 3961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13329 "Parser/parser.cc"
    break;

  case 1029:
#line 3963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13335 "Parser/parser.cc"
    break;

  case 1030:
#line 3965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13341 "Parser/parser.cc"
    break;

  case 1032:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13347 "Parser/parser.cc"
    break;

  case 1034:
#line 3983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13353 "Parser/parser.cc"
    break;

  case 1035:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13359 "Parser/parser.cc"
    break;

  case 1036:
#line 3988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13365 "Parser/parser.cc"
    break;

  case 1037:
#line 3991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13371 "Parser/parser.cc"
    break;

  case 1038:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13377 "Parser/parser.cc"
    break;

  case 1039:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13383 "Parser/parser.cc"
    break;

  case 1041:
#line 4010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13389 "Parser/parser.cc"
    break;

  case 1042:
#line 4012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13395 "Parser/parser.cc"
    break;

  case 1043:
#line 4017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13401 "Parser/parser.cc"
    break;

  case 1044:
#line 4019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13407 "Parser/parser.cc"
    break;

  case 1045:
#line 4021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13413 "Parser/parser.cc"
    break;

  case 1046:
#line 4023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13419 "Parser/parser.cc"
    break;

  case 1047:
#line 4025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13425 "Parser/parser.cc"
    break;

  case 1049:
#line 4031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13431 "Parser/parser.cc"
    break;

  case 1050:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13437 "Parser/parser.cc"
    break;

  case 1051:
#line 4035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13443 "Parser/parser.cc"
    break;

  case 1052:
#line 4040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13449 "Parser/parser.cc"
    break;

  case 1053:
#line 4042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13455 "Parser/parser.cc"
    break;

  case 1056:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13461 "Parser/parser.cc"
    break;

  case 1059:
#line 4063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13467 "Parser/parser.cc"
    break;

  case 1060:
#line 4065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13473 "Parser/parser.cc"
    break;

  case 1061:
#line 4067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13479 "Parser/parser.cc"
    break;

  case 1062:
#line 4069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13485 "Parser/parser.cc"
    break;

  case 1063:
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13491 "Parser/parser.cc"
    break;

  case 1064:
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13497 "Parser/parser.cc"
    break;

  case 1065:
#line 4080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13503 "Parser/parser.cc"
    break;

  case 1066:
#line 4082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13509 "Parser/parser.cc"
    break;

  case 1067:
#line 4084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13515 "Parser/parser.cc"
    break;

  case 1068:
#line 4086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13521 "Parser/parser.cc"
    break;

  case 1069:
#line 4088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13527 "Parser/parser.cc"
    break;

  case 1070:
#line 4091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13533 "Parser/parser.cc"
    break;

  case 1071:
#line 4093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13539 "Parser/parser.cc"
    break;

  case 1072:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13545 "Parser/parser.cc"
    break;

  case 1073:
#line 4097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13551 "Parser/parser.cc"
    break;

  case 1074:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13557 "Parser/parser.cc"
    break;

  case 1075:
#line 4104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13563 "Parser/parser.cc"
    break;

  case 1076:
#line 4106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13569 "Parser/parser.cc"
    break;

  case 1077:
#line 4111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13575 "Parser/parser.cc"
    break;

  case 1078:
#line 4113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 13581 "Parser/parser.cc"
    break;

  case 1080:
#line 4140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13587 "Parser/parser.cc"
    break;

  case 1084:
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13593 "Parser/parser.cc"
    break;

  case 1085:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13599 "Parser/parser.cc"
    break;

  case 1086:
#line 4155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13605 "Parser/parser.cc"
    break;

  case 1087:
#line 4157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13611 "Parser/parser.cc"
    break;

  case 1088:
#line 4159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13617 "Parser/parser.cc"
    break;

  case 1089:
#line 4161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13623 "Parser/parser.cc"
    break;

  case 1090:
#line 4168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13629 "Parser/parser.cc"
    break;

  case 1091:
#line 4170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13635 "Parser/parser.cc"
    break;

  case 1092:
#line 4172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13641 "Parser/parser.cc"
    break;

  case 1093:
#line 4174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13647 "Parser/parser.cc"
    break;

  case 1094:
#line 4176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13653 "Parser/parser.cc"
    break;

  case 1095:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13659 "Parser/parser.cc"
    break;

  case 1096:
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13665 "Parser/parser.cc"
    break;

  case 1097:
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13671 "Parser/parser.cc"
    break;

  case 1098:
#line 4187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13677 "Parser/parser.cc"
    break;

  case 1099:
#line 4194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13683 "Parser/parser.cc"
    break;

  case 1100:
#line 4196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13689 "Parser/parser.cc"
    break;

  case 1103:
#line 4220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13695 "Parser/parser.cc"
    break;

  case 1104:
#line 4222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13701 "Parser/parser.cc"
    break;


#line 13705 "Parser/parser.cc"

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
#line 4225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
